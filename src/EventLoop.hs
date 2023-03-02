-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module EventLoop
(
  convertGithubEvent, -- An internal helper function, but exposed for testing.
  runGithubEventLoop,
  runLogicEventLoop
)
where

import Control.Concurrent.STM.TBQueue
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN, logInfoN)
import Control.Monad.STM (atomically)
import Control.Monad.Free (foldFree)
import Data.Foldable (traverse_)
import Prometheus (MonadMonitor)
import Data.Text (Text)
import qualified Data.Text as Text

import Configuration (ProjectConfiguration, TriggerConfiguration, MergeWindowExemptionConfiguration)
import Github (PullRequestPayload, CommentPayload, CommitStatusPayload, WebhookEvent (..))
import Github (eventProjectInfo)
import Project (ProjectInfo (..), ProjectState, PullRequestId (..))
import Time ( TimeOperationFree )
import Sum (runSum)

import qualified Configuration as Config
import qualified Git
import qualified Github
import qualified GithubApi
import qualified Logic
import qualified Project
import qualified Metrics.Metrics as Metrics

eventFromPullRequestPayload :: PullRequestPayload -> Logic.Event
eventFromPullRequestPayload payload =
  let
    number = Github.number (payload :: PullRequestPayload) -- TODO: Use PullRequestId wrapper from beginning.
    title  = Github.title  (payload :: PullRequestPayload)
    author = Github.author (payload :: PullRequestPayload)
    branch = Github.branch (payload :: PullRequestPayload)
    sha    = Github.sha    (payload :: PullRequestPayload)
    baseBranch = Github.baseBranch (payload :: PullRequestPayload)
  in
    case Github.action (payload :: PullRequestPayload) of
      Github.Opened      -> Logic.PullRequestOpened (PullRequestId number) branch baseBranch sha title author
      Github.Reopened    -> Logic.PullRequestOpened (PullRequestId number) branch baseBranch sha title author
      Github.Closed      -> Logic.PullRequestClosed (PullRequestId number)
      Github.Synchronize -> Logic.PullRequestCommitChanged (PullRequestId number) sha
      Github.Edited      -> Logic.PullRequestEdited (PullRequestId number) title baseBranch

eventFromCommentPayload :: CommentPayload -> Maybe Logic.Event
eventFromCommentPayload payload =
  let number = Github.number (payload :: CommentPayload) -- TODO: Use PullRequestId wrapper from beginning.
      author = Github.author (payload :: CommentPayload) -- TODO: Wrapper type
      body   = Github.body   (payload :: CommentPayload)
      commentAdded = Logic.CommentAdded (PullRequestId number) author body
  in case Github.action (payload :: CommentPayload) of
    Left Github.CommentCreated -> Just commentAdded
    Right Github.ReviewSubmitted -> Just commentAdded
    -- Do not bother with edited and deleted comments, as it would tremendously
    -- complicate handling of approval. Once approved, this cannot be undone.
    -- And if approval undo is desired, it would be better implemented as a
    -- separate magic comment, rather than editing the approval comment.
    _ -> Nothing

mapCommitStatus :: Github.CommitStatus -> Maybe Text.Text -> Project.BuildStatus
mapCommitStatus status murl = case status of
  Github.Pending -> case murl of
                    Nothing -> Project.BuildPending
                    Just url -> Project.BuildStarted url
  Github.Success -> Project.BuildSucceeded
  Github.Failure -> Project.BuildFailed murl
  Github.Error   -> Project.BuildFailed murl

eventFromCommitStatusPayload :: CommitStatusPayload -> Logic.Event
eventFromCommitStatusPayload payload =
  let sha     = Github.sha     (payload :: CommitStatusPayload)
      status  = Github.status  (payload :: CommitStatusPayload)
      url     = Github.url     (payload :: CommitStatusPayload)
      context = Github.context (payload :: CommitStatusPayload)
  in  Logic.BuildStatusChanged sha context (mapCommitStatus status url)

convertGithubEvent :: Github.WebhookEvent -> Maybe Logic.Event
convertGithubEvent event = case event of
  Ping                 -> Nothing -- TODO: What to do with this one?
  PullRequest payload  -> Just $ eventFromPullRequestPayload payload
  CommitStatus payload -> Just $ eventFromCommitStatusPayload payload
  Comment payload      -> eventFromCommentPayload payload

-- The event loop that converts GitHub webhook events into logic events.
runGithubEventLoop
  :: (MonadIO m, MonadLogger m)
  => Github.EventQueue
  -> (ProjectInfo -> Logic.Event -> IO ()) -> m ()
runGithubEventLoop ghQueue enqueueEvent = runLoop
  where
    shouldHandle ghEvent = (ghEvent /= Ping)
    runLoop = do
      ghEvent <- liftIO $ atomically $ readTBQueue ghQueue
      let projectInfo = eventProjectInfo ghEvent
      logDebugN $ "github loop received event: " <> showText ghEvent
      when (shouldHandle ghEvent) $
        -- If conversion yielded an event, enqueue it. Block if the queue is full.
        traverse_ (liftIO . enqueueEvent projectInfo) (convertGithubEvent ghEvent)
      runLoop

runLogicEventLoop
  :: MonadIO m
  => MonadLogger m
  => MonadMonitor m
  => TriggerConfiguration
  -> ProjectConfiguration
  -> MergeWindowExemptionConfiguration
  -- Interpreters for Git and GitHub actions.
  -> (forall a. Metrics.MetricsOperationFree a -> m a)
  -> (forall a. Time.TimeOperationFree a -> m a)
  -> (forall a. Git.GitOperationFree a -> m a)
  -> (forall a. GithubApi.GithubOperationFree a -> m a)
  -- Action that gets the next event from the queue.
  -> m (Maybe Logic.Event)
  -- Action to perform after the state has changed, such as
  -- persisting the new state, and making it available to the
  -- webinterface.
  -> (ProjectState -> m ())
  -> ProjectState
  -> m ProjectState
runLogicEventLoop
  triggerConfig projectConfig mergeWindowExemptionConfig
  runMetrics runTime runGit runGithub
  getNextEvent publish initialState =
  let
    repo          = Config.repository projectConfig
    runAll        = foldFree (runSum (runSum runMetrics runTime) (runSum runGit runGithub))
    runAction     = foldFree (runSum (Logic.runBaseAction projectConfig) (Logic.runRetrieveEnvironment projectConfig))
    handleAndContinue state0 event = do
      -- Handle the event and then perform any additional required actions until
      -- the state reaches a fixed point (when there are no further actions to
      -- perform).
      logInfoN  $ "logic loop received event (" <> repo <> "): " <> showText event
      logDebugN $ "state before (" <> repo <> "): " <> showText state0
      state1 <- runAll $ runAction $
        Logic.handleEvent triggerConfig mergeWindowExemptionConfig event state0
      publish state1
      logDebugN $ "state after (" <> repo <> "): " <> showText state1
      runLoop state1

    runLoop state = do
      -- Before anything, clone the repository if there is no clone.
      foldFree runGit $ Logic.ensureCloned projectConfig
      -- Take one event off the queue, block if there is none.
      eventOrStopSignal <- getNextEvent
      -- Queue items are of type 'Maybe Event'; 'Nothing' signals loop
      -- termination. If there was an event, run one iteration and recurse.
      case eventOrStopSignal of
        Just event -> handleAndContinue state event
        Nothing    -> return state

  in
    runLoop initialState

showText :: Show a => a -> Text
showText  =  Text.pack . show
