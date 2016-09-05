-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Text (Text)

import Git (runGit)
import Configuration (Configuration)
import Github (PullRequestPayload, CommentPayload, CommitStatusPayload, WebhookEvent (..))
import Github (eventRepository, eventRepositoryOwner)
import Project (ProjectState, PullRequestId (..))

import qualified Configuration as Config
import qualified Data.Text as Text
import qualified Github
import qualified Logic
import qualified Project

eventFromPullRequestPayload :: PullRequestPayload -> Logic.Event
eventFromPullRequestPayload payload =
  let number = Github.number (payload :: PullRequestPayload) -- TODO: Use PullRequestId wrapper from beginning.
      author = Github.author (payload :: PullRequestPayload) -- TODO: Wrapper type
      sha    = Github.sha    (payload :: PullRequestPayload)
  in case Github.action (payload :: PullRequestPayload) of
    Github.Opened      -> Logic.PullRequestOpened (PullRequestId number) sha author
    Github.Reopened    -> Logic.PullRequestOpened (PullRequestId number) sha author
    Github.Closed      -> Logic.PullRequestClosed (PullRequestId number)
    Github.Synchronize -> Logic.PullRequestCommitChanged (PullRequestId number) sha

eventFromCommentPayload :: CommentPayload -> Maybe Logic.Event
eventFromCommentPayload payload =
  let number = Github.number (payload :: CommentPayload) -- TODO: Use PullRequestId wrapper from beginning.
      author = Github.author (payload :: CommentPayload) -- TODO: Wrapper type
      body   = Github.body   (payload :: CommentPayload)
  in case Github.action (payload :: CommentPayload) of
    Github.Created -> Just $ Logic.CommentAdded (PullRequestId number) author body
    -- Do not bother with edited and deleted comments, as it would tremendously
    -- complicate handling of approval. Once approved, this cannot be undone.
    -- And if approval undo is desired, it would be better implemented as a
    -- separate magic comment, rather than editing the approval comment.
    Github.Edited  -> Nothing
    Github.Deleted -> Nothing

mapCommitStatus :: Github.CommitStatus -> Project.BuildStatus
mapCommitStatus status = case status of
  Github.Pending -> Project.BuildPending
  Github.Success -> Project.BuildSucceeded
  Github.Failure -> Project.BuildFailed
  Github.Error   -> Project.BuildFailed

eventFromCommitStatusPayload :: CommitStatusPayload -> Logic.Event
eventFromCommitStatusPayload payload =
  let sha    = Github.sha    (payload :: CommitStatusPayload)
      status = Github.status (payload :: CommitStatusPayload)
  in  Logic.BuildStatusChanged sha (mapCommitStatus status)

convertGithubEvent :: Github.WebhookEvent -> Maybe Logic.Event
convertGithubEvent event = case event of
  Ping                 -> Nothing -- TODO: What to do with this one?
  PullRequest payload  -> Just $ eventFromPullRequestPayload payload
  CommitStatus payload -> Just $ eventFromCommitStatusPayload payload
  Comment payload      -> eventFromCommentPayload payload

-- The event loop that converts GitHub webhook events into logic events.
runGithubEventLoop :: (MonadIO m, MonadLogger m)
                   => Text
                   -> Text
                   -> Github.EventQueue
                   -> (Logic.Event -> IO ()) -> m ()
runGithubEventLoop owner repository ghQueue enqueueEvent = runLoop
  where
    shouldHandle ghEvent =
      (ghEvent /= Ping) &&
      (eventRepository ghEvent == repository) &&
      (eventRepositoryOwner ghEvent == owner)
    runLoop = do
      ghEvent <- liftIO $ atomically $ readTBQueue ghQueue
      logDebugN $ Text.append "github loop received event: " (Text.pack $ show ghEvent)
      -- Listen only to events for the configured repository.
      when (shouldHandle ghEvent) $
        -- If conversion yielded an event, enqueue it. Block if the
        -- queue is full.
        maybe (return ()) (liftIO . enqueueEvent) $ convertGithubEvent ghEvent
      runLoop

runLogicEventLoop :: (MonadIO m, MonadLogger m)
                  => Configuration
                  -> (ProjectState -> m ())
                  -> Logic.EventQueue
                  -> ProjectState
                  -> m ProjectState
runLogicEventLoop config persist queue initialState = runLoop initialState
  where
    repoDir = Config.checkout config
    handleAndContinue state0 event = do
      -- Handle the event and then perform any additional required actions until
      -- the state reaches a fixed point (when there are no further actions to
      -- perform).
      logInfoN  $ Text.append "logic loop received event: " (Text.pack $ show event)
      logDebugN $ Text.append "state before: " (Text.pack $ show state0)
      state1 <- runGit repoDir $ Logic.runAction config $ Logic.handleEvent event state0
      state2 <- runGit repoDir $ Logic.runAction config $ Logic.proceedUntilFixedPoint state1
      persist state2
      logDebugN $ Text.append "state after: " (Text.pack $ show state2)
      runLoop state2
    runLoop state = do
      -- Take one event off the queue, block if there is none.
      eventOrStopSignal <- liftIO $ atomically $ readTBQueue queue
      -- Queue items are of type 'Maybe Event'; 'Nothing' signals loop
      -- termination. If there was an event, run one iteration and recurse.
      case eventOrStopSignal of
        Just event -> handleAndContinue state event
        Nothing    -> return state
