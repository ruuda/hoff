-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module EventLoop
(
  runGithubEventLoop,
  runLogicEventLoop
)
where

import Control.Concurrent.STM.TBQueue
import Control.Monad (when)
import Control.Monad.STM (atomically)
import Data.Text (Text)

import Git (runGit)
import Configuration (Configuration)
import Github (PullRequestPayload, CommentPayload, WebhookEvent (..))
import Github (eventRepository, eventRepositoryOwner)
import Project (ProjectState, PullRequestId (..), emptyProjectState, saveProjectState)

import qualified Configuration as Config
import qualified Github
import qualified Logic

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

convertGithubEvent :: Github.WebhookEvent -> Maybe Logic.Event
convertGithubEvent event = case event of
  Ping                 -> Nothing -- TODO: What to do with this one?
  PullRequest payload  -> Just $ eventFromPullRequestPayload payload
  Comment payload      -> eventFromCommentPayload payload
  CommitStatus _pload  -> Nothing -- TODO: Handle this one.

-- The event loop that converts GitHub webhook events into logic events.
runGithubEventLoop :: Text -> Text -> Github.EventQueue -> (Logic.Event -> IO ()) -> IO ()
runGithubEventLoop owner repository ghQueue enqueueEvent = runLoop
  where
    shouldHandle ghEvent =
      (ghEvent /= Ping) &&
      (eventRepository ghEvent == repository) &&
      (eventRepositoryOwner ghEvent == owner)
    runLoop = do
      ghEvent <- atomically $ readTBQueue ghQueue
      putStrLn $ "github loop received event: " ++ (show ghEvent)
      -- Listen only to events for the configured repository.
      when (shouldHandle ghEvent) $
        -- If conversion yielded an event, enqueue it. Block if the
        -- queue is full.
        maybe (return ()) enqueueEvent $ convertGithubEvent ghEvent
      runLoop

runLogicEventLoop :: Configuration -> Logic.EventQueue -> IO ProjectState
runLogicEventLoop config queue = runLoop emptyProjectState -- TODO: Load previous state from disk?
  where
    repoDir = Config.checkout config
    handleAndContinue state0 event = do
      -- Handle the event and then perform any additional required actions until
      -- the state reaches a fixed point (when there are no further actions to
      -- perform).
      putStrLn $ "logic loop received event: " ++ (show event)
      putStrLn $ "state before: " ++ (show state0)
      state1 <- runGit repoDir $ Logic.runAction config $ Logic.handleEvent event state0
      state2 <- runGit repoDir $ Logic.runAction config $ Logic.proceedUntilFixedPoint state1
      saveProjectState "project.json" state2
      putStrLn $ "state after: " ++ (show state2)
      runLoop state2
    runLoop state = do
      -- Take one event off the queue, block if there is none.
      eventOrStopSignal <- atomically $ readTBQueue queue
      -- Queue items are of type 'Maybe Event'; 'Nothing' signals loop
      -- termination. If there was an event, run one iteration and recurse.
      case eventOrStopSignal of
        Just event -> handleAndContinue state event
        Nothing    -> return state
