-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module EventLoop
(
  runGitHubEventLoop,
  runLogicEventLoop
)
where

import Control.Concurrent.STM.TBQueue
import Control.Monad.STM (atomically)
import Data.Text (Text)

import GitHub (PullRequestPayload, PullRequestCommentPayload, WebhookEvent (..))
import GitHub (eventRepository, eventRepositoryOwner)
import Project (PullRequestId (..), emptyProjectState, saveProjectState)

import qualified GitHub
import qualified Logic

eventFromPullRequestPayload :: PullRequestPayload -> Logic.Event
eventFromPullRequestPayload payload =
  let number = GitHub.number (payload :: PullRequestPayload) -- TODO: Use PullRequestId wrapper from beginning.
      author = GitHub.author (payload :: PullRequestPayload) -- TODO: Wrapper type
      sha    = GitHub.sha    (payload :: PullRequestPayload)
  in case GitHub.action (payload :: PullRequestPayload) of
    GitHub.Opened      -> Logic.PullRequestOpened (PullRequestId number) sha author
    GitHub.Reopened    -> Logic.PullRequestOpened (PullRequestId number) sha author
    GitHub.Closed      -> Logic.PullRequestClosed (PullRequestId number)
    GitHub.Synchronize -> Logic.PullRequestCommitChanged (PullRequestId number) sha

eventFromPullRequestCommentPayload :: PullRequestCommentPayload -> Maybe Logic.Event
eventFromPullRequestCommentPayload payload =
  let number = GitHub.number (payload :: PullRequestCommentPayload) -- TODO: Use PullRequestId wrapper from beginning.
      author = GitHub.author (payload :: PullRequestCommentPayload) -- TODO: Wrapper type
      body   = GitHub.body   (payload :: PullRequestCommentPayload)
  in case GitHub.action (payload :: PullRequestCommentPayload) of
    GitHub.Created -> Just $ Logic.CommentAdded (PullRequestId number) author body
    -- Do not bother with edited and deleted comments, as it would tremendously
    -- complicate handling of approval. Once approved, this cannot be undone.
    -- And if approval undo is desired, it would be better implemented as a
    -- separate magic comment, rather than editing the approval comment.
    GitHub.Edited  -> Nothing
    GitHub.Deleted -> Nothing

convertGitHubEvent :: GitHub.WebhookEvent -> Maybe Logic.Event
convertGitHubEvent event = case event of
  Ping                       -> Nothing -- TODO: What to do with this one?
  PullRequest payload        -> Just $ eventFromPullRequestPayload payload
  PullRequestComment payload -> eventFromPullRequestCommentPayload payload

-- The event loop that converts GitHub webhook events into logic events.
runGitHubEventLoop :: Text -> Text -> GitHub.EventQueue -> Logic.EventQueue -> IO ()
runGitHubEventLoop owner repository ghQueue sinkQueue = runLoop
  where
    shouldHandle ghEvent =
      (eventRepository ghEvent == repository) &&
      (eventRepositoryOwner ghEvent == owner)
    -- Enqueues an event, blocks if the queue is full.
    enqueue event = atomically $ writeTBQueue sinkQueue event
    runLoop = do
      ghEvent <- atomically $ readTBQueue ghQueue
      putStrLn $ "github loop received event: " ++ (show ghEvent)
      -- Listen only to events for the configured repository.
      if shouldHandle ghEvent
        -- If conversion yielded an event, enqueue it.
        then maybe (return ()) enqueue $ convertGitHubEvent ghEvent
        else return ()
      runLoop

runLogicEventLoop :: Logic.EventQueue -> IO ()
runLogicEventLoop queue = runLoop emptyProjectState -- TODO: Load previous state from disk?
  where
    runLoop state0 = do
      -- Take one event off the queue (block if there is none), handle it, and
      -- then perform any additional required actions until the state reaches a
      -- fixed point (when there are no further actions to perform).
      event  <- atomically $ readTBQueue queue
      putStrLn $ "logic loop received event: " ++ (show event)
      putStrLn $ "state before: " ++ (show state0)
      state1 <- Logic.runAction $ Logic.handleEvent event state0
      state2 <- Logic.runAction $ Logic.proceedUntilFixedPoint state1
      saveProjectState "project.json" state2
      putStrLn $ "state after: " ++ (show state2)
      runLoop state2
