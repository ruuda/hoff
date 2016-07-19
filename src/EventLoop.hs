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
import Control.Monad.STM (atomically)
import Data.Text (Text)

import Github (PullRequestPayload, PullRequestCommentPayload, WebhookEvent (..))
import Github (eventRepository, eventRepositoryOwner)
import Project (PullRequestId (..), emptyProjectState, saveProjectState)

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

eventFromPullRequestCommentPayload :: PullRequestCommentPayload -> Maybe Logic.Event
eventFromPullRequestCommentPayload payload =
  let number = Github.number (payload :: PullRequestCommentPayload) -- TODO: Use PullRequestId wrapper from beginning.
      author = Github.author (payload :: PullRequestCommentPayload) -- TODO: Wrapper type
      body   = Github.body   (payload :: PullRequestCommentPayload)
  in case Github.action (payload :: PullRequestCommentPayload) of
    Github.Created -> Just $ Logic.CommentAdded (PullRequestId number) author body
    -- Do not bother with edited and deleted comments, as it would tremendously
    -- complicate handling of approval. Once approved, this cannot be undone.
    -- And if approval undo is desired, it would be better implemented as a
    -- separate magic comment, rather than editing the approval comment.
    Github.Edited  -> Nothing
    Github.Deleted -> Nothing

convertGithubEvent :: Github.WebhookEvent -> Maybe Logic.Event
convertGithubEvent event = case event of
  Ping                       -> Nothing -- TODO: What to do with this one?
  PullRequest payload        -> Just $ eventFromPullRequestPayload payload
  PullRequestComment payload -> eventFromPullRequestCommentPayload payload

-- The event loop that converts GitHub webhook events into logic events.
runGithubEventLoop :: Text -> Text -> Github.EventQueue -> Logic.EventQueue -> IO ()
runGithubEventLoop owner repository ghQueue sinkQueue = runLoop
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
        then maybe (return ()) enqueue $ convertGithubEvent ghEvent
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
