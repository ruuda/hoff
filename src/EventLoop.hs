-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module EventLoop (runGitHubEventLoop) where

import Control.Concurrent.STM.TBQueue
import Control.Monad.STM (atomically)

import GitHub (PullRequestPayload, PullRequestCommentPayload, WebhookEvent (..))
import Project (PullRequestId (..))

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

runGitHubEventLoop :: GitHub.EventQueue -> IO ()
runGitHubEventLoop ghQueue = do
  ghEvent <- atomically $ readTBQueue ghQueue
  putStrLn $ "received event: " ++ (show $ convertGitHubEvent ghEvent)
  runGitHubEventLoop ghQueue
