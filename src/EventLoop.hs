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
import Project (PullRequestId (..))

import qualified GitHub
import qualified Logic

eventFromPullRequestPayload :: GitHub.PullRequestPayload -> Logic.Event
eventFromPullRequestPayload payload =
  let number = GitHub.number (payload :: GitHub.PullRequestPayload) -- TODO: Use PullRequestId wrapper from beginning.
      author = GitHub.author (payload :: GitHub.PullRequestPayload) -- TODO: Wrapper type
      sha    = GitHub.sha    (payload :: GitHub.PullRequestPayload)
  in case GitHub.action (payload :: GitHub.PullRequestPayload) of
    GitHub.Opened      -> Logic.PullRequestOpened (PullRequestId number) sha author
    GitHub.Reopened    -> Logic.PullRequestOpened (PullRequestId number) sha author
    GitHub.Closed      -> Logic.PullRequestClosed (PullRequestId number)
    GitHub.Synchronize -> Logic.PullRequestCommitChanged (PullRequestId number) sha

eventFromPullRequestCommentPayload :: GitHub.PullRequestCommentPayload -> Logic.Event
eventFromPullRequestCommentPayload payload = undefined

convertGitHubEvent :: GitHub.WebhookEvent -> Maybe Logic.Event
convertGitHubEvent event = case event of
  GitHub.Ping                       -> Nothing -- TODO: What to do with this one?
  GitHub.PullRequest payload        -> Just $ eventFromPullRequestPayload payload
  GitHub.PullRequestComment payload -> Just $ eventFromPullRequestCommentPayload payload

runGitHubEventLoop :: GitHub.EventQueue -> IO ()
runGitHubEventLoop ghQueue = do
  ghEvent <- atomically $ readTBQueue ghQueue
  putStrLn $ "received event: " ++ (show $ convertGitHubEvent ghEvent)
  runGitHubEventLoop ghQueue
