-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DuplicateRecordFields #-}

module GitHub
(
  PullRequestAction (..),
  PullRequestCommentAction (..),
  PullRequestPayload (..),
  PullRequestCommentPayload (..),
  WebhookEvent (..)
)
where

import Data.Text (Text)
import Git (Sha (..))

data PullRequestAction
  = Opened
  | Closed
  | Reopened
  | Synchronize

data PullRequestCommentAction
  = Created
  | Edited
  | Deleted

data PullRequestPayload = PullRequestPayload {
  owner      :: Text, -- Corresponds to "pull_request.base.repo.owner.login".
  repository :: Text, -- Corresponds to "pull_request.base.repo.name".
  number     :: Int,  -- Corresponds to "pull_request.number".
  sha        :: Sha,  -- Corresponds to "pull_request.head.sha".
  author     :: Text  -- Corresponds to "pull_request.user.login".
}

data PullRequestCommentPayload = PullRequestCommentPayload {
  owner      :: Text, -- Corresponds to "repository.owner.login".
  repository :: Text, -- Corresponds to "repository.name".
  number     :: Int,  -- Corresponds to "issue.number".
  author     :: Text, -- Corresponds to "sender.login".
  body       :: Text  -- Corresponds to "comment.body".
}

-- Note that GitHub calls pull requests "issues" for the sake of comments: the
-- pull request comment event is actually "issue_comment".
data WebhookEvent
  = Ping
  | PullRequest PullRequestAction PullRequestPayload
  | PullRequestComment PullRequestCommentAction PullRequestCommentPayload
