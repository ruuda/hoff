-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Logic (Action (..), Event (..), handleEvent) where

import Control.Monad (mfilter)
import Data.Maybe (maybe)
import Data.Text (Text)
import Project (BuildStatus (..))
import Project (ProjectState)
import Project (PullRequestId (..))
import Project (Sha)

import qualified Project as Pr

data Event
  -- GitHub events
  = PullRequestOpened PullRequestId Sha Text   -- PR, sha, author.
  | PullRequestCommitChanged PullRequestId Sha -- PR, new sha.
  | PullRequestClosed PullRequestId            -- PR.
  | CommentAdded PullRequestId Text Text       -- PR, author and body.
  -- CI events
  | BuildStatusChanged Sha BuildStatus

data Action = Nop

handleEvent :: Event -> ProjectState -> ProjectState
handleEvent event = case event of
  PullRequestOpened pr sha author -> handlePullRequestOpened pr sha author
  PullRequestCommitChanged pr sha -> handlePullRequestCommitChanged pr sha
  PullRequestClosed pr            -> handlePullRequestClosed pr
  CommentAdded pr author body     -> handleCommentAdded pr author body
  BuildStatusChanged sha status   -> handleBuildStatusChanged sha status

handlePullRequestOpened :: PullRequestId -> Sha -> Text -> ProjectState -> ProjectState
handlePullRequestOpened pr sha author = Pr.insertPullRequest pr sha author

handlePullRequestCommitChanged :: PullRequestId -> Sha -> ProjectState -> ProjectState
handlePullRequestCommitChanged pr sha state =
  -- If the commit changes, pretend that the PR was closed. This forgets about
  -- approval and build status. Then pretend a new PR was opened, with the same
  -- author as the original one, but with the new sha.
  let closedState = handlePullRequestClosed pr state
      update pullRequest =
        let author = Pr.author pullRequest
        in  handlePullRequestOpened pr sha author closedState
  -- If the pull request was not present in the first place, do nothing.
  in maybe state update $ Pr.lookupPullRequest pr state

handlePullRequestClosed :: PullRequestId -> ProjectState -> ProjectState
handlePullRequestClosed pr state = Pr.deletePullRequest pr state {
  -- If the PR was the current integration candidate, reset that to Nothing.
  Pr.integrationCandidate = mfilter (/= pr) $ Pr.integrationCandidate state
}

handleCommentAdded :: PullRequestId -> Text -> Text -> ProjectState -> ProjectState
handleCommentAdded pr author body state = state

handleBuildStatusChanged :: Sha -> BuildStatus -> ProjectState -> ProjectState
handleBuildStatusChanged sha status state = state
