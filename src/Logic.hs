-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Logic (Action (..), Event (..), handleEvent) where

import Data.Text (Text)
import Project (BuildStatus (..), ProjectState, PullRequestId (..))
import Project (PullRequestInfo (PullRequestInfo))
import Project (PullRequestState (PullRequestState))
import Project (Sha)

import qualified Data.IntMap as IntMap
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
handlePullRequestOpened (PullRequestId pr) sha author state =
  let prInfo  = PullRequestInfo { Pr.sha = sha, Pr.author = author }
      prState = PullRequestState { Pr.approvedBy = Nothing, Pr.buildStatus = BuildNotStarted }
  in state {
    Pr.pullRequestInfo  = IntMap.insert pr prInfo (Pr.pullRequestInfo state),
    Pr.pullRequestState = IntMap.insert pr prState (Pr.pullRequestState state)
  }

handlePullRequestCommitChanged :: PullRequestId -> Sha -> ProjectState -> ProjectState
handlePullRequestCommitChanged pr sha state = state

handlePullRequestClosed :: PullRequestId -> ProjectState -> ProjectState
handlePullRequestClosed pr state = state

handleCommentAdded :: PullRequestId -> Text -> Text -> ProjectState -> ProjectState
handleCommentAdded pr author body state = state

handleBuildStatusChanged :: Sha -> BuildStatus -> ProjectState -> ProjectState
handleBuildStatusChanged sha status state = state
