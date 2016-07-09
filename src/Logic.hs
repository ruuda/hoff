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

handleEvent :: Event -> ProjectState -> (ProjectState, Action)
handleEvent event = case event of
  PullRequestOpened pr sha author -> handlePullRequestOpened pr sha author
  PullRequestCommitChanged pr sha -> handlePullRequestCommitChanged pr sha
  PullRequestClosed pr            -> handlePullRequestClosed pr
  CommentAdded pr author body     -> handleCommentAdded pr author body
  BuildStatusChanged sha status   -> handleBuildStatusChanged sha status

handlePullRequestOpened :: PullRequestId -> Sha -> Text -> ProjectState -> (ProjectState, Action)
handlePullRequestOpened (PullRequestId pr) sha author state =
  let prInfo  = PullRequestInfo { Pr.sha = sha, Pr.author = author }
      prState = PullRequestState { Pr.approvedBy = Nothing, Pr.buildStatus = BuildNotStarted }
      newState = state {
        Pr.pullRequestInfo  = IntMap.insert pr prInfo (Pr.pullRequestInfo state),
        Pr.pullRequestState = IntMap.insert pr prState (Pr.pullRequestState state)
      }
      action = Nop
  in (newState, action)

handlePullRequestCommitChanged :: PullRequestId -> Sha -> ProjectState -> (ProjectState, Action)
handlePullRequestCommitChanged pr sha state = (state, Nop)

handlePullRequestClosed :: PullRequestId -> ProjectState -> (ProjectState, Action)
handlePullRequestClosed pr state = (state, Nop)

handleCommentAdded :: PullRequestId -> Text -> Text -> ProjectState -> (ProjectState, Action)
handleCommentAdded pr author body state = (state, Nop)

handleBuildStatusChanged :: Sha -> BuildStatus -> ProjectState -> (ProjectState, Action)
handleBuildStatusChanged sha status state = (state, Nop)
