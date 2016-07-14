-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic
(
  Action,
  ActionFree (..),
  Event (..),
  PushResult (..),
  handleEvent
) where

import Control.Monad (mfilter)
import Control.Monad.Free (Free)
import Data.Maybe (fromMaybe, maybe)
import Data.Text (Text)
import Project (Branch (..))
import Project (BuildStatus (..))
import Project (IntegrationStatus (..))
import Project (ProjectState)
import Project (PullRequestId (..))
import Project (Sha (..))

import qualified Data.Text as Text
import qualified Project as Pr

data PushResult
  = PushOk
  | PushRejected
  deriving (Eq, Show)

data ActionFree a
  = FetchCommit Sha a
  | FetchBranch Branch a
  | ForcePush Sha Branch a
  | Push Sha Branch (PushResult -> a)
  | Rebase Sha Branch (Maybe Sha -> a)
  | LeaveComment PullRequestId Text a
  deriving (Functor)

type Action = Free ActionFree

data Event
  -- GitHub events
  = PullRequestOpened PullRequestId Sha Text   -- PR, sha, author.
  | PullRequestCommitChanged PullRequestId Sha -- PR, new sha.
  | PullRequestClosed PullRequestId            -- PR.
  | CommentAdded PullRequestId Text Text       -- PR, author and body.
  -- CI events
  | BuildStatusChanged Sha BuildStatus

handleEvent :: Event -> ProjectState -> Action ProjectState
handleEvent event = case event of
  PullRequestOpened pr sha author -> handlePullRequestOpened pr sha author
  PullRequestCommitChanged pr sha -> handlePullRequestCommitChanged pr sha
  PullRequestClosed pr            -> handlePullRequestClosed pr
  CommentAdded pr author body     -> handleCommentAdded pr author body
  BuildStatusChanged sha status   -> handleBuildStatusChanged sha status

handlePullRequestOpened :: PullRequestId -> Sha -> Text -> ProjectState -> Action ProjectState
handlePullRequestOpened pr sha author = return . Pr.insertPullRequest pr sha author

handlePullRequestCommitChanged :: PullRequestId -> Sha -> ProjectState -> Action ProjectState
handlePullRequestCommitChanged pr sha state =
  -- If the commit changes, pretend that the PR was closed. This forgets about
  -- approval and build status. Then pretend a new PR was opened, with the same
  -- author as the original one, but with the new sha.
  let closedState = handlePullRequestClosed pr state
      update pullRequest =
        let author = Pr.author pullRequest
        in  closedState >>= handlePullRequestOpened pr sha author
  -- If the pull request was not present in the first place, do nothing.
  in maybe (return state) update $ Pr.lookupPullRequest pr state

handlePullRequestClosed :: PullRequestId -> ProjectState -> Action ProjectState
handlePullRequestClosed pr state = return $ Pr.deletePullRequest pr state {
  -- If the PR was the current integration candidate, reset that to Nothing.
  Pr.integrationCandidate = mfilter (/= pr) $ Pr.integrationCandidate state
}

-- Returns whether the message is an approval stamp for the given commit. The
-- message must have a format like "LGTM 3b77e3f", where the SHA must be a
-- prefix of the SHA to be tested, and it must be at least 7 characters long.
isApproval :: Text -> Sha -> Bool
isApproval message (Sha target) =
  let isGood sha = (Text.length sha >= 7) && (sha `Text.isPrefixOf` target)
  in case Text.words message of
    stamp : sha : [] -> (stamp == "LGTM") && (isGood sha)
    _                -> False

handleCommentAdded :: PullRequestId -> Text -> Text -> ProjectState -> Action ProjectState
handleCommentAdded pr author body = return . Pr.updatePullRequest pr update
  -- If the message was a valid approval stamp for the sha of this pull request,
  -- then it was approved by the author of the stamp. Otherwise do nothing.
  where update pullRequest =
          if isApproval body $ Pr.sha pullRequest
            then pullRequest { Pr.approvedBy = Just author }
            else pullRequest

handleBuildStatusChanged :: Sha -> BuildStatus -> ProjectState -> Action ProjectState
handleBuildStatusChanged buildSha newStatus state =
  -- If there is an integration candidate, and its integration sha matches that
  -- of the build, then update the build status for that pull request. Otherwise
  -- do nothing.
  let matchesBuild pr = case Pr.integrationStatus pr of
        Integrated candidateSha -> candidateSha == buildSha
        _                       -> False
      newState = do
        candidateId <- Pr.integrationCandidate state
        -- Set the build status only if the build sha matches that of the
        -- integration candidate.
        _ <- mfilter matchesBuild $ Pr.lookupPullRequest candidateId state
        return $ Pr.setBuildStatus candidateId newStatus state
  in return $ fromMaybe state newState
