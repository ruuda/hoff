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
  handleEvent,
  proceed,
  proceedUntilFixedPoint,
  tryIntegratePullRequest
)
where

import Control.Exception (assert)
import Control.Monad (mfilter)
import Control.Monad.Free (Free, liftF)
import Data.Maybe (fromJust, fromMaybe, isJust, maybe)
import Data.Text (Text)
import Git (Sha (..))
import Project (BuildStatus (..))
import Project (IntegrationStatus (..))
import Project (ProjectState)
import Project (PullRequest)
import Project (PullRequestId (..))

import qualified Data.Text as Text
import qualified Project as Pr

data PushResult
  = PushOk
  | PushRejected
  deriving (Eq, Show)

data ActionFree a
  = TryIntegrate Sha (Maybe Sha -> a)
  | PushNewHead Sha (PushResult -> a)
  | LeaveComment PullRequestId Text a
  deriving (Functor)

type Action = Free ActionFree

tryIntegrate :: Sha -> Action (Maybe Sha)
tryIntegrate candidate = liftF $ TryIntegrate candidate id

pushNewHead :: Sha -> Action PushResult
pushNewHead newHead = liftF $ PushNewHead newHead id

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

-- Determines if there is anything to do, and if there is, generates the right
-- actions and updates the state accordingly. For example, if the current
-- integration candidate has been integrated (and is no longer a candidate), we
-- should find a new candidate. Or after the pull request for which a build is
-- in progress is closed, we should find a new candidate.
proceed :: ProjectState -> Action ProjectState
proceed state = case Pr.getIntegrationCandidate state of
  -- If there is a candidate, nothing needs to be done. TODO: not even if the
  -- build has finished for the candidate? Or if it has not even been started?
  -- Do I handle that here or in the build status changed event? I think the
  -- answer is "do as much as possible here" because the events are ephemeral,
  -- but the state can be persisted to disk, so the process can continue after a
  -- restart.
  Just candidate -> proceedCandidate candidate state
  -- No current integration candidate, find the next one.
  Nothing -> case Pr.candidatePullRequests state of
    -- No pull requests eligible, do nothing.
    []     -> return state
    -- Found a new candidate, try to integrate it.
    pr : _ -> tryIntegratePullRequest pr state

-- TODO: Get rid of the tuple; just pass the ID and do the lookup with fromJust.
proceedCandidate :: (PullRequestId, PullRequest) -> ProjectState -> Action ProjectState
proceedCandidate (pullRequestId, pullRequest) state =
  case Pr.buildStatus pullRequest of
    BuildNotStarted -> error "integration candidate build should at least be pending"
    BuildPending    -> return state
    BuildSucceeded  -> pushCandidate (pullRequestId, pullRequest) state
    -- If the build failed, this is no longer a candidate.
    -- TODO: Leave a comment on the pull request, perhaps post to chatroom.
    BuildFailed     -> return $ Pr.setIntegrationCandidate Nothing state

-- Integrates proposed changes from the pull request into the target branch.
-- The pull request must exist in the project.
tryIntegratePullRequest :: PullRequestId -> ProjectState -> Action ProjectState
tryIntegratePullRequest pr state = fmap handleResult $ tryIntegrate candidateSha
  where candidateSha = Pr.sha $ fromJust $ Pr.lookupPullRequest pr state
        -- If integrating failed, perform no further actions but do set the
        -- state to conflicted. (TODO: leave a comment on the PR?) If it
        -- succeeded, update the integration candidate, and set the build
        -- to pending, as pushing should have triggered a build.
        handleResult result = case result of
          Nothing  -> Pr.setIntegrationStatus pr Conflicted state
          Just sha -> Pr.setIntegrationStatus pr (Integrated sha)
                    $ Pr.setBuildStatus pr BuildPending
                    $ Pr.setIntegrationCandidate (Just pr) state

-- Pushes the integrated commits of the given candidate pull request to the
-- target branch. If the push fails, restarts the integration cycle for the
-- candidate.
-- TODO: Get rid of the tuple; just pass the ID and do the lookup with fromJust.
pushCandidate :: (PullRequestId, PullRequest) -> ProjectState -> Action ProjectState
pushCandidate (pullRequestId, pullRequest) state = do
  -- Look up the sha that will be pushed to the target branch. Also assert that
  -- the pull request has really been approved and built successfully. If it was
  -- not, there is a bug in the program.
  let approved  = isJust $ Pr.approvedBy pullRequest
      succeeded = Pr.buildStatus pullRequest == BuildSucceeded
      status    = Pr.integrationStatus pullRequest
      newHead   = assert (approved && succeeded) $ case status of
        Integrated sha -> sha
        _              -> error "inconsistent state: build succeeded for non-integrated pull request"
  pushResult <- pushNewHead newHead
  case pushResult of
    -- If the push worked, then this was the final stage of the pull
    -- request; reset the integration candidate.
    -- TODO: Leave a comment? And close the PR via the API.
    PushOk -> return $ Pr.setIntegrationCandidate Nothing state
    -- If something was pushed to the target branch while the candidate was
    -- being tested, try to integrate again and hope that next time the push
    -- succeeds.
    PushRejected -> tryIntegratePullRequest pullRequestId state

-- Keep doing a proceed step until the state doesn't change any more. For this
-- to work properly, it is essential that "proceed" does not have any side
-- effects if it does not change the state.
proceedUntilFixedPoint :: ProjectState -> Action ProjectState
proceedUntilFixedPoint state = do
  newState <- proceed state
  if newState == state
    then return state
    else proceedUntilFixedPoint state
