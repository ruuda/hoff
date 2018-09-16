-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic
(
  Action,
  ActionFree (..),
  Event (..),
  EventQueue,
  dequeueEvent,
  enqueueEvent,
  enqueueStopSignal,
  ensureCloned,
  handleEvent,
  newEventQueue,
  newStateVar,
  readStateVar,
  runAction,
  proceed,
  proceedUntilFixedPoint,
  tryIntegratePullRequest,
  updateStateVar
)
where

import Control.Concurrent.STM.TMVar (TMVar, newTMVar, readTMVar, swapTMVar)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Control.Exception (assert)
import Control.Monad (mfilter, when, void)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.STM (atomically)
import Data.Maybe (fromJust, fromMaybe, isJust, maybe)
import Data.Text (Text)
import Data.Text.Format.Params (Params)
import Data.Text.Lazy (toStrict)

import qualified Data.Text as Text
import qualified Data.Text.Format as Text

import Configuration (ProjectConfiguration)
import Git (Branch (..), GitOperation, PushResult (..), Sha (..))
import Project (BuildStatus (..))
import Project (IntegrationStatus (..))
import Project (ProjectState)
import Project (PullRequest)
import Project (PullRequestId (..))

import qualified Git
import qualified Project as Pr
import qualified Configuration as Config

-- Conversion function because of Haskell string type madness. This is just
-- Text.format, but returning a strict Text instead of a lazy one.
-- TODO: Extract into utility module and avoid duplication?
format :: Params ps => Text.Format -> ps -> Text
format formatString params = toStrict $ Text.format formatString params

data ActionFree a
  = TryIntegrate (Branch, Sha) (Maybe Sha -> a)
  | PushNewHead Sha (PushResult -> a)
  | LeaveComment PullRequestId Text a
  deriving (Functor)

type Action = Free ActionFree

tryIntegrate :: (Branch, Sha) -> Action (Maybe Sha)
tryIntegrate candidate = liftF $ TryIntegrate candidate id

pushNewHead :: Sha -> Action PushResult
pushNewHead newHead = liftF $ PushNewHead newHead id

-- Interpreter that translates high-level actions into more low-level ones.
runAction :: ProjectConfiguration -> Action a -> GitOperation a
runAction config action =
  let
    continueWith = runAction config
  in case action of
    Pure result -> return result
    Free (TryIntegrate (ref, sha) cont) -> do
      ensureCloned config
      -- TODO: Change types in config to be 'Branch', not 'Text'.
      maybeSha <- Git.tryIntegrate ref sha (Git.Branch $ Config.branch config) (Git.Branch $ Config.testBranch config)
      continueWith $ cont maybeSha
    Free (PushNewHead sha cont) -> do
      ensureCloned config
      pushResult <- Git.push sha (Git.Branch $ Config.branch config)
      continueWith $ cont pushResult
    Free (LeaveComment _pr _body cont) ->
      -- TODO: Implement GitHub API.
      continueWith cont

ensureCloned :: ProjectConfiguration -> GitOperation ()
ensureCloned config =
  let
    url = format "git@github.com:{}/{}.git" (Config.owner config, Config.repository config)
    -- Just a very basic retry, no exponential backoff or anything. Also, the
    -- reason that the clone fails might not be a temporary issue, but still;
    -- retrying is the best thing we could do.
    cloneWithRetry 0 = pure ()
    cloneWithRetry (triesLeft :: Int) = do
      result <- Git.clone (Git.RemoteUrl url)
      case result of
        Git.CloneOk -> pure ()
        Git.CloneFailed -> cloneWithRetry (triesLeft - 1)
  in do
    exists <- Git.doesGitDirectoryExist
    when (not exists) (cloneWithRetry 3)
    pure ()

data Event
  -- GitHub events
  = PullRequestOpened PullRequestId Branch Sha Text Text -- PR, branch, sha, title, author.
  -- The commit changed event may contain false positives: it may be received
  -- even if the commit did not really change. This is because GitHub just
  -- sends a "something changed" event along with the new state.
  | PullRequestCommitChanged PullRequestId Sha -- PR, new sha.
  | PullRequestClosed PullRequestId            -- PR.
  | CommentAdded PullRequestId Text Text       -- PR, author and body.
  -- CI events
  | BuildStatusChanged Sha BuildStatus
  deriving (Eq, Show)

type EventQueue = TBQueue (Maybe Event)
type StateVar = TMVar ProjectState

-- Creates a new event queue with the given maximum capacity.
newEventQueue :: Int -> IO EventQueue
newEventQueue capacity = atomically $ newTBQueue capacity

-- Enqueues an event, blocks if the queue is full.
enqueueEvent :: EventQueue -> Event -> IO ()
enqueueEvent queue event = atomically $ writeTBQueue queue $ Just event

-- Signals the event loop to stop after processing all events
-- currently in the queue.
enqueueStopSignal :: EventQueue -> IO ()
enqueueStopSignal queue = atomically $ writeTBQueue queue Nothing

-- Dequeue an event or stop signal from an event queue.
dequeueEvent :: EventQueue -> IO (Maybe Event)
dequeueEvent queue = atomically $ readTBQueue queue

-- Creates a new project state variable.
newStateVar :: ProjectState -> IO StateVar
newStateVar initialState = atomically $ newTMVar initialState

-- Put a new value in the project state variable, discarding the previous one.
updateStateVar :: StateVar -> ProjectState -> IO ()
updateStateVar var state = void $ atomically $ swapTMVar var state

-- Read the most recent value from the project state variable.
readStateVar :: StateVar -> IO ProjectState
readStateVar var = atomically $ readTMVar var

handleEvent :: ProjectConfiguration -> Event -> ProjectState -> Action ProjectState
handleEvent config event = case event of
  PullRequestOpened pr branch sha title author -> handlePullRequestOpened pr branch sha title author
  PullRequestCommitChanged pr sha -> handlePullRequestCommitChanged pr sha
  PullRequestClosed pr            -> handlePullRequestClosed pr
  CommentAdded pr author body     -> handleCommentAdded config pr author body
  BuildStatusChanged sha status   -> handleBuildStatusChanged sha status

handlePullRequestOpened
  :: PullRequestId
  -> Branch
  -> Sha
  -> Text
  -> Text
  -> ProjectState
  -> Action ProjectState
handlePullRequestOpened pr branch sha title author =
  return . Pr.insertPullRequest pr branch sha title author

handlePullRequestCommitChanged :: PullRequestId -> Sha -> ProjectState -> Action ProjectState
handlePullRequestCommitChanged pr newSha state =
  -- If the commit changes, pretend that the PR was closed. This forgets about
  -- approval and build status. Then pretend a new PR was opened, with the same
  -- author as the original one, but with the new sha.
  let
    closedState = handlePullRequestClosed pr state
    update pullRequest =
      let
        oldSha   = Pr.sha pullRequest
        branch   = Pr.branch pullRequest
        title    = Pr.title pullRequest
        author   = Pr.author pullRequest
        newState = closedState >>= handlePullRequestOpened pr branch newSha title author
      in
        -- If the change notification was a false positive, ignore it.
        if oldSha == newSha then return state else newState
  in
    -- If the pull request was not present in the first place, do nothing.
    maybe (return state) update $ Pr.lookupPullRequest pr state

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
    [stamp, sha] -> (stamp == "LGTM") && (isGood sha)
    _            -> False

isReviewer :: ProjectConfiguration -> Text -> Bool
isReviewer config username = username `elem` (Config.reviewers config)

handleCommentAdded :: ProjectConfiguration
                   -> PullRequestId
                   -> Text -- TODO: Wrapper type for usernames.
                   -> Text
                   -> ProjectState
                   -> Action ProjectState
handleCommentAdded config pr author body = return . Pr.updatePullRequest pr update
  -- If the message was a valid approval stamp for the sha of this pull request,
  -- then it was approved by the author of the stamp. Otherwise do nothing.
  where update pullRequest =
          if (isApproval body $ Pr.sha pullRequest) && (isReviewer config author)
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

-- Given a pull request id, returns the name of the GitHub ref for that pull
-- request, so it can be fetched.
getPullRequestRef :: PullRequestId -> Branch
getPullRequestRef (PullRequestId n) = Branch $ format "refs/pull/{}/head" [n]

-- Integrates proposed changes from the pull request into the target branch.
-- The pull request must exist in the project.
tryIntegratePullRequest :: PullRequestId -> ProjectState -> Action ProjectState
tryIntegratePullRequest pr state = fmap handleResult $ tryIntegrate candidate
  where
    candidateSha = Pr.sha $ fromJust $ Pr.lookupPullRequest pr state
    candidateRef = getPullRequestRef pr
    candidate    = (candidateRef, candidateSha)
    -- If integrating failed, perform no further actions but do set the state
    -- to conflicted. (TODO: leave a comment on the PR?) If it succeeded, update
    -- the integration candidate, and set the build to pending, as pushing
    -- should have triggered a build.
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
    else proceedUntilFixedPoint newState
