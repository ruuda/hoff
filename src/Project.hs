-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Project
(
  BuildStatus (..),
  IntegrationStatus (..),
  ProjectState (..),
  PullRequest (..),
  PullRequestId (..),
  candidatePullRequests,
  deletePullRequest,
  existsPullRequest,
  emptyProjectState,
  getIntegrationCandidate,
  insertPullRequest,
  loadProjectState,
  lookupPullRequest,
  saveProjectState,
  setApproval,
  setBuildStatus,
  setIntegrationCandidate,
  setIntegrationStatus,
  updatePullRequest
)
where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (readFile)
import Data.ByteString.Lazy (writeFile)
import Data.IntMap (IntMap)
import Data.List (intersect)
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Generics
import Git (Sha (..))
import Prelude hiding (readFile, writeFile)

import qualified Data.IntMap as IntMap

-- A pull request is identified by its number.
data PullRequestId = PullRequestId Int deriving (Eq, Show, Generic)

data BuildStatus
  = BuildNotStarted
  | BuildPending
  | BuildSucceeded
  | BuildFailed
  deriving (Eq, Show, Generic)

-- When attempting to integrated changes, there can be three states: no attempt
-- has been made to integrate; integration (e.g. merge or rebase) was successful
-- and the new commit has the given sha; and an attempt to integrate was made,
-- but it resulted in merge conflicts.
data IntegrationStatus
  = NotIntegrated
  | Integrated Sha
  | Conflicted
  deriving (Eq, Show, Generic)

data PullRequest = PullRequest
  {
    sha               :: Sha,
    author            :: Text,
    approvedBy        :: Maybe Text,
    buildStatus       :: BuildStatus,
    integrationStatus :: IntegrationStatus
  }
  deriving (Eq, Show, Generic)

data ProjectState = ProjectState
  {
    pullRequests         :: IntMap PullRequest,
    integrationCandidate :: Maybe PullRequestId
  }
  deriving (Eq, Show, Generic)

-- TODO: These default instances produce ugly json. Write a custom
-- implementation. For now this will suffice.
instance FromJSON BuildStatus
instance FromJSON IntegrationStatus
instance FromJSON ProjectState
instance FromJSON PullRequest
instance FromJSON PullRequestId

instance ToJSON BuildStatus where toEncoding = genericToEncoding defaultOptions
instance ToJSON IntegrationStatus where toEncoding = genericToEncoding defaultOptions
instance ToJSON ProjectState where toEncoding = genericToEncoding defaultOptions
instance ToJSON PullRequest where toEncoding = genericToEncoding defaultOptions
instance ToJSON PullRequestId where toEncoding = genericToEncoding defaultOptions

-- Reads and parses the state. Returns Nothing if parsing failed, but crashes if
-- the file could not be read.
loadProjectState :: FilePath -> IO (Maybe ProjectState)
loadProjectState = fmap decodeStrict' . readFile

saveProjectState :: FilePath -> ProjectState -> IO ()
saveProjectState fname state = writeFile fname $ encodePretty state

emptyProjectState :: ProjectState
emptyProjectState = ProjectState {
  pullRequests         = IntMap.empty,
  integrationCandidate = Nothing
}

-- Inserts a new pull request into the project, with approval set to Nothing,
-- build status to BuildNotStarted, and integration status to NotIntegrated.
insertPullRequest :: PullRequestId -> Sha -> Text -> ProjectState -> ProjectState
insertPullRequest (PullRequestId n) prSha prAuthor state =
  let pullRequest = PullRequest {
        sha               = prSha,
        author            = prAuthor,
        approvedBy        = Nothing,
        buildStatus       = BuildNotStarted,
        integrationStatus = NotIntegrated
      }
  in state { pullRequests = IntMap.insert n pullRequest $ pullRequests state }

-- Removes the pull request detail from the project. This does not change the
-- integration candidate, which can be equal to the deleted pull request.
deletePullRequest :: PullRequestId -> ProjectState -> ProjectState
deletePullRequest (PullRequestId n) state = state {
  pullRequests = IntMap.delete n $ pullRequests state
}

-- Returns whether the pull request is part of the set of open pull requests.
existsPullRequest :: PullRequestId -> ProjectState -> Bool
existsPullRequest (PullRequestId n) = IntMap.member n . pullRequests

lookupPullRequest :: PullRequestId -> ProjectState -> Maybe PullRequest
lookupPullRequest (PullRequestId n) = IntMap.lookup n . pullRequests

updatePullRequest :: PullRequestId -> (PullRequest -> PullRequest) -> ProjectState -> ProjectState
updatePullRequest (PullRequestId n) f state = state {
  pullRequests = IntMap.adjust f n $ pullRequests state
}

-- Marks the pull request as approved by somebody or nobody.
setApproval :: PullRequestId -> Maybe Text -> ProjectState -> ProjectState
setApproval pr newApprovedBy = updatePullRequest pr changeApproval
  where changeApproval pullRequest = pullRequest { approvedBy = newApprovedBy }

-- Sets the build status for a pull request.
setBuildStatus :: PullRequestId -> BuildStatus -> ProjectState -> ProjectState
setBuildStatus pr newStatus = updatePullRequest pr changeBuildStatus
  where changeBuildStatus pullRequest = pullRequest { buildStatus = newStatus }

-- Sets the integration status for a pull request.
setIntegrationStatus :: PullRequestId -> IntegrationStatus -> ProjectState -> ProjectState
setIntegrationStatus pr newStatus = updatePullRequest pr changeIntegrationStatus
  where changeIntegrationStatus pullRequest = pullRequest { integrationStatus = newStatus }

getIntegrationCandidate :: ProjectState -> Maybe (PullRequestId, PullRequest)
getIntegrationCandidate state = do
  pullRequestId <- integrationCandidate state
  candidate     <- lookupPullRequest pullRequestId state
  return (pullRequestId, candidate)

setIntegrationCandidate :: Maybe PullRequestId -> ProjectState  -> ProjectState
setIntegrationCandidate pr state = state {
  integrationCandidate = pr
}

-- Returns the pull requests that have been approved, in order of ascending id.
approvedPullRequests :: ProjectState -> [PullRequestId]
approvedPullRequests =
  fmap PullRequestId . IntMap.keys . IntMap.filter approved . pullRequests
  where approved = isJust . approvedBy

-- Returns the pull requests that have not been integrated yet, in order of
-- ascending id.
unintegratedPullRequests :: ProjectState -> [PullRequestId]
unintegratedPullRequests =
  fmap PullRequestId . IntMap.keys . IntMap.filter notIntegrated . pullRequests
  where notIntegrated = (== NotIntegrated) . integrationStatus

-- Returns the pull requests that have not been built yet, in order of ascending
-- id.
unbuiltPullRequests :: ProjectState -> [PullRequestId]
unbuiltPullRequests =
  fmap PullRequestId . IntMap.keys . IntMap.filter notBuilt . pullRequests
  where notBuilt = (== BuildNotStarted) . buildStatus

-- Returns the pull requests that have been approved, but for which integration
-- and building has not yet been attempted.
candidatePullRequests :: ProjectState -> [PullRequestId]
candidatePullRequests state =
  let approved     = approvedPullRequests state
      unintegrated = unintegratedPullRequests state
      unbuilt      = unbuiltPullRequests state
  in  approved `intersect` unintegrated `intersect` unbuilt
