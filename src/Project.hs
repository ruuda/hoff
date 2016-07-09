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
  ProjectState (..),
  PullRequest (..),
  PullRequestId (..),
  Sha (..),
  deletePullRequest,
  existsPullRequest,
  emptyProjectState,
  insertPullRequest,
  loadProjectState,
  lookupPullRequest,
  saveProjectState,
  setApproval,
  updatePullRequest
)
where

import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (readFile)
import Data.ByteString.Lazy (writeFile)
import Data.IntMap (IntMap)
import Data.Text (Text)
import GHC.Generics
import Prelude hiding (readFile, writeFile)

import qualified Data.IntMap as IntMap

-- A commit hash is stored as its hexadecimal representation.
data Sha = Sha Text deriving (Eq, Show)

-- A pull request is identified by its number.
data PullRequestId = PullRequestId Int deriving (Eq, Show, Generic)

data BuildStatus
  = BuildNotStarted
  | BuildQueued
  | BuildInProgress
  | BuildSucceeded
  | BuildFailed
  deriving (Eq, Show, Generic)

data PullRequest = PullRequest
  {
    sha         :: Sha,
    author      :: Text,
    approvedBy  :: Maybe Text,
    buildStatus :: BuildStatus
  }
  deriving (Eq, Show, Generic)

data ProjectState = ProjectState
  {
    pullRequests         :: IntMap PullRequest,
    integrationCandidate :: Maybe PullRequestId
  }
  deriving (Eq, Show, Generic)

instance FromJSON Sha where
  parseJSON (String str) = return (Sha str)
  parseJSON _            = mzero

instance ToJSON Sha where
  toJSON (Sha str) = String str

-- TODO: These default instances produce ugly json. Write a custom
-- implementation. For now this will suffice.
instance FromJSON BuildStatus
instance FromJSON ProjectState
instance FromJSON PullRequest
instance FromJSON PullRequestId

instance ToJSON BuildStatus where toEncoding = genericToEncoding defaultOptions
instance ToJSON ProjectState where toEncoding = genericToEncoding defaultOptions
instance ToJSON PullRequest where toEncoding = genericToEncoding defaultOptions
instance ToJSON PullRequestId where toEncoding = genericToEncoding defaultOptions

-- Reads and parses the state. Returns Nothing if parsing failed, but crashes if
-- the file could not be read.
loadProjectState :: FilePath -> IO (Maybe ProjectState)
loadProjectState = (fmap decodeStrict') . readFile

saveProjectState :: FilePath -> ProjectState -> IO ()
saveProjectState fname state = writeFile fname $ encodePretty state

emptyProjectState :: ProjectState
emptyProjectState = ProjectState {
  pullRequests         = IntMap.empty,
  integrationCandidate = Nothing
}

-- Inserts a new pull request into the project, with approval set to Nothing and
-- build status to BuildNotStarted.
insertPullRequest :: PullRequestId -> Sha -> Text -> ProjectState -> ProjectState
insertPullRequest (PullRequestId n) prSha prAuthor state =
  let pullRequest = PullRequest {
        sha         = prSha,
        author      = prAuthor,
        approvedBy  = Nothing,
        buildStatus = BuildNotStarted
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
