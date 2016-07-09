-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Project where

import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import Data.Text (Text)

-- A commit hash is represented by the ASCII encoding of its hexadecimal
-- representation.
data Sha = Sha ByteString deriving (Show)

-- A pull request is identified by its number.
data PullRequest = PullRequest Int deriving (Show)

data BuildStatus
  = BuildNotStarted
  | BuildInProgress
  | BuildSucceeded
  | BuildFailed
  deriving (Show)

data PullRequestInfo = PullRequestInfo
  {
    sha    :: Sha,
    author :: Text
  }
  deriving (Show)

data PullRequestState = PullRequestState
  {
    approvedBy  :: Maybe Text,
    buildStatus :: BuildStatus
  }
  deriving (Show)

data ProjectState = ProjectState
  {
    pullRequestInfo      :: IntMap PullRequestInfo,
    pullRequestStates    :: IntMap PullRequestState,
    integrationCandidate :: Maybe PullRequest
  }
  deriving (Show)
