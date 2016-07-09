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
  ProjectState,
  PullRequestId (..),
  PullRequestInfo,
  PullRequestState,
  Sha (..),
  loadProjectState,
  saveProjectState,
  exampleState
)
where

import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (readFile)
import Data.ByteString.Lazy (writeFile)
import Data.IntMap (IntMap, fromList)
import Data.Text (Text)
import GHC.Generics
import Prelude hiding (readFile, writeFile)

-- A commit hash is stored as its hexadecimal representation.
data Sha = Sha Text deriving (Show)

-- A pull request is identified by its number.
data PullRequestId = PullRequestId Int deriving (Show, Generic)

data BuildStatus
  = BuildNotStarted
  | BuildQueued
  | BuildInProgress
  | BuildSucceeded
  | BuildFailed
  deriving (Show, Generic)

data PullRequestInfo = PullRequestInfo
  {
    sha    :: Sha,
    author :: Text
  }
  deriving (Show, Generic)

data PullRequestState = PullRequestState
  {
    approvedBy  :: Maybe Text,
    buildStatus :: BuildStatus
  }
  deriving (Show, Generic)

data ProjectState = ProjectState
  {
    pullRequestInfo      :: IntMap PullRequestInfo,
    pullRequestState     :: IntMap PullRequestState,
    integrationCandidate :: Maybe PullRequestId
  }
  deriving (Show, Generic)

instance FromJSON Sha where
  parseJSON (String str) = return (Sha str)
  parseJSON _            = mzero

instance ToJSON Sha where
  toJSON (Sha str) = String str

-- TODO: These default instances produce ugly json. Write a custom
-- implementation. For now this will suffice.
instance FromJSON BuildStatus
instance FromJSON ProjectState
instance FromJSON PullRequestId
instance FromJSON PullRequestInfo
instance FromJSON PullRequestState

instance ToJSON BuildStatus where toEncoding = genericToEncoding defaultOptions
instance ToJSON ProjectState where toEncoding = genericToEncoding defaultOptions
instance ToJSON PullRequestId where toEncoding = genericToEncoding defaultOptions
instance ToJSON PullRequestInfo where toEncoding = genericToEncoding defaultOptions
instance ToJSON PullRequestState where toEncoding = genericToEncoding defaultOptions

-- Reads and parses the state. Returns Nothing if parsing failed, but crashes if
-- the file could not be read.
loadProjectState :: FilePath -> IO (Maybe ProjectState)
loadProjectState = (fmap decodeStrict') . readFile

saveProjectState :: FilePath -> ProjectState -> IO ()
saveProjectState fname state = writeFile fname $ encodePretty state

exampleState :: ProjectState
exampleState = ProjectState {
  pullRequestInfo = fromList [(0, PullRequestInfo { sha = Sha "abc", author = "john" }),
                              (2, PullRequestInfo { sha = Sha "def", author = "dave" })],
  pullRequestState = fromList [(0, PullRequestState { approvedBy = Nothing, buildStatus = BuildNotStarted }),
                               (2, PullRequestState { approvedBy = Just "peter", buildStatus = BuildQueued })],
  integrationCandidate = Just (PullRequestId 2)
}
