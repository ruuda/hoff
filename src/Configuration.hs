-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DeriveGeneric #-}

module Configuration
(
  Configuration (..),
  loadConfiguration
)
where

import Data.Aeson (FromJSON, decodeStrict')
import Data.ByteString (readFile)
import Data.Text (Text)
import GHC.Generics
import Prelude hiding (readFile)

data Configuration = Configuration
  {
    owner      :: Text,     -- The GitHub user or organization who owns the repo.
    repository :: Text,     -- The name of the repository.
    branch     :: Text,     -- The branch to guard and integrate commits into.
    testBranch :: Text,     -- The branch to force-push candidates to for testing.
    checkout   :: FilePath, -- The path to a local checkout of the repository.
    reviewers  :: [Text],   -- List of GitHub usernames that are allowed to approve.
    secret     :: Text,     -- Secret for GitHub webhook hmac signature.
    port       :: Int,      -- The port to listen on for webhooks.
    stateFile  :: FilePath  -- The file where application state is stored.
  }
  deriving (Generic)

instance FromJSON Configuration

-- Reads and parses the configuration. Returns Nothing if parsing failed, but
-- crashes if the file could not be read.
loadConfiguration :: FilePath -> IO (Maybe Configuration)
loadConfiguration = fmap decodeStrict' . readFile
