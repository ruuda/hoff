-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DeriveGeneric #-}

module Configuration (Configuration, loadConfiguration) where

import Data.Aeson (FromJSON, decodeStrict')
import Data.ByteString (readFile)
import Data.Text (Text)
import GHC.Generics
import Prelude hiding (readFile)

data Configuration = Configuration
  {
    owner      :: Text,    -- The GitHub user or organization who owns the repo.
    repository :: Text,    -- The name of the repository.
    branch     :: Text,    -- The branch to guard and integrate commits into.
    port       :: Int,     -- The port to listen on for webhooks.
    checkout   :: FilePath -- The path to a local checkout of the repository.
  }
  deriving (Generic, Show)

instance FromJSON Configuration

-- Reads and parses the configuration. Returns Nothing if parsing failed, but
-- crashes if the file could not be read.
loadConfiguration :: FilePath -> IO (Maybe Configuration)
loadConfiguration = (fmap decodeStrict') . readFile
