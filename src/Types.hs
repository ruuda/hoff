-- Hoff -- A gatekeeper for your commits
-- Copyright 2020 The Hoff authors
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
(
  PullRequestId (..),
  Username (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson

-- The name of a user on GitHub.
newtype Username = Username Text deriving (Eq, Show, Generic, IsString)

-- A pull request is identified by its number.
newtype PullRequestId = PullRequestId Int deriving (Eq, Ord, Show, Generic)

instance FromJSON PullRequestId
instance FromJSON Username

instance ToJSON PullRequestId where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToJSON Username where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
