-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Github
(
  EventQueue,
  PullRequestAction (..),
  PullRequestCommentAction (..),
  PullRequestPayload (..),
  PullRequestCommentPayload (..),
  WebhookEvent (..),
  eventRepository,
  eventRepositoryOwner,
  newEventQueue
)
where

import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue)
import Control.Monad (mzero)
import Control.Monad.STM (atomically)
import Data.Aeson (FromJSON (parseJSON), Object, Value (Object, String), (.:))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Git (Sha (..))

data PullRequestAction
  = Opened
  | Closed
  | Reopened
  | Synchronize
  deriving (Eq, Show)

data PullRequestCommentAction
  = Created
  | Edited
  | Deleted
  deriving (Eq, Show)

data PullRequestPayload = PullRequestPayload {
  action     :: PullRequestAction, -- Corresponds to "action".
  owner      :: Text, -- Corresponds to "pull_request.base.repo.owner.login".
  repository :: Text, -- Corresponds to "pull_request.base.repo.name".
  number     :: Int,  -- Corresponds to "pull_request.number".
  sha        :: Sha,  -- Corresponds to "pull_request.head.sha".
  author     :: Text  -- Corresponds to "pull_request.user.login".
} deriving (Eq, Show)

data PullRequestCommentPayload = PullRequestCommentPayload {
  action     :: PullRequestCommentAction, -- Corresponds to "action".
  owner      :: Text, -- Corresponds to "repository.owner.login".
  repository :: Text, -- Corresponds to "repository.name".
  number     :: Int,  -- Corresponds to "issue.number".
  author     :: Text, -- Corresponds to "sender.login".
  body       :: Text  -- Corresponds to "comment.body".
} deriving (Eq, Show)

instance FromJSON PullRequestAction where
  parseJSON (String "opened")      = return Opened
  parseJSON (String "closed")      = return Closed
  parseJSON (String "reopened")    = return Opened
  parseJSON (String "synchronize") = return Synchronize
  parseJSON _                      = mzero

-- A helper function to parse nested fields in json.
getNested :: FromJSON a => Object -> [Text] -> Parser a
getNested rootObject fields =
  -- Build object parsers for every field except the last one. The last field is
  -- different, as it needs a parser of type "a", not "Object".
  let parsers :: [Object -> Parser Object]
      parsers = fmap (\ field -> (.: field)) (init fields)
      object  = foldl (>>=) (return rootObject) parsers
  in  object >>= (.: (last fields))

instance FromJSON PullRequestPayload where
  parseJSON (Object v) = PullRequestPayload
    <$> (v .: "action")
    <*> getNested v ["pull_request", "base", "repo", "owner", "login"]
    <*> getNested v ["pull_request", "base", "repo", "name"]
    <*> getNested v ["pull_request", "number"]
    <*> getNested v ["pull_request", "head", "sha"]
    <*> getNested v ["pull_request", "user", "login"]
  parseJSON _ = mzero

-- Note that GitHub calls pull requests "issues" for the sake of comments: the
-- pull request comment event is actually "issue_comment".
data WebhookEvent
  = Ping
  | PullRequest PullRequestPayload
  | PullRequestComment PullRequestCommentPayload
  deriving (Eq, Show)

-- Returns the owner of the repository for which the webhook was triggered.
eventRepositoryOwner :: WebhookEvent -> Text
eventRepositoryOwner event = case event of
  Ping -> "" -- TODO: Does the ping event have a owner/repository payload?
  PullRequest payload        -> owner (payload :: PullRequestPayload)
  PullRequestComment payload -> owner (payload :: PullRequestCommentPayload)

-- Returns the name of the repository for which the webhook was triggered.
eventRepository :: WebhookEvent -> Text
eventRepository event = case event of
  Ping -> "" -- TODO: Does the ping event have a owner/repository payload?
  PullRequest payload        -> repository (payload :: PullRequestPayload)
  PullRequestComment payload -> repository (payload :: PullRequestCommentPayload)

type EventQueue = TBQueue WebhookEvent

-- Creates a new event queue with the given maximum capacity.
newEventQueue :: Int -> IO EventQueue
newEventQueue capacity = atomically $ newTBQueue capacity
