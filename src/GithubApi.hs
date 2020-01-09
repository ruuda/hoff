-- Hoff -- A gatekeeper for your commits
-- Copyright 2019 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- This module defines high-level Github API operations, plus an interpreter to
-- run those operations against the real API.
module GithubApi
(
  GithubOperationFree (..),
  GithubOperation,
  leaveComment,
  hasPushAccess,
  runGithub,
)
where

import Control.Monad.Free (Free, liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logInfoN, logWarnN)
import Data.Text (Text)

import qualified Data.Text as Text

import qualified GitHub.Data.Id as Github3
import qualified GitHub.Data.Name as Github3
import qualified GitHub.Endpoints.Issues.Comments as Github3

import Project (ProjectInfo)
import Types (PullRequestId (..), Username (..))

import qualified Project

data GithubOperationFree a
  = LeaveComment PullRequestId Text a
  | HasPushAccess Username (Bool -> a)
  deriving (Functor)

type GithubOperation = Free GithubOperationFree

leaveComment :: PullRequestId -> Text -> GithubOperation ()
leaveComment pr remoteBranch = liftF $ LeaveComment pr remoteBranch ()

hasPushAccess :: Username -> GithubOperation Bool
hasPushAccess username = liftF $ HasPushAccess username id

runGithub
  :: MonadIO m
  => MonadLogger m
  => Github3.Auth
  -> ProjectInfo
  -> GithubOperationFree a
  -> m a
runGithub auth projectInfo operation =
  case operation of
    LeaveComment (PullRequestId pr) body cont -> do
      result <- liftIO $ Github3.createComment
        auth
        (Github3.N $ Project.owner projectInfo)
        (Github3.N $ Project.repository projectInfo)
        (Github3.Id pr)
        body
      case result of
        Left err -> logWarnN $ Text.append "Failed to comment: " $ Text.pack $ show err
        Right _ -> logInfoN $ Text.concat ["Posted comment on ", Text.pack $ show pr, ": ", body]
      pure cont

    HasPushAccess (Username _username) cont -> do
      logInfoN "TODO: Make actual API call."
      pure $ cont False
