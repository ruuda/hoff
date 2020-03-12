-- Hoff -- A gatekeeper for your commits
-- Copyright 2019 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- This module defines high-level Github API operations, plus an interpreter to
-- run those operations against the real API.
module GithubApi
(
  GithubOperationFree (..),
  GithubOperation,
  PullRequestState (..),
  getPullRequestState,
  hasPushAccess,
  leaveComment,
  runGithub,
  runGithubReadOnly,
)
where

import Control.Monad.Free (Free, liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN, logInfoN, logWarnN, logErrorN)
import Data.Text (Text)

import qualified GitHub.Data.Name as Github3
import qualified GitHub.Data.Options as Github3
import qualified GitHub.Endpoints.Issues.Comments as Github3
import qualified GitHub.Endpoints.PullRequests as Github3
import qualified GitHub.Endpoints.Repos.Collaborators as Github3
import qualified GitHub.Request as Github3
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Http

import Format (format)
import Project (ProjectInfo)
import Types (PullRequestId (..), Username (..))

import qualified Project

data PullRequestState
  = StateOpen
  | StateClosed
  | StateUnknown -- When checking GitHub fails.

data GithubOperationFree a
  = LeaveComment PullRequestId Text a
  | HasPushAccess Username (Bool -> a)
  | GetPullRequestState PullRequestId (PullRequestState -> a)
  deriving (Functor)

type GithubOperation = Free GithubOperationFree

leaveComment :: PullRequestId -> Text -> GithubOperation ()
leaveComment pr remoteBranch = liftF $ LeaveComment pr remoteBranch ()

hasPushAccess :: Username -> GithubOperation Bool
hasPushAccess username = liftF $ HasPushAccess username id

getPullRequestState :: PullRequestId -> GithubOperation PullRequestState
getPullRequestState pr = liftF $ GetPullRequestState pr id

isPermissionToPush :: Github3.CollaboratorPermission -> Bool
isPermissionToPush perm = case perm of
  Github3.CollaboratorPermissionAdmin -> True
  Github3.CollaboratorPermissionWrite -> True
  Github3.CollaboratorPermissionRead -> False
  Github3.CollaboratorPermissionNone -> False

pattern StatusCodeException :: Http.Response() -> Github3.Error
pattern StatusCodeException response <-
  Github3.HTTPError (
    Http.HttpExceptionRequest _request (Http.StatusCodeException response _body)
  )

is404NotFound :: Github3.Error -> Bool
is404NotFound err = case err of
  StatusCodeException response -> Http.responseStatus response == Http.notFound404
  _ -> False

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
      result <- liftIO $ Github3.github auth $ Github3.createCommentR
        (Github3.N $ Project.owner projectInfo)
        (Github3.N $ Project.repository projectInfo)
        (Github3.IssueNumber pr)
        body
      case result of
        Left err -> logWarnN $ format "Failed to comment: {}" [show err]
        Right _ -> logInfoN $ format "Posted comment on {}: {}" (pr, body)
      pure cont

    HasPushAccess (Username username) cont -> do
      result <- liftIO $ Github3.github auth $ Github3.collaboratorPermissionOnR
        (Github3.N $ Project.owner projectInfo)
        (Github3.N $ Project.repository projectInfo)
        (Github3.N username)

      case result of
        Left err -> do
          logErrorN $ format "Failed to retrive collaborator status: {}" [show err]
          -- To err on the safe side, if the API call fails, we pretend nobody
          -- has push access.
          pure $ cont False

        Right (Github3.CollaboratorWithPermission _user perm) -> do
          logDebugN $ format "User {} has permission {} on {}." (username, show perm, projectInfo)
          pure $ cont $ isPermissionToPush perm

    GetPullRequestState (PullRequestId pr) cont -> do
      logDebugN $ format "Checking the status of pull request {} in {}." (pr, projectInfo)
      result <- liftIO $ Github3.github auth $ Github3.pullRequestR
        (Github3.N $ Project.owner projectInfo)
        (Github3.N $ Project.repository projectInfo)
        (Github3.IssueNumber pr)
      case result of
        Left err | is404NotFound err -> do
          logWarnN $ format "Pull request {} does not exist in {}, assuming closed." (pr, projectInfo)
          pure $ cont StateClosed
        Left err -> do
          logWarnN $ format "Failed to retrieve pull request {} in {}: {}" (pr, projectInfo, show err)
          pure $ cont StateUnknown
        Right details -> case Github3.pullRequestState details of
          Github3.StateOpen -> pure $ cont StateOpen
          Github3.StateClosed -> pure $ cont StateClosed

-- Like runGithub, but does not execute operations that have side effects, in
-- the sense of being observable by Github users. We will still make requests
-- against the read-only endpoints of the API. This is useful for local testing.
runGithubReadOnly
  :: MonadIO m
  => MonadLogger m
  => Github3.Auth
  -> ProjectInfo
  -> GithubOperationFree a
  -> m a
runGithubReadOnly auth projectInfo operation =
  let
    unsafeResult = runGithub auth projectInfo operation
  in
    case operation of
      -- These operations are read-only, we can run them for real.
      HasPushAccess {} -> unsafeResult
      GetPullRequestState {} -> unsafeResult

      -- These operations have side effects, we fake them.
      LeaveComment pr body cont -> do
        logInfoN $ format "Would have posted comment on {}: {}" (show pr, body)
        pure cont
