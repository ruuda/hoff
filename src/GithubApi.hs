-- Hoff -- A gatekeeper for your commits
-- Copyright 2019 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module GithubApi
(
  GithubOperationFree,
  GithubOperation,
  leaveComment,
)
where

import Control.Monad.Free (Free (..), liftF)
import Data.Text (Text)

import Project (PullRequestId (..))

data GithubOperationFree a
  = LeaveComment PullRequestId Text a
  deriving (Functor)

type GithubOperation = Free GithubOperationFree

leaveComment :: PullRequestId -> Text -> GithubOperation ()
leaveComment pr remoteBranch = liftF $ LeaveComment pr remoteBranch ()
