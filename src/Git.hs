-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}

module Git
(
  Branch (..),
  Sha (..),
  fetchBranch,
  forcePush,
  rebase,
  tryIntegrate
)
where

import Control.Monad (mzero)
import Control.Monad.Free (Free, liftF)
import Data.Aeson
import Data.Text (Text)

-- A branch is identified by its name.
data Branch = Branch Text deriving (Eq, Show)

-- A commit hash is stored as its hexadecimal representation.
data Sha = Sha Text deriving (Eq, Show)

instance FromJSON Sha where
  parseJSON (String str) = return (Sha str)
  parseJSON _            = mzero

instance ToJSON Sha where
  toJSON (Sha str) = String str

data GitOperationFree a
  = FetchBranch Branch a
  | ForcePush Sha Branch a
  | Rebase Sha Branch (Maybe Sha -> a)
  deriving (Functor)

type GitOperation = Free GitOperationFree

fetchBranch :: Branch -> GitOperation ()
fetchBranch remoteBranch = liftF $ FetchBranch remoteBranch ()

forcePush :: Sha -> Branch -> GitOperation ()
forcePush sha remoteBranch = liftF $ ForcePush sha remoteBranch ()

rebase :: Sha -> Branch -> GitOperation (Maybe Sha)
rebase sha ontoBranch = liftF $ Rebase sha ontoBranch id

-- Fetches the target branch, rebases the candidate on top of the target branch,
-- and if that was successfull, force-pushses the resulting commits to the test
-- branch.
tryIntegrate :: Sha -> Branch -> Branch -> GitOperation (Maybe Sha)
tryIntegrate candidate targetBranch testBranch = do
  -- Make sure the target branch is up to date. (If later -- after the test
  -- results come in, and we want to push -- it turns out that the target branch
  -- has new commits, then we just restart the cycle.)
  fetchBranch targetBranch
  -- Rebase the candidate commits onto the target branch.
  rebaseResult <- rebase candidate targetBranch
  case rebaseResult of
    -- If the rebase succeeded, then this is our new integration candidate.
    -- Push it to the remote integration branch to trigger a build.
    Just sha -> forcePush sha testBranch >> return (Just sha)
    Nothing  -> return Nothing
