-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Git
(
  Branch (..),
  GitOperation,
  PushResult (..),
  Sha (..),
  fetchBranch,
  forcePush,
  push,
  rebase,
  runGit,
  tryIntegrate
)
where

import Control.Monad (mzero)
import Control.Monad.Free (Free (Free, Pure), liftF)
import Data.Aeson
import Data.List (intersperse)
import Data.Text (Text)
import System.Exit (ExitCode (ExitSuccess))
import System.Process.Text (readProcessWithExitCode)

import qualified Data.Text as Text

-- A branch is identified by its name.
data Branch = Branch Text deriving (Eq)

-- A commit hash is stored as its hexadecimal representation.
data Sha = Sha Text deriving (Eq)

instance Show Branch where
  show (Branch branch) = Text.unpack branch

instance Show Sha where
  show (Sha sha) = Text.unpack sha

instance FromJSON Sha where
  parseJSON (String str) = return (Sha str)
  parseJSON _            = mzero

instance ToJSON Sha where
  toJSON (Sha str) = String str

data PushResult
  = PushOk
  | PushRejected
  deriving (Eq, Show)

data GitOperationFree a
  = FetchBranch Branch a
  | ForcePush Sha Branch a
  | Push Sha Branch (PushResult -> a)
  | Rebase Sha Branch (Maybe Sha -> a)
  deriving (Functor)

type GitOperation = Free GitOperationFree

fetchBranch :: Branch -> GitOperation ()
fetchBranch remoteBranch = liftF $ FetchBranch remoteBranch ()

forcePush :: Sha -> Branch -> GitOperation ()
forcePush sha remoteBranch = liftF $ ForcePush sha remoteBranch ()

push :: Sha -> Branch -> GitOperation PushResult
push sha remoteBranch = liftF $ Push sha remoteBranch id

rebase :: Sha -> Branch -> GitOperation (Maybe Sha)
rebase sha ontoBranch = liftF $ Rebase sha ontoBranch id

-- Invokes Git with the given arguments. Returns its output on success, or the
-- exit code and stderr on error.
callGit :: [String] -> IO (Either (ExitCode, Text) Text)
callGit args = do
  putStrLn $ "executing git " ++ concat (intersperse " " args)
  (exitCode, output, errors) <- readProcessWithExitCode "git" args ""
  if exitCode == ExitSuccess
    then return $ Right output
    else return $ Left (exitCode, errors)

-- Interpreter for the GitOperation free monad that starts Git processes and
-- parses its output.
runGit :: FilePath -> GitOperation a -> IO a
runGit repoDir operation = case operation of
  Pure result -> return result
  Free (FetchBranch branch cont) -> do
    result <- callGitInRepo ["fetch", "origin", show branch]
    case result of
      Left  _ -> putStrLn "warning: git fetch failed"
      Right _ -> return ()
    continueWith cont
  Free (ForcePush sha branch cont) -> do
    -- TODO: Make Sha and Branch constructors sanitize data, otherwise this
    -- could run unintended Git commands.
    result <- callGitInRepo ["push", "--force", "origin", (show sha) ++ ":" ++ (show branch)]
    case result of
      Left  _ -> putStrLn "warning: git push --force failed"
      Right _ -> return ()
    continueWith cont
  Free (Push sha branch cont) -> do
    result <- callGitInRepo ["push", "origin", (show sha) ++ ":" ++ (show branch)]
    let pushResult = case result of
          Left  _ -> PushRejected
          Right _ -> PushOk
    continueWith $ cont pushResult
  Free (Rebase sha branch cont) -> do
    result <- callGitInRepo ["rebase", show branch, show sha]
    case result of
      -- Rebase failed, call the continuation with no rebased sha.
      Left  _ -> continueWith $ cont Nothing
      Right _ -> do
        revResult <- callGitInRepo ["rev-parse", "@"]
        case revResult of
          Left  _   -> do
            putStrLn "warning: git rev-parse failed"
            continueWith $ cont Nothing
          Right newSha -> continueWith $ cont $ Just $ Sha $ Text.strip newSha
  where
    -- Pass the -C /path/to/checkout option to Git, to run operations in the
    -- repository without having to change the working directory.
    callGitInRepo args = callGit $ ["-C", repoDir] ++ args
    continueWith       = runGit repoDir

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
