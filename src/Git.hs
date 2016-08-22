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
  callGit,
  fetchBranch,
  forcePush,
  push,
  rebase,
  runGit,
  tryIntegrate
)
where

import Control.Monad (mzero, when)
import Control.Monad.Free (Free (Free, Pure), liftF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logInfoN, logWarnN)
import Data.Aeson
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text.Format.Params (Params)
import Data.Text.Lazy (toStrict)
import System.Exit (ExitCode (ExitSuccess))
import System.Process.Text (readProcessWithExitCode)

import qualified Data.Text as Text
import qualified Data.Text.Format as Text

-- Conversion function because of Haskell string type madness. This is just
-- Text.format, but returning a strict Text instead of a lazy one.
format :: Params ps => Text.Format -> ps -> Text
format formatString params = toStrict $ Text.format formatString params

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

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

-- Invokes Git with the given arguments. Returns its output on success, or the
-- exit code and stderr on error.
callGit :: (MonadIO m, MonadLogger m) => [String] -> m (Either (ExitCode, Text) Text)
callGit args = do
  let commandText = Text.concat $ intersperse " " $ fmap Text.pack args
      logMessage  = Text.append "executing git " commandText
  logInfoN logMessage
  (exitCode, output, errors) <- liftIO $ readProcessWithExitCode "git" args ""
  if exitCode == ExitSuccess
    then return $ Right output
    else return $ Left (exitCode, errors)

-- Interpreter for the GitOperation free monad that starts Git processes and
-- parses its output.
runGit :: (MonadIO m, MonadLogger m) => FilePath -> GitOperation a -> m a
runGit repoDir operation = case operation of
  Pure result -> return result
  Free (FetchBranch branch cont) -> do
    result <- callGitInRepo ["fetch", "origin", show branch]
    case result of
      Left  _ -> logWarnN "warning: git fetch failed"
      Right _ -> return ()
    continueWith cont
  Free (ForcePush sha branch cont) -> do
    -- TODO: Make Sha and Branch constructors sanitize data, otherwise this
    -- could run unintended Git commands.
    -- Note: the remote branch is prefixed with 'refs/heads/' to specify the
    -- branch unambiguously. This will make Git create the branch if it does
    -- not exist.
    result <- callGitInRepo ["push", "--force", "origin", (show sha) ++ ":refs/heads/" ++ (show branch)]
    case result of
      Left  _ -> logWarnN "warning: git push --force failed"
      Right _ -> return ()
    continueWith cont
  Free (Push sha branch cont) -> do
    result <- callGitInRepo ["push", "origin", (show sha) ++ ":refs/heads/" ++ (show branch)]
    let pushResult = case result of
          Left  _ -> PushRejected
          Right _ -> PushOk
    when (pushResult == PushRejected) $ logInfoN "push was rejected"
    continueWith $ cont pushResult
  Free (Rebase sha branch cont) -> do
    result <- callGitInRepo ["rebase", "origin/" ++ (show branch), show sha]
    case result of
      Left (code, message) -> do
        -- Rebase failed, call the continuation with no rebased sha, but first
        -- abort the rebase.
        -- TODO: Don't spam the log with these, a failed rebase is expected.
        logInfoN $ format "git rebase failed with code {}: {}" (show code, message)
        abortResult <- callGitInRepo ["rebase", "--abort"]
        when (isLeft abortResult) $ logWarnN "warning: git rebase --abort failed"
        continueWith $ cont Nothing
      Right _ -> do
        revResult <- callGitInRepo ["rev-parse", "@"]
        case revResult of
          Left  _   -> do
            logWarnN "warning: git rev-parse failed"
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
