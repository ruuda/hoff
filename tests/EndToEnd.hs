-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.Async (async, wait)
import Control.Monad (void)
import Data.Text (Text)
import Data.UUID.V4 (nextRandom)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

import Configuration (Configuration (..))
import Git (Sha (..))
import Project (PullRequestId (..))

import qualified Configuration as Config
import qualified Data.Text as Text
import qualified EventLoop
import qualified Git
import qualified Logic

-- Invokes Git with the given arguments, returns its stdout. Crashes if invoking
-- Git failed.
callGit :: [String] -> IO Text
callGit args = fmap (either undefined id) $ Git.callGit args

-- Populates the repository with the following history:
--
--                 .-- c5 -- c6  <-- intro
--                /
--   c0 -- c1 -- c2 -- c3 -- c4  <-- ahead
--                \     ^----------- master
--                 `-- c3'       <-- alternative
--
populateRepository :: FilePath -> IO [Sha]
populateRepository dir =
  let git args            = callGit $ ["-C", dir] ++ args
      gitInit             = void $ git ["init"]
      gitConfig key value = void $ git ["config", key, value]
      gitAdd file         = void $ git ["add", file]
      gitBranch name sha  = void $ git ["checkout", "-b", name, show sha]
      gitCheckout brname  = void $ git ["checkout", brname]
      -- Commits with the given message and returns the sha of the new commit.
      gitCommit message   = do
        void $ git ["commit", "-m", message]
        fmap (Sha . Text.strip) $ git ["rev-parse", "@"]
  in  do
      gitInit
      gitConfig "user.email" "testsuite@example.com"
      gitConfig "user.name" "Testbot"

      writeFile (dir </> "tyrell.txt") "I'm surprised you didn't come here sooner.\n"
      gitAdd "tyrell.txt"
      c0 <- gitCommit "Initial commit"

      writeFile (dir </> "roy.txt") "It's not an easy thing to meet your maker.\n"
      gitAdd "roy.txt"
      c1 <- gitCommit "Add new quote"

      appendFile (dir </> "tyrell.txt") "What can he do for you?\n"
      gitAdd "tyrell.txt"
      c2 <- gitCommit "Add new Tyrell quote"

      appendFile (dir </> "roy.txt") "Can the maker repair what he makes?\n"
      gitAdd "roy.txt"
      c3 <- gitCommit "Add new Roy quote"

      -- Create a branch "ahead", one commit ahead of master.
      gitBranch "ahead" c3
      appendFile (dir </> "tyrell.txt") "Would you like to be modified?\n"
      gitAdd "tyrell.txt"
      c4 <- gitCommit "Add Tyrell  response"
      gitCheckout "master"

      -- Now make an alternative commit that conflicts with c3.
      gitBranch "alternative" c2
      appendFile (dir </> "roy.txt") "You could make me a sandwich.\n"
      gitAdd "roy.txt"
      c3' <- gitCommit "Write alternative ending"

      -- Also add a commit that does not conflict.
      gitBranch "intro" c2
      writeFile (dir </> "leon.txt") "What do you mean, I'm not helping?\n"
      gitAdd "leon.txt"
      c5 <- gitCommit "Add more characters"

      writeFile (dir </> "holden.txt") "I mean, you're not helping! Why is that, Leon?\n"
      gitAdd "holden.txt"
      c6 <- gitCommit "Add response"

      return [c0, c1, c2, c3, c3', c4, c5, c6]

-- Sets up two repositories: one with a few commits in the origin directory, and
-- a clone of that in the repository directory. The clone ensures that the
-- origin repository is set as the "origin" remote in the cloned repository.
initializeRepository :: FilePath -> FilePath -> IO [Sha]
initializeRepository originDir repoDir = do
  -- Create the directory for the origin repository, and parent directories.
  createDirectoryIfMissing True originDir
  shas <- populateRepository originDir
  _    <- callGit ["clone", "file://" ++ originDir, repoDir]
  return shas

-- Generate a configuration to be used in the test environment.
buildConfig :: FilePath -> Configuration
buildConfig repoDir = Configuration {
  Config.owner      = "ruuda",
  Config.repository = "blog",
  Config.branch     = "master",
  Config.testBranch = "integration",
  Config.port       = 5261,
  Config.checkout   = repoDir
}

-- Sets up a test environment with an actual Git repository on the file system,
-- and a thread running the main event loop. Then invokes the body, and tears
-- down the test environment afterwards.
withTestEnv :: ([Sha] -> (Logic.Event -> IO ()) -> IO ()) -> IO ()
withTestEnv body = do
  -- To run these tests, a real repository has to be made somewhere. Do that in
  -- /tmp because it can be mounted as a ramdisk, so it is fast and we don't
  -- unnecessarily wear out SSDs. Put a uuid in there to ensure we don't
  -- overwrite somebody else's files, and to ensure that the tests do not affect
  -- eachother.
  uuid       <- nextRandom
  tmpBaseDir <- getTemporaryDirectory
  let testDir   = tmpBaseDir </> ("testsuite-" ++ (show uuid))
      originDir = testDir </> "repo-origin"
      repoDir   = testDir </> "repo-local"
  -- Create and populate a test repository with a local remote "origin". Record
  -- the shas of the commits as documented in populateRepository.
  shas <- initializeRepository originDir repoDir

  -- Like the actual application, start a new thread to run the main event loop.
  -- Use 'async' here, a higher-level wrapper around 'forkIO', to wait for the
  -- thread to stop later.
  let config = buildConfig repoDir
  queue           <- Logic.newEventQueue 10
  finalStateAsync <- async $ EventLoop.runLogicEventLoop config queue

  -- Run the actual test code inside the environment that we just set up,
  -- provide it with the commit shas and an enqueue function so it can send
  -- events.
  let enqueueEvent = Logic.enqueueEvent queue
  body shas enqueueEvent

  -- Tell the worker thread to stop after it has processed all events. Then wait
  -- for it to exit. Also clean up the test directory.
  Logic.enqueueStopSignal queue
  finalState <- wait finalStateAsync
  removeDirectoryRecursive testDir

main :: IO ()
main = hspec $ do
  describe "The main event loop" $ do

    it "handles a fast-forwardable pull request" $ withTestEnv $ \ shas enqueueEvent -> do
      let [c0, c1, c2, c3, c3', c4, c5, c6] = shas
      -- Commit c4 is one commit ahead of master, so integrating it can be done
      -- with a fast-forward merge.
      enqueueEvent $ Logic.PullRequestOpened (PullRequestId 1) c4 "decker"
      enqueueEvent $ Logic.CommentAdded (PullRequestId 1) "decker" $ Text.pack $ "LGTM " ++ (show c4)
      -- TODO: Validate output repository.
