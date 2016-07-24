-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Control.Monad (void)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
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

-- To run these tests, a real repository has to be made somewhere. Do that in
-- /tmp because it can be mounted as a ramdisk, so it is fast and we don't
-- unnecessarily wear out SSDs. Put a guid in there to ensure we don't overwrite
-- somebody else's files.
testDir :: FilePath
testDir = "/tmp/testsuite-389614a8-edeb-4993-978b-425cda85a090"

repoDir :: FilePath
repoDir = testDir </> "repo"

originDir :: FilePath
originDir = testDir </> "repo-remote"

config :: Configuration
config = Configuration {
  Config.owner      = "ruuda",
  Config.repository = "blog",
  Config.branch     = "master",
  Config.testBranch = "integration",
  Config.port       = 5261,
  Config.checkout   = testDir
}

-- Invokes Git with the given arguments, returns its stdout. Crashes if invoking
-- Git failed.
callGit :: [String] -> IO Text
callGit args = fmap (either undefined id) $ Git.callGit args

-- Populates the repository with the following history:
--
--                 .-- c4 -- c5  <-- intro
--                /
--   c0 -- c1 -- c2 -- c3        <-- master
--                \
--                 `-- c3'       <-- alternative
--
populateRepository :: FilePath -> IO [Sha]
populateRepository dir =
  let git args            = callGit $ ["-C", dir] ++ args
      gitInit             = void $ git ["init"]
      gitConfig key value = void $ git ["config", key, value]
      gitAdd file         = void $ git ["add", file]
      gitBranch name sha  = void $ git ["checkout", "-b", name, show sha]
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

      -- Now make an alternative commit that conflicts with c3.
      gitBranch "alternative" c2
      appendFile (dir </> "roy.txt") "You could make me a sandwich.\n"
      gitAdd "roy.txt"
      c3' <- gitCommit "Write alternative ending"

      -- Also add a commit that does not conflict.
      gitBranch "intro" c2
      writeFile (dir </> "leon.txt") "What do you mean, I'm not helping?\n"
      gitAdd "leon.txt"
      c4 <- gitCommit "Add more characters"

      writeFile (dir </> "holden.txt") "I mean, you're not helping! Why is that, Leon?\n"
      gitAdd "holden.txt"
      c5 <- gitCommit "Add response"

      return [c0, c1, c2, c3, c3', c4, c5]

-- Sets up two repositories: one with a few commits in the origin directory, and
-- a clone of that in the repository directory. The clone ensures that the
-- origin repository is set as the "origin" remote in the cloned repository.
initializeRepository :: IO [Sha]
initializeRepository = do
  -- Create the directory for the origin repository, and parent directories.
  createDirectoryIfMissing True originDir
  shas <- populateRepository originDir
  callGit ["clone", "file://" ++ originDir, repoDir]
  return shas

cleanupRepository :: IO ()
cleanupRepository = removeDirectoryRecursive testDir

-- Creates and populates a test repository, runs the body and provides to it the
-- shas of the test commits as shown in populateRepository. Then removes the
-- test repository again.
withTestRepository :: ([Sha] -> IO ()) -> IO ()
withTestRepository body = do
  -- TODO: Generate new repo dir with uuid for every test.
  shas <- initializeRepository
  body shas
  cleanupRepository

-- Starts a new thread that runs the main event loop. Then runs the body and
-- provides to it the queue that the main loop pops from. Finally stops the
-- forked thread.
withMainLoop :: (Logic.EventQueue -> IO ()) -> IO ()
withMainLoop body = do
  mainQueue <- Logic.newEventQueue 10
  -- Like the actual application, start a worker thread to run the main event
  -- loop.
  -- TODO: Non-global config?
  threadId <- forkIO $ void $ EventLoop.runLogicEventLoop config mainQueue
  body mainQueue
  killThread threadId

withTestEnv :: (([Sha], Logic.EventQueue) -> IO ()) -> IO ()
withTestEnv body =
  withTestRepository $ \ shas ->
  withMainLoop $ \ mainQueue ->
  body (shas, mainQueue)

main :: IO ()
main = hspec $ do
  describe "The main event loop" $ do

    it "handles a fast-forwardable pull request" $ withTestEnv $ \ (shas, queue) -> do
      let [c0, c1, c2, c3, c3', c4, c5] = shas
          sendEvent event = atomically $ writeTBQueue queue event
      sendEvent $ Logic.PullRequestOpened (PullRequestId 1) c3 "decker"
      sendEvent $ Logic.CommentAdded (PullRequestId 1) "decker" $ Text.pack $ "LGTM " ++ (show c3)
