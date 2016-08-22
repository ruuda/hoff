-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

-- This file contains sort-of end-to-end tests for the event loop. Incoming
-- messages are faked (supplied by the test code, as oppsed to received via the
-- web server), but the event loop does run with actual IO with a test
-- repository. The purpose of these tests is not to test the *logic* of the
-- event loop (there are unit tests for that), but to test that the interaction
-- with a real Git process works as expected.

module EventLoopSpec (eventLoopSpec) where

import Control.Concurrent.Async (async, wait)
import Control.Monad (forM_, void, when)
import Control.Monad.Logger (runNoLoggingT)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.UUID.V4 (nextRandom)
import Prelude hiding (appendFile, writeFile)
import System.FilePath ((</>))
import Test.Hspec

import qualified System.Directory as FileSystem

import Configuration (Configuration (..))
import Git (Branch (..), Sha (..))
import Project (BuildStatus (..), IntegrationStatus (..), ProjectState, PullRequestId (..))

import qualified Configuration as Config
import qualified Data.Text as Text
import qualified EventLoop
import qualified Git
import qualified Logic
import qualified Prelude
import qualified Project

-- Invokes Git with the given arguments, returns its stdout. Crashes if invoking
-- Git failed. Discards all logging.
callGit :: [String] -> IO Text
callGit args = fmap (either undefined id) $ runNoLoggingT $ Git.callGit args

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
  let writeFile fname msg  = Prelude.writeFile (dir </> fname) (msg ++ "\n")
      appendFile fname msg = Prelude.appendFile (dir </> fname) (msg ++ "\n")
      git args             = callGit $ ["-C", dir] ++ args
      gitInit              = void $ git ["init"]
      gitConfig key value  = void $ git ["config", key, value]
      gitAdd file          = void $ git ["add", file]
      gitBranch name sha   = void $ git ["checkout", "-b", name, show sha]
      gitCheckout brname   = void $ git ["checkout", brname]
      getHeadSha           = fmap (Sha . Text.strip) $ git ["rev-parse", "@"]
      -- Commits with the given message and returns the sha of the new commit.
      gitCommit message    = git ["commit", "-m", message] >> getHeadSha
  in  do
      gitInit
      gitConfig "user.email" "testsuite@example.com"
      gitConfig "user.name" "Testbot"

      writeFile "tyrell.txt" "I'm surprised you didn't come here sooner."
      gitAdd "tyrell.txt"
      c0 <- gitCommit "c0: Initial commit"

      writeFile "roy.txt" "It's not an easy thing to meet your maker."
      gitAdd "roy.txt"
      c1 <- gitCommit "c1: Add new quote"

      appendFile "tyrell.txt" "What can he do for you?"
      gitAdd "tyrell.txt"
      c2 <- gitCommit "c2: Add new Tyrell quote"

      appendFile "roy.txt" "Can the maker repair what he makes?"
      gitAdd "roy.txt"
      c3 <- gitCommit "c3: Add new Roy quote"

      -- Create a branch "ahead", one commit ahead of master.
      gitBranch "ahead" c3
      appendFile "tyrell.txt" "Would you like to be modified?"
      gitAdd "tyrell.txt"
      c4 <- gitCommit "c4: Add Tyrell  response"
      gitCheckout "master"

      -- Now make an alternative commit that conflicts with c3.
      gitBranch "alternative" c2
      appendFile "roy.txt" "You could make me a sandwich."
      gitAdd "roy.txt"
      c3' <- gitCommit "c3': Write alternative ending"

      -- Also add a commit that does not conflict.
      gitBranch "intro" c2
      writeFile "leon.txt" "What do you mean, I'm not helping?"
      gitAdd "leon.txt"
      c5 <- gitCommit "c5: Add more characters"

      writeFile "holden.txt" "I mean, you're not helping! Why is that, Leon?"
      gitAdd "holden.txt"
      c6 <- gitCommit "c6: Add response"

      -- Make HEAD point at master again, for when it is cloned later.
      gitCheckout "master"

      return [c0, c1, c2, c3, c3', c4, c5, c6]

-- Sets up two repositories: one with a few commits in the origin directory, and
-- a clone of that in the repository directory. The clone ensures that the
-- origin repository is set as the "origin" remote in the cloned repository.
initializeRepository :: FilePath -> FilePath -> IO [Sha]
initializeRepository originDir repoDir = do
  -- Create the directory for the origin repository, and parent directories.
  FileSystem.createDirectoryIfMissing True originDir
  shas <- populateRepository originDir
  -- Clone with --single-branch, to make sure that we do not have all commits
  -- in the repo dir: when this is running for real, we won't have new commits
  -- already in the repository either. They need to be fetched.
  _    <- callGit ["clone", "--single-branch", "file://" ++ originDir, repoDir]
  -- Set the author details in the cloned repository as well, to ensure that
  -- there is no implicit dependency on a global Git configuration.
  _    <- callGit ["-C", repoDir, "config", "user.email", "testsuite@example.com"]
  _    <- callGit ["-C", repoDir, "config", "user.name", "Testbot"]
  return shas

-- Generate a configuration to be used in the test environment.
buildConfig :: FilePath -> Configuration
buildConfig repoDir = Configuration {
  Config.owner      = "ruuda",
  Config.repository = "blog",
  Config.branch     = "master",
  Config.testBranch = "integration",
  Config.port       = 5261,
  Config.checkout   = repoDir,
  Config.secret     = "N6MAC41717"
}

-- Runs the main loop in a separate thread, and feeds it the given events.
runMainEventLoop :: Configuration -> ProjectState -> [Logic.Event] -> IO ProjectState
runMainEventLoop config initialState events = do
  -- Like the actual application, start a new thread to run the main event loop.
  -- Use 'async' here, a higher-level wrapper around 'forkIO', to wait for the
  -- thread to stop later. Discard log messages from the event loop, to avoid
  -- polluting the test output. Ignore requests to persist the state.
  --
  -- To aid debugging when a test fails, you can replace 'runNoLoggingT' with
  -- 'runStdoutLoggingT'. You should also remove 'parallel' from main then.
  let persist _     = return ()
  queue            <- Logic.newEventQueue 10
  finalStateAsync  <- async
    $ runNoLoggingT
    $ EventLoop.runLogicEventLoop config persist queue initialState

  -- Enqueue all provided events.
  forM_ events (Logic.enqueueEvent queue)

  -- Tell the worker thread to stop after it has processed all events. Then wait
  -- for it to exit, and return the final state.
  Logic.enqueueStopSignal queue
  wait finalStateAsync

-- Recursively makes all files and directories in a directory writable.
-- On Windows this is required to be able to recursively delete the directory.
makeWritableRecursive :: FilePath -> IO ()
makeWritableRecursive path = do
  permissions <- FileSystem.getPermissions path
  FileSystem.setPermissions path (FileSystem.setOwnerWritable True permissions)
  isDirectory <- FileSystem.doesDirectoryExist path
  when isDirectory $ do
    contents <- FileSystem.listDirectory path
    forM_ contents $ \ item -> makeWritableRecursive (path </> item)

type LoopRunner = ProjectState -> [Logic.Event] -> IO ProjectState
type GitRunner = [String] -> IO ()

-- Sets up a test environment with an actual Git repository on the file system,
-- and a thread running the main event loop. Then invokes the body, and tears
-- down the test environment afterwards. Returns a list of commit message
-- prefixes of the remote master branch log. The body function is provided with
-- the shas of the test repository and a function to run the event loop.
withTestEnv :: ([Sha] -> LoopRunner -> GitRunner -> IO ()) -> IO [Text]
withTestEnv body = do
  -- To run these tests, a real repository has to be made somewhere. Do that in
  -- /tmp because it can be mounted as a ramdisk, so it is fast and we don't
  -- unnecessarily wear out SSDs. Put a uuid in there to ensure we don't
  -- overwrite somebody else's files, and to ensure that the tests do not affect
  -- eachother.
  uuid       <- nextRandom
  tmpBaseDir <- FileSystem.getTemporaryDirectory
  let testDir   = tmpBaseDir </> ("testsuite-" ++ (show uuid))
      originDir = testDir </> "repo-origin"
      repoDir   = testDir </> "repo-local"
  -- Create and populate a test repository with a local remote "origin". Record
  -- the shas of the commits as documented in populateRepository.
  shas <- initializeRepository originDir repoDir

  -- Run the actual test code inside the environment that we just set up,
  -- provide it with the commit shas, the function to run the event loop, and a
  -- function to invoke Git in the cloned repository.
  let config   = buildConfig repoDir
      git args = void $ callGit $ ["-C", repoDir] ++ args
  body shas (runMainEventLoop config) git

  -- Retrieve the log of the remote repository master branch. Only show the
  -- commit message subject lines. The repository has been setup to prefix
  -- messages with a commit number followed by a colon. Strip off the rest.
  -- Commit messages are compared later, rather than shas, because these do not
  -- change when rebased, and they do not depend on the current timestamp.
  -- (Commits do: the same rebase operation can produce commits with different
  -- shas depending on the time of the rebase.)
  masterLog <- callGit ["-C", originDir, "log", "--format=%s", "master"]
  let commits = reverse $ fmap (Text.takeWhile (/= ':')) $ Text.lines masterLog

  makeWritableRecursive testDir
  FileSystem.removeDirectoryRecursive testDir
  return commits

eventLoopSpec :: Spec
eventLoopSpec = parallel $ do
  describe "The main event loop" $ do

    it "handles a fast-forwardable pull request" $ do
      history <- withTestEnv $ \ shas runLoop _git -> do
        let [_c0, _c1, _c2, _c3, _c3', c4, _c5, _c6] = shas
            pr1 = PullRequestId 1

        -- Commit c4 is one commit ahead of master, so integrating it can be done
        -- with a fast-forward merge. Run the main event loop for these events
        -- and discard the final state by using 'void'.
        void $ runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr1 c4 (Branch "ahead") "deckard",
            Logic.CommentAdded pr1 "rachael" $ Text.pack $ "LGTM " ++ (show c4),
            Logic.BuildStatusChanged c4 BuildSucceeded
          ]

      history `shouldBe` ["c0", "c1", "c2", "c3", "c4"]

    it "handles a non-conflicting non-fast-forwardable pull request" $ do
      history <- withTestEnv $ \ shas runLoop _git -> do
        let [_c0, _c1, _c2, _c3, _c3', _c4, _c5, c6] = shas
            pr1 = PullRequestId 1

        -- Commit c6 is two commits ahead and one behind of master, so
        -- integrating it produces new rebased commits.
        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr1 c6 (Branch "intro") "deckard",
            Logic.CommentAdded pr1 "rachael" $ Text.pack $ "LGTM " ++ (show c6)
          ]

        -- Extract the sha of the rebased commit from the project state.
        let Just (_prId, pullRequest)     = Project.getIntegrationCandidate state
            Project.Integrated rebasedSha = Project.integrationStatus pullRequest

        -- The rebased commit should have been pushed to the remote repository
        -- 'integration' branch. Tell that building it succeeded.
        void $ runLoop state [Logic.BuildStatusChanged rebasedSha BuildSucceeded]

      history `shouldBe` ["c0", "c1", "c2", "c3", "c5", "c6"]

    it "handles multiple pull requests" $ do
      history <- withTestEnv $ \ shas runLoop _git -> do
        let [_c0, _c1, _c2, _c3, _c3', c4, _c5, c6] = shas
            pr1 = PullRequestId 1
            pr2 = PullRequestId 2

        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr1 c4 (Branch "ahead") "deckard",
            Logic.PullRequestOpened pr2 c6 (Branch "intro") "deckard",
            -- Note that although c4 has a lower pull request number, c6 should
            -- still be integrated first because it was approved earlier.
            Logic.CommentAdded pr2 "rachael" $ Text.pack $ "LGTM " ++ (show c6),
            Logic.CommentAdded pr1 "rachael" $ Text.pack $ "LGTM " ++ (show c4)
          ]

        -- Extract the sha of the rebased commit from the project state.
        let Just (_prId, pullRequest2)    = Project.getIntegrationCandidate state
            Project.Integrated rebasedSha = Project.integrationStatus pullRequest2

        -- The rebased commit should have been pushed to the remote repository
        -- 'integration' branch. Tell that building it succeeded.
        state' <- runLoop state [Logic.BuildStatusChanged rebasedSha BuildSucceeded]

        -- Repeat for the other pull request, which should be the candidate by
        -- now.
        let Just (_prId, pullRequest1)     = Project.getIntegrationCandidate state'
            Project.Integrated rebasedSha' = Project.integrationStatus pullRequest1
        void $ runLoop state' [Logic.BuildStatusChanged rebasedSha' BuildSucceeded]

      history `shouldBe` ["c0", "c1", "c2", "c3", "c5", "c6", "c4"]

    it "skips conflicted pull requests" $ do
      history <- withTestEnv $ \ shas runLoop _git -> do
        let [_c0, _c1, _c2, _c3, c3', c4, _c5, _c6] = shas
            pr1 = PullRequestId 1
            pr2 = PullRequestId 2

        -- Commit c3' conflicts with master, so a rebase should be attempted, but
        -- because it conflicts, the next pull request should be considered.
        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr1 c3' (Branch "alternative") "deckard",
            Logic.PullRequestOpened pr2 c4 (Branch "ahead") "deckard",
            Logic.CommentAdded pr1 "rachael" $ Text.pack $ "LGTM " ++ (show c3'),
            Logic.CommentAdded pr2 "rachael" $ Text.pack $ "LGTM " ++ (show c4)
          ]

        -- The first pull request should be marked as conflicted. Note: this
        -- test also verifies that the repository is left in a good state after
        -- the conflicted rebase, so that the next commit can be integrated
        -- properly.
        let Just pullRequest1 = Project.lookupPullRequest pr1 state
        Project.integrationStatus pullRequest1 `shouldBe` Conflicted

        -- The second pull request should still be pending, awaiting the build
        -- result.
        let Just (prId, pullRequest2) = Project.getIntegrationCandidate state
        prId `shouldBe` pr2
        Project.buildStatus pullRequest2 `shouldBe` BuildPending

      -- We did not send a build status notification for c4, so it should not
      -- have been integrated.
      history `shouldBe` ["c0", "c1", "c2", "c3"]

    it "restarts the sequence after a rejected push" $ do
      history <- withTestEnv $ \ shas runLoop git -> do
        let [_c0, _c1, _c2, _c3, _c3', c4, _c5, c6] = shas
            pr1 = PullRequestId 1

        state <- runLoop Project.emptyProjectState
          [
            Logic.PullRequestOpened pr1 c6 (Branch "intro") "deckard",
            Logic.CommentAdded pr1 "rachael" $ Text.pack $ "LGTM " ++ (show c6)
          ]

        -- At this point, c6 has been rebased and pushed to the "integration"
        -- branch for building. Before we notify build success, push commmit c4
        -- to the origin "master" branch, so that pushing the rebased c6 will
        -- fail later on.
        git ["push", "origin", (show c4) ++ ":refs/heads/master"]

        -- Extract the sha of the rebased commit from the project state, and
        -- tell the loop that building the commit succeeded.
        let Just (_prId, pullRequest)     = Project.getIntegrationCandidate state
            Project.Integrated rebasedSha = Project.integrationStatus pullRequest
        state' <- runLoop state [Logic.BuildStatusChanged rebasedSha BuildSucceeded]

        -- The push should have failed, hence there should still be an
        -- integration candidate.
        Project.getIntegrationCandidate state' `shouldSatisfy` isJust

        -- Again notify build success, now for the new commit.
        let Just (_prId, pullRequest')      = Project.getIntegrationCandidate state'
            Project.Integrated rebasedSha'  = Project.integrationStatus pullRequest'
        state'' <- runLoop state' [Logic.BuildStatusChanged rebasedSha' BuildSucceeded]

        -- After the second build success, the pull request should have been
        -- integrated properly, so there should not be a new candidate.
        Project.getIntegrationCandidate state'' `shouldBe` Nothing

      history `shouldBe` ["c0", "c1", "c2", "c3", "c4", "c5", "c6"]
