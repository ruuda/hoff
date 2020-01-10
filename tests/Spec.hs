-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Writer (Writer, tell, runWriter)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (readFile)
import Data.Function ((&))
import Data.Foldable (foldlM)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (Text)
import Prelude hiding (readFile)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Hspec

import qualified Data.IntMap.Strict as IntMap
import qualified Data.UUID.V4 as Uuid

import EventLoop (convertGithubEvent)
import Git (Branch (..), PushResult(..), Sha (..))
import Github (CommentPayload, CommitStatusPayload, PullRequestPayload)
import Logic hiding (runAction)
import Project (ProjectState (ProjectState), PullRequest (PullRequest))
import Types (PullRequestId (..), Username (..))

import qualified Configuration as Config
import qualified Github
import qualified Project

-- Trigger config used throughout these tests.
testTriggerConfig :: Config.TriggerConfiguration
testTriggerConfig = Config.TriggerConfiguration {
  Config.commentPrefix = "@bot"
}

-- Functions to prepare certain test states.

singlePullRequestState :: PullRequestId -> Branch -> Sha -> Username -> ProjectState
singlePullRequestState pr prBranch prSha prAuthor =
  let
    event = PullRequestOpened pr prBranch prSha "Untitled" prAuthor
  in
    fst $ handleEventFlat event Project.emptyProjectState

candidateState :: PullRequestId -> Branch -> Sha -> Username -> Sha -> ProjectState
candidateState pr prBranch prSha prAuthor candidateSha =
  let state0 = singlePullRequestState pr prBranch prSha prAuthor
      state1 = Project.setIntegrationStatus pr (Project.Integrated candidateSha) state0
  in  state1 { Project.integrationCandidate = Just pr }

-- Types and functions to mock running an action without actually doing anything.

data ActionFlat
  = ATryIntegrate Text (Branch, Sha)
  | ATryPromote Branch Sha
  | ALeaveComment PullRequestId Text
  | AIsReviewer Username
  deriving (Eq, Show)

-- This function simulates running the actions, and returns the final state,
-- together with a list of all actions that would have been performed. Some
-- actions require input from the outside world. Simulating these actions will
-- return the pushResult and rebaseResult passed in here.
runActionWithInit :: Maybe Sha -> PushResult -> Action a -> Writer [ActionFlat] a
runActionWithInit integrateResult pushResult =
  let
    -- In the tests, only "deckard" is a reviewer.
    isReviewer username = username == "deckard"
  in
    foldFree $ \case
      TryIntegrate msg candi h -> do
        tell [ATryIntegrate msg candi]
        pure $ h integrateResult
      TryPromote prBranch headSha h -> do
        tell [ATryPromote prBranch headSha]
        pure $ h pushResult
      LeaveComment pr body x -> do
        tell [ALeaveComment pr body]
        pure x
      IsReviewer username h -> do
        tell [AIsReviewer username]
        pure $ h $ isReviewer username

-- Simulates running the action. Pretends that integration always conflicts.
-- Pretends that pushing is always successful.
runAction :: Action a -> (a, [ActionFlat])
runAction = runWriter . runActionWithInit Nothing PushOk

-- Handle an event and simulate its side effects, then ignore the side effects
-- and return the new state.
handleEventFlat :: Event -> ProjectState -> (ProjectState, [ActionFlat])
handleEventFlat event state = runAction $ handleEvent testTriggerConfig event state

-- Handle events and simulate their side effects, then ignore the side effects
-- and return the new state.
handleEventsFlat :: [Event] -> ProjectState -> (ProjectState, [ActionFlat])
handleEventsFlat events state = runAction
  $ foldlM (flip $ handleEvent testTriggerConfig) state events

-- Proceed with a state until a fixed point, simulate and collect the side
-- effects.
proceedUntilFixedPointFlat
  :: Maybe Sha
  -> PushResult
  -> ProjectState
  -> (ProjectState, [ActionFlat])
proceedUntilFixedPointFlat integrateResult pushResult state =
  runWriter $ runActionWithInit integrateResult pushResult $ proceedUntilFixedPoint state

main :: IO ()
main = hspec $ do
  describe "Logic.handleEvent" $ do

    it "handles PullRequestOpened" $ do
      let event = PullRequestOpened (PullRequestId 3) (Branch "p") (Sha "e0f") "title" "lisa"
          state = fst $ handleEventFlat event Project.emptyProjectState
      state `shouldSatisfy` Project.existsPullRequest (PullRequestId 3)
      let pr = fromJust $ Project.lookupPullRequest (PullRequestId 3) state
      Project.sha pr         `shouldBe` Sha "e0f"
      Project.author pr      `shouldBe` "lisa"
      Project.approvedBy pr  `shouldBe` Nothing
      Project.buildStatus pr `shouldBe` Project.BuildNotStarted

    it "handles PullRequestClosed" $ do
      let event1 = PullRequestOpened (PullRequestId 1) (Branch "p") (Sha "abc") "title" "peter"
          event2 = PullRequestOpened (PullRequestId 2) (Branch "q") (Sha "def") "title" "jack"
          event3 = PullRequestClosed (PullRequestId 1)
          state  = fst $ handleEventsFlat [event1, event2, event3] Project.emptyProjectState
      state `shouldSatisfy` not . Project.existsPullRequest (PullRequestId 1)
      state `shouldSatisfy` Project.existsPullRequest (PullRequestId 2)

    it "handles closing the integration candidate PR" $ do
      let event  = PullRequestClosed (PullRequestId 1)
          state  = candidateState (PullRequestId 1) (Branch "p") (Sha "ea0") "frank" (Sha "cf4")
          state' = fst $ handleEventFlat event state
      Project.integrationCandidate state' `shouldBe` Nothing

    it "does not modify the integration candidate if a different PR was closed" $ do
      let event  = PullRequestClosed (PullRequestId 1)
          state  = candidateState (PullRequestId 2) (Branch "p") (Sha "a38") "franz" (Sha "ed0")
          state' = fst $ handleEventFlat event state
      Project.integrationCandidate state' `shouldBe` (Just $ PullRequestId 2)

    it "loses approval after the PR commit has changed" $ do
      let event  = PullRequestCommitChanged (PullRequestId 1) (Sha "def")
          state0 = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "abc") "alice"
          state1 = Project.setApproval (PullRequestId 1) (Just "hatter") state0
          state2 = fst $ handleEventFlat event state1
          pr1    = fromJust $ Project.lookupPullRequest (PullRequestId 1) state1
          pr2    = fromJust $ Project.lookupPullRequest (PullRequestId 1) state2
      Project.approvedBy pr1 `shouldBe` Just "hatter"
      Project.approvedBy pr2 `shouldBe` Nothing

    it "resets the build status after the PR commit has changed" $ do
      let event  = PullRequestCommitChanged (PullRequestId 1) (Sha "def")
          state0 = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "abc") "thomas"
          state1 = Project.setBuildStatus (PullRequestId 1) Project.BuildPending state0
          state2 = fst $ handleEventFlat event state1
          pr1    = fromJust $ Project.lookupPullRequest (PullRequestId 1) state1
          pr2    = fromJust $ Project.lookupPullRequest (PullRequestId 1) state2
      Project.buildStatus pr1 `shouldBe` Project.BuildPending
      Project.buildStatus pr2 `shouldBe` Project.BuildNotStarted

    it "ignores false positive commit changed events" $ do
      let event  = PullRequestCommitChanged (PullRequestId 1) (Sha "000")
          state0 = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "000") "cindy"
          state1 = Project.setApproval (PullRequestId 1) (Just "daniel") state0
          state2 = Project.setBuildStatus (PullRequestId 1) Project.BuildPending state1
          (state3, actions) = handleEventFlat event state2
      state3 `shouldBe` state2
      actions `shouldBe` []

    it "sets approval after a stamp from a reviewer" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "6412ef5") "toby"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "@bot merge"
          state' = fst $ handleEventFlat event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approvedBy pr `shouldBe` Just "deckard"

    it "does not set approval after a stamp from a non-reviewer" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "6412ef5") "toby"
          -- Note: the comment is a valid approval command, but "rachael" is not
          -- marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "rachael" "@bot merge"
          state' = fst $ handleEventFlat event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approvedBy pr `shouldBe` Nothing

    it "does not set approval after a comment with the wrong prefix" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "6412ef5") "patrick"
          -- Note: "deckard" is marked as reviewer in the test config, but the
          -- prefix is "@bot ", so none of the comments below should trigger approval.
          event1 = CommentAdded (PullRequestId 1) "deckard" "@hoffbot merge"
          event2 = CommentAdded (PullRequestId 1) "deckard" "LGTM :shipit:"
          event3 = CommentAdded (PullRequestId 1) "deckard" "!merge"
          -- In these cases, the prefix is correct, but the command is wrong.
          event4 = CommentAdded (PullRequestId 1) "deckard" "@botmerge"
          event5 = CommentAdded (PullRequestId 1) "deckard" "@bot, merge"
          state' = fst $ handleEventsFlat [event1, event2, event3, event4, event5] state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approvedBy pr `shouldBe` Nothing

    it "accepts command comments case-insensitively" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "6412ef5") "sacha"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "@BoT MeRgE"
          state' = fst $ handleEventFlat event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.approvedBy pr `shouldBe` Just "deckard"

    it "handles a build status change of the integration candidate" $ do
      let event  = BuildStatusChanged (Sha "84c") Project.BuildSucceeded
          state  = candidateState (PullRequestId 1) (Branch "p") (Sha "a38") "johanna" (Sha "84c")
          state' = fst $ handleEventFlat event state
          pr     = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
      Project.buildStatus pr `shouldBe` Project.BuildSucceeded

    it "ignores a build status change of random shas" $ do
      let event0 = PullRequestOpened (PullRequestId 2) (Branch "p") (Sha "0ad") "title" "harry"
          event1 = BuildStatusChanged (Sha "0ad") Project.BuildSucceeded
          state  = candidateState (PullRequestId 1) (Branch "p") (Sha "a38") "harry" (Sha "84c")
          state' = fst $ handleEventsFlat [event0, event1] state
          pr1    = fromJust $ Project.lookupPullRequest (PullRequestId 1) state'
          pr2    = fromJust $ Project.lookupPullRequest (PullRequestId 2) state'
      -- Even though the build status changed for "0ad" which is a known commit,
      -- only the build status of the integration candidate can be changed.
      Project.buildStatus pr1 `shouldBe` Project.BuildNotStarted
      Project.buildStatus pr2 `shouldBe` Project.BuildNotStarted

    it "only checks if a comment author is a reviewer for comment commands" $ do
      let
        state = candidateState (PullRequestId 1) (Branch "p") (Sha "a38") "tyrell" (Sha "84c")
        event0 = CommentAdded (PullRequestId 1) "deckard" "I don't get it, Tyrell"
        event1 = CommentAdded (PullRequestId 1) "deckard" "@bot merge"
        actions0 = snd $ handleEventFlat event0 state
        actions1 = snd $ handleEventFlat event1 state
      actions0 `shouldBe` []
      actions1 `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved by @deckard, rebasing now."
        ]

    it "notifies approvers about queue position" $ do
      let
        state = candidateState (PullRequestId 1) (Branch "p") (Sha "a38") "tyrell" (Sha "84c")
              & Project.insertPullRequest (PullRequestId 2) (Branch "s") (Sha "dec") "Some PR" (Username "rachael")
              & Project.insertPullRequest (PullRequestId 3) (Branch "s") (Sha "f16") "Another PR" (Username "rachael")
        events = [ CommentAdded (PullRequestId 1) "deckard" "@bot merge"
                 , CommentAdded (PullRequestId 2) "deckard" "@bot merge"
                 , CommentAdded (PullRequestId 3) "deckard" "@bot merge"
                 ]
        actions = snd $ handleEventsFlat events state
      actions `shouldBe`
        [ AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 1) "Pull request approved by @deckard, rebasing now."
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 2) "Pull request approved by @deckard, waiting for rebase at the front of the queue."
        , AIsReviewer "deckard"
        , ALeaveComment (PullRequestId 3) "Pull request approved by @deckard, waiting for rebase behind 2 pull requests."
        ]

  describe "Logic.proceedUntilFixedPoint" $ do

    it "finds a new candidate" $ do
      let state = Project.setApproval (PullRequestId 1) (Just "fred") $
                  singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "f34") "sally"
          (state', actions)  = proceedUntilFixedPointFlat (Just (Sha "38c")) PushRejected state
          (prId, pullRequest) = fromJust $ Project.getIntegrationCandidate state'
      Project.integrationStatus pullRequest `shouldBe` Project.Integrated (Sha "38c")
      Project.buildStatus pullRequest       `shouldBe` Project.BuildPending
      prId    `shouldBe` PullRequestId 1
      actions `shouldBe`
        [ ATryIntegrate "Merge #1\n\nApproved-by: fred" (Branch "refs/pull/1/head", Sha "f34")
        , ALeaveComment (PullRequestId 1) "Rebased as 38c, waiting for CI \x2026"
        ]

    it "pushes after a successful build" $ do
      let pullRequest = PullRequest
            {
              Project.branch            = Branch "results/rachael",
              Project.sha               = Sha "f35",
              Project.title             = "Add my test results",
              Project.author            = "rachael",
              Project.approvedBy        = Just "deckard",
              Project.buildStatus       = Project.BuildSucceeded,
              Project.integrationStatus = Project.Integrated (Sha "38d")
            }
          state = ProjectState
            {
              Project.pullRequests         = IntMap.singleton 1 pullRequest,
              Project.integrationCandidate = Just $ PullRequestId 1
            }
          (state', actions) = proceedUntilFixedPointFlat (Just (Sha "38e")) PushOk state
          candidate         = Project.getIntegrationCandidate state'
      -- After a successful push, the candidate should be gone.
      candidate `shouldBe` Nothing
      actions   `shouldBe` [ATryPromote (Branch "results/rachael") (Sha "38d")]

    it "restarts the sequence after a rejected push" $ do
      -- Set up a pull request that has gone through the review and build cycle,
      -- and is ready to be pushed to master.
      let pullRequest = PullRequest
            {
              Project.branch            = Branch "results/rachael",
              Project.sha               = Sha "f35",
              Project.title             = "Add my test results",
              Project.author            = "rachael",
              Project.approvedBy        = Just "deckard",
              Project.buildStatus       = Project.BuildSucceeded,
              Project.integrationStatus = Project.Integrated (Sha "38d")
            }
          state = ProjectState
            {
              Project.pullRequests         = IntMap.singleton 1 pullRequest,
              Project.integrationCandidate = Just $ PullRequestId 1
            }
          -- Run 'proceedUntilFixedPoint', and pretend that pushes fail (because
          -- something was pushed in the mean time, for instance).
          (state', actions) = proceedUntilFixedPointFlat (Just (Sha "38e")) PushRejected state
          (_, pullRequest') = fromJust $ Project.getIntegrationCandidate state'

      Project.integrationStatus pullRequest' `shouldBe` Project.Integrated (Sha "38e")
      Project.buildStatus       pullRequest' `shouldBe` Project.BuildPending
      actions `shouldBe`
        [ ATryPromote (Branch "results/rachael") (Sha "38d")
        , ATryIntegrate "Merge #1\n\nApproved-by: deckard" (Branch "refs/pull/1/head", Sha "f35")
        , ALeaveComment (PullRequestId 1) "Rebased as 38e, waiting for CI \x2026"
        ]

    it "picks a new candidate from the queue after a successful push" $ do
      let pullRequest1 = PullRequest
            {
              Project.branch            = Branch "results/leon",
              Project.sha               = Sha "f35",
              Project.title             = "Add Leon test results",
              Project.author            = "rachael",
              Project.approvedBy        = Just "deckard",
              Project.buildStatus       = Project.BuildSucceeded,
              Project.integrationStatus = Project.Integrated (Sha "38d")
            }
          pullRequest2 = PullRequest
            {
              Project.branch            = Branch "results/rachael",
              Project.sha               = Sha "f37",
              Project.title             = "Add my test results",
              Project.author            = "rachael",
              Project.approvedBy        = Just "deckard",
              Project.buildStatus       = Project.BuildNotStarted,
              Project.integrationStatus = Project.NotIntegrated
            }
          prMap = IntMap.fromList [(1, pullRequest1), (2, pullRequest2)]
          -- After a successful push, the state of pull request 1 will still be
          -- BuildSucceeded and Integrated, but the candidate will be Nothing.
          state = ProjectState
            {
              Project.pullRequests         = prMap,
              Project.integrationCandidate = Nothing
            }
          -- Proceeding should pick the next pull request as candidate.
          (state', actions) = proceedUntilFixedPointFlat (Just (Sha "38e")) PushOk state
          Just (cId, _candidate) = Project.getIntegrationCandidate state'
      cId     `shouldBe` PullRequestId 2
      actions `shouldBe`
        [ ATryIntegrate "Merge #2\n\nApproved-by: deckard" (Branch "refs/pull/2/head", Sha "f37")
        , ALeaveComment (PullRequestId 2) "Rebased as 38e, waiting for CI \x2026"
        ]

  describe "Github._Payload" $ do

    it "parses a PullRequestPayload correctly" $ do
      examplePayload <- readFile "tests/data/pull-request-payload.json"
      let maybePayload :: Maybe PullRequestPayload
          maybePayload = decode examplePayload
      maybePayload `shouldSatisfy` isJust
      let payload    = fromJust maybePayload
          action     = Github.action     (payload :: PullRequestPayload)
          owner      = Github.owner      (payload :: PullRequestPayload)
          repository = Github.repository (payload :: PullRequestPayload)
          number     = Github.number     (payload :: PullRequestPayload)
          headSha    = Github.sha        (payload :: PullRequestPayload)
          title      = Github.title      (payload :: PullRequestPayload)
          prAuthor   = Github.author     (payload :: PullRequestPayload)
          prBranch   = Github.branch     (payload :: PullRequestPayload)
      action     `shouldBe` Github.Opened
      owner      `shouldBe` "baxterthehacker"
      repository `shouldBe` "public-repo"
      number     `shouldBe` 1
      headSha    `shouldBe` (Sha "0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c")
      prBranch   `shouldBe` (Branch "changes")
      title      `shouldBe` "Update the README with new information"
      prAuthor   `shouldBe` "baxterthehacker2"

    it "parses a CommentPayload correctly" $ do
      examplePayload <- readFile "tests/data/issue-comment-payload.json"
      let maybePayload :: Maybe CommentPayload
          maybePayload = decode examplePayload
      maybePayload `shouldSatisfy` isJust
      let payload       = fromJust maybePayload
          action        = Github.action     (payload :: CommentPayload)
          owner         = Github.owner      (payload :: CommentPayload)
          repository    = Github.repository (payload :: CommentPayload)
          number        = Github.number     (payload :: CommentPayload)
          commentAuthor = Github.author     (payload :: CommentPayload)
          commentBody   = Github.body       (payload :: CommentPayload)
      action        `shouldBe` Github.Created
      owner         `shouldBe` "baxterthehacker"
      repository    `shouldBe` "public-repo"
      number        `shouldBe` 2
      commentAuthor `shouldBe` "baxterthehacker2"
      commentBody   `shouldBe` "You are totally right! I'll get this fixed right away."

    it "parses a CommitStatusPayload correctly" $ do
      examplePayload <- readFile "tests/data/status-payload.json"
      let maybePayload :: Maybe CommitStatusPayload
          maybePayload = decode examplePayload
      maybePayload `shouldSatisfy` isJust
      let payload       = fromJust maybePayload
          owner         = Github.owner      (payload :: CommitStatusPayload)
          repository    = Github.repository (payload :: CommitStatusPayload)
          status        = Github.status     (payload :: CommitStatusPayload)
          url           = Github.url        (payload :: CommitStatusPayload)
          commitSha     = Github.sha        (payload :: CommitStatusPayload)
      owner      `shouldBe` "baxterthehacker"
      repository `shouldBe` "public-repo"
      status     `shouldBe` Github.Success
      url        `shouldBe` Nothing
      commitSha  `shouldBe` (Sha "9049f1265b7d61be4a8904a9a27120d2064dab3b")

  describe "Configuration" $ do

    it "loads the example config file correctly" $ do
      -- This test loads the example configuration file that is packaged. It
      -- serves two purposes: to test that loading a config file works, but
      -- mainly, to enforce that the example file is kept up to date.
      Right cfg <- Config.loadConfiguration "package/example-config.json"
      Config.secret      cfg `shouldBe` "run 'head --bytes 32 /dev/urandom | base64' and paste output here"
      Config.accessToken cfg `shouldBe` "paste a personal access token for a bot user here"
      Config.port        cfg `shouldBe` 1979
      Config.tls         cfg `shouldSatisfy` isNothing

      let
        projects = Config.projects cfg
        project  = head projects
        user     = Config.user cfg
        trigger  = Config.trigger cfg

      Config.owner      project `shouldBe` "your-github-username-or-organization"
      Config.repository project `shouldBe` "your-repo"
      Config.branch     project `shouldBe` "master"
      Config.testBranch project `shouldBe` "testing"
      Config.checkout   project `shouldBe` "/var/lib/hoff/checkouts/your-username/your-repo"
      Config.stateFile  project `shouldBe` "/var/lib/hoff/state/your-username/your-repo.json"

      Config.name user          `shouldBe` "CI Bot"
      Config.email user         `shouldBe` "cibot@example.com"
      Config.sshConfigFile user `shouldBe` "/etc/hoff/ssh_config"

      Config.commentPrefix trigger `shouldBe` "@hoffbot"

  describe "EventLoop.convertGithubEvent" $ do

    let testPullRequestPayload action = Github.PullRequestPayload
          { action     = action
          , owner      = "deckard"
          , repository = "repo"
          , number     = 1
          , branch     = Branch "results"
          , sha        = Sha "b26354"
          , title      = "Add test results"
          , author     = "rachael"
          }

    it "converts a pull request opened event" $ do
      let payload = testPullRequestPayload Github.Opened
          Just event = convertGithubEvent $ Github.PullRequest payload
      event `shouldBe`
        (PullRequestOpened (PullRequestId 1) (Branch "results") (Sha "b26354") "Add test results" "rachael")

    it "converts a pull request reopened event" $ do
      let payload = testPullRequestPayload Github.Reopened
          Just event = convertGithubEvent $ Github.PullRequest payload
      -- Reopened is treated just like opened, there is no memory in the system.
      event `shouldBe`
        (PullRequestOpened (PullRequestId 1) (Branch "results") (Sha "b26354") "Add test results" "rachael")

    it "converts a pull request closed event" $ do
      let payload = testPullRequestPayload Github.Closed
          Just event = convertGithubEvent $ Github.PullRequest payload
      event `shouldBe` (PullRequestClosed (PullRequestId 1))

    it "converts a pull request synchronize event" $ do
      let payload = testPullRequestPayload Github.Synchronize
          Just event = convertGithubEvent $ Github.PullRequest payload
      event `shouldBe` (PullRequestCommitChanged (PullRequestId 1) (Sha "b26354"))

    let testCommentPayload action = Github.CommentPayload
          { action     = action
          , owner      = "rachael"
          , repository = "owl"
          , number     = 1
          , author     = "deckard"
          , body       = "Must be expensive."
          }

    it "converts a comment created event" $ do
      let payload = testCommentPayload Github.Created
          Just event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` (CommentAdded (PullRequestId 1) "deckard" "Must be expensive.")

    it "ignores a comment edited event" $ do
      let payload = testCommentPayload Github.Edited
          event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` Nothing

    it "ignores a comment deleted event" $ do
      let payload = testCommentPayload Github.Deleted
          event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` Nothing

    let testCommitStatusPayload status = Github.CommitStatusPayload
          { owner      = "rachael"
          , repository = "owl"
          , status     = status
          , url        = Just "https://travis-ci.org/rachael/owl/builds/1982"
          , sha        = Sha "b26354"
          }

    it "converts a commit status pending event" $ do
      let payload = testCommitStatusPayload Github.Pending
          Just event = convertGithubEvent $ Github.CommitStatus payload
      event `shouldBe` (BuildStatusChanged (Sha "b26354") Project.BuildPending)

    it "converts a commit status success event" $ do
      let payload = testCommitStatusPayload Github.Success
          Just event = convertGithubEvent $ Github.CommitStatus payload
      event `shouldBe` (BuildStatusChanged (Sha "b26354") Project.BuildSucceeded)

    it "converts a commit status failure event" $ do
      let payload = testCommitStatusPayload Github.Failure
          Just event = convertGithubEvent $ Github.CommitStatus payload
      event `shouldBe` (BuildStatusChanged (Sha "b26354") Project.BuildFailed)

    it "converts a commit status error event" $ do
      let payload = testCommitStatusPayload Github.Error
          Just event = convertGithubEvent $ Github.CommitStatus payload
      -- The error and failure statuses are both converted to "failed".
      event `shouldBe` (BuildStatusChanged (Sha "b26354") Project.BuildFailed)

  describe "ProjectState" $ do

    it "can be restored exactly after roundtripping through json" $ do
      let emptyState  = Project.emptyProjectState
          singleState = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "071") "deckard"
          candState   = candidateState (PullRequestId 2) (Branch "p") (Sha "073") "rachael" (Sha "079")
          Just emptyState'  = decode $ encode emptyState
          Just singleState' = decode $ encode singleState
          Just candState'   = decode $ encode candState
      emptyState  `shouldBe` emptyState'
      singleState `shouldBe` singleState'
      candState   `shouldBe` candState'

    it "loads correctly after persisting to disk" $ do
      -- Generate a random filename in /tmp (or equivalent) to persist the state
      -- to during the test.
      uuid       <- Uuid.nextRandom
      tmpBaseDir <- getTemporaryDirectory
      let fname = tmpBaseDir </> ("state-" ++ (show uuid) ++ ".json")
          state = singlePullRequestState (PullRequestId 1) (Branch "p") (Sha "071") "deckard"
      Project.saveProjectState fname state
      Just state' <- Project.loadProjectState fname
      state `shouldBe` state'
      removeFile fname
