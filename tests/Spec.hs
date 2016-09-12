-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Control.Monad.Free (Free (..))
import Data.Aeson (decode)
import Data.ByteString.Lazy (readFile)
import Data.Foldable (foldlM)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Prelude hiding (readFile)
import Test.Hspec

import qualified Data.IntMap as IntMap

import Configuration (Configuration)
import EventLoop (convertGithubEvent)
import Git (Branch (..), PushResult(..), Sha (..))
import Github (CommentPayload, CommitStatusPayload, PullRequestPayload)
import Logic hiding (runAction)
import Project

import qualified Configuration as Config
import qualified Github

-- Config used throughout these tests.
testConfig :: Configuration
testConfig = Config.Configuration {
  Config.owner = "deckard",
  Config.repository = "voight-kampff",
  Config.branch = "master",
  Config.testBranch = "testing",
  Config.checkout = "/tmp/deckard/voight-kampff",
  Config.reviewers = ["deckard"],
  Config.secret = "secret",
  Config.port = 5261,
  Config.stateFile = "/dev/null"
}

-- Functions to prepare certain test states.

singlePullRequestState :: PullRequestId -> Sha -> Text -> ProjectState
singlePullRequestState pr prSha prAuthor =
  let event = PullRequestOpened pr prSha prAuthor
  in  handleEventFlat event emptyProjectState

candidateState :: PullRequestId -> Sha -> Text -> Sha -> ProjectState
candidateState pr prSha prAuthor candidateSha =
  let state0 = singlePullRequestState pr prSha prAuthor
      state1 = setIntegrationStatus pr (Integrated candidateSha) state0
  in  state1 { integrationCandidate = Just pr }

-- Types and functions to mock running an action without actually doing anything.

data ActionFlat
  = ATryIntegrate (Branch, Sha)
  | APushNewHead Sha
  | ALeaveComment PullRequestId Text
  deriving (Eq, Show)

-- This function simulates running the actions, and returns the final state,
-- together with a list of all actions that would have been performed. Some
-- actions require input from the outside world. Simulating these actions will
-- return the pushResult and rebaseResult passed in here.
runActionWithInit :: Maybe Sha -> PushResult -> [ActionFlat] -> Action a -> (a, [ActionFlat])
runActionWithInit integrateResult pushResult actions action =
  let prepend cont act =
        let (result, acts') = runActionWithInit integrateResult pushResult actions cont
        in  (result, act : acts')
  in case action of
    Pure result                   -> (result, [])
    Free (TryIntegrate candi h)   -> prepend (h integrateResult) $ ATryIntegrate candi
    Free (PushNewHead headSha h)  -> prepend (h pushResult) $ APushNewHead headSha
    Free (LeaveComment pr body x) -> prepend x $ ALeaveComment pr body

-- Simulates running the action. Pretends that integration always conflicts.
-- Pretends that pushing is always successful.
runAction :: Action a -> (a, [ActionFlat])
runAction = runActionWithInit Nothing PushOk []

-- TODO: Do not ignore actions information, assert that certain events do not
-- have undesirable side effects.
getState :: Action ProjectState -> ProjectState
getState = fst . runAction

-- Handle an event and simulate its side effects, then ignore the side effects
-- and return the new state.
handleEventFlat :: Event -> ProjectState -> ProjectState
handleEventFlat event state = getState $ handleEvent testConfig event state

-- Handle events and simulate their side effects, then ignore the side effects
-- and return the new state.
handleEventsFlat :: [Event] -> ProjectState -> ProjectState
handleEventsFlat events state = getState $ foldlM (flip $ handleEvent testConfig) state events

-- Proceed with a state until a fixed point, simulate and collect the side
-- effects.
proceedUntilFixedPointFlat :: Maybe Sha -> PushResult -> ProjectState -> (ProjectState, [ActionFlat])
proceedUntilFixedPointFlat integrateResult pushResult state =
  runActionWithInit integrateResult pushResult [] $ proceedUntilFixedPoint state

main :: IO ()
main = hspec $ do
  describe "Logic.handleEvent" $ do

    it "handles PullRequestOpened" $ do
      let event = PullRequestOpened (PullRequestId 3) (Sha "e0f") "lisa"
          state = handleEventFlat event emptyProjectState
      state `shouldSatisfy` existsPullRequest (PullRequestId 3)
      let pr = fromJust $ lookupPullRequest (PullRequestId 3) state
      sha pr         `shouldBe` Sha "e0f"
      author pr      `shouldBe` "lisa"
      approvedBy pr  `shouldBe` Nothing
      buildStatus pr `shouldBe` BuildNotStarted

    it "handles PullRequestClosed" $ do
      let event1 = PullRequestOpened (PullRequestId 1) (Sha "abc") "peter"
          event2 = PullRequestOpened (PullRequestId 2) (Sha "def") "jack"
          event3 = PullRequestClosed (PullRequestId 1)
          state  = handleEventsFlat [event1, event2, event3] emptyProjectState
      state `shouldSatisfy` not . existsPullRequest (PullRequestId 1)
      state `shouldSatisfy` existsPullRequest (PullRequestId 2)

    it "handles closing the integration candidate PR" $ do
      let event  = PullRequestClosed (PullRequestId 1)
          state  = candidateState (PullRequestId 1) (Sha "ea0") "frank" (Sha "cf4")
          state' = handleEventFlat event state
      integrationCandidate state' `shouldBe` Nothing

    it "does not modify the integration candidate if a different PR was closed" $ do
      let event  = PullRequestClosed (PullRequestId 1)
          state  = candidateState (PullRequestId 2) (Sha "a38") "franz" (Sha "ed0")
          state' = handleEventFlat event state
      integrationCandidate state' `shouldBe` (Just $ PullRequestId 2)

    it "loses approval after the PR commit has changed" $ do
      let event  = PullRequestCommitChanged (PullRequestId 1) (Sha "def")
          state0 = singlePullRequestState (PullRequestId 1) (Sha "abc") "alice"
          state1 = setApproval (PullRequestId 1) (Just "hatter") state0
          state2 = handleEventFlat event state1
          pr1    = fromJust $ lookupPullRequest (PullRequestId 1) state1
          pr2    = fromJust $ lookupPullRequest (PullRequestId 1) state2
      approvedBy pr1 `shouldBe` Just "hatter"
      approvedBy pr2 `shouldBe` Nothing

    it "resets the build status after the PR commit has changed" $ do
      let event  = PullRequestCommitChanged (PullRequestId 1) (Sha "def")
          state0 = singlePullRequestState (PullRequestId 1) (Sha "abc") "thomas"
          state1 = setBuildStatus (PullRequestId 1) BuildPending state0
          state2 = handleEventFlat event state1
          pr1    = fromJust $ lookupPullRequest (PullRequestId 1) state1
          pr2    = fromJust $ lookupPullRequest (PullRequestId 1) state2
      buildStatus pr1 `shouldBe` BuildPending
      buildStatus pr2 `shouldBe` BuildNotStarted

    it "ignores false positive commit changed events" $ do
      let event  = PullRequestCommitChanged (PullRequestId 1) (Sha "000")
          state0 = singlePullRequestState (PullRequestId 1) (Sha "000") "cindy"
          state1 = setApproval (PullRequestId 1) (Just "daniel") state0
          state2 = setBuildStatus (PullRequestId 1) BuildPending state1
          state3 = handleEventFlat event state2
      state3 `shouldBe` state2
      -- TODO: Also assert that no actions are performed.

    it "sets approval after a stamp from a reviewer" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Sha "6412ef5") "toby"
          -- Note: "deckard" is marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "deckard" "LGTM 6412ef5"
          state' = handleEventFlat event state
          pr     = fromJust $ lookupPullRequest (PullRequestId 1) state'
      approvedBy pr `shouldBe` Just "deckard"

    it "does not set approval after a stamp from a non-reviewer" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Sha "6412ef5") "toby"
          -- Note: the comment is a valid LGTM stamp, but "rachael" is not
          -- marked as reviewer in the test config.
          event  = CommentAdded (PullRequestId 1) "rachael" "LGTM 6412ef5"
          state' = handleEventFlat event state
          pr     = fromJust $ lookupPullRequest (PullRequestId 1) state'
      approvedBy pr `shouldBe` Nothing

    it "does not set approval after a random comment" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Sha "6412ef5") "patrick"
          -- Test coments with 2 words and more or less. (The stamp expects
          -- exactly two words.)
          event1 = CommentAdded (PullRequestId 1) "thomas" "We're up all night"
          event2 = CommentAdded (PullRequestId 1) "guyman" "to get"
          event3 = CommentAdded (PullRequestId 1) "thomas" "lucky."
          state' = handleEventsFlat [event1, event2, event3] state
          pr     = fromJust $ lookupPullRequest (PullRequestId 1) state'
      approvedBy pr `shouldBe` Nothing

    it "requires a long enough sha for approval" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Sha "6412ef5") "sacha"
          -- A 6-character sha is not long enough for approval.
          event  = CommentAdded (PullRequestId 1) "richard" "LGTM 6412ef"
          state' = handleEventFlat event state
          pr     = fromJust $ lookupPullRequest (PullRequestId 1) state'
      approvedBy pr `shouldBe` Nothing

    it "handles a build status change of the integration candidate" $ do
      let event  = BuildStatusChanged (Sha "84c") BuildSucceeded
          state  = candidateState (PullRequestId 1) (Sha "a38") "johanna" (Sha "84c")
          state' = handleEventFlat event state
          pr     = fromJust $ lookupPullRequest (PullRequestId 1) state'
      buildStatus pr `shouldBe` BuildSucceeded

    it "ignores a build status change of random shas" $ do
      let event0 = PullRequestOpened (PullRequestId 2) (Sha "0ad") "harry"
          event1 = BuildStatusChanged (Sha "0ad") BuildSucceeded
          state  = candidateState (PullRequestId 1) (Sha "a38") "harry" (Sha "84c")
          state' = handleEventsFlat [event0, event1] state
          pr1    = fromJust $ lookupPullRequest (PullRequestId 1) state'
          pr2    = fromJust $ lookupPullRequest (PullRequestId 2) state'
      -- Even though the build status changed for "0ad" which is a known commit,
      -- only the build status of the integration candidate can be changed.
      buildStatus pr1 `shouldBe` BuildNotStarted
      buildStatus pr2 `shouldBe` BuildNotStarted

  describe "Logic.proceedUntilFixedPoint" $ do

    it "finds a new candidate" $ do
      let state = setApproval (PullRequestId 1) (Just "fred") $
                  singlePullRequestState (PullRequestId 1) (Sha "f34") "sally"
          (state', actions)  = proceedUntilFixedPointFlat (Just (Sha "38c")) PushRejected state
          (prId, pullRequest) = fromJust $ getIntegrationCandidate state'
      prId                          `shouldBe` PullRequestId 1
      buildStatus pullRequest       `shouldBe` BuildPending
      integrationStatus pullRequest `shouldBe` Integrated (Sha "38c")
      actions `shouldBe` [ATryIntegrate (Branch "refs/pull/1/head", Sha "f34")]

    it "pushes after a successful build" $ do
      let pullRequest = PullRequest
            {
              sha               = Sha "f35",
              author            = "rachael",
              approvedBy        = Just "deckard",
              buildStatus       = BuildSucceeded,
              integrationStatus = Integrated (Sha "38d")
            }
          state = ProjectState
            {
              pullRequests         = IntMap.singleton 1 pullRequest,
              integrationCandidate = Just $ PullRequestId 1
            }
          (state', actions) = proceedUntilFixedPointFlat (Just (Sha "38e")) PushOk state
          candidate         = getIntegrationCandidate state'
      -- After a successful push, the candidate should be gone.
      candidate `shouldBe` Nothing
      actions   `shouldBe` [APushNewHead (Sha "38d")]

    it "restarts the sequence after a rejected push" $ do
      -- Set up a pull request that has gone through the review and build cycle,
      -- and is ready to be pushed to master.
      let pullRequest = PullRequest
            {
              sha               = Sha "f35",
              author            = "rachael",
              approvedBy        = Just "deckard",
              buildStatus       = BuildSucceeded,
              integrationStatus = Integrated (Sha "38d")
            }
          state = ProjectState
            {
              pullRequests         = IntMap.singleton 1 pullRequest,
              integrationCandidate = Just $ PullRequestId 1
            }
          -- Run 'proceedUntilFixedPoint', and pretend that pushes fail (because
          -- something was pushed in the mean time, for instance).
          (state', actions) = proceedUntilFixedPointFlat (Just (Sha "38e")) PushRejected state
          (_, pullRequest') = fromJust $ getIntegrationCandidate state'

      integrationStatus pullRequest' `shouldBe` Integrated (Sha "38e")
      buildStatus       pullRequest' `shouldBe` BuildPending
      actions `shouldBe` [ APushNewHead (Sha "38d")
                         , ATryIntegrate (Branch "refs/pull/1/head", Sha "f35")
                         ]

    it "picks a new candidate from the queue after a successful push" $ do
      let pullRequest1 = PullRequest
            {
              sha               = Sha "f35",
              author            = "rachael",
              approvedBy        = Just "deckard",
              buildStatus       = BuildSucceeded,
              integrationStatus = Integrated (Sha "38d")
            }
          pullRequest2 = PullRequest
            {
              sha               = Sha "f37",
              author            = "rachael",
              approvedBy        = Just "deckard",
              buildStatus       = BuildNotStarted,
              integrationStatus = NotIntegrated
            }
          prMap = IntMap.fromList [(1, pullRequest1), (2, pullRequest2)]
          -- After a successful push, the state of pull request 1 will still be
          -- BuildSucceeded and Integrated, but the candidate will be Nothing.
          state = ProjectState
            {
              pullRequests         = prMap,
              integrationCandidate = Nothing
            }
          -- Proceeding should pick the next pull request as candidate.
          (state', actions) = proceedUntilFixedPointFlat (Just (Sha "38e")) PushOk state
          Just (cId, _candidate) = getIntegrationCandidate state'
      cId     `shouldBe` PullRequestId 2
      actions `shouldBe` [ATryIntegrate (Branch "refs/pull/2/head", Sha "f37")]

  describe "Github.PullRequestPayload" $ do

    it "should be parsed correctly" $ do
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
          prAuthor   = Github.author     (payload :: PullRequestPayload)
      action     `shouldBe` Github.Opened
      owner      `shouldBe` "baxterthehacker"
      repository `shouldBe` "public-repo"
      number     `shouldBe` 1
      headSha    `shouldBe` (Sha "0d1a26e67d8f5eaf1f6ba5c57fc3c7d91ac0fd1c")
      prAuthor   `shouldBe` "baxterthehacker2"

  describe "Github.CommentPayload" $ do

    it "should be parsed correctly" $ do
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

  describe "Github.CommitStatusPayload" $ do

    it "should be parsed correctly" $ do
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

  describe "EventLoop.convertGithubEvent" $ do

    let testPullRequestPayload action = Github.PullRequestPayload
          { action     = action
          , owner      = "deckard"
          , repository = "repo"
          , number     = 1
          , sha        = Sha "b26354"
          , author     = "rachael"
          }

    it "should convert a pull request opened event" $ do
      let payload = testPullRequestPayload Github.Opened
          Just event = convertGithubEvent $ Github.PullRequest payload
      event `shouldBe` (PullRequestOpened (PullRequestId 1) (Sha "b26354") "rachael")

    it "should convert a pull request reopened event" $ do
      let payload = testPullRequestPayload Github.Reopened
          Just event = convertGithubEvent $ Github.PullRequest payload
      -- Reopened is treated just like opened, there is no memory in the system.
      event `shouldBe` (PullRequestOpened (PullRequestId 1) (Sha "b26354") "rachael")

    it "should convert a pull request closed event" $ do
      let payload = testPullRequestPayload Github.Closed
          Just event = convertGithubEvent $ Github.PullRequest payload
      event `shouldBe` (PullRequestClosed (PullRequestId 1))

    it "should convert a pull request synchronize event" $ do
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

    it "should convert a comment created event" $ do
      let payload = testCommentPayload Github.Created
          Just event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` (CommentAdded (PullRequestId 1) "deckard" "Must be expensive.")

    it "should ignore a comment edited event" $ do
      let payload = testCommentPayload Github.Edited
          event = convertGithubEvent $ Github.Comment payload
      event `shouldBe` Nothing

    it "should ignore a comment deleted event" $ do
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

    it "should convert a commit status pending event" $ do
      let payload = testCommitStatusPayload Github.Pending
          Just event = convertGithubEvent $ Github.CommitStatus payload
      event `shouldBe` (BuildStatusChanged (Sha "b26354") BuildPending)

    it "should convert a commit status success event" $ do
      let payload = testCommitStatusPayload Github.Success
          Just event = convertGithubEvent $ Github.CommitStatus payload
      event `shouldBe` (BuildStatusChanged (Sha "b26354") BuildSucceeded)

    it "should convert a commit status failure event" $ do
      let payload = testCommitStatusPayload Github.Failure
          Just event = convertGithubEvent $ Github.CommitStatus payload
      event `shouldBe` (BuildStatusChanged (Sha "b26354") BuildFailed)

    it "should convert a commit status error event" $ do
      let payload = testCommitStatusPayload Github.Error
          Just event = convertGithubEvent $ Github.CommitStatus payload
      -- The error and failure statuses are both converted to "failed".
      event `shouldBe` (BuildStatusChanged (Sha "b26354") BuildFailed)
