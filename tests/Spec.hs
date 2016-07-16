-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Free (Free (..))
import Data.Foldable (foldlM)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Test.Hspec

import Git (Sha (..))
import Logic
import Project

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
  = ATryIntegrate Sha
  | ALeaveComment PullRequestId Text
  deriving (Eq, Show)

-- This function simulates running the actions, and returns the final state,
-- together with a list of all actions that would have been performed. Some
-- actions require input from the outside world. Simulating these actions will
-- return the pushResult and rebaseResult passed in here.
runActionWithInit :: Maybe Sha -> [ActionFlat] -> Action a -> (a, [ActionFlat])
runActionWithInit integrateResult actions action =
  let prepend cont act =
        let (result, acts') = runActionWithInit integrateResult actions cont
        in  (result, act : acts')
  in case action of
    Pure result                   -> (result, [])
    Free (TryIntegrate trySha h)  -> prepend (h integrateResult) $ ATryIntegrate trySha
    Free (LeaveComment pr body x) -> prepend x $ ALeaveComment pr body

-- Simulates running the action. Pretends that integration always conflicts.
runAction :: Action a -> (a, [ActionFlat])
runAction = runActionWithInit Nothing []

-- TODO: Do not ignore actions information, assert that certain events do not
-- have undesirable side effects.
getState :: Action ProjectState -> ProjectState
getState = fst . runAction

-- Handle an event and simulate its side effects, then ignore the side effects
-- and return the new state.
handleEventFlat :: Event -> ProjectState -> ProjectState
handleEventFlat event state = getState $ handleEvent event state

-- Handle events and simulate their side effects, then ignore the side effects
-- and return the new state.
handleEventsFlat :: [Event] -> ProjectState -> ProjectState
handleEventsFlat events state = getState $ foldlM (flip handleEvent) state events

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

    it "sets approval after a comment containing an approval stamp" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Sha "6412ef5") "toby"
          event  = CommentAdded (PullRequestId 1) "marie" "LGTM 6412ef5"
          state' = handleEventFlat event state
          pr     = fromJust $ lookupPullRequest (PullRequestId 1) state'
      approvedBy pr `shouldBe` Just "marie"

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
