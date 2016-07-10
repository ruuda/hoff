-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (fromJust)
import Data.Text (Text)
import Test.Hspec

import Logic
import Project

singlePullRequestState :: PullRequestId -> Sha -> Text -> ProjectState
singlePullRequestState pr prSha prAuthor =
  let event = PullRequestOpened pr prSha prAuthor
  in  handleEvent event emptyProjectState

candidateState :: PullRequestId -> Sha -> Text -> Sha -> ProjectState
candidateState pr prSha prAuthor candidateSha =
  let state0 = singlePullRequestState pr prSha prAuthor
      state1 = setIntegrationStatus pr (Integrated candidateSha) state0
  in  state1 { integrationCandidate = Just pr }

main :: IO ()
main = hspec $ do
  describe "Logic.handleEvent" $ do

    it "handles PullRequestOpened" $ do
      let event = PullRequestOpened (PullRequestId 3) (Sha "e0f") "lisa"
          state = handleEvent event emptyProjectState
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
          state  = foldr handleEvent emptyProjectState [event3, event2, event1]
      state `shouldSatisfy` not . existsPullRequest (PullRequestId 1)
      state `shouldSatisfy` existsPullRequest (PullRequestId 2)

    it "handles closing the integration candidate PR" $ do
      let event  = PullRequestClosed (PullRequestId 1)
          state  = candidateState (PullRequestId 1) (Sha "ea0") "frank" (Sha "cf4")
          state' = handleEvent event state
      integrationCandidate state' `shouldBe` Nothing

    it "does not modify the integration candidate if a different PR was closed" $ do
      let event  = PullRequestClosed (PullRequestId 1)
          state  = candidateState (PullRequestId 2) (Sha "a38") "franz" (Sha "ed0")
          state' = handleEvent event state
      integrationCandidate state' `shouldBe` (Just $ PullRequestId 2)

    it "loses approval after the PR commit has changed" $ do
      let event  = PullRequestCommitChanged (PullRequestId 1) (Sha "def")
          state0 = singlePullRequestState (PullRequestId 1) (Sha "abc") "alice"
          state1 = setApproval (PullRequestId 1) (Just "hatter") state0
          state2 = handleEvent event state1
          pr1    = fromJust $ lookupPullRequest (PullRequestId 1) state1
          pr2    = fromJust $ lookupPullRequest (PullRequestId 1) state2
      approvedBy pr1 `shouldBe` Just "hatter"
      approvedBy pr2 `shouldBe` Nothing

    it "resets the build status after the PR commit has changed" $ do
      let event  = PullRequestCommitChanged (PullRequestId 1) (Sha "def")
          state0 = singlePullRequestState (PullRequestId 1) (Sha "abc") "thomas"
          state1 = setBuildStatus (PullRequestId 1) BuildQueued state0
          state2 = handleEvent event state1
          pr1    = fromJust $ lookupPullRequest (PullRequestId 1) state1
          pr2    = fromJust $ lookupPullRequest (PullRequestId 1) state2
      buildStatus pr1 `shouldBe` BuildQueued
      buildStatus pr2 `shouldBe` BuildNotStarted

    it "sets approval after a comment containing an approval stamp" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Sha "6412ef5") "toby"
          event  = CommentAdded (PullRequestId 1) "marie" "LGTM 6412ef5"
          state' = handleEvent event state
          pr     = fromJust $ lookupPullRequest (PullRequestId 1) state'
      approvedBy pr `shouldBe` Just "marie"

    it "does not set approval after a random comment" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Sha "6412ef5") "patrick"
          -- Test coments with 2 words and more or less. (The stamp expects
          -- exactly two words.)
          event3 = CommentAdded (PullRequestId 1) "thomas" "We're up all night"
          event2 = CommentAdded (PullRequestId 1) "guyman" "to get"
          event1 = CommentAdded (PullRequestId 1) "thomas" "lucky."
          state' = foldr handleEvent state [event1, event2, event3]
          pr     = fromJust $ lookupPullRequest (PullRequestId 1) state'
      approvedBy pr `shouldBe` Nothing

    it "requires a long enough sha for approval" $ do
      let state  = singlePullRequestState (PullRequestId 1) (Sha "6412ef5") "sacha"
          -- A 6-character sha is not long enough for approval.
          event  = CommentAdded (PullRequestId 1) "richard" "LGTM 6412ef"
          state' = handleEvent event state
          pr     = fromJust $ lookupPullRequest (PullRequestId 1) state'
      approvedBy pr `shouldBe` Nothing

    it "handles a build status change of the integration candidate" $ do
      let event  = BuildStatusChanged (Sha "84c") BuildSucceeded
          state  = candidateState (PullRequestId 1) (Sha "a38") "johanna" (Sha "84c")
          state' = handleEvent event state
          pr     = fromJust $ lookupPullRequest (PullRequestId 1) state'
      buildStatus pr `shouldBe` BuildSucceeded

    it "ignores a build status change of random shas" $ do
      let event0 = PullRequestOpened (PullRequestId 2) (Sha "0ad") "harry"
          event1 = BuildStatusChanged (Sha "0ad") BuildSucceeded
          state  = candidateState (PullRequestId 1) (Sha "a38") "harry" (Sha "84c")
          state' = handleEvent event1 $ handleEvent event0 state
          pr1    = fromJust $ lookupPullRequest (PullRequestId 1) state'
          pr2    = fromJust $ lookupPullRequest (PullRequestId 2) state'
      -- Even though the build status changed for "0ad" which is a known commit,
      -- only the build status of the integration candidate can be changed.
      buildStatus pr1 `shouldBe` BuildNotStarted
      buildStatus pr2 `shouldBe` BuildNotStarted
