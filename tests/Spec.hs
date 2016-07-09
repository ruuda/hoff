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
          state  = emptyProjectState { integrationCandidate = Just $ PullRequestId 1 }
          state' = handleEvent event state
      integrationCandidate state' `shouldBe` Nothing

    it "does not modify the integration candidate if a different PR was closed" $ do
      let event  = PullRequestClosed (PullRequestId 1)
          state  = emptyProjectState { integrationCandidate = Just $ PullRequestId 2 }
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
