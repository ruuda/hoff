-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Data.IntMap as IntMap

import Logic
import Project

main :: IO ()
main = hspec $ do
  describe "Logic.handleEvent" $ do

    it "handles PullRequestOpened" $ do
      let event = PullRequestOpened (PullRequestId 3) (Sha "e0f") "lisa"
          state = handleEvent event emptyProjectState
      pullRequestInfo state `shouldSatisfy` IntMap.member 3
      let prInfo  = pullRequestInfo state IntMap.! 3
          prState = pullRequestState state IntMap.! 3
      sha prInfo          `shouldBe` Sha "e0f"
      author prInfo       `shouldBe` "lisa"
      approvedBy prState  `shouldBe` Nothing
      buildStatus prState `shouldBe` BuildNotStarted

    it "handles PullRequestClosed" $ do
      let event1 = PullRequestOpened (PullRequestId 1) (Sha "abc") "peter"
          event2 = PullRequestOpened (PullRequestId 2) (Sha "def") "jack"
          event3 = PullRequestClosed (PullRequestId 1)
          state  = foldr handleEvent emptyProjectState [event3, event2, event1]
      pullRequestInfo  state `shouldSatisfy` not . (IntMap.member 1)
      pullRequestState state `shouldSatisfy` not . (IntMap.member 1)

    it "handles closing the integration candidate PR" $ do
      let event  = PullRequestClosed (PullRequestId 1)
          state  = emptyProjectState { integrationCandidate = Just $ PullRequestId 1 }
          state' = handleEvent event state
      integrationCandidate state' `shouldBe` Nothing
