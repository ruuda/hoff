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
