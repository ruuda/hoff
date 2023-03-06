{-# LANGUAGE OverloadedStrings #-}

module ProjectSpec (projectSpec) where

import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)

import Test.QuickCheck.Instances.Text ()

import qualified Project
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map

projectSpec :: Spec
projectSpec = do
  describe "Project.subMapByOwner" $ do
    prop "the ord instance guarentees owners are grouped together" $ \(owners, repositories) -> do
      let projectInfos = [Project.ProjectInfo owner repo | owner <- owners, repo <- repositories]
          projectInfoMap = Map.fromList (map (\p -> (p, Project.repository p)) projectInfos)

      forM_ owners $ \owner -> do
        Project.subMapByOwner owner projectInfoMap `shouldBe`
          Map.filterWithKey (\key _ -> Project.owner key == owner) projectInfoMap
