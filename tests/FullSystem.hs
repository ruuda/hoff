-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

import System.FilePath ((</>))
import System.Process (callProcess)

import Configuration (Configuration (Configuration))

import qualified Configuration as Config

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

-- Sets up two repositories: one with a few commits in the origin directory, and
-- a clone of that in the repository directory. The clone ensures that the
-- origin repository is set as the "origin" remote in the cloned repository.
populateRepo :: IO ()
populateRepo = do
  callProcess "mkdir" ["-p", originDir]
  gitOrigin ["init"]
  gitOrigin ["config", "user.email", "testsuite@example.com"]
  gitOrigin ["config", "user.name", "Testbot"]

  writeFile (originDir </> "tyrell.txt") "I'm surprised you didn't come here sooner.\n"
  gitOrigin ["add", "tyrell.txt"]
  gitOrigin ["commit", "-m", "Initial commit"]

  writeFile (originDir </> "roy.txt") "It's not an easy thing to meet your maker.\n"
  gitOrigin ["add", "roy.txt"]
  gitOrigin ["commit", "-m", "Add new quote"]

  appendFile (originDir </> "tyrell.txt") "What can he do for you?\n"
  appendFile (originDir </> "roy.txt") "Can the maker repair what he makes?\n"
  gitOrigin ["add", "roy.txt", "tyrell.txt"]
  gitOrigin ["commit", "-m", "Add more quotes"]

  git ["clone", "file://" ++ originDir, repoDir]
  -- TODO: Get commit shas.
  where
    git       args = callProcess "git" args
    gitOrigin args = git $ ["-C", originDir] ++ args

cleanupRepo :: IO ()
cleanupRepo = callProcess "rm" ["-fr", testDir]

main :: IO ()
main = do
  populateRepo
  cleanupRepo
