-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration (loadConfiguration)
import Logic (Event (..), handleEvent)
import Project (PullRequestId (..), Sha (..), emptyProjectState, saveProjectState)

main :: IO ()
main = do
  maybeConfig <- loadConfiguration "config.json"
  case maybeConfig of
    Just config -> putStrLn $ show config
    Nothing     -> putStrLn "failed to load configuration"
  saveProjectState "project.json" emptyProjectState
