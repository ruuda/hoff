-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration (loadConfiguration)
import Logic (Event (..), handleEvent)
import Project (PullRequestId (..), Sha (..), exampleState, saveProjectState)

main :: IO ()
main = do
  maybeConfig <- loadConfiguration "config.json"
  case maybeConfig of
    Just config -> putStrLn $ show config
    Nothing     -> putStrLn "failed to load configuration"
  let state = exampleState
  putStrLn $ "state: " ++ (show state)
  let event = PullRequestOpened (PullRequestId 3) (Sha "e0f") "lisa"
  let (state', action) = handleEvent event state
  putStrLn $ "state after event: " ++ (show state')
  saveProjectState "project.json" exampleState
