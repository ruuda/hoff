-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration (loadConfiguration)
import GitHub (newEventQueue)
import Project (emptyProjectState, saveProjectState)
import Server (runServer)

main :: IO ()
main = do
  maybeConfig <- loadConfiguration "config.json"
  case maybeConfig of
    Just config -> putStrLn $ show config
    Nothing     -> putStrLn "failed to load configuration"
  saveProjectState "project.json" emptyProjectState

  -- Create an event queue for GitHub webhook events. The server enqueues events
  -- here when a webhook is received, and a worker thread will process these
  -- events. Limit the number of queued events to 10 to avoid overloading the
  -- server: new hooks are rejected when the queue is full. Webhooks are
  -- low-volume (in the range of ~once per minute) and processing events
  -- should be fast (a few milliseconds, or perhaps a few seconds for a heavy
  -- Git operation), so the queue is expected to be empty most of the time.
  ghQueue <- newEventQueue 10

  runServer 3000 ghQueue
