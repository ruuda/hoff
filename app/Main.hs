-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)

import Configuration (Configuration)
import EventLoop (runGithubEventLoop, runLogicEventLoop)
import Project (emptyProjectState, saveProjectState)
import Server (buildServer)

import qualified Configuration as Config
import qualified Github
import qualified Logic

withConfig :: (Configuration -> IO ()) -> IO ()
withConfig handler = do
  maybeConfig <- Config.loadConfiguration "config.json"
  case maybeConfig of
    Nothing     -> putStrLn "failed to load configuration"
    Just config -> do
      putStrLn $ "configuration: " ++ (show config)
      handler config

main :: IO ()
main = withConfig $ \ config -> do
  saveProjectState "project.json" emptyProjectState

  -- Create an event queue for GitHub webhook events. The server enqueues events
  -- here when a webhook is received, and a worker thread will process these
  -- events. Limit the number of queued events to 10 to avoid overloading the
  -- server: new hooks are rejected when the queue is full. Webhooks are
  -- low-volume (in the range of ~once per minute) and processing events
  -- should be fast (a few milliseconds, or perhaps a few seconds for a heavy
  -- Git operation), so the queue is expected to be empty most of the time.
  ghQueue <- Github.newEventQueue 10

  -- Events do not stay in the webhook queue for long: they are converted into
  -- logic events and put in the main queue, where the main event loop will
  -- process them. This conversion process does not reject events, but it blocks
  -- if the main queue is full (which will cause the webhook queue to fill up,
  -- so the server will reject new events).
  mainQueue <- Logic.newEventQueue 10

  -- Start a worker thread to put the GitHub webhook events in the main queue.
  -- Discard events that are not intended for the configured repository.
  let owner        = Config.owner config
      repository   = Config.repository config
      enqueueEvent = Logic.enqueueEvent mainQueue
  _ <- forkIO $ runStdoutLoggingT
              $ runGithubEventLoop owner repository ghQueue enqueueEvent

  -- When the event loop wants to persist the current project state,
  -- save it to project.json.
  let persist = liftIO . saveProjectState "project.json"

  -- Start a worker thread to run the main event loop.
  -- TODO: Load previous state from disk.
  _ <- forkIO $ void
              $ runStdoutLoggingT
              $ runLogicEventLoop config persist mainQueue emptyProjectState

  let port = Config.port config
  putStrLn $ "Listening for webhooks on port " ++ (show port)
  runServer <- fmap fst $ buildServer port ghQueue
  runServer

  -- Note that a stop signal is never enqueued. The application just runs until
  -- it is killed.
