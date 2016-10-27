-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.List (zip4)
import System.Exit (die)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

import qualified System.Directory as FileSystem
import qualified System.Environment

import Configuration (Configuration)
import EventLoop (runGithubEventLoop, runLogicEventLoop)
import Project (ProjectState, emptyProjectState, loadProjectState, saveProjectState)
import Project (ProjectInfo (ProjectInfo))
import Server (buildServer)

import qualified Configuration as Config
import qualified Github
import qualified Logic

getConfigFilePathOrExit :: IO FilePath
getConfigFilePathOrExit = do
  args <- System.Environment.getArgs
  case args of
    fname : _ -> do
      exists <- FileSystem.doesFileExist fname
      if exists then
        return fname
      else
        die $ "Cannot load configuration: the file '" ++ fname ++ "' does not exist."
    [] ->
      die $ "No config file specified.\n" ++
            "The first argument must be the path to the config file."

loadConfigOrExit :: FilePath -> IO Configuration
loadConfigOrExit fname = do
  maybeConfig <- Config.loadConfiguration fname
  case maybeConfig of
    Just config -> return config
    Nothing -> die $ "Failed to parse configuration file '" ++ fname ++ "'."

initializeProjectState :: FilePath -> IO ProjectState
initializeProjectState fname = do
  exists <- FileSystem.doesFileExist fname
  if exists then do
    maybeState <- loadProjectState fname
    case maybeState of
      Just projectState -> do
        putStrLn $ "Loaded project state from '" ++ fname ++ "'."
        return projectState
      Nothing -> do
        -- Fail loudly if something is wrong, and abort the program.
        die $ "Failed to parse project state in '" ++ fname ++ "'.\n" ++
              "Please repair or remove the file."
  else do
    putStrLn $ "File '" ++ fname ++ "' not found, starting with an empty state."
    return emptyProjectState

main :: IO ()
main = do
  -- When the runtime detects that stdout is not connected to a console, it
  -- defaults to block buffering instead of line buffering. When running under
  -- systemd, this prevents log messages (which are written to stdout) from
  -- showing up until the buffer is flushed. Therefore, explicitly select line
  -- buffering, to enforce a flush after every newline.
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- Load configuration from the file specified as first program argument.
  configFilePath <- getConfigFilePathOrExit
  config <- loadConfigOrExit configFilePath

  -- Create an event queue for GitHub webhook events. The server enqueues events
  -- here when a webhook is received, and a worker thread will process these
  -- events. Limit the number of queued events to 10 to avoid overloading the
  -- server: new hooks are rejected when the queue is full. Webhooks are
  -- low-volume (in the range of ~once per minute) and processing events
  -- should be fast (a few milliseconds, or perhaps a few seconds for a heavy
  -- Git operation), so the queue is expected to be empty most of the time.
  ghQueue <- Github.newEventQueue 10

  -- Events do not stay in the webhook queue for long: they are converted into
  -- logic events and put in the project queues, where the main event loop will
  -- process them. This conversion process does not reject events, but it blocks
  -- if the project queue is full (which will cause the webhook queue to fill
  -- up, so the server will reject new events).
  projectQueues <- forM (Config.projects config) $ \ pconfig -> do
    projectQueue <- Logic.newEventQueue 10
    let
      owner        = Config.owner pconfig
      repository   = Config.repository pconfig
      projectInfo  = ProjectInfo owner repository
    return (projectInfo, projectQueue)

  -- Define a function that enqueues an event in the right project queue.
  let
    enqueueEvent projectInfo event =
      -- Call the corresponding enqueue function if the project exists,
      -- otherwise drop the event on the floor.
      maybe (return ()) (\ queue -> Logic.enqueueEvent queue event) $
      -- TODO: This is doing a linear scan over a linked list to find the right
      -- queue. That can be improved. A lot.
      lookup projectInfo projectQueues

  -- Start a worker thread to put the GitHub webhook events in the right queue.
  _ <- forkIO $ runStdoutLoggingT $ runGithubEventLoop ghQueue enqueueEvent

  -- Restore the previous state from disk if possible, or start clean.
  projectStates <- forM (Config.projects config) $ \ pconfig -> do
    -- TODO: Have a per-project state file.
    projectState <- initializeProjectState (Config.stateFile config)
    let
      -- TODO: DRY.
      owner        = Config.owner pconfig
      repository   = Config.repository pconfig
      projectInfo  = ProjectInfo owner repository
    return (projectInfo, projectState)

  -- Keep track of the most recent state for every project, so the webinterface
  -- can use it to serve a status page.
  stateVars <- forM projectStates $ \ (projectInfo, projectState) -> do
    stateVar <- Logic.newStateVar projectState
    return (projectInfo, stateVar)

  -- Start a main event loop for every project.
  let 
    -- TODO: This is very, very ugly. Get these per-project collections sorted
    -- out.
    zipped = zip4 (Config.projects config) projectQueues stateVars projectStates
    tuples = map (\(cfg, (_, a), (_, b), (_, c)) -> (cfg, a, b, c)) zipped
  forM_ tuples $ \ (projectConfig, projectQueue, stateVar, projectState) -> do
    let
      -- When the event loop publishes the current project state, save it to
      -- the configured file, and make the new state available to the
      -- webinterface.
      publish newState = do
        -- TODO: per project state file.
        liftIO $ saveProjectState (Config.stateFile config) newState
        liftIO $ Logic.updateStateVar stateVar newState

      -- When the event loop wants to get the next event, take one off the queue.
      getNextEvent = liftIO $ Logic.dequeueEvent projectQueue

    -- Start a worker thread to run the main event loop for the project.
    forkIO $ void
           $ runStdoutLoggingT
           $ runLogicEventLoop projectConfig getNextEvent publish projectState

  let
    -- When the webhook server receives an event, enqueue it on the webhook
    -- event queue if it is not full.
    ghTryEnqueue = Github.tryEnqueueEvent ghQueue

    -- Allow the webinterface to retrieve the latest project state per project.
    getProjectState projectInfo =
      fmap Logic.readStateVar $ lookup projectInfo stateVars

  let
    port      = Config.port config
    tlsConfig = Config.tls config
    secret    = Config.secret config
    -- TODO: Do this in a cleaner way.
    infos     = fmap (\ pc -> ProjectInfo (Config.owner pc) (Config.repository pc)) $ Config.projects config
  putStrLn $ "Listening for webhooks on port " ++ (show port) ++ "."
  runServer <- fmap fst $ buildServer port tlsConfig infos secret ghTryEnqueue getProjectState
  runServer

  -- Note that a stop signal is never enqueued. The application just runs until
  -- it is killed.
