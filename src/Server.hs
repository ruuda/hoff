-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Server (runServer) where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TBQueue
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types (status400, status404, status503)
import Network.Wai.Handler.Warp (run)
import Web.Scotty (ActionM, ScottyM, get, header, jsonData, notFound, post, scottyApp, status, text)

import qualified Github
import qualified Control.Monad.STM as STM

-- Helper to perform an STM operation from within a Scotty action.
atomically :: STM a -> ActionM a
atomically = liftIO . STM.atomically

-- Router for the web server.
router :: Github.EventQueue -> ScottyM ()
router ghQueue = do
  post "/hook/github" $ serveGithubWebhook ghQueue
  get  "/hook/github" $ serveWebhookDocs
  get  "/"            $ serveWebInterface
  notFound            $ serveNotFound

serveGithubWebhook :: Github.EventQueue -> ActionM ()
serveGithubWebhook ghQueue = do
  eventName <- header "X-GitHub-Event"
  case eventName of
    Just "pull_request" -> do
      payload <- jsonData :: ActionM Github.PullRequestPayload
      serveEnqueueEvent ghQueue $ Github.PullRequest payload
    Just "issue_comment" -> do
      payload <- jsonData :: ActionM Github.CommentPayload
      serveEnqueueEvent ghQueue $ Github.Comment payload
    _ -> do
      text "hook ignored, X-GitHub-Event does not match expected value"

-- Handles replying to the client when a GitHub webhook is received.
serveEnqueueEvent :: Github.EventQueue -> Github.WebhookEvent -> ActionM ()
serveEnqueueEvent ghQueue event = do
  -- Enqueue the event if the queue is not full. Normally writeTBQueue would
  -- block if the queue is full, but instead we don't want to enqueue the event
  -- and tell the client to retry in a while.
  enqueued <- atomically $ do
    isFull <- isFullTBQueue ghQueue
    if isFull
      then return False
      else (writeTBQueue ghQueue event) >> (return True)
  if enqueued
    then text "hook received"
    else do
      status status503 -- 503: Service Unavailable
      text "webhook event queue full, please try again in a few minutes"

serveWebhookDocs :: ActionM ()
serveWebhookDocs = do
  status status400
  text "expecting POST request at /hook/github"

serveWebInterface :: ActionM ()
serveWebInterface = do
  text "not yet implemented"

serveNotFound :: ActionM ()
serveNotFound = do
  status status404
  text "not found"

-- Runs a webserver at the specified port. When GitHub webhooks are received,
-- an event will be added to the event queue.
runServer :: Int -> Github.EventQueue -> IO ()
runServer port ghQueue = do
  putStrLn $ "Listening for webhooks on port " ++ (show port)
  app <- scottyApp $ router ghQueue
  run port app
