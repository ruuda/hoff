-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Server (runServer) where

import GitHub (PullRequestPayload)
import Network.HTTP.Types (status400, status404)
import Network.Wai.Handler.Warp (run)
import Web.Scotty (ActionM, ScottyM, get, jsonData, notFound, post, scottyApp, status, text)

-- Router for the web server.
router :: ScottyM ()
router = do
  post "/hook/github" serveGitHubWebhook
  get  "/hook/github" serveWebhookDocs
  get  "/"            serveWebInterface
  notFound            serveNotFound

serveGitHubWebhook :: ActionM ()
serveGitHubWebhook = do
  payload <- jsonData :: ActionM PullRequestPayload
  text "hook received"

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

-- Runs a webserver at the specified port.
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Listening for webhooks on port " ++ (show port)
  app <- scottyApp router
  run port app
