-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Server (runServer) where

import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, responseLBS, pathInfo)
import Network.Wai.Handler.Warp (run)

-- Router for the web server:
--  * The GitHub webhook endpoint is at /hook/github.
--  * The webinterface is at /.
router :: Application
router request = case pathInfo request of
  "hook" : "github" : [] -> githubWebhook request
  []                     -> webInterface request
  otherpath              -> notFound request

-- Serves the GitHub webhook endpoint.
githubWebhook :: Application
githubWebhook request f =
  f $ responseLBS status200 [(hContentType, "text/plain")] "hook received"

-- Serves the webinterface.
webInterface :: Application
webInterface request f =
  f $ responseLBS status200 [(hContentType, "text/plain")] "not yet implemented"

-- Fallback if no route matched.
notFound :: Application
notFound request f =
  f $ responseLBS status404 [(hContentType, "text/plain")] "not found"

-- Runs a webserver at the specified port.
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Listening for webhooks on port " ++ (show port)
  run port router
