-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Server (runServer) where

import Data.ByteString (ByteString)
import Network.HTTP.Types (methodPost, status200, status400, status404)
import Network.HTTP.Types.Header (HeaderName, hContentType)
import Network.Wai (Application, requestMethod, responseLBS, pathInfo)
import Network.Wai.Handler.Warp (run)

plainTextHeaders :: [(HeaderName, ByteString)]
plainTextHeaders = [(hContentType, "text/plain")]

-- Router for the web server:
--  * The GitHub webhook endpoint is at /hook/github.
--  * The webinterface is at /.
router :: Application
router request = case pathInfo request of
  "hook" : "github" : [] -> githubWebhook request
  []                     -> webInterface request
  _                      -> notFound request

-- Serves the GitHub webhook endpoint.
githubWebhook :: Application
githubWebhook request f = case requestMethod request of
  m | m == methodPost  -> f $ responseLBS status200 plainTextHeaders "hook received"
  _ | otherwise        -> f $ responseLBS status400 plainTextHeaders "expecting POST request at /hook/github"

-- Serves the webinterface.
webInterface :: Application
webInterface _request f =
  f $ responseLBS status200 plainTextHeaders "not yet implemented"

-- Fallback if no route matched.
notFound :: Application
notFound _request f =
  f $ responseLBS status404 plainTextHeaders "not found"

-- Runs a webserver at the specified port.
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Listening for webhooks on port " ++ (show port)
  run port router
