-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Server (runServer) where

import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)

webhookListener :: Application
webhookListener request f =
  f $ responseLBS status200 [(hContentType, "text/plain")] "hi"

runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Listening for webhooks on port " ++ (show port)
  run port webhookListener
