-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module EventLoop (runGitHubEventLoop) where

import Control.Concurrent.STM.TBQueue
import Control.Monad.STM (atomically)

import qualified GitHub

runGitHubEventLoop :: GitHub.EventQueue -> IO ()
runGitHubEventLoop ghQueue = do
  event <- atomically $ readTBQueue ghQueue
  putStrLn $ "received event: " ++ (show event)
  runGitHubEventLoop ghQueue
