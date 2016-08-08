-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

-- This file contains sort-of end-to-end tests for the server. A real server is
-- run, and the events is produces are tested, but they are not fed into an
-- actual event loop. The purpose of these tests it not to test the *logic* of
-- the server (e.g. proper parsing of the request payload), there are unit tests
-- for that.

module ServerSpec (serverSpec) where

import Control.Concurrent (forkIO, killThread)
import Test.Hspec

import Server (runServer)

import qualified Github

testPort :: Int
testPort = 5273

withServer :: (Github.EventQueue -> IO ()) -> IO ()
withServer body = do
  -- Create an event queue with a capacity of 5 events.
  ghQueue      <- Github.newEventQueue 5

  -- Start the server on the test port, run the body with access to the queue,
  -- and stop the server afterwards by killing the thread.
  serverThread <- forkIO $ runServer testPort ghQueue
  body ghQueue
  killThread serverThread

serverSpec :: Spec
serverSpec = parallel $ do
  describe "The webhook server" $ do

    it "serves 'not found' at a non-existing url" $
      withServer $ \ _ghQueue -> do
        -- TODO: Send an actual request, inspect the response.
        True `shouldBe` True
