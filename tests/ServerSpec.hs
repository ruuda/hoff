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
import Control.Lens (view)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Types.Status (badRequest400, notFound404)
import Network.Wreq.Lens (Response)
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Types as WreqTypes

import Server (buildServer)

import qualified Github

testPort :: Int
testPort = 5273

testHost :: String
testHost = "http://localhost:" ++ (show testPort)

-- The normal Wreq.get function throws when the response is a 404, which is not
-- what we want. The solution is to use custom options, with the 'checkStatus'
-- function set to one that does not throw for non-200 statuses.
noThrowOptions :: Wreq.Options
noThrowOptions = Wreq.defaults { WreqTypes.checkStatus = Just ignoreStatus }
  where
    ignoreStatus _ _ _ = Nothing

httpGet :: String -> IO (Response ByteString)
httpGet = Wreq.getWith noThrowOptions

withServer :: (Github.EventQueue -> IO ()) -> IO ()
withServer body = do
  -- Create an event queue with a capacity of 5 events.
  ghQueue      <- Github.newEventQueue 5

  -- Start the server on the test port, run the body with access to the queue,
  -- and stop the server afterwards by killing the thread.
  (runServer, blockUntilReady) <- buildServer testPort ghQueue
  serverThread <- forkIO runServer
  blockUntilReady
  body ghQueue
  killThread serverThread

serverSpec :: Spec
serverSpec = do
  -- Note: these tests cannot be run in parallel, because the server must
  -- listen on a fixed port. We could give every test its own port, but that
  -- would complicate things unnecessarily.

  describe "The webhook server" $ do

    it "serves 'not found' at a non-existing url" $
      withServer $ \ _ghQueue -> do
        response <- httpGet $ testHost ++ "/bogus/url"
        let statusCode = view Wreq.responseStatus response
        statusCode `shouldBe` notFound404

    it "responds with 'bad request' to a GET for a webhook url" $
      withServer $ \ _ghQueue -> do
        response <- httpGet $ testHost ++ "/hook/github"
        let statusCode = view Wreq.responseStatus response
        statusCode `shouldBe` badRequest400
