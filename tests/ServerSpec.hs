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

import Control.Concurrent.Async (async, asyncThreadId, waitCatch)
import Control.Concurrent (killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (tryReadTBQueue)
import Control.Lens (view)
import Data.ByteString.Lazy (fromStrict)
import Data.Digest.Pure.SHA (hmacSha256)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Header (Header, HeaderName, hContentType)
import Network.HTTP.Types.Status (badRequest400, notFound404, ok200)
import Network.Wreq.Lens (Response)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import qualified Data.ByteString.Char8 as ByteString.Strict
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Types as WreqTypes

import Server (buildServer)

import qualified Github

-- Bring a tiny bit of sense into the Haskell string type madness.
type LazyByteString = ByteString.Lazy.ByteString
type StrictByteString = ByteString.Strict.ByteString

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

httpGet :: String -> IO (Response LazyByteString)
httpGet = Wreq.getWith noThrowOptions

httpPost :: WreqTypes.Postable p => String -> [Header] -> p -> IO (Response LazyByteString)
httpPost url headers body = Wreq.postWith options url body
  where
    options = noThrowOptions { WreqTypes.headers = headers }

hGithubEvent :: HeaderName
hGithubEvent = "X-GitHub-Event"

hGithubSignature :: HeaderName
hGithubSignature = "X-Hub-Signature" -- Not a typo, really 'Hub', not 'GitHub'.

-- Why three different string types? The secret is Text, which will be encoded
-- as utf-8 to provide the key for the mac. The message is the data to be
-- posted, and Wreq expects a lazy bytestring here (TODO: Or can I use a strict
-- one too?). The result must be put in a http header, and the http-types
-- package chose to use strict bytestrings for those. So yeah, it's a mess.
computeSignature :: Text -> LazyByteString -> StrictByteString
computeSignature secret message =
  let secretAsBytes = fromStrict $ encodeUtf8 secret
  in  ByteString.Strict.pack $ show $ hmacSha256 secretAsBytes message

-- Pops one event from the queue, assuming there is already an event there. This
-- does not block and wait for an event to arrive, because that could make tests
-- deadlock in case an event is never pushed.
popQueue :: Github.EventQueue -> IO Github.WebhookEvent
popQueue = fmap fromJust . atomically . tryReadTBQueue

isPullRequestEvent :: Github.WebhookEvent -> Bool
isPullRequestEvent event = case event of
  Github.PullRequest _ -> True
  _                    -> False

withServer :: (Github.EventQueue -> IO ()) -> IO ()
withServer body = do
  -- Create an event queue with a capacity of 5 events.
  ghQueue      <- Github.newEventQueue 5

  -- Start the server on the test port, wait until it is ready to handle
  -- requests, and then run the body with access to the queue.
  (runServer, blockUntilReady) <- buildServer testPort ghQueue
  serverAsync <- async runServer
  blockUntilReady
  body ghQueue

  -- Stop the server by killing the thread. This will not immediately stop the
  -- server, but we do need to ensure that the port is available after we exit
  -- the function, so the next test can use it, so also wait for the thread to
  -- finish after killing it.
  killThread $ asyncThreadId serverAsync
  _ <- waitCatch serverAsync
  return ()

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

    it "accepts a pull_request webhook" $
      withServer $ \ ghQueue -> do
        examplePayload <- ByteString.Lazy.readFile "tests/data/pull-request-payload.json"
        let signature = computeSignature "secret" examplePayload
            headers   = [ (hContentType,     "application/json")
                        , (hGithubEvent,     "pull_request")
                        , (hGithubSignature, signature) ]
        response <- httpPost (testHost ++ "/hook/github") headers examplePayload
        event    <- popQueue ghQueue
        -- Only check that an event was received, there are unit tests already
        -- that verify that a request was parsed correctly.
        (view Wreq.responseBody response) `shouldBe` "hook received"
        (view Wreq.responseStatus response) `shouldBe` ok200
        event `shouldSatisfy` isPullRequestEvent
