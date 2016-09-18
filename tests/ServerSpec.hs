-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

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
import Control.Monad (replicateM_)
import Crypto.Hash.Algorithms (SHA1)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Header (Header, HeaderName, hContentType)
import Network.HTTP.Types.Status (badRequest400, internalServerError500)
import Network.HTTP.Types.Status (notFound404, notImplemented501, ok200)
import Network.HTTP.Types.Status (serviceUnavailable503)
import Network.Wreq.Lens (Response)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import qualified Data.ByteString.Char8 as ByteString.Strict
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Types as WreqTypes

import Server (buildServer)

import qualified Github
import qualified Project

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

-- Peforms an http get request. The host is prepended to the url automatically.
httpGet :: String -> IO (Response LazyByteString)
httpGet url = Wreq.getWith noThrowOptions (testHost ++ url)

-- Peforms an http post request. The host is prepended to the url automatically.
httpPost :: WreqTypes.Postable p => String -> [Header] -> p -> IO (Response LazyByteString)
httpPost url headers body = Wreq.postWith options (testHost ++ url) body
  where
    options = noThrowOptions { WreqTypes.headers = headers }

hGithubEvent :: HeaderName
hGithubEvent = "X-GitHub-Event"

hGithubSignature :: HeaderName
hGithubSignature = "X-Hub-Signature" -- Not a typo, really 'Hub', not 'GitHub'.

testSecret :: Text
testSecret = "N6MAC41717"

-- Why three different string types? The secret is Text, which will be encoded
-- as utf-8 to provide the key for the mac. The message is the data to be
-- posted, and Wreq expects a lazy bytestring here (TODO: Or can I use a strict
-- one too?). The result must be put in a http header, and the http-types
-- package chose to use strict bytestrings for those. So yeah, it's a mess.
computeSignature :: Text -> StrictByteString -> StrictByteString
computeSignature secret message =
  let digest = hmac (encodeUtf8 secret) message :: HMAC SHA1
  in  ByteString.Strict.pack $ "sha1=" ++ (show $ hmacGetDigest digest)

-- Peforms an http post request for an event with the given body payload. The
-- host is prepended to the url automatically. (Also, three different string
-- types in one signature ... please ecosystem, can we sort this out?)
httpPostGithubEvent :: String -> StrictByteString -> StrictByteString -> IO (Response LazyByteString)
httpPostGithubEvent url eventName body =
  let signature = computeSignature testSecret body
      headers   = [ (hContentType, "application/json")
                  , (hGithubEvent, eventName)
                  , (hGithubSignature, signature) ]
  in  httpPost url headers body

-- Pops one event from the queue, assuming there is already an event there. This
-- does not block and wait for an event to arrive, because that could make tests
-- deadlock in case an event is never pushed.
popQueue :: Github.EventQueue -> IO Github.WebhookEvent
popQueue = fmap fromJust . atomically . tryReadTBQueue

isPullRequestEvent :: Github.WebhookEvent -> Bool
isPullRequestEvent event = case event of
  Github.PullRequest _ -> True
  _                    -> False

isCommitStatusEvent :: Github.WebhookEvent -> Bool
isCommitStatusEvent event = case event of
  Github.CommitStatus _ -> True
  _                     -> False

withServer :: (Github.EventQueue -> IO ()) -> IO ()
withServer body = do
  -- Create an event queue with a capacity of 5 events.
  ghQueue      <- Github.newEventQueue 5

  let
    info = Project.ProjectInfo "deckard" "voight-kampff"
    tryEnqueue = Github.tryEnqueueEvent ghQueue
    -- Fake the project state, always return the empty state.
    getProjectState = return Project.emptyProjectState

  -- Start the server on the test port, wait until it is ready to handle
  -- requests, and then run the body with access to the queue.
  (runServer, blockUntilReady) <-
    buildServer testPort info testSecret tryEnqueue getProjectState
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

    it "serves something at the root" $
      withServer $ \ _ghQueue -> do
        response <- httpGet "/"
        let statusCode = view Wreq.responseStatus response
        statusCode `shouldBe` ok200

    it "serves 'not found' at a non-existing url" $
      withServer $ \ _ghQueue -> do
        response <- httpGet "/bogus/url"
        let statusCode = view Wreq.responseStatus response
        statusCode `shouldBe` notFound404

    it "responds with 'bad request' to a GET for a webhook url" $
      withServer $ \ _ghQueue -> do
        response <- httpGet "/hook/github"
        let statusCode = view Wreq.responseStatus response
        statusCode `shouldBe` badRequest400

    it "accepts a pull_request webhook" $
      withServer $ \ ghQueue -> do
        payload  <- ByteString.Strict.readFile "tests/data/pull-request-payload.json"
        response <- httpPostGithubEvent "/hook/github" "pull_request" payload
        event    <- popQueue ghQueue
        -- Only check that an event was received, there are unit tests already
        -- that verify that a request was parsed correctly.
        (view Wreq.responseBody response) `shouldBe` "hook received"
        (view Wreq.responseStatus response) `shouldBe` ok200
        event `shouldSatisfy` isPullRequestEvent

    it "accepts a (commit) status webhook" $
      withServer $ \ ghQueue -> do
        payload  <- ByteString.Strict.readFile "tests/data/status-payload.json"
        response <- httpPostGithubEvent "/hook/github" "status" payload
        event    <- popQueue ghQueue
        -- Only check that an event was received, there are unit tests already
        -- that verify that a request was parsed correctly.
        (view Wreq.responseBody response) `shouldBe` "hook received"
        (view Wreq.responseStatus response) `shouldBe` ok200
        event `shouldSatisfy` isCommitStatusEvent

    it "serves 503 service unavailable when the queue is full" $
      withServer $ \ ghQueue -> do
        payload  <- ByteString.Strict.readFile "tests/data/pull-request-payload.json"

        -- The first five responses should be accepted, which will fill up the
        -- queue (that has a capacity of 5 in these tests).
        replicateM_ 5 $ do
          resp <- httpPostGithubEvent "/hook/github" "pull_request" payload
          (view Wreq.responseStatus resp) `shouldBe` ok200

        -- The next request should therefore be denied.
        resp6 <- httpPostGithubEvent "/hook/github" "pull_request" payload
        (view Wreq.responseStatus resp6) `shouldBe` serviceUnavailable503

        -- After popping one event, a new request should be allowed.
        _     <- popQueue ghQueue
        resp7 <- httpPostGithubEvent "/hook/github" "pull_request" payload
        (view Wreq.responseStatus resp7) `shouldBe` ok200

    it "requires an X-Hub-Signature header to be present for webhook calls" $
      withServer $ \ _ghQueue -> do
        let headers = [ (hContentType, "application/json")
                      , (hGithubEvent, "pull_request") ]
        response <- httpPost "/hook/github" headers ("{}" :: StrictByteString)
        let status = view Wreq.responseStatus response
            msg    = view Wreq.responseBody response
        status `shouldBe` badRequest400
        msg    `shouldBe` "missing or malformed X-Hub-Signature header"

    it "requires an X-Hub-Signature header to be valid for webhook calls" $
      withServer $ \ _ghQueue -> do
        let headers = [ (hContentType, "application/json")
                      , (hGithubEvent, "pull_request")
                      -- Provivide the header, but put bogus in it.
                      , (hGithubSignature, "sha1=not even hexadecimal") ]
        response <- httpPost "/hook/github" headers ("{}" :: StrictByteString)
        let status = view Wreq.responseStatus response
            msg    = view Wreq.responseBody response
        status `shouldBe` badRequest400
        msg    `shouldBe` "signature does not match, is the secret set up properly?"

        -- Now try again, but with the "sha1=" prefix in the signature.
        let headers' = (hGithubSignature, "badc0ffee") : (take 2 headers)
        response' <- httpPost "/hook/github" headers' ("{}" :: StrictByteString)
        let status' = view Wreq.responseStatus response'
            msg'    = view Wreq.responseBody response'
        status' `shouldBe` badRequest400
        msg'    `shouldBe` "missing or malformed X-Hub-Signature header"


    it "continues serving after receiving an invalid webhook" $
      withServer $ \ ghQueue -> do
        -- First send an invalid webhook with a bad payload.
        let badPayload = "this is definitely not valid json"
        badResponse <- httpPostGithubEvent "/hook/github" "status" badPayload
        (view Wreq.responseStatus badResponse) `shouldBe` internalServerError500
        -- Now sends a valid one, and verify that an event arrives.
        goodPayload  <- ByteString.Strict.readFile "tests/data/status-payload.json"
        goodResponse <- httpPostGithubEvent "/hook/github" "status" goodPayload
        event        <- popQueue ghQueue
        (view Wreq.responseStatus goodResponse) `shouldBe` ok200
        event `shouldSatisfy` isCommitStatusEvent


    it "serves 501 not implemented for unknown webhooks" $
      withServer $ \ _ghQueue -> do
        payload  <- ByteString.Strict.readFile "tests/data/pull-request-payload.json"
        -- Send a webhook event with correct signature, but bogus event name.
        response <- httpPostGithubEvent "/hook/github" "launch_missiles" payload
        let status = view Wreq.responseStatus response
            msg    = view Wreq.responseBody response
        status `shouldBe` notImplemented501
        msg    `shouldBe` "hook ignored, the event type is not supported"
