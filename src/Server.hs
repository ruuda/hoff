-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module Server (buildServer) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (isFullTBQueue, writeTBQueue)
import Control.Concurrent.STM.TMVar (newEmptyTMVar, putTMVar, takeTMVar)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Digest.Pure.SHA (hmacSha256)
import Data.SecureMem (SecureMem, secureMemFromByteString)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Types (status400, status404, status503)
import Web.Scotty (ActionM, ScottyM, body, get, header, jsonData, notFound, post, scottyApp, status, text)

import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Network.Wai.Handler.Warp as Warp

import qualified Github

-- Router for the web server.
router :: Github.EventQueue -> ScottyM ()
router ghQueue = do
  post "/hook/github" $ withSignatureCheck "secret" $ serveGithubWebhook ghQueue
  get  "/hook/github" $ serveWebhookDocs
  get  "/"            $ serveWebInterface
  notFound            $ serveNotFound

makeSecureMem :: ByteString -> SecureMem
makeSecureMem = secureMemFromByteString . toStrict

-- Checks the signature (encoded as hexadecimal characters in 'hexDigest') of
-- the message, given the secret, and the actual message bytes.
isSignatureValid :: Text -> Text -> ByteString -> Bool
isSignatureValid secret hexDigest message =
  let expected = ByteString.pack $ show $ hmacSha256 (encodeUtf8 secret) message
      actual   = encodeUtf8 hexDigest
  -- Convert the bytestrings to SecureMem before comparison, because SecureMem
  -- implements a constant-time comparison. (Note that we convert from Text to
  -- lazy byte string to strict byte string anyway, but the running time of
  -- these operations does not depend on secret data.)
  in (makeSecureMem expected) == (makeSecureMem actual)

withSignatureCheck :: Text -> ActionM () -> ActionM ()
withSignatureCheck secret bodyAction = do
  maybeHexDigest <- header "X-Hub-Signature"
  -- Compute the HMAC as from the body, encode as hexadecimal characters in a
  -- bytestring. Scotty reads headers as Text, so convert the header to a byte
  -- string as well.
  case maybeHexDigest of
    Nothing -> do
      status status400
      text "missing X-Hub-Signature header"
    Just hexDigest -> do
      bodyBytes <- body
      if isSignatureValid secret hexDigest bodyBytes
        then bodyAction
        else do
          status status400
          text "signature does not match, is the secret set up properly?"

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
    Just "ping" ->
      serveEnqueueEvent ghQueue $ Github.Ping
    _ ->
      text "hook ignored, X-GitHub-Event does not match expected value"

-- Handles replying to the client when a GitHub webhook is received.
serveEnqueueEvent :: Github.EventQueue -> Github.WebhookEvent -> ActionM ()
serveEnqueueEvent ghQueue event = do
  -- Enqueue the event if the queue is not full. Normally writeTBQueue would
  -- block if the queue is full, but instead we don't want to enqueue the event
  -- and tell the client to retry in a while.
  enqueued <- liftIO $ atomically $ do
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
serveWebInterface = text "not yet implemented"

serveNotFound :: ActionM ()
serveNotFound = do
  status status404
  text "not found"

warpSettings :: Int -> IO () -> Warp.Settings
warpSettings port beforeMainLoop
  = Warp.setPort port
  $ Warp.setBeforeMainLoop beforeMainLoop
  $ Warp.defaultSettings

-- Runs a webserver at the specified port. When GitHub webhooks are received,
-- an event will be added to the event queue. Returns a pair of two IO
-- operations: (runServer, blockUntilReady). The first should be used to run
-- the server, the second may be used to wait until the server is ready to
-- serve requests.
buildServer :: Int -> Github.EventQueue -> IO (IO (), IO ())
buildServer port ghQueue = do
  -- Create a variable that will be signalled when the server is ready.
  readyVar <- atomically newEmptyTMVar
  let signalReady     = atomically $ putTMVar readyVar ()
      blockUntilReady = atomically $ takeTMVar readyVar

  -- Make Warp signal the variable when it is ready to serve requests.
  let settings = warpSettings port signalReady

  -- Build the Scotty app, but do not start serving yet, as that would never
  -- return, so we wouldn't have the opportunity to return the 'blockUntilReady'
  -- function to the caller.
  app <- scottyApp $ router ghQueue
  let runServer = Warp.runSettings settings app

  -- Return two IO actions: one that will run the server (and never return),
  -- and one that blocks until 'readyVar' is signalled from the server.
  return (runServer, blockUntilReady)
