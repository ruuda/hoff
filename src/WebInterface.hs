-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}

module WebInterface (renderPage, viewProject) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text.Format.Params (Params)
import Data.Text.Lazy (toStrict)
import Prelude hiding (id, div, head, span)
import Text.Blaze ((!), toValue)
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 (Html, a, body, div, docTypeHtml, h1, h2, head, meta, p, span, title, toHtml)
import Text.Blaze.Html5.Attributes (class_, charset, content, href, id, name)

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Format as Text

import Project (ProjectInfo, ProjectState, PullRequest, PullRequestId (..))

import qualified Project

-- Conversion function because of Haskell string type madness. This is just
-- Text.format, but returning a strict Text instead of a lazy one.
format :: Params ps => Text.Format -> ps -> Text
format formatString params = toStrict $ Text.format formatString params

-- Wraps the given body html in html for an actual page, and encodes the
-- resulting page in utf-8.
renderPage :: Text -> Html -> LazyByteString.ByteString
renderPage pageTitle bodyHtml = renderHtml $ docTypeHtml $ do
  head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    title $ toHtml pageTitle
  body $
    div ! id "content" $
      bodyHtml

-- Renders the body html for the status page of a project.
viewProject :: ProjectInfo -> ProjectState -> Html
viewProject info state =
  let
    owner = Project.owner info
    repo  = Project.repository info
    -- TODO: Actually point these links at internal pages?
    ownerUrl = format "https://github.com/{}" [owner]
    repoUrl  = format "https://github.com/{}/{}" (owner, repo)
  in do
    h1 $ do
      a ! href (toValue ownerUrl) $ toHtml owner
      _ <- "\x2009/\x2009" -- U+2009 is a thin space.
      a ! href (toValue repoUrl) $ toHtml repo

    viewProjectQueues info state

-- Render the html for the queues in a project, excluding the header and footer.
viewProjectQueues :: ProjectInfo -> ProjectState -> Html
viewProjectQueues info state = do
  h2 "Building"
  p "There are no builds in progress at the moment." -- TODO

  h2 "Approved"
  viewList viewPullRequestWithApproval info state $
    Project.approvedPullRequests state

  h2 "Awaiting approval"
  viewList viewPullRequest info state $ Project.approvedPullRequests state -- TODO: use proper list.

  h2 "Failed to integrate"
  viewList viewPullRequestWithApproval info state $
    Project.conflictedPullRequests state

  h2 "Failed to build"
  viewList viewPullRequestWithApproval info state $
    Project.failedPullRequests state

  h2 "Recently integrated"
  viewList viewPullRequestWithApproval info state $
    Project.conflictedPullRequests state -- TODO: Use proper list.

-- Renders the contents of a list item with a link for a pull request.
viewPullRequest :: ProjectInfo -> PullRequestId -> PullRequest -> Html
viewPullRequest info (PullRequestId n) _pullRequest =
  let
    url = format "https://github.com/{}/{}/pull/{}"
      (Project.owner info, Project.repository info, n)
  in do
    span ! class_ "prnum" $ toHtml n
    a ! href (toValue url) $ "TODO: Pull request titles"

viewPullRequestWithApproval :: ProjectInfo -> PullRequestId -> PullRequest -> Html
viewPullRequestWithApproval info prId pullRequest = do
  viewPullRequest info prId pullRequest
  case Project.approvedBy pullRequest of
    Just username ->
      span ! class_ "review" $ do
        _ <- "Approved by "
        -- TODO: Link to approval comment, not just username.
        let url = Text.append "https://github.com/" username
        a ! href (toValue url) $ toHtml username
    Nothing ->
      fail $
        "Tried to render approval link for pull request " ++ (show prId) ++
        " which was not approved. This is a programming error."

-- Render all pull requests in the list with the given view function.
-- TODO: Use a safer abstraction, than a list of IDs for which it is not clear
-- from the types that lookup will not fail.
viewList :: (ProjectInfo -> PullRequestId -> PullRequest -> Html)
         -> ProjectInfo
         -> ProjectState
         -> [PullRequestId]
         -> Html
viewList view info state prIds = forM_ prIds $ \ prId ->
  let
    Just pr = Project.lookupPullRequest prId state
  in
    p $ view info prId pr
