-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WebInterface (renderPage, viewIndex, viewProject, viewOwner, stylesheet, stylesheetUrl) where

import Control.Monad (forM_, unless, void)
import Crypto.Hash (Digest, SHA256, hash)
import Data.List (sortOn)
import Data.Bifunctor (second)
import Data.ByteArray.Encoding (Base (Base64, Base64URLUnpadded), convertToBase)
import Data.FileEmbed (embedStringFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Prelude hiding (div, head, id, span)
import Text.Blaze (toValue, (!))
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 (Html, a, body, div, docTypeHtml, h1, h2, h3, head, link, meta, p, span,
                         title, toHtml)
import Text.Blaze.Html5.Attributes (charset, class_, content, href, id, name, rel)
import Text.Blaze.Internal (Attribute, AttributeValue, attribute)

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text

import Format (format)
import Project (Approval (..), Owner, ProjectInfo, ProjectState, PullRequest)
import Types (PullRequestId (..), Username (..))

import qualified Project

-- TODO: Minify this css at inclusion time.
stylesheet :: Text
stylesheet = $(embedStringFile "static/style.css")

stylesheetDigest :: Digest SHA256
stylesheetDigest = hash $ encodeUtf8 stylesheet

-- Render a digest to a text in a given base.
showAs :: Base -> Digest a -> Text
showAs base = decodeUtf8 . convertToBase base

stylesheetUrlDigest :: Text
stylesheetUrlDigest = showAs Base64URLUnpadded stylesheetDigest

stylesheetBase64Digest :: Text
stylesheetBase64Digest = showAs Base64 stylesheetDigest

-- URL to the Google Fonts stylesheet. The family parameter is a pipe-separated
-- list of font families. The display parameter corresponds to the CSS
-- font-display property.
googlefontsUrl :: Text
googlefontsUrl = "https://fonts.googleapis.com/css?family=Source+Sans+Pro&display=swap"

-- URL to host the stylesheet at. Including a digest in this URL means we
-- can set the @Cache-Control: immutable@ header to facilitate caching.
-- That's both less wasteful and easier to implement than 304 responses
-- and ETag headers.
stylesheetUrl :: Text
stylesheetUrl = "/style/" <> stylesheetUrlDigest <> ".css"

-- Wraps the given body html in html for an actual page, and encodes the
-- resulting page in utf-8.
renderPage :: Text -> Html -> LazyByteString.ByteString
renderPage pageTitle bodyHtml = renderHtml $ docTypeHtml $ do
  head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    meta ! name "robots" ! content "noindex, nofollow"
    title $ toHtml pageTitle
    link ! rel "stylesheet" ! href (toValue googlefontsUrl)
    link ! rel "stylesheet" ! href (toValue stylesheetUrl) ! integrity (toValue $ "sha256-" <> stylesheetBase64Digest)
  body $
    div ! id "content" $
      bodyHtml

-- Integrity attribute for subresource integrity. Blaze doesn't have
-- this yet, but this is what their implementation would look like.
integrity :: AttributeValue -> Attribute
integrity = attribute "integrity" " integrity=\""

-- Render an "owner/repo" link.
viewProjectInfo :: ProjectInfo -> Html
viewProjectInfo info =
  let
    owner = Project.owner info
    repo  = Project.repository info
    ownerUrl = format "/{}" [owner]
    repoUrl  = format "/{}/{}" [owner, repo]
  in
    p $ do
      a ! href (toValue ownerUrl) $ (toHtml owner)
      void "\x2009/\x2009" -- U+2009 is a thin space.
      a ! href (toValue repoUrl) $ (toHtml repo)

-- Renders the body html for the index page.
viewIndex :: [ProjectInfo] -> Html
viewIndex infos =
  let
  in do
    h1 "Hoff"
    h2 "About"
    p $ do
      void "Hoff is a gatekeeper for your commits. See "
      a ! href "https://github.com/ruuda/hoff" $ "github.com/ruuda/hoff"
      void " for more information."
    h2 "Tracked repositories"
    mapM_ viewProjectInfo infos

-- Renders the body html for the status page of a project.
viewProject :: ProjectInfo -> ProjectState -> Html
viewProject info state =
  let
    owner = Project.owner info
    repo  = Project.repository info
    ownerUrl = format "/{}" [owner]
    repoUrl  = format "https://github.com/{}/{}" (owner, repo)
  in do
    h1 $ do
      a ! class_ "back" ! href "/" $ void "«"
      a ! href (toValue ownerUrl) $ toHtml owner
      void "\x2009/\x2009" -- U+2009 is a thin space.
      a ! href (toValue repoUrl) $ toHtml repo

    viewProjectQueues info state

viewOwner :: Owner -> [(ProjectInfo, ProjectState)] -> Html
viewOwner owner projects = do
  let
    ownerUrl = format "https://github.com/{}" [owner]
  h1 $ do
    a ! class_ "back" ! href "/" $ void "«"
    a ! href (toValue ownerUrl) $ toHtml owner
  viewGroupedProjectQueues projects

-- Render the html for the queues in a project, excluding the header and footer.
viewProjectQueues :: ProjectInfo -> ProjectState -> Html
viewProjectQueues info state = do
  let
    pullRequests :: [(PullRequestId, PullRequest, Project.PullRequestStatus)]
    pullRequests = Project.classifyPullRequests state
    filterPrs predicate = filter (\(_, _, status) -> predicate status) pullRequests

  let building = filterPrs (== Project.PrStatusBuildPending)
  h2 "Building"
  if null building
    then p "There are no builds in progress at the moment."
    else viewList viewPullRequestWithApproval info building

  let approved = filterPrs (== Project.PrStatusApproved)
  unless (null approved) $ do
    h2 "Approved"
    let approvedSorted = sortOn (\(_, pr, _) -> approvalOrder <$> Project.approval pr) approved
    viewList viewPullRequestWithApproval info approvedSorted

  let awaitingApproval = reverse $ filterPrs (== Project.PrStatusAwaitingApproval)
  unless (null awaitingApproval) $ do
    h2 "Awaiting approval"
    viewList viewPullRequest info awaitingApproval

  let failed = filterPrs prFailed

  unless (null failed) $ do
    h2 "Failed"
    -- TODO: Also render failure reason: conflicted or build failed.
    viewList viewPullRequestWithApproval info failed

  -- TODO: Keep a list of the last n integrated pull requests, so they stay
  -- around for a bit after they have been closed.
  let integrated = filterPrs (== Project.PrStatusIntegrated)
  unless (null integrated) $ do
    h2 "Recently integrated"
    viewList viewPullRequestWithApproval info integrated

-- Render the html for the queues in a project, excluding the header and footer.
viewGroupedProjectQueues :: [(ProjectInfo, ProjectState)] -> Html
viewGroupedProjectQueues projects = do
  let
    pullRequests :: [(ProjectInfo, [(PullRequestId, PullRequest, Project.PullRequestStatus)])]
    pullRequests = map (second Project.classifyPullRequests) projects
    filterPrs predicate = let
      predicateTriple (_, _, status) = predicate status
      in  filter (not . null . snd) $ map (second (filter predicateTriple)) pullRequests
  let
    building = filterPrs (== Project.PrStatusBuildPending)
  h2 "Building"
  if null building
    then p "There are no builds in progress at the moment."
    else mapM_ (uncurry $ viewList' viewPullRequestWithApproval) building
  let approved = filterPrs (== Project.PrStatusApproved)
  unless (null approved) $ do
    h2 "Approved"
    mapM_ (uncurry $ viewList' viewPullRequestWithApproval) approved

  let awaitingApproval = reverse $ filterPrs (== Project.PrStatusAwaitingApproval)
  unless (null awaitingApproval) $ do
    h2 "Awaiting approval"
    mapM_ (uncurry $ viewList' viewPullRequest) awaitingApproval

  let failed = filterPrs prFailed

  unless (null failed) $ do
    h2 "Failed"
    -- TODO: Also render failure reason: conflicted or build failed.
    mapM_ (uncurry $ viewList' viewPullRequestWithApproval) failed

  -- TODO: Keep a list of the last n integrated pull requests, so they stay
  -- around for a bit after they have been closed.
  let integrated = filterPrs (== Project.PrStatusIntegrated)
  unless (null integrated) $ do
    h2 "Recently integrated"
    mapM_ (uncurry $ viewList' viewPullRequestWithApproval) integrated

  where
    viewList'
          :: (ProjectInfo -> PullRequestId -> PullRequest -> Html)
          -> ProjectInfo
          -> [(PullRequestId, PullRequest, status)]
          -> Html
    viewList' view info prs = do
      h3 (toHtml $ Project.repository info)
      forM_ prs $ \(prId, pr, _) -> p $ view info prId pr

-- Renders the contents of a list item with a link for a pull request.
viewPullRequest :: ProjectInfo -> PullRequestId -> PullRequest -> Html
viewPullRequest info (PullRequestId n) pullRequest =
  let
    url = format "https://github.com/{}/{}/pull/{}"
      (Project.owner info, Project.repository info, n)
  in
    a ! href (toValue url) $ toHtml $ Project.title pullRequest

viewPullRequestWithApproval :: ProjectInfo -> PullRequestId -> PullRequest -> Html
viewPullRequestWithApproval info prId pullRequest = do
  viewPullRequest info prId pullRequest
  case Project.approval pullRequest of
    Just Approval { approver = Username username, approvedFor = approvalType } ->
      span ! class_ "review" $ do
        let approvedAction = Project.displayApproval approvalType
        toHtml $ format "Approved for {} by " [approvedAction]
        -- TODO: Link to approval comment, not just username.
        let url = Text.append "https://github.com/" username
        a ! href (toValue url) $ toHtml username
    Nothing ->
      error $
        "Tried to render approval link for pull request " ++ (show prId) ++
        " which was not approved. This is a programming error."

-- Render all pull requests in the list with the given view function.
viewList  :: (ProjectInfo -> PullRequestId -> PullRequest -> Html)
          -> ProjectInfo
          -> [(PullRequestId, PullRequest, status)]
          -> Html
viewList view info prs = forM_  prs $ \(prId, pr, _) -> p $ view info prId pr

prFailed :: Project.PullRequestStatus -> Bool
prFailed Project.PrStatusFailedConflict  = True
prFailed (Project.PrStatusFailedBuild _) = True
prFailed _                               = False
