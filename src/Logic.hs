-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Logic (Action (..), Event (..), handleEvent) where

import Data.Text (Text)
import Project (BuildStatus, ProjectState, PullRequestId, Sha)

data Event
  -- GitHub events
  = PullRequestOpened PullRequestId Sha Text   -- PR, sha, author.
  | PullRequestCommitChanged PullRequestId Sha -- PR, new sha.
  | PullRequestClosed PullRequestId            -- PR.
  | CommentAdded PullRequestId Text Text       -- PR, author and body.
  -- CI events
  | BuildStatusChanged Sha BuildStatus

data Action = Nop

handleEvent :: Event -> ProjectState -> (ProjectState, Action)
handleEvent event state = case event of
  PullRequestOpened pr sha author  -> (state, Nop)
  PullRequestCommitChanged pr sha  -> (state, Nop)
  PullRequestClosed pr             -> (state, Nop)
  CommentAdded pr author body      -> (state, Nop)
  BuildStatusChanged sha newstatus -> (state, Nop)
