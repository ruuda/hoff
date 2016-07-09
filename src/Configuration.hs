-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Configuration where

import Data.Text (Text)

data Configuration = Configuration
  {
    owner      :: Text, -- The GitHub user or organization who owns the repo.
    repository :: Text, -- The name of the repository.
    branch     :: Text, -- The branch to guard and integrate commits into.
    port       :: Int   -- The port to listen on for webhooks.
  }
