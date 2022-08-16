-- Hoff -- A gatekeeper for your commits
-- Copyright 2022 Channable
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.
module Version (version) where

-- | Hoff's version number encoded in a String.
--
-- To be displayed on the web interface or by running @ hoff --version @.
version :: String
version = "0.26.2" -- please also update hoff.cabal and hoff.nix
