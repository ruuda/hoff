-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

import Test.Hspec (hspec)
import EventLoopSpec (eventLoopSpec)

-- This test suite tests interaction of the system with the outside world, as
-- opposed to its internals (there are unit tests for that). It is not a full
-- end-to-end test, in the sense that the final executable is tested. Instead,
-- the program is tested in two stages: one tests the Git interaction and the
-- event loop, but incoming messages are faked. The other tests the web server,
-- ignoring the messages it receives.

main :: IO ()
main = hspec eventLoopSpec
