module Util.Assert(assertM) where

import Control.Exception(assert)

assertM condition = assert condition $ return ()

