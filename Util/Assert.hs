-- | Utility functions for testing.
module Util.Assert(assertM) where

import Control.Exception(assert)

-- | Assertion as a monadic action.
assertM :: Monad m => Bool -> m ()
assertM condition = assert condition $ return ()

