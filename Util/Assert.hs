-- | Utility functions for testing.
{-# OPTIONS_GHC -O0 #-}
module Util.Assert(assertM, errorWithStack) where

import Control.Exception(assert)
import GHC.Stack(currentCallStack, renderStack)
import System.IO.Unsafe

-- | Assertion as a monadic action.
assertM :: Monad m => Bool -> m ()
assertM condition = assert condition $ return ()

{-# NOINLINE errorWithStack #-}
-- | Utility rendering current stack when reporting fatal error.
errorWithStack msg = error $ msg ++ ":\n" ++ unsafePerformIO (renderStack `fmap` currentCallStack)
