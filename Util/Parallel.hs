{-# LANGUAGE CPP #-}
-- | Wrapper for parallel evaluation of a list of IO actions.
-- Encapsulates whole interface with parallel/withParallel pair.
module Util.Parallel(parallel, withParallel) where

import qualified Control.Concurrent.ParallelIO
import qualified GHC.Conc
import qualified Data.List as L(isPrefixOf)
import GHC.Environment(getFullArgs) -- to check RTS params
import Control.Monad(when)

-- | Perform a set of operations in parallel.
parallel ::  [IO a] -> IO [a]
parallel = Control.Concurrent.ParallelIO.parallel

-- * Parallelization with parallel-io
--   Here is code for parallellism
-- | Sets up as many capabilities as we have processors.
-- #ifdef __GLASGOW_HASKELL__
setupParallel = do rtsConcArgs <- filter (L.isPrefixOf "-N") `fmap` getFullArgs
                   when (rtsConcArgs /= []) $ do cap <- GHC.Conc.getNumCapabilities
                                                 putStrLn $ concat ["Using ", show cap, " parallel threads."]
                   when (rtsConcArgs == []) $ do
                     nProc <- GHC.Conc.getNumProcessors
                     let nCap = min 12 nProc
                     putStrLn $ concat ["Found ", show nProc, " processors ",
                                        " and no -N argument - initializing ",
                                        show nCap, " capabilities."]
                     GHC.Conc.setNumCapabilities nCap
-- #else     
-- setupParallel = putStrLn "No parallelization!" 
-- #endif
    
-- | Finalization of parallel pool.
stopParallel = Control.Concurrent.ParallelIO.stopGlobalPool

-- | Wraps parallel-io computation with setupParallel and stopParallel.
--   NOTE: Not yet exception-proof.
withParallel act = do setupParallel
                      r <- act
                      stopParallel
                      return r
