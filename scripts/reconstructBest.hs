{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import System.IO
import System.Environment
import qualified Data.ByteString.Char8 as BS
import Data.List(minimumBy)
import Control.Monad(when)
import System.Exit(exitFailure)

import Rosetta.Silent
import Topo

-- | Converts a SilentModel structure to a Cartesian model.
model2topo :: SilentModel -> CartesianTopo
model2topo = computePositions . silentModel2TorsionTopo

printUsage = do hPutStrLn stderr "Usage: reconstructBest <input.out> <output.pdb>"
                exitFailure

--   TODO: optional trailing arguments - extract only given decoys
main = do lenArgs <- length `fmap` getArgs 
          when (lenArgs /=2) printUsage
          [silentInputFilename, pdbOutputFilename] <- getArgs
          mdls <- processSilentFile silentInputFilename
          let bestMdl = bestSilentModel mdls
          putStrLn $ BS.unpack (name bestMdl) ++ ": " ++ show (modelScoreIfAvailable bestMdl)
          writeFile pdbOutputFilename $ showCartesianTopo $ model2topo bestMdl



