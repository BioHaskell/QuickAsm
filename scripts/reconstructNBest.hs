{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import System.IO
import System.Environment
import System.Exit(exitFailure)
import System.FilePath((</>), (<.>))
import System.Directory(createDirectoryIfMissing)
import qualified Data.ByteString.Char8 as BS
import Data.List(minimumBy)
import Control.Monad(forM, when)
import Control.DeepSeq

import Rosetta.Silent
import Rosetta.PyMol
import Topo

-- | Converts a SilentModel structure to a Cartesian model.
model2topo :: SilentModel -> CartesianTopo
model2topo = computePositions . silentModel2TorsionTopo

printUsage = do hPutStrLn stderr "Usage: reconstructNBest <count> <input.out> <output directory>"
                exitFailure

debug = hPutStrLn stderr

readNBest n silentInputFilename = do
          debug "Reading n best"
          mdls <- processSilentFile silentInputFilename
          debug "Read silent"
          return $! rnf mdls
          debug "rnf silent"
          let bestMdls = take n $ sortModelsByScore mdls
          debug "Sorted best"
          return $! rnf bestMdls
          debug "rnf best"
          return $! bestMdls

--   TODO: optional trailing arguments - extract only given decoys
main = do lenArgs <- length `fmap` getArgs 
          when (lenArgs /= 3) printUsage
          [number, silentInputFilename, pdbOutputDir] <- getArgs
          createDirectoryIfMissing True pdbOutputDir
          let ((n :: Int, []):_) = reads number
          bestMdls <- readNBest n silentInputFilename
          forM bestMdls $ \bestMdl ->
            do let outFname = pdbOutputDir </> BS.unpack (name bestMdl)
               putStrLn $ (outFname <.> "pdb") ++ ": " ++ show (modelScoreIfAvailable bestMdl)
               writeFile (outFname <.> "pdb") $ showCartesianTopo $ model2topo bestMdl
               pymolScriptFile (outFname <.> "pml") bestMdl


