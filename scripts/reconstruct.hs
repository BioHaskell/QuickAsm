{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import System.IO
import System.Environment
import qualified Data.ByteString.Char8 as BS

import Rosetta.Silent
import Topo

-- | Converts a SilentModel structure to a Cartesian model.
model2topo :: SilentModel -> CartesianTopo
model2topo = computePositions . silentModel2TorsionTopo

--   TODO: optional trailing arguments - extract only given decoys
main = do [silentInputFilename, pdbOutputFilename] <- getArgs
          [mdl] <- processSilentFile $ BS.pack silentInputFilename
          writeFile pdbOutputFilename $ showCartesianTopo $ model2topo mdl



