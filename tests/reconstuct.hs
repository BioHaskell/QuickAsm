{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO
import System.Environment
import qualified Data.ByteString.Char8 as BS

import Rosetta.Silent
import Topo

model2topo :: SilentModel -> CartesianTopo
model2topo = computePositions . silentModel2TorsionTopo

-- TODO: optional trailing arguments - extract only given decoys
main = do [silentInputFilename, pdbOutputFilename] <- getArgs
          [mdl] <- processSilent $ BS.pack silentInputFilename
          writeFile pdbOutputFilename $ showCartesianTopo $ model2topo mdl

