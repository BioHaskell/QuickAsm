{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment
import qualified Data.ByteString.Char8 as BS

import Rosetta.Silent
import Topo
import Fasta(fastacode2resname)
import Data.Tree(flatten)

silentModel2TorsionTopo = constructBackbone . prepare
  where
    prepare mdl = zipWith extractAngles
                    (BS.unpack $ fastaSeq mdl)
                    (residues mdl)
    extractAngles code silentRec = ( fastacode2resname code
                                   , phi   silentRec
                                   , psi   silentRec
                                   , omega silentRec        )


model2topo :: SilentModel -> CartesianTopo
model2topo = computePositions . silentModel2TorsionTopo

-- TODO: optional trailing arguments - extract only given decoys
main = do [silentInputFilename, pdbOutputFilename] <- getArgs
          [mdl] <- processSilent $ BS.pack silentInputFilename
          putStrLn $ showCartesianTopo $ model2topo mdl

