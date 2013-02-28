module Main where

import System.Environment(getArgs)

import Rosetta.Silent
import Topo

initialize_topology = undefined
samplingStep        = undefined
writeSilentModel    = undefined
writeCartesianModel = undefined

steps=1000

-- TODO: scoring?

main = do [fastaInput, fragmentInput, silentOutput, pdbOutput] <- getArgs
          topo <- initialize_topology fastaInput
          result <- iterate samplingStep topo !! steps
          writeSilentModel silentOutput result
          writeCartesianModel pdbOutput . computePositions $ result
         
