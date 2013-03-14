module Main where

import System.FilePath

import Rosetta.Silent

import Topo
import Score.Sparta
import Util.Timing

exDir = "examples/sparta"

spartaOutputFilename    = exDir </> "sparta.out"
spartaOutputFilename2   = exDir </> "sparta_asyn.out"
silentInputFilename     = exDir </> "S_0005_3426.out"
structureOutputFilename = exDir </> "S_0005_3426.pdb" -- used by SPARTA+ after writing
chemShiftInputFilename  = exDir </> "asyn_gs_long.tab"

main = do parseResult  <- parseSpartaFile   spartaOutputFilename
          print parseResult
          parseResult2 <- parseSpartaFile   spartaOutputFilename2
          print parseResult2
          [mdl]  <- processSilentFile silentInputFilename
          let cartopo = computePositions $ silentModel2TorsionTopo mdl
          spartaResult <- runSparta chemShiftInputFilename structureOutputFilename cartopo
          print spartaResult

