module Main where

import System.FilePath
import Control.DeepSeq
import qualified Data.ByteString.Char8 as BS

import Rosetta.Silent

import Topo
import Score.Sparta
import Util.Timing

exDir = "examples/sparta"

instance NFData BS.ByteString where

spartaOutputFilename    = exDir </> "sparta.out"
spartaOutputFilename2   = exDir </> "sparta_asyn.out"
silentInputFilename     = exDir </> "S_0005_3426.out"
structureOutputFilename = exDir </> "S_0005_3426.pdb" -- used by SPARTA+ after writing
chemShiftInputFilename  = exDir </> "asyn_gs_long.tab"

main = do parseResult  <- parseSpartaFile   spartaOutputFilename
          print parseResult
          parseResult2 <- parseSpartaFile   spartaOutputFilename2
          print parseResult2
          [mdl]  <- time "Reading silent decoy" $ processSilentFile silentInputFilename
          let cartopo = computePositions $ silentModel2TorsionTopo mdl
          timePure "Computing topology" $ cartopo `deepseq` cartopo
          spartaResult <- sysTime "Running SPARTA+" $ runSparta chemShiftInputFilename structureOutputFilename cartopo
          print spartaResult

