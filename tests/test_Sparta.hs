module Main where

import System.IO(hPutStrLn, stderr)
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
          let torsionTopo = silentModel2TorsionTopo mdl
          let cartopo     = computePositions torsionTopo
          timePure "Computing topology" $ cartopo `deepseq` cartopo
          spartaResult <- sysTime "Running SPARTA+" $ runSparta chemShiftInputFilename cartopo
          case spartaResult of
            Left errMsg  -> hPutStrLn stderr errMsg
            Right result -> do putStr "Expected score"
                               print $ weightChemShifts result
          spartaScore <- sysTime "Scoring with SPARTA+" $ scoreSparta chemShiftInputFilename (torsionTopo, cartopo)
          putStr "Actual score:"
          print spartaScore

