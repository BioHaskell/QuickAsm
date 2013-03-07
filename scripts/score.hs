module Main where

import qualified Data.ByteString.Char8 as BS
import System.Environment
import System.IO  (hPutStrLn, stderr)
import System.Exit(exitFailure      )
import Control.Exception (assert)
import Control.Monad     (forM, when, forM_)
import Data.Tree(flatten   )
import Data.List(intercalate)
import Numeric  (showFFloat)

import Rosetta.Silent
import Rosetta.Restraints
import Topo
import Score.Steric
import Score.DistanceRestraints
import Data.Octree(dist)
import System.CPUTime
import Control.DeepSeq(deepseq)

time name computation = do start  <- getCPUTime
                           result <- deepseq computation computation
                           end    <- getCPUTime
                           let diff = fromIntegral (end - start) / 10^12
                           putStrLn $ "Computing " ++ name ++ " took " ++ showFFloat (Just 3) diff " seconds."
                           return result

timePure name f = time name (return f)

evaluateScores rset cart = distScore `seq` clashCount `seq` (distScore, clashCount)
  where
    distScore  = scoreDistanceRestraints rset cart
    clashCount = fromIntegral $ length $ selfClashCheck $ flatten cart

main = do args <- getArgs
          when (length args /= 2) $ do hPutStrLn stderr "USAGE: checkRestraints <silent.out> <distances.cst>"
                                       exitFailure 
          let [ silentInputFilename, distanceRestraintsInput ] = args
          sMdls <- processSilentFile $ BS.pack $ silentInputFilename
          let cMdls = map (computePositions . silentModel2TorsionTopo) sMdls
          let names = map name sMdls
          let fstMdl = head cMdls
          rset <- time "restraint set" $ prepareRestraintsFile fstMdl distanceRestraintsInput
          forM_ (zip names cMdls) $ \(nam, cart) -> do clashCount <- timePure ("steric    scoring " ++ BS.unpack nam) $
                                                                                  fromIntegral $ length $ selfClashCheck $ flatten cart
                                                       distScore  <- timePure ("restraint scoring " ++ BS.unpack nam) $
                                                                                  scoreDistanceRestraints rset cart
                                                       putStrLn $ intercalate ", " [BS.unpack nam, show clashCount, show distScore]

