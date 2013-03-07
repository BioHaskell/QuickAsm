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
import System.CPUTime
import Control.DeepSeq(NFData(..), deepseq)

import Util.Timing

evaluateScores rset cart = distScore `seq` clashCount `seq` (distScore, clashCount)
  where
    distScore  = scoreDistanceRestraints rset cart
    clashCount = fromIntegral $ length $ selfClashCheck $ flatten cart

main = do args <- getArgs
          when (length args /= 2) $ do hPutStrLn stderr "USAGE: score <silent.out> <distances.cst>"
                                       exitFailure 
          let [ silentInputFilename, distanceRestraintsInput ] = args
          sMdls <- time' "Parsing silent input" $ processSilentFile $ BS.pack $ silentInputFilename
          let cMdls  = map (computePositions . silentModel2TorsionTopo) sMdls
          let names  = map name sMdls
          let fstMdl = head cMdls
          rset <- time "Preparing restraint set" $ prepareRestraintsFile fstMdl distanceRestraintsInput
          forM_ (zip names cMdls) $ \(nam, cart) -> do clashCount <- timePure ("Steric    scoring " ++ BS.unpack nam) $
                                                                                  ((fromIntegral $ length $ selfClashCheck $ flatten cart) :: Double)
                                                       distScore  <- timePure ("Restraint scoring " ++ BS.unpack nam) $
                                                                                  scoreDistanceRestraints rset cart
                                                       putStrLn $ intercalate ", " [ BS.unpack nam
                                                                                   , showFFloat (Just 3) clashCount ""
                                                                                   , showFFloat (Just 3) distScore  "" ]

