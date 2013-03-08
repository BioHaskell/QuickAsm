module Main where

import qualified Data.ByteString.Char8 as BS
import System.Environment
import System.IO(hPutStrLn, stderr)
import System.Exit(exitFailure)
import Control.Exception (assert)
import Control.Monad     (forM, when)
import Data.Tree(flatten   )
import Numeric  (showFFloat)

import Rosetta.Silent
import Rosetta.Restraints
import Topo
import Score.Steric
import Score.DistanceRestraints
import Data.Octree(dist)

visualizeClash (a, b) = show a ++ "X:X" ++ show b ++ " = " ++ showFFloat (Just 3) (cPos a `dist` cPos b) ""

main = do args <- getArgs
          when (length args /= 2) $ do hPutStrLn stderr "USAGE: checkRestraints <silent.out> <distances.cst>"
                                       exitFailure 
          let [ silentInputFilename, distanceRestraintsInput ] = args
          sMdls <- processSilentFile silentInputFilename
          let cMdls = map (computePositions . silentModel2TorsionTopo) sMdls
          let names = map name sMdls
          let fstMdl = head cMdls
          rset <- prepareRestraintsFile fstMdl distanceRestraintsInput
          forM (zip names cMdls) $ \(nam,cart) -> do let clashes = Prelude.map visualizeClash $ selfClashCheck $ flatten $ cart
                                                     mapM putStrLn clashes
                                                     putStrLn $ show (length clashes) ++ " steric clashes detected in " ++ BS.unpack nam
                                                     let dists = checkDistanceRestraints rset cart
                                                     mapM print dists
          return ()
          
