module Main where

import qualified Data.ByteString.Char8 as BS
import System.Environment
import System.IO(hPutStrLn, stderr)
import System.Exit(exitFailure)
import Control.Exception (assert)
import Control.Monad     (forM_, when)
import Data.Tree(flatten   )
import Numeric  (showFFloat)
import Data.Either(partitionEithers)

import Rosetta.Silent     as RS
import Rosetta.Restraints as RR
import Topo
import Score.Steric
import Score.DistanceRestraints
import Data.Octree(dist)

visualizeClash (a, b) = show a ++ "X:X" ++ show b ++ " = " ++ showFFloat (Just 3) (cPos a `dist` cPos b) ""

splitRestraints :: [RR.Restraint] -> ([RR.Restraint]
                                     ,[RR.Restraint])
splitRestraints = Data.Either.partitionEithers . map classifyRestraint
  where
    -- | Checks if the distance within restraint function corresponds to immediate beta-sheet neighbourhood.
    neighbourFunc :: RR.RestraintFunction -> Bool
    neighbourFunc (RR.RGaussian avg _ _) = avg <= 4.9
    -- | Sequential distance between two residues (according to numbering.)
    seqDist :: RR.AtomId -> RR.AtomId -> Int
    at1 `seqDist` at2 = abs $ RR.resId at2 - RR.resId at1
    classifyRestraint :: RR.Restraint -> Either RR.Restraint RR.Restraint
    classifyRestraint r@(DistR at1 at2 rfunc) | neighbourFunc rfunc && at1 `seqDist` at2 > 3 = Right r
    classifyRestraint other                                                                  = Left  other

main = do args <- getArgs
          when (length args /= 2) $ do hPutStrLn stderr "USAGE: checkRestraints <silent.out> <distances.cst>"
                                       exitFailure 
          let [ silentInputFilename, distanceRestraintsInput ] = args
          sMdls <- processSilentFile silentInputFilename
          let cMdls  = map (computePositions . silentModel2TorsionTopo) sMdls
          let names  = map name sMdls
          let fstMdl = head cMdls
          allRestraints <- RR.processRestraintsFile distanceRestraintsInput
          let (otherRestraints, skippableRestraints) = splitRestraints allRestraints
          --forM_ otherRestraints print -- DEBUG!!!
          forM_ skippableRestraints print -- DEBUG!!!
          putStrLn $ concat ["There are ", show $ length skippableRestraints, " skippable restraints."]
          putStrLn $ concat ["There are ", show $ length otherRestraints,     " other restraints."    ]
          return ()
          {-
          rset <- prepareRestraintsFile fstMdl distanceRestraintsInput
          forM_ (zip names cMdls) $ \(nam,cart) -> do let clashes = Prelude.map visualizeClash $ selfClashCheck $ flatten cart
                                                      mapM_ putStrLn clashes
                                                      putStrLn $ concat [show (length clashes)
                                                                        ," steric clashes detected in "
                                                                        ,BS.unpack nam]
                                                      let dists = checkDistanceRestraints rset cart
                                                      mapM_ print dists
          -}
          return ()
