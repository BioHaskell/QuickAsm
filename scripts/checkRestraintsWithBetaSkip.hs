{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Char8 as BS
import System.Environment
import System.IO(hPutStrLn, putStr, hPrint, stderr)
import System.Exit(exitFailure)
import Control.Exception (assert)
import Control.Monad     (forM_, forM, when)
import Numeric  (showFFloat)
import Data.Tree    (flatten)
import Data.Either  (partitionEithers)
import Data.List    (groupBy, sortBy, partition, intercalate)
import Data.Function(on)
import Data.Maybe   (fromJust)

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

-- | Extracts atom names from a ROSETTA restraint.
rAtNames ::  RR.Restraint -> (BS.ByteString, BS.ByteString)
rAtNames (DistR at1 at2 _) = (RR.atName at1, RR.atName at2)

-- | Extracts residue numbers from a ROSETTA restraint.
rResNums ::  RR.Restraint -> (Int, Int)
rResNums (DistR at1 at2 _) = (RR.resId  at1, RR.resId  at2)

-- | Groups restraints by atom names, and sorts each group by residue ids.
groupByAtNames ::  [RR.Restraint] -> [[RR.Restraint]]
groupByAtNames = map (sortBy  (compare `on` rResNums)) .
                      groupBy ((==)    `on` rAtNames)  .
                      sortBy  (compare `on` rAtNames)

precomputeOrderAndPrintErrors :: CartesianTopo -> [RR.Restraint] -> IO RestraintSet
precomputeOrderAndPrintErrors mdl restraints = 
  do let (rset, errs) = precomputeOrder restraints mdl
     forM errs $ hPrint stderr
     return rset

filterRSets rsets = do when (not $ null bad) $ hPrint stderr $ shows (length bad) "null restraint sets."
                       return good
  where
    (bad, good) = partition (null . byNum) rsets

showRestraintSetAtoms :: RestraintSet -> String
showRestraintSetAtoms  = showRestraintListAtoms . byNum

showRestraintListAtoms :: [Score.DistanceRestraints.Restraint] -> String
showRestraintListAtoms = show . fromJust . atNames . source . head

atNames :: RR.Restraint -> Maybe (BS.ByteString, BS.ByteString)
atNames (RR.DistR at1 at2 _func) = Just (atName at1, atName at2) -- we expect distance restraints only!!!
atNames unexpectedRestraint      = error $ "Unexpected restraint " ++ show unexpectedRestraint

--markMinimalScores ::  Ord t1 => [(t, t1)] -> [(Bool, t, t1)]
markMinimalScores rs = zipWith isMinimal rs (head rs:rs)
  where
    isMinimal (r, cur) (rp, prev) = (min prev cur == cur, r, cur)

showMinimalScores = ("\n" `intercalate`) . map shower . markMinimalScores
  where
    shower (cond, restr, val) = sign:(shows (source restr) $ ' ':showFFloat (Just 3) val "")
      where
        sign = if cond then '+' else '-'

minimalScoreValues rs = zipWith min vals $ head vals:vals
  where
    vals = map snd rs

avg l = sum l/fromIntegral (length l)

--takeTotal = sqrt . avg
takeTotal = sum

computeSkippableScores ::  [RestraintSet] -> CartesianTopo -> IO Double
computeSkippableScores skippableRSets cart = do
    let sScores :: [[(Score.DistanceRestraints.Restraint, Double)]] = map (flip checkDistanceRestraints cart) skippableRSets
    sums <- forM (zip skippableRSets sScores) printSScore
    -- TODO: implement!!!
    return $ takeTotal $ concat sums
  where
    printSScore :: (RestraintSet, [(Score.DistanceRestraints.Restraint, Double)]) -> IO [Double]
    printSScore (sSet, sScore) = do
      print "printSScore" 
      case sScore of 
        [] -> do print $ "Empty sSet " ++ showRestraintSetAtoms sSet
                 return []
        _  -> do print $ showRestraintSetAtoms sSet
                 putStr "sSet: "
                 putStrLn $ showMinimalScores sScore
                 let s = minimalScoreValues sScore
                 putStr "Total score RMSd: "
                 putStrLn $ showFFloat (Just 3) (takeTotal s) ""
                 return s

main = do args <- getArgs
          when (length args /= 2) $ do hPutStrLn stderr "USAGE: checkRestraints <silent.out> <distances.cst>"
                                       exitFailure
          let [ silentInputFilename, distanceRestraintsInput ] = args
          sMdls <- processSilentFile silentInputFilename
          let cMdls  = map (computePositions . silentModel2TorsionTopo) sMdls
          let names  = map name sMdls
          let fstMdl = head cMdls
          -- Read restraint sets
          allRestraints <- RR.processRestraintsFile distanceRestraintsInput
          -- Prepare splitted restraint sets
          let (otherRestraints, skippableRestraints) = splitRestraints allRestraints
          --forM_ otherRestraints     print -- DEBUG!!!
          --forM_ skippableRestraints print -- DEBUG!!!
          putStrLn $ concat ["There are ", show $ length skippableRestraints, " skippable restraints."]
          putStrLn $ concat ["There are ", show $ length otherRestraints,     " other restraints."    ]
          let restrGroups   = groupByAtNames skippableRestraints
          otherRSet       <- precomputeOrderAndPrintErrors fstMdl otherRestraints
          -- Precompute order on a given model...
          skippableRSets' <- forM restrGroups $ precomputeOrderAndPrintErrors fstMdl
          skippableRSets  <- filterRSets skippableRSets' -- TODO: integrate with precomputing order...
          -- Evaluate...
          forM_ (zip names cMdls) $ \(nam,cart) -> do let otherScore = scoreDistanceRestraints otherRSet cart
                                                      --skippedChecks <- forM skippableRSets $ flip checkDistanceRestraints cart
                                                      skippableScore <- computeSkippableScores skippableRSets cart -- *** TODO: implement!!! ***
                                                      putStrLn $ concat [showFFloat (Just 3) otherScore ""
                                                                        ," standard restraints score "
                                                                        ," and "
                                                                        ,showFFloat (Just 3) skippableScore ""
                                                                        ," skippable restraints score of "
                                                                        ,BS.unpack nam]
                                                      --let sScores = map (flip checkDistanceRestraints cart) skippableRSets
                                                      --let dists = checkDistanceRestraints otherRSet cart
                                                      --mapM_ print dists
                                                      return ()
          return ()
