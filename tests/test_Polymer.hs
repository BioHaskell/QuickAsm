{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
-- | Tests polymer construction and instantiation.
module Main where

import           System.IO(hPutStrLn, stderr)
import           System.Exit(exitFailure)
import           System.Random(getStdRandom)
import           Control.Exception(assert)
import           Control.Monad(forM_)
import qualified Data.Vector as V
import           Data.List(intercalate, nub)
import           Control.Arrow((&&&))

import           Rosetta.Silent
import qualified Rosetta.Fragments as F

import           RepeatPolymer
import           Annealing
import           FragReplace
import           Topo
import           Score.ScoreSet
import           Score.DistanceRestraints
import           Score.Steric
import           Modelling
import           Util.Fasta
import           Util.Assert(assertM)
import           Util.Timing

inputSilent     = "examples/polymer/polymer.out"
inputFragSet    = "examples/assembly/aat000_09.200_R3"
restraintsInput = "examples/assembly/asyn_gs_long_bb.newcst"

monomerLength = 61
monomerCount  = 5
linkerLength  = 32

computePolymerLength monomerCount monomerLength linkerLength = monomerLength * monomerCount + linkerLength * (monomerCount - 1)

computePolymerSequence monomerLength linkerLength seq =
    assertions (monomerSeq, linkerSeq)
  where
    (monomerSeq :: String, rest)  = splitAt monomerLength seq
    linkerSeq   :: String         = take linkerLength     rest
    expectedLength                = computePolymerLength monomerCount monomerLength linkerLength
    reSeq       :: String         = concat $ concat $ replicate (monomerCount - 1) [monomerSeq, linkerSeq] ++ [[monomerSeq]]
    assertions                    = assert (expectedLength == length seq) .
                                    assert (reSeq          ==        seq) .
                                    assert (monomerCount   >= 2         )

showPolymer = showCartesianTopo . computePositions . instantiate

debugFragSet fragSet = do putStrLn $ "Fragment set length: "             ++ (show . V.length . F.unRFragSet) fragSet
                          putStrLn $ "Fragment set starting positions: " ++ showEach F.startPos fragSet
                          putStrLn $ "Fragment set ending   positions: " ++ showEach F.endPos   fragSet
  where
    showEach projection = intercalate " " . map show . V.toList . V.map (projection . V.head) . F.unRFragSet

-- | Checks that a topology has OXT at the end.
hasOXT :: TorsionTopo -> Bool
hasOXT = (\t -> tAtName t == "OXT") . last . backbone

tRes tors = tResName tors ++ show (tResId tors)

debugPolymer polymer monoSeq linkerSeq = do putStrLn $ "Monomer has OXT:" ++ (show . hasOXT . monomer) polymer
                                            putStrLn $ "Polymer has OXT:" ++ (show . hasOXT . linker ) polymer
                                            putStrLn $ "Monomer residues:" ++ (intercalate " " . nub . map tRes . backbone . monomer ) polymer
                                            putStrLn $ "Extracted monomer seq: "   ++ monoSeq2
                                            putStrLn $ "Extracted linker  seq: "   ++ linkerSeq2
                                            putStrLn $ "Recorded monomer length: " ++ (show . monomerLen)       polymer
                                            putStrLn $ "Actual   monomer length: " ++ (show . length . topo2sequence . monomer) polymer
                                            putStrLn $ "Recorded linker  length: " ++ (show . linkerLen)        polymer
                                            putStrLn $ "Actual   linker  length: " ++ (show . length . topo2sequence . linker ) polymer
                                            assertM  $ monoSeq   == monoSeq2
                                            assertM  $ linkerSeq == linkerSeq2
  where
    monoSeq2   = topo2sequence $ monomer polymer
    linkerSeq2 = topo2sequence $ linker  polymer

main = do topo <- time "Read input model" $ (head . map silentModel2TorsionTopo) `fmap` processSilentFile inputSilent
          preFrags <- time "Reading fragment set"  $ F.processFragmentsFile inputFragSet
          fragSet' <- time "Checking fragment set" $ checkFragments topo preFrags
          let seq = topo2sequence topo
          let (monoSeq, linkerSeq) = computePolymerSequence monomerLength linkerLength seq
          print $ monomerLength * monomerCount + linkerLength * (monomerCount - 1)
          print $ length seq
          putStrLn seq
          putStr "Monomer: "
          putStrLn monoSeq
          putStr "Linker : "
          putStrLn linkerSeq
          distScore <- time' "Preparing distance restraints" $ prepareDistanceScore (computePositions topo) restraintsInput
          let scoreSet = makeScoreSet "score" [ distScore
                                              , stericScore ]
          forM_ [0..3] $ \i -> 
            case getPolymer topo i of
              Left errMsg   -> do hPutStrLn stderr errMsg
                                  exitFailure
              Right polymer -> do debugPolymer polymer monoSeq linkerSeq
                                  let fragSet = polymer `delimitFragSet` fragSet'
                                  debugFragSet fragSet
                                  let polySampler = modelling $ \m -> getStdRandom $ samplePolymerModel fragSet m
                                  let polySampler' m = do r <- polySampler m
                                                          debugPolymer (RepeatPolymer.polymer $ model r) monoSeq linkerSeq
                                                          return r
                                  --annealingProtocol polySampler scoreSet 1.0 0.8 30 100 $ makePolymerModel polymer
                                  annealingProtocol polySampler scoreSet 1.0 0.5 10 10 $ makePolymerModel polymer
                                  assertM $ seq       == topo2sequence (instantiate polymer)
                                  writeFile ("poly_" ++ show i ++ ".pdb") $ showPolymer polymer
  where
    getPolymer topo i = extractPolymer first
                                       (first + monomerLength                - 1)
                                       (first + monomerLength + linkerLength - 1)
                                       5
                                       topo
      where
        first             = (monomerLength + linkerLength) * i + 1

