{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
-- | Tests polymer construction and instantiation.
module Main where

import           System.IO(hPutStrLn, stderr)
import           System.Exit(exitFailure)
import           System.Random(getStdRandom)
import           System.Mem(performGC)
import           Control.Exception(assert)
import           Control.Monad(forM_)
import qualified Data.Vector as V
import           Data.List(intercalate, nub)
import           Control.Arrow((&&&))
import           Numeric(showFFloat)
import           Control.DeepSeq(deepseq, NFData(..), force)

import           Rosetta.Silent
import qualified Rosetta.Fragments as F

import           RepeatPolymer
import           Annealing
import           REMC
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
    showEach projection = unwords . map show . V.toList . V.map (projection . V.head) . F.unRFragSet

debugPolymer ::  Polymer -> String -> String -> IO ()
debugPolymer polymer monoSeq linkerSeq = do putStrLn $ "Monomer has OXT:" ++ (show . tHasOXT . monomer) polymer
                                            putStrLn $ "Polymer has OXT:" ++ (show . tHasOXT . linker ) polymer
                                            putStrLn $ "Monomer residues:" ++ (unwords . nub . map tShowRes . backbone . monomer ) polymer
                                            putStrLn $ "Extracted monomer seq: "   ++ monoSeq2
                                            putStrLn $ "Extracted linker  seq: "   ++ linkerSeq2
                                            putStrLn $ "Recorded monomer length: " ++ (show . monomerLen)       polymer
                                            putStrLn $ "Actual   monomer length: " ++ (show . length . topo2sequence . monomer) polymer
                                            putStrLn $ "Recorded linker  length: " ++ (show . linkerLen)        polymer
                                            putStrLn $ "Actual   linker  length: " ++ (show . length . topo2sequence . linker ) polymer
                                            assertM  $ monoSeq            == monoSeq2
                                            assertM  $ linkerSeq          == linkerSeq2
                                            assertM  $ length monoSeq     == monomerLen polymer
                                            assertM  $ length linkerSeq   == linkerLen polymer
                                            assertM  $ lastResidueId (monomer polymer) == monomerLen polymer                                  
                                            assertM  $ lastResidueId (linker polymer)  == (uncurry (+) . (linkerLen &&& monomerLen)) polymer  
  where
    monoSeq2   = topo2sequence $ monomer polymer
    linkerSeq2 = topo2sequence $ linker  polymer

instance NFData ScoringFunction where

readInputs inputSilent inputFragSet restraintsInput = do
    topo <- time "Read input model" $ (head . map silentModel2TorsionTopo) `fmap` processSilentFile inputSilent
    preFrags <- time "Reading fragment set"  $ F.processFragmentsFile inputFragSet
    fragSet' <- time "Checking fragment set" $ checkFragments topo preFrags
    distScore <- time' "Preparing distance restraints" $ prepareDistanceScore (computePositions topo) restraintsInput
    scoreSet <- time' "Preparing distance restraints" $ makeAllScores 0.001 1 restraintsInput topo 
    let seq = topo2sequence topo
    let (monoSeq, linkerSeq) = computePolymerSequence monomerLength linkerLength seq
    print $ monomerLength * monomerCount + linkerLength * (monomerCount - 1)
    print $ length seq
    putStrLn seq
    putStr "Monomer: "
    putStrLn monoSeq
    putStr "Linker : "
    putStrLn linkerSeq
    case getPolymer topo 0 of
      Left errMsg   -> do hPutStrLn stderr errMsg
                          exitFailure
      Right poly    -> let result = (poly, fragSet, scoreSet, seq, monoSeq, linkerSeq)
                           fragSet = poly `delimitFragSet` fragSet'
                       in result `deepseq`
                            return result

getPolymer topo i = extractPolymer first
                                   (first + monomerLength                - 1)
                                   (first + monomerLength + linkerLength - 1)
                                   5
                                   topo
  where
    first             = (monomerLength + linkerLength) * i + 1


main = do (poly, fragSet, scoreSet,
           topoSeq, monoSeq, linkerSeq) <- time "Read all inputs, and constructed polymer" $
                                             readInputs inputSilent inputFragSet restraintsInput
          performGC
          debugPolymer poly monoSeq linkerSeq
          debugFragSet fragSet

          let polySampler = force . modelling $ \m -> getStdRandom $ samplePolymerModel fragSet m
          let polySampler' m = do r <- polySampler m
                                  r `deepseq` debugPolymer (RepeatPolymer.polymer $ model r) monoSeq linkerSeq
                                  return r
          --polymer' <- annealingProtocol polySampler scoreSet 1.0 0.8 30 100 $ makePolymerModel polymer
          --tests/test_REMC.hs
          let numReplicas      = 30
          let iniModels        = replicate numReplicas $ makePolymerModel poly -- TODO: use initial models within a polymer?
          let stepsPerExchange = 3
          let numExchanges     = 1000
          temperatures <- prepareTemperatureSet numReplicas 8000.0 1.0
          putStrLn "Temperatures: "
          putStrLn $ unwords $ map (\t -> showFFloat (Just 3) t "") temperatures
          finalState <- remcProtocol polySampler (writeREMCStateEvery 10 "remc.out" "remc.pdb")
                                     scoreSet    temperatures stepsPerExchange numExchanges iniModels
          writeREMCState "remc.out" "remc.pdb" finalState

polymerOfLastReplica = polymer . model . best . ann . last . replicas

