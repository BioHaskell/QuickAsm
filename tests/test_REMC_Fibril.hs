{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
-- | Tests fibril construction and instantiation.
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

import           Fibril
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
restraintsInput = "examples/fibril/asyn_epr.newcst"

monomerLength = 61
monomerCount  = 3
linkerLength  = 32

computeFibrilLength monomerCount monomerLength = monomerLength * monomerCount

showFibril = showCartesianTopo . instantiate

debugFragSet fragSet = do putStrLn $ "Fragment set length: "             ++ (show . V.length . F.unRFragSet) fragSet
                          putStrLn $ "Fragment set starting positions: " ++ showEach F.startPos              fragSet
                          putStrLn $ "Fragment set ending   positions: " ++ showEach F.endPos                fragSet
  where
    showEach projection = unwords . map show . V.toList . V.map (projection . V.head) . F.unRFragSet

debugFibril ::  Fibril -> IO ()
debugFibril aFibril = do putStrLn $ "Monomer has OXT:"  ++ (show . tHasOXT . monomer    )                       aFibril
                         putStrLn $ "Fibril has OXT:"   ++ (show . cHasOXT . instantiate)                       aFibril
                         putStrLn $ "Monomer residues:" ++ (unwords . nub . map tShowRes . backbone . monomer ) aFibril
                         putStrLn $ "Extracted monomer seq: "   ++ monoSeq
                         putStrLn $ "Recorded monomer length: " ++ (show . monomerLen)                          aFibril
                         putStrLn $ "Actual   monomer length: " ++ (show . length . topo2sequence . monomer)    aFibril
                         assertM  $ lastResidueId (monomer aFibril) == monomerLen                               aFibril
  where
    monoSeq = topo2sequence $ monomer aFibril

instance NFData ScoringFunction where

readInputs inputSilent inputFragSet restraintsInput = do
    topo      <- time "Read input model" $ (head . map silentModel2TorsionTopo) `fmap`
                                           processSilentFile inputSilent
    preFrags  <- time "Reading fragment set"  $ F.processFragmentsFile inputFragSet
    fragSet'  <- time "Checking fragment set" $ checkFragments topo preFrags
    distScore <- time' "Preparing distance restraints" $ prepareDistanceScore (computePositions topo)
                                                                              restraintsInput
    scoreSet  <- time' "Preparing distance restraints" $ makeAllScores 0.001 1 restraintsInput topo
    let seq = topo2sequence topo
    print $ monomerLength * monomerCount
    print $ length seq
    putStrLn seq
    case getFibril topo 0 of
      Left errMsg   -> do hPutStrLn stderr errMsg
                          exitFailure
      Right aFibril -> let result  = (aFibril, fragSet, scoreSet, seq)
                           fragSet = aFibril `delimitFragSet` fragSet'
                       in result `deepseq`
                            return result

getFibril topo i = extractFibril first
                                 (first + monomerLength - 1)
                                 5
                                 topo
  where
    first             = (monomerLength + linkerLength) * i + 1


main = do (aFibril ,
           fragSet ,
           scoreSet,
           monoSeq ) <- time "Read all inputs, and constructed fibril" $
                          readInputs inputSilent inputFragSet restraintsInput
          performGC
          debugFibril aFibril
          debugFragSet fragSet
          
          let (shiftProb, twistProb) = (0.05, 0.05)
          let fibrilSampler    = force . modelling $ \m -> getStdRandom $
                                                             sampleFibrilModel shiftProb twistProb fragSet m
          let fibrilSampler' m = do r <- fibrilSampler m
                                    r `deepseq` debugFibril (Fibril.fibril $ model r)
                                    return r
          let numReplicas      = 30
          let iniModels        = replicate numReplicas $ makeFibrilModel aFibril
          let stepsPerExchange = 3
          let numExchanges     = 1000
          temperatures <- prepareTemperatureSet numReplicas 8000.0 1.0
          putStrLn "Temperatures: "
          putStrLn $ unwords $ map (\t -> showFFloat (Just 3) t "") temperatures
          remcProtocol fibrilSampler
                       (writeREMCState "remc.out" "remc.pdb")
                       scoreSet temperatures stepsPerExchange numExchanges iniModels

--remcProtocol :: (NFData m, Model.Model m) => (Modelling m -> IO (Modelling m))
--                                          -> (REMCState m -> IO ())
--                                          -> ScoringFunction
--                                          -> [Double]
--                                          -> Int
--                                          -> Int
--                                          -> [m]
--                                          -> IO (REMCState m)
fibrilOfLastReplica = fibril . model . best . ann . last . replicas

