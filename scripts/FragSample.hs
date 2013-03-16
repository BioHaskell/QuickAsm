{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import System.Environment
import System.Random (randomR, getStdRandom, RandomGen)
import System.Exit   (exitFailure, exitSuccess)
import Control.Monad (when)
import System.IO     (hPutStrLn, stderr)

import qualified Data.Vector           as V
import qualified Data.ByteString.Char8 as BS

import qualified Rosetta.Fragments     as F
import qualified Rosetta.Silent        as S
import FragReplace
import Topo
import Util.Timing
import Score.DistanceRestraints(prepareDistanceScore)
import Score.Steric(stericScore)
import Score.ScoreSet(makeScoreSet)
import Score.ScoringFunction(score, ScoringFunction)

main = do args <- getArgs
          when (length args /= 6) $ do hPutStrLn stderr "FragSample <fragments_R3> <silentInput.out> <restraints.cst> <currentOutput.out> <best.out> <output.pdb>"
                                       hPutStrLn stderr "NOTE: only first model from <silentInput.out> is taken."
                                       exitFailure
          let [ fragmentInputFilename, silentInputFilename, restraintsInput ,
                silentCurrentOutputFilename , silentBestOutputFilename, pdbOutputFilename ] = args
          main' fragmentInputFilename silentInputFilename restraintsInput silentCurrentOutputFilename silentBestOutputFilename pdbOutputFilename
          exitSuccess

type AnnealingState = (TorsionTopo, Double, TorsionTopo, Double, Int)

--samplingStep :: FragmentSet -> ScoringFunction -> AnnealingState -> AnnealingState
samplingStep :: F.RFragSet-> ScoringFunction-> Double -> AnnealingState -> IO AnnealingState
samplingStep fragset scoreSet temperature (bestTopo, bestScore, curTopo, curScore, successes) = 
  do newTopo <- getStdRandom $ randomReplace fragset curTopo
     let newCarTopo = computePositions newTopo
     newScore <- score scoreSet (newTopo, newCarTopo)
     crit <- checkMetropolisCriterion temperature newScore curScore
     let newSuccesses = if crit
                          then successes + 1
                          else successes
     let (newCurTopo, newCurScore)   = if crit
                                          then (newTopo, newScore)
                                          else (curTopo, curScore)
     let (newBestTopo, newBestScore) = if curScore < bestScore
                                          then (newTopo,  newScore )
                                          else (bestTopo, bestScore)
     return $! (newBestTopo, newBestScore, newCurTopo, newCurScore, newSuccesses)

annealingStage :: F.RFragSet -> ScoringFunction -> Int -> Double -> AnnealingState -> IO AnnealingState
annealingStage fragSet scoreSet steps temperature annealingState = time "Annealing stage" $ 
    do newState <- steps `timesM` samplingStep fragSet scoreSet temperature $ annealingState
       putStrLn $ concat ["Had ",                      show $ successCount newState,
                          "successes at temperature ", show temperature,
                          ", best score:",             show $ bestScore annealingState,
                          " current score:",           show $ currentScore annealingState]
       return newState

infix 4 `timesM`
composeM a b t = do r <- a t
                    b r
timesM n = foldl1 composeM . replicate n
-- TODO: annealing state should be a record
bestScore    (_, b, _, _, _) = b
currentScore (_, _, _, c, _) = c
successCount (_, _, _, _, f) = f

annealingProtocol fragSet scoreSet initialTemperature temperatureDrop stages steps initialTopo =
    do initialScore <- score scoreSet (initialTopo, computePositions initialTopo)
       let initialState = (initialTopo, initialScore, initialTopo, initialScore, 0)
       doit initialState
  where
    temperatures = take stages $ iterate (*temperatureDrop) initialTemperature
    doit :: AnnealingState-> IO AnnealingState
    doit = foldl1 composeM $ map (annealingStage fragSet scoreSet steps) temperatures 

instantiate scoreSet topo = do score <- score scoreSet (topo, cartopo)
                               return $! (topo, cartopo, score)
  where
    cartopo = computePositions topo

main' fragmentInputFilename silentInputFilename restraintsInput silentCurrentOutputFilename silentBestOutputFilename pdbOutputFilename = 
    do fragset <- time "Reading fragment set" $ F.processFragmentsFile fragmentInputFilename
       mdls    <- time "Reading silent file " $ S.processSilentFile    silentInputFilename
       mdl <- timePure "Converting silent model to topology" $ silentModel2TorsionTopo $ head mdls
       let cartopo = computePositions mdl
       distScore <- time' "Preparing distance restraints" $ prepareDistanceScore cartopo restraintsInput
       let scoreSet = makeScoreSet "score" [ distScore
                                           , stericScore ]
       iniScore <- time "Computing initial score" $ score scoreSet (mdl, cartopo)
       --(newMdl, newCartopo, newScore) <- samplingStep fragset scoreSet (mdl, cartopo, iniScore)
       ((newMdl, newScore, bestMdl, bestScore, successes) :: AnnealingState) <- time "Annealing protocol" $ annealingProtocol fragset scoreSet (iniScore*0.2) 0.9 100 100 mdl
       let newCartopo = computePositions bestMdl
       putStrLn $ "Final score " ++ show newScore
       putStrLn $ "Best score "  ++ show bestScore
       time "Writing silent file for final model" $ S.writeSilentFile silentCurrentOutputFilename [torsionTopo2SilentModel newMdl]
       time "Writing silent file for best model"  $ S.writeSilentFile silentBestOutputFilename    [torsionTopo2SilentModel bestMdl]
       time "Writing PDB file"    $ writeFile pdbOutputFilename $ showCartesianTopo newCartopo
       

