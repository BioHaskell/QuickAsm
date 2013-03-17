{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs, StandaloneDeriving #-}
module Main where

import System.Environment
import System.Random (randomR, getStdRandom, RandomGen)
import System.Exit   (exitFailure, exitSuccess)
import Control.Monad (when)
import Control.DeepSeq(deepseq, NFData(..))
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
import Modelling
import Model

main = do args <- getArgs
          when (length args /= 6) $ do hPutStrLn stderr "FragSample <fragments_R3> <silentInput.out> <restraints.cst> <currentOutput.out> <best.out> <output.pdb>"
                                       hPutStrLn stderr "NOTE: only first model from <silentInput.out> is taken."
                                       exitFailure
          let [ fragmentInputFilename, silentInputFilename, restraintsInput ,
                silentCurrentOutputFilename , silentBestOutputFilename, pdbOutputFilename ] = args
          main' fragmentInputFilename silentInputFilename restraintsInput silentCurrentOutputFilename silentBestOutputFilename pdbOutputFilename
          exitSuccess

data AnnealingState = AnnState { best      :: TorsionModelling
                               , current   :: TorsionModelling
                               , successes :: !Int
                               , stages    :: !Int
                               , steps     :: !Int
                               , fragSet   :: F.RFragSet
                               }

instance Show AnnealingState where
  show annState = concat ["Had ",                       show $ successes            annState,
                          ", best score:",              show $ modelScore $ best    annState,
                          " current score:",            show $ modelScore $ current annState]

-- TODO: ignoring fragSet here...
instance NFData AnnealingState where


-- TODO: here verify consistency of model and fragset!
initAnnealing fragset scoreFxn mdl = do mdling <- initModelling scoreFxn mdl
                                        return $! AnnState { best      = mdling
                                                           , current   = mdling
                                                           , fragSet   = fragset
                                                           , successes = 0
                                                           , stages    = 0
                                                           , steps     = 0
                                                           }

-- | Runs a single sampling trial at a given temperature.
samplingStep :: Double -> AnnealingState -> IO AnnealingState
samplingStep temperature annState = 
  do newMdl <- modellingFragReplacement (fragSet annState) (current annState)
     let newScore = modelScore newMdl
     crit <- checkMetropolisCriterion temperature (modelScore $ current annState) newScore
     return $! annState { successes = if crit
                                         then successes annState + 1
                                         else successes annState
                        , current   = if crit
                                         then newMdl
                                         else current annState
                        , best      = if newScore < modelScore (best annState)
                                         then newMdl
                                         else best annState }

modellingFragReplacement fragset = modelling $ modifyTorsionModelM $ \t -> getStdRandom $ randomReplace fragset t

annealingStage :: F.RFragSet -> ScoringFunction -> Int -> Double -> AnnealingState -> IO AnnealingState
annealingStage fragSet scoreSet steps temperature annealingState = time "Annealing stage" $ 
    do newState <- steps `timesM` samplingStep temperature $ annealingState
       putStrLn $ show newState
       return newState

infix 4 `timesM`
composeM a b t = do r <- a t
                    b r
-- TODO: check why it leaks stack space...
--timesM n = foldl1 composeM . replicate n

timesM 0 f a = return a
timesM n f a = do b <- f a
                  b `deepseq` timesM (n-1) f b

--type AnnealingState = (TorsionTopo, Double, TorsionTopo, Double, Int)

-- TODO: annealing state should be a record
bestScore    (_, b, _, _, _) = b
currentScore (_, _, _, c, _) = c
successCount (_, _, _, _, f) = f

annealingProtocol fragSet scoreSet initialTemperature temperatureDrop stages steps initialTopo =
    do initialState <- initAnnealing fragSet scoreSet $ initTorsionModel initialTopo
       doit initialState
  where
    temperatures = take stages $ iterate (*temperatureDrop) initialTemperature
    doit :: AnnealingState-> IO AnnealingState
    doit = foldl1 composeM $ map (annealingStage fragSet scoreSet steps) temperatures 

main' fragmentInputFilename silentInputFilename restraintsInput silentCurrentOutputFilename silentBestOutputFilename pdbOutputFilename = 
    do fragset <- time "Reading fragment set" $ F.processFragmentsFile fragmentInputFilename
       mdls    <- time "Reading silent file " $ S.processSilentFile    silentInputFilename
       mdl <- timePure "Converting silent model to topology" $ silentModel2TorsionTopo $ head mdls
       let cartopo = computePositions mdl
       distScore <- time' "Preparing distance restraints" $ prepareDistanceScore cartopo restraintsInput
       let scoreSet = makeScoreSet "score" [ distScore
                                           , stericScore ]
       iniScore <- time "Computing initial score" $ score scoreSet $ initTorsionModel mdl
       annState <- time "Annealing protocol" $ annealingProtocol fragset scoreSet (iniScore*0.2) 0.9 30 100 mdl
       let bestMdl    = model $ best    annState
       let currentMdl = model $ current annState
       putStrLn $ "Final score " ++ show (modelScore $ current annState)
       putStrLn $ "Best score "  ++ show (modelScore $ best    annState)
       time "Writing silent file for final model" $ S.writeSilentFile silentCurrentOutputFilename [torsionTopo2SilentModel $ tTopo currentMdl]
       time "Writing silent file for best model"  $ S.writeSilentFile silentBestOutputFilename    [torsionTopo2SilentModel $ tTopo bestMdl   ]
       time "Writing PDB file"   $ writeFile pdbOutputFilename $ showCartesianTopo $ cartesianTopo bestMdl

