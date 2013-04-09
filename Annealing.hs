{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
-- | Annealing protocol abstracting from Model representation.
module Annealing( AnnealingState(..)
                , annealingProtocol
                , initAnnealing
                , performAnnealing
                , annealingStage
                , torsionFragSampler ) where

import System.Random
import Control.DeepSeq(NFData(..), deepseq)

import Util.Timing
import Util.Monad
import qualified Rosetta.Fragments as F
import Score.ScoringFunction
import Score.ScoreSet
import Topo
import Model
import Modelling
import FragReplace

-- * Annealing state parametrized by Modelling environment.
-- | Holds current and best models, number of successes, stages, and steps.
data AnnealingState m = AnnState { best      :: Modelling m
                                 , current   :: Modelling m
                                 , successes :: !Int
                                 , stages    :: !Int
                                 , steps     :: !Int
                                 }

instance Show (AnnealingState m) where
  show annState = concat ["Had ",            show $ successes            annState,
                          ", best score:",   show $ modelScore $ best    annState,
                          " current score:", show $ modelScore $ current annState]

-- TODO: ignoring fragSet here...
instance (NFData (Modelling m)) => NFData (AnnealingState m) where
  rnf a = rnf (best a) `seq` rnf (current a)

-- * Interface
-- | Complete annealing protocol from a given starting topology, with a
-- given Modelling object.  Parametrized by initial model (last argument),
-- and sampling trial step (first argument).
annealingProtocol :: (NFData m, Model m) => (Modelling m -> IO (Modelling m))-> ScoringFunction-> Double-> Double-> Int-> Int-> m -> IO (AnnealingState m)
annealingProtocol sampler scoreSet initialTemperature temperatureDrop stages steps initialModel =
    do initialState <- initAnnealing scoreSet initialModel
       performAnnealing sampler initialTemperature temperatureDrop stages steps initialState

-- * Sampler function template
-- | Example sampling function for TorsionModel                           
torsionFragSampler :: F.RFragSet -> Modelling TorsionModel -> IO (Modelling TorsionModel)
torsionFragSampler fragset = modelling $ modifyTorsionModelM $ \t -> getStdRandom $ randomReplace fragset t

-- | Runs a single sampling trial at a given temperature.
samplingStep :: (Modelling m -> IO (Modelling m))-> Double -> AnnealingState m -> IO (AnnealingState m)
samplingStep sampler temperature annState =
  do newMdl <- sampler $ current annState
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
-- TODO: here verify consistency of model and fragset!
-- | Initializes annealing by creating Modelling objects, and AnnealingState
initAnnealing :: (Model m) => ScoringFunction -> m -> IO (AnnealingState m)
initAnnealing scoreFxn mdl = do mdling <- initModelling scoreFxn mdl
                                return AnnState { best      = mdling
                                                , current   = mdling
                                                , successes = 0
                                                , stages    = 0
                                                , steps     = 0
                                                }


-- | A single temperature stage of annealing protocol with a given number
-- of sampler trials.
annealingStageWithReport :: (NFData m) => (Modelling m -> IO (Modelling m))-> Int -> Double -> AnnealingState m -> IO (AnnealingState m)
annealingStageWithReport sampler steps temperature annealingState = time "Annealing stage" $
    do newState <- annealingStage sampler steps temperature annealingState
       print newState
       return newState

annealingStage sampler steps temperature annealingState =
       steps `timesM` samplingStep sampler temperature $ annealingState

-- | Performs N stages of M steps of Metropolis MC annealing protocol with
-- a given, initial temperature, temperature drop, and sampling step function.
performAnnealing :: NFData m =>(Modelling m -> IO (Modelling m))-> Double -> Double-> Int -> Int -> AnnealingState m -> IO (AnnealingState m)
performAnnealing sampler initialTemperature temperatureDrop stages steps =
    foldl1 composeM $ map (annealingStageWithReport sampler steps) temperatures
  where
    temperatures = take stages $ iterate (*temperatureDrop) initialTemperature


