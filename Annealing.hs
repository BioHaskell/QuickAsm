{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
-- | Annealing protocol abstracting from Model representation.
module Annealing( AnnealingState(..)
                , annealingProtocol
                , initAnnealing
                , performAnnealing
                , annealingStage
                , torsionFragSampler ) where

import System.Random
import Control.DeepSeq(NFData (..),
                       deepseq)

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
data AnnealingState m = AnnState { best      :: Modelling m -- ^ best model with scoring function
                                 , current   :: Modelling m -- ^ current model with scoring function
                                 , successes :: !Int        -- ^ number of successes during simulated annealing so far
                                 , stages    :: !Int        -- ^ number of stages during simulated annealing so far
                                 , steps     :: !Int        -- ^ number of sampling attempts during simulated annealing so far
                                 }

instance Show (AnnealingState m) where
  show annState = concat ["After ",          show $ steps                annState,
                          " steps, and " ,   show $ stages               annState,
                          " stages had ",    show $ successes            annState,
                          ", best score:",   show $ modelScore $ best    annState,
                          " current score:", show $ modelScore $ current annState]

-- TODO: ignoring fragSet here...
instance (NFData (Modelling m)) => NFData (AnnealingState m) where
  rnf a = rnf (best a) `seq` rnf (current a)

-- * Interface
-- | Complete annealing protocol from a given starting topology, with a
-- given Modelling object.  Parametrized by initial model (last argument),
-- and sampling trial step (first argument).
annealingProtocol :: (NFData m, Model m) => (Modelling m -> IO (Modelling m)) -- ^ sampling step
                                         -> ScoringFunction                   -- ^ scoring function
                                         -> Double                            -- ^ initial temperature
                                         -> Double                            -- ^ temperature drop after each stage
                                         -> Int                               -- ^ number of stages
                                         -> Int                               -- ^ number of steps
                                         -> m                                 -- ^ initial model
                                         -> IO (AnnealingState m)             -- ^ result of the annealing
annealingProtocol sampler scoreSet initialTemperature temperatureDrop stages steps initialModel =
    do initialState <- initAnnealing scoreSet initialModel
       performAnnealing sampler initialTemperature temperatureDrop stages steps initialState

-- * Sampler function template
-- | Example sampling function for TorsionModel                           
torsionFragSampler :: F.RFragSet                  -- ^ fragments
                   -> Modelling TorsionModel      -- ^ Modelling environment
                   -> IO (Modelling TorsionModel) -- ^ result
torsionFragSampler fragset = modelling $ modifyTorsionModelM $ \t -> getStdRandom $ randomReplace fragset t

-- | Runs a single sampling trial at a given temperature.
samplingStep :: (Modelling m -> IO (Modelling m)) -- ^ sampling function
             -> Double                            -- ^ temperature
             -> AnnealingState m                  -- ^ current AnnealingState
             -> IO (AnnealingState m)             -- ^ results AnnealingState
samplingStep sampler temperature annState =
  do newMdl <- sampler $ current annState
     let newScore = modelScore newMdl
     crit <- checkMetropolisCriterion temperature (modelScore $ current annState) newScore
     return $! annState { steps = steps annState + 1
                        , successes = if crit
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
initAnnealing :: (Model m) => ScoringFunction       -- ^ scoring function for a modle
                           -> m                     -- ^ initial model
                           -> IO (AnnealingState m) -- ^ initial AnnealingState
initAnnealing scoreFxn mdl = do mdling <- initModelling scoreFxn mdl
                                return AnnState { best      = mdling
                                                , current   = mdling
                                                , successes = 0
                                                , stages    = 0
                                                , steps     = 0
                                                }


-- | A single temperature stage of annealing protocol with a given number
-- of sampler trials.
annealingStageWithReport :: (NFData m) => (Modelling m -> IO (Modelling m)) -- ^ sampling function
                                       -> Int                               -- ^ number of sampling steps
                                       -> Double                            -- ^ temperature of the stage
                                       -> AnnealingState m                  -- ^ starting AnnealingState
                                       -> IO (AnnealingState m)             -- ^ result
annealingStageWithReport sampler steps temperature annealingState = --time "Annealing stage" $
    do newState <- annealingStage sampler steps temperature annealingState
       print newState
       return newState

-- | 
annealingStage :: NFData m => (Modelling m -> IO (Modelling m)) -- ^ sampling function
                           -> Int                               -- ^ number of sampling steps in a stage
                           -> Double                            -- ^ temperature of the stage
                           -> AnnealingState m                  -- ^ starting AnnealingState
                           -> IO (AnnealingState m)             -- ^ result
annealingStage sampler steps temperature annealingState =
    do result <- steps `timesM` samplingStep sampler temperature $ annealingState
       result `deepseq` return $! result { stages = stages result + 1 }


-- | Performs N stages of M steps of Metropolis MC annealing protocol with
-- a given, initial temperature, temperature drop, and sampling step function.
performAnnealing :: NFData m => (Modelling m -> IO (Modelling m)) -- ^ sampling function
                             -> Double                            -- ^ starting (highest) temperature
                             -> Double                            -- ^ ratio of temperatures between stages (should be >1.0)
                             -> Int                               -- ^ number of stages with a given temperature
                             -> Int                               -- ^ number of steps at each stage
                             -> AnnealingState m                  -- ^ input AnnealingState
                             -> IO (AnnealingState m)
performAnnealing sampler initialTemperature temperatureDrop stages steps =
    foldl1 composeM $ map (annealingStageWithReport sampler steps) temperatures
  where
    temperatures = take stages $ iterate (*temperatureDrop) initialTemperature


