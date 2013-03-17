{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
-- | Annealing protocol abstracting from Model representation
module Annealing( AnnealingState(..)
                , annealingProtocol
                , torsionFragSampler ) where

import System.Random
import Control.DeepSeq(NFData(..), deepseq)

import Util.Timing
import qualified Rosetta.Fragments as F
import Score.ScoringFunction
import Score.ScoreSet
import Topo
import Model
import Modelling
import FragReplace

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

-- TODO: here verify consistency of model and fragset!
-- | Initializes annealing by creating Modelling objects, and AnnealingState
initAnnealing :: (Model m) => ScoringFunction -> m -> IO (AnnealingState m)
initAnnealing scoreFxn mdl = do mdling <- initModelling scoreFxn mdl
                                return $! AnnState { best      = mdling
                                                   , current   = mdling
                                                   , successes = 0
                                                   , stages    = 0
                                                   , steps     = 0
                                                   }

-- | Runs a single sampling trial at a given temperature.
samplingStep :: (Modelling m -> IO (Modelling m))-> Double -> (AnnealingState m) -> IO (AnnealingState m)
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

-- | A single temperature stage of annealing protocol with a given number of sampler trials. 
annealingStage :: (NFData m) => (Modelling m -> IO (Modelling m))-> Int -> Double -> AnnealingState m -> IO (AnnealingState m)
annealingStage sampler steps temperature annealingState = time "Annealing stage" $ 
    do newState <- steps `timesM` samplingStep sampler temperature $ annealingState
       putStrLn $ show newState
       return newState

infix 4 `timesM`
composeM ::  Monad m => (a -> m b) -> (b -> m c) -> a -> m c
composeM a b t = do r <- a t
                    b r
-- TODO: check why it leaks stack space...
--timesM n = foldl1 composeM . replicate n

timesM :: (Monad m, NFData a) => Int -> (a -> m a) -> a -> m a
timesM 0 f a = return a
timesM n f a = do b <- f a
                  b `deepseq` timesM (n-1) f b

-- | Complete annealing protocol from a given starting topology, with a given Modelling object.
-- Parametrized by initial model (last argument), and sampling trial step (first argument).
annealingProtocol :: (NFData m, Model m) => (Modelling m -> IO (Modelling m))-> ScoringFunction-> Double-> Double-> Int-> Int-> m -> IO (AnnealingState m)
annealingProtocol sampler scoreSet initialTemperature temperatureDrop stages steps initialModel =
    do initialState <- initAnnealing scoreSet initialModel
       doit initialState
  where
    temperatures = take stages $ iterate (*temperatureDrop) initialTemperature
    --doit :: (AnnealingState -> IO AnnealingState
    doit = foldl1 composeM $ map (annealingStage sampler steps) temperatures 

-- | Example sampling function for TorsionModel                           
torsionFragSampler fragset = modelling $ modifyTorsionModelM $ \t -> getStdRandom $ randomReplace fragset t

