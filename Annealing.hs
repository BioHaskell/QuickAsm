module Annealing( AnnealingState(..)
                , annealingProtocol ) where

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

data AnnealingState = AnnState { best      :: TorsionModelling
                               , current   :: TorsionModelling
                               , successes :: !Int
                               , stages    :: !Int
                               , steps     :: !Int
                               }

instance Show AnnealingState where
  show annState = concat ["Had ",                       show $ successes            annState,
                          ", best score:",              show $ modelScore $ best    annState,
                          " current score:",            show $ modelScore $ current annState]

-- TODO: ignoring fragSet here...
instance NFData AnnealingState where


-- TODO: here verify consistency of model and fragset!
initAnnealing scoreFxn mdl = do mdling <- initModelling scoreFxn mdl
                                return $! AnnState { best      = mdling
                                                   , current   = mdling
                                                   , successes = 0
                                                   , stages    = 0
                                                   , steps     = 0
                                                   }

-- | Runs a single sampling trial at a given temperature.
--samplingStep :: (Double -> AnnealingState -> IO AnnealingState
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

--annealingStage :: F.RFragSet -> ScoringFunction -> Int -> Double -> AnnealingState -> IO AnnealingState
annealingStage sampler scoreSet steps temperature annealingState = time "Annealing stage" $ 
    do newState <- steps `timesM` samplingStep sampler temperature $ annealingState
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

annealingProtocol sampler scoreSet initialTemperature temperatureDrop stages steps initialTopo =
    do initialState <- initAnnealing scoreSet $ initTorsionModel initialTopo
       doit initialState
  where
    temperatures = take stages $ iterate (*temperatureDrop) initialTemperature
    doit :: AnnealingState-> IO AnnealingState
    doit = foldl1 composeM $ map (annealingStage sampler scoreSet steps) temperatures 
