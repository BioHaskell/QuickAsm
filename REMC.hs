{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
-- | Annealing protocol abstracting from Model representation.
module REMC( REMCState(..)
           , Replica  (..)
           , exchangeCriterion
           , checkExchangeCriterion
           , exchanges
           , remcProtocol
           ) where

import           System.Random
import           Control.DeepSeq(NFData(..), deepseq)
import           Control.Exception(assert)
import           Data.List(intercalate)
import           Control.Arrow((&&&))
import           Control.Monad(forM)

import qualified Rosetta.Fragments as F
import           Util.Monad
import           Util.Timing
import           Util.Assert(assertM)
import           Score.ScoringFunction
import           Score.ScoreSet
import           Topo
import           Model
import           Modelling
import           FragReplace
import           Annealing

-- * Exchange
-- | Replica exchange probability
-- $ P = min( 1, exp((E_i - E_j)(1/T_i - 1/T_j)) $
-- Assuming units where k=1
-- Note that first argument always corresponds to lower temperature,
-- and second to higher temperature.
exchangeCriterion :: Double -> Double -> Double -> Double -> Double
exchangeCriterion t1 t2 e1 e2 = assert (t1 < t2) $
                                  min 1 $ exp $ (e1 - e2) * (1/t1 - 1/t2)

-- | Checks Metropolis criterion, if given parameters, and a random number generator.
checkExchangeCriterion t1 t2 e1 e2 = checkCriterionIO $ exchangeCriterion t1 t2 e1 t2 

correctREMCState = (uncurry (==)) . (length . replicas &&& length . temperatures)

exchanges :: REMCState m -> IO (REMCState m)
exchanges remcSt = do assertM (correctREMCState remcSt)
                      replicas' <- (uncurry exchange) . (replicas &&& temperatures) $ remcSt
                      let remcSt' = remcSt { replicas = replicas' }
                      assertM (correctREMCState remcSt')
                      return $! remcSt' 
  where
    exchange :: [Replica m] -> [Double] -> IO [Replica m]
    exchange (ra:rb:rs) (ta:tb:ts) = do cond <- checkExchangeCriterion tb ta (replicaScore rb) (replicaScore ra)
                                        if not cond
                                          then do rbs <- exchange (rb:rs) (tb:ts)
                                                  return $ ra:rbs
                                          else do rbs <- exchange (ra:rs) (tb:ts)
                                                  return $ rb:rbs

-- * Annealing state parametrized by Modelling environment.
-- | Holds current and best models, number of successes, stages, and steps.
data Replica   m = Replica { ann    :: AnnealingState m
                           , replId :: !Int
                           }

data REMCState m = REMCState { replicas     :: [Replica m]
                             , temperatures :: [Double]
                             }

remc :: [Modelling m] -> [Double] -> [Modelling m]
remc = undefined

-- * Showing replicas and REMC state
instance Show (Replica m) where
  show replica = concat [ "Replica ",  show $ replId replica
                        , " h", tail $ show $ ann    replica ]

instance Show (REMCState m) where
  show = ("\n  " `intercalate`   ) .
         ("REMC with replicas: ":) .
         map showReplicaState      .
         uncurry zip               .
         (replicas &&& temperatures)

-- | Extracts current energy of a replica.
replicaScore ::  Replica m -> Double
replicaScore = modelScore . current . ann 

-- | Shows replica id, energy and current temperature together.
showReplicaState :: (Replica m, Double) -> String
showReplicaState (repl, temp) = concat [      show $ replId repl,
                                         ":", show $ replicaScore repl, -- replica's current score
                                         "@", show temp -- replica's current temperature
                                       ]

instance (NFData (AnnealingState m)) => NFData (Replica m) where
  rnf = rnf . ann

instance (NFData (Replica m)) => NFData (REMCState m) where
  rnf = (uncurry seq) . (rnf . replicas &&& rnf . temperatures)

-- TODO: parallel REMC stage (using CloudHaskell), and reasonable switching option.
-- TODO: check sampler compatibility with Annealing module.

remcProtocol sampler scoreSet temperatures stepsPerExchange numExchanges modelSet =
  do remcState <- initREMC scoreSet temperatures modelSet
     numExchanges `timesM` remcStep sampler $ remcState
          

initREMC scoreSet temperatures modelSet =
  do assertM $ length temperatures == length modelSet
     annStates <- forM modelSet $ initAnnealing scoreSet
     let replicas = zipWith Replica annStates [1..]
     return $! REMCState replicas temperatures

remcStep :: (Modelling m -> IO (Modelling m)) -> (REMCState m) -> IO (REMCState m)
remcStep sampler remcState = undefined

{-
-- | A single temperature stage of annealing protocol with a given number
-- of sampler trials.
annealingStage :: (NFData m) => (Modelling m -> IO (Modelling m))-> Int -> Double -> AnnealingState m -> IO (AnnealingState m)
annealingStage sampler steps temperature annealingState = time "Annealing stage" $ 
    do newState <- steps `timesM` samplingStep sampler temperature $ annealingState
       putStrLn $ show newState
       return newState
 -}

{-
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

-- * Internal functions for starting and performing a given number of annealing stages.

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


-- | A single temperature stage of annealing protocol with a given number
-- of sampler trials.
annealingStage :: (NFData m) => (Modelling m -> IO (Modelling m))-> Int -> Double -> AnnealingState m -> IO (AnnealingState m)
annealingStage sampler steps temperature annealingState = time "Annealing stage" $ 
    do newState <- steps `timesM` samplingStep sampler temperature $ annealingState
       putStrLn $ show newState
       return newState

-- | Performs N stages of M steps of Metropolis MC annealing protocol with
-- a given, initial temperature, temperature drop, and sampling step function.
performAnnealing :: NFData m =>(Modelling m -> IO (Modelling m))-> Double -> Double-> Int -> Int -> AnnealingState m -> IO (AnnealingState m)
performAnnealing sampler initialTemperature temperatureDrop stages steps =
    foldl1 composeM $ map (annealingStage sampler steps) temperatures
  where
    temperatures = take stages $ iterate (*temperatureDrop) initialTemperature
-}

