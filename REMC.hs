{-# LANGUAGE FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
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
import           Numeric(showFFloat)

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
exchangeCriterion ::  Double -> Double -> Double -> Double -> Double
exchangeCriterion t1 t2 e1 e2 = assert (t1 < t2) $
                                  min 1 $ exp $ (e1 - e2) * (1/t1 - 1/t2)

-- | Checks Metropolis criterion, if given parameters, and a random number generator.
checkExchangeCriterion :: Double -> Double -> Double -> t -> IO Bool
checkExchangeCriterion t1 t2 e1 e2 = checkCriterionIO $ exchangeCriterion t1 t2 e1 t2 

correctREMCState = (uncurry (==)) . (length . replicas &&& length . temperatures)

-- | Perform a single iteration of replica exchange attempts between neighbouring replicas.
-- Does it in a such way, that if a lowest score appears at highest temperature,
-- it is possible for it to reach lowest temperature in a single round of exchanges.
exchanges ::  REMCState m -> IO (REMCState m)
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
    exchange [ra]       [ta]       = return $! [ra]

-- * Annealing state parametrized by Modelling environment.
-- | Holds current and best models, number of successes, stages, and steps.
data Replica   m = Replica { ann    :: AnnealingState m
                           , replId :: !Int
                           }

data REMCState m = REMCState { replicas     :: [Replica m]
                             , temperatures :: [Double]
                             }

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
showReplicaState (repl, temp) = concat [      show  $ replId repl,
                                         ":", showF $ replicaScore repl, -- replica's current score
                                         "@", showF   temp               -- replica's current temperature
                                       ]
  where
    showF f = showFFloat (Just 3) f ""

instance (NFData (AnnealingState m)) => NFData (Replica m) where
  rnf = rnf . ann

instance (NFData (Replica m)) => NFData (REMCState m) where
  rnf = (uncurry seq) . (rnf . replicas &&& rnf . temperatures)

-- TODO: parallel REMC stage (using CloudHaskell), and reasonable switching option.
-- TODO: check sampler compatibility with Annealing module.

-- | Perform REMC protocol sequentially for given sampler, scoring
-- function, a set of temperatures, number of samplings per exchange, number
-- of exchanges(stages), and a set of models.
remcProtocol :: (NFData m, Model m) => (Modelling m -> IO (Modelling m)) ->
                                       ScoringFunction -> [Double] -> Int -> Int ->
                                       [m] -> IO (REMCState m)
remcProtocol sampler scoreSet temperatures stepsPerExchange numExchanges modelSet =
  do remcState <- initREMC scoreSet temperatures modelSet
     numExchanges `timesM` remcStageAndReport sampler stepsPerExchange $ remcState
  where
    remcStageAndReport sampler steps remcSt = do remcSt' <- remcStage sampler stepsPerExchange remcSt
                                                 print remcSt' -- DEBUG
                                                 return remcSt'

-- | Initialize state of REMC protocol.
initREMC :: Model m => ScoringFunction -> [Double] -> [m] -> IO (REMCState m)
initREMC scoreSet temperatures modelSet =
  do assertM $ length temperatures == length modelSet
     annStates <- forM modelSet $ initAnnealing scoreSet
     let replicas = zipWith Replica annStates [1..]
     return $! REMCState replicas temperatures

-- | A single stage of N annealing steps per replica, and a single exchange.
-- Takes a sampling step as a parameter.
remcStage :: NFData m => (Modelling m -> IO (Modelling m))-> Int -> REMCState m -> IO (REMCState m)
remcStage sampler steps remcState = do annStates <- jobRunner (uncurry zip $ replicas &&& temperatures $ remcState) $
                                         (\(replica, temperature) -> annealingStage sampler steps temperature $ ann replica)
                                       exchanges $! remcState { replicas = zipWith updateReplica (replicas remcState) annStates }
  where
    updateReplica replica newAnnState = replica { ann = newAnnState }
    -- for now only sequential evaluation...
    jobRunner = forM
    -- later one can use some kind of parallel jobRunner:
    -- 1. Parallel monad
    -- 2. Cloud Haskell

