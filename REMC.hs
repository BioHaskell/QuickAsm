{-# LANGUAGE FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Annealing protocol abstracting from Model representation.
module REMC( REMCState(..)
           , Replica  (..)
           -- Exchange steps
           , exchangeCriterion
           , checkExchangeCriterion
           , exchanges
           -- Computing protocol parameters
           , temperatureStep
           , prepareTemperatureSet
           -- Running Replica Exchange
           , remcProtocol
           -- Writing REMC state
           , replica2PDB
           , replica2SilentModel
           , writeREMC2PDB
           , writeREMC2Silent
           , writeREMCState
           , writeREMCStateEvery
           ) where

import           System.Random
import           Control.DeepSeq(NFData(..), deepseq, force)
import           Control.Exception(assert)
import           Data.List(intercalate)
import           Control.Arrow((&&&))
import           Control.Monad(forM, when, void)
import           System.IO(hPutStrLn, hPutStr, hPrint, stderr)
import qualified Data.ByteString.Char8 as BS
import           GHC.Conc(forkIO)

import           Util.Parallel(parallel, withParallel)
--import qualified Control.Monad.Parallel as ParallelMonad(mapM)

import qualified Rosetta.Fragments as F
import qualified Rosetta.Silent    as S
import           Rosetta.Util(bshow)
import           Util.Monad
import           Util.Timing
import           Util.Assert(assertM)
import           Util.Show(adjust, showFloat, replace)
import           Score.ScoringFunction
import           Score.ScoreSet
import           Topo
import           Model
import           Modelling
import           FragReplace
import           Annealing

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

-- | Extracts Model object from a Replica.
replica2Model ::  Replica c -> c
replica2Model = model . current . ann

-- | Shows replica id, energy and current temperature together.
showReplicaState :: (Replica m, -- ^ replica
                     Double)    -- ^ temperature of the replica
                 -> String
showReplicaState (repl, temp) =
    concat [      adjust  2 $ show      $ replId       repl,
             ":", adjust 12 $ showFloat $ replicaScore repl, -- replica's current score
             "@", adjust  5 $ showFloat   temp               -- replica's current temperature
           ]

instance NFData (AnnealingState m) => NFData (Replica m) where
  rnf = rnf . ann

instance NFData (Replica m) => NFData (REMCState m) where
  rnf = uncurry seq . (rnf . replicas  &&&
                       rnf . temperatures)

-- | Checks integrity of REMCState.
correctREMCState = uncurry (==) . (length . replicas  &&&
                                   length . temperatures)

-- * Exchange
-- | Replica exchange probability
-- $ P = min( 1, exp((E_i - E_j)(1/T_i - 1/T_j)) $
-- Assuming units where k=1
-- Note that first argument always corresponds to lower temperature,
-- and second to higher temperature.
exchangeCriterion :: Double  -- ^ temperature of first replica (lower)
                  -> Double  -- ^ temperature of second replica (higher)
                  -> Double  -- ^ current energy of the first replica
                  -> Double  -- ^ current energy of the second replica
                  -> Double  -- ^ probability that exchange happens
exchangeCriterion t1 t2 e1 e2 = assert (t1 < t2) $
                                   min 1 $ exp $ (e1 - e2) * (1/t1 - 1/t2)

-- | Checks Metropolis criterion, if given parameters, and a random number generator.
checkExchangeCriterion :: Double  -- ^ temperature of first replica (lower)
                       -> Double  -- ^ temperature of second replica (higher)
                       -> Double  -- ^ current energy of the first replica
                       -> Double  -- ^ current energy of the second replica
                       -> IO Bool -- ^ whether replicas will be exchanged
checkExchangeCriterion t1 t2 e1 e2 = checkCriterionIO $ exchangeCriterion t1 t2 e1 e2
{- DEBUG:
checkExchangeCriterion t1 t2 e1 e2 = do result <- checkCriterionIO $ exchangeCriterion t1 t2 e1 e2
                                        putStrLn $ concat ["Exchange criterion T1=", showFloat t1,
                                                           " T2=", showFloat t2,
                                                           " E1=", showFloat e1,
                                                           " E2=", showFloat e2,
                                                           " result: ", show result]
                                        return result
 -}

-- | Perform a single iteration of replica exchange attempts between
-- neighbouring replicas.  Does it in a such way, that if a lowest score
-- appears at highest temperature, it is possible for it to reach lowest
-- temperature in a single round of exchanges.
exchanges ::  REMCState m -> IO (REMCState m)
exchanges remcSt = do assertM $ correctREMCState remcSt
                      replicas' <- uncurry exchanges' . (replicas &&& temperatures) $ remcSt
                      let remcSt' = remcSt { replicas = replicas' }
                      assertM $ correctREMCState remcSt'
                      return $! remcSt' 
  where
    exchanges' :: [Replica m] -> [Double] -> IO [Replica m]
    exchanges' (ra:rb:rs) (ta:tb:ts) = do cond <- checkExchangeCriterion tb ta (replicaScore rb) (replicaScore ra)
                                          if not cond
                                            then do rbs <- exchanges' (rb:rs) (tb:ts)
                                                    return $! ra:rbs
                                            else do rbs <- exchanges' (ra:rs) (tb:ts)
                                                    return $! rb:rbs
    exchanges' [ra]       [ta]       = return [ra]

-- * Computing temperature set for a given range of temperatures and number of replicas.
-- | Computes a fixed temperature step that takes us from high starting
-- temperatures to low goal temperature in a given number of multiplications.
temperatureStep :: Int    -- ^ number of replicas from minimal up to maximal temperature
                -> Double -- ^ highest temperature
                -> Double -- ^ lowest temperature
                -> Double
temperatureStep numReplicas maxT minT = exp $ (log minT - log maxT)/fromIntegral numReplicas

-- | Prepares a set of temperatures with given starting parameters (see
-- `temperatureStep`), and prints it to output.
prepareTemperatureSet :: Int         -- ^ number of temperature levels
                      -> Double      -- ^ highest temperature level
                      -> Double      -- ^ lowest temperature level
                      -> IO [Double] -- ^ list of temperature levels
prepareTemperatureSet numReplicas maxT minT = do when (step < 0.5) $
                                                   hPutStrLn stderr $ "Temperature step is likely too steep: " ++ shows step "."
                                                 return tempSet
  where
    step = temperatureStep numReplicas maxT minT
    tempSet = take numReplicas $ iterate (*step) maxT

-- * Running Replica Exchange
-- TODO: parallel REMC stage (using CloudHaskell), and reasonable switching option.
-- TODO: check sampler compatibility with Annealing module.

-- | Perform REMC protocol sequentially for given sampler, scoring
-- function, a set of temperatures, number of samplings per exchange, number
-- of exchanges(stages), and a set of models.
remcProtocol :: (NFData m, Model m) =>
                      (Modelling m    -> IO (Modelling m)) -- ^ sampling step
                   -> (REMCState m    -> IO ()           ) -- ^ action to run after every stage
                   -> ScoringFunction                      -- ^ scoring function
                   -> [Double]                             -- ^ temperatures
                   -> Int                                  -- ^ sampling steps to perform between exchanges
                   -> Int                                  -- ^ number of exchange stages to perform
                   -> [m]                                  -- ^ initial models
                   -> IO (REMCState m)
remcProtocol sampler everyStageAction scoreSet temperatures stepsPerExchange numExchanges modelSet =
  do remcState <- initREMC scoreSet temperatures modelSet
     putStr "Expected number of exchanges: "
     print numExchanges
     withParallel $ numExchanges `timesM` remcStageAndReport sampler stepsPerExchange $ remcState
  where
    remcStageAndReport sampler steps remcSt = do
        remcSt' <- time "REMC stage" $
            remcStage sampler stepsPerExchange remcSt
        --hPutStrLn stderr "REMC stage: "
        hPrint stderr remcSt' -- DEBUG
        --hPutStr stderr "Score components for last replica:\n"
        --reportModellingScore . current . ann . last. replicas $ remcSt
        hPrint stderr "Running end of stage action..."
        everyStageAction remcSt'
        return remcSt'

-- | Initialize state of REMC protocol.
initREMC :: Model m => ScoringFunction  -- ^ scoring function used for all Modeling objects
                    -> [Double]         -- ^ set of temperatures (sorted from highest to lowest)
                    -> [m]              -- ^ set of input models
                    -> IO (REMCState m) -- ^ initial state of simulated annealing
initREMC scoreSet temperatures modelSet =
  do assertM $ length temperatures == length modelSet
     annStates <- forM modelSet $ initAnnealing scoreSet
     let replicas = zipWith Replica annStates [1..]
     return $! REMCState replicas temperatures

-- | A single stage of N annealing steps per replica, and a single exchange.
-- Takes a sampling step as a parameter.
remcStage :: NFData m => (Modelling m -> IO (Modelling m)) -- ^ sampling step
                      -> Int                               -- ^ number of sampling steps to perform
                      -> REMCState m                       -- ^ input annealing state
                      -> IO (REMCState m)
remcStage sampler steps remcState = do putStrLn "Starting REMC stage..."
                                       annStates <- jobRunner (uncurry zip $ replicas &&& temperatures $ remcState)
                                         (\(replica, temperature) -> annealingStage sampler steps temperature $ ann replica)
                                       remcState' <- exchanges $! remcState { replicas = zipWith updateReplica (replicas remcState) annStates }
                                       print "Exchange done!"
                                       print remcState'
                                       return remcState'
  where
    updateReplica replica newAnnState = replica { ann = newAnnState }
    -- for now only sequential evaluation...
    --jobRunner = forM
    jobRunner :: [a] -> (a -> IO b) -> IO [b]
    jobRunner list action = parallel $ map action list
    -- later one can use some kind of parallel jobRunner:
    -- 1. Parallel monad
    -- 2. Cloud Haskell

-- * Saving structures from REMCState to file.
-- | Writes a REMC state as a silent file.
-- Lowest energy first.
writeREMC2Silent ::  Model m => FilePath    -- ^ output filename
                             -> REMCState m -- ^ annealing state
                             -> IO ()
writeREMC2Silent fname remc = S.writeSilentFile fname     $
                                reverse                   $
                                  zipWith assignName mdls $
                                    replicaNames remc
  where
    assignName mdl description = mdl { S.name = description }
    mdls = map replica2SilentModel . replicas $ remc

-- | Returns descriptive decoy names for all replicas within REMCState.
replicaNames ::  REMCState m -> [BS.ByteString]
replicaNames remc = zipWith nameReplica (map replId $ replicas remc) (temperatures remc)
  where
    nameReplica ::  Int -> Double -> BS.ByteString
    nameReplica rNum rTemp = BS.pack $ "R_" ++ show rNum ++ "_T" ++ (filter (/=' ') . replace '.' '_' . showFloat) rTemp

-- | Converts a single Replica into an unnamed SilentModel.
replica2SilentModel :: Model m => Replica m -> S.SilentModel
replica2SilentModel = uncurry assignScores . (conversion &&& mscore)
  where
    assignScores :: S.SilentModel -> ScoreList -> S.SilentModel
    assignScores m s = m { S.scores = s }
    mscore ::  Replica m -> ScoreList
    mscore = modelScores . current . ann
    conversion :: Model m => Replica m -> S.SilentModel
    conversion = torsionTopo2SilentModel . torsionTopo . replica2Model

-- | Saving output of REMC to a single PDB file with multiple models.
writeREMC2PDB ::  Model m => FilePath    -- ^ output filename
                          -> REMCState m -- ^ annealing state
                          -> IO ()
writeREMC2PDB fname remc = BS.writeFile fname $ BS.intercalate "\n" $
                             BS.concat ["REMARK      Writing ",
                                        bshow $ length $ replicas remc,
                                        " replicas."
                                       ] :
                             zipWith3 replica2PDB (revOrdinals  remc)
                                                  (temperatures remc)
                                                  (replicas     remc)

-- | Helper function returning reversed ordinal numbers of the
-- replicas/models in REMCState.
revOrdinals remc = reverse [1..length (replicas remc)]

-- | Converts a temperature and replice to PDB format string.
replica2PDB ::  Model m => Int           -- ^ ordinal number of the replica
                        -> Double        -- ^ current temperature of the replica
                        -> Replica m     -- ^ saved replica
                        -> BS.ByteString
replica2PDB nth temp repl = BS.concat [ "REMARK "
                                      , scoreHeader
                                      , "\nREMARK "
                                      , showScores smdl
                                      , "\nREMARK Final temperature: "
                                      , bshow temp
                                      , "\nREMARK Replica id: "
                                      , bshow $ replId repl
                                      , "\nMODEL "
                                      , bshow nth
                                      , "\n"
                                      , BS.pack              $
                                        showTorsionTopoAsPDB $
                                        torsionTopo          $
                                        replica2Model repl
                                      , "\nENDMDL" ]
  where
    scores                    = modelScores . current . ann $ repl
    smdl                      = replica2SilentModel repl
    (scoreHeader, showScores) = S.makeScoreShower [smdl]

-- | Write annealing state to disk every Nth exchange.
writeREMCStateEvery :: Model m => Int         -- ^ every how many steps to write files
                               -> FilePath    -- ^ silent output filename
                               -> FilePath    -- ^ PDB output filename
                               -> REMCState m -- ^ annealing state
                               -> IO ()
writeREMCStateEvery n silentOutput pdbOutput remcState = do
    hPutStrLn stderr $ concat ["Performed steps: ",
                               show $ remcPerformedExchangeStages remcState,
                               " every ",
                               show n,
                               " condition is ",
                               show $ remcPerformedExchangeStages remcState `mod` n
                              ]
    when (remcPerformedExchangeStages remcState `mod` n == 0) $
      writeREMCState silentOutput pdbOutput remcState

-- | Return number of stages performed during entire annealing protocol.
remcPerformedExchangeStages ::  REMCState m ->  Int
remcPerformedExchangeStages = stages . ann . head . replicas

-- | Return number of steps performed during entire annealing protocol.
remcPerformedSamplingSteps ::  REMCState m ->  Int
remcPerformedSamplingSteps = steps . ann . head . replicas

-- | Saves REMC state into both silent and PDB output files.
-- Intended to work as an action applied at every REMC stage.
writeREMCState :: Model m => FilePath    -- ^ silent output filename
                          -> FilePath    -- ^ PDB output filename
                          -> REMCState m -- ^ current annealing state
                          -> IO ()
writeREMCState silentOutput pdbOutput remcState = void . forkIO . time "Writing current REMC state" $
  do writeREMC2Silent silentOutput remcState
     writeREMC2PDB    pdbOutput    remcState

