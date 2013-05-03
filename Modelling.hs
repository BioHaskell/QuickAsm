{-# LANGUAGE GADTs, CPP #-}
-- | This state captures a model along its ScoringFunction.
module Modelling( Modelling(..)
                , TorsionModelling
                , modelling
                , initModelling
                , modelScore
                , showModellingScore
                , reportModellingScore
                ) where

import qualified Data.ByteString.Char8 as BS

import Score.ScoringFunction as SF(ScoringFunction(scoreShow), ScoreList, scores, totalScore)
import Model
import Control.DeepSeq(NFData(..))
import Control.Monad(mapM_)

-- | Data structure containing a model and its modelling environment
-- (scoring function.)  It also stores a list of current score values, so
-- one can easily update model, or use memoized score values.
data Modelling m =
  (Model m) => Modelling { model       :: m
                         , modelScores :: ScoreList
                         , scoring     :: ScoringFunction
                         }

#ifdef OLD_BYTESTRING
instance NFData BS.ByteString where
#endif

instance (NFData m) => NFData (Modelling m) where
  rnf ming = rnf (model ming) `seq` rnf (modelScores ming)

-- | Type alias for torsion space modelling.
type TorsionModelling = Modelling TorsionModel

-- | Returns a total score of a model.
modelScore ::  Modelling m -> Double
modelScore = SF.totalScore . modelScores

-- | Runs an action on a model, and updates Modelling environment.
modelling :: Model m => (m -> IO m) -- ^ action transforming a Model
                     -> Modelling m -- ^ Modelling environment to which the action is applied
                     -> IO (Modelling m)
modelling act m = do m' <- act $ model m
                     sl <- SF.scores sf m'
                     return m { model       = m'
                              , modelScores = sl
                              , scoring     = sf
                              }
  where
    sf = scoring m

-- | Pretty showing score components within a given Modelling environment.
showModellingScore ::  Model m => Modelling m
                               -> IO [BS.ByteString]
showModellingScore m = scoreShow (scoring m) (model m)

-- | Reports current score components within Modelling environment to stdout.
reportModellingScore ::  Model m => Modelling m -> IO ()
reportModellingScore m = showModellingScore m >>= mapM_ BS.putStrLn

-- | Initializes Modelling environment with a scoring function and a model.
initModelling :: Model m => ScoringFunction -- ^ scoring function
                         -> m               -- ^ input model
                         -> IO (Modelling m)
initModelling sf m = (\vals -> Modelling m vals sf) `fmap` scores sf m


