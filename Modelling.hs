{-# LANGUAGE GADTs #-}
-- | This state captures a model along its ScoringFunction.
module Modelling( Modelling(..)
                , TorsionModelling
                , modelling
                , initModelling
                , modelScore
                ) where

import qualified Data.ByteString.Char8 as BS

import Score.ScoringFunction as SF(ScoringFunction, ScoreList, scores, totalScore)
import Model
import Control.DeepSeq(NFData(..))

data Modelling m =
  (Model m) => Modelling { model       :: m
                         , modelScores :: ScoreList
                         , scoring     :: ScoringFunction
                         }

instance NFData BS.ByteString where

instance (NFData m) => NFData (Modelling m) where
  rnf ming = rnf (model ming) `seq` rnf (modelScores ming)

type TorsionModelling = Modelling TorsionModel

modelScore ::  Modelling m -> Double
modelScore = SF.totalScore . modelScores

modelling :: Model m => (m -> IO m) -> Modelling m -> IO (Modelling m)
modelling act m = do m' <- act $ model m
                     sl <- SF.scores sf m'
                     return m { model       = m'
                              , modelScores = sl
                              , scoring     = sf
                              }
  where
    sf = scoring m

initModelling sf m = (\vals -> Modelling m vals sf) `fmap` scores sf m


