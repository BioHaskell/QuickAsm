{-# LANGUAGE GADTs #-}
-- | This state captures a model along its ScoringFunction.
module Modelling( Modelling(..)
                , modelling
                , initModelling
                ) where

import Score.ScoringFunction as SF(ScoringFunction, ScoreList, scores, totalScore)
import Model
import Control.Monad.State

data Modelling m =
  (Model m) => Modelling { model   :: m
                         , score   :: ScoreList
                         , scoring :: ScoringFunction
                         }

modelScore = SF.totalScore . score

modelling act m = do m' <- act $ model m
                     sl <- SF.scores sf m'
                     return m { model   = m'
                              , score   = sl
                              , scoring = sf
                              }
  where
    sf = scoring m

initModelling sf m = (\vals -> Modelling m vals sf) `fmap` scores sf m
                           

