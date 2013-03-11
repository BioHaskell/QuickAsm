{-# LANGUAGE ExistentialQuantification #-}
-- | Implementation of polymorphic lists of scoring functions.
module ScoreSet( ScoringFunction (..)
               , makeScoreSet
               ) where

import qualified Data.ByteString.Char8 as BS

import Topo


-- | Actions available for any generic scoring function or potential.
data ScoringFunction = ScoringFunction {
    -- | Computes a value of a scoring function.
    score       :: (TorsionTopo, CartesianTopo) -> Double
    -- | Shows details of scoring function components.
  , scoreShow   :: (TorsionTopo, CartesianTopo) -> [BS.ByteString]
    -- | Shows a label used when showing this function along with others
  , scoreLabel  :: BS.ByteString
  , scores      :: (TorsionTopo, CartesianTopo) -> [(BS.ByteString, Double)]
  , components  :: [ScoringFunction]
  }

makeScoreSet name components =
  ScoringFunction {
    score      = sum . values 
  , scoreShow  = \arg -> concatMap (flip scoreShow arg) components 
  , scoreLabel = name
  , components = components
  , scores     = \arg -> zip (map scoreLabel components) (values arg)
  }
  where
    values         arg = map (flip score arg) components 

