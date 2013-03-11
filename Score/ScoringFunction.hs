{-# LANGUAGE ExistentialQuantification #-}
-- | Implementation of polymorphic lists of scoring functions.
module Score.ScoringFunction( ScoringFunction (..)
                            ) where

import qualified Data.ByteString.Char8 as BS

import Topo(TorsionTopo, CartesianTopo)

-- TODO: Move ScoringFunction to a separate module? (Than makeScoreSet.)
-- TODO: add function that scores a cross of two models.
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

