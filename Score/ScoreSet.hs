{-# LANGUAGE ImpredicativeTypes #-}
-- | Implementation of polymorphic lists of scoring functions.
module ScoreSet where

import qualified Data.ByteString.Char8 as BS

import Topo


-- | Actions available for any generic scoring function or potential.
class ScoringFunction s where
  -- | Computes a value of a scoring function.
  score       :: s -> (TorsionTopo, CartesianTopo) -> Double
  -- | Shows details of scoring function components.
  scoreShow   :: s -> (TorsionTopo, CartesianTopo) -> [BS.ByteString]
  -- | Shows a label used when showing this function along with others
  scoreLabel  :: s -> BS.ByteString
  scores      :: s -> (TorsionTopo, CartesianTopo) -> [(BS.ByteString, Double)]

-- | Convenience alias for a polymorphic scoring function argument.
newtype GenericScoringFunction = SFun { unSFun :: (forall s. (ScoringFunction s) => s) }

-- | Set of polymorphic scoring functions.
data ScoreSet = ScoreSet { name       :: BS.ByteString
                         , components :: [GenericScoringFunction]
                         }

scoreComponents s arg = map (flip (score . unSFun)  arg) (components s)

instance ScoringFunction ScoreSet where
  scoreLabel   = name
  score  s arg = sum $ scoreComponents s arg
  scores s arg = zip (map (scoreLabel . unSFun) $ components s) (scoreComponents s arg)

