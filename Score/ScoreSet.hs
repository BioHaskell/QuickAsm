{-# LANGUAGE ExistentialQuantification #-}
-- | Implementation of polymorphic lists of scoring functions.
module ScoreSet( ScoringFunction(..)
               , AnyScoringFun  (..)
               , ScoreSet       (..)
               ) where

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

-- | Existiential wrapping for any scoring function. (Eg. OOP comes back.)
data AnyScoringFun = forall s. ScoringFunction s => ScoringFun s

instance ScoringFunction AnyScoringFun where
  score      (ScoringFun s) arg = score      s arg
  scoreShow  (ScoringFun s) arg = scoreShow  s arg
  scoreLabel (ScoringFun s)     = scoreLabel s
  scores     (ScoringFun s) arg = scores     s arg

-- | Set of polymorphic scoring functions.
data ScoreSet = ScoreSet { name       :: BS.ByteString
                         , components :: [AnyScoringFun]
                         }

scoreComponents s arg = map (flip score  arg) $ components s

instance ScoringFunction ScoreSet where
  scoreLabel   = name
  score  s arg = sum $ scoreComponents s arg
  scores s arg = zip (map scoreLabel $ components s) (scoreComponents s arg)

