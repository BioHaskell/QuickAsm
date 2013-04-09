{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-- Unit tests for REMC module.
module Main where

import System.Random(randomRIO)

import Test.QuickCheck.All
import Test.QuickCheck

import Score.ScoringFunction
import Model
import REMC

-- | Fake Model instance, holding just its score.
newtype TestModel = TestModel { unTestModel :: Double }

newModel = sampler $! TestModel 0.0

sampler (TestModel v) = do r <- randomRIO (0.0, 1.0)
                           return $! TestModel $ v + r
             

instance Model TestModel where
  torsionTopo   _ = undefined
  cartesianTopo _ = undefined

testScore :: ScoringFunction
testScore = ScoringFunction { scoreShow  = \m -> return [""]
                            , scoreLabel = name 
                            , scores     = \m -> undefined -- return [(name, unTestModel m)]
                            , components = [testScore]
                            }
  where
    name = "TestScore"


prop_criterion_certain_drop (Positive t1) (Positive tDelta) (Positive eDelta) (Positive e2) = (t1 < t2) && (e1 > e2) ==> exchangeCriterion t1 t2 e1 e2 == 1.0
  where
    e1 = e2 + eDelta
    t2 = t1 + tDelta -- guarantees t1 < t2

prop_criterion_range (Positive t1) (Positive tDelta) (Positive e1) (Positive e2) = (t1 < t2)              ==> (c >= 0.0) && (c <= 1.0)
  where
    t2 = t1 + tDelta -- guarantees t1 < t2
    c = exchangeCriterion t1 t2 e1 e2

prop_criterion_monotonic (Positive t1) (Positive tDelta) (Positive e1) (Positive e2) (Positive e3) = (t1 < t2) && (e2 < e3) ==> c2 >= c3
  where
    t2 = t1 + tDelta -- guarantees t1 < t2
    c2 = exchangeCriterion t1 t2 e1 e2
    c3 = exchangeCriterion t1 t2 e1 e3

main = $quickCheckAll

