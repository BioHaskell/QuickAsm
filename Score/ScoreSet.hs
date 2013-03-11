{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
-- | Implementation of polymorphic lists of scoring functions.
module Score.ScoreSet( makeScoreSet
                     , makeAllScores
                     , reportModelScore
                     , hReportModelScore
                     , ScoringFunction
                     , showScores
                     ) where

import System.IO(stdout)

import Topo(computePositions)
import Score.ScoringFunction
import Score.DistanceRestraints(makeDistanceScore)
import Score.Steric            (stericScore)

import qualified Data.ByteString.Char8 as BS

--import Topo(TorsionTopo, CartesianTopo)

-- Makes a compound score out of a set of components, and assigns a name to it.
makeScoreSet :: BS.ByteString -> [ScoringFunction] -> ScoringFunction
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

makeAllScores rset = makeScoreSet "score" [ makeDistanceScore rset
                                          , stericScore            ]

reportModelScore = hReportModelScore stdout

hReportModelScore handle sf mdl = do BS.hPutStrLn handle $ showScores labelsValues
                                     return $ snd $ head labelsValues
  where
    labelsValues = scores sf (mdl, computePositions mdl)

