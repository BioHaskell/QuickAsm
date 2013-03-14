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

import Topo(computePositions, TorsionTopo, CartesianTopo)
import Score.ScoringFunction
import Score.DistanceRestraints(makeDistanceScore)
import Score.Steric            (stericScore)

import qualified Data.ByteString.Char8 as BS

type Scored = (TorsionTopo, CartesianTopo)

-- Makes a compound score out of a set of components, and assigns a name to it.
makeScoreSet :: BS.ByteString -> [ScoringFunction] -> ScoringFunction
makeScoreSet name components =
  ScoringFunction {
    score      = fmap sum . values 
  , scoreShow  = \arg -> concat `fmap` mapM (`scoreShow` arg) components 
  , scoreLabel = name
  , components = components
  , scores     = fmap (zip $ map scoreLabel components) . values
  }
  where
    values :: Scored -> IO [Double]
    values arg = mapM (`score` arg) components 

makeAllScores rset = makeScoreSet "score" [ makeDistanceScore rset
                                          , stericScore            ]

reportModelScore = hReportModelScore stdout

hReportModelScore handle sf mdl = do labelsValues <- scores sf (mdl, computePositions mdl)
                                     BS.hPutStrLn handle $ showScores labelsValues
                                     return $ snd $ head labelsValues

