{-# LANGUAGE ExistentialQuantification, OverloadedStrings, NoMonomorphismRestriction #-}
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
import Model

import qualified Data.ByteString.Char8 as BS

-- Makes a compound score out of a set of components, and assigns a name to it.
makeScoreSet :: BS.ByteString -> [ScoringFunction] -> ScoringFunction
makeScoreSet name subComponents = self
  where
    self = ScoringFunction {
             scoreShow  = \arg -> concat `fmap` mapM (`scoreShow` arg) (tail $ components self)
           , scoreLabel = name
           , components = self:subComponents
           , scores     = fmap (zip $ map scoreLabel subComponents) . values
           }
    values :: (Model m) => m -> IO [Double]
    values arg = do subValues <- mapM (`score` arg) subComponents
                    let total = sum subValues
                    total `seq` return $! total:subValues

makeAllScores rset = makeScoreSet "score" [ makeDistanceScore rset
                                          , stericScore            ]

reportModelScore = hReportModelScore stdout

hReportModelScore handle sf mdl = do labelsValues <- scores sf mdl
                                     BS.hPutStrLn handle $ showScores labelsValues
                                     return $ snd $ head labelsValues

