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
import Score.DistanceRestraints(prepareDistanceScore)
import Score.Steric            (stericScore)
import Model

import qualified Data.ByteString.Char8 as BS

-- | Makes a compound score out of a set of components, and assigns a name to it.
makeScoreSet :: BS.ByteString -> [(Double, ScoringFunction)] -> ScoringFunction
makeScoreSet name weightsAndSubComponents = self
  where
    self = ScoringFunction {
             scoreShow  = \arg -> concat `fmap` mapM (`scoreShow` arg) (tail $ components self)
           , scoreLabel = name
           , components = self:subComponents
           , scores     = fmap (zip $ map scoreLabel subComponents) . values
           }
    subComponents = map snd weightsAndSubComponents
    weights       = map fst weightsAndSubComponents
    -- | Valuation of a Model in result ScoreSet.
    values :: (Model m) => m -> IO [Double]
    values arg = do subValues <- mapM (`score` arg) subComponents
                    let total = sum $ zipWith (*) weights subValues
                    total `seq` return $! total:subValues

-- TODO: more convenient interface to scoring functions.
-- | Makes a default set of scoring functions, given a distance restraint set.
makeAllScores stericWeight restraintsInput topo = do distScore <- prepareDistanceScore (computePositions topo) restraintsInput
                                                     return $! makeScoreSet "score" [ (1.0, distScore)
                                                                                    , (stericWeight, stericScore) ]

-- | Reports details of a scoring function to the standard output.
reportModelScore = hReportModelScore stdout

-- | Writes a detailed report of a scoring function evaluation for a given
-- Model to a given file Handle.
hReportModelScore handle sf mdl = do labelsValues <- scores sf mdl
                                     BS.hPutStrLn handle $ showScores labelsValues
                                     return $ snd $ head labelsValues

