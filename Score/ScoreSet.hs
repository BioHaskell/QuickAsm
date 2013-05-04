{-# LANGUAGE ExistentialQuantification, OverloadedStrings, NoMonomorphismRestriction #-}
-- | Implementation of polymorphic lists of scoring functions.
module Score.ScoreSet( makeScoreSet
                     , makeAllScores
                     , makeAllScores'
                     , reportModelScore
                     , hReportModelScore
                     , ScoringFunction
                     , showScores
                     ) where

import System.IO(stdout, Handle)

import Topo(computePositions, TorsionTopo, CartesianTopo)
import Score.ScoringFunction
import Score.DistanceRestraints(prepareDistanceScore)
import Score.Steric            (stericScore)
import Model

import qualified Data.ByteString.Char8 as BS

-- | Makes a compound score out of a set of components, and assigns a name to it.
makeScoreSet :: BS.ByteString               -- ^ name of compound score
             -> [(Double, ScoringFunction)] -- ^ list of weights and scoring functions
             -> ScoringFunction
makeScoreSet name weightsAndSubComponents = self
  where
    self = ScoringFunction {
             scoreShow  = \arg -> concat `fmap` mapM (`scoreShow` arg) (tail $ components self)
           , scoreLabel = name
           , components = self:subComponents
           , scores     = fmap (zip $ name:map scoreLabel subComponents) . values
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
makeAllScores :: Double                      -- ^ weight of steric clash
              -> Double                      -- ^ weight of distance restraints
              -> FilePath                    -- ^ distance restraints input filename
              -> TorsionTopo                 -- ^ starting structure TorsionTopo
              -> IO ScoringFunction
makeAllScores stericWeight distWeight restraintsInput topo = makeAllScores' stericWeight distWeight restraintsInput topo []

-- | Makes a default set of scoring functions, given a distance restraint
-- set, and adds additional components.
makeAllScores' :: Double                      -- ^ weight of steric clash
               -> Double                      -- ^ weight of distance restraints
               -> FilePath                    -- ^ distance restraints input filename
               -> TorsionTopo                 -- ^ starting structure TorsionTopo
               -> [(Double,                   
                    ScoringFunction)]         -- ^ list of weights and scoring functions.
               -> IO ScoringFunction
makeAllScores' stericWeight distWeight restraintsInput topo otherComponents = 
                     do distScore <- prepareDistanceScore (computePositions topo) restraintsInput
                        return $! makeScoreSet "score" ([ (distWeight,   distScore  )
                                                        , (stericWeight, stericScore) ] ++
                                                          otherComponents)

-- | Reports details of a scoring function to the standard output.
reportModelScore :: Model m => ScoringFunction -- ^ scoring function
                            -> m               -- ^ model structure with scores
                            -> IO Double
reportModelScore = hReportModelScore stdout

-- | Writes a detailed report of a scoring function evaluation for a given
-- Model to a given file Handle.
hReportModelScore :: Model m => Handle          -- ^ output handle
                             -> ScoringFunction -- ^ scoring function
                             -> m               -- ^ model structure with scores
                             -> IO Double
hReportModelScore handle sf mdl = do labelsValues <- scores sf mdl
                                     BS.hPutStrLn handle $ showScores labelsValues
                                     return $ snd $ head labelsValues

