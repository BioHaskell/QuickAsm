{-# LANGUAGE OverloadedStrings #-}
-- | Planarity score (for beta-sheet elements in the fibril.)
module Score.Planar where

import           Data.List(groupBy)
import qualified Data.Vector           as V
import qualified Data.Vector.V3        as V3
import qualified Data.Vector.Class     as V3
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe(fromMaybe)
import           Control.Arrow((&&&))
import           Control.Exception(assert)

import Rosetta.Util(bshow)
import Util.Show(showEmpty)
import Score.ScoringFunction
import Topo
import Model

-- | Structure with information about planarity score.
data PlanarityScore = PlanarityScore {
                        sequence       :: V.Vector Char
                      , planarResidues :: V.Vector Bool
                      }

instance Show PlanarityScore where
  show (PlanarityScore s b) = concat [ ">Seq\n"
                                     , V.toList s
                                     , ">Planar"
                                     , map boolSign $ V.toList b ]

-- | Single character to show boolean value.
boolSign ::  Bool -> Char
boolSign True  = '+'
boolSign False = '-'

-- | Prepares PlanarityScore object from sequence.
prepare ::  String         -- ^ FASTA residue sequence
        ->  String         -- ^ secondary structure
        ->  PlanarityScore
prepare seq ss = PlanarityScore (V.fromList seq) $ V.fromList $ map isStrand ss
  where
    isStrand 'E' = True
    isStrand '-' = False

bondPlanarity ::  Cartesian -- first atom in the bond
               -> Cartesian -- second atom in the bond
               -> Double
bondPlanarity at1 at2 = assert (result >= 0.0) result
  where
    result = abs $ (cPos at1 - cPos at2) `V3.vdot` V3.Vector3 0 0 1

planaritySelectAtoms ps = groupBy   residEq                  .
                          filter    residueInStrand          .
                          backbone
  where
    residueInStrand c = fromMaybe False $ planarResidues ps V.!? (cResId c - 1)
    a `residEq` b     = cResId a == cResId b

-- | Shows planarity scores for all residues.
planarity ::  PlanarityScore -> CartesianTopo -> [Double]
planarity ps = concatMap (pairwise bondPlanarity) .
               planaritySelectAtoms ps

--showPlanarity :: PlanarityScore -> CartesianTopo -> [BS.ByteString]
showPlanarity ps cartopo = zipWith fmt atIds vals
  where
    fmt (id1, id2) v = BS.concat [bshow id1, bshow id2, bshow v]
    vals  = planarity ps cartopo
    atIds = concatMap (pairwise (,))    .
            map       (map cartesianId) .
            planaritySelectAtoms ps     $ cartopo

-- | Applies operator to consecutive pairs in the list, and returns a list
-- of results (shorter by one from input list.)
pairwise ::  (t -> t -> a) -> [t] -> [a]
pairwise op (a:b:cs) = a `op` b : pairwise op (b:cs)
pairwise op _        = []

-- * Scoring function wrapping
-- | ScoringFunction object returning a clash score.
planarityScore :: String
               -> String
               -> ScoringFunction
planarityScore seq ss = simpleScoringFunction "planar" fun showFun
  where
    ps = prepare seq ss
    fun ::  (Monad m, Model a) => a -> m Double
    fun     = return . sum . planarity ps . cartesianTopo
    showFun ::  (Monad m, Model a) => a -> m [BS.ByteString]
    showFun = return . showEmpty "No planarity violations." .
                       map bshow . planarity ps . cartesianTopo

