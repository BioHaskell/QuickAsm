{-# LANGUAGE OverloadedStrings #-}
module Score.DistanceRestraints where

import Data.List(sortBy)

import qualified Rosetta.Restraints as R

import Topo

-- | Contains a list of restraints,
--   sorted by both first and second atom (thus two copies of the same set.)
data RestraintSet = RSet { byLeftAtom, byRightAtom :: [Restraint] }

prepareDistanceRestraints :: FilePath -> IO RestraintSet
prepareDistanceRestraints filepath = makeRestraintSet `fmap` processRestraintsFile filepath

makeRestraintSet :: [Restraint] -> RestraintSet
makeRestraintSet rs = RSet { byLeftAtom  = sortBy compareRestraintsByFirstAtom  rs
                           , byRightAtom = sortBy compareRestraintsBySecondAtom rs
                           }

a `compareRestraintsByFirstAtom`  b = at1 a `compareAtoms` at2 b
a `compareRestraintsBySecondAtom` b = at1 a `compareAtoms` at2 b

{-
data AtomId = AtomId { resName :: BS.ByteString, -- may be empty!
                       atName  :: BS.ByteString,
                       resId   :: Int
                     }
-}
-- Compare atoms in accord to the expected backbone topology
-- 1. By residue index
-- 2. By backbone order (N -> CA -> C -> next residue)
-- 3. For other atoms -> also by order in sidechain or just after previous atom etc.
-- Given that, we may easily sort restraints so that they can be checked in the order
-- in which they occur within the topology.
atA `compareAtoms` atB = case resId atA `compare` resId atB of
                           EQ   -> atName atA `atCmp` atName atB
                           ineq -> ineq

-- TODO: less fallible alternative would be to keep atom indices (as their assignment doesn't vary!)

atCmp "N"   _     = LT
atCmp _    "N"    = GT
atCmp "CA" "C"    = LT
atCmp "C"  "CA"   = LT
atCmp "C"  "O"    = LT
atCmp _    "C"    = LT
atCmp "O"  "C"    = GT
atCmp "O"  _      = GT

atCmp "HA" "CA"   = GT
atCmp "CA" "HA"   = LT
atCmp "HA" "N"    = GT
atCmp "N"  "HA"   = LT
atCmp "HA" "H"    = GT
atCmp "HA" _      = LT -- includes sidechain
atCmp _    "HA"   = GT

atCmp "N"  "H"    = LT
atCmp "H"  "N"    = GT
atCmp "H"  _      = LT
atCmp a     b   | a == b = EQ
atCmp a     b   = error $ "Still undefined atom comparison within topology: " ++ show a ++ " vs " ++ show b

-- | Show value of each restraint.
checkDistanceRestraints :: RestraintSet -> CartesianTopo -> [(Restraint, Double)]
checkDistanceRestraints restraints carTopo = undefined

-- | Give a synthetic restraint score.
scoreDistanceRestraints :: RestraintSet -> CartesianTopo -> Double
scoreDistanceRestraints = undefined 

