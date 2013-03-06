{-# LANGUAGE OverloadedStrings #-}
module Score.DistanceRestraints where

import qualified Data.ByteString.Char8 as BS
import Data.List(sortBy, find)
import Data.Tree(flatten)
import Data.Either(partitionEithers)

import qualified Rosetta.Restraints as R

import Topo

-- | Contains a restraint with a precomputed atom number
data Restraint = Restraint { source               :: R.Restraint
                           , leftAt, rightAt, num :: Int
                           , distance             :: Double
                           }

-- | Contains a list of restraints,
--   sorted by both first and second atom (thus two copies of the same set.)
data RestraintSet = RSet { byLeftAtom, byRightAtom :: [Restraint] }

prepareDistanceRestraints :: FilePath -> IO RestraintSet
prepareDistanceRestraints filepath = makeRestraintSet `fmap` R.processRestraintsFile filepath

makeRestraintSet :: [R.Restraint] -> RestraintSet
makeRestraintSet rs = undefined
--RSet { byLeftAtom  = sortBy compareRestraintsByFirstAtom  rs
--                           , byRightAtom = sortBy compareRestraintsBySecondAtom rs
--                           }

a `compareRestraintsByFirstAtom`  b = R.at1 a `compareAtoms` R.at2 b
a `compareRestraintsBySecondAtom` b = R.at1 a `compareAtoms` R.at2 b

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
atA `compareAtoms` atB = case R.resId atA `compare` R.resId atB of
                           EQ   -> R.atName atA `atCmp` R.atName atB
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

-- | Matches two atom descriptions. Reports an error if atom names and
-- residue ids are the same, but residue names are not.
match :: (BS.ByteString, BS.ByteString, Int) -> (BS.ByteString, BS.ByteString, Int) -> Bool
match a@(res1, at1, resid1) b@(res2, at2, resid2) = if (at1, resid1) /= (at2, resid2)
                                                  then False
                                                  else if res1 == res2
                                                         then True
                                                         else if (res1 == "") || (res2 == "")
                                                                then True
                                                                else error $ "Mismatch in residue names: " ++ show a ++ " vs " ++ show b

-- | Tries to match Cartesian atom against Rosetta.DistanceRestraints.Restraint atom id.
matchTopoAtId :: R.AtomId -> Cartesian -> Bool
matchTopoAtId atid cart = match (BS.pack $ cResName $ cart, BS.pack $ cAtName $ cart, cResId  cart)
                                (R.resName            atid,           R.atName  atid, R.resId atid)

-- | Precompute restraint order
-- Returns a correct `RestraintSet` and a list of error messages about incorrect `R.Restraint`s.
precomputeOrder :: [R.Restraint] -> CartesianTopo -> (RestraintSet, [String])
precomputeOrder restrs cartopo = (RSet { byLeftAtom  = sortByKey leftAt  restraints
                                       , byRightAtom = sortByKey rightAt restraints
                                       }, errors)
  where
    restraints = zipWith (\i r -> r { num = i }) [1..] unnumberedRestraints
    (errors, unnumberedRestraints) = partitionEithers $ map makeRestraint restrs
    sortByKey k = sortBy (\x y -> k x `compare` k y)
    makeRestraint :: R.Restraint -> Either String Restraint
    makeRestraint rRestr = do left  <- findAt $ R.at1 rRestr
                              right <- findAt $ R.at2 rRestr
                              return $ Restraint { source   = rRestr
                                                 , leftAt   = left
                                                 , rightAt  = right
                                                 , distance = R.goal rRestr
                                                 , num      = 0 -- to assign later
                                                 }
      where
        findAt :: R.AtomId -> Either String Int
        findAt at = case find (matchTopoAtId at) topoOrder of
                      Just cart -> Right $ cAtId cart
                      Nothing   -> Left $ "Cannot find atom: " ++ show at
    topoOrder = flatten cartopo

-- | Finds all atom positions
findPositions which restraint topo = findPositions' 0 which restraint (flatten topo)
  where
    findPositions' index which restrs@(r:rs) aList@(b:bs) | which r == index = (cPos b, r):findPositions' index     which rs     aList 
    findPositions' index which []            _                               = []
    findPositions' index which restrs@(r:rs) aList@(b:bs) | which r >  index =             findPositions' (index+1) which restrs bs
    findPositions' index which restrs        []                              = error $ "Cannot find atoms for following restraints: " ++ show restrs

-- | Show value of each restraint.
checkDistanceRestraints :: RestraintSet -> CartesianTopo -> [(R.Restraint, Double)]
checkDistanceRestraints restraints carTopo = undefined
    

-- | Give a synthetic restraint score.
scoreDistanceRestraints :: RestraintSet -> CartesianTopo -> Double
scoreDistanceRestraints = undefined 

