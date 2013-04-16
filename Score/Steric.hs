{-# LANGUAGE TupleSections, OverloadedStrings, NoMonomorphismRestriction #-}
module Score.Steric( crossClashCheck
                   , selfClashCheck
                   , crossClashCheck'
                   , selfClashCheck'
                   , atomClashCheck
                   , atomClashScore
                   , selfClashScore
                   , cartesian2octree
                   , stericScore      ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Octree as O
import Data.Tree       (flatten)
import Control.DeepSeq (deepseq)

import Topo
import Model

import AtomParams
import Score.ScoringFunction(ScoringFunction(..), simpleScoringFunction)
import Util.Show(showFloat, showEmpty)

-- | Converts list of Cartesian records, into an Octree with Cartesian payload.
cartesian2octree :: [Cartesian] -> O.Octree Cartesian
cartesian2octree = O.fromList . map prepare
  where
    prepare cart = (cPos cart, cart)

-- | Given a percent of van der Waals radius, returns all those atoms
-- that are closer than the radius from an argument atom
-- (without filtering out those involved in covalent bonds.)
atomClashCheck :: Double -> O.Octree Cartesian -> Cartesian -> [Cartesian]
atomClashCheck percent oct cart = filter (vdwClash cart) (atomsWithinRange oct radius cart)
  where
    cRadius = atomicRadius . atomType . cAtName
    radius = (cRadius cart + maxAtomicRadius) * percent
    at1 `vdwClash` at2 = vdwScore percent at1 at2 > 0.0
    --at1 `vdwClash` at2 = cPos at1 `O.dist` cPos at2 < (cRadius at1 + cRadius at2) * percent

-- | Scores a given atom for cross-clashes with all atoms in an Octree,
-- with a given percent of van der Waals radius.
atomClashScore :: Double -> O.Octree Cartesian -> Cartesian -> Double
atomClashScore percent oct cart = sum $ map (vdwScore percent cart) (atomsWithinRange oct radius cart)
  where
    radius = (cRadius cart + maxAtomicRadius) * percent


-- | Simple extraction of atomic radius for a Cartesian record.
cRadius ::  Cartesian -> Double
cRadius = atomicRadius . atomType . cAtName

-- | With a given percent of van der Waals radius, returns a clash score between two atoms.
-- Uses soft-sphere approximation of vdw potential, approximating sigma
-- by average of van der Waals radii of respective atoms.
-- http://www.sklogwiki.org/SklogWiki/index.php/Soft_sphere_potential
vdwScore ::  Double -> Cartesian -> Cartesian -> Double
vdwScore percent at1 at2 = if notConnected (at1, at2) && notSame (at1, at2)
                             then bounded 0.0 maxVdW $ ((cRadius at1 + cRadius at2) * percent / (cPos at1 `O.dist` cPos at2)) ** 12
                             else                      0.0
  where
    bounded aMin aMax value = max aMin . min aMax $ value
    maxVdW = 100

-- | Returns all atoms in an Octree that are within a given radius of a given Cartesian atom.
atomsWithinRange :: O.Octree Cartesian -> Double -> Cartesian -> [Cartesian]
atomsWithinRange oct radius cart = map snd . O.withinRange oct radius . cPos $ cart

-- | Simple check for covalently connected atoms.
notConnected ::  (Cartesian, Cartesian) -> Bool
notConnected (at1, at2) = notSame (at1, at2) && not (covalentlyBound at1 at2)

-- | Simple check for the same atoms.
notSame ::  (Cartesian, Cartesian) -> Bool
notSame (at1, at2) = residueId at1 /= residueId at2

-- | Total clash score for a given topology and percentage of van der Waals radius.
selfClashScore :: Double -> CartesianTopo -> Double
selfClashScore percent carts = sum $ map (atomClashScore percent oct) $ flatten carts
  where
    oct = cartesian2octree $ flatten carts

-- | Checks for steric clashes between two different lists of Cartesian records.
crossClashCheck' :: Double -> [Cartesian] -> [Cartesian] -> [(Cartesian, Cartesian)]
crossClashCheck' percent carts1 carts2 = concatMap (clashingPairs percent oct) carts1
  where
    oct = cartesian2octree carts2

-- | Returns list of clashing pairs with the given Cartesian object.
clashingPairs :: Double -> O.Octree Cartesian -> Cartesian -> [(Cartesian, Cartesian)]
clashingPairs percent oct c = map (c,) $ atomClashCheck percent oct c

-- | For a given percent of van der Waals radius that is forbidden,
-- computes list of clashes, and filters out those that correspond
-- to covalently bound atoms.
selfClashCheck' :: Double -> [Cartesian] -> [(Cartesian, Cartesian)]
selfClashCheck' percent carts = filter notConnected $ crossClashCheck' percent carts carts

-- TODO: make a more clever check for topology? (e.g. return CartesianTopo instead of Cartesian?)

-- | Checks whether two atoms are likely to be covalently bound.
covalentlyBound ::  Cartesian -> Cartesian -> Bool
covalentlyBound at1 at2 = cov (cAtName at1) (cAtName at2)
  where
    cov "N" "C" = True
    cov "C" "N" = True
    cov _   _   = False

-- | Returns residue identifier of a given Cartesian atom.
residueId ::  Cartesian -> (Int, String)
residueId cart = (cResId cart, cResName cart)

-- | Default limit for steric clash relative to atoms' van der Waals radius.
defaultPercent ::  Double
defaultPercent = 0.5

-- | Checks for all steric clashes in a given set of Cartesian atoms,
-- with default parameters.
selfClashCheck ::  [Cartesian] -> [(Cartesian, Cartesian)]
selfClashCheck  = selfClashCheck'  defaultPercent

-- | Checks for steric clashes with other molecule, with default parameters.
crossClashCheck :: [Cartesian] -> [Cartesian] -> [(Cartesian, Cartesian)]
crossClashCheck = crossClashCheck' defaultPercent

-- | ScoringFunction object returning a clash score.
stericScore :: ScoringFunction
stericScore = simpleScoringFunction "steric" fun showFun
  where
    fun ::  (Monad m, Model a) => a -> m Double
    fun     = return . selfClashScore defaultPercent . cartesianTopo
    showFun ::  (Monad m, Model a) => a -> m [BS.ByteString]
    showFun = return . showEmpty "No steric clashes." .
                       map (showClash defaultPercent) . selfClashCheck . flatten . cartesianTopo

-- | Shows a clash scored with a given percent (0.5 by default.)
showClash ::  Double -> (Cartesian, Cartesian) -> BS.ByteString
showClash percent (a, b) = BS.pack . shows a . (' ':) . shows b . (' ':) . showFloat $ vdwScore percent a b

