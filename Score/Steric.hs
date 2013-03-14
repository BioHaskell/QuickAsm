{-# LANGUAGE TupleSections, OverloadedStrings #-}
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

import AtomParams
import Score.ScoringFunction(ScoringFunction(..))

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
    at1 `vdwClash` at2 = cPos at1 `O.dist` cPos at2 < (cRadius at1 + cRadius at2) * percent
      where
        pos1 = cPos at1
        pos2 = cPos at2

atomClashScore :: Double -> O.Octree Cartesian -> Cartesian -> Double
atomClashScore percent oct cart = sum $ map (vdwScore cart) (atomsWithinRange oct radius cart)
  where
    cRadius = atomicRadius . atomType . cAtName
    radius = (cRadius cart + maxAtomicRadius) * percent
    at1 `vdwScore` at2 = if notConnected (at1, at2) && notSame (at1, at2)
                           then min 0.0 $ (cRadius at1 + cRadius at2) * percent - (cPos at1 `O.dist` cPos at2)
                           else     0.0

atomsWithinRange oct radius cart = map snd . O.withinRange oct radius . cPos $ cart


notConnected (at1, at2) = notSame (at1, at2) && not (covalentlyBound at1 at2)

notSame (at1, at2) = residueId at1 /= residueId at2

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
covalentlyBound at1 at2 = cov (cAtName at1) (cAtName at2)
  where
    cov "N" "C" = True
    cov "C" "N" = True
    cov _   _   = False

-- | Returns residue identifier of a given Cartesian atom.
residueId cart = (cResId cart, cResName cart)

-- | Default limit for steric clash relative to atoms' van der Waals radius.
defaultPercent = 0.5

-- | Checks for steric clashes with self, with default parameters.
selfClashCheck  = selfClashCheck'  defaultPercent

-- | Checks for steric clashes with other molecule, with default parameters.
crossClashCheck = crossClashCheck' defaultPercent

stericScore :: ScoringFunction
stericScore = sf
  where sf = ScoringFunction
               { score      = return . selfClashScore defaultPercent . snd
               , scoreShow  = return . map (BS.pack . show) . selfClashCheck . flatten . snd
               , scoreLabel = "steric"
               , scores     = \topos -> do result <- score sf topos
                                           result `deepseq`
                                             return [("steric", result)]
               , components = [sf]
               }
-- TODO: abstract creation of single scoring functions.

