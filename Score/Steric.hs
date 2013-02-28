{-# LANGUAGE TupleSections #-}
module Score.Steric( crossClashCheck
                   , selfClashCheck
                   , atomClashCheck
                   , cartesian2octree ) where

import qualified Data.Octree as O
import Data.Tree(flatten)

import Topo

import AtomParams

cartesian2octree :: [Cartesian] -> O.Octree Cartesian
cartesian2octree = O.fromList . map prepare
  where
    prepare cart = (cPos cart, cart)

atomClashCheck :: Double -> O.Octree Cartesian -> Cartesian -> [Cartesian]
atomClashCheck percent oct cart = filter (vdwClash cart) $ ats 
  where
    ats = map snd . O.withinRange oct radius . cPos $ cart
    cRadius = atomicRadius . atomType . cAtName
    radius = (cRadius cart + maxAtomicRadius) * percent
    at1 `vdwClash` at2 = cPos at1 `O.dist` cPos at2 < (cRadius at1 + cRadius at2) * percent
      where
        pos1 = cPos at1
        pos2 = cPos at2

crossClashCheck' :: Double -> [Cartesian] -> [Cartesian] -> [(Cartesian, Cartesian)]
crossClashCheck' percent carts1 carts2 = concatMap (clashingPairs percent oct) carts1
  where
    oct = cartesian2octree carts2

clashingPairs :: Double -> O.Octree Cartesian -> Cartesian -> [(Cartesian, Cartesian)]
clashingPairs percent oct c = map (c,) $ atomClashCheck percent oct c

selfClashCheck' :: Double -> [Cartesian] -> [(Cartesian, Cartesian)]
selfClashCheck' percent carts = filter notConnected $ crossClashCheck' percent carts carts
  where
    notConnected (at1, at2) = notSame (at1, at2) && not (covalentlyBound at1 at2)
    notSame (at1, at2) = residueId at1 /= residueId at2

-- TODO: make a more clever check for topology? (e.g. return CartesianTopo instead of Cartesian?)

covalentlyBound at1 at2 = cov (cAtName at1) (cAtName at2)
  where
    cov "N" "C" = True
    cov "C" "N" = True
    cov _   _   = False

residueId cart = (cResId cart, cResName cart)

defaultPercent = 0.5

selfClashCheck  = selfClashCheck'  defaultPercent

crossClashCheck = crossClashCheck' defaultPercent

