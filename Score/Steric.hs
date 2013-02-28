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

atomClashCheck :: O.Octree Cartesian -> Cartesian -> [Cartesian]
atomClashCheck oct cart = filter (vdwClash cart) $ ats 
  where
    ats = map snd . O.withinRange oct radius . cPos $ cart
    cRadius = atomicRadius . atomType . cAtName
    radius = cRadius cart + maxAtomicRadius
    at1 `vdwClash` at2 = cPos at1 `O.dist` cPos at2 < cRadius at1 + cRadius at2
      where
        pos1 = cPos at1
        pos2 = cPos at2

crossClashCheck :: [Cartesian] -> [Cartesian] -> [(Cartesian, Cartesian)]
crossClashCheck carts1 carts2 = concatMap (clashingPairs oct) carts1
  where
    oct = cartesian2octree carts2

clashingPairs :: O.Octree Cartesian -> Cartesian -> [(Cartesian, Cartesian)]
clashingPairs oct c = map (c,) $ atomClashCheck oct c

selfClashCheck :: [Cartesian] -> [(Cartesian, Cartesian)]
selfClashCheck carts = filter notSame $ crossClashCheck carts carts
  where
    notSame (at1, at2) = cartesianId at1 /= cartesianId at2

