module Topo where

import Data.Vector.V3
import Data.Tree

data Torsion = Torsion { tPlanar, tDihedral :: !Double
                       , tBondLen           :: !Double
                       , tAtName            :: !String
                       , tResName           :: !String
                       , tResId             :: !Int    }

data Cartesian = Cartesian { cPos     :: !Vector3
                           , cAtName  :: !String
                           , cResName :: !String
                           , cResId   :: !Int     }

type TorsionTopo   = Tree Torsion

type CartesianTopo = Tree Cartesian

xferC tors = Cartesian { cPos     = 0
                       , cAtName  = tAtName  tors
                       , cResName = tResName tors
                       , cResId   = tResId   tors }

xferT cart = Torsion { tPlanar   = 0
                     , tDihedral = 0
                     , tBondLen  = 0
                     , tAtName   = cAtName  cart
                     , tResName  = cResName cart
                     , tResId    = cResId   cart }

-- | Creates protein backbone from residue name, identifier and torsion angles.
--   Also accepts an optional argument for next residue in chain.
proteinBackboneT resName resId psi phi omega tail = Node n [
                                                      Node ca [
                                                        Node c tail,
                                                        Node o []
                                                              ]
                                                           ]
  where
    at planar dihe bondLen name = Torsion { tPlanar   = planar
                                          , tDihedral = dihe
                                          , tBondLen  = bondLen
                                          , tAtName   = name
                                          , tResName  = resName
                                          , tResId    = resId   }
    -- TODO: check that angles are not shifted
    -- 1.45:1.52:1.32
    c  = at   111.2  phi   1.45 "C"
    ca = at   116.2  psi   1.52 "CA"
    n  = at   122.7  omega 1.32 "N"
    o  = at (-120.5) omega 1.24 "N" -- TODO: check that O is indeed on the same plane as N (so it shares dihedral angle)

