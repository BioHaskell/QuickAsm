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

xferC ::  Torsion -> Cartesian
xferC tors = Cartesian { cPos     = 0
                       , cAtName  = tAtName  tors
                       , cResName = tResName tors
                       , cResId   = tResId   tors }

xferT ::  Cartesian -> Torsion
xferT cart = Torsion { tPlanar   = 0
                     , tDihedral = 0
                     , tBondLen  = 0
                     , tAtName   = cAtName  cart
                     , tResName  = cResName cart
                     , tResId    = cResId   cart }

-- | Creates protein backbone from residue name, identifier and torsion angles.
--   Also accepts an optional argument for next residue in chain.
proteinBackboneT :: String-> Int-> Double-> Double-> Double-> [Tree Torsion] -> [Tree Torsion]-> Tree Torsion
proteinBackboneT resName resId psi phi omega sc tail =
    Node n [
      Node ca [ -- TODO: add sidechain
        Node c $ tail ++ [Node o []] -- TODO: should O connection have 0 degree dihedral, and reflect bond topology?
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
    n  = at   116.2  omega 1.32 "N"
    c  = at   122.7  phi   1.45 "C"
    ca = at   111.2  psi   1.52 "CA"
    o  = at (-120.5) omega 1.24 "N"  -- TODO: check that O is indeed on the same plane as N (so it shares dihedral angle.)
-- TODO: add sidechains from: http://www.msg.ucsf.edu/local/programs/garlic/commands/dihedrals.html

onlyProteinBackboneT :: String-> Int-> Double-> Double-> Double-> [Tree Torsion]-> Tree Torsion
onlyProteinBackboneT resName resId psi phi omega tail = proteinBackboneT resName resId psi phi omega [] tail

constructBackbone :: [Char] -> [(Double, Double, Double)] -> Tree Torsion
constructBackbone seq dihedrals = head $ constructBackbone' seq dihedrals []
-- TODO: now we are using single-letter aminoacid codes -> convert to PDB codes

constructBackbone' :: [Char]-> [(Double, Double, Double)] -> [Tree Torsion] -> [Tree Torsion]
constructBackbone' seq dihedrals = foldr1 (.) $ zipWith3 buildResidue seq [1..] dihedrals
  where
    -- TODO: convert resName
    buildResidue resName resId (psi, phi, omega) tail = [onlyProteinBackboneT [resName] resId psi phi omega tail]

