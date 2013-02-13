module Topo( Tree(..)
           , Torsion(..), Cartesian(..)
           , xferC, xferT
           , proteinBackboneT
           , constructBackbone
           , computePositions
           ) where

import System.IO
import Text.Printf

import Data.Vector.V3
import Data.Vector.Class
import Data.Tree

import Angle
import Geom

-- | Descends a tree, inheriting accumulator
--   May be generalized into descend f a = fmap snd . inherit f a,
--   where inherit is top-down correspondent of scanl.
descendTree ::  (a -> b -> (a, c)) -> a -> Tree b -> Tree c
descendTree f acc (Node rec forest) = Node rec' $ map (descendTree f acc') $ forest
  where
    (acc', rec') = f acc rec

data Torsion = Torsion { tPlanar, tDihedral :: !Double
                       , tBondLen           :: !Double
                       , tAtName            :: !String
                       , tResName           :: !String
                       , tResId             :: !Int
                       , tAtId              :: !Int    }

data Cartesian = Cartesian { cPos     :: !Vector3
                           , cAtName  :: !String
                           , cResName :: !String
                           , cAtId    :: !Int
                           , cResId   :: !Int     }

type TorsionTopo   = Tree Torsion

type CartesianTopo = Tree Cartesian

xferC ::  Torsion -> Cartesian
xferC tors = Cartesian { cPos     = 0
                       , cAtName  = tAtName  tors
                       , cResName = tResName tors
                       , cAtId    = tAtId    tors
                       , cResId   = tResId   tors }

xferT ::  Cartesian -> Torsion
xferT cart = Torsion { tPlanar   = 0
                     , tDihedral = 0
                     , tBondLen  = 0
                     , tAtName   = cAtName  cart
                     , tAtId     = cAtId    cart
                     , tResName  = cResName cart
                     , tResId    = cResId   cart }

-- | Creates protein backbone from residue name, identifier and torsion angles.
--   Also accepts an optional argument for next residue in chain.
proteinBackboneT :: String-> Int-> Double-> Double-> Double-> (Double -> [Tree Torsion]) -> [Tree Torsion]-> Tree Torsion
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
                                          , tAtId     = 0 -- to be filled by another method
                                          , tResName  = resName
                                          , tResId    = resId   }
    -- TODO: check that angles are not shifted
    -- 1.45:1.52:1.32
    n  = at   116.2  omega 1.32 "N"
    ca = at   111.2  psi   1.45 "CA"
    c  = at   122.7  phi   1.52 "C"
    o  = at (-120.5) omega 1.24 "N"  -- TODO: check that O is indeed on the same plane as N (so it shares dihedral angle.)
-- TODO: add sidechains from: http://www.msg.ucsf.edu/local/programs/garlic/commands/dihedrals.html

onlyProteinBackboneT :: String -> Int -> Double -> Double -> Double -> [Tree Torsion] -> Tree Torsion
onlyProteinBackboneT resName resId psi phi omega tail = proteinBackboneT resName resId psi phi omega (const []) tail

constructBackbone :: [Char] -> [(Double, Double, Double)] -> Tree Torsion
constructBackbone seq dihedrals = head $ constructBackbone' seq dihedrals []
-- TODO: now we are using single-letter aminoacid codes -> convert to PDB codes

constructBackbone' :: [Char] -> [(Double, Double, Double)] -> [Tree Torsion] -> [Tree Torsion]
constructBackbone' seq dihedrals = foldr1 (.) $ zipWith3 buildResidue seq [1..] dihedrals
  where
    -- TODO: convert resName
    buildResidue resName resId (psi, phi, omega) tail = [onlyProteinBackboneT [resName] resId psi phi omega tail]

-- TODO: computing Cartesian chain from Torsion
-- TODO: printing Torsion as Silent
-- TODO: reading Torsion as Silent

-- TODO: Method that gives unique atom numbers

-- | Prints Cartesian as PDB ATOM record to a given output file.
printPDBAtom :: Handle -> Cartesian -> IO ()
printPDBAtom outh (Cartesian { cPos     = position
                             , cAtName  = atName
                             , cResName = resName
                             , cResId   = resNum
                             , cAtId    = atNum    }) =
   hPrintf outh
           "ATOM  %5d%3s   %3s A%4d     %7.3f %7.3f %7.3f  1.00  0.00\n"
              atNum atName resName resNum (v3x position)
                                          (v3y position)
                                          (v3z position) >> return ()

-- Compute positions from torsion angles
placeAtoms :: TorsionTopo -> CartesianTopo
placeAtoms = undefined -- placeAtoms' (Vector3 0 0 0) (Vector3 0 1 0) (Vector3 0 0 1)

-- Use:
-- Or rather para from:
-- http://hackage.haskell.org/packages/archive/pointless-haskell/latest/doc/html/Generics-Pointless-Combinators.html#t:Ann
-- http://hackage.haskell.org/packages/archive/uniplate/latest/doc/html/Data-Generics-Uniplate-Operations.html

computeNextCartesian :: (Vector3, Vector3, Vector3) -> Torsion ->
                           ((Vector3, Vector3, Vector3), Cartesian)
computeNextCartesian (prevDir, curDir, curPos) torsion =
    ((curDir, nextDir, nextPos), cart)
  where
    nextPos = curPos + tBondLen torsion *| nextDir
    ex = vnormalise $ ey `vcross` ez
    ey = vnormalise $ prevDir `vperpend` ez
    ez = vnormalise $ curDir -- normalization unnecessary?
    dihe = degree2radian $ tDihedral torsion - pi -- due to reversed directionality of ey
    ang  = degree2radian $ tPlanar   torsion
    nextDir  = vnormalise $ ez |* (-cos ang) +sin ang *| (ey |* cos dihe + ex |* sin dihe)
    cart     = xferC torsion 
    

computePositions :: TorsionTopo -> CartesianTopo
computePositions topo = descendTree computeNextCartesian initialVectors topo
  where
    initialVectors = (Vector3 0 1 0, Vector3 0 0 1, Vector3 0 0 0)

-- | Compute torsion angles from a Cartesian topology.
computeTorsions :: CartesianTopo -> TorsionTopo
computeTorsions = undefined

-- | Take a list of atom records, and Cartesian topology of a chain.
reconstructTopology = undefined

-- TODO: move unit tests to this module
-- TODO: add silent2PDB script

