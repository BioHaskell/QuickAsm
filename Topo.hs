{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Topo( Tree     (..)
           , Torsion  (..)
           , Cartesian(..)
           , xferC
           , xferT
           , proteinBackboneT
           , constructBackbone

           , computePositions
           , computeTorsions
           , showCartesian
           , showCartesianTopo

           , backboneDihedrals
           , backbonePlanars
           , backbone
           ) where

import System.IO

import Data.Vector.V3
import Data.Vector.Class
import Data.Tree
import Data.Tree.Util
import Data.List(intercalate)

import Angle
import Geom

import MyPrintf

data Torsion = Torsion { tPlanar, tDihedral :: !Double
                       , tBondLen           :: !Double
                       , tAtName            :: !String
                       , tResName           :: !String
                       , tResId             :: !Int
                       , tAtId              :: !Int    }
  deriving(Show)

data Cartesian = Cartesian { cPos     :: !Vector3
                           , cAtName  :: !String
                           , cResName :: !String
                           , cAtId    :: !Int
                           , cResId   :: !Int     }

showCartesian :: Cartesian -> String
showCartesian (Cartesian { cPos     = Vector3 x y z
                         , cAtName  = atName
                         , cResName = resName
                         , cAtId    = atId
                         , cResId   = resId         }) = s
    where
      Just s = printf "ATOM  %5d  %-3s%4s A %3d    %8.3f%8.3f%8.3f  1.00  0.00" atId atName resName resId x y z

showCartesianTopo :: CartesianTopo -> String
showCartesianTopo = intercalate "\n" . map showCartesian . Data.Tree.flatten

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
    -- TODO: check that angles are not shifted
    -- 1.45:1.52:1.32
    [n, ca, c, o] = zipWith (atWithDihe resName resId) ["N", "CA", "C", "O"] [omega, psi, phi, omega] -- TODO: screwed, omega places NEXT "N" atom!!!
-- TODO: add sidechains from: http://www.msg.ucsf.edu/local/programs/garlic/commands/dihedrals.html

{-# INLINE mkAt #-}
atWithDihe resName resId name dihe = (mkAt name resName resId) { tDihedral = dihe }
mkAt "N"  = at   116.2  1.32  "N"
mkAt "CA" = at   111.2  1.458 "CA"
mkAt "C"  = at   122.7  1.52  "C"
mkAt "O"  = at (-120.5) 1.24  "N"  -- TODO: check that O is indeed on the same plane as N (so it shares dihedral angle.)

-- Constructs an atom with given parameters as Torsion element.
at planar bondLen name resName resId = Torsion { tPlanar   = planar
                                               , tDihedral = 0.0 -- to be filled by another function
                                               , tBondLen  = bondLen
                                               , tAtName   = name
                                               , tAtId     = 0   -- to be filled by another method
                                               , tResName  = resName
                                               , tResId    = resId   }

onlyProteinBackboneT :: String -> Int -> Double -> Double -> Double ->
                          [Tree Torsion] -> Tree Torsion
onlyProteinBackboneT resName resId psi phi omega tail = proteinBackboneT resName resId psi phi omega (const []) tail

constructBackbone :: [(String, Double, Double, Double)] -> Tree Torsion
constructBackbone recs = head $ constructBackbone' recs []
-- TODO: now we are using single-letter aminoacid codes -> convert to PDB codes

constructBackbone' :: [(String, Double, Double, Double)] ->
                        [Tree Torsion] ->
                        [Tree Torsion]
constructBackbone' recs = foldr1 (.) $ zipWith buildResidue recs [1..]
  where
    -- TODO: convert resName
    buildResidue (resName, psi, phi, omega) resId tail = [onlyProteinBackboneT resName resId psi phi omega tail]

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

-- | Takes two most recent consecutive bond vectors, and current position
--   as a tuple, and a `Torsion` record to produce Cartesian position.
computeNextCartesian :: (Vector3, Vector3, Double, Vector3) ->
                           Torsion ->
                           ((Vector3, Vector3, Double, Vector3), Cartesian)
computeNextCartesian (prevDir, curDir, prevDihe, curPos) torsion =
    ((curDir, nextDir, tDihedral torsion, nextPos), cart)
  where
    nextPos = curPos + tBondLen torsion *| nextDir
    ex = vnormalise $ ey `vcross` ez
    ey = vnormalise $ prevDir `vperpend` ez
    ez = vnormalise $ curDir -- normalization unnecessary?
    dihe = degree2radian $ prevDihe -- due to reversed directionality of ey
    ang  = degree2radian $ tPlanar   torsion
    nextDir  = vnormalise $ ez |* (-cos ang) + sin ang *| (ey |* (-cos dihe) + ex |* sin dihe)
    cart     = (xferC torsion) { cPos = nextPos }  
    
-- | Converts a topology in `Torsion` angles to topology in `Cartesian` coordinates.
computePositions :: TorsionTopo -> CartesianTopo
computePositions (Node a [Node b tail]) = Node newA [Node newB $ map subforest tail]
  where
    subforest = descending computeNextCartesian initialVectors
    newA = (xferC a) { cPos = aPos } 
    newB = (xferC b) { cPos = bPos } 
    aPos = Vector3 0 0 0
    bPos = Vector3 1 0 0 |* tBondLen b
    initialVectors = (Vector3 1 0 0, Vector3 0 0 1, 0.0, Vector3 0 0 0)

computeNextTopo (bv1, bv2, lastPos) cartesian =
    ((bv2, bv3, cPos cartesian), tors)
  where
    bv3 = cPos cartesian - lastPos
    tors = (xferT cartesian) { tPlanar   = bv2 `vangle` (-bv3)
                             , tDihedral = vdihedral bv1 bv2 bv3
                             , tBondLen  = vmag bv3
                             } -- TODO: add angles

-- | Compute torsion angles from a Cartesian topology.
computeTorsions :: CartesianTopo -> TorsionTopo
computeTorsions topo = descending computeNextTopo initialInputs topo
  where
    (a:b:_)       = Data.Tree.flatten topo
    bv            = cPos a - cPos b
    initialInputs = (bv, bv, cPos a) -- something will be wrong for first two...

-- | Take a list of atom records, and Cartesian topology of a chain.
reconstructTopology = undefined

backbone (Node a []    ) = [a]
backbone (Node a (bb:_)) = a:backbone bb

backbonePlanars   = tail . map tPlanar   . backbone

backboneDihedrals = tail . map tDihedral . backbone

-- TODO: move unit tests to this module
-- TODO: add silent2PDB script

_test = "ATOM      1  N   VAL A   1       0.000   0.000   0.000  1.00  0.00              "

