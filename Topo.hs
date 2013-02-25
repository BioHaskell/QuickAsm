{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Topo( Tree     (..)
           , Torsion  (..)
           , Cartesian(..)
           , xferC
           , xferT
           , proteinBackboneT
           , constructBackbone
           , constructCartesianBackbone

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

-- | Topology node record with torsion angles.
data Torsion = Torsion { tPlanar, tDihedral :: !Double
                       , tBondLen           :: !Double
                       , tAtName            :: !String
                       , tResName           :: !String
                       , tResId             :: !Int
                       , tAtId              :: !Int    }
  deriving(Show)

-- | Topology node record with cartesian coordinates.
data Cartesian = Cartesian { cPos     :: !Vector3
                           , cAtName  :: !String
                           , cResName :: !String
                           , cAtId    :: !Int
                           , cResId   :: !Int     }

-- | Shows Cartesian record as PDB `ATOM` coordinate record.
showCartesian :: Cartesian -> String
showCartesian (Cartesian { cPos     = Vector3 x y z
                         , cAtName  = atName
                         , cResName = resName
                         , cAtId    = atId
                         , cResId   = resId         }) = s
    where
      Just s = printf "ATOM  %5d  %-3s%4s A %3d    %8.3f%8.3f%8.3f  1.00  0.00" atId atName resName resId x y z

-- | Shows Cartesian topology as PDB `ATOM` coordinate records.
showCartesianTopo :: CartesianTopo -> String
showCartesianTopo = intercalate "\n" . map showCartesian . Data.Tree.flatten

-- | Molecule topology in torsion angles.
type TorsionTopo   = Tree Torsion

-- | Molecule topology in cartesian coordinates.
type CartesianTopo = Tree Cartesian

-- | Transfer common attributes to Cartesian record from Torsion record.
xferC ::  Torsion -> Cartesian
xferC tors = Cartesian { cPos     = 0
                       , cAtName  = tAtName  tors
                       , cResName = tResName tors
                       , cAtId    = tAtId    tors
                       , cResId   = tResId   tors }

-- | Transfer common attributes to Torsion record from Cartesian record.
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
proteinBackboneT :: String -> Int -> -- residue name and number
                      Double -> Double -> Double -> Double -> -- dihedral angles
                      (Double -> [Tree Torsion]) -> -- sidechain generator (optional)
                      [Tree Torsion] -> Tree Torsion
proteinBackboneT resName resId psiPrev omegaPrev phi psi sc tail =
    Node n [
      Node ca [ -- TODO: add sidechain
        Node c $ tail ++ [Node o []] -- TODO: should O connection have 0 degree dihedral, and reflect bond topology?
      ]
    ]
  where
    -- TODO: check that angles are not shifted
    -- 1.45:1.52:1.32
    [n, ca, c, o] = zipWith (atWithDihe resName resId) ["N", "CA", "C", "O"] [psiPrev, omegaPrev, phi, psi] -- TODO: screwed, omega places NEXT "N" atom!!!
-- TODO: add sidechains from: http://www.msg.ucsf.edu/local/programs/garlic/commands/dihedrals.html

proteinBackboneC resName resId coords scGen tail =
    Node n [
      Node ca ([ -- TODO: add sidechain
        Node c $ tail ++ [Node o []] -- TODO: should O connection have 0 degree dihedral, and reflect bond topology?
      ] ++ scGen otherCoords) -- TODO: check that scGen eaten all coords?
    ]
  where
    (bbCoords, otherCoords) = splitAt 4 coords
    [n, ca, c, o] = zipWith (atWithCoord resName resId)
                            ["N", "CA", "C", "O"]
                            (take 4 coords)-- TODO: screwed, omega places NEXT "N" atom!!!

constructCartesianBackbone :: [(String, [Vector3])] -> CartesianTopo
constructCartesianBackbone recs = head $ constructCartesianBackbone' recs []
  where
    constructCartesianBackbone' :: [(String, [Vector3])] -> [CartesianTopo] -> [CartesianTopo]
    constructCartesianBackbone' recs = foldr1 (.) $ zipWith buildResidue recs [1..]
    buildResidue (resName, coords) resId tail =
      [proteinBackboneC resName resId coords (\[] -> []) tail]

-- | Makes a Cartesian record with the given dihedral
atWithCoord resName resId atName coord = Cartesian { cPos     = coord
                                                   , cAtName  = atName
                                                   , cResName = resName
                                                   , cAtId    = 0 -- to be filled by another function
                                                   , cResId   = resId   }

-- | Makes a Torsion record with the given dihedral.
atWithDihe resName resId name dihe = (mkAt name resName resId) { tDihedral = dihe }

{-# INLINE mkAt #-}
-- | Make a Torsion "ATOM" record for protein backbone atom
--   with idealized planar angle, and bond length.
mkAt "N"  = at   116.2  1.32  "N"
mkAt "CA" = at   111.2  1.458 "CA"
mkAt "C"  = at   122.7  1.52  "C"
mkAt "O"  = at (-120.5) 1.24  "N"  -- TODO: check that O is indeed on the same plane as N (so it shares dihedral angle.)

-- | Constructs an atom with given parameters as Torsion element.
at planar bondLen name resName resId = Torsion { tPlanar   = planar
                                               , tDihedral = 0.0 -- to be filled by another function
                                               , tBondLen  = bondLen
                                               , tAtName   = name
                                               , tAtId     = 0   -- to be filled by another method
                                               , tResName  = resName
                                               , tResId    = resId   }

-- | Takes arguments and constructs another node of protein backbone.
onlyProteinBackboneT :: String -> Int ->
                          Double -> Double -> Double -> Double ->
                          [Tree Torsion] -> Tree Torsion
onlyProteinBackboneT resName resId psiPrev omegaPrev phi psi tail = proteinBackboneT resName resId psiPrev omegaPrev phi psi (const []) tail

-- | Construct a protein backbone from sequence of residue codes and angles:
--   (residue name, phi, psi, omega).
constructBackbone :: [(String, Double, Double, Double)] -> TorsionTopo
constructBackbone recs = head $ constructBackbone' recs []
-- TODO: now we are using single-letter aminoacid codes -> convert to PDB codes

-- | Build another node of a protein backbone from sequence of residue
--   codes and angles, with a given continuation of the backbone as second
--   parameter.
constructBackbone' :: [(String, Double, Double, Double)] ->
                        [Tree Torsion] ->
                        [Tree Torsion]
constructBackbone' recs = foldr1 (.) $ zipWith3 buildResidue prevRecs recs [1..]
  where
    prevRecs = ("N-TER", 0.0, 0.0, 0.0):recs
    -- TODO: convert resName
    buildResidue (_,       phiPrev, psiPrev, omegaPrev)
                 (resName, phi,     psi,     omega    )
                 resId tail = [onlyProteinBackboneT resName resId psiPrev omegaPrev phi psi tail]

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
computeNextCartesian :: (Vector3, Vector3, Vector3) ->
                           Torsion ->
                           ((Vector3, Vector3, Vector3)
                           ,Cartesian)
computeNextCartesian (prevDir, curDir, curPos) torsion =
    ((curDir, nextDir, nextPos), cart)
  where
    nextPos = curPos + tBondLen torsion *| nextDir
    ex = vnormalise $ ey `vcross` ez
    ey = vnormalise $ prevDir `vperpend` ez
    ez = vnormalise $ curDir -- normalization unnecessary?
    dihe = degree2radian $ tDihedral torsion -- due to reversed directionality of ey
    ang  = degree2radian $ tPlanar   torsion
    nextDir  = vnormalise $ ez |* (-cos ang) + sin ang *| (ey |* (-cos dihe) + ex |* (sin dihe))
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
    initialVectors = (Vector3 0 (-1) 0, Vector3 1 0 0, bPos)

-- | Given two previous bond vectors, and last previous atom position,
--   converts Cartesian atom into Torsion atom record.
computeNextTorsion (bv1, bv2, lastPos) cartesian =
    ((bv2, bv3, cPos cartesian), tors)
  where
    bv3 = cPos cartesian - lastPos
    tors = (xferT cartesian) { tPlanar   = bv2 `vangle` (-bv3)
                             , tDihedral = vdihedral bv1 bv2 bv3
                             , tBondLen  = vmag bv3
                             } -- TODO: add angles

-- | Compute torsion angles from a Cartesian topology.
computeTorsions :: CartesianTopo -> TorsionTopo
computeTorsions topo = descending computeNextTorsion initialInputs topo
  where
    (a:b:_)       = Data.Tree.flatten topo
    bv            = cPos b - cPos a
    initialInputs = (-bv, bv, cPos b) -- something will be wrong for first two...

-- | Take a list of atom records, and Cartesian topology of a chain.
reconstructTopology = undefined

-- | Returns a list of topology nodes along topology backbone
--   (which contains only first elements of forest list.)
backbone (Node a []    ) = [a]
backbone (Node a (bb:_)) = a:backbone bb

-- | Returns a list of planar angles along the topology backbone.
backbonePlanars   = tail . map tPlanar   . backbone

-- | Returns a list of dihedral angles along the topology backbone.
backboneDihedrals = tail . map tDihedral . backbone

-- TODO: move unit tests to this module
-- TODO: add silent2PDB script

_test = "ATOM      1  N   VAL A   1       0.000   0.000   0.000  1.00  0.00              "

