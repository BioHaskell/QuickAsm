{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Topo( Tree          (..)
           , Torsion       (..)
           , Cartesian     (..)
           , TorsionTopo   (..)
           , CartesianTopo (..)

           , cartesianId
           , torsionId

           , xferC
           , xferT

           , tOXT
           , hasOXT

           , tShowRes

           , proteinBackboneT
           , constructBackbone
           , constructCartesianBackbone

           , computePositions
           , computeTorsions
           , showCartesian
           , showCartesianTopo
           , showTorsionTopoAsPDB

           -- Backbone inspection
           , backboneDihedrals
           , backbonePlanars
           , backbone
           , topo2sequence

           -- conversions from ROSETTA input
           , silentModel2TorsionTopo
           , torsionTopo2SilentModel 
           
           -- atom and residue renumbering
           , renumberResiduesT
           , renumberResiduesC

           , renumberAtomsT
           , renumberAtomsC

           , lastResidueId
           ) where

import System.IO

import Prelude hiding (mapM)
import Control.DeepSeq
import Data.Tree
import Data.Tree.Util
import Data.Traversable(mapM)
import Data.List(intercalate, group)
import Control.Monad(when)

import Control.Monad.State(State, get, put, modify, evalState, void)
import qualified Data.ByteString.Char8 as BS
import Data.Vector.V3
import Data.Vector.Class

import Rosetta.Silent

import Util.Fasta(fastacode2resname, resname2fastacode)
import Util.Angle
import Util.Geom
import Util.MyPrintf

import IdealGeomParams(idealGeom)


-- | Topology node record with torsion angles.
data Torsion = Torsion { tPlanar, tDihedral :: !Double
                       , tBondLen           :: !Double
                       , tAtName            :: !String
                       , tResName           :: !String
                       , tResId             :: !Int
                       , tAtId              :: !Int    }
  deriving(Show)

instance NFData Torsion   where

instance NFData Cartesian where

-- | Topology node record with cartesian coordinates.
data Cartesian = Cartesian { cPos     :: !Vector3
                           , cAtName  :: !String
                           , cResName :: !String
                           , cAtId    :: !Int
                           , cResId   :: !Int
                           }

-- * Extracting an atom identifier that should be unique within a topology.
-- | Extracts atom identifiers from Cartesian record.
cartesianId :: Cartesian -> (Int, Int, String, String)
cartesianId c = (cAtId c, cResId c, cAtName c, cResName c)

-- | Extracts atom identifiers from Torsion record.
torsionId   :: Torsion   -> (Int, Int, String, String)
torsionId   t = (tAtId t, tResId t, tAtName t, tResName t)

-- | Molecule topology in torsion angles.
type TorsionTopo   = Tree Torsion

-- | Molecule topology in cartesian coordinates.
type CartesianTopo = Tree Cartesian

-- | Ordinal number of the last residue in the Topology.
lastResidueId ::  Tree Torsion -> Int
lastResidueId = tResId . last . backbone

-- * Visualization
instance Show Cartesian where
  show = showCartesian

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

-- | Shows Torsion topology as PDB `ATOM` coordinate records (omitting angles.)
showTorsionTopoAsPDB :: TorsionTopo -> String
showTorsionTopoAsPDB = showCartesianTopo . computePositions

-- | Prints Cartesian as PDB ATOM record to a given output file.
printPDBAtom :: Handle -> Cartesian -> IO ()
printPDBAtom outh (Cartesian { cPos     = position
                             , cAtName  = atName
                             , cResName = resName
                             , cResId   = resNum
                             , cAtId    = atNum    }) =
   void $ hPrintf outh
             "ATOM  %5d%3s   %3s A%4d     %7.3f %7.3f %7.3f  1.00  0.00\n"
                atNum atName resName resNum (v3x position)
                                            (v3y position)
                                            (v3z position)

-- | Shows residue name and number in a conventional way.
tShowRes ::  Torsion -> String
tShowRes tors = tResName tors ++ show (tResId tors)

-- | Computes aminoacid sequence in a topology. Result is given as string
-- of FASTA codes.
topo2sequence ::  Tree Torsion -> String
topo2sequence = map (resname2fastacode . tResName) . filter isCAlpha . backbone
  where
    isCAlpha rec = tAtName rec == "CA" 

-- * Backbone construction from residue sequence and coordinates.
-- | Creates protein backbone from residue name, identifier and torsion angles.
--   Also accepts an optional argument for next residue in chain.
proteinBackboneT :: String -> Int -> -- residue name and number
                      Double -> Double -> Double -> Double -> -- dihedral angles
                      (Double -> [Tree Torsion]) -> -- sidechain generator (optional)
                      [Tree Torsion] -> Tree Torsion
proteinBackboneT resName resId psiPrev omegaPrev phi psi sc tail =
    Node n [
      Node ca [ -- TODO: add sidechain
        Node c (Node o [] : tail) -- TODO: should O connection have 0 degree dihedral, and reflect bond topology?
      ]
    ]
  where
    -- TODO: check that angles are not shifted
    -- 1.45:1.52:1.32
    [n, ca, c, o] = zipWith (atWithDihe resName resId)
                            ["N", "CA", "C", "O"]
                            [psiPrev, omegaPrev, phi, psi] -- TODO: screwed, omega places NEXT "N" atom!!!
-- TODO: add sidechains from: http://www.msg.ucsf.edu/local/programs/garlic/commands/dihedrals.html

-- | Creates single residue protein backbone from residue name, identifier,
-- and cartesian coordinates of atoms.
-- Also accepts optional argument for next residue in chain, and sidechain generator (from coordinate list.)
proteinBackboneC :: String-> Int-> [Vector3]-> ([Vector3] -> [CartesianTopo])-> [CartesianTopo]-> CartesianTopo
proteinBackboneC resName resId coords scGen tail =
    Node n [
      Node ca ( -- TODO: add sidechain
               Node c (tail ++ [Node o []]) -- TODO: should O connection have 0 degree dihedral, and reflect bond topology?
               : scGen otherCoords) -- TODO: check that scGen eaten all coords?
    ]
  where
    (bbCoords, otherCoords) = splitAt 4 coords
    [n, ca, c, o] = zipWith (atWithCoord resName resId)
                            ["N", "CA", "C", "O"]
                            (take 4 coords)-- TODO: screwed, omega places NEXT "N" atom!!!

-- TODO: OXT at the end of Cartesian backbone
-- | Constructs a Cartesian backbone, from a given string of residues names,
-- and coordinate lists in an order in which topology is supposed to create them.
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
-- | Makes a template `Torsion` record for a given atom name.
mkAt atName = at angle bondLen atName
  where
    (angle, bondLen) = idealGeom atName

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
constructBackbone recs = head $ constructBackbone' recs [cterminus]
  where
    cterminus = tOXT terResName (length recs) terOmega
    (terResName, _, _, terOmega) = last recs

-- | Adds OXT at the end.
tOXT resName resId omega = Node (atWithDihe resName resId "OXT" omega) []

-- | Checks that a topology has OXT at the end.
hasOXT :: TorsionTopo -> Bool
hasOXT = (\t -> tAtName t == "OXT") . last . backbone

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

-- * Conversion between Torsion and Cartesian atom.
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

-- * Conversion between Torsion angle and Cartesian coordinate topology.
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
    ez = vnormalise   curDir -- normalization unnecessary?
    dihe = degree2radian $ tDihedral torsion -- due to reversed directionality of ey
    ang  = degree2radian $ tPlanar   torsion
    --nextDir  = vnormalise $ ez |* (-cos ang) + (ey |* (-cos dihe) + ex |* (sin dihe))
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
    initialVectors = (Vector3 0 1 0, Vector3 1 0 0, bPos)

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
    a             = rootLabel topo
    b             = rootLabel . last . subForest $ topo
    bv            = cPos b - cPos a
    initialInputs = (-bv, bv, cPos b) -- something will be wrong for first two...

-- | Take a list of atom records, and Cartesian topology of a chain.
-- TODO: IMPLEMENT IT!!!
reconstructTopology = undefined

-- | Residue descriptor for TorsionTopo
tDescriptor tors = (tResName tors, tResId tors)


-- * Backbone inspection
-- | Returns a list of topology nodes along topology backbone
-- (which contains only first elements of forest list.)
backbone ::  Tree t -> [t]
backbone (Node a []) = [a]
backbone (Node a bb) = a:backbone (last bb)

-- | Returns a list of planar angles along the topology backbone.
backbonePlanars :: TorsionTopo -> [Double]
backbonePlanars   = tail . map tPlanar   . backbone

-- | Returns a list of dihedral angles along the topology backbone.
backboneDihedrals :: TorsionTopo -> [Double]
backboneDihedrals = tail . map tDihedral . backbone

-- * Residue and atom renumbering
-- | Renumbers residues from 1 within topology with a given "setter" and "getter" for residue id.
renumberResiduesWith :: (a -> Int -> a) -> (a -> Int) -> Int -> Tree a -> Tree a
renumberResiduesWith setter getter initial t = evalState (mapM renum t) (initial, getter $ rootLabel t)
  where
    renum a = do (i, prevNum) <- get
                 when (newNum /= prevNum) $ put (i+1, newNum)
                 (j, _) <- get
                 return $ a `setter` j
      where
        newNum = getter a

-- | Renumbers residues from a given number within Cartesian topology.
renumberResiduesC :: Int -> CartesianTopo -> CartesianTopo
renumberResiduesC = renumberResiduesWith (\a i -> a { cResId = i}) cResId

-- | Renumbers residues from a given number within Torsion   topology.
renumberResiduesT :: Int -> TorsionTopo -> TorsionTopo
renumberResiduesT = renumberResiduesWith (\a i -> a { tResId = i}) tResId

-- | Numbers atoms starting from 1 within topology with a given "setter".
renumberAtomsWith :: (a -> Int -> a) -> Int -> Tree a -> Tree a
renumberAtomsWith setter initial t = evalState (mapM renum t) initial
  where
    renum a = do i <- get
                 modify (+1)
                 return $ a `setter` i

-- | Renumbers Cartesian atoms within `CartesianTopo` starting from a given number.
renumberAtomsC :: Int -> CartesianTopo -> CartesianTopo
renumberAtomsC = renumberAtomsWith (\a i -> a { cAtId = i })

-- | Renumbers Torsion atoms within `TorsionTopo` starting from from a given number.
renumberAtomsT :: Int -> TorsionTopo -> TorsionTopo
renumberAtomsT = renumberAtomsWith (\a i -> a { tAtId = i })

--_test = "ATOM      1  N   VAL A   1       0.000   0.000   0.000  1.00  0.00              "

-- * Convertion from and to `SilentModel`.
-- | Converts a ROSETTA `SilentModel` to a Torsion topology.
silentModel2TorsionTopo ::  SilentModel -> TorsionTopo
silentModel2TorsionTopo = renumberAtomsT 1 . constructBackbone . prepare
  where
    prepare mdl = zipWith extractInput
                    (BS.unpack $ fastaSeq mdl)
                    (residues mdl)
    extractInput code silentRec = ( fastacode2resname code
                                  , phi   silentRec
                                  , psi   silentRec
                                  , omega silentRec        )


torsionTopo2residueDescriptors = map head . group . map tDescriptor . backbone
{-
  where
    bbAts = backbone topo
    descriptors _    []     = []
    descriptors desc (d:ds) = if desc' == desc
                                then       descriptors desc ds
                                else desc':descriptors desc' 
      where
        desc' = descriptor d
-}

-- | Converts a `TorsionTopo` to a `SilentModel` with dihedrals.
torsionTopo2SilentModel topo = SilentModel { fastaSeq          = BS.pack resSeq
                                           , residues          = residueRecords
                                           , name              = "<unnamed topology>"
                                           , otherDescriptions = []
                                           , scores            = []
                                           }
  where
    residueRecords     = zipWith makeSilentRec dihes $ map snd resDesc
    resDesc            = torsionTopo2residueDescriptors topo 
    dihes              = triples $ tail $ backboneDihedrals topo -- last omega angle is absent
    triples (a:b:c:cs) =  (a, b, c):triples cs
    triples [a, b]     = [(a, 0, 0)] -- last, skip O angle
    triples oh         = error $ "Inexhaustive match in triples: " ++ show oh
    makeSilentRec (phi, psi, omega) resId = emptySilentRec { phi     = phi
                                                           , psi     = psi
                                                           , omega   = omega
                                                           , resId   = resId
                                                           }
    resSeq = map (resname2fastacode . fst) resDesc
-- TODO: fill in caX, caY, caZ by computing Cartesian topology
-- TODO: preserve SS?

