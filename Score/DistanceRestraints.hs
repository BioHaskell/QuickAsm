{-# LANGUAGE OverloadedStrings #-}
-- | Implements distance restraints.
module Score.DistanceRestraints where

import System.IO(hPutStrLn, stderr)
import qualified Data.ByteString.Char8 as BS
import Data.List(sortBy, find)
import Data.Tree(flatten)
import Data.Function(on)
import Data.Either(partitionEithers)
import Control.Monad(forM_)
import Control.DeepSeq(NFData(..), deepseq)
import Control.Exception(assert)
import qualified Data.Vector        as V
import qualified Data.Vector.V3     as V3
import qualified Data.Vector.Class  as V3
import qualified Data.IntMap        as IMap
import qualified Rosetta.Restraints as R
import Debug.Trace(traceShow)

import Model
import Topo
import Score.ScoringFunction(ScoringFunction, simpleScoringFunction)
import Modelling
import Util.Show(showFloat, showEmpty)

-- | Contains a restraint with a precomputed atom number
data Restraint = Restraint { source                 :: !R.Restraint
                           , leftAt,  rightAt
                           , leftRes, rightRes, num :: !Int
                           , rFunc                  :: !R.RestraintFunction
                           }
  deriving (Show)

instance NFData Restraint where

-- | Contains a list of restraints,
--   sorted by both first and second atom (thus two copies of the same set.)
data RestraintSet = RSet { byLeftAtom, byRightAtom, byNum :: [Restraint]
                         , size :: Int }
  deriving (Show)

instance NFData RestraintSet where
  rnf rset = rnf (byLeftAtom rset) `seq` rnf (byRightAtom rset) `seq` rnf (byNum rset) 

a `compareRestraintsByFirstAtom`  b = R.at1 a `compareAtoms` R.at2 b
a `compareRestraintsBySecondAtom` b = R.at1 a `compareAtoms` R.at2 b

{-
data AtomId = AtomId { resName :: BS.ByteString, -- may be empty!
                       atName  :: BS.ByteString,
                       resId   :: Int
                     }
-}
-- | Compare atoms in accord to the expected backbone topology
-- 1. By residue index
-- 2. By backbone order (N -> CA -> C -> next residue)
-- 3. For other atoms -> also by order in sidechain or just after previous atom etc.
-- Given that, we may easily sort restraints so that they can be checked in the order
-- in which they occur within the topology.
atA `compareAtoms` atB = case R.resId atA `compare` R.resId atB of
                           EQ   -> R.atName atA `atCmp` R.atName atB
                           ineq -> ineq

-- TODO: less fallible alternative would be to keep atom indices (as their assignment doesn't vary!)

-- | Compares atom names in accord with expected protein backbone topology (UNUSED.)
atCmp "N"   _     = LT
atCmp _    "N"    = GT
atCmp "CA" "C"    = LT
atCmp "C"  "CA"   = LT
atCmp "C"  "O"    = LT
atCmp "O"  _      = GT

atCmp "HA" "CA"   = GT
atCmp "CA" "HA"   = LT
atCmp "HA" "H"    = GT
atCmp "HA" _      = LT -- includes sidechain
atCmp _    "HA"   = GT

atCmp "H"  _      = LT
atCmp a     b   | a == b = EQ
atCmp a     b   = error $ "Still undefined atom comparison within topology: " ++ show a ++ " vs " ++ show b

-- | Matches two atom descriptions. Reports an error if atom names and
-- residue ids are the same, but residue names are not.
match :: (BS.ByteString, BS.ByteString, Int) -> (BS.ByteString, BS.ByteString, Int) -> Bool
match a@(res1, at1, resid1) b@(res2, at2, resid2) | (at1, resid1) /= (at2, resid2) = False
match a@(res1, at1, resid1) b@(res2, at2, resid2) | res1 == res2                   = True
match a@(res1, at1, resid1) b@(res2, at2, resid2) | res1 == "" || res2 == ""       = True
match a@(res1, at1, resid1) b@(res2, at2, resid2)                                  =
                                                  error $ "Mismatch in residue names: " ++ show a ++ " vs " ++ show b

-- | Tries to match Cartesian atom against
-- Rosetta.DistanceRestraints.Restraint atom id.
matchTopoAtId :: R.AtomId -> Cartesian -> Bool
matchTopoAtId atid cart = match (BS.pack $ cResName cart, BS.pack $ cAtName  cart, cResId  cart)
                                (R.resName          atid,           R.atName atid, R.resId atid)

-- | Reads restraints file, and returns a RestraintSet matching given initial model.
prepareRestraintsFile :: CartesianTopo   -- ^ initial model
                      -> FilePath        -- ^ distance restraints input filename
                      -> IO RestraintSet
prepareRestraintsFile mdl fname = do (rset, errs) <- flip precomputeOrder mdl `fmap` R.processRestraintsFile fname
                                     forM_ errs $ \msg -> hPutStrLn stderr $ "ERROR in restraints from " ++
                                                                             fname ++ ": " ++ msg
                                     rset `deepseq` return $! rset

-- | Precompute restraint order
-- Returns a correct `RestraintSet` and a list of error messages about incorrect `R.Restraint`s.
precomputeOrder :: [R.Restraint] -> CartesianTopo -> (RestraintSet, [String])
precomputeOrder restrs cartopo = ( RSet { byLeftAtom  = sortByKey leftAt  restraints
                                        , byRightAtom = sortByKey rightAt restraints
                                        , byNum       = sortByKey num     restraints
                                        , size        = length restraints
                                        }
                                 , errors )
  where
    restraints = zipWith (\i r -> r { num = i }) [0..] unnumberedRestraints
    (errors, unnumberedRestraints) = partitionEithers $ map makeRestraint restrs
    makeRestraint :: R.Restraint -> Either String Restraint
    makeRestraint rRestr = do left  <- findAt $ R.at1 rRestr
                              right <- findAt $ R.at2 rRestr
                              return Restraint { source   = rRestr
                                               , leftAt   = left
                                               , rightAt  = right
                                               , leftRes  = R.resId $ R.at1 rRestr
                                               , rightRes = R.resId $ R.at2 rRestr
                                               , rFunc    = R.func rRestr
                                               , num      = 0 -- to assign later with renumbering
                                               }
      where
        findAt :: R.AtomId -> Either String Int
        findAt at = case find (matchTopoAtId at) topoOrder of
                      Just cart -> Right $ cAtId cart
                      Nothing   -> Left $ "Cannot find atom: " ++ showAtom at
    topoOrder = flatten cartopo
    showAtom atId = concat [ BS.unpack $ R.resName atId
                           , show      $ R.resId   atId
                           , " "
                           , BS.unpack $ R.atName  atId ]

-- TODO: move to Util, add QuickCheck
-- | Sorts by a given key (and standard comparison.)
sortByKey k = sortBy (compare `on` k)

-- | Finds all atom positions, taking as argument a selector, restraint
-- list, and Cartesian topology.
findPositions :: (Restraint -> Int) -> [Restraint] -> Tree Cartesian -> [(Int, V3.Vector3)]
findPositions which restraints topo = findPositions' 0 which restraints $ flatten topo
  where
    findPositions' :: Int -> (Restraint -> Int) -> [Restraint] -> [Cartesian] -> [(Int, V3.Vector3)]
    findPositions' index which restrs@(r:rs) aList@(b:bs) | which r == index = (num r, cPos b):findPositions'  index    which rs     aList 
    findPositions' index which []            _                               = []
    findPositions' index which restrs@(r:rs) aList@(b:bs) | which r >  index =                 findPositions' (index+1) which restrs bs
    findPositions' index which restrs        []                              = error $ "Cannot find atoms for following restraints: " ++ show restrs

-- | Checks distance restraints, and returns a list of scoring function values.
checkDistanceRestraints' :: RestraintSet    -- ^ restraint set
                         -> CartesianTopo   -- ^ model
                         -> V.Vector Double
checkDistanceRestraints' rset carTopo = scores
  where
    empty  = V.replicate (size rset) 0 -- may also give just 0.0, since th
    finder findCriterion rsetProj  = empty V.// findPositions findCriterion (rsetProj rset) carTopo
    lefts  = finder leftAt  byLeftAtom
    rights = finder rightAt byRightAtom
    funcs  = V.fromList $ map rFunc $ byNum rset 
    score  pos1 pos2 = rScore $ V3.vmag $ pos2 - pos1
    rScore dist (R.RGaussian avg dev est) = (dif*dif)/(2*dev*dev) -- ignored log of exp part... -> different constant
      where -- NOTE: we ignore lower bounds!
        dif = max 0 $ dist - avg
    -- NOTE: in ROSETTA there is a separate equation for violation larger than stdev above hibound
    rScore dist (R.RBounded lo hi dev) = dif*dif/dev
      where
        --dif = maximum [lo - dist, dist - hi, 0] -- ignoring lower bound again!
        dif = max 0 $ dist - hi
    -- TODO: do with need max penalty?
    scores = V.zipWith3 score lefts rights funcs

-- | Show value of each restraint.
checkDistanceRestraints :: RestraintSet          -- ^ distance restraints
                        -> CartesianTopo         -- ^ input model
                        -> [(Restraint, Double)] -- ^ list of restraints and their current score values.
checkDistanceRestraints rset carTopo = filter isPositive $ zip (byNum rset) $ V.toList $ checkDistanceRestraints' rset carTopo
  where
    isPositive (_r, v) = v > 0.0

-- | Give a synthetic restraint score.
scoreDistanceRestraints :: RestraintSet  -- ^ restraints set
                        -> CartesianTopo -- ^ input model
                        -> Double        -- ^ total score
scoreDistanceRestraints rset carTopo = sqrt $ V.foldl' addSq 0.0 $ checkDistanceRestraints' rset carTopo
  where
    addSq acc d = acc + d * d

-- NOTE: due to necessity of renumbering, prepareRestraintSet is better way to make subrange query!
-- | Selects a subset of restraints between two given residue ordinal numbers.
subrange :: RestraintSet -- ^ restraint set
         -> (Int, Int)   -- ^ limits of residue numbers
         -> RestraintSet
subrange rset (from, to) = assertions
                             RSet { byLeftAtom  = map renum lefts
                                  , byRightAtom = map renum rights
                                  , byNum       = map renum toRenum
                                  , size        = size' -- we keep the same size, to keep valid vector!
                                  }
  where
    lefts  = filter restraintInRange $ byLeftAtom  rset
    rights = filter restraintInRange $ byRightAtom rset
    restraintInRange restraint = resInRange (leftRes  restraint) &&
                                 resInRange (rightRes restraint)
    resInRange i = (from <= i) && (i <= to)
    size'   = length lefts
    size''  = length rights
    size''' = length toRenum
    assertions = assert $ size'  == size''  &&
                          size'' == size'''
    toRenum = filter restraintInRange $ byNum rset
    renumDict = IMap.fromList $ zip (map num toRenum) [0..] 
    renum restr = restr { num = renumDict IMap.! num restr }

-- | Makes a distance restraint set into a ScoringFunction.
makeDistanceScore :: RestraintSet -> ScoringFunction
makeDistanceScore rset = simpleScoringFunction "cst" fun showFun
  where
    fun ::  (Monad m, Model a) => a -> m Double
    fun     = return . scoreDistanceRestraints rset . cartesianTopo
    showFun ::  (Monad m, Model a) => a -> m [BS.ByteString]
    showFun = return . showEmpty "All distances satisfied." .
                       map (\(a, b) -> BS.pack . shows (source a) . (' ':) $ showFloat b) .
                       checkDistanceRestraints rset . cartesianTopo

-- | Read distance restraints from file and return a ScoringFunction.
prepareDistanceScore :: CartesianTopo -> FilePath -> IO ScoringFunction
prepareDistanceScore topo fname = makeDistanceScore `fmap` prepareRestraintsFile topo fname

