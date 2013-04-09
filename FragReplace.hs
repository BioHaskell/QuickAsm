{-# LANGUAGE ScopedTypeVariables #-}
-- | Functions changing fragments of topology, including picking random
-- fragment from database.
module FragReplace( randomV
                  , randomV'
                  , randomVIO
                  , randomF
                  , randomReplace
                  , applyFragment
                  , replaceFragment

                  , changeTopoAt
                  , splitTopoAt
                  , cutTopoAt
                  , cTerminus

                  , replaceBackboneDihedrals
                  , metropolisCriterion
                  , checkCriterionR
                  , checkCriterionIO
                  --, checkMetropolisCriterionR
                  , checkMetropolisCriterion
                  , validFragments
                  , checkFragments
                  , showFragSetRange
                  , showTopoResidueRange
                  ) where

import System.Environment
import System.Random (randomR, getStdRandom, RandomGen)
import System.Exit   (exitFailure, exitSuccess)
import Control.Monad (when)
import Control.Arrow ((***), (&&&))
import System.IO     (hPutStrLn, stderr)
import Data.Maybe    (fromMaybe)
import qualified Data.List as L(group, sort, intercalate, nub) -- DEBUG: nub

import qualified Data.Vector           as V
import qualified Data.ByteString.Char8 as BS

import qualified Rosetta.Fragments     as F
import qualified Rosetta.Silent        as S
import Topo
import Util.Timing
import Util.Assert(errorWithStack) -- DEBUG
import Debug.Trace(traceShow)      -- DEBUG

-- | Draw a random element of a vector.
randomV v gen = (e, gen')
  where
    ((i, e), gen') = randomV' v gen

-- | Take random element of the vector, and position from which it was drawn.
randomV' :: RandomGen r => V.Vector a -> r -> ((Int, a), r)
randomV' v gen = ((i+1, v V.! i), gen')
  where
    (i, gen') = randomR (0 :: Int, V.length v-1) gen

-- | Random element of a vector, using standard random number generator in `IO` monad.
randomVIO ::  V.Vector a -> IO a
randomVIO v = getStdRandom $ randomV v

-- | Random position, and a fragment for this position.
randomF :: RandomGen t => F.RFragSet -> t -> ((Int, F.RFrag), t)
randomF fragset gen = ((pos, frag), gen'')
  where
    ((pos, site), gen' ) = randomV' (F.unRFragSet fragset) gen
    (frag,        gen'') = randomV  site                   gen'

-- | Perform a random fragment replacement on a TorsionTopo.
randomReplace :: (RandomGen r) => F.RFragSet -> TorsionTopo -> r -> (TorsionTopo, r)
randomReplace fragset topo gen = case replaceFragment pos frag topo of
                                   Just topo' -> topo' `seq` (topo', gen')
                                   Nothing    -> error $ concat [ "replaceFragment failed to find position "
                                                                , show pos, " for fragment "
                                                                , show frag ]
  where
    -- Just topo' = replaceFragment pos frag topo -- TODO: fix a bug that causes this failure!
    -- topo' = fromMaybe topo $ replaceFragment pos frag topo
    ((pos, frag), gen') = randomF fragset gen

-- | Replaces a fragment at a given position in the topology.
replaceFragment :: Int -> F.RFrag -> Tree Torsion -> Maybe (Tree Torsion)
replaceFragment pos frag topo = debuggingOff $ topo `changeTopoAt` (\t -> tResId t == pos) $ applyFragment $ F.res frag
  where
    -- DEBUG:
    debuggingOff = id
    debuggingOn  = traceShow ("replaceFragment", pos, frag, availablePositions topo) 
    availablePositions = map (tResId &&& tAtName) . backbone

-- | Changes a topology at a first backbone position given by a predicate (if such a position is found.)
changeTopoAt :: Tree a -> (a -> Bool) -> (Tree a -> Tree a) -> Maybe (Tree a)
changeTopoAt t pred mod = do (context, last, t) <- t `splitTopoAt` pred
                             return $ context $ mod t

splitTopoAt :: Tree a -> (a -> Bool) -> Maybe (Tree a -> Tree a, Maybe a, Tree a)
splitTopoAt topo pred = splitTopoAt' topo pred Nothing id

-- | Helper function to split topology at first backbone position satisfying predicate.
-- Last two arguments are continuation for the successful change, and continuation when nothing is found.
-- Values passed over to the success continuation are:
--  * effect of applying a given modification to the context,
--  * last atom of a context (for convenience),
--  * splitted topology tail.
-- Result is handed over to success continuation.
splitTopoAt' :: Tree a-> (a -> Bool)-> Maybe a-> (Tree a -> c)-> Maybe (Tree a -> c, Maybe a, Tree a)
splitTopoAt' t@(Node a forest) pred lastAt context | pred a = Just (context, lastAt, t)
splitTopoAt' t@(Node a []    ) pred lastAt context          = Nothing
splitTopoAt' t@(Node a forest) pred lastAt context          = splitTopoAt' (last forest) pred (Just a) context'
  where context' = context . Node a . replaceLastElement forest

-- | Cuts a Torsion topology at a given atom, replacing this atom with
-- C-terminal OXT.
cutTopoAt :: TorsionTopo -> (Torsion -> Bool) -> Maybe (TorsionTopo, TorsionTopo)
topo `cutTopoAt` pred = do (context, lastAt, tail) <- topo `splitTopoAt` pred
                           return $! ( context $ cTerminus lastAt tail
                                     , tail )

-- | Computes C-terminus, given a last backbone atom of a chain, and maybe
-- continuation of a backbone that could be used to copy omega angle.
lastAt `cTerminus` tail = tOXT resName resId omega
  where
    omega   = if tResName (rootLabel tail) `elem` ["N", "CA", "C"] -- still backbone
                then tDihedral $ rootLabel tail
                else 180
    resName = maybe "UNK" tResName  lastAt
    resId   = maybe 1     tResId    lastAt

-- | Replace last element of the list with a second argument.  
replaceLastElement ::  [t] -> t -> [t]
replaceLastElement l e = mapLastElement (const e) l

-- | Replaces dihedral angles along the backbone.
applyFragment :: V.Vector F.RFragRes -> TorsionTopo -> TorsionTopo
applyFragment vres t = assertions $ replaceBackboneDihedrals dihes t
  where
    assertions = id -- TODO: check sequence!
    dihes = concatMap getDihes $ V.toList vres
    getDihes (F.RFragRes { F.phi   = phi
                         , F.psi   = psi
                         , F.omega = omega }) = [phi, psi, omega]

-- | Maps a function over the last element of the list, and returns modified list.
mapLastElement ::  (t -> t) -> [t] -> [t]
--mapLastElement f []     = errorWithStack "mapLastElement called on an empty list!"
mapLastElement f []     = []
mapLastElement f [c]    = [f c]
mapLastElement f (c:cs) = c:mapLastElement f cs

-- | Replaces torsion angles along backbone with those given by a list,
-- until the list ends.
replaceBackboneDihedrals ::  [Double] -> TorsionTopo -> TorsionTopo
replaceBackboneDihedrals []     t               = t
replaceBackboneDihedrals (d:ds) (Node c forest) = Node (c { tDihedral = d })
                                                       $ mapLastElement (replaceBackboneDihedrals ds) forest
-- | Minimum starting temperature for annealing.
minStartingTemp = 1.0

-- | Returns a probability of keeping a new model, and discarding old
-- model in Metropolis Monte-Carlo sampling.
metropolisCriterion temp oldScore newScore = if diff < 0
                                               then 1.0
                                               else exp (-(min (diff/temp) maxScore))
  where
    diff     = newScore - oldScore
    maxScore = 100

-- | Returns a Bool value with a given probability for True
checkCriterionR p gen = (r <= p, gen')
  where
    (r, gen') = randomR (0.0, 1.0) gen

checkCriterionIO :: Double -> IO Bool
checkCriterionIO = getStdRandom . checkCriterionR

-- | Checks Metropolis criterion, if given parameters, and a random number generator.
checkMetropolisCriterionR :: (RandomGen t) => Double -> Double -> Double -> t -> (Bool, t)
checkMetropolisCriterionR temp old new = checkCriterionR $ metropolisCriterion temp old new

-- | Checks Metropolis criterion within IO monad.
checkMetropolisCriterion :: Double -> Double -> Double -> IO Bool
checkMetropolisCriterion temp old new = checkCriterionIO $ metropolisCriterion temp old new

-- | Validates that fragments in RFragSet refer to positions present in a
-- given Torsion topology, and returns valid subset, and a list of positions
-- in RFragSet that are invalid.
validFragments :: TorsionTopo -> F.RFragSet -> (F.RFragSet, [Int])
validFragments tTopo = (F.RFragSet *** (V.toList . V.map (whichNotInRange . pos . V.head))) . V.partition (inRangeT . pos . V.head) . F.unRFragSet
  where
    pos = F.startPos &&& F.endPos
    topoRange :: [Int] = map head $ L.group $ L.sort $ map tResId $ backbone tTopo
    topoMin         = minimum topoRange
    topoMax         = maximum topoRange
    inRangeT (a, b) = inRange a      && inRange b
    inRange  x      = (x >= topoMin) && (x <= topoMax)
    whichNotInRange (a, b) = if not (inRange a)
                               then a
                               else b

-- | Checks consistency between topology and fragment set. Returns subset
-- of fragments at valid positions, and print invalid positions to stderr.
checkFragments tTopo fragSet = do when (not $ null invalidPositions) $
                                    hPutStrLn stderr $ "Dropping fragments referring to positions absent in topology: " ++
                                                       L.intercalate ", " (map show invalidPositions)
                                  return fragSet'
  where
    (fragSet', invalidPositions) = validFragments tTopo fragSet


-- | Shows range of residue positions within fragment set.
showFragSetRange :: F.RFragSet -> String
showFragSetRange inFragSet = concat [ "Fragment set covers range between "
                                    , show start, " and ", show end        ]
  where
    takePositions :: (F.RFrag -> Int) -> [Int]
    takePositions proj = map head $ L.group $ L.sort $ V.toList $ V.map (proj . V.head) $ F.unRFragSet inFragSet
    start = minimum $ takePositions F.startPos
    end   = maximum $ takePositions F.endPos

-- | Shows range of residue numbers within a topology. (For debugging.)
showTopoResidueRange :: TorsionTopo -> String
showTopoResidueRange topo = concat ["Topology residues in range: ", show minRes, "-", show maxRes]
  where
    residues = map head $ L.group $ L.sort $ map tResId $ backbone topo
    minRes = minimum residues
    maxRes = maximum residues
