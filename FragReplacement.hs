-- | Functions changing fragments of topology, including picking random
-- fragment from database.
module FragReplacement( randomV
                      , randomV'
                      , randomVIO
                      , randomF
                      , randomReplace
                      , applyFragment
                      , replaceFragment
                      , changeAt
                      , replaceBackboneDihedrals
                      , metropolisCriterion
                      , checkMetropolisCriterionR
                      , checkMetropolisCriterion
                      ) where

import System.Environment
import System.Random (randomR, getStdRandom, RandomGen)
import System.Exit   (exitFailure, exitSuccess)
import Control.Monad (when)
import System.IO     (hPutStrLn, stderr)

import qualified Data.Vector           as V
import qualified Data.ByteString.Char8 as BS

import qualified Rosetta.Fragments     as F
import qualified Rosetta.Silent        as S
import Topo
import Util.Timing

-- | Draw a random element of a vector.
randomV v gen = (e, gen')
  where
    ((i, e), gen') = randomV' v gen

-- | Take random element of the vector, and position from which it was drawn.
randomV' :: RandomGen r => V.Vector a -> r -> ((Int, a), r)
randomV' v gen = ((i, v V.! i), gen')
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
randomReplace fragset topo gen = topo' `seq` (topo', gen')
  where
    Just topo' = replaceFragment pos frag topo
    ((pos, frag), gen') = randomF fragset gen

-- | Replaces a fragment at a given position in the topology.
replaceFragment :: Int -> F.RFrag -> Tree Torsion -> Maybe (Tree Torsion)
replaceFragment pos frag topo = topo `changeAt` (\t -> tResId t == pos) $ applyFragment $ F.res frag

-- | Changes a topology at a first backbone position given by a predicate (if such a position is found.)
changeAt :: Tree a -> (a -> Bool) -> (Tree a -> Tree a) -> Maybe (Tree a)
changeAt t pred mod = changeAt' t pred mod Just Nothing

-- | Helper function to change topology at first backbone position satisfying
-- predicate. Last two arguments are continuation for the successful change,
-- and continuation when nothing is found.
changeAt' :: Tree a-> (a -> Bool) -> (Tree a -> Tree a) -> (Tree a -> t) -> t -> t
changeAt' t@(Node a forest) pred mod cont notFoundCont | pred a = cont $ mod t 
changeAt' t@(Node a []    ) pred mod cont notFoundCont          = notFoundCont
changeAt' t@(Node a forest) pred mod cont notFoundCont          = changeAt' (last forest) pred mod cont' notFoundCont
 where
   cont' e = cont $ Node a $ replaceLastElement forest e

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
mapLastElement f []     = error "mapLastElement called on an empty list!"
mapLastElement f [c]    = [f c]
mapLastElement f (c:cs) = c:mapLastElement f cs

-- | Replaces torsion angles along backbone with those given by a list,
-- until the list ends.
replaceBackboneDihedrals ::  [Double] -> TorsionTopo -> TorsionTopo
replaceBackboneDihedrals []     t               = t
replaceBackboneDihedrals (d:ds) (Node c forest) = Node (c { tDihedral = d })
                                                       $ mapLastElement (replaceBackboneDihedrals ds) forest
-- | Minimum starting temperature for annealing.
min_starting_temp = 1.0

-- | Returns a probability of keeping a new model, and discarding old
-- model in Metropolis Monte-Carlo sampling.
metropolisCriterion temp oldScore newScore = if diff < 0
                                               then 1.0
                                               else exp (-(min (diff/temp) maxScore))
  where
    diff     = newScore - oldScore
    maxScore = 100

-- Checks Metropolis criterion, if given parameters, and a random number generator.
checkMetropolisCriterionR temp old new gen = (r < metropolisCriterion temp old new, gen')
  where
    (r, gen') = randomR (0.0, 1.0) gen

-- Checks Metropolis
checkMetropolisCriterion temp old new gen = getStdRandom $ checkMetropolisCriterionR temp old new

