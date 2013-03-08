module Main where

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
replaceFragment pos frag topo = topo `changeAt` (\t -> tResId t == pos) $ applyFragment $ F.res frag

-- | Changes a topology at a first backbone position given by a predicate (if such a position is found.)
changeAt :: Tree a -> (a -> Bool) -> (Tree a -> Tree a) -> Maybe (Tree a)
changeAt t pred mod = changeAt' t pred mod Just Nothing

-- | Helper function to change topology at first backbone position satisfying
-- predicate. Last two arguments are continuation for the successful change,
-- and continuation when nothing is found.
changeAt' t@(Node a forest) pred mod cont notFoundCont | pred a = cont $ mod t 
changeAt' t@(Node a []    ) pred mod cont notFoundCont          = notFoundCont
changeAt' t@(Node a forest) pred mod cont notFoundCont          = changeAt' (last forest) pred mod cont' notFoundCont
 where
   cont' e = cont $ Node a $ replaceLastElement forest e

-- | Replace last element of the list with a second argument.  
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
mapLastElement f []     = error "mapLastElement called on an empty list!"
mapLastElement f [c]    = [f c]
mapLastElement f (c:cs) = c:mapLastElement f cs

-- | Replaces torsion angles along backbone with those given by a list,
-- until the list ends.
replaceBackboneDihedrals :: [Double] -> TorsionTopo -> TorsionTopo
replaceBackboneDihedrals []     t               = t
replaceBackboneDihedrals (d:ds) (Node c forest) = Node (c { tDihedral = d })
                                                       $ mapLastElement (replaceBackboneDihedrals ds) forest

main = do args <- getArgs
          when (length args /= 4) $ do hPutStrLn stderr "FragSample <fragments_R3> <silentInput.out> <output.out> <output.pdb>"
                                       hPutStrLn stderr "NOTE: only first model from <silentInput.out> is taken."
                                       exitFailure
          let [ fragmentInputFilename, silentInputFilename, silentOutputFilename , pdbOutputFilename ] = args
          main' fragmentInputFilename silentInputFilename silentOutputFilename pdbOutputFilename
          exitSuccess

main' fragmentInputFilename silentInputFilename silentOutputFilename pdbOutputFilename = 
    do fragset <- time "Reading fragment set" $ F.processFragmentsFile fragmentInputFilename
       mdls    <- time "Reading silent file " $ S.processSilentFile    silentInputFilename
       mdl <- timePure "Converting silent model to topology" $ silentModel2TorsionTopo $ head mdls
       newMdl <- time "Replacing a random fragment" $ getStdRandom $ randomReplace fragset mdl
       -- TODO: implement torsionTopo2SilentModel
       smdl <- timePure "Computing silent model" $ torsionTopo2SilentModel newMdl
       time "Writing silent file" $ S.writeSilentFile silentOutputFilename [smdl]
       time "Writing PDB file"    $ writeFile pdbOutputFilename $ showCartesianTopo $ computePositions newMdl  
       

