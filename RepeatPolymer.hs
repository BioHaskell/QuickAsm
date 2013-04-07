{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Allows for making a repeat polymer.
module RepeatPolymer( Polymer      (..)
                    , makePolymer
                    , extractPolymer
                    , glueChain -- move to 
                    , instantiate
                    , PolymerModel (..)
                    , makePolymerModel
                    , delimitFragSet
                    , samplePolymer
                    , samplePolymerModel ) where

import System.Random(RandomGen)
import qualified Data.Vector as V
import Data.Maybe(fromMaybe, fromJust) -- DEBUG: fromMaybe
import Data.Ix(inRange)
import Control.Exception(assert)
import Control.DeepSeq(NFData(..))

import Debug.Trace(traceShow) -- DEBUG

import Rosetta.Fragments(RFragSet(..), RFrag(..))

import Topo
import FragReplace
import Model

-- * Polymer data structure and its expansion.
-- | Holds all information necessary for constructing a polymer
-- of Nx monomer repeats, joined by a linker.
data Polymer = Polymer { monomer, linker       :: TorsionTopo
                       , repeats               :: !Int
                       , monomerLen, linkerLen :: !Int
                       }
  deriving(Show)

instance NFData Polymer where
  rnf poly = rnf (monomer poly) `seq` rnf (linker poly)

-- | Makes a Polymer out of a monomer and linker topologies, and a number
-- of monomer repeats.
makePolymer monomerTopo linkerTopo repeats = Polymer {
                                               monomer    = monomerTopo'
                                             , linker     = linkerTopo'
                                             , repeats    = repeats
                                             , monomerLen = monoLen
                                             , linkerLen  = lastResidueId linkerTopo' - monoLen
                                             }
  where
    monomerTopo' = renumberResiduesT 1             monomerTopo
    linkerTopo'  = renumberResiduesT (monoLen + 1) linkerTopo
    monoLen = lastResidueId monomerTopo'
    lastResidueId = tResId . last . backbone -- assuming they're numbered from 1.

-- * PolymerModel as used during modeling.
-- | Polymer model data structure and its cached expansions.
data PolymerModel = PModel { polymer :: Polymer
                           , tPoly   :: TorsionTopo
                           , cPoly   :: CartesianTopo
                           }

instance Model PolymerModel where
  cartesianTopo = cPoly
  torsionTopo   = tPoly

instance NFData PolymerModel where
  rnf m = rnf (polymer m) `seq` rnf (tPoly m) `seq` rnf (cPoly m)

-- | Converts Polymer to a PolymerModel, by lazy caching of its expansions
-- as both TorsionTopo and CartesianTopo.
makePolymerModel poly = PModel { polymer = poly
                               , tPoly   = topo
                               , cPoly   = computePositions topo
                               }
  where
    topo = instantiate poly

-- | Instantiates a polymer of N identical monomer units, linked by a linker unit.
instantiate :: Polymer -> TorsionTopo
instantiate p = renumberAtomsT              1 $
                renumberResiduesT           1 $
                foldr glueChain (monomer p)   $
                concat                        $
                replicate (repeats p - 1)     [monomer p, linker p]

-- | Takes TorsionTopo of a polymer, starting, ending residues of a monomer,
-- and an ending residue of a linker afterwards, and constructs an N-repeat
-- polymer. That means that we cannot pick last monomer, and broken torsion
-- angles suggest we should pick first monomer either.
extractPolymer :: Int -> Int -> Int -> Int -> TorsionTopo -> Either String Polymer
extractPolymer i j k n topo = do (_, _, monomerAndTail)   <- tagError "Cannot find start of the monomer" $
                                                             topo           `splitTopoAt` findResId i
                                 (monomer, linkerAndTail) <- tagError "Cannot find end of the monomer"   $
                                                             monomerAndTail `cutTopoAt`   findResId (j+1)
                                 (linker,  _leftover    ) <- tagError "Cannot find end of the linker"    $
                                                             linkerAndTail  `cutTopoAt`   findResId (k+1)
                                 return $! makePolymer monomer linker n
  where
    -- TODO: tagError may be generic transformation from Maybe monad to Either monad! (Or `ifMissing`?)
    tagError :: String -> Maybe a -> Either String a
    tagError msg = maybe (Left msg) Right
-- TODO: Try to repair broken first two torsion angles in case of a monomer, by filling with two next torsion angles after the linker?
-- TODO: assertions for length in residues

-- | Checks that we are at given residue id.
findResId i torsion = tResId torsion == i


-- | Takes a TorsionTopo of a polymer, automatically extracts its parameters,
-- and creates an averaged version.
extractAveragePolymer :: TorsionTopo -> Polymer
extractAveragePolymer = undefined

-- TODO: class InstantiateModel a b where instantiate :: a -> b
-- TODO: instance TorsionTopo -> CartesianTopo
-- TODO: instance Polymer     -> TorsionTopo
-- TODO: instance Polymer     -> CartesianTopo
-- TODO: with memoization?
-- TODO: limited sampling?

-- * Utilities for gluing chains
-- | Replaces OXT of first chain with the second chain.
-- TODO: move it to FragReplacement.
glueChain :: TorsionTopo -> TorsionTopo -> TorsionTopo
glueChain topo1 topo2 = result
  where
    Just result = topo1 `changeTopoAt` (\t -> tAtName t == "OXT") $ const topo2

-- | Prepare fragment set by removing those that span the boundary between
-- monomer and linker.
delimitFragSet :: Polymer -> RFragSet -> RFragSet
delimitFragSet poly = RFragSet . V.filter criterion . unRFragSet
  where
    notOnBoundary :: RFrag -> Bool
    notOnBoundary f = (endPos   f <= monomerLen poly) ||
                      (startPos f >  monomerLen poly) &&
                      (endPos   f <= monomerLen poly + linkerLen poly)
    criterion :: V.Vector RFrag -> Bool
    criterion = notOnBoundary . V.head

-- | Sampling of a polymer with random fragment.
samplePolymer fragSet poly gen = if pos <= monomerLen poly
                                   then (polyXchgMono,   gen')
                                   else (polyXchgLinker, gen')
  where
   polyXchgMono   = poly { monomer = xchg $ monomer poly }
   polyXchgLinker = poly { linker  = xchg $ linker  poly }
   -- TODO: change fromMaybe to fromJust, after cutting fragment set at right place
   xchg topo = fromJust $ replaceFragment pos frag topo
   --xchg topo = fromMaybe topo $ replaceFragment pos frag topo
   ((_pos, frag), gen') = randomF fragSet gen
   pos = startPos frag 
   assertions = assert $ pos <= monomerLen poly + linkerLen poly

-- | Sampling method to be applied to whole PolymerModel.
samplePolymerModel :: RandomGen t => RFragSet -> PolymerModel -> t -> (PolymerModel, t)
samplePolymerModel fragSet polyModel gen = debuggingOn $
                                            (PModel { polymer = poly'
                                                    , tPoly   = topo'
                                                    , cPoly   = computePositions topo'
                                                    }, gen' )
  where
    (poly', gen') = samplePolymer fragSet (polymer polyModel) gen
    topo' = instantiate poly'
    debuggingOff = id
    debuggingOn  = traceShow ( "samplePolymerModel"
                             , monomerLen $ polymer polyModel
                             , linkerLen  $ polymer polyModel)

