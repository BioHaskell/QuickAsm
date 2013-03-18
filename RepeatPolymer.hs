{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Allows for making a repeat polymer.
module RepeatPolymer( Polymer      (..)
                    , makePolymer
                    , extractPolymer
                    , glueChain -- move to 
                    , instantiate
                    , PolymerModel (..)
                    , makePolymerModel
                    , samplePolymer
                    , polymerFragSampler ) where

import System.Random(RandomGen)
import Data.Maybe(fromMaybe)
import Control.Exception(assert)

import Rosetta.Fragments(RFragSet)

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


makePolymer monomerTopo linkerTopo repeats = Polymer {
                                               monomer    = monomerTopo
                                             , linker     = linkerTopo
                                             , repeats    = repeats
                                             , monomerLen = lengthInResidues monomerTopo
                                             , linkerLen  = lengthInResidues linkerTopo
                                             }
  where
    lengthInResidues = tResId . last . backbone -- assuming they're numbered from 1.

data PolymerModel = PModel { polymer :: Polymer
                           , tPoly   :: TorsionTopo
                           , cPoly   :: CartesianTopo
                           }

instance Model PolymerModel where
  cartesianTopo = cPoly
  torsionTopo   = tPoly

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
    numberedTopo = renumberResiduesT 1 topo
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

samplePolymer fragSet poly gen = if pos <= monomerLen poly
                                   then (polyXchgMono,   gen')
                                   else (polyXchgLinker, gen')
  where
   polyXchgMono   = poly { monomer = xchg $ monomer poly }
   polyXchgLinker = poly { linker  = xchg $ linker  poly }
   -- TODO: change fromMaybe to fromJust, after cutting fragment set at right place
   xchg topo = fromMaybe topo $ replaceFragment pos frag topo
   ((pos, frag), gen') = randomF fragSet gen
   assertions = assert $ pos <= monomerLen poly + linkerLen poly

polymerFragSampler :: RandomGen t => RFragSet -> PolymerModel -> t -> PolymerModel
polymerFragSampler fragSet polyModel gen = PModel { polymer = poly'
                                                  , tPoly   = topo'
                                                  , cPoly   = computePositions topo'
                                                  }
  where
    (poly', gen') = samplePolymer fragSet (polymer polyModel) gen
    topo' = instantiate poly'
