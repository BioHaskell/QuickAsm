{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Allows for making a repeat polymer.
module RepeatPolymer( Polymer     (..)
                    , makePolymer
                    , extractPolymer
                    , glueChain -- move to 
                    , instantiate      ) where

import Topo
import FragReplace

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

    

