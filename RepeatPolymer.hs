{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Allows for making a repeat polymer.
module RepeatPolymer( Polymer     (..)
                    , makePolymer
                    , glueChain -- move to 
                    , instantiate      ) where

import Topo
import FragReplacement

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

-- | Takes TorsionTopo of a polymer, automatically extracts its parameters, and coordinates.
-- For convenience i-th polymer is given as a reference.
extractPolymer :: Int -> TorsionTopo -> Polymer
extractPolymer i = undefined

-- | Takes a TorsionTopo of a polymer, automatically extracts its parameters,
-- and creates an averaged version.
extractAveragePolymer :: TorsionTopo -> Polymer
extractAveragePolymer = undefined

-- TODO: class InstantiateModel a b where instantiate :: a -> b
-- TODO: instance TorsionTopo -> CartesianTopo
-- TODO: instance Polymer     -> TorsionTopo
-- TODO: instance Polymer     -> CartesianTopo
-- TODO: with memoization?

-- * Utilities for gluing chains
-- | Replaces OXT of first chain with the second chain.
-- TODO: move it to FragReplacement.
glueChain :: TorsionTopo -> TorsionTopo -> TorsionTopo
glueChain topo1 topo2 = result
  where
    Just result = topo1 `changeAt` (\t -> tAtName t == "OXT") $ const topo2

    

