{-# OPTIONS_GHC -funbox-strict-fields #-}
module RepeatPolymer where

-- | Holds all information necessary for constructing a polymer
-- of Nx monomer repeats, joined by a linker.
data Polymer a = Polymer { monomer, linker       :: Tree a
                         , repeats               :: !Int
                         , monomerLen, linkerLen :: !Int
                         }

instantiate :: Polymer -> TorsionTopo
instantiate = undefined

-- TODO: class InstantiateModel a b where instantiate :: a -> b
-- TODO: instance TorsionTopo -> CartesianTopo
-- TODO: instance Polymer     -> TorsionTopo
-- TODO: instance Polymer     -> CartesianTopo
-- TODO: with memoization?

