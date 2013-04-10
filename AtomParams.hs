-- | Chemical parameters of atoms.
module AtomParams( atomType
                 , atomicRadius
                 , atomRadius
                 , maxAtomicRadius ) where

-- | Returns atom's element for a given atom code.  May be used to substitute
-- element code when it is not provided.
atomType "C"   = "C"
atomType "CA"  = "C"
atomType "CB"  = "C"
atomType "CG"  = "C"
atomType "N"   = "N"
atomType "HA"  = "H"
atomType "H"   = "H"
atomType "HB"  = "H"
atomType "O"   = "O"
atomType "OXT" = "O"
atomType "P"   = "P"
atomType "S"   = "S"
atomType at    = error $ "Unknown atom type for: " ++ show at

-- | Returns van der Waals radius of an atom of given element.
-- Taken from Wikipedia, cited Bondi 1964.
atomicRadius "H" = 1.2 -- or 1.09?
--atomicRadius "C" = 1.7
atomicRadius "C" = 2.0 -- LJ radius consistent with ROSETTA
atomicRadius "N" = 1.55
atomicRadius "O" = 1.52
atomicRadius "P" = 1.8
atomicRadius at  = error $ "Atomic radius of unknown element: " ++ show at

-- | Given an atom code, returns van der Waals radius.
atomRadius = atomicRadius . atomType

-- | Constant of highest van der Waals radius among known atom types.
-- Useful as upper bound for neighbourhood checks.
maxAtomicRadius = atomicRadius "P"

