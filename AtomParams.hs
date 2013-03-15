module AtomParams( atomType
                 , atomicRadius
                 , atomRadius
                 , maxAtomicRadius ) where

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

-- Taken from Wikipedia, cited Bondi 1964.
atomicRadius "H" = 1.2 -- or 1.09?
atomicRadius "C" = 1.7
atomicRadius "N" = 1.55
atomicRadius "O" = 1.52
atomicRadius "P" = 1.8
atomicRadius at  = error $ "Atomic radius of unknown element: " ++ show at

atomRadius = atomicRadius . atomType


maxAtomicRadius = atomicRadius "P"

