{-# LANGUAGE ScopedTypeVariables #-}
-- | Tests polymer construction and instantiation.
module Main where

import Control.Exception(assert)

import Rosetta.Silent

import RepeatPolymer
import FragReplacement
import Topo
import Util.Fasta

inputSilent = "examples/polymer/polymer.out"

monomerLength = 61
monomerCount  = 5
linkerLength  = 32

topo2sequence = map (resname2fastacode . tResName) . filter isCAlpha . backbone
  where
    isCAlpha rec = tAtName rec == "CA" 

assertM condition = assert condition $ return ()

computePolymerLength monomerCount monomerLength linkerLength = monomerLength * monomerCount + linkerLength * (monomerCount - 1)

computePolymerSequence monomerLength linkerLength seq =
    assertions (monomerSeq, linkerSeq)
  where
    (monomerSeq :: String, rest)  = splitAt monomerLength seq
    linkerSeq   :: String         = take linkerLength     rest
    expectedLength                = computePolymerLength monomerCount monomerLength linkerLength
    reSeq :: String               = concat $ concat $ replicate (monomerCount - 1) [monomerSeq, linkerSeq] ++ [[monomerSeq]]
    assertions                    = assert (expectedLength == length seq) .
                                    assert (reSeq          ==        seq) .
                                    assert (monomerCount   >= 2         )

-- TODO: use contexts

-- | 
-- Migrate to FragReplace (next to changeAt.)
splitTopoAt :: (Torsion -> Bool)-> Tree Torsion -> (Double -> Tree Torsion) -> Maybe (Tree Torsion)
splitTopoAt pred topo filler = topo `splitTopoAt` pred $ filler . tDihedral . rootLabel

cutTopoAt pred topo = splitTopoAt tOxt 


main = do topo <- (head . map silentModel2TorsionTopo) `fmap` processSilentFile inputSilent
          let seq = topo2sequence topo
          let (monoSeq, linkerSeq) = computePolymerSequence monomerLength linkerLength seq
          print $ monomerLength * monomerCount + linkerLength * (monomerCount - 1)
          print $ length seq
          putStrLn seq
          putStr "Monomer: "
          putStrLn monoSeq
          putStr "Linker : "
          putStrLn linkerSeq
          


