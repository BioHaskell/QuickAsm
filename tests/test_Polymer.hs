{-# LANGUAGE ScopedTypeVariables #-}
-- | Tests polymer construction and instantiation.
module Main where

import System.IO(hPutStrLn, stderr)
import System.Exit(exitFailure)
import Control.Exception(assert)
import Control.Monad(forM_)

import Rosetta.Silent

import RepeatPolymer
import FragReplacement
import Topo
import Util.Fasta
import Util.Assert(assertM)

inputSilent = "examples/polymer/polymer.out"

monomerLength = 61
monomerCount  = 5
linkerLength  = 32

computePolymerLength monomerCount monomerLength linkerLength = monomerLength * monomerCount + linkerLength * (monomerCount - 1)

computePolymerSequence monomerLength linkerLength seq =
    assertions (monomerSeq, linkerSeq)
  where
    (monomerSeq :: String, rest)  = splitAt monomerLength seq
    linkerSeq   :: String         = take linkerLength     rest
    expectedLength                = computePolymerLength monomerCount monomerLength linkerLength
    reSeq       :: String         = concat $ concat $ replicate (monomerCount - 1) [monomerSeq, linkerSeq] ++ [[monomerSeq]]
    assertions                    = assert (expectedLength == length seq) .
                                    assert (reSeq          ==        seq) .
                                    assert (monomerCount   >= 2         )

showPolymer = showCartesianTopo . computePositions . instantiate

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
          forM_ [0..3] $ \i -> 
            case getPolymer topo i of
              Left errMsg   -> do hPutStrLn stderr errMsg
                                  exitFailure
              Right polymer -> do let monoSeq2   = topo2sequence $ monomer polymer
                                  let linkerSeq2 = topo2sequence $ linker  polymer
                                  putStrLn $ "Extracted monomer seq: " ++ monoSeq2
                                  putStrLn $ "Extracted linker  seq: " ++ linkerSeq2
                                  assertM $ monoSeq   == monoSeq2
                                  assertM $ linkerSeq == linkerSeq2
                                  assertM $ seq       == topo2sequence (instantiate polymer)
                                  writeFile ("poly_" ++ show i ++ ".pdb") $ showPolymer polymer
  where
    getPolymer topo i = extractPolymer first
                                       (first + monomerLength                - 1)
                                       (first + monomerLength + linkerLength - 1)
                                       5
                                       topo
      where
        first             = (monomerLength + linkerLength) * i + 1

