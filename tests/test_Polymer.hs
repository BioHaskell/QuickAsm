{-# LANGUAGE ScopedTypeVariables #-}
-- | Tests polymer construction and instantiation.
module Main where

import Control.Exception(assert)
import System.IO(hPutStrLn, stderr)
import System.Exit(exitFailure)

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
    reSeq       :: String         = concat $ concat $ replicate (monomerCount - 1) [monomerSeq, linkerSeq] ++ [[monomerSeq]]
    assertions                    = assert (expectedLength == length seq) .
                                    assert (reSeq          ==        seq) .
                                    assert (monomerCount   >= 2         )

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
          case getPolymer topo of
            Left errMsg   -> do hPutStrLn stderr errMsg
                                exitFailure
            Right polymer -> do let monoSeq2   = topo2sequence $ monomer polymer
                                let linkerSeq2 = topo2sequence $ linker  polymer
                                putStrLn $ "Extracted monomer seq: " ++ monoSeq2
                                putStrLn $ "Extracted linker  seq: " ++ linkerSeq2
                                assertM $ monoSeq   == monoSeq2
                                assertM $ linkerSeq == linkerSeq2
  where
    i               = 1
    first           = (monomerLength + linkerLength) * i + 1
    getPolymer topo = extractPolymer first
                                     (first + monomerLength                - 1)
                                     (first + monomerLength + linkerLength - 1)
                                     5
                                     topo

