{-# LANGUAGE OverloadedStrings #-}
-- | Testing RDC restraint score.
module Main where

import           Control.Monad(forM)
import qualified Data.Vector as V

import Score.RDC
import Rosetta.RDC

-- | Input RDC file.
inputFilename = "examples/restraints/ideal.rdc"

szz, syy, sxx :: Double
(szz, syy, sxx) = (5.0153e-05, 7.1572e-04, -7.6588e-04)

main = do rdcSet <- parseRDCRestraintsFile inputFilename
          let rdcList = V.toList $ unRDCSet rdcSet
          forM   rdcList $ \rdc1 ->
            forM rdcList $ \rdc2 ->
              do putStrLn $ "RDC 1: " ++ show rdc1
                 putStrLn $ "RDC 2: " ++ show rdc2
                 putStrLn $ "Intervector projection bounds: " ++
                   show (rdcInterprojection sxx syy szz rdc1 rdc2)

