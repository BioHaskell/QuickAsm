{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Score.Sparta( parseSparta
                   , parseSpartaFile
                   , runSparta
                   , runSpartaWithFilename
                   , scoreSparta
                   , weightChemShifts
                   ) where

import qualified Data.ByteString.Char8 as BS
import System.Process(readProcessWithExitCode)
import System.IO.Temp(withSystemTempFile)
import System.IO(hPutStr, hFlush, hPutStrLn, stderr)
import System.Exit(ExitCode(..))

import Rosetta.Util(readEither)

import Score.ScoringFunction
import Topo

-- TODO: ScoringFunction
-- * Synthetic score using SPARTA+.
-- | Takes chemical shifts file, and pair of Torsion and Cartesian topologies,
-- and returns SPARTA score.
scoreSparta :: FilePath -> (TorsionTopo, CartesianTopo) -> IO Double
scoreSparta  csFilename (_, cartopo) = do spartaResult <- runSparta csFilename cartopo
                                          case spartaResult of
                                            Left  errMsg -> do hPutStrLn stderr $ "SPARTA error: " ++ errMsg
                                                               return 0
                                            Right result -> return $ weightChemShifts result

-- | Computes chemical shift score like that of Preditor.
weightChemShifts rmsList = sum $ map weightChemShift rmsList

-- | Returns a chemical shift weight for each atom type.
-- Weights from: http://www-vendruscolo.ch.cam.ac.uk/montalvao08jacs.pdf
-- http://www-vendruscolo.ch.cam.ac.uk/montalvao08jacs.pdf
-- H-alpha = 75, N, C-alpha, C-beta = 25
-- Total C correlation is capped by 15
-- Uses correlations!!!
-- http://www.pnas.org/content/104/23/9615.full
-- This one uses differences (not RMSds)
weightChemShift ("H",  rms, _) = rms * 3
weightChemShift ("CA", rms, _) = rms
weightChemShift ("CB", rms, _) = rms
weightChemShift ("N",  rms, _) = rms
weightChemShift (_,    rms, _) = 0 -- not included

-- * Running SPARTA
-- | Runs SPARTA with given chemical shifts file, and Cartesian topology argument.
runSparta csFilename cartopo = withSystemTempFile "spartaInputXXX.pdb" cont
  where
    cont path handle = do hPutStr handle $ showCartesianTopo cartopo
                          hFlush handle
                          runSpartaWithFilename csFilename path

-- TODO: Package SpartaError exception
-- TODO: use ByteString handles?
-- | Runs SPARTA with given chemical shifts file, and PDB filename.
runSpartaWithFilename csFilename structureFilename =
  do (exitCode, result, errors) <- readProcessWithExitCode spartaExecutable ["-in", structureFilename, "-ref", csFilename] ""
     case exitCode of
       ExitFailure code -> return $ Left $ "SPARTA process returned error " ++ show code ++ ": " ++ errors
       ExitSuccess      -> return $ parseSparta $ BS.pack result

-- | SPARTA executable used by runSparta and runSpartaWithFilename.
spartaExecutable = "sparta"

-- * Parsing SPARTA standard output
-- | Takes a ByteString and returns list of tuples with
-- (atom name, RMS, count), where count is a number of shifts for a given atom type.
parseSparta input = if BS.null x
                      then Left "Cannot find SPARTA header!"
                      else mapM extract records
  where
    (x, input')   = header `BS.breakSubstring` input
    lines         = BS.lines $ BS.drop (BS.length header) input'
    records       = filter isRMSRecord $ filter ((>=5) . length) $ filter (not . null) $ map BS.words lines
    header        = "Analysis of Observed vs Predicted Shifts:"
    isRMSRecord r = (r !! 1 == "RMS:") && (r !! 4 == "Count:")
    extract r     = do let atName = r !! 0
                       rms   :: Double <- readEither ("SPARTA RMS of "   ++ BS.unpack atName) $ r !! 2
                       count :: Int    <- readEither ("SPARTA count of " ++ BS.unpack atName) $ r !! 5
                       return (r !! 0, rms, count)

-- | Reads SPARTA standard output from a file, and returns results in the same format as parseSparta.
parseSpartaFile fname = parseSparta `fmap` BS.readFile fname

