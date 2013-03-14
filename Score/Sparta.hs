{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Score.Sparta( parseSparta
                   , parseSpartaFile
                   , runSparta
                   , runSpartaWithFilename
                   , scoreSparta
                   , weightChemShifts
                   ) where

import qualified Data.ByteString.Char8 as BS
import Util.Process     (readProcessWithExitCodeAndWorkingDir)
import System.IO.Temp   (withSystemTempDirectory)
import System.IO        (hPutStr, hFlush, hPutStrLn, stderr)
import System.Exit      (ExitCode(..))
import System.Directory (copyFile)
import System.FilePath  ((</>))
import Control.Exception

import Rosetta.Util(readEither)

import Score.ScoringFunction
import Topo

-- TODO: ScoringFunction
-- * Synthetic score using SPARTA+.
-- | Takes chemical shifts file, and pair of Torsion and Cartesian topologies,
-- and returns SPARTA score.
scoreSparta :: FilePath -> (TorsionTopo, CartesianTopo) -> IO Double
scoreSparta  csFilename (_, cartopo) = do spartaResult <- runSparta csFilename cartopo
                                          return $ weightChemShifts spartaResult

-- | Computes chemical shift score like that of Preditor.
weightChemShifts rmsList = sum $ map weightChemShift rmsList

-- | Returns a chemical shift weight for each atom type.
-- Weights from: http://www-vendruscolo.ch.cam.ac.uk/montalvao08jacs.pdf
-- http://www-vendruscolo.ch.cam.ac.uk/montalvao08jacs.pdf
-- H-alpha = 75, N, C-alpha, C-beta = 25
-- Total C correlation is capped by 15
-- Uses correlations!!!
-- http://www.pnas.org/content/104/23/9615.full
-- This one uses differences (not RMSds.)
weightChemShift ("H",  rms, _) = rms * 3
weightChemShift ("CA", rms, _) = rms
weightChemShift ("CB", rms, _) = rms
weightChemShift ("N",  rms, _) = rms
weightChemShift (_,    rms, _) = 0 -- not included

-- * Running SPARTA
-- | Runs SPARTA with given chemical shifts file, and Cartesian topology argument, in a given working directory,
-- where pred.tab, csCalc.tab, and struct.tab will appear.
runSparta csFilename cartopo = withSystemTempDirectory "sparta" cont
  where
    cont dir = do let modelPath = dir </> "model.pdb"
                  let csPath    = dir </> "input.cs"
                  writeFile modelPath $ showCartesianTopo cartopo
                  copyFile csFilename csPath
                  runSpartaWithFilenameAndWorkingDir csPath modelPath (Just dir)

-- | Runs SPARTA with given chemical shifts file, and Cartesian topology argument, in a current directory.
runSpartaWithFilename :: String-> String -> IO [(BS.ByteString, Double, Int)]
runSpartaWithFilename csFilename structureFilename = runSpartaWithFilenameAndWorkingDir csFilename structureFilename Nothing

-- TODO: Package SpartaError exception
-- TODO: use ByteString handles?
-- | Runs SPARTA with given chemical shifts file, and PDB filename.
runSpartaWithFilenameAndWorkingDir csFilename structureFilename workingDir =
  do (exitCode, result, errors) <- readProcessWithExitCodeAndWorkingDir spartaExecutable ["-in", structureFilename, "-ref", csFilename] "" workingDir
     let spartaError msg = do hPutStrLn stderr $ "SPARTA ERROR: "           ++ msg    ++
                                                 "\nERROR OUTPUT WAS:\n"    ++ errors ++
                                                 "\nSTANDARD OUTPUT WAS:\n" ++ result
                              return []
     case exitCode of
       ExitFailure code -> spartaError $ "exit code was " ++ show code
       ExitSuccess      -> case parseSparta $ BS.pack result of
                             Left msg                                    ->    spartaError msg
                             Right result           | length result >= 5 ->    return result
                             Right incompleteResult                      -> do spartaError "incomplete parse of result"
                                                                               return incompleteResult

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

