{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Score.Sparta where

import qualified Data.ByteString.Char8 as BS
import System.Posix.Process(getProcessID)
import System.Process
import System.IO.Temp
import System.FilePath
import System.Exit(ExitCode(..))

import Rosetta.Util(readEither)

import Score.ScoringFunction
import Topo

{- Example excerpt from SPARTA+ output
Analysis of Observed vs Predicted Shifts:
 N  RMS:   5.424 ppm Count:  51 Average Difference:  0.264 +/- 5.472 ppm
 HA RMS:   0.466 ppm Count:  58 Average Difference:  0.046 +/- 0.468 ppm
 C  RMS:   1.501 ppm Count:  54 Average Difference: -0.339 +/- 1.476 ppm
 CA RMS:   1.941 ppm Count:  54 Average Difference:  0.231 +/- 1.945 ppm
 CB RMS:   1.434 ppm Count:  46 Average Difference: -0.011 +/- 1.450 ppm
 HN RMS:   0.593 ppm Count:  51 Average Difference:  0.090 +/- 0.592 ppm
 -}

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

parseSpartaFile fname = parseSparta `fmap` BS.readFile fname

scoreSparta  csFilename (_, cartopo) = scoreSparta' csFilename Nothing cartopo

scoreSparta' csFilename tempfileSuggestion cartopo = undefined

spartaExecutable = "sparta"

-- TODO: Package SpartaError exception
-- TODO: use ByteString handles?
runSparta csFilename structureFilename cartopo =
  do writeFile structureFilename $ showCartesianTopo cartopo
     (exitCode, result, errors) <- readProcessWithExitCode spartaExecutable ["-in", structureFilename, "-ref", csFilename] ""
     case exitCode of
       ExitFailure code -> return $ Left $ "SPARTA process returned error " ++ show code ++ ": " ++ errors
       ExitSuccess      -> return $ parseSparta $ BS.pack result


{-
withTempFile action = do pid <- show `fmap` getProcessID
                                  withSystemTempDirectory ("Sparta_" ++ pid ++ "_") $
                                    \tempdir -> do
                                       let fname = tempdir </> "model.pdb"
-}                                          
                                  

                       

