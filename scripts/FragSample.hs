{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs, StandaloneDeriving #-}
module Main where

import System.Environment
import System.Random (randomR, getStdRandom, RandomGen)
import System.Exit   (exitFailure, exitSuccess)
import Control.Monad (when)
import Control.DeepSeq(deepseq, NFData(..))
import System.IO     (hPutStrLn, stderr)

import qualified Data.Vector           as V
import qualified Data.ByteString.Char8 as BS

import Annealing
import qualified Rosetta.Fragments     as F
import qualified Rosetta.Silent        as S
import FragReplace
import Topo
import Util.Timing
import Score.DistanceRestraints(prepareDistanceScore)
import Score.Steric(stericScore)
import Score.ScoreSet(makeScoreSet)
import Score.ScoringFunction(score, ScoringFunction)
import Modelling
import Model

main = do args <- getArgs
          when (length args /= 6) $ do hPutStrLn stderr "FragSample <fragments_R3> <silentInput.out> <restraints.cst> <currentOutput.out> <best.out> <output.pdb>"
                                       hPutStrLn stderr "NOTE: only first model from <silentInput.out> is taken."
                                       exitFailure
          let [ fragmentInputFilename, silentInputFilename, restraintsInput ,
                silentCurrentOutputFilename , silentBestOutputFilename, pdbOutputFilename ] = args
          main' fragmentInputFilename silentInputFilename restraintsInput silentCurrentOutputFilename silentBestOutputFilename pdbOutputFilename
          exitSuccess


main' fragmentInputFilename silentInputFilename restraintsInput silentCurrentOutputFilename silentBestOutputFilename pdbOutputFilename = 
    do fragset <- time "Reading fragment set" $ F.processFragmentsFile fragmentInputFilename
       mdls    <- time "Reading silent file " $ S.processSilentFile    silentInputFilename
       mdl <- timePure "Converting silent model to topology" $ silentModel2TorsionTopo $ head mdls
       let cartopo = computePositions mdl
       distScore <- time' "Preparing distance restraints" $ prepareDistanceScore cartopo restraintsInput
       let scoreSet = makeScoreSet "score" [ distScore
                                           , stericScore ]
       iniScore <- time "Computing initial score" $ score scoreSet $ initTorsionModel mdl
       annState <- time "Annealing protocol" $ annealingProtocol (torsionFragSampler fragset) scoreSet (iniScore*0.2) 0.9 30 100 $ initTorsionModel mdl
       let bestMdl    = model $ best    annState
       let currentMdl = model $ current annState
       putStrLn $ "Final score " ++ show (modelScore $ current annState)
       putStrLn $ "Best score "  ++ show (modelScore $ best    annState)
       time "Writing silent file for final model" $ S.writeSilentFile silentCurrentOutputFilename [torsionTopo2SilentModel $ tTopo currentMdl]
       time "Writing silent file for best model"  $ S.writeSilentFile silentBestOutputFilename    [torsionTopo2SilentModel $ tTopo bestMdl   ]
       time "Writing PDB file"   $ writeFile pdbOutputFilename $ showCartesianTopo $ cartesianTopo bestMdl

