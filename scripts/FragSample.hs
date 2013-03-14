{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import System.Random (randomR, getStdRandom, RandomGen)
import System.Exit   (exitFailure, exitSuccess)
import Control.Monad (when)
import System.IO     (hPutStrLn, stderr)

import qualified Data.Vector           as V
import qualified Data.ByteString.Char8 as BS

import qualified Rosetta.Fragments     as F
import qualified Rosetta.Silent        as S
import FragReplace
import Topo
import Util.Timing
import Score.DistanceRestraints(prepareDistanceScore)
import Score.Steric(stericScore)
import Score.ScoreSet(makeScoreSet)
import Score.ScoringFunction(score)


main = do args <- getArgs
          when (length args /= 5) $ do hPutStrLn stderr "FragSample <fragments_R3> <silentInput.out> <restraints.cst> <output.out> <output.pdb>"
                                       hPutStrLn stderr "NOTE: only first model from <silentInput.out> is taken."
                                       exitFailure
          let [ fragmentInputFilename, silentInputFilename, restraintsInput ,
                silentOutputFilename , pdbOutputFilename                    ] = args
          main' fragmentInputFilename silentInputFilename restraintsInput silentOutputFilename pdbOutputFilename
          exitSuccess

main' fragmentInputFilename silentInputFilename restraintsInput silentOutputFilename pdbOutputFilename = 
    do fragset <- time "Reading fragment set" $ F.processFragmentsFile fragmentInputFilename
       mdls    <- time "Reading silent file " $ S.processSilentFile    silentInputFilename
       mdl <- timePure "Converting silent model to topology" $ silentModel2TorsionTopo $ head mdls
       let cartopo = computePositions mdl
       distScore <- time' "Preparing distance restraints" $ prepareDistanceScore cartopo restraintsInput
       let scoreSet = makeScoreSet "score" [ distScore
                                           , stericScore ]
       iniScore <- time "Computing initial score" $ score scoreSet (mdl, cartopo)
       putStrLn $ "Initial score: " ++ show iniScore
       newMdl <- time "Replacing a random fragment" $ getStdRandom $ randomReplace fragset mdl
       let newCarTopo = computePositions mdl
       newScore <- time "Computing new score" $ score scoreSet (mdl, cartopo)
       putStrLn $ "New score: " ++ show newScore
       -- TODO: implement torsionTopo2SilentModel
       smdl <- timePure "Computing silent model" $ torsionTopo2SilentModel newMdl
       time "Writing silent file" $ S.writeSilentFile silentOutputFilename [smdl]
       time "Writing PDB file"    $ writeFile pdbOutputFilename $ showCartesianTopo $ computePositions newMdl  
       

