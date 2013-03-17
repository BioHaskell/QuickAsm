{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, Rank2Types #-}
-- | Implementation of polymorphic lists of scoring functions.
module Score.ScoringFunction( ScoringFunction (..)
                            , ScoreList
                            , showScores
                            , score
                            , simpleScoringFunction
                            , totalScore
                            ) where

import qualified Data.ByteString.Char8 as BS
import System.IO(stdout, hPutStrLn)

import Model

type ScoreList = [(BS.ByteString, Double)]

type DetailShower = (Model m) => m -> IO [BS.ByteString]
type ScoreLister  = (Model m) => m -> IO ScoreList
type Scorer       = (Model m) => m -> IO Double

-- TODO: Move ScoringFunction to a separate module? (Than makeScoreSet.)
-- TODO: add function that scores a cross of two models.
-- | Actions available for any generic scoring function or potential.
data ScoringFunction = ScoringFunction {
    -- | Shows details of scoring function components.
    scoreShow   :: DetailShower
    -- | Shows a label used when showing this function along with others
  , scoreLabel  :: BS.ByteString
  , scores      :: ScoreLister 
  , components  :: [ScoringFunction]
  }

-- | Computes a value of a scoring function.
score :: (Model m) => ScoringFunction -> m -> IO Double
score sf arg = totalScore `fmap` scores sf arg

-- | Takes a total score from a scores list.
totalScore :: ScoreList -> Double
totalScore = snd . head

-- | Defines a simple scoring function with a given name
simpleScoringFunction :: BS.ByteString -> Scorer -> DetailShower -> ScoringFunction
simpleScoringFunction name fun detailsFun = self
  where
    self = ScoringFunction
             { scoreShow  = detailsFun
             , scoreLabel = name
             , scores     = \arg -> do val <- fun arg
                                       return [(name, val)]
             , components = [self]
             }

-- | Show labels and scoring function values in two lines.
-- To just get values, one could take just second line,
-- but then alignment breaks.
showScores :: [(BS.ByteString, Double)] -> BS.ByteString
showScores labelledScores = mkLine labels `BS.append` mkLine numbers
  where
    mkLine columns = "," `BS.intercalate` zipWith adjust lens columns
    lens           = zipWith max (map BS.length labels )
                                 (map BS.length numbers)
    numbers        = map (BS.pack . show . snd) labelledScores
    labels         = map fst                    labelledScores

-- | Right justifies a ByteString to reach a given length.
adjust ::  Int -> BS.ByteString -> BS.ByteString
adjust i l = BS.replicate (i - BS.length l) ' ' `BS.append` l


