{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of polymorphic lists of scoring functions.
module Score.ScoringFunction( ScoringFunction (..)
                            , showScores
                            ) where

import qualified Data.ByteString.Char8 as BS
import System.IO(stdout, hPutStrLn)

import Topo(TorsionTopo, CartesianTopo, computePositions)

-- TODO: Move ScoringFunction to a separate module? (Than makeScoreSet.)
-- TODO: add function that scores a cross of two models.
-- | Actions available for any generic scoring function or potential.
data ScoringFunction = ScoringFunction {
    -- | Computes a value of a scoring function.
    score       :: (TorsionTopo, CartesianTopo) -> Double
    -- | Shows details of scoring function components.
  , scoreShow   :: (TorsionTopo, CartesianTopo) -> [BS.ByteString]
    -- | Shows a label used when showing this function along with others
  , scoreLabel  :: BS.ByteString
  , scores      :: (TorsionTopo, CartesianTopo) -> [(BS.ByteString, Double)]
  , components  :: [ScoringFunction]
  }

showScores :: [(BS.ByteString, Double)] -> BS.ByteString
showScores labelledScores = mkLine labels `BS.append` mkLine numbers
  where
    mkLine columns = "," `BS.intercalate` zipWith adjust lens columns
    lens           = zipWith max (map BS.length labels )
                                 (map BS.length numbers)
    numbers        = map (BS.pack . show . snd) labelledScores
    labels         = map fst                    labelledScores

adjust ::  Int -> BS.ByteString -> BS.ByteString
adjust i l = BS.replicate (i - BS.length l) ' ' `BS.append` l


