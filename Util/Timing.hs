module Util.Timing(time, timePure, sysTime) where

import System.IO(hPutStrLn, stderr)
import System.CPUTime(getCPUTime)
import System.Time   (getClockTime, ClockTime(..))
import Control.DeepSeq(NFData(..), deepseq)
import Numeric(showFFloat)

time    :: (NFData a) => String -> IO a -> IO a
time    = deepSeqTime (((/ 1e12) . fromIntegral) `fmap` getCPUTime)

sysTime :: (NFData a) => String -> IO a -> IO a
sysTime = deepSeqTime clockTime

clockTime = clock2Float `fmap` getClockTime
  where
    clock2Float (TOD sec picosec) = fromIntegral sec + 1e-12 * fromIntegral picosec

deepSeqTime :: (NFData a) => IO Double -> String -> IO a -> IO a
deepSeqTime timer name computation = time' timer name $ do result <- computation
                                                           result `deepseq` return result


time' :: IO Double -> String -> IO a -> IO a
time' timer name computation = do start  <- timer
                                  result <- computation
                                  end    <- timer
                                  let diff = end - start
                                  hPutStrLn stderr $ name ++ " took " ++ showFFloat (Just 3) diff " seconds."
                                  return result

timePure :: (NFData a) => String -> a -> IO a
timePure name computation = time name (return computation)

