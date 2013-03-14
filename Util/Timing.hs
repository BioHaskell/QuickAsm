-- | Utilities to perform basic timing of program fragments.
module Util.Timing(time, time', timePure, sysTime) where

import System.IO(hPutStrLn, stderr)
import System.CPUTime(getCPUTime)
import Data.Time.Clock.POSIX(getPOSIXTime)
import Control.DeepSeq(NFData(..), deepseq)
import Numeric(showFFloat)

-- | Strict measurement of CPU time spend on computation.
time    :: (NFData a) => String -> IO a -> IO a
time    = deepSeqTime getCPUTimeInSeconds

-- | Lenient measurement of CPU time spend on computation (without forcing strictness.)
time' = time'' getCPUTimeInSeconds

getCPUTimeInSeconds = do t <- getCPUTime
                         return $! fromIntegral t / 1e12

-- | Strict measurement of POSIX system time spend during computation.
sysTime :: (NFData a) => String -> IO a -> IO a
sysTime = deepSeqTime (realToFrac `fmap` getPOSIXTime)

deepSeqTime :: (NFData a) => IO Double -> String -> IO a -> IO a
deepSeqTime timer name computation = time'' timer name $ do result <- computation
                                                            result `deepseq` return result


time'' :: IO Double -> String -> IO a -> IO a
time'' timer name computation = do start  <- timer
                                   result <- computation
                                   end    <- timer
                                   let diff = end - start
                                   hPutStrLn stderr $ name ++ " took " ++ showFFloat (Just 3) diff " seconds."
                                   return result

-- | Strict measurement of CPU time spend during a pure computation.
timePure :: (NFData a) => String -> a -> IO a
timePure name computation = time name (return computation)

