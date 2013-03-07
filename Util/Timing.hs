module Util.Timing where

import System.IO(hPutStrLn, stderr)
import System.CPUTime(getCPUTime)
import Control.DeepSeq(NFData(..), deepseq)
import Numeric(showFFloat)

time :: (NFData a) => String -> IO a -> IO a
time name computation = time' name $ do result <- computation
                                        result `deepseq` return result

time' :: String -> IO a -> IO a
time' name computation = do start  <- getCPUTime
                            result <- computation
                            end    <- getCPUTime
                            let diff = fromIntegral (end - start) / 10^12
                            hPutStrLn stderr $ name ++ " took " ++ showFFloat (Just 3) diff " seconds."
                            return result

timePure :: (NFData a) => String -> a -> IO a
timePure name computation = time name (return computation)

