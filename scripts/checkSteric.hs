module Main where

import qualified Data.ByteString.Char8 as BS
import System.Environment
import Control.Exception (assert)
import Control.Monad     (forM)
import Data.Tree(flatten   )
import Numeric  (showFFloat)

import Rosetta.Silent
import Topo
import Score.Steric
import Data.Octree(dist)

visualizeClash (a, b) = show a ++ "X:X" ++ show b ++ " = " ++ showFFloat (Just 3) (cPos a `dist` cPos b) ""

main = do args <- getArgs
          assert (length args >= 1) $ return () -- TODO: define assertM?
          mdls <- processSilentFile $ BS.pack $ args !! 0
          forM mdls $ \mdl -> do let cart = computePositions $ silentModel2TorsionTopo mdl
                                 let clashes = Prelude.map visualizeClash $ selfClashCheck $ flatten $ cart
                                 mapM putStrLn clashes
                                 putStrLn $ show (length clashes) ++ " steric clashes detected in " ++ BS.unpack (name mdl)
          return ()
          
