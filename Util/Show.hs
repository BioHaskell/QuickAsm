{-# LANGUAGE OverloadedStrings #-}
-- | Utilities for showing different values.
module Util.Show( showFloat
                , showEmpty
                , adjust
                , bAdjust
                , replace   ) where

import qualified Data.ByteString.Char8 as BS
import           Numeric(showFFloat)

-- | Shows float with 3 significant digits after comma, and right justified to 9 characters (if less than 9, including comma.)
showFloat :: (RealFloat f) => f      -- ^ float to be shown
                           -> String
showFloat f = adjust 9 $ showFFloat (Just 3) f ""

-- | Right justifies a String     to reach a given length.
adjust :: Int    -- ^ minimum length of output ByteString
       -> String -- ^ input to be justified
       -> String
adjust i l = iterate (' ':) l !! n
  where
    n = max 0 (i - length l)

-- | Right justifies a ByteString to reach a given length.
bAdjust :: Int           -- ^ minimum length of output ByteString
        -> BS.ByteString -- ^ input to be justified
        -> BS.ByteString
bAdjust i l = BS.replicate (i - BS.length l) ' ' `BS.append` l

-- | If a list of some strings is empty, it puts in a given message.
showEmpty ::  t  -- ^ error message
          -> [t] -- ^ list that should be non-empty
          -> [t]
showEmpty msg [] = [msg]
showEmpty _   e  = e

-- | Replaces all values equal to the first argument to the value of a second argument.
replace ::  Eq b => b   -- ^ value to be replaced
                 -> b   -- ^ value replacing it
                 -> [b] -- ^ input list
                 -> [b]
replace a b = map (\c -> if a == c then b else c)
