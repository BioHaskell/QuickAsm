module Util.Show( showFloat
                , adjust
                , bAdjust   ) where

import qualified Data.ByteString.Char8 as BS
import           Numeric(showFFloat)

-- | Shows float with 3 significant digits after comma, and right justified to 9 characters (if less than 9, including comma.)
showFloat :: (RealFloat f) => f -> String
showFloat f = adjust 9 $ showFFloat (Just 3) f ""

-- | Right justifies a String     to reach a given length.
adjust i l = iterate (' ':) l !! n
  where
    n = max 0 (i - length l)

-- | Right justifies a ByteString to reach a given length.
bAdjust ::  Int -> BS.ByteString -> BS.ByteString
bAdjust i l = BS.replicate (i - BS.length l) ' ' `BS.append` l

