{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module UniTree(UniTree.para, depth, descendTree) where

import Data.Tree
import qualified Data.Generics.Uniplate.Direct as U

-- TODO: check that it is indeed most efficient implementation?
instance U.Uniplate (Tree a) where
  uniplate (Node rec forest) = U.plate Node U.|- rec U.||+ forest

instance U.Biplate (Tree a) (Tree a) where
  biplate = U.plateSelf

{-# INLINE para #-}
para :: (Tree a -> [r] -> r) -> Tree a -> r
para = U.para

depth = UniTree.para aux
  where
    aux _ [] = 1 
    aux _ l  = maximum l

-- TODO: define with uniplate
-- (a -> b -> (a, c)) -> a -> t b -> (a, t c)
