{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module UniTree() where

import Data.Tree
import Data.Generics.Uniplate.Direct

-- TODO: check that it is indeed most efficient implementation?
instance Uniplate (Tree a) where
  uniplate (Node rec forest) = plate Node |- rec ||+ forest

instance Biplate (Tree a) (Tree a) where
  biplate = plateSelf

{-# INLINE para #-}
para :: (Tree a -> [r] -> r) -> Tree a -> r
para = Data.Generics.Uniplate.Direct.para


depth = UniTree.para aux
  where
    aux _ [] = 1 
    aux _ l  = maximum l
