-- | Contains utility methods for Data.Tree.Tree.
module Data.Tree.Util( descending
                     , transforming
                     , rightistFlatten ) where

import Data.Tree
import Data.List(foldl')

-- | Descends a tree, inheriting accumulator
--   May be generalized into descend f a = fmap snd . inherit (\(a, _) c -> f a c) (a, undefined),
--   where inherit is top-down correspondent of scanl.
descending ::  (a -> b -> (a, c)) -> a -> Tree b -> Tree c
descending f acc (Node rec forest) = Node rec' $ map (descending f acc') forest
  where
    (acc', rec') = f acc rec

-- | Descends a tree, inheriting accumulator, and allowing deeper inspection of subtrees (not just labels!)
transforming ::  (a -> Tree b -> (a, c)) -> a -> Tree b -> Tree c
transforming f acc node@(Node rec forest) = Node rec' $ map (transforming f acc') forest
  where
    (acc', rec') = f acc node

-- | Flattens the tree in such a way, that node is before its nodes, and nodes are traversed in the reverse list order.
rightistFlatten :: Tree a -> [a]
rightistFlatten t = proceed [] t
  where
    proceed :: [a] -> Tree a -> [a]
    proceed xs (Node x ts) = x:foldl' proceed xs ts

