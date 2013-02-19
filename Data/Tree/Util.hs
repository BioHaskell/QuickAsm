module Data.Tree.Util(descending) where

-- ^ Contains utility methods for Data.Tree.Tree.
import Data.Tree

-- | Descends a tree, inheriting accumulator
--   May be generalized into descend f a = fmap snd . inherit (\(a, _) c -> f a c) (a, undefined),
--   where inherit is top-down correspondent of scanl.
descending ::  (a -> b -> (a, c)) -> a -> Tree b -> Tree c
descending f acc (Node rec forest) = Node rec' $ map (descending f acc') $ forest
  where
    (acc', rec') = f acc rec

-- | Descends a tree, inheriting accumulator, and allowing deeper inspection of subtrees (not just labels!)
transforming ::  (a -> Tree b -> (a, c)) -> a -> Tree b -> Tree c
transforming f acc node@(Node rec forest) = Node rec' $ map (transforming f acc') $ forest
  where
    (acc', rec') = f acc node

