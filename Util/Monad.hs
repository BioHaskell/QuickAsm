-- | Various Monad utilities possibly hidden in a contrived way inside standard library.
module Util.Monad( timesM
                 , composeM ) where

import Control.DeepSeq

-- * Functions for starting and performing a given number of annealing stages.

-- | Compose two monadic action (possibly Kleisli Category composition?)
-- TODO: Strictly evaluate result after first action.
composeM ::  Monad m => (a -> m b) -> (b -> m c) -> a -> m c
composeM a b t = do r <- a t
                    b r
--                  r `deepseq` b r

-- TODO: check why it leaks stack space...
--timesM n = foldl1 composeM . replicate n

-- | Repeat a given action N times, strictly evaluating result after
-- each time.
timesM :: (Monad m, NFData a) => Int -> (a -> m a) -> a -> m a
timesM 0 f a = return a
timesM n f a = do b <- f a
                  b `deepseq` timesM (n-1) f b
infix 4 `timesM`
