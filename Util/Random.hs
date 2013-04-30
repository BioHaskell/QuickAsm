-- | Generating normally distributed variables using Box-Muller and
-- System.Random.stdRandom.
module Util.Random(boxMuller, normal, twoNormals) where

import System.Random

-- | Generate two independent normally distributed variables using
-- Box-Muller transform.
-- See: http://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
boxMuller :: RandomGen r => r -- ^ random number generator
                        -> ((Double, Double), r)
boxMuller gen = ((r * cos theta 
                 ,r * sin theta)
                 ,gen'')
  where
    (u1, gen' ) = randomR (0, 1) gen
    (u2, gen'') = randomR (0, 1) gen'
    r     = sqrt $ -2 *      log u1
    theta =         2 * pi *     u2

-- | Generate a single normally distributed random variable.
normal ::  RandomGen t => Double -- ^ mean
                       -> Double -- ^ standard deviation
                       -> t      -- ^ random number generator
                       -> (Double, t)
normal mean stdev gen = (x, gen')
  where
    ((x, _), gen') = twoNormals mean stdev undefined undefined gen

-- | Generate two normally distributed random variables.
twoNormals :: RandomGen t => Double -- ^ mean of the first random variable
                          -> Double -- ^ standard deviation of the first variable
                          -> Double -- ^ mean of the second random variable
                          -> Double -- ^ standard deviation of the second variable
                          -> t
                          -> ((Double, Double), t)
twoNormals mean1 stdev1 mean2 stdev2 gen =
    (( mean1 + u1*stdev1
     , mean2 + u2*stdev2 )
    ,gen'')
  where
    ((u1, u2), gen'') = boxMuller gen

