{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Allows for making a repeat polymer.
module Fibril( Fibril       (..)
             , makeFibril
             , extractFibril
             , glueCartesianChain -- move to 
             , instantiate
             , FibrilModel (..)
             , makeFibrilModel
             , delimitFragSet
             , sampleFibrilMonomer
             , sampleFibril
             , sampleFibrilModel ) where

import Control.Monad.ST
--import Control.Monad.Primitive
import System.Random                  (RandomGen, randomR)
import qualified Data.Vector as V
import Data.Maybe(fromMaybe, fromJust) -- DEBUG: fromMaybe
import Data.Ix(inRange)
import Control.Exception(assert)
import Control.DeepSeq(NFData(..))
import Data.Vector.V3 as V3
import Util.Angle(degree2radian)
import Util.Random(normal)

import Debug.Trace(traceShow) -- DEBUG

import Rosetta.Fragments( RFragSet(..)
                        , RFrag   (..) )

import Topo
import FragReplace
import Model

-- * Fibril data structure and its expansion.
-- | Holds all information necessary for constructing a polymer
-- of N monomer repeats, without any linker.
data Fibril = Fibril { monomer    :: TorsionTopo
                     , shift      :: !Double
                     , twist      :: !Double
                     , repeats    :: !Int
                     , monomerLen :: !Int
                     }
  deriving(Show)

instance NFData Fibril where
  rnf = rnf . monomer

-- | Makes a Polymer out of a monomer and linker topologies, and a number
-- of monomer repeats.
makeFibril monomerTopo repeats shift twist = assert (repeats > 1) $
                                               Fibril {
                                                 monomer    = monomerTopo'
                                               , shift      = shift
                                               , twist      = twist
                                               , repeats    = repeats
                                               , monomerLen = monoLen
                                               }
  where
    monomerTopo' = renumberResiduesT 1 monomerTopo
    monoLen      = lastResidueId monomerTopo'

-- * PolymerModel as used during modeling.
-- | Polymer model data structure and its cached expansions.
data FibrilModel = FModel { fibril  :: Fibril
                          , tFibril :: TorsionTopo -- with "gapped" connections!
                          , cFibril :: CartesianTopo
                          }

-- TODO: here TorsionTopo is of different length than CartesianTopo!

instance Model FibrilModel where
  cartesianTopo fm = traceShow ("Fibril::cartesianTopo", length $ filter ((=="CA") . cAtName) $ backbone $ cFibril fm) $
                               cFibril fm
  torsionTopo   = tFibril -- there are no "cut" nodes in TorsionTopo so far!

instance NFData FibrilModel where
  rnf m = rnf (fibril m) `seq` rnf (cFibril m)

-- | Converts Polymer to a PolymerModel, by lazy caching of its expansions
-- as both TorsionTopo and CartesianTopo.
makeFibrilModel f = FModel { fibril  = f
                           , tFibril = monomer f
                           , cFibril = instantiate f
                           }

-- | Instantiates a polymer of N identical monomer units, linked by a linker unit.
instantiate :: Fibril -> CartesianTopo
instantiate f = traceShow ("instantiate",
                           length $ filter ((=="CA") . cAtName) $ backbone result,
                           repeats f) result
  where
    result = (  renumberAtomsC       1              $
                renumberResiduesC    1              $
                foldr1  glueCartesianChain          $
                zipWith shiftMonomer [1..repeats f] $
                repeat                              $
                computePositions                    $
                monomer              f              )
    shiftMonomer num mono = fmap xform mono
      where
        -- TODO: add twist!
        xform cart@(Cartesian { cPos =                                         pos }) =
                         cart { cPos = shiftTwist (shift f * fromIntegral num)
                                                  (twist f                   ) pos }

{-# INLINE shiftTwist #-}
-- | Shift & twist along Z axis.
shiftTwist s t = shiftZ s . twistXY t

-- | Translation along Z axis.
shiftZ  s (V3.Vector3 x y z) = V3.Vector3 x y $ z + s

-- | Counterclockwise twist along Z axis (along XY plane.)
twistXY t (V3.Vector3 x y z) = V3.Vector3 (x*cost - y*sint)
                                          (x*sint + y*cost) z
  where
    sint = sin $ degree2radian t
    cost = cos $ degree2radian t

-- | Takes TorsionTopo of a polymer, starting, ending residues of a monomer,
-- and an ending residue of a linker afterwards, and constructs an N-repeat
-- polymer. That means that we cannot pick last monomer, and broken torsion
-- angles suggest we should pick first monomer either.
extractFibril :: Int -> Int -> Int -> TorsionTopo -> Either String Fibril
extractFibril i j n topo = do (_, _, monomerAndTail) <- tagError "Cannot find start of the monomer" $
                                                          topo           `splitTopoAt` findResId i
                              (monomer, tail       ) <- tagError "Cannot find end of the monomer"   $
                                                          monomerAndTail `cutTopoAt`   findResId (j+1)
                              return $! makeFibril monomer n 4.8 0.0
                              -- TODO: extract shift&twist
  where
    -- TODO: tagError may be generic transformation from Maybe monad to Either monad! (Or `ifMissing`?)
    tagError :: String -> Maybe a -> Either String a
    tagError msg = maybe (Left msg) Right

-- | Checks that we are at given residue id.
findResId i torsion = tResId torsion == i

-- * Utilities for gluing chains
-- | Replaces OXT of first chain with the second chain.
-- TODO: move it to FragReplacement.
glueCartesianChain :: CartesianTopo -> CartesianTopo -> CartesianTopo
topo1 `glueCartesianChain` topo2 = traceShow ("glueCartesianChain",
                                              length $ backbone topo1 ,
                                              length $ backbone topo2 ,
                                              length $ backbone result)
                                             result
  where
    Just result = topo1 `changeTopoAt` (\t -> cAtName t == "OXT") $ const topo2

-- | Prepare fragment set by removing those that span the boundary between
-- monomer and linker.
delimitFragSet :: Fibril -> RFragSet -> RFragSet
delimitFragSet fibril = RFragSet . V.filter criterion . unRFragSet
  where
    notOnBoundary :: RFrag -> Bool
    notOnBoundary f = endPos f <= monomerLen fibril
    criterion :: V.Vector RFrag -> Bool
    criterion = notOnBoundary . V.head

-- | Sampling of a polymer with random fragment.
sampleFibrilMonomer fragSet fibril gen = (fibrilXchgMono, gen')
  where
   fibrilXchgMono = fibril { monomer = xchg $ monomer fibril }
   -- TODO: change fromMaybe to fromJust, after cutting fragment set at right place
   xchg topo = fromJust $ replaceFragment pos frag topo
   ((_pos, frag), gen') = randomF fragSet gen
   pos = startPos frag
   assertions = assert $ pos <= monomerLen fibril

-- | Stationary process along the given mean, with given standard deviation (for sampling shift and twist.)
transition :: (RandomGen r) => Double -- ^ target (base) value
                            -> Double -- ^ standard deviation of a single sampling
                            -> Double -- ^ current (starting) value 
                            -> r      -- ^ random number generator
                            -> (Double, r)
transition mean stdev current gen = normal ((mean + current)/2.0) stdev gen

-- NOTE: Standard deviation at starting temperature of 1.0.
baseTwist    = 0
baseTwistDev = 30 -- +-30 degrees
baseShift    = 4.8
baseShiftDev = 0.8 -- 0.8 Angstroem

-- current * exp (-theta * t) + mean (1-exp(-theta*t) + stdev/sqrt (2

-- | Sampling method to be applied to whole FibrilModel.
-- 
-- TODO: Sampling of twist and shift should depend on temperature?
sampleFibril :: RandomGen t => Double   -- ^ probability of the shift transition.
                            -> Double   -- ^ probability of the twist transition.
                            -> RFragSet -- ^ fragment set for replacement transition.
                            -> Fibril   -- ^ input Fibril.
                            -> t        -- ^ random number generator
                            -> (Fibril, t)
sampleFibril shiftProb twistProb fragSet aFibril gen = assertions
                                                         (newFibril, gen'')
  where
    -- Pick type of MC move.
    (shiftOrTwist, gen')   = randomR (0.0, 1.0) gen
    moveFragment           = shiftOrTwist >= shiftProb + twistProb
    moveShift              = shiftOrTwist <= shiftProb
    moveTwist              = shiftOrTwist >  shiftProb && shiftOrTwist <= twistProb
    -- Twist sampling
    (newTwist, gen''twist) = transition baseTwist baseTwistDev (twist aFibril) gen'
    -- Shift sampling
    (newShift, gen''shift) = transition baseShift baseShiftDev (shift aFibril) gen'
    -- Perform MC move.
    (newFibril, gen'') =
      if moveShift
        then       (aFibril { shift = newShift }, gen''shift) -- TODO: implement
        else
          if moveTwist
            then   (aFibril { twist = newTwist }, gen''twist) -- TODO: implement
            else assert moveFragment $
                   sampleFibrilMonomer fragSet aFibril gen'
    -- Check input parameters
    assertions   = assert $ ((shiftProb + twistProb              < 1.0) &&
                             (shiftProb                          > 0.0) &&
                             (twistProb                          > 0.0) &&
                             (moveShift /= (moveTwist /= moveFragment)) &&
                             (moveFragment <= not moveShift           ))

-- | Sampling function for a fibril model, either changes shift&twist parameters,
-- or exchanges a fragment within a monomer.
sampleFibrilModel :: RandomGen t => Double      -- ^ Probability of the shift transition.
                                 -> Double      -- ^ Probability of the twist transition.
                                 -> RFragSet    -- ^ Fragment set for replacement transition.
                                 -> FibrilModel -- ^ Input model.
                                 -> t           -- ^ Random number generator
                                 -> (FibrilModel, t)
sampleFibrilModel shiftProb twistProb fragSet fm gen = (makeFibrilModel newFibril, gen')
  where
    (newFibril, gen') = sampleFibril shiftProb twistProb fragSet (fibril fm) gen

