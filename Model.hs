{-# LANGUAGE CPP #-}
-- | Generic class for handling different types of models.
module Model( Model            (..)
            , TorsionModel     (..)
            , initTorsionModel
            , modifyTorsionModelM ) where

import Control.DeepSeq(NFData(..))
import Debug.Trace(traceShow) -- DEBUG

import Topo

#ifdef OLD_BYTESTRING
instance NFData BS.ByteString where
#endif

-- | Represents a model that can be instantiated as Torsion topology or
-- Cartesian topology.
class Model m where
  torsionTopo   :: m -> TorsionTopo
  cartesianTopo :: m -> CartesianTopo

-- | Model based on Torsion topology, with cached Cartesian positions.
data TorsionModel = TModel { tTopo       :: TorsionTopo
                           , cTopo       :: CartesianTopo
                           }

instance Model TorsionModel where
  cartesianTopo m = debugging $ cTopo m
    where
      debugging = traceShow ("TorsionModel::cartesianTopo",
                             length $ filter ((=="CA") . cAtName) $ backbone $ cTopo m)
  torsionTopo     = tTopo

instance NFData TorsionModel where
  rnf a = rnf tTopo `seq` rnf cTopo

-- | Given a monadic action on Torsion topology, lifts it into monadic
-- action on TorsionModel.
modifyTorsionModelM :: (Monad m) => (TorsionTopo -> m TorsionTopo) -> TorsionModel -> m TorsionModel
modifyTorsionModelM topoFun tModel = do tTopo' <- topoFun $ tTopo tModel
                                        return $ TModel tTopo' $ computePositions tTopo'

-- | Creates TorsionModel for a given Torsion topology.
initTorsionModel topo = TModel { tTopo = topo
                               , cTopo = computePositions topo
                               }

