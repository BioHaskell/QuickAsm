-- | Generic class for handling different types of models.
module Model( Model            (..)
            , TorsionModel     (..)
            , initTorsionModel
            , modifyTorsionModelM ) where

import Control.DeepSeq(NFData(..))

import Topo

class Model m where
  torsionTopo   :: m -> TorsionTopo
  cartesianTopo :: m -> CartesianTopo

data TorsionModel = TModel { tTopo       :: TorsionTopo
                           , cTopo       :: CartesianTopo
                           }

instance Model TorsionModel where
  cartesianTopo = cTopo
  torsionTopo   = tTopo

instance NFData TorsionModel where
  rnf a = rnf tTopo `seq` rnf cTopo

modifyTorsionModelM :: (Monad m) => (TorsionTopo -> m TorsionTopo) -> TorsionModel -> m TorsionModel
modifyTorsionModelM topoFun tModel = do tTopo' <- topoFun $ tTopo tModel
                                        return $ TModel tTopo' $ computePositions tTopo'

initTorsionModel topo = TModel { tTopo = topo
                               , cTopo = computePositions topo
                               }
