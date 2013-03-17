-- | Generic class for handling different types of models.
module Model( Model            (..)
            , TorsionModel     (..)
            , initTorsionModel
            , modifyTorsionModelM ) where

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

modifyTorsionModelM :: (Monad m) => (TorsionTopo -> m TorsionTopo) -> TorsionModel -> m TorsionModel
modifyTorsionModelM topoFun tModel = do tTopo' <- topoFun $ tTopo tModel
                                        return $ TModel tTopo' $ computePositions tTopo'

initTorsionModel topo = TModel { tTopo = topo
                               , cTopo = computePositions topo
                               }
