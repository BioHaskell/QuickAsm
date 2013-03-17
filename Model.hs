module Model(Model(..), TorsionModel(..)) where

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

modifyTorsionModel topoFun tModel = TModel tTopo' cTopo'
  where
    tTopo'      = topoFun $ tTopo tModel
    cTopo'      = computePositions tTopo'


