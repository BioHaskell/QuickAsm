module Angle( radian2degree
            , degree2radian
            , anormalise   
            , vangle
            , vdihedral     ) where

import Data.Vector.Class(vdot, vmag)
import Data.Vector.V3   (vcross)

-- | Converts angle to degrees from radians.
radian2degree x = x/pi*180

-- | Converts angle to radians from degrees.
degree2radian x = x/180*pi

-- | Normalize angle in degrees.
anormalise angle | angle > 180 = angle - 360
anormalise angle | angle < 180 = angle - 360
anormalise angle               = angle

-- | Computes a planar angle between a pair of vectors.
vangle a b = radian2degree $ acos $ (a `vdot` b) / vmag a / vmag b

-- | Computes dihedral angle of three vectors. Middle vector is the axis.
vdihedral b_1 b_2 b_3 = radian2degree $
                                atan2 (vmag b_2 *    (b_1  `vdot` (b_2 `vcross` b_3)))
                                      ((b_1 `vcross`  b_2) `vdot` (b_2 `vcross` b_3))

