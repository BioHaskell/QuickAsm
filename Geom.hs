module Geom(vperpend, vproj) where

import Data.Vector.V3
import Data.Vector.Class

-- ^ Module with methods dealing exclusively with vector and angle geometry
import Angle

-- | Component of v that is perpendicular to w.
v `vperpend` w = v - (v `vproj` w)

-- | Component of v that is parallel to w.
v `vproj`    w = ((v `vdot` w)/(w `vdot` w)) *| w

