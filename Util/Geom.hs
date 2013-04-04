-- | Utility basic geometric operations on Data.Vector.V3.Vector3.
module Util.Geom(vperpend, vproj) where

-- TODO: this should go vector library?

import Data.Vector.V3
import Data.Vector.Class

-- ^ Module with methods dealing exclusively with vector and angle geometry

-- NOTE: I have tried to submit this as a patch to AC-Vector...
-- If AC-Vector doesn't become compatible with Tensor (which is basis of OpenGL)
-- it may be good to put these methods and vdot, vcross, etc. into a Tensor-Algebra package

-- | Component of v that is perpendicular to w.
v `vperpend` w = v - (v `vproj` w)

-- | Component of v that is parallel to w.
v `vproj`    w = ((v `vdot` w)/(w `vdot` w)) *| w

