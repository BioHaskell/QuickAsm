-- | Ideal bond geometry: bond planar angles, bond lengths.
-- Values of angles are *essential* for ROSETTA compatibility.
module IdealGeomParams(idealGeom) where

{-# INLINE idealGeom #-}
-- | Returns ideal planar angle/bond length for each atom in protein backbone (with respect to previous atom.)
idealGeom "N"  = ( 116.2, 1.328)
idealGeom "CA" = ( 121.7, 1.458)
idealGeom "C"  = ( 111.2, 1.523)
-- NOTE: for backbone reconstruction to match ROSETTA's, one has to be extremely careful about above angles!
idealGeom "O"     = (-123.0, 1.231)
idealGeom "OXT"   = ( 120.0, 1.200) -- TODO: check, now bond length of "O", and angle of "N".
idealGeom ('C':_) = ( 111.2, 1.523) -- assume it is similar to CA-C bond.
idealGeom a       = error $ "Unknown bond geometry for atom " ++ show a

-- TODO: add sidechain chi angles and geometry from:
-- http://www.msg.ucsf.edu/local/programs/garlic/commands/dihedrals.html
