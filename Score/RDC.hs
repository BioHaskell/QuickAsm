-- | Using RDC values to restraint intervector angles within a molecule.
module Score.RDC where

import Score.ScoringFunction

-- TODO: implement score by RDC restraints
data RDCRestraintSet

-- * Preparing RDCs to be evaluated for a given model.
-- | Prepares RDC restraints for scoring:
-- 1. First computes axial and rhombic parameters from distribution.
-- 2. Then computes intervector projection angle limits epsilon1, and epsilon2
-- for each pair of RDC restraints.
-- 3. Then creates two lists sorted by residue number, so that they can be evaluated quickly.
-- (Just like we do for distance restraints.)
prepareRDCRestraints rdcSet topo = undefined

-- | Computes axial and rhombic parameters of RDC restraint set from distribution.
rdcEstimate = undefined

-- | Compute intervector projection and angle limits.
rdcIntervectorLimits tensorParameters rdc1 rdc2 = undefined

-- | Factor out restraint sorting from Score.DistanceRestraints.
rdcSortRestraints = undefined

-- | Evaluates an RDC restraint set.
computeRDC rdcSet model = undefined

-- | ScoringFunction for RDC restraint set.
scoreRDC = undefined
