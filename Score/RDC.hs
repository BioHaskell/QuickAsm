-- | Using RDC values to restraint intervector angles within a molecule.
module Score.RDC where

import Rosetta.RDC

import Score.ScoringFunction

-- TODO: implement score by RDC restraints
data RDCRestraintSet = NotYetDefined

data RDCParams = RDCParams { dxx, dyy, dzz :: Double }

-- * Preparing RDCs to be evaluated for a given model.
-- | Prepares RDC restraints for scoring:
-- 1. First computes axial and rhombic parameters from distribution.
-- 2. Then computes intervector projection angle limits epsilon1,
-- and epsilon2 for each pair of RDC restraints.
-- 3. Then creates two lists sorted by residue number, so that they can
-- be evaluated quickly. (Just like we do for distance restraints.)
prepareRDCRestraints rdcSet rdcParams topo = undefined

rdcInterprojection :: Double-> Double-> Double-> RDCRestraint -> RDCRestraint-> [Double]
rdcInterprojection dxx dyy dzz rdc1 rdc2 = [phiVal a1min a2min True  True
                                           ,phiVal a1min a2min True  False
                                           ,phiVal a1min a2min False True
                                           ,phiVal a1min a2min False False
                                           ,phiVal a1max a2min True  True
                                           ,phiVal a1max a2min True  False
                                           ,phiVal a1max a2min False True
                                           ,phiVal a1max a2min False False
                                           ,phiVal a1min a2max True  True
                                           ,phiVal a1min a2max True  False
                                           ,phiVal a1min a2max False True
                                           ,phiVal a1min a2max False False
                                           ,phiVal a1max a2max True  True
                                           ,phiVal a1max a2max True  False
                                           ,phiVal a1max a2max False True
                                           ,phiVal a1max a2max False False]
  where
    underRoot rdcValue alphaValue = 2*(3*rdcValue - numTens)/3/(3/2*denomTens*cos alphaValue - numTens)
    firstRoot  rv av = sqrt $     underRoot rv av
    secondRoot rv av = sqrt $ 1 - underRoot rv av
    phiVal alpha1 alpha2 sign1 sign2 = firstRoot rdcv1 alpha1 * firstRoot rdcv2 alpha2 * 
                                         cos (alpha1 + mkSign sign1 * alpha2) + mkSign sign2 *
                                         secondRoot rdcv1 alpha1 * secondRoot rdcv2 alpha2
    rdcv1 = rdcValue rdc1
    rdcv2 = rdcValue rdc2
    mkSign True  =   1
    mkSign False = (-1)
    (a1min, a1max) = alphaBounds rdcv1
    (a2min, a2max) = alphaBounds rdcv2
    numTens   = 2*dzz - dxx - dyy -- commonly appearing in numerator
    denomTens = dxx - dyy -- commonly appearing in denominator
    alphaBounds :: Double -> (Double, Double)
    alphaBounds rdc = if abs ((6*rdc + numTens) /
                                       denomTens) <= 1.0
                        then (angle,
                              pi - angle)
                        else (0, pi)
      where
        angle = 1/2*acos (6*rdc + 2*dzz - dxx - dyy)/3/(dxx - dyy)

-- | Computes axial and rhombic parameters of RDC restraint set from distribution.
rdcEstimate = undefined

-- | Compute intervector projection and angle limits.
rdcIntervectorLimits tensorParameters rdc1 rdc2 = undefined

-- | Factor out restraint sorting from Score.DistanceRestraints.
rdcSortRestraints = undefined

-- | Evaluates an RDC restraint set.
computeRDCs rdcSet model = undefined

-- | ScoringFunction for RDC restraint set.
scoreRDC = undefined

computeRDC rdcRestraint rdcTensor = undefined
