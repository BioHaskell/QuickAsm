#!/home/m/bin/runghc
module Main where

import Data.Vector.V3
import Data.Vector.Class
import Topo
import Numeric(showFFloat)

exampleTorsions = [("GLY",    0.000, 177.618, 176.418),
                   ("ARG", -128.131, 136.491, 178.184),
                   ("ASN", -102.727, 101.438, 179.424),
                   ("SER",  -99.776, 143.352, 176.874),
                   ("ALA", -145.088, 147.030, 175.930)]

sourceTriples ((_, a, b, c):rs) = a:b:c:sourceTriples rs
sourceTriples []                = []

showVector (Vector3 x y z) = concatMap showFloat [x, y, z]

showVectors = join '\n' . map showVector

showFloatTriples :: (RealFloat f) => [f] -> String
showFloatTriples = join '\n'. map (join ' ') . triples . map showFloat

join c = foldl1 $ join' c
join' c a b = a ++ c:b

showFloat :: (RealFloat f) => f -> String
showFloat f = adjust 9 $ showFFloat (Just 3) f ""
  where
    adjust i l = iterate (' ':) l !! (i - length l)

triples (a:b:c:ds) = [a, b, c]:triples ds
triples []         = []
triples ds         = [ds] -- for 1 or 2 remaining

quadruples (a:b:c:d:es) = [a, b, c, d]:quadruples es
quadruples []           = []
quadruples es           = [es] -- for 1 or 2 remaining

exampleDecoy = [("N",  "VAL",   1, 0.0,    0.0,    0.000),
                ("CA", "VAL",   1, 1.458,  0.0,    0.000),
                ("C",  "VAL",   1, 2.009,  1.42,   0.000),
                ("O",  "VAL",   1, 1.352,  2.351, -0.467),
                ("N",  "LEU",   2, 3.218,  1.58,   0.526),
                ("CA", "LEU",   2, 3.48,   2.578,  1.557),
                ("C",  "LEU",   2, 4.894,  3.132,  1.441),
                ("O",  "LEU",   2, 5.824,  2.414,  1.074),
                ("N",  "TYR",   3, 5.05,   4.413,  1.758),
                ("CA", "TYR",   3, 5.995,  5.267,  1.049),
                ("C",  "TYR",   3, 6.482,  6.405,  1.938),
                ("O",  "TYR",   3, 6.197,  6.436,  3.135),
                ("N",  "VAL",   4, 7.219,  7.338,  1.345),
                ("CA", "VAL",   4, 8.556,  7.052,  0.840),
                ("C",  "VAL",   4, 9.571,  6.988,  1.974),
                ("O",  "VAL",   4, 9.209,  6.783,  3.133),
                ("N",  "GLY",   5, 10.843, 7.164,  1.633),
                ("CA", "GLY",   5, 11.93,  6.904,  2.570),
                ("C",  "GLY",   5, 12.207, 8.12,   3.444),
                ("O",  "GLY",   5, 11.481, 8.387,  4.401),
                ("N",  "SER",   6, 13.261, 8.856,  3.109),
                ("CA", "SER",   6, 14.241, 9.287,  4.099),
                ("C",  "SER",   6, 15.042, 10.481, 3.597),
                ("O",  "SER",   6, 14.695, 11.631, 3.867),
                ("N",  "LYS",   7, 16.116, 10.202, 2.866),
                ("CA", "LYS",   7, 17.356, 10.956, 3.007),
                ("C",  "LYS",   7, 17.173, 12.404, 2.571),
                ("O",  "LYS",   7, 16.496, 12.683, 1.581),
                ("N",  "THR",   8, 17.779, 13.322, 3.315),
                ("CA", "THR",   8, 18.001, 14.679, 2.829),
                ("C",  "THR",   8, 19.363, 15.204, 3.263),
                ("O",  "THR",   8, 19.713, 15.151, 4.442)]

backboneAtomNamesT = map tAtName . backbone
backboneAtomNamesC = map cAtName . backbone

every :: Int -> [a] -> [a] 
every n l = head l:drop n l

exampleDecoy' = zipWith (,) (map getResName $ every 4 exampleDecoy) (quadruples exampleCoords)
  where
    getResName (_, resName, _, _, _, _) = resName

exampleCoords = map mkVec exampleDecoy
  where
    mkVec (at, res, num, x, y, z) = Vector3 x y z

tree      = constructBackbone exampleTorsions

origCoord = constructCartesianBackbone exampleDecoy'

main = do return () --print $ tree
          let cartopo = computePositions tree
          putStrLn $ showCartesianTopo $ cartopo
          let retors = computeTorsions cartopo
          putStrLn   "Input dihedrals:"
          putStrLn $ showFloatTriples $ sourceTriples $ exampleTorsions
          putStrLn   "Generated dihedrals:"
          --print $ triples $ backboneDihedrals retors
          putStrLn $ showFloatTriples $ tail $ backboneDihedrals retors
          putStrLn   "Generated planar angles:"
          putStrLn $ showFloatTriples $ backbonePlanars retors
          putStrLn   "Original  planar angles:"
          putStrLn $ showFloatTriples $ backbonePlanars $ computeTorsions origCoord
          putStrLn $ "Generated backbone atoms:" ++ show (backboneAtomNamesT retors)
          putStrLn $ "Original  backbone atoms:" ++ show (backboneAtomNamesC origCoord)
          putStrLn   "Source bond vectors:"
          putStrLn $ showVectors $ bondVectors' exampleCoords
          putStrLn   "Generated bond vectors:"
          putStrLn $ showVectors $ bondDirs  cartopo
          putStrLn   "Source bond directions:"
          putStrLn $ showVectors $ bondVectors' exampleCoords
          putStrLn   "Generated bond directions:"
          putStrLn $ showVectors $ bondDirs  cartopo

vnorm v = v |* recip (vmag v)

bondDirs        = map vnormalise . bondVectors

bondVectors     = bondVectors' . map cPos . backbone

bondVectors' bb = zipWith (-) (tail bb) bb

