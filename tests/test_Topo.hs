#!/home/m/bin/runghc
module Main where

import Data.Vector.V3
import Topo
import Numeric(showFFloat)

exampleTorsions = [("GLY",    0.000, 177.618, 176.418),
                   ("ARG", -128.131, 136.491, 178.184),
                   ("ASN", -102.727, 101.438, 179.424),
                   ("SER",  -99.776, 143.352, 176.874),
                   ("ALA", -145.088, 147.030, 175.930)]

sourceTriples ((_, a, b, c):rs) = a:b:c:sourceTriples rs
sourceTriples []                = []

showFloatTriples :: (RealFloat f) => [f] -> String
showFloatTriples = join '\n'. map (join ' ') . triples . map ffmt
  where
    join c = foldl1 $ join' c
    join' c a b = a ++ c:b

ffmt :: (RealFloat f) => f -> String
ffmt f = adjust 9 $ showFFloat (Just 3) f ""
  where
    adjust i l = iterate (' ':) l !! (i - length l)

triples (a:b:c:ds) = [a, b, c]:triples ds
triples []         = []
triples ds         = [ds] -- for 1 or 2 remaining

exampleCoords = [Vector3  0.000   0.000   0.000 ,
                 Vector3  1.458   0.000   0.000 ,
                 Vector3  2.009   1.420   0.000 ,
                 Vector3  3.332   1.536   0.050 ,
                 Vector3  3.990   2.835 (-0.022),
                 Vector3  5.069   2.847 (-1.097),
                 Vector3  5.113   3.925 (-1.872),
                 Vector3  6.122   4.080 (-2.913),
                 Vector3  7.224   5.036 (-2.474),
                 Vector3  8.357   4.476 (-2.064),
                 Vector3  9.494   5.279 (-1.630),
                 Vector3 10.535   5.403 (-2.735)]

tree = constructBackbone exampleTorsions

main = do return () --print $ tree
          let cartopo = computePositions tree
          putStrLn $ showCartesianTopo $ cartopo
          let retors = computeTorsions cartopo
          putStrLn "Input dihedrals:"
          putStrLn $ showFloatTriples $ sourceTriples $ exampleTorsions
          putStrLn "generated dihedrals:"
          print $ triples $ backboneDihedrals retors
          putStrLn $ showFloatTriples $ tail $ backboneDihedrals retors
          putStrLn $ showFloatTriples $ backbonePlanars   retors
