import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [{- scProps, qcProps -}]


qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

unitTests = testGroup "Unit tests"
  [ testCase "format line cut" $
      show (LineCut "1.0.2" Vertical 13) @?= "cut [1.0.2] [X] [13]"
  , testCase "format point cut" $
      show (PointCut "2.1" 12 15) @?= "cut [2.1] [12,15]"
  , testCase "format color" $
      show (Color "3" (makeColor 1 2 3 4)) @?= "color [3] [1,2,3,4]"
  , testCase "format swap" $
      show (Swap "4.0" "2") @?= "swap [4.0] [2]"
  , testCase "format merge" $
      show (Merge "4.1" "2.2") @?= "merge [4.1] [2.2]"
  ]
