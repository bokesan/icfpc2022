import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Codec.Picture
import Data.List
import Data.Ord

import Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [{- scProps, qcProps -}]

makeColor = PixelRGBA8

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
  [
    testCase "mixColors" $
      mixColors (PixelRGBA8 20 40 60 80) (PixelRGBA8 10 20 30 6) @?= (PixelRGBA8 15 30 45 43)
  , testCase "format line cut" $
      show (LineCut (Block "1.0.2" undefined) Vertical 13) @?= "cut [1.0.2] [X] [13]"
  , testCase "format point cut" $
      show (PointCut (Block "2.1" undefined) 12 15) @?= "cut [2.1] [12,15]"
  , testCase "format color" $
      show (Color (Block "3" undefined) (makeColor 1 2 3 4)) @?= "color [3] [1,2,3,4]"
  , testCase "format swap" $
      show (Swap (Block "4.0" undefined) (Block "2" undefined)) @?= "swap [4.0] [2]"
  , testCase "format merge" $
      show (Merge (Block "4.1" undefined) (Block "2.2" undefined)) @?= "merge [4.1] [2.2]"
  ]
