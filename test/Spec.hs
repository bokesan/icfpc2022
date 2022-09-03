{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Codec.Picture
import Data.Aeson
import Data.List
import Data.Ord

import Types
import Configuration

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
  , testCase "parse configuration" $
      decode "{\"width\":24,\"height\":30,\"blocks\":[{\"blockId\":\"9\", \"bottomLeft\":[1,2],\"topRight\":[5,3],\"color\":[5,6,7,8]}]}"
       @?= Just (Configuration { width = 24, height = 30, blocks = [ ConfBlock { blockId = "9", shape = Rectangle 1 2 5 3, color = PixelRGBA8 5 6 7 8 } ] })
  , testCase "format line cut" $
      show (LineCut (blk "1.0.2") Vertical 13) @?= "cut [1.0.2] [X] [13]"
  , testCase "format point cut" $
      show (PointCut (blk "2.1") 12 15) @?= "cut [2.1] [12,15]"
  , testCase "format color" $
      show (Color (blk "3") (makeColor 1 2 3 4)) @?= "color [3] [1,2,3,4]"
  , testCase "format swap" $
      show (Swap (blk "4.0") (blk "2")) @?= "swap [4.0] [2]"
  , testCase "format merge" $
      show (Merge (blk "4.1") (blk "2.2")) @?= "merge [4.1] [2.2]"
  ]

blk :: String -> Block
blk id1 = Block id1 (Rectangle 0 0 0 0)
