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
  [ testCase "problemId with path"    $ problemId "/path/to/35.png" @?= 35
  , testCase "problemId without path" $ problemId "36.initial.json" @?= 36
  , testCase "problemId at root"      $ problemId "/17.out.png"     @?= 17
  , testCase "mixColors" $
      mixColors (PixelRGBA8 20 40 60 80) (PixelRGBA8 10 20 30 6) @?= (PixelRGBA8 15 30 45 43)
  , testCase "parse blockId" $
      parseBlockId "12.34.56" @?= [12, 34, 56]
  , testCase "round to even 1" $ round 2.5 @?= 2
  , testCase "round to even 2" $ round 3.5 @?= 4
  , testCase "roundJS 1" $ roundJS 2.5 @?= 3
  , testCase "roundJS 2" $ roundJS 3.5 @?= 4
  , testCase "splitV" $
      splitV (Rectangle 0 0 10 10) 5 @?= (Rectangle 0 0 5 10, Rectangle 5 0 10 10)
  , testCase "splitH" $
      splitH (Rectangle 0 0 10 10) 4 @?= (Rectangle 0 0 10 4, Rectangle 0 4 10 10)
  , testCase "splitP" $
      splitP (Rectangle 0 0 4 4) (2,2) @?= ( Rectangle 0 0 2 2, Rectangle 2 0 4 2,
                                             Rectangle 2 2 4 4, Rectangle 0 2 2 4 )
  , testCase "parse configuration" $
      decode "{\"width\":400,\"height\":400,\"blocks\":[{\"blockId\":\"0\", \"bottomLeft\":[0,0],\"topRight\":[400,400],\"color\":[255,255,255,255]}]}"
       @?= Just lightningConfig
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
