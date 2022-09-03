module MergeOpt (optimize) where

import Codec.Picture

import ImageUtils
import Types

optimize :: Image PixelRGBA8 -> [Move] -> [Move]
optimize img = go 1
  where
    go _   [] = []
    go blk ms@(m1:rest) = case opt2Colors img blk ms of
                            Nothing -> m1 : go blk rest
                            Just ms' -> go (blk + 1) ms'


opt2Colors :: Image PixelRGBA8 -> Int -> [Move] -> Maybe [Move]
opt2Colors img num (m1@(Color b1 c1) : m2@(Color b2 c2) : ms)
  | canMerge b1 b2 && mc m1 + mc m2 + diffCost img m1 + diffCost img m2 >
                      mc m1' + mc m2' + diffCost img m2'
       = Just (m1' : m2' : ms)
  where
    canvas = imageWidth img * imageHeight img
    mc = fromIntegral . moveCost canvas
    (Just newRect) = combine' b1 b2
    newBlk = Block (show num) newRect
    m1' = Merge b1 b2
    m2' = Color newBlk (mixColors c1 c2)
opt2Colors _ _ _ = Nothing

diffCost :: Image PixelRGBA8 -> Move -> Double
diffCost img (Color (Block _ (Rectangle x0 y0 x1 y1)) c) = 0.005 * totalError c img x0 y0 x1 y1
diffCost _ _ = 0

canMerge :: Block -> Block -> Bool
canMerge b1 b2 = case combine' b1 b2 of
                   Nothing -> False
                   _ -> True

combine' :: Block -> Block -> Maybe Rectangle
combine' (Block _ r1) (Block _ r2) = merge r1 r2
