module P1Solver (solve) where

import Codec.Picture

import ImageUtils
import Types

errAcceptable :: Double -> Int -> Bool
errAcceptable err numBlocks = (err / fromIntegral numBlocks) <= 10


solve :: Int -> Image PixelRGBA8 -> RGBA -> BlockId -> [Move]
solve blockSize img color blockId = moves
  where
    moves = divide img color blockId blockSize 0 0 (imageWidth img) (imageHeight img)


divide :: Image PixelRGBA8 -> RGBA -> BlockId -> Int -> Int -> Int -> Int -> Int -> [Move]
divide img color blockId sz x0 y0 x1 y1
    | w == sz && h == sz && average == color = []
    | w == sz && h == sz = [Color blk average]
    | errAcceptable err (numBlocks sz x0 y0 x1 y1) = [Color blk average]
    | w > sz  && h == sz =   divideFlat
    | w == sz && h > sz  =   divideTall
    | w > sz  && h > sz  =   divideRect
    | otherwise = error "not evenly divisible"
  where
    blk = Block blockId (Rectangle x0 y0 x1 y1)
    w = x1 - x0
    h = y1 - y0
    average = averageColor img (x0 + 5) (y0 + 5) (x1 - 5) (y1 - 5)
    err = 0.005 * totalError average img x0 y0 x1 y1
    err' = 0.005 * totalError color img x0 y0 x1 y1
    divideFlat = let x = midPoint sz x0 x1 in
                 (LineCut blk Vertical x) :
                  divide img color (blockId ++ ".0") sz x0 y0 x y1 ++
                  divide img color (blockId ++ ".1") sz x y0 x1 y1
    divideTall = let y = midPoint sz y0 y1 in
                 (LineCut blk Horizontal y) :
                  divide img color (blockId ++ ".0") sz x0 y0 x1 y ++
                  divide img color (blockId ++ ".1") sz x0 y x1 y1
    divideRect = let x = midPoint sz x0 x1
                     y = midPoint sz y0 y1 in
                 (PointCut blk x y) :
                  divide img color (blockId ++ ".0") sz x0 y0 x  y  ++
                  divide img color (blockId ++ ".1") sz x  y0 x1 y  ++
                  divide img color (blockId ++ ".2") sz x  y  x1 y1 ++
                  divide img color (blockId ++ ".3") sz x0 y  x  y1
    
numBlocks :: Int -> Int -> Int -> Int -> Int -> Int
numBlocks sz x0 y0 x1 y1 = ((x1 - x0) `quot` sz) * ((y1 - y0) `quot` sz)

midPoint :: Int -> Int -> Int -> Int
midPoint sz a b = let midBlock = ((b - a) `quot` sz) `quot` 2
                  in a + sz * midBlock
