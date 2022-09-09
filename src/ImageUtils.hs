module ImageUtils (getPixel, pixelDistance, averageColor, averageColor',
                   totalError, totalError', totalError'',
                   similarity, componentHistogram, bestColor, bestColor') where

import Codec.Picture
import Codec.Picture.Types
import Data.List (maximumBy)
import Data.Bits
import Data.Word
import qualified Data.IntMap.Strict as IM

import Types

getPixel :: Image PixelRGBA8 -> Int -> Int -> PixelRGBA8
getPixel img x y = pixelAt img x (imageHeight img - (y + 1))

pixelDistance :: PixelRGBA8 -> PixelRGBA8 -> Double
pixelDistance (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
    sqrt (sqd r1 r2 + sqd g1 g2 + sqd b1 b2 + sqd a1 a2)
  where
    sqd :: Word8 -> Word8 -> Double
    sqd m n = let v = fromIntegral m - fromIntegral n in v * v

colorToInt :: PixelRGBA8 -> Int
colorToInt (PixelRGBA8 r g b _) = shiftL (fromIntegral r) 16 + shiftL (fromIntegral g) 8 + fromIntegral b

intToColor :: Int -> PixelRGBA8
intToColor n = PixelRGBA8 (fromIntegral (shiftR n 16)) (fromIntegral (shiftR n 8)) (fromIntegral n) 255

data ChannelSums = Sums !Word32 !Word32 !Word32 !Word32

addPixel :: PixelRGBA8 -> ChannelSums -> ChannelSums
addPixel (PixelRGBA8 r g b a) (Sums r' g' b' a') = Sums (r' + fromIntegral r)
                                                        (g' + fromIntegral g)
                                                        (b' + fromIntegral b)
                                                        (a' + fromIntegral a)

rowChannelSums :: Image PixelRGBA8 -> Int -> Int -> Int -> ChannelSums -> ChannelSums
rowChannelSums img y x0 x1 sums = go baseIndex sums
  where
    baseIndex = pixelBaseIndex img x0 (imageHeight img - (y + 1))
    endIndex = baseIndex + (4 * (x1 - x0))
    dat = imageData img
    go i s | i == endIndex = s
           | otherwise     = go (i + 4) (addPixel (unsafePixelAt dat i) s)

-- Total score: 1752282
-- Magic1 3246.4270215293704 - 17429.476346333653
-- Magic2 5.815084943134368 - 87.20739974919016
bestColor' :: Image PixelRGBA8 -> Rectangle -> PixelRGBA8
bestColor' image (Rectangle x0 y0 x1 y1) = bestColor image x0 y0 x1 y1

bestColor :: Image PixelRGBA8 -> Int -> Int -> Int -> Int -> PixelRGBA8
bestColor image x0 y0 x1 y1 = intToColor (fst (maximumBy (\(_,a) (_,b) -> compare a b) (IM.toList histo)))
  where
    -- TODO: take best from existing
    histo = go IM.empty x0 y0
    go m x y | y == y1 = m
             | x == x1 = go m x0 (y + 1)
             | otherwise = let c = getPixel image x y
                               m' = IM.insertWith (+) (colorToInt c) 1 m
                           in go m' (x + 1) y

averageColor' :: Image PixelRGBA8 -> Rectangle -> PixelRGBA8
averageColor' image (Rectangle x0 y0 x1 y1) = averageColor image x0 y0 x1 y1

averageColor :: Image PixelRGBA8 -> Int -> Int -> Int -> Int -> PixelRGBA8
averageColor image x0 y0 x1 y1 = go y0 (Sums 0 0 0 0)
  where
    go :: Int -> ChannelSums -> PixelRGBA8
    go y s@(Sums r g b a) | y == y1 =
                        let n = fromIntegral ((x1 - x0) * (y1 - y0)) in
                        if n == 0 then error ("Zero: " ++ show x0 ++ ", " ++ show y0 ++ ", " ++ show x1 ++ ", " ++ show y1)
                        else
                        PixelRGBA8 (fromIntegral (r `quot` n))
                                   (fromIntegral (g `quot` n))
                                   (fromIntegral (b `quot` n))
                                   (fromIntegral (a `quot` n))
                   | otherwise =
                        go (y + 1) (rowChannelSums image y x0 x1 s)

totalError'' :: Image PixelRGBA8 -> Rectangle -> Double
totalError'' image rect = totalError' (averageColor' image rect) image rect

totalError' :: PixelRGBA8 -> Image PixelRGBA8 -> Rectangle -> Double
totalError' color image (Rectangle x0 y0 x1 y1) = totalError color image x0 y0 x1 y1

totalError :: PixelRGBA8 -> Image PixelRGBA8 -> Int -> Int -> Int -> Int -> Double
totalError color image x0 y0 x1 y1 = go y0 0.0
  where
    go y err | y == y1   = err
             | otherwise = go (y + 1) (rowErrors color image y x0 x1 err)

rowErrors :: PixelRGBA8 -> Image PixelRGBA8 -> Int -> Int -> Int -> Double -> Double
rowErrors color img y x0 x1 = go baseIndex
  where
    baseIndex = pixelBaseIndex img x0 (imageHeight img - (y + 1))
    endIndex = baseIndex + (4 * (x1 - x0))
    dat = imageData img
    go i s | i == endIndex = s
           | otherwise     = go (i + 4) (s + pixelDistance color (unsafePixelAt dat i))


similarity :: Image PixelRGBA8 -> Image PixelRGBA8 -> Int
similarity a b = if w1 /= w2 || h1 /= h2
                   then error "invalid result dimesions"
                   else go 0 0 0.0
   where
      w1 = imageWidth a
      w2 = imageWidth b
      h1 = imageHeight a
      h2 = imageHeight b
      go x y diff | y == h1 = roundJS (diff * 0.005)
                  | x == w1 = go 0 (y + 1) diff
                  | otherwise = go (x + 1) y (diff + pixelDistance (pixelAt a x y) (pixelAt b x y))

componentHistogram :: (PixelRGBA8 -> Word8) -> Image PixelRGBA8 -> [(Word8, Int)]
componentHistogram component img = [(fromIntegral k, v) | (k,v) <- IM.toList histo]
  where
    histo = pixelFold accum IM.empty img
    accum m _ _ v = IM.insertWith (+) (fromIntegral (component v)) 1 m
