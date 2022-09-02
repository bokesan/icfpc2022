module ImageUtils (getPixel, pixelDistance, averageColor, totalError) where

import Codec.Picture
import Data.Word

getPixel :: Image PixelRGBA8 -> Int -> Int -> PixelRGBA8
getPixel img x y = pixelAt img x (imageHeight img - (y + 1))

pixelDistance :: PixelRGBA8 -> PixelRGBA8 -> Double
pixelDistance (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
    sqrt (sqd r1 r2 + sqd g1 g2 + sqd b1 b2 + sqd a1 a2)
  where
    sqd :: Word8 -> Word8 -> Double
    sqd m n = let v = fromIntegral m - fromIntegral n in v * v

averageColor :: Image PixelRGBA8 -> Int -> Int -> Int -> Int -> PixelRGBA8
averageColor image x0 y0 x1 y1 = go x0 y0 0 0 0 0
  where
    go :: Int -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> PixelRGBA8
    go x y r g b a | y == y1 =
                        let n = fromIntegral ((x1 - x0) * (y1 - y0)) in
                        if n == 0 then error ("Zero: " ++ show x0 ++ ", " ++ show y0 ++ ", " ++ show x1 ++ ", " ++ show y1)
                        else
                        PixelRGBA8 (fromIntegral (r `quot` n))
                                   (fromIntegral (g `quot` n))
                                   (fromIntegral (b `quot` n))
                                   (fromIntegral (a `quot` n))
                   | x == x1 =
                        go x0 (y + 1) r g b a
                   | otherwise =
                        case getPixel image x y of
                          (PixelRGBA8 r' g' b' a') -> go (x + 1) y (r + fromIntegral r')
                                                                   (g + fromIntegral g')
                                                                   (b + fromIntegral b')
                                                                   (a + fromIntegral a')

totalError :: PixelRGBA8 -> Image PixelRGBA8 -> Int -> Int -> Int -> Int -> Double
totalError color image x0 y0 x1 y1 = go x0 y0 0.0
  where
    go x y error | y == y1   = error
                 | x == x1   = go x0 (y + 1) error
                 | otherwise = go (x + 1) y (error + pixelDistance color (getPixel image x y))
