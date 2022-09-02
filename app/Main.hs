module Main (main) where

import Codec.Picture
import Data.List
import System.Environment

import qualified QuadTree
import ImageUtils (pixelDistance)

maxError :: Double
maxError = -- 250000.0
           50000.0
           
main :: IO ()
main = do args <- getArgs
          mapM_ solveProblem args

solveProblem :: String -> IO ()
solveProblem path = do img' <- readImage path
                       case img' of
                         Left err -> putStrLn (path ++ ": error: " ++ err)
                         Right img -> writeSolution path (convertRGBA8 img)

writeSolution :: String -> Image PixelRGBA8 -> IO ()
writeSolution path img = do
  let tree = QuadTree.create maxError img
  let code = QuadTree.encode tree "0"
  let prog = concat $ intersperse "\n" (map show code)
  writeFile (path ++ ".txt") prog
  let img' = QuadTree.createImage (imageWidth img) (imageHeight img) tree
  putStrLn (path ++ " cost: " ++ show (QuadTree.cost (canvasSize img) tree)
             ++ ", similarity: " ++ show (similarity img img'))
  writePng (path ++ ".out.png") img'

canvasSize :: Image PixelRGBA8 -> Double
canvasSize img = fromIntegral (imageWidth img * imageHeight img)

similarity :: Image PixelRGBA8 -> Image PixelRGBA8 -> Int
similarity a b = if w1 /= w2 || h1 /= h2
                   then error "invalid result dimesions"
                   else go 0 0 0.0
   where
      w1 = imageWidth a
      w2 = imageWidth b
      h1 = imageHeight a
      h2 = imageHeight b
      go x y diff | y == h1 = round (diff * 0.005)
                  | x == w1 = go 0 (y + 1) diff
                  | otherwise = go (x + 1) y (diff + pixelDistance (pixelAt a x y) (pixelAt b x y))