module Main (main) where

import Codec.Picture
import Data.List
import System.Environment

import qualified QuadTree
import ImageUtils (pixelDistance)

startingMaxError :: Double
startingMaxError = 100000
           
main :: IO ()
main = do args <- getArgs
          mapM_ solveProblem args

solveProblem :: String -> IO ()
solveProblem path = do img' <- readImage path
                       case img' of
                         Left err -> putStrLn (path ++ ": error: " ++ err)
                         Right img -> writeSolution path (convertRGBA8 img)


solveWith :: Double -> Image PixelRGBA8 -> IO (QuadTree.QuadTree, Int, Int)
solveWith maxError img = do
  let tree = QuadTree.create maxError img
  let cost = QuadTree.cost (canvasSize img) tree
  let siml = similarity img (QuadTree.createImage (imageWidth img) (imageHeight img) tree)
  putStrLn ("  maxError " ++ show maxError ++ ": cost=" ++ show cost
            ++ ", similarity=" ++ show siml
            ++ ", total=" ++ show (cost + siml))
  return (tree, cost, siml)

optimize :: Image PixelRGBA8 -> IO Double
optimize img = do
    (_,cost1,siml1) <- solveWith startingMaxError img
    goUp (startingMaxError, cost1 + siml1)
  where
    goUp (err,score) = do  let next = up err
                           (_,cost,siml) <- solveWith next img
                           if cost + siml < score
                            then goUp (next, cost + siml)
                            else goDown (err, score)
    goDown (err,score) = do let next = down err
                            (_,cost,siml) <- solveWith next img
                            if cost + siml < score
                             then goDown (next, cost + siml)
                             else return err

up, down :: Double -> Double
up x = x * 1.05
down x = x * 0.97

writeSolution :: String -> Image PixelRGBA8 -> IO ()
writeSolution path img = do
  maxError <- optimize img
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