module Main (main) where

import Codec.Picture
import Control.Monad
import Data.List (intersperse)
import System.Environment
import System.Random.Stateful

import Types
import qualified QuadTree
import ImageUtils (pixelDistance)
import qualified MergeOpt

main :: IO ()
main = do args <- getArgs
          scores <- mapM solveProblem args
          putStrLn ("Total score: " ++ show (sum scores))

solveProblem :: String -> IO Int
solveProblem path = do img' <- readImage path
                       case img' of
                         Left err ->  do putStrLn (path ++ ": error: " ++ err)
                                         return 10000000
                         Right img -> writeSolution path (convertRGBA8 img)


solveWith :: Double -> Image PixelRGBA8 -> IO (Int, QuadTree.QuadTree)
solveWith maxError img = do
  let tree = QuadTree.create3 maxError img
  let code = MergeOpt.optimize img $ QuadTree.encode (PixelRGBA8 255 255 255 255) tree "0"
  let canvasSize = imageWidth img * imageHeight img
  let cost = sum (map (moveCost canvasSize) code)
  let siml = similarity img (QuadTree.createImage (imageWidth img) (imageHeight img) tree)
  {-
  putStrLn ("  maxError " ++ show maxError ++ ": cost=" ++ show cost
            ++ ", similarity=" ++ show siml
            ++ ", total=" ++ show (cost + siml))  -}
  return (cost + siml, tree)

optimize :: String -> Image PixelRGBA8 -> IO (Int, QuadTree.QuadTree)
optimize path img = do
    maxErrs <- replicateM 20 (uniformRM (0.5 :: Double, 12 :: Double) globalStdGen)
    res <- mapM (\m -> do (s,t) <- solveWith m img; return (s,m,t)) maxErrs
    let (score, err, tree) = minimum res
    putStrLn (path ++ ": best err=" ++ show err ++ ", score=" ++ show score)
    return (score, tree)


writeSolution :: String -> Image PixelRGBA8 -> IO Int
writeSolution path img = do
  (cost, tree) <- Main.optimize path img
  let code = MergeOpt.optimize img $ QuadTree.encode (PixelRGBA8 255 255 255 255) tree "0"
  let prog = concat $ intersperse "\n" (map show code)
  writeFile (path ++ ".txt") prog
  let img' = QuadTree.createImage (imageWidth img) (imageHeight img) tree
  writePng (path ++ ".out.png") img'
  return cost

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