module Main (main) where

import Codec.Picture
import Control.Monad
import qualified Control.Monad.Parallel as PAR
import Data.Aeson
import Data.List (intersperse)
import System.Environment
import System.Directory
import System.Random.Stateful

import Types
import qualified QuadTree
import ImageUtils (pixelDistance)
import Configuration
import qualified MergeOpt

main :: IO ()
main = do args <- getArgs
          scores <- PAR.mapM solveProblem args
          putStrLn ("Total score: " ++ show (sum scores))

solveProblem :: String -> IO Int
solveProblem path = do img' <- readImage path
                       conf <- readInitialConfig path
                       case img' of
                         Left err ->  do putStrLn (path ++ ": error: " ++ err)
                                         return 10000000
                         Right img -> writeSolution path conf (convertRGBA8 img)

readInitialConfig :: String -> IO Configuration
readInitialConfig pngPath = do
    let path = take (length pngPath - 3) pngPath ++ "initial.json"
    exists <- doesFileExist path
    if exists
     then do conf <- decodeFileStrict' path
             case conf of
               Nothing -> error "initial config json parse error"
               Just c  -> return c
     else return lightningConfig


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
    maxErrs <- replicateM 100 (uniformRM (2 :: Double, 15 :: Double) globalStdGen)
    res <- mapM (\m -> do (s,t) <- solveWith m img; return (s,m,t)) maxErrs
    let (score, err, tree) = minimum res
    putStrLn (path ++ ": best err=" ++ show err ++ ", score=" ++ show score)
    return (score, tree)


writeSolution :: String -> Configuration -> Image PixelRGBA8 -> IO Int
writeSolution path initialConf img = do
  let (blocks', code1, id1) = reduceBlocksToOne (blocks initialConf)
  putStrLn ("flattened initial: " ++ show (id1, blocks'))
  (_, tree) <- Main.optimize path img
  let code = code1 ++ MergeOpt.optimize img (QuadTree.encode (PixelRGBA8 255 255 255 255) tree (show (id1 - 1)))
  let prog = concat $ intersperse "\n" (map show code)
  writeFile (path ++ ".txt") prog
  let img' = QuadTree.createImage (imageWidth img) (imageHeight img) tree
  writePng (path ++ ".out.png") img'
  let canvasSize = imageWidth img * imageHeight img
  let cost = sum (map (moveCost canvasSize) code)
  let siml = similarity img (QuadTree.createImage (imageWidth img) (imageHeight img) tree)
  return (cost + siml)

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