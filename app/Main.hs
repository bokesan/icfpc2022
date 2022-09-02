module Main (main) where

import Codec.Picture
import Data.List
import System.Environment

import qualified QuadTree

maxError :: Double
maxError = 250000.0

main :: IO ()
main = do args <- getArgs
          mapM_ solveProblem args

solveProblem :: String -> IO ()
solveProblem path = do img' <- readImage path
                       case img' of
                         Left err -> putStrLn (path ++ ": error: " ++ err)
                         Right img -> writeSolution path (convertRGBA8 img)
                         
loadImage :: String -> IO ()
loadImage path = do img' <- readImage path
                    case img' of
                      Left err -> putStrLn (path ++ ": error: " ++ err)
                      Right img -> showInfo path (convertRGBA8 img)

writeSolution :: String -> Image PixelRGBA8 -> IO ()
writeSolution path img = do
  let tree = QuadTree.create maxError img
  let code = QuadTree.encode tree "0"
  let prog = concat $ intersperse "\n" (map show code)
  writeFile (path ++ ".txt") prog
  
showInfo :: String -> Image PixelRGBA8 -> IO ()
showInfo path img = do
  putStr path
  putStr ": "
  putStrLn (show w ++ "x" ++ show h)
  showPixelAt 0 0
  showPixelAt 0 (h - 1)
  showPixelAt (w - 1) 0
  showPixelAt (w - 1) (h - 1)
  let tree = QuadTree.create maxError img
  putStrLn ("  Size: " ++ show (w * h) ++ ", quadtree(" ++ show maxError ++ ") size: " ++ show
             (QuadTree.size tree))
  where
    w = imageWidth img
    h = imageHeight img
    showPixelAt x y = let v = pixelAt img x y in
                      putStrLn ("  [" ++ show x ++ "," ++ show y ++ "] = "
                                ++ show v)
