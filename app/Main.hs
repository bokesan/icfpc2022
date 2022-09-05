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
import BlockSwap
import ImageUtils (similarity)
import Configuration
import qualified MergeOpt

main :: IO ()
main = do args <- getArgs
          case args of
            ("-similarity" : args') -> showSimilarity args'
            ("-blockSwap" : args') ->
                 do scores <- PAR.mapM solveWithBlockSwap args'
                    putStrLn ("Total score: " ++ show (sum scores))
            _ -> do results <- PAR.mapM solveProblem args
                    let scores = map fst results
                        magic = map snd results
                    putStrLn ("Total score: " ++ show (sum scores))
                    putStrLn ("Magic1 " ++ showRange (map fst magic))
                    putStrLn ("Magic2 " ++ showRange (map snd magic))

showRange :: [Double] -> String
showRange xs = show (minimum xs) ++ " - " ++ show (maximum xs)


showSimilarity :: [String] -> IO ()
showSimilarity [file1, file2] = do
    img1 <- readImage' file1
    img2 <- readImage' file2
    putStrLn ("similarity: " ++ show (similarity img1 img2))
showSimilarity _ = error "usage: prog -similarity image1 image2"

solveWithBlockSwap :: String -> IO Int
solveWithBlockSwap path = do img  <- readImage' path
                             conf <- readInitialConfig path
                             let sim1 = similarity (confToImage conf) img
                             let (conf', moves) = blockSwap img conf
                             let canvasSize = imageWidth img * imageHeight img
                             let cost = sum (map (moveCost canvasSize) moves)
                             let sim2 = similarity (confToImage conf') img
                             let id1 = problemId path
                             putStrLn (show id1 ++ ": initial similarity=" ++ show sim1
                                       ++ ", move cost=" ++ show cost
                                       ++ " (" ++ show (length moves) ++ ")"
                                       ++ ", similarity=" ++ show sim2
                                       ++ ", score=" ++ show (cost + sim2))
                             let prog = concat $ intersperse "\n" (map show moves)
                             writeFile (path ++ ".txt") prog
                             return (cost + sim2)
                             
readImage' :: FilePath -> IO (Image PixelRGBA8)
readImage' path = do img' <- readImage path
                     case img' of
                       Left err  -> error (path ++ ": error " ++ err)
                       Right img -> return (convertRGBA8 img)

solveProblem :: String -> IO (Int, (Double, Double))
solveProblem path = do img  <- readImage' path
                       conf <- readInitialConfig path
                       writeSolution path conf img

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


solveWith :: (Double,Double) -> Image PixelRGBA8 -> IO (Int, QuadTree.QuadTree)
solveWith (magic1,magic2) img = do
  let tree = QuadTree.create4 magic1 magic2 img
  let code = MergeOpt.optimize img $ QuadTree.encode2 (PixelRGBA8 255 255 255 255) tree "0"
  let canvasSize = imageWidth img * imageHeight img
  let cost = sum (map (moveCost canvasSize) code)
  let siml = similarity img (QuadTree.createImage (imageWidth img) (imageHeight img) tree)
  return (cost + siml, tree)

optimize :: Image PixelRGBA8 -> IO (Int, (Double, Double), QuadTree.QuadTree)
optimize img = do
    -- observed: 3383 - 18105
    diffLimit <- replicateM 50 (uniformRM (2000 :: Double, 21000 :: Double) globalStdGen)
    -- observed: 5.99 - 21.77
    treeScale <- replicateM 50 (uniformRM (5 :: Double, 30 :: Double) globalStdGen)
    res <- mapM (\m -> do (s,t) <- solveWith m img; return (s,m,t)) (zip diffLimit treeScale)
    return (minimum res)

writeSolution :: String -> Configuration -> Image PixelRGBA8 -> IO (Int, (Double, Double))
writeSolution path initialConf img = do
  let canvasSize = imageWidth img * imageHeight img
  let (_, code1, id1) = reduceBlocksToOne (imageWidth img) (imageHeight img) (blocks initialConf)
  (_, magic, tree) <- Main.optimize img
  let code = code1 ++ MergeOpt.optimize img (QuadTree.encode2 (PixelRGBA8 255 255 255 255) tree (show (id1 - 1)))
  let prog = concat $ intersperse "\n" (map show code)
  writeFile (path ++ ".txt") prog
  let img' = QuadTree.createImage (imageWidth img) (imageHeight img) tree
  writePng (take (length path - 3) path ++ "out.png") img'
  let cost = sum (map (moveCost canvasSize) code)
  let siml = similarity img (QuadTree.createImage (imageWidth img) (imageHeight img) tree)
  putStrLn (path ++ ": cost=" ++ show cost ++ ", similarity=" ++ show siml ++ ", score=" ++ show (cost + siml))
  return (cost + siml, magic)
