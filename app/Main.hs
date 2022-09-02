module Main (main) where

import Codec.Picture
import System.Environment

import Types

main :: IO ()
main = do args <- getArgs
          mapM_ loadImage args

loadImage :: String -> IO ()
loadImage path = do img' <- readImage path
                    case img' of
                      Left err -> putStrLn (path ++ ": error: " ++ err)
                      Right img -> showDimensions path (convertRGBA8 img)

showDimensions :: String -> Image PixelRGBA8 -> IO ()
showDimensions path img = putStrLn (path ++ ": " ++ show (imageWidth img) ++ "x"
                                    ++ show (imageHeight img))

