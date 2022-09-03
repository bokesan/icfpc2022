{-# LANGUAGE OverloadedStrings #-}
module Configuration (
         Configuration(..)
       , ConfBlock(..)
  ) where

import Codec.Picture
import Data.Aeson

import Types

data Configuration = Configuration {
                         width :: !Int
                       , height :: !Int
                       , blocks :: [ ConfBlock ]
                     }
                     deriving (Eq, Show)

data ConfBlock = ConfBlock {
                     blockId :: String
                   , shape :: !Rectangle
                   , color :: !RGBA
                 }
                 deriving (Eq, Show)

instance FromJSON Configuration where
    parseJSON = withObject "Configuration" $ \v -> Configuration
        <$> v .: "width"
        <*> v .: "height"
        <*> v .: "blocks"

instance FromJSON ConfBlock where
    parseJSON = withObject "ConfBlock" $ \v -> do
        id1 <- v .: "blockId"
        [x0,y0] <- v .: "bottomLeft"
        [x1,y1] <- v .: "topRight"
        [r,g,b,a] <- v .: "color"
        return (ConfBlock id1 (Rectangle x0 y0 x1 y1) (PixelRGBA8 r g b a))

reduceBlocks :: [ConfBlock] -> ([ConfBlock], [Move])
reduceBlocks blks = (blks, [])
  where
    num = length blks
    