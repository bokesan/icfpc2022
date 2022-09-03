{-# LANGUAGE OverloadedStrings #-}
module Configuration (
         Configuration(..)
       , ConfBlock(..)
       , reduceBlocksToOne
       , lightningConfig
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


lightningConfig :: Configuration
lightningConfig = Configuration { width = 400, height = 400, blocks = [
                                    ConfBlock { blockId = "0", shape = Rectangle 0 0 400 400,
                                                color = PixelRGBA8 255 255 255 255 } ] }

reduceBlocksToOne :: [ConfBlock] -> ([ConfBlock], [Move], Int)
reduceBlocksToOne blks = go num blks
  where
    num = length blks
    go id1 []  = ([] , [], id1)
    go id1 [x] = ([x], [], id1)
    go id1 bs  = let r@(bs', instr, id') = reduce id1 bs in
                 if null instr then r else
                   let (bs'', instr', id'') = go id' bs'
                   in (bs'', instr ++ instr', id'')
    reduce id1 [] = ([], [], id1)
    reduce id1 [x] = ([x], [], id1)
    reduce id1 (a:b:rest) = case merge (shape a) (shape b) of
                              Nothing -> let (bs,instr,id') = reduce id1 (b:rest) in
                                         (a : bs, instr, id')
                              Just r -> let move = Merge (blk a) (blk b)
                                            newBlock = ConfBlock { blockId = show id1, shape = r, color = color a }
                                            (rest', instr', id'') = reduce (id1 + 1) rest
                                        in (newBlock : rest', move : instr', id'')

blk :: ConfBlock -> Block
blk b = Block (blockId b) (shape b)