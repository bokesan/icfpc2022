{-# LANGUAGE OverloadedStrings #-}
module Configuration (
         Configuration(..)
       , ConfBlock(..)
       , reduceBlocksToOne
       , lightningConfig
       , confToImage
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
                     blockId :: !String
                   , shape :: !Rectangle
                   , color :: !RGBA
                 }
                 deriving (Eq, Ord, Show)

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

reduceBlocksToOne :: Int -> Int -> [ConfBlock] -> ([ConfBlock], [Move], Int)
reduceBlocksToOne width height blks | allSameSize blks = mergeRowWise width height blks
                                    | otherwise = go num blks
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


mergeRowWise :: Int -> Int -> [ConfBlock] -> ([ConfBlock], [Move], Int)
mergeRowWise _ _ [] = ([], [], 0)
mergeRowWise _ _ [b] = ([b], [], 1)
mergeRowWise width height bs@(b1:_) = go undefined undefined 0 0 num []
  where
    num = length bs
    w = blockWidth b1
    h = blockHeight b1
    getBlock x y = case [b | b <- bs, isAt x y b] of
                     [b] -> b
                     [] -> error ("no block at " ++ show (x,y))
                     _ -> error ("multiple blocks at " ++ show (x,y))
    go big prev x y id1 moves
       | y == height = ([big], reverse moves, id1)
       | x == width && y == 0 = go prev         undefined 0 (y + h) id1 moves
       | x == width  = go (mergeH big prev id1) undefined 0 (y + h) (id1 + 1) (Merge (blk big) (blk prev) : moves)
       | x == 0      = go big (getBlock x y) (x + w) y id1 moves
       | otherwise   = let b = getBlock x y in go big (mergeH prev b id1) (x + w) y (id1 + 1)
                                                    (Merge (blk prev) (blk b) : moves)

isAt :: Int -> Int -> ConfBlock -> Bool
isAt x y b = case shape b of
               Rectangle x0 y0 _ _ -> x == x0 && y == y0

mergeH :: ConfBlock -> ConfBlock -> Int -> ConfBlock
mergeH b1 b2 idc = ConfBlock{blockId = show idc, shape = newShape, color = color b1}
  where
    Just newShape = merge (shape b1) (shape b2)

blockWidth, blockHeight :: ConfBlock -> Int
blockWidth b = case shape b of Rectangle x0 _ x1 _ -> x1 - x0
blockHeight b = case shape b of Rectangle _ y0 _ y1 -> y1 - y0


allSameSize :: [ConfBlock] -> Bool
allSameSize [] = True
allSameSize (b:bs) = all (sameSize b) bs

sameSize :: ConfBlock -> ConfBlock -> Bool
sameSize b1 b2 = sameSize' (shape b1) (shape b2)

sameSize' :: Rectangle -> Rectangle -> Bool
sameSize' (Rectangle x0 y0 x1 y1) (Rectangle x2 y2 x3 y3) =
  (x1 - x0) == (x3 - x2) && (y1 - y0) == (y3 - y2)


blk :: ConfBlock -> Block
blk b = Block (blockId b) (shape b)

confToImage :: Configuration -> Image PixelRGBA8
confToImage conf = generateImage f (width conf) h
  where
    h = height conf
    f x y' = let y = h - (y' + 1) in
             case [b | b <- blocks conf, contains (shape b) x y] of
               [b] -> color b
               [] -> error "undefined location in configuration"
               _ ->  error "location defined multiple times"
