module Types (
          problemId
        , Block(..), BlockId, parseBlockId, Shape
        , RGBA, mixColors
        , Orientation(..)
        , Move(..)
        , moveCost
        , moveContainsBlock
        , Rectangle(..), contains, size, merge, splitV, splitH, splitP
        , roundJS
  ) where

import Codec.Picture
import Data.Word

problemId :: String -> Int
problemId filePath = let (tl',_) = break ('/'==) (reverse filePath)
                         tl = reverse tl'
                         (n, _) = break ('.'==) tl
                     in read n

type BlockId = String

parseBlockId :: BlockId -> [Int]
parseBlockId s = map read (splitAround '.' s)

splitAround :: (Eq a) => a -> [a] -> [[a]]
splitAround sep xs = case break (sep ==) xs of
                       ([], []) -> []
                       ([], tail) -> splitAround sep tail
                       (init,[]) -> [init]
                       (init,_:tail) -> init : splitAround sep tail

type RGBA = PixelRGBA8

mixColors :: RGBA -> RGBA -> RGBA
mixColors (PixelRGBA8 a b c d) (PixelRGBA8 e f g h) =
    PixelRGBA8 (m a e) (m b f) (m c g) (m d h)
  where
    m p q = let sum = ((fromIntegral p) :: Word16) + fromIntegral q
            in fromIntegral (sum `quot` 2)
            
data Rectangle = Rectangle !Int !Int !Int !Int
               deriving (Eq, Ord, Show)

contains :: Rectangle -> Int -> Int -> Bool
contains (Rectangle x0 y0 x1 y1) x y = x0 <= x && x < x1 && y0 <= y && y < y1

size :: Rectangle -> Int
size (Rectangle x0 y0 x1 y1) = (x1 - x0) * (y1 - y0)

merge :: Rectangle -> Rectangle -> Maybe Rectangle
merge a b = case combine2 a b of
              Nothing -> combine2 b a
              r -> r

combine2 :: Rectangle -> Rectangle -> Maybe Rectangle
combine2 (Rectangle x0 y0 x1 y1) (Rectangle x2 y2 x3 y3)
  | (x0,y1, x1,y1) == (x2,y2, x3,y2) = Just (Rectangle x0 y0 x1 y3)
  | (x1,y0, x1,y1) == (x2,y2, x2,y3) = Just (Rectangle x0 y0 x3 y3)
  | otherwise = Nothing

splitV, splitH :: Rectangle -> Int -> (Rectangle, Rectangle)
splitV (Rectangle x0 y0 x1 y1) x = (mkRect x0 y0 x y1, mkRect x y0 x1 y1)
splitH (Rectangle x0 y0 x1 y1) y = (mkRect x0 y0 x1 y, mkRect x0 y x1 y1)

splitP :: Rectangle -> (Int,Int) -> (Rectangle, Rectangle, Rectangle, Rectangle)
splitP (Rectangle x0 y0 x1 y1) (x,y) = ( mkRect x0 y0 x  y ,
                                         mkRect x  y0 x1 y ,
                                         mkRect x  y  x1 y1,
                                         mkRect x0 y  x  y1 )

mkRect :: Int -> Int -> Int -> Int -> Rectangle
mkRect x0 y0 x1 y1 | x0 < x1 && y0 < y1 = Rectangle x0 y0 x1 y1
                   | otherwise = error ("empty rectangle: " ++ show (x0,y0,x1,y1))
                                    

showRGBA :: RGBA -> String -> String
showRGBA (PixelRGBA8 r g b a) = showChar '['
                              . shows r . showChar ','
                              . shows g . showChar ','
                              . shows b . showChar ','
                              . shows a . showChar ']'

type Shape = Rectangle

data Orientation = Vertical | Horizontal
                 deriving (Eq, Ord)

instance Show Orientation where
  showsPrec _ Vertical   = showChar 'X'
  showsPrec _ Horizontal = showChar 'Y'

data Block = Block !BlockId !Shape
           deriving (Eq, Show)

           
data Move = LineCut !Block !Orientation !Int
          | PointCut !Block !Int !Int
          | Color !Block !RGBA
          | Swap !Block !Block
          | Merge !Block !Block
          deriving (Eq)

instance Show Move where
  showsPrec _ (LineCut id1 orientation offset) = showString "cut " . showBlockId id1 . showString " ["
                                               . shows orientation . showString "] ["
                                               . shows offset . showChar ']'
  showsPrec _ (PointCut id1 x y) = showString "cut " . showBlockId id1 . showString " ["
                                 . shows x . showChar ',' . shows y . showChar ']'
  showsPrec _ (Color id1 rgba)   = showString "color " . showBlockId id1 . showString " " . showRGBA rgba
  showsPrec _ (Swap id1 id2)     = showString "swap "  . showBlockId id1 . showChar ' ' . showBlockId id2
  showsPrec _ (Merge id1 id2)    = showString "merge " . showBlockId id1 . showChar ' ' . showBlockId id2
        
showBlockId :: Block -> ShowS
showBlockId (Block id1 _) = showChar '[' . showString id1 . showChar ']' 

moveCost :: Int -> Move -> Int
moveCost canvasSize (LineCut b _ _) = cost 7 canvasSize (blockSize b)
moveCost canvasSize (PointCut b _ _) = cost 10 canvasSize (blockSize b)
moveCost canvasSize (Color b _) = cost 5 canvasSize (blockSize b)
moveCost canvasSize (Swap b1 _) = cost 3 canvasSize (blockSize b1)
moveCost canvasSize (Merge b1 b2) = cost 1 canvasSize (max (blockSize b1) (blockSize b2))

blockSize :: Block -> Int
blockSize (Block _ s) = size s

cost :: Int -> Int -> Int -> Int
cost factor canvasSize blockSize = roundJS (fromIntegral factor * fromIntegral canvasSize / fromIntegral blockSize)

moveContainsBlock :: Block -> Move -> Bool
moveContainsBlock blk move =
  case move of
    LineCut b _ _  -> b == blk
    PointCut b _ _ -> b == blk
    Color b _      -> b == blk
    Swap b1 b2     -> b1 == blk || b2 == blk
    Merge b1 b2    -> b1 == blk || b2 == blk

roundJS :: Double -> Int
roundJS x = let f = floor x
                c = ceiling x
                df = x - fromIntegral f
                dc = fromIntegral c - x
             in
                if df < dc then f else c

