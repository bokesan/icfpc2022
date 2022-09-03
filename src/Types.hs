module Types (
          Block(..), BlockId, Shape
        , RGBA, mixColors
        , Orientation(..)
        , Move(..)
        , moveCost
        , Rectangle(..), contains, size
  ) where

import Codec.Picture
import Data.Word

type BlockId = String

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

           
data Move = LineCut !Block !Orientation !Int
          | PointCut !Block !Int !Int
          | Color !Block !RGBA
          | Swap !Block !Block
          | Merge !Block !Block

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

baseCost :: Move -> Int
baseCost (LineCut _ _ _)  =  7
baseCost (PointCut _ _ _) = 10
baseCost (Color _ _)      =  5
baseCost (Swap _ _)       =  3
baseCost (Merge _ _)      =  1

moveCost :: Int -> Move -> Int
moveCost canvasSize (LineCut b _ _) = cost 7 canvasSize (blockSize b)
moveCost canvasSize (PointCut b _ _) = cost 10 canvasSize (blockSize b)
moveCost canvasSize (Color b _) = cost 5 canvasSize (blockSize b)
moveCost canvasSize (Swap b1 _) = cost 3 canvasSize (blockSize b1)
moveCost canvasSize (Merge b1 b2) = cost 1 canvasSize (max (blockSize b1) (blockSize b2))

blockSize :: Block -> Int
blockSize (Block _ s) = size s

cost :: Int -> Int -> Int -> Int
cost factor canvasSize blockSize = round (fromIntegral factor * fromIntegral canvasSize / fromIntegral blockSize)