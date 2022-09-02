module Types where

import Codec.Picture
import Data.Word

type BlockId = String

type RGBA = PixelRGBA8

showRGBA :: RGBA -> String -> String
showRGBA (PixelRGBA8 r b g a) = showChar '['
                              . shows r . showChar ','
                              . shows g . showChar ','
                              . shows b . showChar ','
                              . shows a . showChar ']'

data Shape = Shape !Int !Int
           deriving (Eq, Ord, Show)

type Canvas = Shape

data Orientation = Vertical | Horizontal
                 deriving (Eq, Ord)

instance Show Orientation where
  showsPrec _ Vertical   = showChar 'X'
  showsPrec _ Horizontal = showChar 'Y'

data SimpleBlock = SimpleBlock Shape RGBA
                 deriving (Show)

data Block = Simple SimpleBlock
           | Complex Shape [SimpleBlock]
           deriving (Show)
           
data Move = LineCut BlockId !Orientation !Int
          | PointCut BlockId !Int !Int
          | Color BlockId !RGBA
          | Swap BlockId BlockId
          | Merge BlockId BlockId
          deriving (Eq, Ord)
          
instance Show Move where
  showsPrec _ (LineCut id1 orientation offset) = showString "cut [" . shows id1 . showString "] ["
                                               . shows orientation . showString "] ["
                                               . shows offset . showChar ']'
  showsPrec _ (PointCut id1 x y) = showString "cut [" . shows id1 . showString "] ["
                                 . shows x . showChar ',' . shows y . showChar ']'
  showsPrec _ (Color id1 rgba) = showString "color [" . shows id1 . showString "] "
                               . showRGBA rgba
  showsPrec _ (Swap id1 id2) = showString "swap [" . shows id1 . showString "] ["
                             . shows id2 . showChar ']'
  showsPrec _ (Merge id1 id2) = showString "merge [" . shows id1 . showString "] ["
                              . shows id2 . showChar ']'
        


baseCost :: Move -> Int
baseCost (LineCut _ _ _)  =  7
baseCost (PointCut _ _ _) = 10
baseCost (Color _ _)      =  5
baseCost (Swap _ _)       =  3
baseCost (Merge _ _)      =  1


