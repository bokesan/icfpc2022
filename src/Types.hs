module Types (
          BlockId
        , RGBA
        , Orientation(..)
        , Move(..)
  ) where

import Codec.Picture

type BlockId = String

type RGBA = PixelRGBA8

showRGBA :: RGBA -> String -> String
showRGBA (PixelRGBA8 r g b a) = showChar '['
                              . shows r . showChar ','
                              . shows g . showChar ','
                              . shows b . showChar ','
                              . shows a . showChar ']'

data Shape = Shape !Int !Int
           deriving (Eq, Ord, Show)

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
  showsPrec _ (LineCut id1 orientation offset) = showString "cut " . showBlockId id1 . showString " ["
                                               . shows orientation . showString "] ["
                                               . shows offset . showChar ']'
  showsPrec _ (PointCut id1 x y) = showString "cut " . showBlockId id1 . showString " ["
                                 . shows x . showChar ',' . shows y . showChar ']'
  showsPrec _ (Color id1 rgba)   = showString "color " . showBlockId id1 . showString " " . showRGBA rgba
  showsPrec _ (Swap id1 id2)     = showString "swap "  . showBlockId id1 . showChar ' ' . showBlockId id2
  showsPrec _ (Merge id1 id2)    = showString "merge " . showBlockId id1 . showChar ' ' . showBlockId id2
        
showBlockId :: BlockId -> ShowS
showBlockId id1 = showChar '[' . showString id1 . showChar ']' 

