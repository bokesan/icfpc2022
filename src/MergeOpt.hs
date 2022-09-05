module MergeOpt (optimize) where

import Codec.Picture
import Data.List (maximumBy)

import ImageUtils
import Types

optimize :: Image PixelRGBA8 -> [Move] -> [Move]
optimize img moves = go moves
  where
    go ms = case opt2Colors' img (maxMajorBlockNumber ms + 1) ms of
                  Nothing -> ms
                  Just ms' -> go ms'

maxMajorBlockNumber :: [Move] -> Int
maxMajorBlockNumber [] = 0
maxMajorBlockNumber moves = maximum (map mmbn moves)
  where
    mbn (Block id1 _) = head (parseBlockId id1)
    mmbn (Color b _) = mbn b
    mmbn (Merge b1 b2) = max (mbn b1) (mbn b2)
    mmbn (Swap b1 b2) = max (mbn b1) (mbn b2)
    mmbn (LineCut b _ _) = mbn b
    mmbn (PointCut b _ _) = mbn b


opt2Colors' :: Image PixelRGBA8 -> Int -> [Move] -> Maybe [Move]
opt2Colors' img num (m1@(Color b1 c1) : m2@(Color b2 c2) : ms)
  | canMerge b1 b2 && mc m1 + mc m2 + diffCost img m1 + diffCost img m2 >
                      mc m1' + mc m2' + diffCost img m2'
                   && not (b1 `occursIn` ms || b2 `occursIn` ms)
       = Just (m1' : m2' : ms)
  where
    canvas = imageWidth img * imageHeight img
    mc = fromIntegral . moveCost canvas
    (Just newRect) = combine' b1 b2
    newBlk = Block (show num) newRect
    m1' = Merge b1 b2
    m2' = Color newBlk (mixColors c1 c2)
opt2Colors' _ _ _ = Nothing

diffCost :: Image PixelRGBA8 -> Move -> Double
diffCost img (Color (Block _ shape) c) = 0.005 * totalError' c img shape
diffCost _ _ = 0

canMerge :: Block -> Block -> Bool
canMerge b1 b2 = case combine' b1 b2 of
                   Nothing -> False
                   _ -> True

occursIn :: Block -> [Move] -> Bool
b `occursIn` moves = any (moveContainsBlock b) moves

combine' :: Block -> Block -> Maybe Rectangle
combine' (Block _ r1) (Block _ r2) = merge r1 r2

isColorMove :: Move -> Bool
isColorMove (Color _ _) = True
isColorMove _ = False

getBlock :: Move -> Block
getBlock (Color b _) = b

getBlockSize :: Move -> Int
getBlockSize move = case getBlock move of
                      (Block _ shape) -> size shape

opt2colors :: Image PixelRGBA8 -> Int -> [Move] -> Maybe [Move]
opt2colors img num moves = findFirst [] moves
  where
    findFirst init moves =
        case break isColorMove moves of
          (_,[]) -> Nothing
          (init', cm:ms) | getBlock cm `occursIn` ms -> Nothing
                         | otherwise ->
                             case findSecond cm ms of
                               Nothing -> findFirst (init ++ init' ++ [cm]) ms
                               Just (a,cm2,b) ->
                                       Just (init ++ init' ++ a ++ combineColors cm cm2 ++ b)
  
    compareBlockSize (_,m1,_) (_,m2,_) = compare (getBlockSize m1) (getBlockSize m2)

    findSeconds cm moves = [ ( init, cm2, ms )
                             | cm2 <- filter (isCompatibleColor cm) moves,
                               let (init, _:ms) = break (cm2 ==) moves,
                               not (getBlock cm2 `occursIn` ms) ]

    findSecond cm moves = case break (isCompatibleColor cm) moves of
                            (_,[]) -> Nothing
                            (init,cm2:ms) | getBlock cm2 `occursIn` ms -> Nothing
                                          | otherwise -> Just (init,cm2,ms)

    isCompatibleColor m1@(Color b1 c1) m2@(Color b2 c2) =
                      canMerge b1 b2 && mc m1 + mc m2 + diffCost img m1 + diffCost img m2 >
                                        mc m1' + mc m2' + diffCost img m2'
       where
         (Just newRect) = combine' b1 b2
         newBlk = Block (show num) newRect
         m1' = Merge b1 b2
         m2' = Color newBlk (mixColors c1 c2)
    isCompatibleColor _ _ = False

    canvas = imageWidth img * imageHeight img
    mc = fromIntegral . moveCost canvas
    combineColors (Color b1 c1) (Color b2 c2) = let (Just newRect) = combine' b1 b2
                                                    newBlk = Block (show num) newRect
                                                in [Merge b1 b2, Color newBlk (mixColors c1 c2)]
