module BlockSwap (blockSwap) where

import Codec.Picture

import Types
import Configuration
import ImageUtils


blockSwap :: Image PixelRGBA8 -> Configuration -> (Configuration, [Move])
blockSwap img conf | length blks <= 1 = (conf, [])
                   | not (all (sameSize firstBlock) blks) =
                       error "blockSwap only works when all blocks have the same size"
                   | otherwise = let moves = go blks [] in (doSwaps conf moves, moves)
  where
    blks = blocks conf
    firstBlock = head blks
    sim1 = similarity (confToImage conf) img
    go []   moves'   = reverse moves'
    go [_]  moves'   = reverse moves'
    go (b:bs) moves'
         | b `occursIn` moves' = go bs moves'
         | otherwise = let err11 = totalError' (color b) img (shape b)
                           cand = [ (err21 + err12, b2)
                                      | b2 <- bs,
                                        not (b2 `occursIn` moves'),
                                        let err21 = totalError' (color b2) img (shape b),
                                        let err22 = totalError' (color b2) img (shape b2),
                                        let err12 = totalError' (color b) img (shape b2),
                                        err21 < err11 && err12 < err22 ]
                       in case cand of
                            [] -> go bs moves'
                            cs -> case minimum cs of
                                    (_, b2) -> go bs (Swap (toBlock b) (toBlock b2) : moves')

occursIn :: ConfBlock -> [Move] -> Bool
b `occursIn` moves = any f moves
  where
    bid = blockId b
    f (Swap (Block id1 _) (Block id2 _)) = id1 == bid || id2 == bid
    f _ = False

sameSize :: ConfBlock -> ConfBlock -> Bool
sameSize b1 b2 = sameSize' (shape b1) (shape b2)

sameSize' :: Rectangle -> Rectangle -> Bool
sameSize' (Rectangle x0 y0 x1 y1) (Rectangle x2 y2 x3 y3) =
  (x1 - x0) == (x3 - x2) && (y1 - y0) == (y3 - y2)

toBlock :: ConfBlock -> Block
toBlock b = Block (blockId b) (shape b)

doSwaps :: Configuration -> [Move] -> Configuration
doSwaps conf moves = conf{blocks = foldr doSwap (blocks conf) moves}
  where
    doSwap :: Move -> [ConfBlock] -> [ConfBlock]
    doSwap (Swap (Block id1 r1) (Block id2 r2)) bs =
        let flip b | blockId b == id1 = b{shape = r2}
                   | blockId b == id2 = b{shape = r1}
                   | otherwise        = b
        in map flip bs
    doSwap _ bs = bs