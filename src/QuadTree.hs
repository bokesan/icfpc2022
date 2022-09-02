module QuadTree (QuadTree, create, size, encode, cost, createImage) where

import Codec.Picture

import ImageUtils
import Types

data QuadTree = Node {
                  nodeColor :: !RGBA,
                  nodeX0, nodeY0, nodeX1, nodeY1 :: !Int,
                  subNodes :: Split
                }
          deriving (Show)

data Split = None
           | V QuadTree QuadTree
           | H QuadTree QuadTree
           | Quad QuadTree QuadTree QuadTree QuadTree
           deriving (Show)

size :: QuadTree -> Int
size node = 1 + splitSize (subNodes node)

splitSize :: Split -> Int
splitSize None = 0
splitSize (V a b) = size a + size b
splitSize (H a b) = size a + size b
splitSize (Quad a b c d) = size a + size b + size c + size d

encode :: QuadTree -> BlockId -> [Move]
encode node id1 = case subNodes node of
                    None -> [ Color id1 (nodeColor node) ]
                    V a b -> LineCut id1 Vertical (nodeX0 b)
                             : encode a (id1 ++ ".0") ++ encode b (id1 ++ ".1")
                    H a b -> LineCut id1 Horizontal (nodeY0 b)
                             : encode a (id1 ++ ".0") ++ encode b (id1 ++ ".1")
                    Quad a b c d -> PointCut id1 (nodeX0 c) (nodeY0 c)
                                     : encode a (id1 ++ ".0")
                                     ++ encode b (id1 ++ ".1")
                                     ++ encode c (id1 ++ ".2")
                                     ++ encode d (id1 ++ ".3")

cost :: Double -> QuadTree -> Int
cost canvasSize node = case subNodes node of
              None -> round (5.0 * canvasSize / blockSize node)
              V a b -> round (7.0 * canvasSize / blockSize node)
                       + cost canvasSize a + cost canvasSize b
              H a b -> round (7.0 * canvasSize / blockSize node)
                       + cost canvasSize a + cost canvasSize b
              Quad a b c d -> round (10.0 * canvasSize / blockSize node)
                              + cost canvasSize a + cost canvasSize b
                              + cost canvasSize c + cost canvasSize d

getColorBlocks :: QuadTree -> [(Int,Int,Int,Int,PixelRGBA8)]
getColorBlocks node = case subNodes node of
                        None -> [(nodeX0 node, nodeY0 node, nodeX1 node, nodeY1 node, nodeColor node)]
                        V a b -> getColorBlocks a ++ getColorBlocks b
                        H a b -> getColorBlocks a ++ getColorBlocks b
                        Quad a b c d -> getColorBlocks a ++ getColorBlocks b
                                        ++ getColorBlocks c ++ getColorBlocks d

createImage :: Int -> Int -> QuadTree -> Image PixelRGBA8
createImage w h tree = generateImage f w h
  where
    blocks = getColorBlocks tree
    f x y' = let y = (h - (y' + 1)) in
             case [ c | (x0,y0,x1,y1,c) <- blocks, x0 <= x && x < x1 && y0 <= y && y < y1 ] of
               [] -> PixelRGBA8 255 255 255 255
               (c : _) -> c

blockSize :: QuadTree -> Double
blockSize node = fromIntegral ((nodeX1 node - nodeX0 node) * (nodeY1 node - nodeY0 node))

create :: Double -> Image PixelRGBA8 -> QuadTree
create maxError image = go 0 0 w h
  where
    w = imageWidth image
    h = imageHeight image
    go x0 y0 x1 y1 = let average = averageColor image x0 y0 x1 y1
                         err = totalError average image x0 y0 x1 y1 / log (fromIntegral (min w h))
                     in
                         Node { nodeColor = average,
                                nodeX0 = x0, nodeY0 = y0,
                                nodeX1 = x1, nodeY1 = y1,
                                subNodes =
                                  if err <= maxError then None
                                  else divide x0 y0 x1 y1 }
    divide x0 y0 x2 y2
      | x2 == x0 + 1 && y2 == y0 + 1 = None
      | x2 == x0 + 1 =
         let y1 = (y0 + y2) `quot` 2 in
         H (go x0 y0 x2 y1) (go x0 y1 x2 y2)
      | y2 == y0 + 1 =
         let x1 = (x0 + x2) `quot` 2 in
         V (go x0 y0 x1 y2) (go x1 y0 x2 y2)
      | otherwise =
         let x1 = (x0 + x2) `quot` 2
             y1 = (y0 + y2) `quot` 2
         in
             Quad (go x0 y0 x1 y1)
                  (go x1 y0 x2 y1)
                  (go x1 y1 x2 y2)
                  (go x0 y1 x1 y2)
