module QuadTree (QuadTree, create, create2, create3, encode, cost, createImage) where

import Codec.Picture

import ImageUtils
import Types

data QuadTree = Node {
                  nodeColor :: !RGBA,
                  shape :: !Rectangle,
                  subNodes :: Split
                }
          deriving (Eq, Ord, Show)

data Split = None
           | V QuadTree QuadTree
           | H QuadTree QuadTree
           | Quad QuadTree QuadTree QuadTree QuadTree
           deriving (Eq, Ord, Show)

encode :: RGBA -> QuadTree -> BlockId -> [Move]
encode topColor node id1 =
                  let blk = Block id1 (shape node) in
                  case subNodes node of
                    None | nodeColor node == topColor -> []
                         | otherwise -> [ Color blk (nodeColor node) ]
                    V a b -> LineCut blk Vertical (nodeX0 b)
                             : encode topColor a (id1 ++ ".0") ++ encode topColor b (id1 ++ ".1")
                    H a b -> LineCut blk Horizontal (nodeY0 b)
                             : encode topColor a (id1 ++ ".0") ++ encode topColor b (id1 ++ ".1")
                    Quad a b c d -> PointCut blk (nodeX0 c) (nodeY0 c)
                                     : encode topColor a (id1 ++ ".0")
                                     ++ encode topColor b (id1 ++ ".1")
                                     ++ encode topColor c (id1 ++ ".2")
                                     ++ encode topColor d (id1 ++ ".3")

nodeX0, nodeY0 :: QuadTree -> Int
nodeX0 node = case shape node of Rectangle x0 _ _ _ -> x0
nodeY0 node = case shape node of Rectangle _ y0 _ _ -> y0

blockSize :: QuadTree -> Double
blockSize node = fromIntegral (size (shape node))

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

getColorBlocks :: QuadTree -> [(Rectangle,PixelRGBA8)]
getColorBlocks node = case subNodes node of
                        None -> [(shape node, nodeColor node)]
                        V a b -> getColorBlocks a ++ getColorBlocks b
                        H a b -> getColorBlocks a ++ getColorBlocks b
                        Quad a b c d -> getColorBlocks a ++ getColorBlocks b
                                        ++ getColorBlocks c ++ getColorBlocks d

createImage :: Int -> Int -> QuadTree -> Image PixelRGBA8
createImage w h tree = generateImage f w h
  where
    blocks = getColorBlocks tree
    f x y' = let y = (h - (y' + 1)) in
             case [ c | (rect,c) <- blocks, contains rect x y ] of
               [] -> PixelRGBA8 255 255 255 255
               (c : _) -> c

create :: Double -> Image PixelRGBA8 -> QuadTree
create maxError image = go 0 0 w h
  where
    w = imageWidth image
    h = imageHeight image
    go x0 y0 x1 y1 = let average = averageColor image x0 y0 x1 y1
                         err = totalError average image x0 y0 x1 y1 / log (fromIntegral (min w h))
                     in
                         Node { nodeColor = average,
                                shape = Rectangle x0 y0 x1 y1,
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

create2 :: Double -> Image PixelRGBA8 -> QuadTree
create2 targetCost img = go (Rectangle 0 0 (imageWidth img) (imageHeight img))
  where
    canvasSize = fromIntegral (imageWidth img * imageHeight img)
    go sh@(Rectangle x0 y0 x1 y1) =
        let average = averageColor img x0 y0 x1 y1
            siml = 0.005 * totalError average img x0 y0 x1 y1
        in Node { nodeColor = average, shape = sh,
                  subNodes = if shouldImprove siml sh then divide sh else None }
    shouldImprove siml rect = siml > targetCost * canvasSize / fromIntegral (size rect)
    divide (Rectangle x0 y0 x2 y2)
      | x2 == x0 + 1 && y2 == y0 + 1 = None
      | x2 == x0 + 1 =
         let y1 = (y0 + y2) `quot` 2 in
         H (go (Rectangle x0 y0 x2 y1)) (go (Rectangle x0 y1 x2 y2))
      | y2 == y0 + 1 =
         let x1 = (x0 + x2) `quot` 2 in
         V (go (Rectangle x0 y0 x1 y2)) (go (Rectangle x1 y0 x2 y2))
      | otherwise =
         let x1 = (x0 + x2) `quot` 2
             y1 = (y0 + y2) `quot` 2
         in
             Quad (go (Rectangle x0 y0 x1 y1))
                  (go (Rectangle x1 y0 x2 y1))
                  (go (Rectangle x1 y1 x2 y2))
                  (go (Rectangle x0 y1 x1 y2))


create3 :: Double -> Image PixelRGBA8 -> QuadTree
create3 targetCost img = snd (go (Rectangle 0 0 (imageWidth img) (imageHeight img)))
  where
    canvasSize = fromIntegral (imageWidth img * imageHeight img)
    go :: Rectangle -> (Double, QuadTree)
    go sh@(Rectangle x0 y0 x1 y1) =
        let average = averageColor img x0 y0 x1 y1
            siml = 0.005 * totalError average img x0 y0 x1 y1
        in (siml, Node { nodeColor = average, shape = sh,
                         subNodes = bestOf ( -- magic constant 6000 here
                                             (if siml > 6000 then [] else [(5 * siml, None)])
                                             ++ map (mapFst (targetCost *)) (divide sh)
                                           )
                        } )

    bestOf :: [(Double, Split)] -> Split
    bestOf = snd . minimum
    divide :: Rectangle -> [(Double, Split)]
    divide rect@(Rectangle x0 y0 x2 y2)
      | x2 == x0 + 1 && y2 == y0 + 1 = []
      | otherwise =
         lcutH rect (d2 y0 y2) ++
         lcutH rect (d25 y0 y2) ++
         lcutH rect (d35 y0 y2) ++
         lcutV rect (d2 x0 x2) ++
         lcutV rect (d25 x0 x2) ++
         lcutV rect (d35 x0 x2) ++
         pcut rect (d2 x0 x2) (d2 y0 y2) ++
         pcut rect (d2 x0 x2) (d25 y0 y2) ++
         pcut rect (d2 x0 x2) (d35 y0 y2) ++
         pcut rect (d25 x0 x2) (d2 y0 y2) ++
         pcut rect (d25 x0 x2) (d25 y0 y2) ++
         pcut rect (d25 x0 x2) (d35 y0 y2) ++
         pcut rect (d35 x0 x2) (d2 y0 y2) ++
         pcut rect (d35 x0 x2) (d25 y0 y2) ++
         pcut rect (d35 x0 x2) (d35 y0 y2)
    lcutH rect@(Rectangle x0 y0 x2 y2) y1
               | y1 == y0 = []
               | otherwise = let (c1,t1) = go (Rectangle x0 y0 x2 y1)
                                 (c2,t2) = go (Rectangle x0 y1 x2 y2)
                             in [(c1+c2 + 7 * sizeFactor rect, H t1 t2)]
    lcutV rect@(Rectangle x0 y0 x2 y2) x1
               | x1 == x0 = []
               | otherwise = let (c1,t1) = go (Rectangle x0 y0 x1 y2)
                                 (c2,t2) = go (Rectangle x1 y0 x2 y2)
                             in [(c1+c2 + 7 * sizeFactor rect, V t1 t2)]
    pcut rect@(Rectangle x0 y0 x2 y2) x1 y1
               | x1 == x0 || y1 == y0 = []
               | otherwise =
                   let (c1, t1) = go (Rectangle x0 y0 x1 y1)
                       (c2, t2) = go (Rectangle x1 y0 x2 y1)
                       (c3, t3) = go (Rectangle x1 y1 x2 y2)
                       (c4, t4) = go (Rectangle x0 y1 x1 y2)
                    in [(c1+c2+c3+c4 + 10 * sizeFactor rect, Quad t1 t2 t3 t4)]
    sizeFactor r = canvasSize / fromIntegral (size r)
    
d2 :: Int -> Int -> Int
d2 a b = (a + b) `quot` 2

d25 :: Int -> Int -> Int
d25 a b = let dist = b - a
              d = dist `quot` 5
          in a + 2 * d


d35 :: Int -> Int -> Int
d35 a b = let dist = b - a
              d = dist `quot` 5
          in a + 3 * d

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a,b) = (f a, b)