module QuadTree (QuadTree, create4, encode, encode2, cost, createImage) where

import Codec.Picture
import Data.List (maximumBy)
import qualified Data.Map as Map

import ImageUtils
import Types

data QuadTree = Node {
                  nodeColor :: !RGBA,
                  shape :: !Rectangle,
                  subNodes :: !Split
                }
          deriving (Eq, Ord, Show)

data Split = None
           | V !QuadTree !QuadTree
           | H !QuadTree !QuadTree 
           | Quad !QuadTree !QuadTree !QuadTree !QuadTree
           deriving (Eq, Ord, Show)

encode :: RGBA -> QuadTree -> BlockId -> [Move]
encode topColor node id1 =
                  let blk = Block id1 (shape node)
                      color = nodeColor node in
                  ( if color == topColor then []
                     else [ Color blk color ] ) ++
                  case subNodes node of
                    None -> []
                    V a b -> LineCut blk Vertical (nodeX0 b)
                             : encode color a (id1 ++ ".0") ++ encode color b (id1 ++ ".1")
                    H a b -> LineCut blk Horizontal (nodeY0 b)
                             : encode color a (id1 ++ ".0") ++ encode color b (id1 ++ ".1")
                    Quad a b c d -> PointCut blk (nodeX0 c) (nodeY0 c)
                                     : encode color a (id1 ++ ".0")
                                     ++ encode color b (id1 ++ ".1")
                                     ++ encode color c (id1 ++ ".2")
                                     ++ encode color d (id1 ++ ".3")

encode2 :: RGBA -> QuadTree -> BlockId -> [Move]
encode2 topColor node id1 =
                  let blk = Block id1 (shape node)
                      color = mostCommonColor node in
                  ( if color == topColor then []
                     else [ Color blk color ] ) ++
                  case subNodes node of
                    None -> []
                    V a b -> LineCut blk Vertical (nodeX0 b)
                             : encode color a (id1 ++ ".0") ++ encode color b (id1 ++ ".1")
                    H a b -> LineCut blk Horizontal (nodeY0 b)
                             : encode color a (id1 ++ ".0") ++ encode color b (id1 ++ ".1")
                    Quad a b c d -> PointCut blk (nodeX0 c) (nodeY0 c)
                                     : encode color a (id1 ++ ".0")
                                     ++ encode color b (id1 ++ ".1")
                                     ++ encode color c (id1 ++ ".2")
                                     ++ encode color d (id1 ++ ".3")

mostCommonColor :: QuadTree -> RGBA
mostCommonColor tree = case Map.toList (go Map.empty tree) of
                         [] -> nodeColor tree
                         xs -> fst (maximumBy compareTupleRightToLeft xs)
  where
    go m node = case subNodes node of
                  None -> Map.insertWith (+) (nodeColor node) (1::Int) m
                  V a b -> let m' = go m a in go m' b
                  H a b -> let m' = go m a in go m' b
                  Quad a b c d -> let m1 = go m a
                                      m2 = go m1 b
                                      m3 = go m2 c
                                  in go m3 d

compareTupleRightToLeft :: (Ord a, Ord b) => (a,b) -> (a,b) -> Ordering
compareTupleRightToLeft (a1,b1) (a2,b2) =
  case compare b1 b2 of
    EQ -> compare a1 a2
    r  -> r

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
        let average = averageColor' img sh
            siml = 0.005 * totalError' average img sh
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


create3 :: Double -> Double -> Image PixelRGBA8 -> QuadTree
create3 magic1 magic2 img = snd (go (Rectangle 0 0 (imageWidth img) (imageHeight img)))
  where
    canvasSize = fromIntegral (imageWidth img * imageHeight img)
    go :: Rectangle -> (Double, QuadTree)
    go sh@(Rectangle x0 y0 x1 y1) =
        let average = averageColor' img sh
            siml = 0.005 * totalError' average img sh
        in (siml, Node { nodeColor = average, shape = sh,
                         subNodes = bestOf ( 
                                             (if siml > magic1 then [] else [(5 * siml, None)])
                                             ++ map (mapFst (magic2 *)) (divide sh)
                                           )
                        } )

    bestOf :: [(Double, Split)] -> Split
    bestOf = snd . minimum
    divide :: Rectangle -> [(Double, Split)]
    divide rect@(Rectangle x0 y0 x2 y2)
      | x2 == x0 + 1 && y2 == y0 + 1 = []
      | otherwise =
         let xo1 = d2  x0 x2
             xo2 = d25 x0 x2
             xo3 = d35 x0 x2
             xo4 = d14 x0 x2
             xo5 = d34 x0 x2
             yo1 = d2  y0 y2
             yo2 = d25 y0 y2
             yo3 = d35 y0 y2
             yo4 = d14 y0 y2
             yo5 = d34 y0 y2
         in
         lcutH rect yo1 ++
         lcutH rect yo2 ++
         lcutH rect yo3 ++
         lcutH rect yo4 ++
         lcutH rect yo5 ++
         lcutV rect xo1 ++
         lcutV rect xo2 ++
         lcutV rect xo3 ++
         lcutV rect xo4 ++
         lcutV rect xo5 ++
         pcut rect xo1 yo1 ++
         pcut rect xo1 yo2 ++
         pcut rect xo1 yo3 ++
         -- pcut rect xo1 yo4 ++
         -- pcut rect xo1 yo5 ++
         pcut rect xo2 yo1 ++
         pcut rect xo2 yo2 ++
         pcut rect xo2 yo3 ++
         -- pcut rect xo2 yo4 ++
         -- pcut rect xo2 yo5 ++
         pcut rect xo3 yo1 ++
         pcut rect xo3 yo2 ++
         pcut rect xo3 yo3 ++
         -- pcut rect xo3 yo4 ++
         -- pcut rect xo3 yo5 ++
         pcut rect xo4 yo1 ++
         pcut rect xo4 yo2 ++
         pcut rect xo4 yo3 ++
         -- pcut rect xo4 yo4 ++
         -- pcut rect xo4 yo5 ++
         pcut rect xo5 yo1 ++
         pcut rect xo5 yo2 ++
         pcut rect xo5 yo3 -- ++
         -- pcut rect xo5 yo4 ++
         -- pcut rect xo5 yo5
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

create4 :: Double -> Double -> Image PixelRGBA8 -> QuadTree
create4 magic1 magic2 img = snd (go (Rectangle 0 0 (imageWidth img) (imageHeight img)))
  where
    canvasSize = fromIntegral (imageWidth img * imageHeight img)
    go :: Rectangle -> (Double, QuadTree)
    go sh@(Rectangle x0 y0 x1 y1) =
        let average = bestColor' img sh -- bestColor'
            siml = 0.005 * totalError' average img sh
        in (siml, Node { nodeColor = average, shape = sh,
                         subNodes = bestOf ( 
                                             (5 * siml, None)
                                             : map (mapFst (magic2 *)) (divide sh average (magic1 * siml))
                                           )
                        } )
    bestOf :: [(Double, Split)] -> Split
    bestOf [] = None
    bestOf xs = snd (minimum xs)
    divide :: Rectangle -> RGBA -> Double -> [(Double, Split)]
    divide rect@(Rectangle x0 y0 x2 y2) average siml
      | size rect < 25 = []
      | otherwise =
         ( case bestHcut img canvasSize rect average siml of
             Nothing -> []
             Just y  -> lcutH rect y ) ++
         ( case bestVcut img canvasSize rect average siml of
             Nothing -> []
             Just x  -> lcutV rect x ) ++
         ( case bestPcut img canvasSize rect average siml of
             Nothing -> []
             Just (x,y) -> pcut rect x y )
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

d14 :: Int -> Int -> Int
d14 a b = let dist = b - a
              d = dist `quot` 4
          in a + d

d34 :: Int -> Int -> Int
d34 a b = let dist = b - a
              d = dist `quot` 4
          in a + 3 * d
          
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

stepSize :: Int -> Int
stepSize n = max 1 (n `quot` 16)

bestVcut :: Image PixelRGBA8 -> Double -> Rectangle -> RGBA -> Double -> Maybe Int
bestVcut img canvasSize rect@(Rectangle x0 y0 x1 y1) averageColor diff =
    case costs of
      [] -> Nothing
      cs -> case minimum cs of
              (cost, x) | cost <= diff -> Just x
                        | otherwise -> Nothing
  where
    step = stepSize (x1 - x0)
    candidateXs = [x0 + step, x0 + 2*step .. x1 - 2]
    costs :: [(Double, Int)]
    costs = [(calc x, x) | x <- candidateXs]
    calc x = let (r1,r2) = splitV rect x
                 diff1 = 0.005 * totalError'' img r1
                 diff2 = 0.005 * totalError'' img r2
                 cutCost = 7 * sizeFactor rect
              in
                 cutCost + diff1 + diff2
    sizeFactor r = canvasSize / fromIntegral (size r)


bestHcut :: Image PixelRGBA8 -> Double -> Rectangle -> RGBA -> Double -> Maybe Int
bestHcut img canvasSize rect@(Rectangle x0 y0 x1 y1) averageColor diff =
    case costs of
      [] -> Nothing
      cs -> case minimum cs of
              (cost, y) | cost <= diff -> Just y
                        | otherwise -> Nothing
  where
    step = stepSize (y1 - y0)
    candidateYs = [y0 + step, y0 + 2*step .. y1 - 2]
    costs :: [(Double, Int)]
    costs = [(calc y, y) | y <- candidateYs]
    calc y = let (r1,r2) = splitH rect y
                 diff1 = 0.005 * totalError'' img r1
                 diff2 = 0.005 * totalError'' img r2
                 cutCost = 7 * sizeFactor rect
              in
                 cutCost + diff1 + diff2
    sizeFactor r = canvasSize / fromIntegral (size r)

bestPcut :: Image PixelRGBA8 -> Double -> Rectangle -> RGBA -> Double -> Maybe (Int,Int)
bestPcut img canvasSize rect@(Rectangle x0 y0 x1 y1) averageColor diff =
    case costs of
      [] -> Nothing
      cs -> case minimum cs of
              (cost, xy) | cost <= diff -> Just xy
                         | otherwise -> Nothing
  where
    xstep = stepSize (x1 - x0)
    ystep = stepSize (y1 - y0)
    candidateXYs = [(x,y) | x <- [x0 + xstep, x0 + 2*xstep .. x1 - 2],
                            y <- [y0 + ystep, y0 + 2*ystep .. y1 - 2]]
    costs :: [(Double, (Int,Int))]
    costs = [(calc xy, xy) | xy <- candidateXYs]
    calc xy = let (r1,r2,r3,r4) = splitP rect xy
                  diff1 = 0.005 * totalError'' img r1
                  diff2 = 0.005 * totalError'' img r2
                  diff3 = 0.005 * totalError'' img r3
                  diff4 = 0.005 * totalError'' img r4
                  cutCost = 10 * sizeFactor rect
              in
                  cutCost + diff1 + diff2 + diff3 + diff4
    sizeFactor r = canvasSize / fromIntegral (size r)
