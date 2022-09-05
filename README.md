# icfpc2022

Team: Sleep is your Superpower

## Solve Problem

- Load PNG file
- Do color quantization to reduce number of colors?
- Build quadtree
- Quadtree -> ISL program

## Improvements

- Omit white color
- Always Lift color moves out of leaves
- Merge blocks before coloring to make it much cheaper

Go back to first approach. Use error function and "continue down" as
tunable params. The run an optimization strategy to approximate
best params.

As a further parameter, add "division strategy" or "target block size".
I.e. for 10x10 problems, we know that we should not use size/2 divisions

## Main round

### make white

cost for 100 blocks: 6444     new  2858
cost for 256 blocks: 43690         13636
cost for 400 blocks: 105764       28451


### swap approach

Cost: numSwaps * 3 * numBlocks 
