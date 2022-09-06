# icfpc2022

Team: Sleep is your Superpower

- (Name chosen to remind me to sleep during the contest)
- One person based in Hannover, Germany
- Implementation language: Haskell

## Solution approach

- Load target PNG file
- (Do color quantization to reduce number of colors? - not implemented)
- Build quadtree using point cuts until the total error goes below a
   certain threshold
- generate ISL program from quadtree

### later improvements / variations

- Lift color moves out of leaves
- Merge blocks before coloring to make it cheaper
- Add line cuts and try other than central subdivisions

### Main round

I just combine the initial blocks into one and use the same
solver as before.

## Problems encountered / Lessons learned

I had no idea for a viable optimization strategy, i.e.
how to solve it in a bottom-up manner or how to extend
partial solutions. Hence, my scores mostly stayed near the end
of the field.

My score calculation was slightly off from the contest
server. The culprit was likely in the similarity part, but
I never found the problem. I suspected the somewhat
unusual Javascript Math.round definition, but simulating
that in Haskell did not fix it.

As usual, I should have spent more time exploring the moves
manually. I never had a good idea how to use swap and
completely ignored it. Merge was also underused.

## Overall

- Well presented problem spec
- Suitable for both one-person and larger teams
- Few small errors that were quickly fixed
- The portal and REST API were very helpful

The contest was a lot of fun. Thanks to the organizers, who did
a fantastic job despite having little preparation time!
