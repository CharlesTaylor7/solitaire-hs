# A Solitaire solver
Solves solitaire games with modified A*-star pathfinding algorithm.

# Boring Solitaire Rules
A vastly simplified Spider solitaire:
  - Cards are suitless and have rank between 1 to 5
  - There is no leftover deck to deal from.

# Games implemented
[x] Boring
[x] Yukon
[ ] Spider


# To Do
- Compute stats on solitaire win rates
- solve randomized games efficiently
  - switch from breadth first approach back to depth first?
  - better heuristics?
  - move defaults?

- benchmark
  - average amount of branching?
  - space usage?
  - time spent exploring unfruitful decision paths?
- yukon solve sorta works
  - heuristic weights were chosen arbitrarily
  - it either finds a solution in about 5 seconds or it hangs itself by fruitlessly enumerating the universe of hopeless game states
  - time out solves that don't terminate in under 10 seconds, and tune heuristic weights based on statistical refinement?
