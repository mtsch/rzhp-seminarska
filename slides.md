# Problem

* Dance line
* Some dancers want to be close, others apart
* Try to find optimal permutation
* Problem is hard - $O(n!)$

# Methods

* Classic local search
* Variable Neighbourhood Search
* Memetic algorithm

# Local Search and VNS

* Local search is a special case of VNS
* Multiple restarts
* Tend to get stuck in local optimum

# Local Search and VNS

Neighbourhoods used:

* Swap two neighbours
* Split the permutation into two parts and swap them
* Swap two elements in the permutation
* Choose a part of the permutation and reverse it
* Combinations of above

# Memetic Algorithm

Based on a simple genetic algorithm.

* Selection:
    * Two variants of Roulette wheel selection
    * Deterministic tournament selection
    * Truncation selection
    * Tournaments selection
    * Hybrid approach
* Ordered crossover
* Mutation

# Memetic Algorithm

Additional improvements:

* Local search with limited number of steps after selection
* Local search before returning the result

# Results on DS3.csv

* Local search: 1992
* VNS: 1985
* MA: 1992

# Result evaluation

Local search worked well because it's simple and fast.

# Result evaluation

There was no added value in using multiple neighbourhoods.

# Result evaluation

MA works well, but is very slow and sometimes unpredictable.

# Possible improvements

* Faster language?
* More careful parameter selection for MA?
* Finding some other neighborhood?
* Replacing VNS with a different algorithm?

# Questions
