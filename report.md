---
title: "RZHP, 5. domača naloga"
author: Tadej Ciglarič, Matija Čufar
output: pdf_document

---
















# Memetic algorithm

For our third algorithm, we have decided to implement a simple memetic algorithm.
We started by implementing a genetic algorithm. We have later extended it with
additional optimization and a local search at the end.

The basic idea of the algorithm is:
* Randomly generate initial population
* Repeat until selected number of iterations is reached:
  * Calculate the fitness for all members of the population.
  * Select a pool from the population using the selection function.
  * Perform optimization on all individuals in the pool. (this step is parallelized)
  * Randomly select pairs of individuals from the pool and perform crossover on
    them
  * Merge the old pool with the newly created one.
  * Apply the mutation function to the entire new pool.
  * Use the new pool as the initial population in the next iteration.
* Select the best permutation discovered in this process and optimize it further
  using local search before returning it.

We have used the following for functions with the algorithm:
For selection, we used two varians of roulette wheel selection, deterministic 
tournament selection and a simple function that selects the n best individuals
from the population (deterministic selection).
The roulette wheel selection algorithms were very unpredictable and produced a 
wide variety of differing results. Deterministic tournament selection and
deterministic selection on were TODO:


We have later extended the algorithm to support additional optimization after
pool selection and to perform a local search on the best permutation before
returning it.
