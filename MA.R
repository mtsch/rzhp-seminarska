source("general.R")
source("vnsearch.R")
library(snowfall)

MA <- function(adjmat,                   # Adjacency matrix
               score         = scoreFun, # Score function
               selectFun     = dSelect,  # Selection function
               improveFun    = id,       # Additional optimization
               crossFun      = orderedX, # Crossover function
               mutateFun     = mutate,   # Mutation function
               mutation.rate = 0.01,     # Probability of mutation
               init.pop.size = 10000,    # Initial population size
               n.iter        = 100,      # Number of iterations
               n.pool        = 100,      # Size of the breeding pool
               verbose       = T,        # Verbose?
               finish.steps  = 1000,     # Local search steps at the end.
               finish.neigh  = n3,       # Neighbourhood used at the end.
               cores         = 1,        # Number of cores used.
               ...                       # Arguments to improveFun
               )                         # Returns permutation
{
    # Initialize snowfall if needed.
    if (cores > 1) {
        sfInit(parallel=T, cpus=cores)
        sfExport(list=list("adjmat", "score", "improveFun"))
    }

    n <- nrow(adjmat)
    # Overall best
    best.p <- NA
    best.f <- 0
    # Initial population - inidividuals are placed in columns.
    pop  <- sapply(1:init.pop.size, function(.) sample(n))

    for (i in 1:n.iter) {
        # Get fitnesses
        fitness  <- apply(pop, 2, score, A=adjmat)

        # Current best
        c.best.f <- max(fitness)
        if (c.best.f > best.f) {
            best.p <- pop[, which.max(fitness)]
            best.f <- c.best.f
        }

        # Print status
        if (verbose) {
            message(paste0("Iteration ", i, ":"))
            message(c.best.f)
        }

        # Select breeding pool
        pool <- selectFun(pop, fitness, n.pool)
        # Apply optimization
        if (cores > 1) {
            sfExport(list=list("pool"))
            pool <- sfApply(pool, 2, improveFun, adjmat, score, ...)
        } else {
            pool <- apply(pool, 2, improveFun, adjmat, score, ...)
        }

        # Select individuals to breed
        breed1 <- sample(n.pool, n.pool, replace=T)
        breed2 <- sample(n.pool, n.pool, replace=T)

        # Produce offspring
        children <- lapply(1:n.pool,
                            function(i) crossFun(pool[, breed1[i]],
                                                 pool[, breed2[i]]))

        children <- unlist(children)
        children <- matrix(children[!is.na(children)], ncol=2*n.pool)

        # Join children with old population
        pop <- cbind(children, pool)
        # Apply mutation
        pop <- apply(pop, 2, mutateFun, mutation.rate)
    }

    # Select best
    fitness <- apply(pop, 2, score, A=adjmat)

    # Select the overall best specimen.
    c.best.f <- max(fitness)
    if (c.best.f > best.f) {
        best.p <- pop[, which.max(fitness)]
        best.f <- c.best.f
    }

    if (verbose)
        message("Finishing...")

    # Finish by performing a local search on the result.
    res <- localSearch(best.p,
                       adj   = adjmat,
                       score = score,
                       neigh = finish.neigh,
                       steps = finish.steps)
    message(paste0("Final score: ", score(adjmat, res), "."))

    # Stop snowfall.
    if (cores > 1)
        sfStop()

    res
}

# Check if perm is a valid permutation.
# isperm : permutation → logical
isperm <- function(perm)
{
    length(unique(perm)) == length(perm) && max(perm) == length(perm)
}

# Swap two places in permutation with the probability of mutation.rate
# mutate : permutation, probability → permutation
mutate <- function(perm, mutation.rate)
{
    while (runif(1) < mutation.rate) {
        is  <- sample(perm, 2)
        tmp <- perm[is[1]]

        perm[is[1]] <- perm[is[2]]
        perm[is[2]] <- tmp
    }
    perm
}

# Select n individuals from pop
# xyzSelect : population, fitnesses, n.pool → population

# Roulette wheel selection based on ranks
rouletteWheelSelect1 <- function(pop, fitness, n)
{
    fitness <- order(fitness)

    vs <- runif(n, min=0, max=sum(fitness))
    is <- findInterval(vs, cumsum(fitness), rightmost.closed=T) + 1

    pop[, is]
}

# Roulette wheel selection with the minimum fitness set to 0
rouletteWheelSelect2 <- function(pop, fitness, n)
{
    fitness <- fitness - min(fitness)

    vs <- runif(n, min=0, max=sum(fitness))
    is <- findInterval(vs, cumsum(fitness), rightmost.closed=T) + 1

    pop[, is]
}

# Deterministic selection
dSelect <- function(pop, fitness, n)
{
    pop[, order(fitness, decreasing=T)][, 1:n]
}

# Deterministic tournament selection
tournamentSelect <- function(pop, fitness, n)
{
    fst <- sample(n, replace=T)
    snd <- sample(n, replace=T)

    fst.filter <- fitness[fst] >= fitness[snd]
    snd.filter <- fitness[fst] <  fitness[snd]

    pop[, c(fst[fst.filter], snd[snd.filter])]
}

# Pick n/4 best, n/4 by roulette and n/2 by tournament select.
hybridSelect <- function(pop, fitness, n)
{
  h  <- round(n/2)
  hh <- round(h/2)
  cbind(dSelect(pop, fitness, hh),
        rouletteWheelSelect1(pop, fitness, hh),
        tournamentSelect(pop, fitness, h))
}

# Ordered crossover function
# orderedX : permutation, permutation → permutation list(2)
orderedX <- function(perm1, perm2)
{
    n  <- length(perm1)
    # Start and end incides.
    is <- sort(sample(n, 2))

    # Init children
    child1 <- perm2[is[1]:is[2]]
    child2 <- perm1[is[1]:is[2]]

    # Fill in the rest of the children
    rest1  <- c(perm1[(is[2]+1):n], perm1)
    rest2  <- c(perm2[(is[2]+1):n], perm2)

    child1 <- unique(c(child1, rest1[!(rest1 %in% child1)]))
    child2 <- unique(c(child2, rest2[!(rest2 %in% child2)]))

    # Rotate the children to match parents
    shift  <- n - is[1] + 1
    child1 <- c(tail(child1, -shift), head(child1, shift))
    child2 <- c(tail(child2, -shift), head(child2, shift))

    list(child1[!is.na(child1)],
         child2[!is.na(child2)])
}

# Improve the permutation by performing a few steps.
# localSearch : permutation, mat, fun, fun, number → permutation
localSearch <- function(perm, adj, score, neigh, steps)
{
    for (i in 1:steps) {
        neighs  <- t(neigh(perm))
        fitness <- apply(neighs, 2, score, A=adj)
        if (max(fitness) > score(adj, perm))
            perm <- neighs[, which.max(fitness)]
        else
            return(perm)
    }
    perm
}

# Identity function that can take more than one argument
# id : any, ... → any
id <- function(x, ...) x
