# readFile : filename -> adjacency matrix
readFile <- function(filename)
{
  require(reshape2)

  # read
  tbl  <- read.table(filename, sep=" ", stringsAsFactors=F)

  # make sure columns 2 and 3 have the same factor levels
  names  <- union(tbl[, 2], tbl[, 3])
  people <- lapply(tbl[, 2:3], factor, levels=names)

  # numeric values of preferences
  pref <- as.character(tbl[, 1])
  pref[pref == "+"] <-  1
  pref[pref == "-"] <- -1
  pref <- as.numeric(pref)

  # rebuild table with numeric values
  nppl <- length(names)
  tbl  <- data.frame(pref, lapply(people, as.numeric))
  tbl  <- rbind(tbl, data.frame(pref=rep(0, nppl), V2=1:nppl, V3=1:nppl))

  # convert to adjacency matrix
  A <- acast(tbl, V2 ~ V3, value.var = "pref", fill=0)

  # make the graph undirected
  A = A + t(A)

  colnames(A) <- names
  rownames(A) <- names

  A
}

# scoreFun : adjacency matrix, permutation -> score
scoreFun <- function(A, perm)
{
  n <- nrow(A)

  # row indices according to permutation
  Ar <- matrix(rep(perm, n), nrow=n)

  # distance between two students is equal to:
  #   col.index - row.index - 1 * -weight
  #sum((t(Ar) - Ar - 1) * -A) / 2 # (/ 2) = upper.tri(A, diag=F)
  sum(((t(Ar) - Ar - 1) * -A)[upper.tri(A, diag=F)])
}

# plotGraph : adjacency matrix -> plot
plotGraph <- function(A, ...)
{
  require(igraph)


  # create graph, make sure weights of 0 are treated as edges
  g <- graph_from_adjacency_matrix( A + .1
                                  , weighted=T
                                  , mode="upper"
                                  , diag=F)

  E(g)$weight <- E(g)$weight - .1
  weights     <- E(g)$weight

  edge.cols <- c("red", "red", "gray", "green", "green")[weights + 3]

  layoutrng <- 2*pi*1:nrow(A) / nrow(A)
  layoutFun <- function(i) matrix(c( sin(layoutrng)
                                   , cos(layoutrng)), ncol=2)

  plot( g
      , edge.label   = weights
      , vertex.color = "gray"
      , edge.color   = edge.cols
      , edge.width   = sapply(weights, function(x) 2 * max(1, abs(x)))
      , layout       = layoutFun
      , ...)
}
