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
  tbl <- data.frame(pref, lapply(people, as.numeric))

  # convert to adjacency matrix
  A <- acast(tbl, V2 ~ V3, value.var = "pref", fill=0)

  # make the graph undirected
  diag(A) <- NA

  A = A + t(A)

  colnames(A) <- names
  rownames(A) <- names

  A
}

# scoreFun : permutation, adjacency matrix -> score
scoreFun <- function(perm, A)
{
  n <- length(perm)

  sum(diag(A[ perm[1:(n - 1)]
            , perm[2:n] ]))
}

# plotGraph : adjacency matrix -> plot
plotGraph <- function(A, ...)
{
  require(igraph)

  g <- graph_from_adjacency_matrix(A, weighted=T, mode="undirected", diag=F)

  plot(g, edge.label=E(g)$weight, ...)
}
