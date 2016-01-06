library(reshape2)

# readFile : filename -> adjacency matrix
readFile <- function(filename)
{
  # read
  tbl  <- read.table(filename, sep=" ", stringsAsFactors=F)

  # make sure columns 2 and 3 have the same factor levels
  lvls   <- union(tbl[, 2], tbl[, 3])
  people <- lapply(tbl[, 2:3], factor, levels=lvls)

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
  A + t(A)
}

# scoreFun : permutation, adjacency matrix -> score
scoreFun <- function(perm, A)
{
  n    <- length(perm)
  from <- perm[1:(n - 1)]
  to   <- to  [2:n]

  sum(diag(A[ perm[1:(n - 1)]
            , perm[2:n] ]))
}
