source("MA.R")

A3 <- readFile("DS3.csv")
system.time(res <- MA(A3,
                      n.iter=10,
                      select=dSelect,
                      improveFun=multiStepLocalSearch,
                      steps=100,
                      neigh=n3,
                      cores=2
                      ))
system.time(res <- MA(A3,
                      n.iter=10,
                      select=tournamentSelect,
                      improveFun=multiStepLocalSearch,
                      steps=10,
                      neigh=n3,
                      mutation.rate=0.1,
                      n.pool=100,
                      cores=2
                      ))
# Final score: ~1986
