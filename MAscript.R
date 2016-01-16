source("MA.R")

A3 <- readFile("DS3.csv")
system.time(res <- MA(A3,
                      n.iter=10,
                      select=dTournamentSelect,
                      improveFun=multiStepLocalSearch,
                      steps=100,
                      neigh=n3,
                      cores=2
                      ))
# Final score: ~1972
