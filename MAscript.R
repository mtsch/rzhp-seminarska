# 
A3 <- readFile("DS3.R")
system.time(res <- MA(A3,
                      n.iter=10,
                      select=dTournamentSelect,
                      improveFun=multiStepLocalSearch
                      steps=100,
                      neigh=n1
                      ))
# Final score: 1972
# Time:        257.353
