library("microbenchmark")

microbenchmark(1 + 1, 1 + -1, 1 + --1, 1 + ---1,
               times=1000L, control=list(order="block"))
