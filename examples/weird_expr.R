library("microbenchmark")

b <- microbenchmark({1+1}, (1+1), times=100L)
print(b)
