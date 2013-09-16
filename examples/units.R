library("microbenchmark")

b_unit <- microbenchmark(1+1, 1-1, 1, -1, 1e1, unit="relative")
b_none <- microbenchmark(1+1, 1-1, 1, -1, 1e1)

summary(b_unit)
print(b_unit, order="median")
print(b_unit, unit="t")

options("microbenchmark.unit"="f")
print(b_none)
print(b_none, unit="eps")
