library("microbenchmark")

dyn.load("native.so")

do_nothing <- getNativeSymbolInfo("do_nothing")

res <- microbenchmark(.Call(do_nothing, NULL),
                      .Call("do_nothing", NULL),
                      times=100L,
                      control=list(warmup=2^20))
print(res)
print(relative_slowdown(res))
