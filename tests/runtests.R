# Verbatim from examples in help("source")
sourceDir <- function(path, trace = TRUE, ...) {
   for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
      if(trace) cat(nm,":")
      source(file.path(path, nm), ...)
      if(trace) cat("\n")
   }
}
source(system.file("tests/test_regression.R", package = "microbenchmark"))
