context("Regressions")

test_that("units argument is sane", {
  expect_error(microbenchmark(NULL, units=a))  
})
          
