test_that("simulated data", {
  set.seed(1)
  n <- 100
  x <- cbind(1, matrix(runif(n*5, 0, 10), nrow = n, ncol = 5))
  beta <- rnorm(6, 5, 5)
  y <- x %*% beta + rnorm(n)
  df <- list(X = x, Y = y)
  res <- sdg(data = df, verb = F)
  expect_equal(as.numeric(res$Beta_hat), beta, tolerance = 1)
})
