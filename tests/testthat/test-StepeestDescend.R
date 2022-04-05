test_that("simulated data", {
  set.seed(1)
  n <- 100
  x <- cbind(1, matrix(runif(n*10, 0, 10), nrow = n, ncol = 10))
  beta <- rnorm(11, 5, 5)
  y <- x %*% beta + rnorm(n)
  df <- list(X = x, Y = y)
  res <- sdg(data = df, verb = F)
  expect_equal(as.numeric(res$Beta_hat), beta, tolerance = 1)
})
