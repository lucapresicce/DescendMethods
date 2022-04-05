test_that("simulated data", {
  set.seed(1)
  n <- 100
  x <- cbind(1, matrix(runif(n*3, 0, 10), nrow = n, ncol = 3))
  beta <- rnorm(4, 5, 5)
  y <- x %*% beta + rnorm(n)
  df <- list(X = x, Y = y)
  res <- gd(data = df, verb = F, maxit = 3000)
  expect_equal(as.numeric(res$Beta_hat), beta, tolerance = 1)
})

test_that("real data", {
  set.seed(1)
  n <- 100
  x <- cbind(1, matrix(runif(n*3, 0, 10), nrow = n, ncol = 3))
  beta <- rnorm(4, 5, 5)
  y <- x %*% beta + rnorm(n)
  df <- list(X = x, Y = y)
  res <- gd(data = df, verb = F, maxit = 3000)
  expect_equal(as.numeric(res$Beta_hat), beta, tolerance = 1)
})
