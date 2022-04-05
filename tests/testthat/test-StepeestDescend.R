test_that("simulated data", {
  set.seed(1)
  n <- 100
  x <- cbind(1, matrix(runif(n*5, 0, 10), nrow = n, ncol = 5))
  beta <- rnorm(6, 5, 5)
  y <- x %*% beta + rnorm(n)
  df <- list(X = x, Y = y)
  res <- SteepD(data = df, verb = F)
  expect_equal(as.numeric(res$Beta_hat), beta, tolerance = 1)
})

test_that("simulated data - loss evaluation", {
  set.seed(1)
  n <- 100
  x <- cbind(1, matrix(runif(n*5, 0, 10), nrow = n, ncol = 5))
  beta <- rnorm(6, 5, 5)
  y <- x %*% beta + rnorm(n)
  df <- list(X = x, Y = y)
  res <- SteepD(data = df, check_loss = T, verb = F)
  expect_equal(as.numeric(res$Beta_hat), beta, tolerance = 1)
})


test_that("real data", {
  # simple lm
  fit = lm(Girth ~ Volume, data = trees)
  x <- model.matrix.lm(fit)
  y = trees$Girth
  df <- list(X = x, Y = y)
  res <- SteepD(data = df, check_loss = T, verb = F)
  expect_equal(as.numeric(res$Beta_hat), as.numeric(coef(fit)), tolerance = 1)
})


test_that("real data - multiple", {
  # simple lm
  fit = lm(Girth ~ ., data = trees)
  x <- model.matrix.lm(fit)
  y = trees$Girth
  df <- list(X = x, Y = y)
  res <- SteepD(data = df, check_loss = T, verb = F)
  expect_equal(as.numeric(res$Beta_hat), as.numeric(coef(fit)), tolerance = 1)
})


