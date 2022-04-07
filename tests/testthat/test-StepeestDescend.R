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


# No check loss

test_that("real data - simple linear regression", {
  # simple lm
  x <- as.matrix(trees$Height)
  y <- trees$Girth
  df <- list(X = x, Y = y)
  opt <- optim(par = rep(1, 1), fn = LossD, X = x, Y = y, method = c("BFGS"))
  res <- GradD(data = df, stepsize = 1e-6, check_loss = F, verb = F)
  expect_equal(as.numeric(res$Beta_hat), as.numeric(opt$par), tolerance = 1)
})



test_that("real data - multiple linear regression", {
  # multiple lm
  x <- as.matrix(trees[, -1])
  y <- trees$Girth
  df <- list(X = x, Y = y)
  opt <- optim(par = rep(1, 2), fn = LossD, X = x, Y = y, method = c("BFGS"))
  res <- GradD(data = df, stepsize = 1e-6, check_loss = F, verb = F)
  expect_equal(as.numeric(res$Beta_hat), as.numeric(opt$par), tolerance = 1)
})

# Real data tests


# No check loss

test_that("real data - simple linear regression", {
  # simple lm
  x <- as.matrix(trees$Height)
  y <- trees$Girth
  df <- list(X = x, Y = y)
  opt <- optim(par = rep(1, 1), fn = LossD, X = x, Y = y, method = c("BFGS"))
  res <- SteepD(data = df, check_loss = F, verb = F)
  expect_equal(as.numeric(res$Beta_hat), as.numeric(opt$par), tolerance = 1)
})



test_that("real data - multiple linear regression", {
  # multiple lm
  x <- as.matrix(trees[, -1])
  y <- trees$Girth
  df <- list(X = x, Y = y)
  opt <- optim(par = rep(1, 2), fn = LossD, X = x, Y = y, method = c("BFGS"))
  res <- SteepD(data = df, check_loss = F, verb = F)
  expect_equal(as.numeric(res$Beta_hat), as.numeric(opt$par), tolerance = 1)
})



# With check loss

test_that("real data - simple linear regression - L", {
  # simple lm
  x <- as.matrix(trees$Height)
  y <- trees$Girth
  df <- list(X = x, Y = y)
  opt <- optim(par = rep(1, 1), fn = LossD, X = x, Y = y, method = c("BFGS"))
  res <- SteepD(data = df, check_loss = T, verb = F)
  expect_equal(as.numeric(res$Beta_hat), as.numeric(opt$par), tolerance = 1)
})



test_that("real data - multiple linear regression - L", {
  # multiple lm
  x <- as.matrix(trees[, -1])
  y <- trees$Girth
  df <- list(X = x, Y = y)
  opt <- optim(par = rep(1, 2), fn = LossD, X = x, Y = y, method = c("BFGS"))
  res <- SteepD(data = df, check_loss = T, verb = F)
  expect_equal(as.numeric(res$Beta_hat), as.numeric(opt$par), tolerance = 1)
})


