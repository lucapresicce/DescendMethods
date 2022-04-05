test_that("simulated data", {
  set.seed(1)
  n <- 100
  x <- cbind(1, matrix(runif(n*3, 0, 10), nrow = n, ncol = 3))
  beta <- rnorm(4, 5, 5)
  y <- x %*% beta + rnorm(n)
  df <- list(X = x, Y = y)
  res <- GradD(data = df, verb = F, maxit = 3000)
  expect_equal(as.numeric(res$Beta_hat), beta, tolerance = 1)
})

test_that("simulated data - loss evaluation", {
  set.seed(1)
  n <- 100
  x <- cbind(1, matrix(runif(n*3, 0, 10), nrow = n, ncol = 3))
  beta <- rnorm(4, 5, 5)
  y <- x %*% beta + rnorm(n)
  df <- list(X = x, Y = y)
  res <- GradD(data = df, verb = F, check_loss = T, maxit = 3000)
  expect_equal(as.numeric(res$Beta_hat), beta, tolerance = 1)
})

test_that("real data", {
  # simple lm
  fit = lm(Girth ~ Volume, data = trees)
  x <- model.matrix.lm(fit)
  y = trees$Girth
  df <- list(X = x, Y = y)
  res <- GradD(data = df, stepsize = 1e-5, check_loss = T, maxit = 30000, verb = T)
  expect_equal(as.numeric(res$Beta_hat), as.numeric(coef(fit)), tolerance = 1)
})

test_that("real data - multiple", {
  # simple lm
  fit = lm(Girth ~ ., data = trees)
  x <- model.matrix.lm(fit)
  y = trees$Girth
  df <- list(X = x, Y = y)
  res <- GradD(data = df, check_loss = T,stepsize = 1e-8, maxit = 30000, verb = T)
  expect_equal(as.numeric(res$Beta_hat), as.numeric(coef(fit)), tolerance = 1)
})
