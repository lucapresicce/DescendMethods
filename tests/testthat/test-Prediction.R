test_that("prediction - SteepD", {
  set.seed(1)
  n <- 100
  x <- cbind(1, matrix(runif(n*5, 0, 10), nrow = n, ncol = 5))
  beta <- rnorm(6, 5, 5)
  y <- x %*% beta + rnorm(n)
  df <- list(X = x, Y = y)
  res <- SteepD(data = df, verb = F)
  
  newdata = data.frame( matrix(1,nrow = 1,ncol=6) )
  predicted = PredictD(res$Beta_hat, newdata = newdata)
  
  ypred = as.matrix(newdata) %*% beta
  expect_equal(predicted, ypred, tolerance = 1)
})

test_that("prediction - SteepD (multidimensional)", {
  set.seed(1)
  n <- 100
  x <- cbind(1, matrix(runif(n*5, 0, 10), nrow = n, ncol = 5))
  beta <- rnorm(6, 5, 5)
  y <- x %*% beta + rnorm(n)
  df <- list(X = x, Y = y)
  res <- SteepD(data = df, verb = F)
  
  newdata = data.frame( matrix(1,nrow = 2,ncol=6) )
  predicted = PredictD(res$Beta_hat, newdata = newdata)

  ypred = as.matrix(newdata) %*% beta
  expect_equal(predicted, ypred, tolerance = 1)
})
