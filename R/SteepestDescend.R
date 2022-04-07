#' Steepest Descend
#'
#' \loadmathjax Implements steepest descend method to find the coefficients \mjseqn{\beta} that minimize the following loss function
#' \mjsdeqn{L(\beta) = (X\beta - y)^2}
#' In this implementation, the stepsize is updated at each iteration employing the gradient 
#' \mjsdeqn{\nabla L(\beta) = 2X^{T}X\beta - 2X^{T}y}
#' and the Hessian matrix 
#' \mjsdeqn{H L(\beta) = 4X^{T}X}
#' @inheritParams GradD
#'
#' @return
#' @export
#' @import tictoc beepr
#'
SteepD <- function(data,
               init = NULL,
               tol = 1e-4,
               maxit = 1e3L,
               verb = F,
               check_loss = F) {
  
  ## 0 - input controls
  
  # control on data
  if (!is.list(data) || !all(names(data) == c('X','Y')) ) {
    stop('data must be a list, with 2 elements: X & Y')
  }
  if(nrow(data$X)!=length(data$Y)) stop('X and Y must have the same number of observation')
  
  # control on maximum iteration
  if(maxit <= 0 & !as.integer(maxit)) stop('The Maximum number of Iteration must be a Positive Integer')
  
  # control on tolerance
  if(tol <= 0) stop('Step size must be greater than zero')
  
  
  ## 1 - deinfe utils, init beta and hessian
  
  # define structure of data and dimension
  X <- as.matrix(data$X)
  Y <- as.matrix(data$Y)
  n <- nrow(X)
  k <- ncol(X)
  
  # init beta
  if(is.null(init)){
    init <- rep(1, k)
  }
  
  beta_old <- as.matrix(init)
  if(check_loss) loss_old <- t(X %*% beta_old - Y) %*% (X %*% beta_old - Y)
  
  # compute hessian
  hes <- 4 * (t(X) %*% X)
  
  tictoc::tic()
  for (iter in 1:maxit) {
    
    ## 2 - gradient computation
    grad <- 2 * (t(X) %*% X %*% beta_old - t(X) %*% Y)
    
    ## 3 - update
    step <- sum(grad^2) / (t(grad) %*% hes %*% grad)
    beta_new <- beta_old - (as.double(step) * grad)
    loss_new <- t(X %*% beta_new - Y) %*% (X %*% beta_new - Y)
    
    ## 4 - error computation
    if(!check_loss){
      err <- max(abs(beta_new - beta_old))
    } else {
      err <- max(abs(loss_new - loss_old))
      loss_old <- loss_new
    }
    
    ## 5 - iterate
    beta_old <- beta_new
    
    
    if(verb) cat('\n Not reached yet the minimum, at the step: ', iter, ' the error is: ', err)
    
    ## 6 - stop rule
    if(err < tol) {
      if(verb) beepr::beep(1)
      Sys.sleep(2)
      break
    }
    
  }
  
  t <- tictoc::toc(quiet = T)
  
  if (verb) {
    return(list(
      'Beta_hat' = beta_new,
      'Minimum' = t(X %*% beta_new - Y) %*% (X %*% beta_new - Y),
      'Final_error' = err,
      'Num_iter' = ifelse(iter < maxit, iter, paste('Reach the Maximum number of Iteration: ', maxit)),
      'Time' = (t$toc - t$tic)
    ))
  } else {
    return(list(
      'Beta_hat' = beta_new,
      'Final_error' = err,
      'Num_iter' = ifelse(iter < maxit, iter, paste('Reach the Maximum number of Iteration: ', maxit)),
      'Time' = (t$toc - t$tic)
    ))
  }
  
}
