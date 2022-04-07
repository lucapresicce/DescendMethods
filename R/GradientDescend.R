#' Gradient Descend
#'
#' \loadmathjax Implements gradient descend method to find the coefficients \mjseqn{\beta} that minimize the following loss function
#' \mjsdeqn{L(\beta) = (X\beta - Y)^2}
#' In this implementation, the stepsize is kept constant. To reach the minimum, it employs the gradient 
#' \mjsdeqn{\nabla L(\beta) = 2X^{T}X\beta - 2X^{T}Y}
#' @param data [list] containing the data, elements must be named \code{X} and \code{Y}.
#' @param stepsize [numeric] it must be strictly positive. It is the step size, also called learning parameter. The suggested value is \code{0.1*(1/nrow(data$X))} 
#' @param init [vector] initial guesses of the parameter of interest. If \code{NULL}, values are all set equal to \code{1}. 
#' @param tol [numeric] it must be strictly positive. It is the tolerance on the error evaluation between subsequent iterations. It is use to determine the stopping criteria.
#' @param maxit [integer] it must be strictly positive. It is the maximum number of iterations.
#' @param verb [boolean] if \code{TRUE}, it prints more information about the status of the algorithm. 
#' @param check_loss [boolean] if \code{TRUE}, the algorithm stops when 
#'  \mjsdeqn{|| L(\beta(1) - L\beta(0))||\infty < tol} 
#' otherwise, it stops when \mjsdeqn{||\beta(1) - \beta(0))||_\infty < tol}.
#'
#' @return
#' @export
#' @import tictoc beepr
#'
GradD <- function(data,
               stepsize = 1e-4, # suggested : 0.1*(1/nrow(X))
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
  
  # control on step size
  if(stepsize <= 0) stop('Step size must be greater than zero')
  
  # control on maximum iteration
  if(maxit <= 0 & !as.integer(maxit)) stop('The Maximum number of Iteration must be a Positive Integer')
  
  # control on tolerance
  if(tol <= 0) stop('Step size must be greater than zero')
  
  
  ## 1 - init beta 
  
  # define structure of data and dimension
  X <- as.matrix(data$X)
  Y <- as.matrix(data$Y)
  n <- nrow(X)
  k <- ncol(X)
  
  if(is.null(init)){
    init <- rep(1, k)
  }
  
  beta_old <- as.matrix(init)
  if(check_loss) loss_old <- LossD(b = beta_old, X = X, Y = Y) # t(X %*% beta_old - Y) %*% (X %*% beta_old - Y)
  
  
  tictoc::tic()
  for (iter in 1:maxit) {
    
    ## 2 - gradient computation
    grad <- 2 * (t(X) %*% X %*% beta_old - t(X) %*% Y)
    
    ## 3 - update
    beta_new <- beta_old - (stepsize * grad)
    loss_new <-  LossD(b = beta_new, X = X, Y = Y) # t(X %*% beta_new - Y) %*% (X %*% beta_new - Y)
    
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
      'Minimum' =  LossD(b = beta_new, X = X, Y = Y), # t(X %*% beta_new - Y) %*% (X %*% beta_new - Y),
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
