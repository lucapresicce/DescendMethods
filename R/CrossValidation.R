#' CrossVD
#'
#' This function calculates the estimated K-fold or Leave-one-out cross-validation mean squared prediction error.
#' @param K [integer] the number of folds. Set equal to \code{NULL} to run a Leave-one-out cross validation.
#' If \code{K} is larger than the number of observations, the Leave-one-out cross validation is run by default.
#' @param get_mean [boolean] if \code{TRUE}, the CV-MSE is returned, otherwise the function returns the MSE computed for each fold.
#' @param OPT [function] the optimization function whose prediction power has to be tested. If can only be equal to 
#' \code{\link{GradD}} or \code{\link{SteepD}}.
#' @param ... optional arguments to OPT
#' @inheritParams GradD
#' 
#' @return if \code{get_mean} is \code{TRUE}, the CV-MSE is returned.
#' Otherwise the function returns a [vector] containing all MSE computed for each fold.
#' @export
CrossVD = function(data, 
                   K = NULL, 
                   get_mean = T,
                   OPT, ...){
  
  ## 0 - input controls
  
  # control on data
  if (!is.list(data) || !all(names(data) == c('X','Y')) ) {
    stop('data must be a list, with 2 elements: X & Y')
  }
  X = data$X
  Y = data$Y
  
  if(nrow(X)!=length(Y)) stop('X and Y must have the same number of observation')
  n = nrow(X)
  p = ncol(X)
  
  l = list(...)
  ## 1 - data splitting
  if(is.null(K)||K>=n){
    pb = txtProgressBar(min = 0, max = n, initial = 0, style = 3)
    MSE = unlist(lapply(1:n, FUN = function(i)
      {
        # fit without i
        df = list(X = X[-i,], Y = Y[-i])
        beta_hat = OPT(data = df, ...)$Beta_hat
        
        # compute prediction
        newdata = as.data.frame( t(X[i,]) )
        ypred = PredictD(coef = beta_hat, newdata = newdata)
        setTxtProgressBar(pb, i)
        return((ypred - Y[i])^2)
      }))
  } else{
    
    # K fold cross validation
    if(K <= 1 & !as.integer(K)) stop('The number of folds K must be a greater than 1 and positive integer')
    folds = sample(1:K, n, T)
    
    pb = txtProgressBar(min = 0, max = K, initial = 0, style = 3)
    MSE = unlist(lapply(1:K, FUN = function(k)
    {
      # fit for fold k
      df = list(X = X[folds!=k,], Y = Y[folds!=k])
      beta_hat = OPT(data = df, ...)$Beta_hat
      
      # compute prediction
      newdata = as.data.frame( X[folds==k,] )
      ypred = PredictD(coef = beta_hat, newdata = newdata)
      setTxtProgressBar(pb, k)
      return( mean((ypred - Y[folds==k])^2) )
    }))
    
  }
  if(get_mean) MSE = mean(MSE)
  
  return(MSE)
   
}



#' PcrossVD
#'
#' This function is the parallel version of \code{\link{CrossVD}}.
#' @param n_clust [integer] the number of clusters to use. If \code{NULL}, the function selects all available cores but one.
#' @inheritParams GradD
#'
#' @inherit CrossVD return
#' @export
#' @import doSNOW parallel
PcrossVD = function(data, 
                   K = NULL, # se K null, fa loocv
                   get_mean = T,
                   n_clust = NULL,
                   OPT, ...){
  
  ## 0 - input controls
  
  # control on data
  if (!is.list(data) || !all(names(data) == c('X','Y')) ) {
    stop('data must be a list, with 2 elements: X & Y')
  }
  X = data$X
  Y = data$Y
  
  if(nrow(X)!=length(Y)) stop('X and Y must have the same number of observation')
  n = nrow(X)
  p = ncol(X)
  
  ### ? trucco, se lo togli non funziona ? ###
  OPT(df, maxit=1)
  
  ## 1 - define the cluster
  if(is.null(n_clust) || n_clust >= parallel::detectCores()) n_clust <- parallel::detectCores() - 1
  if(n_clust <= 1  || !as.integer(n_clust)) stop('The number of Cluster must be a greater than 1 and positive integer')
  cluster <- makeCluster(n_clust, type = "SOCK")
  registerDoSNOW(cluster)
  
  ## 2 - cross validation
  
  if(is.null(K) || K>=n){
    MSE = unlist(
      snow::parLapply(cl = cluster,
                      x = 1:n,
                      fun = function(i)
      {
        # fit without i
        df = list(X = X[-i,], Y = Y[-i])
        beta_hat = OPT(data = df, ...)$Beta_hat
        
        # compute prediction
        newdata = as.data.frame( t(X[i,]) )
        ypred = PredictD(coef = beta_hat, newdata = newdata)
        return((ypred - Y[i])^2)
      }))
  } else{
    
    # K fold cross validation
    if(K <= 1 & !as.integer(K)) stop('The number of folds K must be a greater than 1 and positive integer')
    folds = sample(1:K, n, T)
    
    MSE = unlist(
      snow::parLapply(cl = cluster,
                      x = 1:K,
                      fun = function(k)
                      {
                        # fit without i
                        df = list(X = X[folds!=k,], Y = Y[folds!=k])
                        beta_hat = OPT(data = df, ...)$Beta_hat
                        
                        # compute prediction
                        newdata = as.data.frame( X[folds==k,] )
                        ypred = PredictD(coef = beta_hat, newdata = newdata)
                        return( mean((ypred - Y[folds==k])^2) )
                      }))
  }
  parallel::stopCluster(cluster)
  
  # 3 - reduce results
  if(get_mean) MSE = mean(MSE)
  
  return(MSE)
   
}



