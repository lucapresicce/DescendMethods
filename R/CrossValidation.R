#' CrossVD
#'
#' @param data 
#' @param K 
#' @param FUN 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
CrossVD = function(data, 
                   K = NULL, # se K null, fa loocv
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



