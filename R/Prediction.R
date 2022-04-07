#' PredictD
#'
#' @param model 
#' @param newdata 
#'
#' @return
#' @export
PredictD = function(coef, newdata){
   
  ## 0 - input controls
  
  # control on newdata
  if(!is.data.frame(newdata)) stop('newdata must be a dataframe')
  
  # check dimension
  if(ncol(newdata)!=length(coef)) stop('The number of columns of newdata in not compatible with the number of estimated parameters')
  
  
  # define dimension
  newdata <- as.matrix(newdata)
  n <- nrow(newdata)
  k <- ncol(newdata)
  
  # predict
  return( newdata%*%coef )
  
}



#' MseD
#'
#' @param model 
#' @param data 
#'
#' @return
#' @export
MseD = function(coef, data){
  
  # control on data
  if (!is.list(data) || !all(names(data) == c('X','Y')) ) {
    stop('data must be a list, with 2 elements: X & Y')
  }
  if(nrow(data$X)!=length(data$Y)) stop('X and Y must have the same number of observation')
  
  
  # check dimension
  if(ncol(data$X)!=length(coef)) stop('The number predictors in data in not compatible with the number of estimated parameters')
  
  # compute predicted values
  ypred = PredictD(coef, as.data.frame(data$X))
  
  # return MSE
  return( mean((data$Y - ypred)^2) )
  
  
}





















  