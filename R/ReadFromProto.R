#' ReadFromProto
#'
#' @param res output from BayesLM
#'
#' @return
#' @export
ReadFromProto = function(res){
  out = list()
  out[[1]] = list()
  out[[2]] = list()
  for(it in 1:length(res)){
    state = RProtoBuf::read(State, res[[it]])
    out[[1]][[it]] = state$mu
    data = as.numeric(as.character(state$prec[[1]][[3]]))
    nrow = as.numeric(as.character(state$prec[[1]][[1]]))
    ncol = as.numeric(as.character(state$prec[[1]][[2]]))
    out[[2]][[it]] = matrix(data = data, nrow = nrow, ncol = ncol)
  }
  names(out) = c("mu","prec")
  out
}


#' ReadFromProto
#'
#' @param res output from BayesLM
#'
#' @return
#' @export
ReadFromBayesLM = function(res){
  out = list()
  out[[1]] = list()
  out[[2]] = list()
  for(it in 1:length(res)){
    state = RProtoBuf::read(State, res[[it]])
    out[[1]][[it]] = state$mu
    data = as.numeric(as.character(state$prec[[1]][[3]]))
    nrow = as.numeric(as.character(state$prec[[1]][[1]]))
    ncol = as.numeric(as.character(state$prec[[1]][[2]]))
    out[[2]][[it]] = matrix(data = data, nrow = nrow, ncol = ncol)
  }
  names(out) = c("mu","prec")
  out
}

