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
    out[[1]][it] = state$mu
    state$prec[[1]]
    out[[2]][it] = as.numeric(as.character(state$prec[[1]][[3]]))
  }
}

