# utils
#' @importFrom Rcpp evalCpp
#' @noMd
reservoirSampling_pair <- function(i, indexs, k, data) {
  indexs <- indexs[-(i)]
  p <- rep(i, k)
  samples <- reservoirSampling(indexs, k)
  p <- data[p]
  samples <- data[samples]
  out <- cbind(p, samples)
  return(as.data.frame(out))
}

#' @noMd
elo <- function(df_pw, ratings, initial_rating=0, k=100){
  # getting the number of pairs
  num_pw <- nrow(df_pw)
  mtx_pw <- as.matrix(df_pw) 
  scores <- eloCPP(mtx_pw, ratings, num_pw)
  return(data.frame(EloRating = scores))
}

