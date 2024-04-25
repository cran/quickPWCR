#' randompair
#' @description ramdonly pair players using reservoir sampling method.
#' @param players vector, indicating a list of players.
#' @param k numeric, indicating how many players each player will be paired with.
#' @param cores numeric, indicating the number of CUP cores to be used for parallel  
#' @return dataframe
#'
#' @references JVitter, J. S. (1985). Random sampling with a reservoir. 
#' ACM Transactions on Mathematical Software (TOMS), 11(1), 37-57.
#' 
#' @importFrom pbmcapply pbmclapply
#' @importFrom parallel detectCores
#' @importFrom Rcpp sourceCpp
#' @useDynLib quickPWCR
#' 
#' @export
#' @examples
#' players <- c(1, 'a', 'c', 4, 7, 2, 'w', 'y', 3, 0, 8)
#' pw <- quickPWCR::randompair(players = players, k = 3)
#' 


randompair <- function(players, k, cores = 1) {
  if (missing(players) || !is.vector(players)) {
    stop('please input a list of players')
  }
  if (missing(k) || !inherits(k, "numeric")) {
    stop('please input a positive integer for k')
  }
  if (k >= length(players)) {
    stop('k can not be larger than the length of players')
  }
  if (cores > parallel::detectCores()) {
    cores <- parallel::detectCores() - 1
  }
  # randomize the list of players
  r_players <- sample(players)
  num <- length(r_players)
  # create a random comparison using Reservoir Sampling
  pairwise_comp <- pbmcapply::pbmclapply(
    X = 1:num,
    FUN = reservoirSampling_pair,
    indexs = 1:num, 
    k = k,
    data = r_players,
    mc.cores = cores)
  pairwise_comp_df <- do.call(rbind, pairwise_comp)
  names(pairwise_comp_df) <- c('left', 'right')
  # remove duplicated pairs
  duplicate_pairwise <- duplicated(
    do.call(
      rbind.data.frame,
      pbmcapply::pbmclapply(
        1:nrow(pairwise_comp_df),
        function(x){
          d <- pairwise_comp_df[x, ]
          d <- d[order(as.matrix(d))]
          as.vector(as.matrix(d))
        },
        mc.cores = cores
      )
    )
  )
  return(pairwise_comp_df[!(duplicate_pairwise), ])
}

