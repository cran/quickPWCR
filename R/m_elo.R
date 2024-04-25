#' randompair
#' @description ramdonly pair players using reservoir sampling method.
#' @param df_pw dataframe, indicating a dataframe that includes the columns of 
#' winners and losers.
#' @param wl vector, indicating the column names of winners and losers (c('w', 'l')). 
#' The first column name is for winners and the second column name is for losers.
#' @param elo_randomisations numeric, indicating the number of interactions that 
#' the Elo rating system is run with the randomized pairwise comparisons
#' @param initial_rating The initial rating of the players at the beginning 
#' @param k numeric, the K-factor determines the amount of change to the updated ratings
#' @param cores numeric, indicating the number of CUP cores to be used for parallel 
#' processing
#' @return dataframe
#'
#' @references Glickman, M. E., & Jones, A. C. (1999). Rating the chess rating system. 
#' CHANCE-BERLIN THEN NEW YORK-, 12, 21-28.
#' 
#' @importFrom dplyr left_join
#' @importFrom dplyr join_by
#' @importFrom dplyr %>%
#' 
#' @export
#' @examples
#' df <- data.frame(a = c(1,6,0,4,'a','v',9,'n'), b = c('w',3,5,2,'d','j',8,'p'))
#' pw <- quickPWCR::m_elo(df_pw = df, 
#'                        wl = c('a', 'b'), 
#'                        elo_randomisations = 10, 
#'                        initial_rating = 1000, 
#'                        k = 100, 
#'                        cores = 1)
#' 

# the Elo rating system does not require a full pairwise comparison of 
# all possible match-ups. It is designed to update players' rankings 
# progressively as individual matches (or pairwise comparisons) are played. 
# This system is particularly useful when it is impractical to have every 
# player compete against every other player due to time or logistical constraints.

m_elo <- function(df_pw, 
                  wl, 
                  elo_randomisations=500, 
                  initial_rating=0, 
                  k=100, 
                  cores = 1){
  if (length(wl) != 2) {
    stop("There should be two column names to indicating 'Winner' and 'Loser' columns")
  }
  if (cores > parallel::detectCores()) {
    cores <- parallel::detectCores() - 1
  }
  names(df_pw)[names(df_pw) == wl[1]] <- 'Winner'
  names(df_pw)[names(df_pw) == wl[2]] <- 'Loser'
  allplayers <- unique(c(unique(df_pw$Winner), unique(df_pw$Loser)))
  rating <- rep(initial_rating, length(allplayers))
  df_j <- data.frame(p=allplayers)
  df_j$index <- as.numeric(row.names(df_j))
  # df_ <- dplyr::left_join(df_pw, df_j, by=dplyr::join_by(Winner==p))
  # df_ <- dplyr::left_join(df_, df_j, by=dplyr::join_by(Loser==p))
  df_ <- df_pw %>%
    left_join(df_j, by=c('Winner' = 'p')) %>%
    left_join(df_j, by=c('Loser' = 'p'))
  df_ <- df_[,c(3,4)]
  names(df_) <- c('Winner', 'Loser')
  # iterating through the mean elo iterations
  elo_list <- pbmcapply::pbmclapply(
    1:elo_randomisations,
    function(x) {
      df_s <- df_[sample(1:nrow(df_)), ] 
      return(elo(df_s, rating, initial_rating, k))
    },
    mc.cores = cores
  )
  # combine each elo rating column together
  df_elo <- data.frame(do.call('cbind', elo_list))
  # calculating mElo by taking the average Elo rating of each row
  m_elo <- rowMeans(df_elo, na.rm = TRUE)
  df_melo <- data.frame(img = allplayers, mEloRating = m_elo)
  return (df_melo)
}