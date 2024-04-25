testthat::test_that("runs correctly", {
  players <- unique(round(runif(n=200, min=1, max=5000), 0))
  pw <- quickPWCR::randompair(players = players, k = 50)
  
  elo_ratings <- quickPWCR::m_elo(pw, 
                                  c('left', 'right'), 
                                  elo_randomisations = 100, 
                                  initial_rating = 100, 
                                  k = 10, 
                                  cores = 1)
  testthat::expect_type(pw, "list")
})