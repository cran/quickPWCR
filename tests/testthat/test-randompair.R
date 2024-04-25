testthat::test_that("runs correctly", {
  players <- unique(round(runif(n=100, min=1, max=5000), 0))
  pw <- quickPWCR::randompair(players = players, k = 30)
  
  testthat::expect_type(pw, "list")
})