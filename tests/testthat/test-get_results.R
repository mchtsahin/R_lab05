

test_that("This application only supports these leagues: {EPL = 2021, BundesLiga = 2002, LaLiga = 2014, Ligue1 = 2015, SerieA = 2019}",{
  expect_error(get_results(list("Spain","EPL = 2021")))
  expect_error(get_results(list("Brazil","BundesLiga = 2002")))
  expect_error(get_results(list()))
})
