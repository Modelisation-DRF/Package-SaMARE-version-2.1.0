test_that("Test Recrutement_Dhp", {
  RecSelect_test <- readRDS(test_path("fixtures", "RecSelect_test.rds"))
  Para.rec_dhp_for_test <- readRDS(test_path("fixtures", "Para.rec_dhp_for_test.rds"))

  actual <- rec_dhp(
    RecSelect = RecSelect_test,
    st_tot0 = 27.6458293053024,
    dens_tot0 = 453.325,
    t = 5,
    ntrt = 1,
    Iterj = 1L,
    Para.rec_dhp = Para.rec_dhp_for_test
  )

  actual <- data.frame(V1 = actual)
  actual <- actual %>% mutate(V1 = round(V1, 7))


  Result_Recrutement_Dhp_test <- readRDS(test_path("fixtures", "Result_Recrutement_Dhp_test.rds"))
  expected <- data.frame(V1 = Result_Recrutement_Dhp_test)
  expected <- expected %>% mutate(V1 = round(V1, 7))

  expect_equal(actual, expected)
})
