test_that(" test-Recrutement_Vigueur", {
  RecSelect_test <- readRDS(test_path("fixtures", "RecSelect_test.rds"))
  Para.rec_vig_test <- readRDS(test_path("fixtures", "Para.rec_vig_test.rds"))
  actual <- rec_vig(RecSelect = RecSelect_test, latitude = 46.7, Iterj = 1L, Para.rec_vig = Para.rec_vig_test)

  actual <- data.frame(V1 = actual)
  actual <- actual %>% mutate(V1 = round(V1, 7))

  expectResult_vigeur_test <- readRDS(test_path("fixtures", "expectResult_vigeur_test.rds"))
  expected <- data.frame(V1 = expectResult_vigeur_test)
  expected <- expected %>% mutate(V1 = round(V1, 7))

  expect_equal(actual, expected)
})
