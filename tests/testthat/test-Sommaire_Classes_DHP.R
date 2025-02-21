test_that("Sommaire_Classes_DHP return the expected data frame", {
  expect_for_Samare_sans_gaules_et_coupe_test <- readRDS(test_path("fixtures", "expect_for_Samare_sans_gaules_et_coupe_test.rds"))
  actual <- Sommaire_Classes_DHP(expect_for_Samare_sans_gaules_et_coupe_test)
  actual <- actual %>%
    mutate(across(where(is.numeric), ~ round(., 6))) %>%
    ungroup()

  expected <- as.data.frame(readRDS(test_path("fixtures", "expect_for_arbre_sommaire_classes_DHP.rds")))
  expected <- expected %>%
    mutate(across(where(is.numeric), ~ round(., 6))) %>%
    ungroup()

  expect_equal(actual, expected)
})
