test_that("SortieDendroSamare return the expected data frame ", {
  actual <- readRDS(test_path("fixtures", "expect_for_Samare_sans_gaules_et_coupe_test.rds"))
  actual <- SortieDendroSamare(SimulHtVol = actual)

  expected <- as.data.frame(readRDS(test_path("fixtures", "expect_for_sortie_dendro_samare_test.rds")))

  expect_equal(actual, expected)
})
