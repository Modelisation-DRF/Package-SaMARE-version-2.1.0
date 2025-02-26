test_that("SortieDendroSamare return the expected data frame ", {
  actual <- readRDS(test_path("fixtures", "expect_for_entree_dendro_samare_test.rds"))
  actual <- SortieDendroSamare(SimulHtVol = actual)

  expected <- readRDS(test_path("fixtures", "expect_for_sortie_dendro_samare_test.rds"))

  expect_equal(actual, expected)
})
