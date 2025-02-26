test_that("SortieDendroIterSamare return the expected data frame ", {
  actual <- readRDS(test_path("fixtures", "expect_for_entree_dendro_samare_test.rds"))
  actual <- SortieDendroIterSamare(SimulHtVol = actual)

  expected <- readRDS(test_path("fixtures", "expect_for_sortie_dendroIter_samare_test.rds"))

  expect_equal(actual, expected)
})
