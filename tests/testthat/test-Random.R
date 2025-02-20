test_that("RandomPlacStepGaules return the expected data frame ", {
  set.seed(NULL)
  set.seed(3)

  actual <- RandomPlacStepGaules(CovparmGaules, GaulesTest2500m2, 2)
  set.seed(NULL)
  expected <- as.data.frame(readRDS(test_path("fixtures", "expected_for_RandomPlacStepGaules.rds")))
  expect_equal(actual, expected)
})
