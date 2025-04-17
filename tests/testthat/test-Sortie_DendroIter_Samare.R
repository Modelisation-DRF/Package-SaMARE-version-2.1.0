test_that("SortieDendroIterSamare return the expected data frame ", {

  expect_for_Samare_sans_gaules_avec_coupe_et_MCH_test <- readRDS(test_path("fixtures", "expect_for_Samare_sans_gaules_avec_coupe_et_MCH_test.rds"))
  expect_for_sortie_dendroIter_samare_test <- readRDS(test_path("fixtures", "expect_for_sortie_dendroIter_samare_test.rds"))
  expect_equal(SortieDendroIterSamare(SimulHtVol = expect_for_Samare_sans_gaules_avec_coupe_et_MCH_test), expect_for_sortie_dendroIter_samare_test )
})
