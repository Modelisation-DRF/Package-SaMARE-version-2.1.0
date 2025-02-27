test_that("simulateur SaMARE return the expected data with Gaules and coupe MCH=0", {
  set.seed(NULL)
  set.seed(3)
  RNGkind("default")

  # TarifQc renvoit des vol_dm3 +- 5 de différence avec le résultat attendu
  actual <- SimulSaMARE(NbIter = 2, Horizon = 6, RecruesGaules = 1, Data = Test400m2Coupe, Gaules = GaulesTest2500m2, MCH = 0) %>%
    select(-vol_dm3)

  set.seed(NULL)

  expected <- as.data.frame(readRDS(test_path("fixtures", "expect_test_for_Simulateur_Samare.rds")))%>%
    select(-vol_dm3)

  expect_equal(actual, expected)
})

# test_that("simulateur SaMARE return the expected data with Gaules and coupe MCH=1", {
#   set.seed(NULL)
#   set.seed(3)
#
#   # TarifQc renvoit des vol_dm3 +- 5 de différence avec le résultat attendu
#   actual <- SimulSaMARE(NbIter = 2, Horizon = 6, RecruesGaules = 1, Data = Test400m2Coupe, Gaules = GaulesTest2500m2, MCH = 1) %>%
#     mutate(hauteur_pred = round(hauteur_pred, 1)) %>%
#     select(-vol_dm3)
#
#   set.seed(NULL)
#
#   expected <- as.data.frame(readRDS(test_path("fixtures", "expect_test_for_Simulateur_Samare_MCH.rds"))) %>%
#     mutate(hauteur_pred = round(hauteur_pred, 1)) %>%
#     select(-vol_dm3)
#   expect_equal(actual, expected)
# })
#
# test_that("simulateur SaMARE return the expected data with gaules and without Gaules MCH=0", {
#   set.seed(NULL)
#   set.seed(3)
#
#   # TarifQc renvoit des vol_dm3 +- 5 de différence avec le résultat attendu
#   actual <- SimulSaMARE(NbIter = 2, Horizon = 6, RecruesGaules = 0, Data = Test400m2Coupe, MCH = 0) %>%
#     mutate(hauteur_pred = round(hauteur_pred, 1)) %>%
#     select(-vol_dm3)
#
#   set.seed(NULL)
#
#   expected <- as.data.frame(readRDS(test_path("fixtures", "expect_for_Samare_sans_gaules_et_coupe_test.rds"))) %>%
#     mutate(hauteur_pred = round(hauteur_pred, 1)) %>%
#     select(-vol_dm3)
#   expect_equal(actual, expected)
# })
#
# test_that("simulateur SaMARE return the expected data with gaules and without Gaules MCH=1", {
#   set.seed(NULL)
#   set.seed(3)
#
#   # TarifQc renvoit des vol_dm3 +- 5 de différence avec le résultat attendu
#   actual <- SimulSaMARE(NbIter = 2, Horizon = 6, RecruesGaules = 0, Data = Test400m2Coupe, MCH = 1) %>%
#     mutate(hauteur_pred = round(hauteur_pred, 1)) %>%
#     select(-vol_dm3)
#
#   set.seed(NULL)
#
#   expected <- as.data.frame(readRDS(test_path("fixtures", "expect_for_Samare_sans_gaules_et_coupe_test_MCH.rds"))) %>%
#     mutate(hauteur_pred = round(hauteur_pred, 1)) %>%
#     select(-vol_dm3)
#
#   expect_equal(actual, expected)
# })
#
# test_that("simulateur SaMARE return the expected data with Gaules and without coupe MCH=0", {
#   set.seed(NULL)
#   set.seed(3)
#
#   # TarifQc renvoit des vol_dm3 +- 5 de différence avec le résultat attendu
#   actual <- SimulSaMARE(NbIter = 2, Horizon = 6, RecruesGaules = 1, Data = Test2500m2, Gaules = GaulesTest2500m2, MCH = 0) %>%
#     mutate(hauteur_pred = round(hauteur_pred, 0)) %>%
#     select(-vol_dm3)
#
#   set.seed(NULL)
#
#   expected <- as.data.frame(readRDS(test_path("fixtures", "expect_for_Samare_avec_gaules_et_coupe_test.rds"))) %>%
#     mutate(hauteur_pred = round(hauteur_pred, 0)) %>%
#     select(-vol_dm3)
#
#   expect_equal(actual, expected)
# })
#
# test_that("simulateur SaMARE return the expected data with Gaules and without coupe MCH=1", {
#   set.seed(NULL)
#   set.seed(3)
#
#   # TarifQc renvoit des vol_dm3 +- 5 de différence avec le résultat attendu
#   actual <- SimulSaMARE(NbIter = 2, Horizon = 6, RecruesGaules = 1, Data = Test2500m2, Gaules = GaulesTest2500m2, MCH = 1) %>%
#     mutate(hauteur_pred = round(hauteur_pred, 0)) %>%
#     select(-vol_dm3)
#   set.seed(NULL)
#
#   expected <- as.data.frame(readRDS(test_path("fixtures", "expect_for_Samare_avec_gaules_et_coupe_test_MCH.rds"))) %>%
#     mutate(hauteur_pred = round(hauteur_pred, 0)) %>%
#     select(-vol_dm3)
#
#   expect_equal(actual, expected)
# })
#
# test_that("simulateur SaMARE return the expected data without Gaules and coupe MCH=1", {
#   set.seed(NULL)
#   set.seed(3)
#
#   # TarifQc renvoit des vol_dm3 +- 5 de différence avec le résultat attendu
#   actual <- SimulSaMARE(NbIter = 2, Horizon = 6, RecruesGaules = 0, Data = Test2500m2, MCH = 1) %>%
#     mutate(hauteur_pred = round(hauteur_pred, -1)) %>%
#     select(-vol_dm3)
#
#   set.seed(NULL)
#
#   expected <- as.data.frame(readRDS(test_path("fixtures", "expect_for_Samare_avec_coupe_sans_gaules_test_MCH.rds")))%>%
#     mutate(hauteur_pred = round(hauteur_pred, -1)) %>%
#     select(-vol_dm3)
#
#   expect_equal(actual, expected)
# })
