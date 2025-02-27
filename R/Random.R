#' Simule les effets aléatoires à l'échelle de la placette et de la période de simulation pour les modules originaux du modèle SaMARE
#'
#' @param CovParms Un dataframe contenant la variance des effets aléatoires en lien avec leur module respectif.
#' @param Data  Un dataframe contenant les arbres à simuler pour lesquels la placette sera extraite afin d'y générer des effets aléatoire.
#' @param NbIter Nombre d'itérations de la simulation.
#' @param NbPeriodes Nombre de périodes pour lesquelles les effets aléatoires seront simulés.
#'
#' @return Une liste d'effets aléatoires à l'échelle de la placette et de la période de simulation.
#'
#' @export
RandomPlacStep <- function(CovParms, Data, NbIter, NbPeriodes) {
  select <- dplyr::select
  Placettes <- unique(Data$Placette)

  CovPlac <- CovParms %>%
    lazy_dt() %>%
    filter(Subject == "placette") %>%
    select(SubModuleID, ParameterEstimate) %>%
    as.data.frame()

  suppressMessages(
    RandPlac <- data.frame("Placette" = rep(Placettes, 8)) %>%
      lazy_dt() %>%
      arrange(Placette) %>%
      mutate(SubModuleID = rep(c(1, 2, 3, 4, 5, 6, 7, 8), length(Placettes)), NbIter = NbIter) %>%
      arrange(Placette, SubModuleID) %>%
      inner_join(CovPlac) %>%
      as.data.frame()
  )

  randPlac <- function(RandPlac) {
    SdPara <- sqrt(RandPlac$ParameterEstimate)
    rand <- data.frame(
      "Placette" = RandPlac$Placette, "Iter" = rep(1:RandPlac$NbIter),
      "SubModuleID" = RandPlac$SubModuleID,
      "RandomPlac" = rnorm(n = RandPlac$NbIter, mean = 0, sd = SdPara)
    )
  }

  # DTPLYR AND DATA.TABLE DON'T LIKE map/lapply, GIVES DIFFERENT VALUES THAN DPLYR (BUT DTPLYR == DATA.TABLE)
  PredRandPlac <- RandPlac %>%
    mutate(ID = paste(Placette, "_", SubModuleID)) %>%
    group_by(ID) %>%
    nest() %>%
    mutate(RandPlac = map(data, randPlac)) %>%
    ungroup() %>%
    dt_unnest(RandPlac) %>%
    select(-data, -ID)

  # PredRandPlac <- RandPlac %>%
  #   lazy_dt() %>%
  #   mutate(ID = paste(Placette, "_", SubModuleID)) %>%
  #   group_by(ID) %>%
  #   dt_nest() %>%
  #   as.data.frame() %>%
  #   mutate(RandPlac = map(data, randPlac)) %>%
  #   lazy_dt() %>%
  #   ungroup() %>%
  #   dt_unnest(RandPlac) %>%
  #   select(-data, -ID) %>%
  #   as.data.frame()

  PredRandPlac <- PredRandPlac %>%
    lazy_dt() %>%
    slice(rep(1:n(), each = NbPeriodes)) %>%
    mutate(Step = rep(1:NbPeriodes, NbIter * length(Placettes) * 8)) %>%
    as.data.frame()

  #################################### Random Step####################
  CovStep <- CovParms %>%
    lazy_dt() %>%
    filter(Subject == "step") %>%
    select(SubModuleID, ParameterEstimate) %>%
    as.data.frame()

  suppressMessages(
    RandStep <- data.frame("Placette" = rep(Placettes, 5 * NbPeriodes)) %>%
      lazy_dt() %>%
      arrange(Placette) %>%
      mutate(SubModuleID = rep(c(1, 2, 6, 7, 8), length(Placettes) * NbPeriodes)) %>%
      arrange(Placette, SubModuleID) %>%
      mutate(Step = rep(1:NbPeriodes, length(Placettes) * 5), NbIter = NbIter) %>%
      inner_join(CovStep) %>%
      as.data.frame()
  )

  randStep <- function(RandStep) {
    SdPara <- sqrt(RandStep$ParameterEstimate)
    rand <- data.frame(
      "Placette" = RandStep$Placette, "Iter" = rep(1:RandStep$NbIter),
      "SubModuleID" = RandStep$SubModuleID, "Step" = RandStep$Step,
      "RandomStep" = rnorm(n = RandStep$NbIter, mean = 0, sd = SdPara)
    )
  }

  # DTPLYR AND DATA.TABLE DON'T LIKE map/lapply, GIVES DIFFERENT VALUES THAN DPLYR (BUT DTPLYR == DATA.TABLE)
  PredRandStep <- RandStep %>%
    mutate(ID = paste(Placette, "_", SubModuleID, "_", Step)) %>%
    group_by(ID) %>%
    nest() %>%
    mutate(RandStep = map(data, randStep)) %>%
    ungroup() %>%
    unnest(RandStep) %>%
    select(-data, -ID)

  # PredRandStep <- RandStep %>%
  #   lazy_dt() %>%
  #   mutate(ID = paste(Placette, "_", SubModuleID, "_", Step)) %>%
  #   group_by(ID) %>%
  #   dt_nest() %>%
  #   as.data.frame() %>%
  #   mutate(RandStep = map(data, randStep)) %>%
  #   lazy_dt() %>%
  #   ungroup() %>%
  #   dt_unnest(RandStep) %>%
  #   select(-data, -ID) %>%
  #   as.data.frame()

  suppressMessages(
    Random <- left_join(PredRandPlac, PredRandStep)
  )

  return(Random)
}

#' Simule les effets aléatoires à l'échelle de la placette pour les modules de recrutement basé sur les gaules et d'évolution des gaules de SaMARE
#'
#' @param CovParms Un dataframe contenant la variance des effets aléatoires en lien avec leur module respectif
#' @param Data Un dataframe contenant les informations sur les gaules pour lesquels la placette sera extraite afin d'y générer des effets aléatoire
#' @param NbIter Nombre d'itérations de la simulation
#'
#' @return Une liste d'effets aléatoires à l'échelle de la placette
#'
#' @export
RandomPlacStepGaules <- function(CovParms, Data, NbIter) {
  select <- dplyr::select
  Placettes <- unique(Data$Placette)

  CovPlac <- CovParms %>%
    lazy_dt() %>%
    filter(Subject == "placette") %>%
    select(SubModuleID, ParameterEstimate, response) %>%
    as.data.frame()

  suppressMessages(
    RandPlac <- data.frame("Placette" = rep(Placettes, 13)) %>%
      lazy_dt() %>%
      arrange(Placette) %>%
      mutate(
        SubModuleID = rep(c(10, 10, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16), length(Placettes)),
        response = rep(c(
          "pi", "count", "count", "pi", "count", "pi", "count", "pi", "count",
          "pi", "count", "pi", "count"
        ), length(Placettes)), NbIter = NbIter
      ) %>%
      arrange(Placette, SubModuleID) %>%
      inner_join(CovPlac) %>%
      as.data.frame()
  )

  randPlac <- function(RandPlac) {
    rand <- data.frame(
      "Placette" = RandPlac$Placette, "Iter" = rep(1:RandPlac$NbIter),
      "SubModuleID" = RandPlac$SubModuleID,
      "response" = RandPlac$response,
      "RandomPlac" = rnorm(n = RandPlac$NbIter, mean = 0, sd = RandPlac$ParameterEstimate)
    )
  }

  # DTPLYR AND DATA.TABLE DON'T LIKE map/lapply, GIVES DIFFERENT VALUES THAN DPLYR (BUT DTPLYR == DATA.TABLE)
  PredRandPlac <- RandPlac %>%
    mutate(ID = paste(Placette, "_", SubModuleID, "_", response)) %>%
    group_by(ID) %>%
    nest() %>%
    mutate(RandPlac = map(data, randPlac)) %>%
    ungroup() %>%
    unnest(RandPlac) %>%
    select(-data, -ID)

# PredRandPlac <- RandPlac %>%
#   lazy_dt() %>%
#   mutate(ID = paste(Placette, "_", SubModuleID, "_", response)) %>%
#   group_by(ID) %>%
#   dt_nest() %>%
#   as.data.frame() %>%
#   mutate(RandPlac = map(data, randPlac)) %>%
#   lazy_dt() %>%
#   ungroup() %>%
#   dt_unnest(RandPlac) %>%
#   select(-data, -ID) %>%
#   as.data.frame()

  PredRandPlac <- PredRandPlac %>%
    lazy_dt() %>%
    slice(rep(1:n())) %>%
    as.data.frame()
}
