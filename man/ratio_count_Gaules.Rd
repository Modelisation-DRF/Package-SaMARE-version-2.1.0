% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ratio_count_Gaules.R
\name{ratio_count_Gaules}
\alias{ratio_count_Gaules}
\title{Fonction qui qui prévoit la proportion du nombre de gaules par espèce pour la période
de simulation suivante. Cette fonction est la deuxième partie du modèle zero-inflated
de Rijal et al. 2023 et prévoit un ratio par groupe d'espèce lorsqu'il y a
présence de gaules.}
\usage{
ratio_count_Gaules(
  Ratio,
  Rec,
  RecGaules,
  t,
  st_tot0,
  latitude,
  longitude,
  prec,
  trt,
  t0_aj_,
  Iterj,
  RandomPlacGaules,
  Para.ratio_gaules
)
}
\arguments{
\item{Ratio}{Dataframe qui possède une ligne par groupe d'espèce dans lequel
les prévisions de ratios seront rapportées.}

\item{Rec}{Un dataframe qui contient la prévision du nombre de recrues
par groupes d'espèces.}

\item{RecGaules}{Dataframe qui contient les informations sur la distribution
des gaules dans la placette.}

\item{t}{La longueur du pas de simulation en annees (en annees).}

\item{st_tot0}{Surface terrière marchande de la placette au début du pas
de simulation.}

\item{latitude}{Latitude de la placette en degrés décimal.}

\item{longitude}{Longitude de la placette en degrés décimal.}

\item{prec}{Précipitations annuelles moyennes de la placette.}

\item{trt}{Variable distinguant les peuplements traités des témoins,
si St >26 = TEM.}

\item{t0_aj_}{Temps écoulé depuis la dernière coupe partielle.}

\item{Iterj}{Itération en cours.}

\item{RandomPlacGaules}{Un dataframe contenant les effets aléatoires à
l'échelle de la placette du module de
recrutement basé sur les gaules et du module d'évolution des gaules.}

\item{Para.ratio_gaules}{Paramètres de l'équation de la prévision du ratio du
nombre de gaules par espèce.}
}
\value{
Retourne une prévision du ratio du nombre de gaules pour
         chacun des groupes d'espèces lorsqu'il y a
         présence de gaules de l'espèce.
}
\description{
Fonction qui qui prévoit la proportion du nombre de gaules par espèce pour la période
de simulation suivante. Cette fonction est la deuxième partie du modèle zero-inflated
de Rijal et al. 2023 et prévoit un ratio par groupe d'espèce lorsqu'il y a
présence de gaules.
}
