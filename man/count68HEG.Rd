% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gaules_68_HEG.R
\name{count68HEG}
\alias{count68HEG}
\title{Fonction qui calcul le nombre de Gaules de hêtre à grandes feuilles classes de 6 ou 8 cm de diamètre lorsquelle sont présente.
Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière.
Les prévisions sont basées sur un modèle de type Zero inflated}
\usage{
count68HEG(RecGaules, Ratio, Rec, Iterj, RandomPlacGaules, Para.68_HEG)
}
\arguments{
\item{RecGaules}{Dataframe qui contient les information sur la distribution des gaules dans la placette}

\item{Ratio}{Un dataframe qui contient la répartition du nombre de gaule prédit entre les groupes d'espèces}

\item{Rec}{Un dataframe qui contient la prévision du nombre de recrues par groupes d'espèces}

\item{Iterj}{Itération en cours}

\item{RandomPlacGaules}{Un dataframe contenant les effets aléatoires à l'échelle de la placette du module de
recrutement basé sur les gaules et du module d'évolution des gaules}

\item{Para.68_HEG}{Paramètres de l'équation de prévision du nombre de gaules de hêtre à grande feuille de 6 et 8 cm de diamètre}
}
\value{
Retourne une prévision du nombre de gaules de 6 et 8 cm de DHP de hêtre à grande feuille lorsquelles sont présentes
}
\description{
Fonction qui calcul le nombre de Gaules de hêtre à grandes feuilles classes de 6 ou 8 cm de diamètre lorsquelle sont présente.
Cette fonction utilise une variante des paramètre publiée par Rijal et al. 2023 Journal canadien de la recherche forestière.
Les prévisions sont basées sur un modèle de type Zero inflated
}
