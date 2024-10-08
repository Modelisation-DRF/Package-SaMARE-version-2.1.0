% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trouver_noms_des_colonnes_absents.R
\name{trouver_noms_absents_gaules}
\alias{trouver_noms_absents_gaules}
\title{Vérifier la présence des colonnes obligatoires dans le fichier des gaules}
\usage{
trouver_noms_absents_gaules(Data)
}
\arguments{
\item{Data}{Un dataframe représentant le fichier des gaules.}
}
\value{
Une liste des noms des colonnes manquantes.
}
\description{
La fonction \code{trouver_noms_absents_gaules} vérifie si toutes les colonnes obligatoires
sont présentes dans un dataframe représentant le fichier des gaules. Elle retourne une liste
des noms des colonnes manquantes, le cas échéant.
}
\examples{
\dontrun{
# Supposons que nous ayons un dataframe data avec les colonnes suivantes :
# "Placette", "Espece", "DHPcm", "Nombre"

# Appel de la fonction
# noms_absents <- trouver_noms_absents_gaules(data)

# Si la colonne "Sup_PE" est absente, la fonction retournera :
# [1] "sup_pe"
}

}
