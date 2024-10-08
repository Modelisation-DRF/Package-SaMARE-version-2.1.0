% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ConversionMSCR.R
\name{ConvMSCRVig}
\alias{ConvMSCRVig}
\title{Fonction qui permet de convertir le classement MSCR Boulet-2007 en vigueur
selon le classement de Majcen-1990.}
\usage{
ConvMSCRVig(Data, Para.ConvMSCRVig)
}
\arguments{
\item{Data}{Un dataframe contenant une ligne par arbre avec un champ MSCR
copntenant le classement MSCR de l'arbre. Les classements acceptés
sont M, S, C, MS ou CR.}

\item{Para.ConvMSCRVig}{Un dataframe  contenant les paramettres des équations
de conversion du classement MSCR en vigueur.}
}
\value{
Retourne un vecteur de probabilité que l'arbre soit vigoureux.
}
\description{
Fonction qui permet de convertir le classement MSCR Boulet-2007 en vigueur
selon le classement de Majcen-1990.
}
