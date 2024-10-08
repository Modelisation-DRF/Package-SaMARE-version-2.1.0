% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Validation_colonne.R
\name{valide_Latitude}
\alias{valide_Latitude}
\title{Fonction pour vérifier que les valeurs saisies dans la colonne 'Latitude' sont correctes.}
\usage{
valide_Latitude(data)
}
\arguments{
\item{data}{fichier des arbres}
}
\value{
retourne vrai ou faux s'il détecte des erreurs.

# Exemple avec un dataframe valide
valide_Latitude(data) # Devrait retourner TRUE

# Exemple avec un dataframe invalide (colonne 'Latitude' manquante)
valide_Latitude(data) # Devrait retourner FALSE

# Exemple avec un dataframe invalide (valeurs en dehors de l'intervalle)
valide_Latitude(data) # Devrait retourner FALSE

# Exemple avec un dataframe invalide (valeurs NA)
valide_Latitude(data) # Devrait retourner FALSE

# Exemple avec un dataframe invalide (plusieurs valeurs différentes pour 'Latitude' par 'Placette')
valide_Latitude(data) # Devrait retourner FALSE
}
\description{
Fonction pour vérifier que les valeurs saisies dans la colonne 'Latitude' sont correctes.
}
