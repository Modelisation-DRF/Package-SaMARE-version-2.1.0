[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0) [![R-CMD-check](https://github.com/Modelisation-DRF/Package-SaMARE-version-2.1.0/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Modelisation-DRF/Package-SaMARE-version-2.1.0/actions/workflows/R-CMD-check.yaml)

## Le package SimulateurSaMARE

Auteurs: 
Hugues Power - Ministère des Ressources Naturelles et des Forêts du Québec

Junior Peumi - Ministère des Ressources Naturelles et des Forêts du Québec

## Introduction
Un package pour la simulation de l'accroissement, de la mortalité et du recrutement des arbres pour les forêts feuillues et mixtes du Québec (SaMARE).


## Documentation et références
Non disponibles pour l'instant.

## Dépendences
Ce package dépend des packages ExtractMap, TarifQC et Billonage.

TarifQC est disponible ici: https://github.com/Modelisation-DRF/TarifQC

ExtractMap est disponible ici: https://github.com/Modelisation-DRF/ExtractMap

Billonage est disponible ici: https://github.com/Modelisation-DRF/Billonage


## Comment installer le package dans R
```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
require(remotes)
install_github("https://github.com/Modelisation-DRF/Package-SaMARE-version-2.1.0", ref="main", auth_token = "demander_un_token")
```

## Historique des versions
| Date |  Version  | Issues |      Détails     |
|:-----|:---------:|:-------|:-----------------|
| 2024-04-05 |	2.1.0 |		Version stable |
| 2025-02-04 |  2.3.1 |   Version incluant les corrections et améliorations effectuées suites aux validations des chercheurs sans la                              validation statistique du modèle (calcul biais et erreurs)|
| 2025-04-16 | 2.3.2 |    Version avec limitation de la variable temps après coupe|
| 2025-04-16 | 2.3.3 |    Correction de la prévision du nombre de gaules de hêtre de 6 et 8 cm|
