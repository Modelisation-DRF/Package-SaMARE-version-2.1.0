# Tous les fichiers internes excel/csv/sas7bdat doivent être convertis en en seul fichier rda nommé sysdata.rda sous /R
# Tous les fichiers d'exemples doivent être convertis individuellement en rda et mis sous /data
# le fichier avec le code pour créer le fichier sysdata.rda doit être sauvegardé sous R/data-raw

# exemple:
# param_tarif = read.sas7bdat("c:/Mes docs/ data/ beta_volume.sas7bdat")
# param_ht = read.sas7bdat("c:/Mes docs/ data/ beta_ht.sas7bdat")
# Puis utiliser la ligne de code suivant (toujours dans le projet du package)
# usethis::use_data(param_tarif, param_ht, internal=TRUE): ça fonctionne seulement si le projet est un package

# library(readxl)
# library(sas7bdat)


# write_delim(MatchModuleCovparms, "data_raw\\Parametre_Samare\\MatchModuleCovparms.csv", delim = ';')
MatchModuleCovparms <- fread("data-raw\\Parametre_Samare\\MatchModuleCovparms.csv")

# write_delim(AttribQual, "data_raw\\Parametre_Samare\\AttribQual.csv", delim = ';')
AttribQual <- fread("data-raw\\Parametre_Samare\\AttribQual.csv")

# write_delim(EffetCovParms, "data_raw\\Parametre_Samare\\EffetCovParms.csv", delim = ';')
EffetCovParms <- fread("data-raw\\Parametre_Samare\\EffetCovParms.csv")

# write_delim(MatchModuleOmega, "data_raw\\Parametre_Samare\\MatchModuleOmega.csv", delim = ';')
MatchModuleOmega <- fread("data-raw\\Parametre_Samare\\MatchModuleOmega.csv")

# write_delim(MatchModuleParameters, "data_raw\\Parametre_Samare\\MatchModuleParameters.csv", delim = ';')
MatchModuleParameters <- fread("data-raw\\Parametre_Samare\\MatchModuleParameters.csv")

# write_delim(OmegaEvolQual, "data_raw\\Parametre_Samare\\OmegaEvolQual.csv", delim = ';')
OmegaEvolQual <- fread("data-raw\\Parametre_Samare\\OmegaEvolQual.csv")

# write_delim(OmegaGaulesFormat, "data_raw\\Parametre_Samare\\OmegaGaulesFormat.csv", delim = ';')
OmegaGaulesFormat <- fread("data-raw\\Parametre_Samare\\OmegaGaulesFormat.csv")

# write_delim(ParametresEvolQual, "data_raw\\Parametre_Samare\\ParametresEvolQual.csv", delim = ';')
ParametresEvolQual <- fread("data-raw\\Parametre_Samare\\ParametresEvolQual.csv")

# write_delim(ParametresGaules, "data_raw\\Parametre_Samare\\ParametresGaules.csv", delim = ';')
ParametresGaules <- fread("data-raw\\Parametre_Samare\\ParametresGaules.csv")

# write_delim(CovparmGaules, "data_raw\\Parametre_Samare\\CovparmGaules.csv", delim = ';')
CovparmGaules <- fread("data-raw\\Parametre_Samare\\CovparmGaules.csv")

# write_delim(Species, "data_raw\\Parametre_Samare\\Species.csv", delim = ';')
Species <- fread("data-raw\\Parametre_Samare\\Species.csv")

# write_delim(SpeciesGroups, "data_raw\\Parametre_Samare\\SpeciesGroups.csv", delim = ';')
SpeciesGroups <- fread("data-raw\\Parametre_Samare\\SpeciesGroups.csv")

# write_delim(MatchSpeciesGroups, "data_raw\\Parametre_Samare\\MatchSpeciesGroups.csv", delim = ';')
MatchSpeciesGroups <- fread("data-raw\\Parametre_Samare\\MatchSpeciesGroups.csv")

# write_delim(SubModuleID, "data_raw\\Parametre_Samare\\SubModuleID.csv", delim = ';')
SubModuleID <- fread("data-raw\\Parametre_Samare\\SubModuleID.csv")




# créer les fichiers d'association d'essences pour les équations ht et volume
# en utilisant les mêmes associations que dans Samare2018-Capsis
# pour le volume du BOJ, on utilise l'ERS dans le fichier MatchSpeciesGroups (VolumeSpeciesID=10) au lieu du BOJ (VolumeSpeciesID=2)
# vérifier avec Filip si c'est volontaire (dans Artemis-Capsis, on utilise le volume du BOJ pour le BOJ)

VolMatchSpeciesGroups <- fread("data-raw/0_VolMatchSpeciesGroups.csv")[, -c("HarvestSpeciesID", "VegPotID")]
EssenceID_Ht <- fread("data-raw/0_SpeciesHD.csv")
EssenceID_Vol <- fread("data-raw/0_SpeciesVol.csv")

VolMatchSpeciesGroups <- VolMatchSpeciesGroups[Species, on = "SpeciesID", nomatch = NULL][, -c("SpeciesID")]
VolMatchSpeciesGroups <- VolMatchSpeciesGroups[SpeciesGroups, on ="SpeciesGroupID", nomatch = NULL][, -c("SpeciesGroupID", "OldID")]
VolMatchSpeciesGroups <- VolMatchSpeciesGroups[EssenceID_Ht, on = "RelationHDSpeciesID", nomatch = NULL][, -c("RelationHDSpeciesID")]

ass_ess_ht_vol <- setnames(VolMatchSpeciesGroups[EssenceID_Vol, on = "VolumeSpeciesID", nomatch = NULL], c("SpeciesName", "SpeciesGroupName"), c("Espece", "GrEspece"))

# tous les fichiers à mettre dans le fichier sysdata.rda
usethis::use_data(MatchModuleCovparms, AttribQual, EffetCovParms, MatchModuleOmega, MatchModuleParameters, OmegaEvolQual, ParametresEvolQual,
  OmegaGaulesFormat, ParametresGaules, CovparmGaules,
  Species, SpeciesGroups, MatchSpeciesGroups, ass_ess_ht_vol,
  SubModuleID,
  internal = TRUE, overwrite = TRUE
)
