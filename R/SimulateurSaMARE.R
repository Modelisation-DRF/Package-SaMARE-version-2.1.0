#' Appele le simulateur SaMARE et fournit les données initiales ainsi qu'un choix de paramètres pour la simulation.
#'
#' @param NbIter Valeur numérique du nombre d'iterations à effectuer (ex: 30).
#' @param Horizon Valeur numérique du nombre de périodes de 5 ans sur lesquelles le simulateur effectuera ses simulations (ex: 6 pour 30 ans de simulation).
#' @param RecruesGaules Variable prenant la valeur de "1" pour utiliser les paramètres de recrutement basé sur l'inventaire des gaules de la placette et de "0" pour utiliser le module de recrutement basé sur les arbres de dimension marchande.
#' @param Data Un dataframe contenant les valeurs de départ pour une liste d'arbres à simuler. Les champs: "Placette","NoArbre","Espece", "Etat","DHPcm","Vigueur","Nombre","Sup_PE","Annee_Coupe", "Latitude","Longitude","Altitude","Pente","Ptot","Tmoy","GrwDays", "Reg_Eco","Type_Eco", "MSCR","ntrt" doivent être présents. Si l'information sur certains champs n'est pas disponible, on peut le laisser vide.
#' @param Gaules Un dataframe contenant les valeurs de départ du nombre de gaules par espèce et par classe de diamètre. Cette information doit être fournie si le paramètre "RecruesGaules=1. Les champs: "Placette","Espece","DHPcm",#' "Nombre","Sup_PE" doivent être présents.
#' @param MCH Variable prenant la valeur de 1 en présence de maladie corticale du hêtre dans la placette et 0 lorsque la maladie est absente. Lorsque la maladie corticale est présente,la probabilité de mortalié des hêtres est estimée avec l'équation de l'avis technique AT-SSRF 20 de la Direction de la recherche forestière.
#'
#' @return Un dataframe contenant la liste des arbres, leur état, leur DHP, leur hauteur et leur volume pour chaque placette, chaque pas de simulation et chaque iteration.
#'
#' @examples
#' \dontrun{
#' # Simulation sur 10 ans, recrutement de base
#' result <- SimulSaMARE(NbIter = 10, Horizon = 2, RecruesGaules = 0, Data = Test2500m2)
#' # Simulation sur 10 ans, recrutement avec les gaules
#' result <- SimulSaMARE(
#'   NbIter = 10, Horizon = 2, RecruesGaules = 1, Data = Test2500m2,
#'   Gaules = GaulesTest2500m2
#' )
#' }
#' @export
SimulSaMARE <- function(NbIter, Horizon, RecruesGaules, Data, Gaules, MCH = 0, coreNumbers = 1) {
  select <- dplyr::select

  ################################ Lecture des fichiers de placette et de parametres ###################
  Data <- renommer_les_colonnes(Data)

  Gaules <- if (!missing(Gaules)) renommer_les_colonnes_gaules(Gaules) else NA

  Data <- Data %>%
    lazy_dt() %>%
    filter(DHPcm >= 9) %>%
    as.data.frame()

  anneeInventaire <- as.numeric(format(Sys.Date(), "%Y"))

  # Fichier des effets aleatoires
  CovParms <- MatchModuleCovparms
  EfCovParms <- EffetCovParms
  CovParmsGaules <- CovparmGaules

  ####### Fichier des parametres
  Para <- MatchModuleParameters

  Para <- Para %>%
    lazy_dt() %>%
    mutate(Effect = str_to_lower(Effect)) %>%
    rename(GrEspece = Ess_groupe) %>%
    select(-VegPotID, -Veg_Pot) %>%
    as.data.frame()

  ParaGaules <- ParametresGaules %>%
    lazy_dt() %>%
    rename(GrEspece = Ess_groupe) %>%
    as.data.frame()

  # Fichier des especes
  Sp <- Species
  SpGroups <- SpeciesGroups

  # Fichier des especes dans chacun des groupes d'especes
  MatchSpGroups <- MatchSpeciesGroups

  # Omega
  Omega <- MatchModuleOmega
  OmegaGaules <- OmegaGaulesFormat

  ############################# Construction de vecteurs pour simulation ##############################
  # Merge des fichers des especes et des groupes d'especes, et ensuite merge avec les Essence,
  # pour obtenir la liste des essences dans chaque groupe d'essences
  ListeSp <- merge(MatchSpGroups, SpGroups, by = "SpeciesGroupID") %>%
    lazy_dt() %>%
    merge(Sp, by = "SpeciesID") %>%
    rename(Espece = SpeciesName, GrEspece = SpeciesGroupName) %>%
    select(GrEspece, Espece) %>%
    as.data.frame()

  #################### Importation et préparation des donnes des arbre a simuler################
  # Fichier des arbres
  ColOrdre <- c(
    "Placette", "NoArbre", "Espece", "GrEspece", "Etat", "DHPcm", "Vigueur", "Nombre",
    "Sup_PE", "Annee_Coupe", "Latitude", "Longitude", "Altitude", "Pente", "Ptot", "Tmoy",
    "GrwDays", "Reg_Eco", "Type_Eco", "MSCR", "ntrt", "ABCD"
  )

  Data <- Data %>%
    lazy_dt() %>%
    left_join(ListeSp, by = "Espece") %>%
    as.data.frame()

  Data <- Data[ColOrdre]

  ################ Gaules######################
  if (RecruesGaules == 1) {
    ColOrdre <- c("Placette", "Espece", "GrEspece", "DHPcm", "Nombre", "Sup_PE")

    Gaules <- Gaules %>%
      lazy_dt() %>%
      inner_join(ListeSp, by = "Espece") %>%
      as.data.frame()

    Gaules <- Gaules[ColOrdre]

    ############# Sélection des placetes avec Gaules
    IndexGaules <- Gaules %>%
      lazy_dt() %>%
      group_by(Placette) %>%
      summarise() %>%
      as.data.frame()

    Data <- Data %>%
      lazy_dt() %>%
      inner_join(IndexGaules, by = "Placette") %>%
      as.data.frame()
  }

  ######### Selection nombre d'iteration et de l'horizon de simulation#############
  #################################################################################
  ##################### Génération des effets aléatoires###########################
  ###############################################################################
  RandPlacStep <- RandomPlacStep(
    CovParms = CovParms,
    Data = Data,
    NbIter = NbIter,
    NbPeriodes = Horizon
  )

  ###################### Gaules###########
  if (RecruesGaules == 1) {
    RandPlacStepGaules <- RandomPlacStepGaules(
      CovParms = CovParmsGaules, Data = Gaules,
      NbIter = NbIter
    )
  }

  ##############################################################################
  ########################### Copie des données initiale * Nb Iter##############
  ##############################################################################
  ListeIter <- rep(1:NbIter)
  ListeIter <- Data %>%
    lazy_dt() %>%
    group_by(Placette) %>%
    summarise() %>%
    merge(ListeIter) %>%
    rename(Iter = y) %>%
    mutate(PlacetteID = paste(Placette, "_", Iter, sep = "")) %>%
    relocate(PlacetteID, .before = Placette) %>%
    arrange(PlacetteID) %>%
    as.data.frame()

  registerDoFuture()
  plan(multisession)

  # liste de placette/iter, donc on parallélise les placettes/iter
  list_plot <- unique(ListeIter$PlacetteID)

  # utilisation de doRNG permet de controler la seed
  Simul <- foreach(x = list_plot, .combine = bind_rows) %do% {
    SaMARE(
      Random = RandPlacStep,
      RandomGaules = RandPlacStepGaules,
      Data = Data,
      Gaules = Gaules,
      ListeIter = ListeIter[ListeIter$PlacetteID == x, ],
      Annee_Inventaire = anneeInventaire,
      Horizon = Horizon,
      RecruesGaules = RecruesGaules,
      MCH = MCH,
      CovParms = CovParms,
      CovParmsGaules = CovParmsGaules,
      Para = Para,
      ParaGaules = ParaGaules,
      Omega = Omega,
      OmegaGaules = OmegaGaules
    )
  }

  plan(sequential)

  VarEco <- Data %>%
    lazy_dt() %>%
    group_by(Placette) %>%
    summarise(
      Sup_PE = first(Sup_PE), reg_eco = first(Reg_Eco), Type_Eco = first(Type_Eco),
      Altitude = first(Altitude), Ptot = first(Ptot), Tmoy = first(Tmoy)
    ) %>%
    mutate(veg_pot = substr(Type_Eco, 1, 3), milieu = substr(Type_Eco, 4, 4)) %>%
    as.data.frame()

  # renommer les variables pour l'équation de ht
  Simul <- Simul %>%
    lazy_dt() %>%
    inner_join(VarEco, relationship = "many-to-many", by = "Placette") %>%
    mutate(nb_tige = Nombre / Sup_PE / 25, step = (Annee - anneeInventaire) / 5 + 1) %>% # Conversion pour relation HD
    rename(
      id_pe = Placette, dhpcm = DHPcm, no_arbre = ArbreID, # IA: j'ai enlevé essence=GrEspece
      altitude = Altitude, p_tot = Ptot, t_ma = Tmoy, iter = Iter
    ) %>%
    as.data.frame()

  ass_ess_ht_vol2 <- ass_ess_ht_vol %>%
    lazy_dt() %>%
    group_by(GrEspece) %>%
    slice(1) %>%
    select(-Espece) %>%
    as.data.frame()

  Simul <- left_join(Simul, ass_ess_ht_vol2, by = "GrEspece")

  SimulHtVol1 <- Simul[which(Simul$Residuel == 0), ]
  nb_iter <- length(unique(SimulHtVol1$iter))
  nb_periodes <- Horizon + 1

  SimulHtVol1 <- SimulHtVol1 %>%
    lazy_dt() %>%
    rename(essence = essence_hauteur) %>%
    as.data.frame()

  ht <- TarifQC::relation_h_d(fic_arbres = SimulHtVol1, mode_simul = "STO", nb_iter = ifelse(nb_iter == 1, nb_iter + 1, nb_iter), nb_step = nb_periodes, reg_eco = TRUE, dt = 5) %>%
    lazy_dt() %>%
    select(-essence) %>%
    as.data.frame()

  ht <- ht %>%
    lazy_dt() %>%
    rename(essence = essence_volume) %>%
    as.data.frame()

  SimulHtVol2 <- TarifQC::cubage(fic_arbres = ht, mode_simul = "STO", nb_iter = ifelse(nb_iter == 1, nb_iter + 1, nb_iter), nb_step = nb_periodes) %>%
    lazy_dt() %>%
    select(-essence) %>%
    as.data.frame()

  rm(SimulHtVol1)
  # Garde juste les variables de hauteur et volume pour
  SimulHtVol2 <- SimulHtVol2[, c("id_pe", "Annee", "iter", "no_arbre", "hauteur_pred", "vol_dm3")]
  # joindre avec Simul pour garder les morts

  SimulHtVol <- Simul %>%
    lazy_dt() %>%
    left_join(SimulHtVol2, by = c("id_pe", "Annee", "no_arbre", "iter")) %>%
    rename(
      Placette = id_pe, DHPcm = dhpcm, ArbreID = no_arbre,
      Altitude = altitude, Ptot = p_tot, Tmoy = t_ma, Iter = iter
    ) %>%
    mutate(PlacetteID = paste(Placette, "_", Iter, sep = "")) %>%
    # enlever les variables qui étaient nécessaire seulement pour tarifqc
    select(-milieu, -veg_pot, -essence_hauteur, -essence_volume, -step, -nb_tige) %>%
    as.data.frame()

  return(SimulHtVol)
}
