#' Fonction qui prépare les graphiques à insérer dans l'interface utilisateur
#' cette function prend en parametre une sortie de la fonction SimulSaMARE
#'
#' @param SimulHtVol Un dataframe contenant les résultats de simulation pour chacune des iterations du simulateur SaMARE. Typiquement un résultat retourné par la fonction "SimulSaMARE".
#' @param Espece Un code de trois lettre majuscule présentant l'espèce pour laquelle on veut faire les graphiques. Le code d'espèce doit être celui d'un des groupes d'espèce de SaMARE ou le code "TOT" pour l'ensemble des espèces.
#' @param Variable  La variable pour laquelle le graphique d'évolution sera créé. Cette variable doit être une des variable de la sortie "Dendro-SaMARE" soit "Vol_HA", "ST_HA", "DQM","HDomM" ou "nbTi_HA"
#' @param listePlacette Vecteur contenant la liste des placettes à inclure dans le graphique
#'
#' @return  Une liste de deux graphiques, le premier montrant l'évolution d'une variables dendrométrique et le second la distribution en diamètre au début et à la fin de la simulation moyen quadratique et la hauteur dominante.
#'
#' @export
Graph <- function(SimulHtVol, Espece = "TOT", Variable = "ST_HA", listePlacette) {
  Data <- SortieDendroSamare(SimulHtVol) %>%
    lazy_dt() %>%
    filter(GrEspece == Espece & Placette %in% listePlacette) %>%
    mutate(Yvar = NA) %>%
    as.data.frame()

  Data <- Data %>%
    lazy_dt() %>%
    group_by(Placette, Annee) %>%
    slice_tail() %>%
    ungroup() %>%
    as.data.frame()

  switch(Variable,
    "Vol_HA" = {
      Data$Yvar <- Data$Vol_HA
      Etiquette <- "Volume marchand (m3/ha)"
    },
    "ST_HA" = {
      Data$Yvar <- Data$ST_HA
      Etiquette <- "Surface terri\uE8re marchande (m2/ha)"
    },
    "DQM" = {
      Data$Yvar <- Data$DQM
      Etiquette <- "Diam\uE8tre quadratique moyen (cm)"
    },
    "HDomM" = {
      Data$Yvar <- Data$HDomM
      Etiquette <- "Hauteur dominante (m)"
    },
    "nbTi_HA" = {
      Data$Yvar <- Data$nbTi_HA
      Etiquette <- "Densit\uE9 (nb/ha)"
    }
  )

  switch(Espece,
    "TOT" = {
      Essence <- "Toutes essences"
    },
    "BOJ" = {
      Essence <- "Bouleau jaune"
    },
    "ERR" = {
      Essence <- "\uC9rable rouge"
    },
    "ERS" = {
      Essence <- "\uC9rable \uE0 sucre"
    },
    "FEN" = {
      Essence <- "Feuillus nobles"
    },
    "FIN" = {
      Essence <- "Feuillus intol\uE9rants"
    },
    "EPX" = {
      Essence <- "\uC9pinettes"
    },
    "SAB" = {
      Essence <- "Sapin baumier"
    },
    "RES" = {
      Essence <- "R\uE9sineux"
    },
    "HEG" = {
      Essence <- "H\uEAtre \uE0 grandes feuilles"
    },
    "AUT" = {
      Essence <- "Autres essences"
    }
  )

  ymax <- max(Data$Yvar)

  AnneeDep <- min(Data$Annee)
  AnneeFin <- max(Data$Annee)

  dernieres_valeurs <- Data %>%
    lazy_dt() %>%
    group_by(Placette) %>%
    slice(n()) %>%
    ungroup() %>%
    as.data.frame()

  GraphEvol <- Data %>%
    ggplot(aes(x = Annee, y = Yvar, group = Placette, label = Placette)) +
    geom_line(aes(), show.legend = FALSE, lwd = 1.25, colour = "#008000") +
    ylim(0, ymax + 5) +
    xlab(bquote(bold("Ann\uE9\u65 de la simulation"))) +
    ylab(paste(Etiquette)) +
    scale_x_continuous(breaks = seq(AnneeDep, AnneeFin, by = 5)) +
    theme_bw() +
    ggtitle(paste(Etiquette, "  ", Essence)) +
    theme(
      strip.background = element_rect(fill = "white"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    ) +
    geom_text(data = dernieres_valeurs, aes(label = Placette), hjust = 1, vjust = -0.2, size = 3)
  GraphEvol

  NbPlac <- length(unique(SimulHtVol$Placette))

  Sommaire <- Sommaire_Classes_DHP(SimulHtVol) %>%
    lazy_dt() %>%
    filter((Annee %in% c(AnneeDep, AnneeFin)) & GrEspece == Espece & (Placette %in% listePlacette)) %>%
    mutate(DHP_cl = round(DHP_cl / 5) * 5) %>%
    group_by(Annee, DHP_cl) %>%
    summarise(NbHa = (sum(NbHa) / NbPlac), .groups = "drop") %>%
    as.data.frame()

  MaxDHP <- max(Sommaire$DHP_cl)

  ClassesDHP <- data.frame("Annee" = c(rep(AnneeDep, 13), rep(AnneeFin, 13)), "DHP_cl" = rep(seq(10, 70, by = 5), 2))

  GraphDist <- Sommaire %>%
    lazy_dt() %>%
    full_join(ClassesDHP, relationship = "many-to-many", by = join_by(Annee, DHP_cl)) %>%
    mutate(NbHa = ifelse(is.na(NbHa) == TRUE, 0, NbHa), Annee = as.factor(Annee)) %>%
    as.data.frame() %>%
    ggplot(aes(x = DHP_cl, y = NbHa, fill = Annee)) +
    geom_bar(position = position_dodge(preserve = "single"), stat = "identity", color = "black", width = 3) +
    ggtitle(paste("Distribution diam\uE9trale", "  ", Essence)) +
    xlab(bquote(bold("Classe de DHP (cm)"))) +
    ylab("Nombre de tiges par hectare") +
    scale_x_continuous(breaks = seq(10, 70, by = 5)) +
    scale_fill_manual(values = c("#D95F02", "#008000")) +
    theme(
      strip.background = element_rect(fill = "white"),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      strip.text = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12)
    ) +
    labs(fill = "Ann\uE9\u65") +
    theme(legend.position = "top")
  GraphDist

  Graphiques <- list(GraphEvol, GraphDist)

  return(Graphiques)
}
