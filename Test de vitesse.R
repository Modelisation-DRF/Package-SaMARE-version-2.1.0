library(tidyverse)
library(SimulateurSaMARE)
library(microbenchmark)
library(tictoc)
library(dplyr)
library(profvis)
library(parallel)
library(doParallel)

# dans le fichier et le code, les peuplements sont nommés Placette
#########################################################
# importer le fichier des arbres
fic <- read_delim("TestVitesseSimul.csv", delim = ";") %>%
  mutate(
    Placette = dense_rank(Placette),
    id = 1
  )

#########################################################
# tester différentes grosseurs de fichier
n <- 10 # nombre de fois qu'on multiplie le jeu de données initial
fic_test <- NULL
for (i in 1:n) {
  fic$id <- i
  if (i == 1) {
    fic_test <- fic
  } else {
    fic_test <- bind_rows(fic_test, fic)
  }
}

fic_test <- fic_test %>%
  mutate(Placette = dense_rank(paste0(id, Placette))) %>%
  select(-id)

########################################################
# faire des test avec fic
# ou avec fic_test
########################################################

#############################################################################################################################

##########################################################
# Test de parallélisation avec different nombre de cores #
##########################################################
# IH
# I
testIter <- 6
# H
testHozizon <- 4

#   1 CORE = BASELINE   #   1 CORE = BASELINE   #   1 CORE = BASELINE   #   1 CORE = BASELINE   #   1 CORE = BASELINE   #
microbenchmark(
  simul <- SimulSaMARE(NbIter = testIter, Horizon = testHozizon, RecruesGaules = 0, Data = fic, coreNumbers = 1),
  times = 10
)
#   TIME
#   49.44

#   2 CORES   #   2 CORES   #   2 CORES   #   2 CORES   #   2 CORES   #   2 CORES   #   2 CORES   #   2 CORES   #   2 CORES   #
microbenchmark(
  simul <- SimulSaMARE(NbIter = testIter, Horizon = testHozizon, RecruesGaules = 0, Data = fic, coreNumbers = 2),
  times = 10
)
#   TIME    Gains
#   27.63   178.94%

#   4 CORES   #   4 CORES   #   4 CORES   #   4 CORES   #   4 CORES   #   4 CORES   #   4 CORES   #   4 CORES   #   4 CORES   #
microbenchmark(
  simul <- SimulSaMARE(NbIter = testIter, Horizon = testHozizon, RecruesGaules = 0, Data = fic, coreNumbers = 4),
  times = 10
)
#   TIME    Gains
#   19.76   250,20%

#   8 CORES   #   8 CORES   #   8 CORES   #   8 CORES   #   8 CORES   #   8 CORES   #   8 CORES   #   8 CORES   #   8 CORES   #
microbenchmark(
  simul <- SimulSaMARE(NbIter = testIter, Horizon = testHozizon, RecruesGaules = 0, Data = fic, coreNumbers = 8),
  times = 10
)
#   TIME    Gains
#   17.16   288,11%

#   16 CORES    #   16 CORES    #   16 CORES    #   16 CORES    #   16 CORES    #   16 CORES    #   16 CORES    #   16 CORES    #
microbenchmark(
  simul <- SimulSaMARE(NbIter = testIter, Horizon = testHozizon, RecruesGaules = 0, Data = fic, coreNumbers = 16),
  times = 10
)
#   TIME    Gains
#   17.90   276,20%

#   MAX CORE    #   MAX CORE    #   MAX CORE    #   MAX CORE    #   MAX CORE    #   MAX CORE    #   MAX CORE    #   MAX CORE    #
microbenchmark(
  simul <- SimulSaMARE(NbIter = testIter, Horizon = testHozizon, RecruesGaules = 0, Data = fic, coreNumbers = detectCores()),
  times = 10
)
#   TIME    Gains
#   20.09   246,09%

#   MAX/2 CORES   #   MAX/2 CORES   #   MAX/2 CORES   #   MAX/2 CORES   #   MAX/2 CORES   #   MAX/2 CORES   #   MAX/2 CORES   #
microbenchmark(
  simul <- SimulSaMARE(NbIter = testIter, Horizon = testHozizon, RecruesGaules = 0, Data = fic, coreNumbers = detectCores() / 2),
  times = 10
)
#   TIME    Gains
#   17.48   282,84%


# Choose between 16 and Max/2 cores for optimal performances (using max 16 cores)
optimal_numbers_of_cores <- min(c(16, detectCores()/2))


#############################################################################################################################

# Avec microbenchmark, tester la fonction SimulSAMARE 100 fois sur 30 ans et 30 itérations (100 est la valeur par défaut).
set.seed(1)
microbenchmark(
  simul <- SimulSaMARE(NbIter = 3, Horizon = 2, RecruesGaules = 0, Data = fic)
)

# AUGIS
# mon ordi : 32 coeurs et 128 go de ram

# Unit: seconds
# expr
# simul <- SimulSaMARE(NbIter = 3, Horizon = 2, RecruesGaules = 0, Data = fic)
#      min      lq     mean   median       uq      max neval
# 8.979392 11.28407 14.45331 13.67844 17.71934 23.03785   100
# donc en moyenne c'est 14.5 secondes

# Donc 3 iter x 2 pas x 6 placettes = 36 ==>  14.5 sec / 36 = 0.4 sec par place/pas/iter

# NADTO1
# 24 coeurs 32gb ram
# 40-80% CPU
# 40-60% RAM
#
# Unit: seconds
#                                                                         expr      min       lq     mean   median       uq      max neval
# simul <- SimulSaMARE(NbIter = 3, Horizon = 2, RecruesGaules = 0, Data = fic) 9.891809 12.13325 12.18683 12.23953 12.35096 13.43614   100
#
# 12.19s/36 => 0.3 sec par iteration
#
# AVEC MULTI-THREAD#
# 15-100% CPU
# 45-60% RAM
#
# Unit: seconds
#                                                                        expr      min       lq     mean   median       uq      max neval
# simul <- SimulSaMARE(NbIter = 3, Horizon = 2, RecruesGaules = 0, Data = fic) 11.55615 13.84221 14.10641 13.98999 14.26313 16.85646   100


#############################################################################################################################

microbenchmark(
  simul <- SimulSaMARE(NbIter = 6, Horizon = 3, RecruesGaules = 0, Data = fic, coreNumbers = 8),
  times = 10
)

# Avec ifelse()
# Mean 20.4087

# AVEC if()elseif()else
# Mean 19.60893

# NEGLIGIBLE DIFFERENCE
