###############################################################
# Codigo para el analisis de los datos de consumo de agua     #
#                                                             #
# Gaston Quero  - Sebastian Simondi                           #
# 1/7/2021                                                    #
###############################################################

getwd ()
setwd ("R:/Paper_Fot_Soja")

# Paquetes 
library (lme4)
library (emmeans)
library ("car")
library ("nlmrt")
library ("easynls")
library (tidyverse)
library ("ggplot2")       
library ("lattice")
library ("latticeExtra")
library (multcompView)
library ("dplyr")
library (ggjoy)
library ("ggridges")
library (hrbrthemes)
library (forcats)
library ("viridis")
library ("lmerTest")
library (nlme)
library (xtable)
library (stringr)
library (data.table)
library (svMisc)
library (ggpubr)
library ("ggsci")
library ("FactoMineR")
library ("factoextra")
library ("corrplot")
library ("readr")


########  se carga los datos 
#R:\Paper_Fot_Soja\Data\rawdata\pesos_macetas

FS2 <- read_delim (file ="./Data/rawdata/pesos_macetas/planilla.riego.FS2.txt" ,
                        delim="\t", na="NA",
                        escape_backslash = FALSE,
                        escape_double = TRUE,
                        col_names = TRUE,
                        col_types = NULL)


############## datos de los pesos de las macetas y el sustrato ########
#470 -453
mean.pss <- 523                    #mean (c(470,458,453,468))   # Esto es el peso seco del sustrato
#sd.pss        <- sd   (c(470,458,453,468))

pm <- 31 + 7                          # Esto es el peso de la maceta


head (FS2)





SPM_data_1 <- SPM_data %>%
  dplyr::mutate (genotype = "Geno_1") %>%
  dplyr::select (genotype, pot, everything())

summary (SPM_data_1)


