#######################################################################################
#  Analisis del espectro de los ambientes luminicos del paper de fotosintesis de soja #
#                                                                                     #
# Datos relevados por Mauro More                                                      #
#       29/06/2021                                                                    #
#######################################################################################

getwd()

setwd ("R:/Paper_Fot_Soja")

# Paquetes 
library(lme4)
library(lmerTest)
library(nlme)
library(car)
library("ggplot2")       
library("lattice")
library("latticeExtra")
library(multcompView)
library(dplyr)
library(plyr)
library(xtable)
library(tidyverse)
library (emmeans)
library("qtl")
library(stringr)
library(data.table)
library(svMisc)
library(ggpubr)
library("ggsci")
library("FactoMineR")
library("factoextra")
library("corrplot")
library(Matrix)

######### distribucion espectral  ####################################

dist_espectral_uw_amb.1.E <- read.table ("./Data/rawdata/soja_amb_1.txt",
                                     header = FALSE, sep = "\t",dec = ",", skip=14)

colnames(dist_espectral_uw_amb.1.E) <- c("lambda", "uW_nm.cm2", "punto")

dist_espectral_uw_amb.1.E  <- dist_espectral_uw_amb.1.E  %>%
                              dplyr::mutate (punto = "Eprim")



dist_espectral_uw_amb.2.E.100 <- read.table ("./Data/rawdata/soja_amb_2_100.txt",
                                         header = FALSE, sep = "\t",dec = ",", skip=14)

colnames(dist_espectral_uw_amb.2.E.100) <- c("lambda", "uW_nm.cm2", "punto")

dist_espectral_uw_amb.2.E.100  <- dist_espectral_uw_amb.2.E.100  %>%
                                  dplyr::mutate (punto = "E.100")


dist_espectral_uw_amb.2.E.75 <- read.table ("./Data/rawdata/soja_amb_2_75.txt",
                                             header = FALSE, sep = "\t",dec = ",", skip=14)

colnames(dist_espectral_uw_amb.2.E.75) <- c("lambda", "uW_nm.cm2", "punto")

dist_espectral_uw_amb.2.E.75 <- dist_espectral_uw_amb.2.E.75  %>%
                                dplyr::mutate (punto = "E.75")



# Construccion de la matriz de lambdas
d1 <- tibble ( bandwidth = "D1", l1 = 400, l2= 425)
d2 <- tibble ( bandwidth = "D2", l1 = 425, l2= 490)
d3 <- tibble ( bandwidth = "D3", l1 = 490, l2= 560)
d4 <- tibble ( bandwidth = "D4", l1 = 560, l2= 585)
d5 <- tibble ( bandwidth = "D5", l1 = 585, l2= 640)
d6 <- tibble ( bandwidth = "D6", l1 = 640, l2= 700)
d7 <- tibble ( bandwidth = "D7", l1 = 700, l2= 780)
dPAR <- tibble ( bandwidth = "PAR", l1 = 400, l2= 700)
dintegr <- tibble ( bandwidth = "total", l1 = 380, l2= 780)

lambdas <- bind_rows (d1, d2, d3, d4, d5, d6, d7, dPAR,dintegr  )

dev.off()
#####  amb_1 #############
spectr.amb.1.Eprim <- run_spectrum (dt = dist_espectral_uw_amb.1.E, lambdas = lambdas, 
                                    id.survey="amb.1", px="Eprim" )

#####  amb_2_100 #############
spectr.amb.2.E.100 <- run_spectrum (dt = dist_espectral_uw_amb.2.E.100, lambdas = lambdas, 
                                    id.survey="amb.2", px="E.100" )


#####  amb_2_75 #############
spectr.amb.2.E.75 <- run_spectrum (dt = dist_espectral_uw_amb.2.E.75, lambdas = lambdas, 
                                    id.survey="amb.2", px="E.75" )

