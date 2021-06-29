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

dist_espectral_uw_amb.1.E <- read.table ("./Data/rawdata/lampara_hpit_AbsoluteIrradiance_0002.txt",
                                     header = FALSE, sep = "\t",dec = ",", skip=14)
