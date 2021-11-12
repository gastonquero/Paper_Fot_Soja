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
#C:\Users\Usuario\OneDrive\Documentos\Paper_fotosintesis_dinamica\Data\rawdata\Relevamiento_Luz\ambiente_luminico_soja

dist_espectral_uw_amb.FS.2 <- read.table ("C:/Users/Usuario/OneDrive/Documentos/Paper_fotosintesis_dinamica/Data/rawdata/Relevamiento_Luz/ambiente_luminico_soja/cam.bq.Eprima.txt",
                                     header = FALSE, sep = "\t",dec = ",", skip=123)

colnames (dist_espectral_uw_amb.FS.2) <- c("lambda", "uW_nm.cm2", "punto")

dist_espectral_uw_amb.FS.2  <- dist_espectral_uw_amb.FS.2  %>%
                               dplyr::mutate (punto = "FS.2")




# FS.4
dist_espectral_uw_amb.FS.4a <- read.table ("C:/Users/Usuario/OneDrive/Documentos/Paper_fotosintesis_dinamica/Data/rawdata/Relevamiento_Luz/ambiente_luminico_soja/LED.nueva.sim.marta_AbsoluteIrradiance_1.txt",
                                          header = FALSE, sep = "\t",dec = ",", skip=14)


dist_espectral_uw_amb.FS.4a  <- dist_espectral_uw_amb.FS.4a  %>%
                                dplyr::mutate (punto = "FS.4a")

colnames (dist_espectral_uw_amb.FS.4a) <- c("lambda", "uW_nm.cm2", "punto")


dist_espectral_uw_amb.FS.4b <- read.table ("C:/Users/Usuario/OneDrive/Documentos/Paper_fotosintesis_dinamica/Data/rawdata/Relevamiento_Luz/ambiente_luminico_soja/LED.nueva.sim.marta_AbsoluteIrradiance_2.txt",
                                           header = FALSE, sep = "\t",dec = ",", skip=14)


dist_espectral_uw_amb.FS.4b  <- dist_espectral_uw_amb.FS.4b  %>%
                                dplyr::mutate (punto = "FS.4b")

colnames (dist_espectral_uw_amb.FS.4b) <- c("lambda", "uW_nm.cm2", "punto")



# FS.5
dist_espectral_uw_amb.FS.5a <- read.table ("C:/Users/Usuario/OneDrive/Documentos/Paper_fotosintesis_dinamica/Data/rawdata/Relevamiento_Luz/ambiente_luminico_soja/LED.colore.amb.rojo.sim.marta_AbsoluteIrradiance_1.txt",
                                           header = FALSE, sep = "\t",dec = ",", skip=14)


dist_espectral_uw_amb.FS.5a  <- dist_espectral_uw_amb.FS.5a  %>%
                                dplyr::mutate (punto = "FS.5a")

colnames (dist_espectral_uw_amb.FS.5a) <- c("lambda", "uW_nm.cm2", "punto")


dist_espectral_uw_amb.FS.5b <- read.table ("C:/Users/Usuario/OneDrive/Documentos/Paper_fotosintesis_dinamica/Data/rawdata/Relevamiento_Luz/ambiente_luminico_soja/LED.colore.amb.rojo.sim.marta_AbsoluteIrradiance_2.txt",
                                           header = FALSE, sep = "\t",dec = ",", skip=14)


dist_espectral_uw_amb.FS.5b  <- dist_espectral_uw_amb.FS.5b  %>%
                                dplyr::mutate (punto = "FS.5b")

colnames (dist_espectral_uw_amb.FS.5b) <- c("lambda", "uW_nm.cm2", "punto")


dist_espectral_uw_amb.FS.5c <- read.table ("C:/Users/Usuario/OneDrive/Documentos/Paper_fotosintesis_dinamica/Data/rawdata/Relevamiento_Luz/ambiente_luminico_soja/LED.colore.amb.rojo.sim.marta_AbsoluteIrradiance_3.txt",
                                           header = FALSE, sep = "\t",dec = ",", skip=14)


dist_espectral_uw_amb.FS.5c  <- dist_espectral_uw_amb.FS.5c  %>%
                                dplyr::mutate (punto = "FS.5c")

colnames (dist_espectral_uw_amb.FS.5c) <- c("lambda", "uW_nm.cm2", "punto")


# FS.6
dist_espectral_uw_amb.FS.6 <- read.table ("C:/Users/Usuario/OneDrive/Documentos/Paper_fotosintesis_dinamica/Data/rawdata/Relevamiento_Luz/ambiente_luminico_soja/LED.colore.amb.azul.sim.marta_AbsoluteIrradiance_15.txt",
                                           header = FALSE, sep = "\t",dec = ",", skip=14)


dist_espectral_uw_amb.FS.6  <- dist_espectral_uw_amb.FS.6  %>%
                                dplyr::mutate (punto = "FS.6")

colnames (dist_espectral_uw_amb.FS.6) <- c("lambda", "uW_nm.cm2", "punto")

ambientes_soja <- bind_rows (dist_espectral_uw_amb.FS.2,
                             dist_espectral_uw_amb.FS.4a,
                       dist_espectral_uw_amb.FS.4b, 
                       dist_espectral_uw_amb.FS.5a, 
                       dist_espectral_uw_amb.FS.5b,
                       dist_espectral_uw_amb.FS.5c,
                       dist_espectral_uw_amb.FS.6 )


# Construccion de la matriz de lambdas
d1 <- tibble ( bandwidth = "D1", l1 = 400, l2= 425)
d2 <- tibble ( bandwidth = "D2", l1 = 425, l2= 490)
d3 <- tibble ( bandwidth = "D3", l1 = 490, l2= 560)
d4 <- tibble ( bandwidth = "D4", l1 = 560, l2= 585)
d5 <- tibble ( bandwidth = "D5", l1 = 585, l2= 640)
d6 <- tibble ( bandwidth = "D6", l1 = 640, l2= 700)
dFS.6 <- tibble ( bandwidth = "D.FS6", l1 = 400, l2= 500)
dPAR <- tibble ( bandwidth = "PAR", l1 = 400, l2= 700)
dintegr <- tibble ( bandwidth = "total", l1 = 400, l2= 800)

lambdas <- bind_rows (d1, d2, d3, d4, d5, d6, dPAR,dintegr  )

#### Correr la funcion run_process_spectrum ####

list_px <- unique (ambientes_soja$punto)
power_soja <- bind_rows( lapply (list_px , function (filt.punto){
  
  pwr_soja_1 <-  run_spectrum (dt =ambientes_soja, lambdas = lambdas, id.survey="amb.soja", px=filt.punto )
  
}))

write_delim (power_soja, file= "./Data/procdata/power_camaras_soja.txt", delim = ",", na = "NA")

write_excel_csv2 ( power_soja,file= "./Data/procdata/power_camaras_soja.csv", na = "NA")


##### figuras ##
# FS.2 
power_FS.2 <- power_soja %>%
              dplyr::filter (punto == "FS.2" )


FS.2 <- ambientes_soja %>%
        dplyr::filter (punto == "FS.2")

plot (uW_nm.cm2 ~ lambda, 
      ylim = c(0,400),
      xlim=  c(400, 800),
      col="black",
      #pch=16,cex=1,
      axes=TRUE,
      #xlab="", 
      #ylab="", 
      type="l",
      main = str_c ( "power",unique(FS.2$punto) ,sep="_",collapse = TRUE),
      data= FS.2) 

abline ( v=400, lty=2, col="gray48" )
abline ( v=700 , lty=2, col="gray48" )


x1.pwr <- 565
x2.pwr <- 1746


FS.2$lambda [c(x1.pwr,x1.pwr:x2.pwr,x2.pwr)]

with (FS.2, polygon(x=c(lambda [c(x1.pwr,x1.pwr:x2.pwr,x2.pwr)]),
                    y= c(0, uW_nm.cm2[x1.pwr:x2.pwr], 0),border ='black',
                    col=scales::alpha( 'gray',.5)))
13724 * 0.01
text(x = 650 , 
     y= 300 ,
     label = "intervalo = 400-800 nm;
              PPFD = 620 um/m2/s;
              power= 137.24 W/m2",
     pos=3,
     cex = 0.9,  col="black")


###### FS.4 
power_FS.4 <- power_soja %>%
              dplyr::filter (punto == "FS.4a" )


FS.4 <- ambientes_soja %>%
        dplyr::filter (punto == "FS.4a")

plot (uW_nm.cm2 ~ lambda, 
      ylim = c(0,400),
      xlim=  c(400, 800),
      col="black",
      #pch=16,cex=1,
      axes=TRUE,
      #xlab="", 
      #ylab="", 
      type="l",
      main = str_c ( "power",unique(FS.4$punto) ,sep="_",collapse = TRUE),
      data= FS.4) 

abline ( v= 400, lty=2, col="gray48" )
abline ( v= 700 , lty=2, col="gray48" )


x1.pwr <- 565
x2.pwr <- 1433


FS.4$lambda [c(x1.pwr,x1.pwr:x2.pwr,x2.pwr)]

with (FS.4, polygon(x=c(lambda [c(x1.pwr,x1.pwr:x2.pwr,x2.pwr)]),
                    y= c(0, uW_nm.cm2[x1.pwr:x2.pwr], 0),border ='black',
                    col=scales::alpha( 'gray',.5)))
13721.92 * 0.01

text(x = 600 , 
     y= 300 ,
     label = "intervalo = 400-700 nm;
              PPFD = 635.87 um/m2/s;
              power= 137.22 W/m2",
     pos=3,
     cex = 0.9,  col="black")

### 
###### FS.5 
power_FS.5 <- power_soja %>%
              dplyr::filter (punto == "FS.5c" )


FS.5 <- ambientes_soja %>%
        dplyr::filter (punto == "FS.5c")

plot (uW_nm.cm2 ~ lambda, 
      ylim = c(0,400),
      xlim=  c(400, 800),
      col="black",
      #pch=16,cex=1,
      axes=TRUE,
      #xlab="", 
      #ylab="", 
      type="l",
      main = str_c ( "power",unique(FS.5$punto) ,sep="_",collapse = TRUE),
      data= FS.5) 

abline ( v=400, lty=2, col="gray48" )
abline ( v=700 , lty=2, col="gray48" )


x1.pwr <- 1089
x2.pwr <- 1433


FS.5$lambda [c(x1.pwr,x1.pwr:x2.pwr,x2.pwr)]

with (FS.5, polygon(x=c(lambda [c(x1.pwr,x1.pwr:x2.pwr,x2.pwr)]),
                    y= c(0, uW_nm.cm2[x1.pwr:x2.pwr], 0),border ='red',
                    col=scales::alpha( 'red',.5)))
11259.68 * 0.01

text(x = 600 , 
     y= 300 ,
     label = "intervalo = 400-700 nm;
              PPFD = 609 um/m2/s;
              power= 112.6 W/m2",
     pos=3,
     cex = 0.9,  col="black")

## FS.6

power_FS.6 <- power_soja %>%
              dplyr::filter (punto == "FS.6" )
  
  
FS.6 <- ambientes_soja %>%
        dplyr::filter (punto == "FS.6")

plot (uW_nm.cm2 ~ lambda, 
      ylim = c(0,400),
      xlim=  c(400, 800),
      col="black",
      #pch=16,cex=1,
      axes=TRUE,
      #xlab="", 
      #ylab="", 
      type="l",
      main = str_c ( "power",unique(FS.6$punto) ,sep="_",collapse = TRUE),
      data= FS.6) 

abline ( v=400, lty=2, col="gray48" )
abline ( v=700 , lty=2, col="gray48" )


x1.pwr <- 565
x2.pwr <- 845


FS.6$lambda [c(x1.pwr,x1.pwr:x2.pwr,x2.pwr)]

with (FS.6, polygon(x=c(lambda [c(x1.pwr,x1.pwr:x2.pwr,x2.pwr)]),
                        y= c(0, uW_nm.cm2[x1.pwr:x2.pwr], 0),border ='blue',
                        col=scales::alpha( 'blue',.3)))
14124 * 0.01
text(x = 600 , 
     y= 300 ,
     label = "intervalo = 400-500 nm;
              PPFD = 538 um/m2/s;
              power= 141.24 W/m2",
     pos=3,
     cex = 0.9,  col="black")


##


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

#dev.off()
#####  amb_1 #############
spectr.amb.1.Eprim <- run_spectrum (dt = dist_espectral_uw_amb.FS.2 , lambdas = lambdas, 
                                    id.survey="amb.1", px="Eprim" )

#####  amb_2_100 #############
spectr.amb.2.E.100 <- run_spectrum (dt = dist_espectral_uw_amb.2.E.100, lambdas = lambdas, 
                                    id.survey="amb.2", px="E.100" )


#####  amb_2_75 #############
spectr.amb.2.E.75 <- run_spectrum (dt = dist_espectral_uw_amb.2.E.75, lambdas = lambdas, 
                                    id.survey="amb.2", px="E.75" )

