################################################################################
# Analisis de datos de consumo de agua soja ensayos preliminares               #
#                                                                              #
# Don Mario 6.8.i y Génesis 5601                                               #
# Gaston Quero - Marta Sainz - Mauro More - Noelia Torres - Sebastian Simondi  # 
# 8/10/2021                                                                   # 
################################################################################


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
library (multcomp)
library ("dplyr")
library (ggjoy)
library ("ggridges")
library (hrbrthemes)
library(tidyverse)
library(forcats)
library("viridis")
library("lmerTest")
library(lubridate)
library(nycflights13)
library(nlme)
library(xtable)
library(stringr)
library(data.table)
library(svMisc)
library(ggpubr)
library("ggsci")
library("FactoMineR")
library("factoextra")
library("corrplot")

## cargar datos
#siR:\Paper_Fot_Soja\Data\rawdata\wtr\pesos_macetas
## FS.3 #########
SPM.FS.3 <- read_delim (file = "./Data/rawdata/wtr/pesos_macetas/SPM_FS.3.txt" , 
                                                   delim ="\t", quote = "\"", 
                                                   escape_backslash = FALSE,
                                                   escape_double = TRUE, 
                                                   col_names = TRUE, 
                                                   col_types = NULL,
                                                   locale = default_locale(), 
                                                   na = "NA")

cod.SPM.FS.3 <- read_delim (file = "./Data/rawdata/wtr/pesos_macetas/cod.SPM_FS.3.txt" , 
                        delim ="\t", quote = "\"", 
                        escape_backslash = FALSE,
                        escape_double = TRUE, 
                        col_names = TRUE, 
                        col_types = NULL,
                        locale = default_locale(), 
                        na = "NA")

cod.SPM.FS.3$maceta <- as.character (cod.SPM.FS.3$maceta)
head ( SPM.FS.3)
SPM.FS.3.1 <- SPM.FS.3 %>%
              dplyr::mutate (maceta = as.character(maceta) ) %>%
              dplyr::inner_join(cod.SPM.FS.3, by="maceta" ) %>%
              dplyr::mutate (x = id)%>%
              tidyr::separate(x , c(NA, "genotype", "amb.luminico", "cond.hidrc", "rep"))%>%
              dplyr::mutate (ensayo = "FS.3") %>% 
              dplyr::select ("ensayo", "amb.luminico","genotype", "cond.hidrc","maceta","rep", "id", everything())%>%
              dplyr::mutate (Date = dmy (dia))%>%
              dplyr::mutate (pot= str_c (ensayo,maceta, id, sep="_" ))
  
mean.pss      <- mean (c(526.2 , 517.6, 521, 518.2, 525.6, 527.4)) # Esto es el peso seco del sustrato
sd.pss        <- sd   (c(526.2 , 517.6, 521, 518.2, 525.6, 527.4))

pm <- 31 +  7                      # Esto es el peso de la maceta

S <- mean.pss + pm   

peso.CC <- 740

list.pot <- unique (SPM.FS.3.1$pot)

##### Esta es la funcion #####
run.plot.WSPM <- function ( dt = NULL, S=NULL, PCC = NULL){
  
  dir.create (file.path ("Figures", "Plots.WSPM"), showWarnings = FALSE)
  
  dt <- dt
  t1.1 <- min (dt$Date)
  
  dt.1 <- dt %>%
          dplyr::mutate (dias = Date - t1.1) %>%
          dplyr::mutate (x = as.numeric(dias))


  wspm.curve <- ggscatter (dt.1 , x = "x", y = "peso",
                           #ylim = c(500,745 ),
                          title = unique(dt.1$pot), 
                          #color = "cond.hidrc", 
                          xlab = "time (d)",
                          ylab = "WSPM (g)",
                          point=FALSE) +
    geom_line (color = "black", linetype =2, size = 0.5) +
    geom_point(color = "black", size = 1.5) +
    geom_hline(yintercept =peso.CC, linetype =3, size = 1,color = "blue" ) +
    geom_hline(yintercept =S, linetype =3, size = 1,color = "red" )+
    geom_hline(yintercept = mean (dt.1$peso, na.rm = TRUE ), linetype =2, size = 1) #+
    #geom_hline(yintercept = peso.obj, linetype =2, col="red", size = 1) 
  
  
  
  ggexport ( wspm.curve, filename = str_c("./Figures/Plots.WSPM/",unique(dt.1$pot), ".tiff"),
            width = 700, height = 500)
  
  print (wspm.curve)          
}


FS.3.wsp <- lapply(list.pot, function(filt.pot){
  
  print (filt.pot)
  pot <- SPM.FS.3.1 %>%
         dplyr::filter (pot == filt.pot)
  
  run.plot.WSPM (dt = pot ,S=S, PCC = peso.CC)    
  
  
})



## FS.4 #########
SPM.FS.4 <- read_delim (file = "./Data/rawdata/wtr/pesos_macetas/SPM_FS.4.txt" , 
                        delim ="\t", quote = "\"", 
                        escape_backslash = FALSE,
                        escape_double = TRUE, 
                        col_names = TRUE, 
                        col_types = NULL,
                        locale = default_locale(), 
                        na = "NA")

cod.SPM.FS.4 <- read_delim (file = "./Data/rawdata/wtr/pesos_macetas/cod.SPM_FS.4.txt" , 
                            delim ="\t", quote = "\"", 
                            escape_backslash = FALSE,
                            escape_double = TRUE, 
                            col_names = TRUE, 
                            col_types = NULL,
                            locale = default_locale(), 
                            na = "NA")

cod.SPM.FS.4$maceta <- as.character (cod.SPM.FS.4$maceta)
head ( SPM.FS.4)
SPM.FS.4.1 <- SPM.FS.4 %>%
              dplyr::mutate (maceta = as.character(maceta) ) %>%
              dplyr::inner_join(cod.SPM.FS.4, by="maceta" ) %>%
              dplyr::mutate (x = id)%>%
              tidyr::separate(x , c(NA, "genotype", "amb.luminico", "cond.hidrc", "rep"))%>%
              dplyr::mutate (ensayo = "FS.4") %>% 
              dplyr::select ("ensayo", "amb.luminico","genotype", "cond.hidrc","maceta","rep", "id", everything())%>%
              dplyr::mutate (Date = dmy (dia))%>%
              dplyr::mutate (pot= str_c (ensayo, maceta, id, sep="_" ))

mean.pss      <- mean (c(526.2 , 517.6, 521, 518.2, 525.6, 527.4)) # Esto es el peso seco del sustrato
sd.pss        <- sd   (c(526.2 , 517.6, 521, 518.2, 525.6, 527.4))

pm <- 31 +  7                      # Esto es el peso de la maceta

S <- mean.pss + pm   

peso.CC <- 740




list.pot <- unique (SPM.FS.4.1$pot)

FS.4.wsp <- lapply(list.pot, function(filt.pot){
  
  print (filt.pot)
  pot <- SPM.FS.4.1 %>%
    dplyr::filter (pot == filt.pot)
  
  run.plot.WSPM (dt = pot ,S=S, PCC = peso.CC)    
  
  
})

#### 
## FS.5 #########
SPM.FS.5 <- read_delim (file = "./Data/rawdata/wtr/pesos_macetas/SPM_FS.5.txt" , 
                        delim ="\t", quote = "\"", 
                        escape_backslash = FALSE,
                        escape_double = TRUE, 
                        col_names = TRUE, 
                        col_types = NULL,
                        locale = default_locale(), 
                        na = "NA")

cod.SPM.FS.5 <- read_delim (file = "./Data/rawdata/wtr/pesos_macetas/cod.SPM_FS.5.txt" , 
                            delim ="\t", quote = "\"", 
                            escape_backslash = FALSE,
                            escape_double = TRUE, 
                            col_names = TRUE, 
                            col_types = NULL,
                            locale = default_locale(), 
                            na = "NA")

cod.SPM.FS.5$maceta <- as.character (cod.SPM.FS.5$maceta)
head ( SPM.FS.5)
SPM.FS.5.1 <- SPM.FS.5 %>%
              dplyr::mutate (maceta = as.character(maceta) ) %>%
              dplyr::inner_join(cod.SPM.FS.5, by="maceta" ) %>%
              dplyr::mutate (x = id)%>%
              tidyr::separate(x , c(NA, "genotype", "amb.luminico", "cond.hidrc", "rep"))%>%
              dplyr::mutate (ensayo = "FS.5") %>% 
              dplyr::select ("ensayo", "amb.luminico","genotype", "cond.hidrc","maceta","rep", "id", everything())%>%
              dplyr::mutate (Date = dmy (dia))%>%
              dplyr::mutate (pot= str_c (ensayo, maceta, id, sep="_" ))

mean.pss      <- mean (c(526.2 , 517.6, 521, 518.2, 525.6, 527.4)) # Esto es el peso seco del sustrato
sd.pss        <- sd   (c(526.2 , 517.6, 521, 518.2, 525.6, 527.4))

pm <- 31 +  7                      # Esto es el peso de la maceta

S <- mean.pss + pm   

peso.CC <- 740




list.pot <- unique (SPM.FS.5.1$pot)

FS.5.wsp <- lapply(list.pot, function(filt.pot){
  
  print (filt.pot)
  pot <- SPM.FS.5.1 %>%
    dplyr::filter (pot == filt.pot)
  
  run.plot.WSPM (dt = pot ,S=S, PCC = peso.CC)    
  
  
})

head(consumo.1)

## condicion hidrica HP%

CC.MN.1 <- read_delim (file = "./Data/rawdata/macetas_cc.txt" , 
                       delim ="\t", quote = "\"", 
                       escape_backslash = FALSE,
                       escape_double = TRUE, 
                       col_names = TRUE, 
                       col_types = NULL,
                       locale = default_locale(), 
                       na = "NA")
summary (CC.MN.1)

peso.sust.seco <- mean (CC.MN.1$peso.sust.seco, na.rm = TRUE)
peso.bolsa <- mean (CC.MN.1$peso.bolsa, na.rm = TRUE)

consumo.2 <-  consumo.1 %>%
              dplyr::mutate (agua = pesof.1 - peso.sust.seco - peso.bolsa) %>%
              dplyr::mutate (hp.porc = (agua *100)/peso.sust.seco)

consumo.2 %>%
   group_by (tratamiento) %>%
   summarise (max(hp.porc, na.rm=TRUE))


list.pot2 <- unique (consumo.2$pot)

run.plot.hp <- function ( dt = NULL, t1=NULL, hp.ref =NULL){
  
  dir.create (file.path ("Figures", "Plots.HP"), showWarnings = FALSE)
  
  dt <- dt
  
  t1.1 <- dmy (t1)
  
  dt.1 <- dt %>%
          dplyr::mutate (dias = Date - t1.1) 
  

  hp.curve <- ggscatter (dt.1 , x = "dias", y = "hp.porc", 
                           title = unique(dt.1$pot), 
                           ylim=c(0, 25),
                           xlab = "time (d)",
                           ylab = "HP (%)",
                           point=FALSE) +
    geom_line(color = "gray48", linetype =2, size = 0.5) +
    geom_point(color = "black", size = 1.5) +
    geom_hline(yintercept = mean (dt.1$hp.porc, na.rm = TRUE ), linetype =3, size = 1) #+
  #geom_hline(yintercept = peso.obj, linetype =2, col="red", size = 1) 
  
  
  
  ggexport ( hp.curve, filename = str_c("./Figures/Plots.HP/",unique(dt.1$pot), ".tiff"),
             width = 700, height = 500)
  
  print (hp.curve)          
}


X.HP <- lapply(list.pot2, function(filt.pot){
  
  print (filt.pot)
  pot <- consumo.2 %>%
    dplyr::filter (pot == filt.pot)
  
  run.plot.hp (dt = pot , t1= "28/11/2019", hp.ref =NULL)    
  
  
})


summary (consumo.2)
t1= "28/11/2019"

t1.1 <- dmy (t1)

consumo.2 <- consumo.2 %>%
             dplyr::mutate (dias = Date - t1.1)
                 
ggscatter (consumo.2 , x = "dias", y = "hp.porc", facet.by = "clon",
           color="tratamiento",
           palette = c("navyblue", "darkorange"),
           #title = unique(dt.1$pot), 
           ylim=c(0, 25),
           xlab = "time (d)",
           ylab = "HP (%)",
           point=TRUE) 

summary (consumo.2)
consumo.3 <- consumo.2 %>%
             group_by (tratamiento)%>%
             summarise(hp.mean = mean(hp.porc , na.rm = TRUE), agua.mean = mean(agua , na.rm = TRUE))

