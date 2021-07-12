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
library ('lubridate')


########  se carga los datos 
#R:\Paper_Fot_Soja\Data\rawdata\pesos_macetas

FS2 <- read_delim (file ="./Data/rawdata/pesos_macetas/datos.riego.FS2.txt" ,
                        delim="\t", na="NA",
                        escape_backslash = FALSE,
                        escape_double = TRUE,
                        col_names = TRUE,
                        col_types = NULL)
head(FS2)

FS2.1 <- FS2  %>% 
         dplyr::mutate   (trat =tratamiento) %>%
         tidyr::separate (tratamiento, c(NA, "genotipo", "ambiente", "cond.hidr", "rep")) %>%
         dplyr::mutate   (fecha.1 = dmy (fecha)) %>%
         dplyr::select (c(genotipo, ambiente, cond.hidr, rep, maceta, trat, fecha, fecha.1, everything()))



############## datos de los pesos de las macetas y el sustrato ########
#470 -453
pss <- mean (c (526.2,517.6,521,518.2,525.6,527.4)) # Esto es el peso seco del sustrato
sd.pss <- sd (c (526.2,517.6,521,518.2,525.6,527.4))

pm <- 31 + 7                          # Esto es el peso de la maceta

S <- pss + pm 

agua.CC <- 792 - S

hp.CC <- (231.3333 * 100)/ pss

agua.CCprim <- (pss * 30 )/100

S + agua.CCprim

###################
head (FS2.1)
FS2.S.1 <- FS2.1 %>%
         dplyr::filter (cond.hidr == "S") %>%
         dplyr::filter (fecha.1 >= "2021-03-30")%>%
         dplyr::select (-W.final)

t0 <- FS2.S.1 %>%
      dplyr::filter (fecha.1== "2021-03-30") %>%
      dplyr::select (fecha.1)

t0.1 <- unique (t0$fecha.1)
class(t0.1)

FS2.S.2 <- FS2.S.1 %>%
         dplyr::mutate (time = difftime (fecha.1 , t0.1 ,units = "days")) %>%
         dplyr::mutate (pot = str_c ("pot_", maceta)) %>%
         dplyr::select (genotipo , pot, fecha.1, time, W.inicial) %>%
         dplyr::rename (genotype = genotipo) %>%
         dplyr::rename (w.SPM = W.inicial) %>%
         dplyr::filter (time >= 0) %>%
         dplyr::mutate (time = as.numeric (time)) %>%
         dplyr::arrange (pot)


FS2.S.2a <- FS2.S.2 %>%
            dplyr::filter (w.SPM <= 720 )


list.pot <- unique(FS2.S.2a$pot)

FS2.S.2b <- bind_rows (lapply(list.pot, function(filt.pot){
  
  pot_x <- FS2.S.2a %>%
           dplyr::filter (pot == filt.pot)
  
  t0 <- pot_x %>%
        dplyr::filter (fecha.1 == min ( fecha.1 ))
  
  
  pot_x.1 <- pot_x %>%
              dplyr::mutate (time = difftime (fecha.1 , t0$fecha.1 ,units = "days")) %>%
             dplyr::mutate (time = as.numeric (time))
  
}))



dt =FS2.S.2b
genotype= "genotype"
t1=4
W="w.SPM" 
pss=pss 
pm =pm
ue="pot"
essay="FS2"


ET.FS2 <- run_eq.ETmodel (dt =FS2.S.2,  genotype= "genotype", t1=4, W="w.SPM", pss=pss , pm =pm, essay="FS2", ue="pot" )
ET.FS2.1 <- run_eq.ETmodel (dt =FS2.S.2,  genotype= "genotype", t1=4, W="w.SPM", pss=pss , pm =pm, essay="FS2", ue="pot" )


dev.off ()


###################### funcion ET.model ################
# cargar la funcion run_eq.ETmodel

W0 <- FS2.2 %>%
      dplyr::filter(time==0)

 ggscatter (FS2.2 , x = "time", y = "w.SPM", facet.by = "genotype",
                  #ylim=c(S, 781),
                  color = "gray38",
                   add="loess",
                  #title = str_c ("weigth",i1, i2,i3, sep="."),
                  ylab = "W (g)",
                  xlab = "time (dwd)") +
                  geom_hline(yintercept = S, lty=2) 
 
 +
  stat_function(fun = fun.AR, lwd=1, colour ="red") +
  geom_hline(yintercept = W0$W, lty=2) +
  geom_hline(yintercept = S, lty=2) +
  geom_hline(yintercept = ARS, lty =2) +
  geom_segment (x=0, y=S, xend=0, yend=ARS, colour ="red", size =2) 















dt = SPM_data_1 
pss = mean.pss
pm = 41 
t1 =4 
W="w.SPM"
essay="ajuste"
ue ="pot"
genotype = "genotype"





SPM_data_1 <- SPM_data %>%
              dplyr::mutate (genotype = "Geno_1") %>%
              dplyr::select (genotype, pot, everything())

summary (SPM_data_1)


