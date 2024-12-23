####____CARGA DE LIBRERIAS Y FUNCIONES_____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
library(stringr)
library(lubridate)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(quantmod)
library(stringr) # Working with strings
library(forcats) 
library(scales)
library(janitor)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.5)
####____SIMULACION POR MONTECARLO____####
set.seed(1234)
n <- 5000
unifdens <- function(x){
  y <- ifelse(x>=0 & x<=1, 1, 0)
  return(y)
}
unif <- data.frame(x = seq(-0.05, 1.10, 0.001)) %>% 
  mutate(y = unifdens(x)) 

ggplot(unif, aes(x=x, y=y))+
  geom_line(linewidth=1)+
  geom_polygon(fill=paleta(12)[3], alpha=0.5)+
  labs(x="x: Variable aleatoria", y = "f(x): Función de densidad",
       title="Función de densidad uniforme en el intervalo [0, 1]")+
  theme_light()+
  theme(text = element_text(face = "bold",size = 14),
        plot.title = element_text(hjust = 0.5))

ggsave("D:/Files/Presentations/TallerGIR_Ago2023/unif.png",plot = last_plot(),
       width = 8, height = 5, units = "in")
####____ACUMULADA DE NORMAL____####
normal <- data.frame(x=seq(-3,4,0.01)) %>% 
  mutate(y = pnorm(x, 0, 1))

ggplot(normal, aes(x=x, y=y))+
  geom_line(linewidth=1)+
  geom_area(fill=paleta(12)[3], alpha=0.5)+
  labs(x="y: Variable aleatoria de interés", y = "f(y): Función de distribución",
       title="Función de distribución acumulada")+
  theme_light()+
  theme(text = element_text(face = "bold",size = 14),
        plot.title = element_text(hjust = 0.5))
ggsave("D:/Files/Presentations/TallerGIR_Ago2023/fdaNorm.png",plot = last_plot(),
       width = 9, height = 6, units = "in")
####____ACUMULADA MATRICES DE TRANSICION____####
arch <- c("avg12","avg12_sens_ab","avg12_sens_ba","P5P95","P10P90","P25P75")
j <- 1
tmCum <- fread(paste0("D:/!bso/transMat/prevAbr2023/tm23_",arch[j],".csv"),encoding = "UTF-8",fill = T) %>% 
  mutate(across(everything(),~as.numeric(.x)/100))

cumA <- function(x){
  y <- ifelse(x>=1 & x<2, tmCum[1,9],
              ifelse(x>=2 & x<3, tmCum[1,10],
                     ifelse(x>=3 & x<4, tmCum[1,11],
                            ifelse(x>=4 & x<5, tmCum[1,12],
                                   ifelse(x>=5 & x<6, tmCum[1,13],
                                          ifelse(x>=6 & x<7, tmCum[1,14],
                                                 ifelse(x>=7 & x<8, tmCum[1,15],
                                                        ifelse(x>=8, tmCum[1,16],0))))))))
  return(as.numeric(y))
}
fdaA <- data.frame(x= seq(-0.5,9,0.01)) %>% 
  mutate(y=cumA(x))


ggplot(fdaA, aes(x=x, y=y))+
  geom_line(linewidth=1)+
  geom_area(fill=paleta(12)[3], alpha=0.5)+
  labs(x="y: Calificación", y = "f(y): Función de distribución",
       title="Función de distribución acumulada de transición B")+
  scale_x_continuous(breaks = 1:8, labels=1:8)+
  theme_light()+
  theme(text = element_text(face = "bold",size = 14),
        plot.title = element_text(hjust = 0.5))

ggsave("D:/Files/Presentations/TallerGIR_Ago2023/acumB.png",plot = last_plot(),
       width = 9, height = 6, units = "in")

fdaA %>% 
  dplyr::filter(x>=1 & x<6) %>%
  ggplot(aes(x=x, y=y))+
  geom_line(linewidth=1)+
  geom_area(fill=paleta(12)[3], alpha=0.5)+
  labs(x="x: Variable aleatoria", y = "f(x): Función de densidad",
       title="Función de distribución acumulada de transición B")+
  scale_x_continuous(breaks = 1:8, labels=1:8)+
  theme_light()+
  theme(text = element_text(face = "bold",size = 14),
        plot.title = element_text(hjust = 0.5))

####____CONVERGENCE OF STATISTICS____####
arch <- c("avg12","avg12_sens_ab","avg12_sens_ba","P5P95","P10P90","P25P75")
i <- 1
dpprev <- fread(paste0("D:/!bso/transMat/prevAbr2023/dpprev_",arch[i],".csv"))
dens <- dpprev %>% 
  select(-CALIFICACION,-califPot) %>% 
  group_by(dummy) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  arrange(dummy) %>% 
  mutate(MeanDifPrev = cummean(difPrev)) %>% 
  mutate(SdDifPrev = cumsd(difPrev)) 

lastmean <- dens$MeanDifPrev[nrow(dens)]
lastsd <- dens$SdDifPrev[nrow(dens)]

#Si elegimos el nivel de precisión deseado hasta las decenas de miles en previsión
ggplot(dens, aes(x=dummy, y=MeanDifPrev)) +
  geom_path(linewidth=0.75, color=paleta(12)[2]) +
  geom_hline(yintercept = lastmean+10000, linewidth=0.8,color="red",linetype="dashed")+
  geom_hline(yintercept = lastmean-10000, linewidth=0.8,color="red",linetype="dashed")+
  scale_y_continuous(labels = comma)+
  labs(x="Iteración", y="Promedio de diferencia de previsión (USD)",
       title = "Convergencia del promedio de diferencia de previsión")+
  theme_light()+
  theme(text= element_text(face="bold",size = 12),
        plot.title = element_text(hjust = 0.5, size=13))
ggsave("D:/Files/Presentations/TallerGIR_Ago2023/convergenceMean.png",
       plot=last_plot(), width=9, height=6, units="in")


ggplot(dens, aes(x=dummy, y=SdDifPrev)) +
  geom_path(linewidth=0.75, color=paleta(12)[2]) +
  geom_hline(yintercept = lastsd+10000, linewidth=0.8,color="red",linetype="dashed")+
  geom_hline(yintercept = lastsd-10000, linewidth=0.8,color="red",linetype="dashed")+
  scale_y_continuous(labels = comma) +
  labs(x="Iteración", y="Desv. est. de la diferencia de Previsión (USD)",
       title = "Convergencia de la desv. est. diferencia de previsión") +
  theme_light() +
  theme(text= element_text(face="bold",size = 12),
        plot.title = element_text(hjust = 0.5, size=13))
ggsave("D:/Files/Presentations/TallerGIR_Ago2023/convergenceSd.png",
       plot=last_plot(), width=9, height=6, units="in")

error <- lastsd/sqrt(500)*qnorm(0.975)
error
