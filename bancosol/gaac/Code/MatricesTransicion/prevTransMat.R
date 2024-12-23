####____CARGA DE LIBRERÍAS____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(lubridate)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(expm)
library(quantmod)
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(janitor)
library(openxlsx)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(ggrepel)
options("encoding" = "UTF-8")
#Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
paleta <- colorRampPalette(c("midnightblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____DENSITY PLOTS____####
dp <- paste0('D:/!bso/transMat/prevMar23/dpprev_',
             c('avg12','avg12_sens_ab','avg12_sens_ba','P5P95',
               'P10P90','P25P75'),'.csv')
tip <- c('Promedio','A-B +0.05%', 'B-A +5%','Perc 5 y Perc 95','Perc 10 y Perc 90',
         'Perc 25 y Perc 75')
densList <- list()
for (i in 1:length(dp)) {
  dpprev_pct <- read.csv(dp[i])
  
  dens <- dpprev_pct %>% 
    group_by(CALIFICACION,califPot) %>%
    mutate(pos=row_number()) %>% #Agrego el nro de simulacion 
    ungroup() %>% 
    select(-CALIFICACION,-califPot) %>% 
    group_by(pos) %>% #Agrupo por nro de simulacion realizada
    summarise_all(sum) %>% 
    mutate(tipo = tip[i])
  if(i==1){
    quant <- dens %>% 
      mutate(difPrev=difPrev/1e6) %>% 
      summarise(prev_avg = mean(difPrev),
                prev_sdm = prev_avg-2*sd(difPrev),
                prev_sdM = prev_avg+2*sd(difPrev)) %>% 
      pivot_longer(cols = everything(),names_to = "Measure",values_to = "Value") %>% 
      mutate(lab = paste0(round(Value,1)," MM USD")) %>% 
      mutate(Measure=case_when(Measure=="prev_avg"~"Promedio",
                               Measure=="prev_sdm"~"Promedio - 2 Desv. Est.",
                               Measure=="prev_sdM"~"Promedio + 2 Desv. Est.",))
    plot_dens <- ggplot(dens,aes(x=difPrev/1e6))+
      geom_density(alpha=0.6,fill=paleta(12)[3])+
      geom_vline(data=quant,aes(xintercept=Value,color=Measure),size=1)+
      geom_label(data=quant,aes(x=Value,y=0.5,label=lab,color=Measure),
                 size=3.5,show.legend = F)+
      scale_color_manual(values = paleta(12)[c(5,7,6)])+
      scale_x_continuous(breaks = seq(10,20,1),labels = scales::comma)+
      labs(x="Diferencia de Previsión (MM USD)", y="Densidad",color = "Medida")+
      theme_light()+ 
      theme(text = element_text(size = 12),
            legend.position = "bottom")
    ggsave("D:/!bso/transMat/prevMar23/dens_avg12.png",plot_dens,width = 9,height = 6,units = "in")
  densList[[i]] <- dens
  }
}

dens_all <- bind_rows(densList)

mean_prev <- dens_all %>% 
  group_by(tipo) %>% 
  summarise(previusNew=mean(previusNew,na.rm = T), difPrev=mean(difPrev,na.rm=T)) %>% 
  mutate(labmean = paste0(comma(round(previusNew/1e6,1))," MM USD")) %>% 
  mutate(labdif = paste0(comma(round(difPrev/1e6,1))," MM USD"))

ggplot(dens_all,aes(x=difPrev/1e6, fill = tipo))+
  geom_density(alpha=0.55)+
  geom_vline(data=mean_prev, aes(xintercept = difPrev/1e6),color=paleta(8)[-c(1:2)],size=1,show.legend = F)+
  scale_fill_manual(values = paleta(8)[-c(1:2)])+
  # geom_label_repel(data=mean_prev,aes(x=previusNew/1e6,y=6e-1,label=lab),size=3.5,
  #                  show.legend = F)
  annotate(geom="label_repel",x=mean_prev$difPrev/1e6,y=6e-1,
           color = paleta(8)[-c(1:2)],
           label=mean_prev$labdif,size=3.5)+
  scale_x_continuous(breaks = seq(10,50,5),labels = scales::comma)+
  labs(x="Diferencia de Previsión (MM USD)", y="Densidad", fill="Matriz de transición")+
  theme_light()+ 
  theme(plot.title = element_text(size = 10,face="bold"),
        legend.position = 'bottom')
ggsave('D:/!bso/transMat/prevMar23/dens_all_mar23.png',width = 9,height = 6,units = "in")
