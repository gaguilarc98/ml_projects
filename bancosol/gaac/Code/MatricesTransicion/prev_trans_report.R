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
library(stringr)    # Working with strings
library(forcats) 
library(scales)
library(janitor)
library(ggplot2)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

cases <- function(quant,levs,values){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- rep(NA,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____READING dprevs####

prevs <- paste0('D:/!bso/transMat/prev/dpprev_',
             c('avg12_dic22','12sensP1P99','12sensP5P95','12sensP10P90','12sensP25P75'),'.csv')
tip <- c('Promedio','P1P99','P5P95','P10P90','P25P75')

densList <- list()
for (i in 1:length(prevs)) {
  dpprev <- read.csv(prevs[i],skip = 2)
  
  names(dpprev) <- c("CALIFICACION","califPot","previus_Sum","previus_Nro",
                     "newPrev_Sum","newPrev_Nro","difPrev_Sum","difPrev_Nro")
  dens <- dpprev %>% 
    group_by(CALIFICACION,califPot) %>%
    mutate(pos=row_number()) %>% 
    ungroup() %>% 
    select(-CALIFICACION,-califPot) %>% 
    group_by(pos) %>%
    summarise_all(sum) %>% 
    mutate(tipo=tip[i])
  
  densList[[i]] <- dens
  
  if(prevs[i]=="D:/!bso/transMat/prev/dpprev_avg12_dic22.csv"){
    q05 <- quantile(dens$newPrev_Sum,0.05,na.rm=T)
    qm <- mean(dens$newPrev_Sum,na.rm=T)
    q95 <- quantile(dens$newPrev_Sum,0.95,na.rm=T)
    plot_dens <- ggplot(dens,aes(x=newPrev_Sum))+
      geom_density(fill="blue",alpha=0.5,color=paleta(1))+
      geom_vline(xintercept = q05,color="blue")+
      geom_vline(xintercept = q95,color="red")+
      geom_vline(xintercept = qm, color="black")+
      geom_text(aes(x=q05,y=5e-7,label=comma(q05)),size=3,color="blue")+
      geom_text(aes(x=q95,y=5e-7,label=comma(q95)),size=3,color="red")+
      geom_text(aes(x=qm,y=5e-7,label=comma(q95)),size=3,color="black")+
      scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
      scale_x_continuous(labels = scales::comma)+
      labs(x="Nueva Previsión (USD)", y="Densidad")+
      theme_minimal()
    ggsave("D:/!bso/transMat/prev/dens_avg12.png",plot_dens,width = 9,height = 6,units = "in")
  }
}

dens_all <- bind_rows(densList) %>% 
  dplyr::rename(`Matriz de transición` = tipo)

m <- NULL  
for (t in tip) {
  m <- c(m,mean(dens_all[dens_all$`Matriz de transición`==t,]$newPrev_Sum, na.rm=T))
}
ggplot(dens_all,aes(x=newPrev_Sum, fill = `Matriz de transición`))+
  geom_density(alpha=0.5)+
  geom_vline(xintercept = sort(m,decreasing = T),color=paleta(8)[5:1])+
  # annotate(geom="text",x=sort(m,decreasing = T),y=seq(14e-7,11e-7,length.out=3),
  #          label=comma(sort(m,decreasing = T)),size=4,color=paleta(12)[1:5])+
  scale_y_continuous(breaks = seq(0,15e-7,3e-7),labels = function(x) format(x, scientific = TRUE))+
  scale_x_continuous(labels = scales::comma)+
  scale_fill_manual(values = paleta(8)[5:1])+
  labs(x="Dif. Previsión USD", y="Densidad")+
  theme_minimal()+ 
  # scale_fill_brewer(palette = 'Set1') +
  theme(plot.title = element_text(size = 10,face="bold"),
        legend.position = 'bottom')
ggsave("D:/!bso/transMat/prev/dens_avg12.png",plot_dens,width=9,height = 6,units="in")

for (i in 1:length(prevs)) {
  dpprev <- read.csv(prevs[i],skip = 2) 
  names(dpprev) <- c("CALIFICACION","califPot","previus_Sum","previus_Nro",
                     "newPrev_Sum","newPrev_Nro","difPrev_Sum","difPrev_Nro")
  dpprev %>% 
    select(-previus_Nro,-newPrev_Nro,-difPrev_Nro) %>% 
    group_by(CALIFICACION,califPot) %>%
    summarise_all(mean) %>% 
    ungroup() %>% 
    select(-califPot) %>% 
    group_by(CALIFICACION) %>% 
    summarise_all(sum) %>% 
    adorn_totals("row") %>% 
    mutate(across(previus_Sum:difPrev_Sum,~ .x/1000)) %>% 
    mutate(across(previus_Sum:difPrev_Sum,~format(.x, big.mark=",",digits=1,scientific=F))) 
}

####____RECONSTRUCTING TRANSITION MATRICES____####
cumdif <- function(x){
  new <- rep(NA, length(x))
  new[1] <- x[1]
  for(i in 2:length(x)){
    new[i] <- x[i]-x[i-1]
  }
  return(new)
}
tm <- read.csv("D:/!bso/transMat/tm22_avg12.csv")

M <- tm %>% 
  pivot_longer(cols = everything(),names_to = "trans",values_to = "cumprob") %>% 
  mutate(cm1=substr(trans,1,1)) %>% 
  mutate(cmt=substr(trans,2,2)) %>% 
  group_by(cm1) %>% 
  mutate(prob=cumdif(cumprob)) %>% 
  ungroup() %>% 
  select(-cumprob,-trans) %>% 
  pivot_wider(names_from = cmt,values_from = prob) %>%
  select(-cm1)

M <- data.matrix(M)/100
MM <- as_tibble(M%^%(1/12))
