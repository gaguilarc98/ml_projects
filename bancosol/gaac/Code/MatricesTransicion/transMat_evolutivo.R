####____CARGA DE PAQUETES____####
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
library(quantmod)
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(scales)
library(ggplot2)
library(gt)
library(knitr)
library(openxlsx)
library(kableExtra)
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)

cases <- function(quant,levs,values,default=NA){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  #new <- vector(mode = 'character',length = length(quant))
  new <- rep(default,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
####____CARGA DE BASE DE OPERACIONES____####
tm_ops <- read.csv("D:/!bso/transMat/Oreports/tmOps_Dic22.csv") %>% 
  dplyr::filter(!is.na(trans))
tm_all <- read.csv("D:/!bso/transMat/Oreports/tmAll_Dic22.csv") %>% 
  dplyr::filter(!is.na(trans))

fcut1 <- 'feb. 2020'
fcut2 <- 'oct. 2021'
####____EVOLUCION POR PROBABILIDAD____####
tm2022_prob <- tm_ops %>% 
  mutate(monDate=as.yearmon(monDate)) %>% 
  arrange(monDate) %>% 
  dplyr::filter(monDate <=fcut1 | monDate >= fcut2) %>% 
  select(prob, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = prob) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  replace_na(list(A=0, B=0, C=0, D=0, E=0, `F`=0, S=0, Z=0)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,F,S,Z) %>% 
  mutate(pos = cases(monDate,unique(monDate),1:length(unique(monDate))))

tm2022_prob %>% 
  group_by(monDate) %>% 
  dplyr::filter(cm1=="B") %>% 
  ungroup() %>% 
  select(monDate, pos, A) %>% 
  ggplot(aes(x=pos,y=A))+
  geom_line(color=paleta(12)[4],size=1.25)+
  #geom_text(aes(x=monDate,y=A,label=A),color=paleta(12)[12],vjust=1.5)+
  geom_smooth(method="lm",se=FALSE,color=paleta(12)[8],size=1.5,linetype=2)+
  labs(x="Mes del año",y="Probabilidad de transición",
       title="Evolución de la transición B-A 2022 (en probabilidad)")+
  theme_minimal()+
  theme(plot.title=element_text(size=12,face="bold"))

tm2022_prob %>% 
  group_by(monDate) %>% 
  dplyr::filter(cm1=="A") %>% 
  ungroup() %>% 
  select(monDate, pos, B) %>% 
  ggplot(aes(x=pos,y=B))+
  geom_line(color=paleta(12)[4],size=1.25)+
  #geom_text(aes(x=monDate,y=A,label=A),color=paleta(12)[12],vjust=1.5)+
  geom_smooth(method="lm",se=FALSE,color=paleta(12)[8],size=1.5,linetype=2)+
  labs(x="Mes del año",y="Probabilidad de transición",
       title="Evolución de la transición A-B 2022 (en probabilidad)")+
  theme_minimal()+
  theme(plot.title=element_text(size=12,face="bold"))

####____EVOLUCION POR OPERACIONES____####
tm2022_ops <- tm_ops %>% 
  mutate(monDate=as.yearmon(monDate)) %>% 
  arrange(monDate) %>% 
  dplyr::filter(monDate <=fcut1 | monDate >= fcut2) %>% 
  select(one, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = one) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  replace_na(list(A=0, B=0, C=0, D=0, E=0, `F`=0, S=0, Z=0)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,F,S,Z) %>% 
  mutate(pos = cases(monDate,unique(monDate),1:length(unique(monDate))))

tm2022_ops %>% 
  group_by(monDate) %>% 
  dplyr::filter(cm1=="B") %>% 
  ungroup() %>% 
  select(monDate, pos, A) %>% 
  ggplot(aes(x=pos,y=A))+
  geom_line(color=paleta(12)[4],size=1.25)+
  #geom_text(aes(x=monDate,y=A,label=A),color=paleta(12)[12],vjust=1.5)+
  geom_smooth(method="lm",se=FALSE,color=paleta(12)[8],size=1.5,linetype=2)+
  labs(x="Mes del año",y="Número de operaciones",
       title="Evolución de la transición B-A 2022 (en operaciones)")+
  theme_minimal()+
  theme(plot.title=element_text(size=12,face="bold"))

tm2022_ops %>% 
  group_by(monDate) %>% 
  dplyr::filter(cm1=="A") %>% 
  ungroup() %>% 
  select(monDate, pos, B) %>% 
  ggplot(aes(x=pos,y=B))+
  geom_line(color=paleta(12)[4],size=1.25)+
  #geom_text(aes(x=monDate,y=A,label=A),color=paleta(12)[12],vjust=1.5)+
  geom_smooth(method="lm",se=FALSE,color=paleta(12)[8],size=1.5,linetype=2)+
  labs(x="Mes del año",y="Número de operaciones",
       title="Evolución de la transición A-B 2022 (en operaciones)")+
  theme_minimal()+
  theme(plot.title=element_text(size=12,face="bold"))

####____EVOLUCION POR SALDO____####
tm2022_saldo <- tm_all %>% 
  mutate(monDate=as.yearmon(monDate)) %>% 
  arrange(monDate) %>% 
  dplyr::filter(monDate <=fcut1 | monDate >= fcut2) %>% 
  select(saldous, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = saldous) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  replace_na(list(A=0, B=0, C=0, D=0, E=0, `F`=0, S=0, Z=0)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,F,S,Z) %>% 
  select(-Z) %>% 
  mutate(pos = cases(monDate,unique(monDate),1:length(unique(monDate))))

tm2022_saldo %>% 
  group_by(monDate) %>% 
  dplyr::filter(cm1=="B") %>% 
  ungroup() %>% 
  select(monDate, pos, A) %>% 
  ggplot(aes(x=pos,y=A))+
  geom_line(color=paleta(12)[4],size=1.25)+
  #geom_text(aes(x=monDate,y=A,label=A),color=paleta(12)[12],vjust=1.5)+
  geom_smooth(method="lm",se=FALSE,color=paleta(12)[8],size=1.5,linetype=2)+
  scale_y_continuous(labels=scales::comma)+
  labs(x="Mes del año",y="Saldo",
       title="Evolución de la transición B-A 2022 (en saldo)")+
  theme_minimal()+
  theme(plot.title=element_text(size=12,face="bold"))

tm2022_saldo %>% 
  group_by(monDate) %>% 
  dplyr::filter(cm1=="A") %>% 
  ungroup() %>% 
  select(monDate, pos, B) %>% 
  ggplot(aes(x=pos,y=B))+
  geom_line(color=paleta(12)[4],size=1.25)+
  #geom_text(aes(x=monDate,y=A,label=A),color=paleta(12)[12],vjust=1.5)+
  geom_smooth(method="lm",se=FALSE,color=paleta(12)[8],size=1.5,linetype=2)+
  scale_y_continuous(labels=scales::comma)+
  labs(x="Mes del año",y="Saldo",
       title="Evolución de la transición A-B 2022 (en saldo)")+
  theme_minimal()+
  theme(plot.title=element_text(size=12,face="bold"))
