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

Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
paleta <- colorRampPalette(c("slateblue4","purple4","slateblue3","darkorchid3","red3","tan2","yellow2","snow2"),bias=1.5)
cases <- function(quant,levs,values){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- vector(mode = 'character',length = length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}

####____READING BDCFULL____####
bdcFull <- readRDS('D:/!bso/transMat/bdcFull.rds')

####____COUNTING LOANS____####
bdcTrans <- bdcFull %>% 
  select(OPERACION, CALIFICACION, monDate, saldous, previus, saldoCast) %>% 
  #mutate(monDate = zoo::as.yearmon(monDate)) %>% 
  #select(-dayDate) %>%
  #dplyr::filter(monDate >= 'Jan 2017') %>% 
  mutate(saldous = ifelse(CALIFICACION == 'S', saldoCast, saldous)) %>% 
  select(-saldoCast) %>% 
  group_by(OPERACION) %>%
  arrange(OPERACION, monDate) %>%
  dplyr::rename(cmt = CALIFICACION) %>% 
  mutate(cm1 = dplyr::lag(cmt, 1)) %>% 
  mutate(difPrev = previus - dplyr::lag(previus)) %>% 
  ungroup() 
bdcTrans <- bdcTrans %>% 
  mutate(trans = ifelse(cm1 %in% c('A','B','C','D','E','F') &
                          cmt %in% c('A','B','C','D','E','F','S'),paste0(cm1,cmt),NA)) %>% 
  mutate(trans = ifelse(cm1=='S'&cmt=='S',paste0(cm1,cmt),trans)) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1, monDate) %>% 
  mutate(rowTot = n()) %>% 
  ungroup() %>%
  mutate(det = ifelse(cm1 < cmt,1,0)) %>% 
  glimpse()

table(bdcTrans[bdcTrans$monDate == 'sep. 2022',]$trans)
write_rds(bdcTrans, 'D:/!bso/transMat/bdcTrans.rds')

####___AGGREGATE TRANSITION MATRICES____####
bdcCancel <- readRDS('D:/!bso/transMat/matCancel.rds') 
tm_ops <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, trans) %>%
  summarise_all(sum) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(bdcCancel) %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% 
  group_by(monDate, cm1) %>% 
  mutate(rowTot = sum(one)) %>% 
  mutate(prob = round(one/rowTot*100,2)) %>% 
  arrange(cm1, monDate) %>% 
  mutate(Deterioro = case_when(cm1 == 'A' ~ sum(prob[!(cmt %in% c('A','Z'))]), 
                               cm1 == 'B' ~ sum(prob[!(cmt %in% c('A','B','Z'))]),
                               cm1 == 'C' ~ sum(prob[!(cmt %in% c('A','B','C','Z'))]),
                               cm1 == 'D' ~ sum(prob[!(cmt %in% c('A','B','C','D','Z'))]),
                               cm1 == 'E' ~ sum(prob[!(cmt %in% c('A','B','C','D','E','Z'))]),
                               cm1 == 'F' ~ sum(prob[!(cmt %in% c('A','B','C','D','E','F','Z'))]),
                               TRUE ~ 0)) %>% 
  mutate(Recuperacion = case_when(cm1 == 'A' ~ sum(prob[cmt == 'Z']),
                                  cm1 == 'Z' ~ 0,
                                  cm1 == 'B' ~ sum(prob[cmt %in% c('A','Z')]),
                                  cm1 == 'C' ~ sum(prob[cmt %in% c('A','B','Z')]),
                                  cm1 == 'D' ~ sum(prob[cmt %in% c('A','B','C','Z')]),
                                  cm1 == 'E' ~ sum(prob[cmt %in% c('A','B','C','D','Z')]),
                                  cm1 == 'F' ~ sum(prob[cmt %in% c('A','B','C','D','E','Z')]),
                                  cm1 == 'S' ~ sum(prob[cmt == 'Z']),
                                  TRUE ~ 0)) %>% 
  mutate(Permanencia = case_when(cm1 == 'A' ~ sum(prob[cmt == 'A']), 
                                 cm1 == 'B' ~ sum(prob[cmt == 'B']),
                                 cm1 == 'C' ~ sum(prob[cmt == 'C']),
                                 cm1 == 'D' ~ sum(prob[cmt == 'D']),
                                 cm1 == 'E' ~ sum(prob[cmt == 'E']),
                                 cm1 == 'F' ~ sum(prob[cmt == 'F']),
                                 cm1 == 'S' ~ sum(prob[cmt == 'S']),
                                 cm1 == 'Z' ~ sum(prob[cmt == 'Z']),
                                 TRUE ~ 0))
bdcTrans <- readRDS("D:/!bso/transMat/bdcTrans.rds")

####____MATRICES DE TRANSICIÓN MENSUALES____####
tm2022_prob <- tm_ops %>% 
  dplyr::filter(monDate > 'dic. 2021') %>% 
  select(prob, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = prob) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  #select(-`NA`) %>% 
  replace_na(list(A = 0, B = 0, C = 0, D=0,E=0, `F` = 0, S = 0, Z = 0)) %>% 
  mutate(monDate=as.character(monDate)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,F,S,Z)
write_xlsx(tm2022_prob, 'D:/!bso/transMat/tmprob_2022.xlsx')