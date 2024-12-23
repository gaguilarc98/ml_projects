####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
library(dplyr)
library(stringr)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(expm)
library(quantmod)
library(openxlsx)
library(scales)
library(janitor)
options("encoding" = "UTF-8")
options(scipen = 999)#Para prevenir que se muestren resultados en notación científica
####____FUNCION____####
prevision <- function(x){
  x %>% 
    mutate(prev=case_when(califPot=='A' & tipoCred %in% c('Micro','PyMe') & SEC_PROD==1 & MONEDA == 'MN'~0,
                          califPot=='B' & tipoCred %in% c('Micro','PyMe') & SEC_PROD==1 & MONEDA == 'MN'~0.025,
                          califPot=='A' & tipoCred %in% c('Micro','PyMe') & SEC_PROD==2 & MONEDA == 'MN'~0.0025,
                          califPot=='B' & tipoCred %in% c('Micro','PyMe') & SEC_PROD==2 & MONEDA == 'MN'~0.05,
                          califPot=='A' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'MN'~0.0025,
                          califPot=='B' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'MN'~0.05,
                          califPot=='A' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'MN'~0.03,
                          califPot=='B' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'MN'~0.065,
                          califPot=='A' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'MN'~0.0025,
                          califPot=='B' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'MN'~0.05,
                          califPot=='A' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & FDES <= as.Date("2010-12-16") & MONEDA == 'MN'~0.015,
                          califPot=='A' & tipoCred=='Consumo' & FDES >= as.Date("2010-12-17") & MONEDA == 'MN'~0.03,
                          califPot=='B' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & MONEDA == 'MN'~0.065,
                          califPot=='C' ~ 0.20,
                          califPot=='D' ~ 0.50,
                          califPot=='E' ~ 0.80,
                          califPot=='F' ~ 1,
                          califPot=='A' & tipoCred %in% c('Micro','PyMe') & CONTINGENTE==0 & MONEDA == 'ME'~0.025,
                          califPot=='A' & tipoCred %in% c('Micro','PyMe') & CONTINGENTE==1 & MONEDA == 'ME'~0.01,
                          califPot=='B' & tipoCred %in% c('Micro','PyMe') & MONEDA == 'ME'~0.05,
                          califPot=='A' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'ME'~0.025,
                          califPot=='B' & TIPO_CRED %in% c('H0','H3','H4') & MONEDA == 'ME'~0.05,
                          califPot=='A' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'ME'~0.07,
                          califPot=='B' & TIPO_CRED %in% c('H1','H2') & MONEDA == 'ME'~0.12,
                          califPot=='A' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'ME'~0.025,
                          califPot=='B' & tipoCred=='Consumo' & FDES < as.Date("2009-12-17") & MONEDA == 'ME'~0.05,
                          califPot=='A' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & FDES <= as.Date("2010-12-16") & MONEDA == 'ME'~0.05,
                          califPot=='B' & tipoCred=='Consumo' & FDES >= as.Date("2009-12-17") & FDES <= as.Date("2010-12-16") & MONEDA == 'ME'~0.08,
                          califPot=='A' & tipoCred=='Consumo' & FDES >= as.Date("2010-12-17") & MONEDA == 'ME'~0.07,
                          califPot=='B' & tipoCred=='Consumo' & FDES >= as.Date("2010-12-17") & MONEDA == 'ME'~0.12,
                          califPot=='S' ~ 1,
                          califPot=='Z' ~ 0,)) %>% 
    mutate(previusNew=saldous*prev)
}
####____PREVISION____####
setwd("D:/!bso/girCartera/rds/")
newMonth <- readRDS("ec_Abr2023.rds")

newMonth <- newMonth %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  select(CTACLIENTE, OPERACION, saldous, previus, ctaCont, CALIFICACION,
         TIPO_CRED = TIPO_CREDITO, FDES=fdes, tipoCred, MONEDA,
         SECTOR_CARTERA) %>% 
  mutate(CONTINGENTE = ifelse(ctaCont=="623",1,0)) %>% #Para cartera contingente
  mutate(SEC_PROD = ifelse(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada","2.Otra prod. Controlada","3.C2.Sector Turismo",
                                                 "4.C3.Prod Intelectual","5.C4.Fab,Ens.,Vent.MaqAutHib","7.Prod.Agropec.No Controlada",
                                                 "8.Otra Prod.No Controlada"),1,2)) %>% 
  mutate(MONEDA = ifelse(MONEDA==0,"MN","ME")) %>% 
  mutate(califPot = CALIFICACION) %>% 
  prevision()

