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
library(openxlsx)
library(ggplot2)
library(ca)
library(openxlsx)
require(XLConnect)
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
####____VALIDACIONES____####
bdcRDS <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Dic2022.rds') %>% 
  dplyr::filter(MODULO!=131)

baseA <- bdc %>% 
  select(ctaCont) %>% 
  dplyr::filter(`91_DIAS_INCUMPLIMIENTO`< 30 & ESTADO!='Vigente' & 
                  CTA=='13853') %>% 
  mutate(Inconsistencia='91_DIAS_INCUMPLIMIENTO< 30 & 88_ESTADO!=Vigente & 
                  76_CTA_CONTABLE_NO_DIFERIDA==13853') %>% 
  mutate(Expli_Inc='La inconsistencia se encuentra entre los días de incumplimiento 
         y la subcuenta contable') %>% 
  group_by(`27_NRO_OPERACION`) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

x <- bdc %>%
dplyr::filter(SALDO_CAPITAL_DIFERIDO!=0.0)%>%
dplyr::filter(RUBRO_CAPITAL_DIFERIDO==0)
