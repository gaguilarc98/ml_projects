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
library(openxlsx)
library(ca)
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
####____READING RDS____####
bdcNov <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Nov2022.rds')
bdcOct <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Oct2022.rds')
xx <- bdcNov %>% 
  dplyr::filter(OPERACION==02635132)
ww <- bdcOct %>% 
  dplyr::filter(OPERACION==02635132)

RafaNov <- read_excel('D:/!bso/girCartera/validacion/rafavalidacion.xlsx',sheet = "Obs 2 - Anexo I",
                      skip = 1)
Rafa2<- RafaNov %>% 
  mutate(OPERACION=str_extract(`27_NRO_OPERACION...27`,pattern = "\\d+$")) %>% 
  mutate(OPERACION = as.numeric(OPERACION))
check1 <- bdcNov %>% 
  mutate(FVEN_ULTPAGO=as.Date(FVEN_ULTPAGO,"%d/%m/%y")) %>% 
  mutate(FVEN_PROXPAGO=as.Date(FVEN_PROXPAGO,"%d/%m/%y")) %>% 
  mutate(FULT_PAGO=as.Date(FULT_PAGO,"%d/%m/%y")) %>% 
  dplyr::filter(FVEN_ULTPAGO<FULT_PAGO & month(FVEN_ULTPAGO)!=month(FULT_PAGO) & DIASMORA==0)

Rafa2$OPERACION[which(!(Rafa2$OPERACION %in% check1$OPERACION))]
