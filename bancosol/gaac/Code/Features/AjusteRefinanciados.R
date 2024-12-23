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
library(fastDummies)
library(openxlsx)
# require(XLConnect)
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
####____OPERACIONES REFINANCIADAS CON CODIGO ORIGINAL____####
####____AJUSTE DE FECHA DE DESEMBOLSO POR REFINANCIAMIENTO____####
Clientes <- readRDS("D:/!bso/features/Clientes_Ene15Oct23.rds")

Clientes_ori <- Clientes %>% 
  dplyr::filter(is.na(FechaRefin) | FechaRefin>=as.Date("2015-02-01")) %>% 
  mutate(fdes_original=fdes)

Clientes_desem <- Clientes %>% 
  select(CTACLIENTE, OPERACION, OPERACION_ORI_REF, fdes)

flag <- TRUE
while (flag) {
  no_ref_prev <- length(which(Clientes_ori$OPERACION_ORI_REF!=0))
  Clientes_ori <- Clientes_ori %>% 
    left_join(Clientes_desem, by=c("CTACLIENTE","OPERACION_ORI_REF"="OPERACION"),
              suffix=c("_old","_new")) %>% 
    mutate(OPERACION_ORI_REF = if_else(is.na(OPERACION_ORI_REF_new),OPERACION_ORI_REF,OPERACION_ORI_REF_new)) %>% 
    mutate(fdes_original = if_else(is.na(fdes_new),fdes_original,fdes_new)) %>% 
    mutate(OPERACION_ORI_REF = if_else(OPERACION_ORI_REF!=0 & fdes_original<as.Date("2015-02-01"),0,OPERACION_ORI_REF)) %>% 
    mutate(fdes = fdes_old) %>% 
    select(-fdes_old, -fdes_new,-OPERACION_ORI_REF_new)
  no_ref <- length(which(Clientes_ori$OPERACION_ORI_REF!=0))
  print(no_ref)
  if(no_ref==0 | no_ref==no_ref_prev){
    flag <- FALSE
  }  
}

Clientes_leftout  <- Clientes %>% 
  mutate(fdes_original = fdes) %>% 
  anti_join(Clientes_ori,by=c("CTACLIENTE","OPERACION"))

Clientes_ori <- Clientes_ori %>% 
  bind_rows(Clientes_leftout)
saveRDS(Clientes_ori,"D:/!bso/features/Clientes_AjusteRef_Ene15Oct23.rds")
