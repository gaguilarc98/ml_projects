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
library(ggrepel)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____SEGMENTACION DE CARTERA POR TIPO DE CLIENTE____####
seq <- fread("D./!bso/girCartera/")
Clientes <- readRDS("D:/!bso/features/Clientes_Ene15Abr23_v7.rds")
bdcX <- bdc %>% select(CTACLIENTE, OPERACION) %>% 
  mutate(enCierre=1)
CNew <- Clientes %>% 
  left_join(bdcX,by=c("CTACLIENTE","OPERACION")) %>% 
  dplyr::filter(CTACLIENTE %in% bdcX$CTACLIENTE) %>% 
  mutate(fdesFoto = if_else(enCierre==1,fdes,NA)) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(fdesFoto=max(fdesFoto,na.rm=T)) %>% 
  mutate(esAntiguo = ifelse(fdes<fdesFoto, 1,0)) %>% 
  summarise(esAntiguo = max(esAntiguo))
bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_Dic2022.rds")) %>% 
  left_join(CNew, by="CTACLIENTE") %>% 
  replace_na(list(esAntiguo=0)) %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"), 1, 0)) %>% 
  mutate(Tipo_Cartera = case_when(OPERACION_ORI_REF!=0 ~ 'Refinanciada',
                                  ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                  esAntiguo==1~'Retorno',
                                  TRUE ~ 'Normal')) %>% 
  select(monDate, CTACLIENTE, OPERACION, MODULO, TIPO_OPER, ctaCont, saldous, par0, saldoMora, TIPO_CLIENTE,
         esFSL, OPERACION_ORI_REF, Tipo_Cartera)

list <- c('Dic2022','Ene2023','Feb2023','Mar2023','Abr2023','May2023','Jun2023','Jul2023','Ago2023')
bdcList <- list()
for (i in 1:length(list)) {
  print(list[i])
  bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",list[i],".rds")) %>% 
    dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
    mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"), 1, 0)) %>% 
    mutate(Tipo_Cartera = case_when(OPERACION_ORI_REF!=0 ~ 'Refinanciada',
                                    ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                    TIPO_CLIENTE=="RETORNO"~'Retorno',
                                    TRUE ~ 'Normal')) %>% 
    select(monDate, CTACLIENTE, OPERACION, MODULO, TIPO_OPER, ctaCont, saldous, par0, saldoMora, TIPO_CLIENTE,
           esFSL, OPERACION_ORI_REF, Tipo_Cartera)
  
  check <- bdc %>% 
    group_by(monDate, MODULO, TIPO_OPER, TIPO_CLIENTE, ctaCont, Tipo_Cartera, esFSL) %>% 
    summarise(across(c(saldous, par0, saldoMora),~sum(.x))) %>% 
    ungroup() %>% 
    mutate(monDate = as.Date(monDate, frac=1))
  
  bdcList[[i]] <- check
}

bdcFull <- rbindlist(bdcList)
write_xlsx(bdcFull, "D:/!bso/girCartera/TipoCartera_Dic2022Ago2023.xlsx")


for (i in 1:length(list)) {
  print(list[i])
  cic <- readRDS(paste0("D:/!bso/CIC/rds/cic_",list[i],".rds")) %>% 
    dplyr::filter(CuentaContable.y %in% c('131','133','134','135','136','137')) %>% 
    dplyr::filter(!(CuentaContable.y=='865' & SaldoCastigado==0)) %>% 
    mutate(esFSL = ifelse(TipoProducto=="CARTERA MIGRADA", 1, 0)) %>% 
    mutate(Tipo_Cartera = case_when(!is.na(IdOperacionOrigen) ~ 'Refinanciada',
                                    ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                    TIPO_CLIENTE=="RETORNO"~'Retorno',
                                    TRUE ~ 'Normal')) %>% 
    select(monDate, CTACLIENTE, OPERACION, MODULO, TIPO_OPER, ctaCont, saldous, par0, saldoMora, TIPO_CLIENTE,
           esFSL, OPERACION_ORI_REF, Tipo_Cartera)
  
  check <- bdc %>% 
    group_by(monDate, MODULO, TIPO_OPER, TIPO_CLIENTE, ctaCont, Tipo_Cartera, esFSL) %>% 
    summarise(across(c(saldous, par0, saldoMora),~sum(.x))) %>% 
    ungroup() %>% 
    mutate(monDate = as.Date(monDate, frac=1))
  
  bdcList[[i]] <- check
}

bdcFull <- rbindlist(bdcList)
write_xlsx(bdcFull, "D:/!bso/girCartera/TipoCartera_Dic2022Ago2023.xlsx")