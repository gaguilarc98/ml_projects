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
library(janitor)
library(ggplot2)

Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3","slateblue3",
                             "red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____CRUECE CON PRÉSTAMOS A FUNCIONARIOS____####
funcs <- read_excel("D:/!bso/bases/excel/DetallePrestamosFuncionarios31dic2022.xls",
                    skip = 4)
glimpse(funcs)

lastCierre <- read.csv("D:/!bso/mph/Oreports/lastCierrreUR_Dic2022_3031_v2.csv")

lastCierre <- lastCierre %>% 
  select(Operacion,Instancias_AR,Instancias_UR,NOMBRE_CLIENTE,Saldo_USD) %>% 
  dplyr::filter(Instancias_UR>=1) %>% 
  mutate(Op_recurrente=1)

length(which(funcs$Operacion %in% lastCierre$Operacion))

funcs <- funcs %>% 
  rename(Operacion=Operación) %>% 
  left_join(lastCierre,by="Operacion") %>% 
  mutate(Op_recurrente=ifelse(is.na(Op_recurrente),0,Op_recurrente))
