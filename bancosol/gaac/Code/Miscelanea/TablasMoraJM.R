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

remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)


bdc <- readRDS('D:/!bso/girCartera/rds/ec_Abr2023.rds')

Tabla <- bdc %>% 
  dplyr::filter(ESTADO!='CASTIGADA') %>%
  dplyr::filter(MODULO!=131) %>%
  select(CTACLIENTE,OPERACION,ESTADO,MODULO, saldous, saldoMora, par0, DIASMORA) %>% 
  mutate(OpMora = ifelse(DIASMORA>0,1,0)) %>% 
  mutate(RangoDiasMora = case_when(DIASMORA<=20~as.character(DIASMORA),
                                   DIASMORA>20~'20+'))

write_xlsx(Tabla,"D:/!bso/TablaDiasMora_Abr2023.xlsx")
  