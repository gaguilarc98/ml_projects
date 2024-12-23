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
####____CARTERA CASTIGADA Y PRODUCTIVIDAD____####
myCast <- c('Jun2023')
castList <- list()
for (i in 1:length(myCast)) {
  print(myCast[i])
  bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",myCast[i],".rds")) %>% 
    dplyr::filter(!MODULO %in% c(131,29)) %>% 
    mutate(opsCast = ifelse(saldoCast>0,1,0)) %>% 
    select(monDate,Regional, Sucursal, saldoCast, saldous, opsCast, opTot) %>% 
    group_by(monDate, Regional,Sucursal) %>% 
    summarise(SaldoCastigado = sum(saldoCast), CarteraBruta = sum(saldous),
              OperacionesCastigadas = sum(opsCast), OperacionesActivas = sum(opTot),
              PorcentajeCastigado_Saldo = SaldoCastigado/CarteraBruta, 
              PorcentajeCastigado_Operacion = OperacionesCastigadas/OperacionesActivas) %>% 
    ungroup() %>% 
    mutate(monDate = as.Date(monDate, frac=1))
  castList[[i]] <- bdc
}
castFull <- rbindlist(castList) 

write_xlsx(castFull, "D:/!bso/requests/CastigosXRegional_Jun23.xlsx")