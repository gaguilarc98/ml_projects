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
####____DESAGREGACION DE SECTOR CARTERA POR CONTROL PRESUPUESTARIO____####
myrds <- c('Ene2023','Feb2023','Mar2023','Abr2023','May2023')
secList <- list()
codSector <- read_excel("D:/!bso/bases/excel/codSectorCarteraJM.xlsx") %>% 
  mutate(across(starts_with('N0'),~as.character(.x)))

for (i in 1:length(myrds)) {
  print(myrds[i])
  bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",myrds[i],".rds")) %>% 
    dplyr::filter(!MODULO %in% c(131,29)) %>% 
    mutate(PlanCuenta ="PRE",
           Company="MKT") %>% 
    mutate(BOB = saldous*6.86) %>% 
    # mutate(MONTOUS = ifelse(MONEDA==0, MONTO/6.86,MONTO)) %>%
    mutate(D03 = case_when(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada", "2.Otra prod. Controlada",
                                                 "6.Vivienda Controlada", "3.C2.Sector Turismo", "4.C3.Prod Intelectual","5.C4.Fab,Ens.,Vent.MaqAutHib")~"Controlada",
                           SECTOR_CARTERA %in% c("7.Prod.Agropec.No Controlada","8.Otra Prod.No Controlada",
                                                 "9.Vivienda No controlada","10.Comercio","11.Servicios","12.Consumo")~"No Controlada",
                           TRUE~SECTOR_CARTERA)) %>% 
    mutate(Desc = case_when(SECTOR_CARTERA == "1.Prod. Agropec. Controlada"~'Producción Agropecuaria Controlada',
                           SECTOR_CARTERA %in% c("2.Otra prod. Controlada","3.C2.Sector Turismo", 
                                                 "4.C3.Prod Intelectual","5.C4.Fab,Ens.,Vent.MaqAutHib")~'Otra Producción Controlada',
                           SECTOR_CARTERA == "6.Vivienda Controlada"~'Vivienda Controlada',
                           SECTOR_CARTERA == "7.Prod.Agropec.No Controlada"~'Producción Agropecuaria No Controlada',
                           SECTOR_CARTERA == "8.Otra Prod.No Controlada"~'Otra Producción No Controlada',
                           SECTOR_CARTERA == "9.Vivienda No controlada"~ 'Vivienda No Controlada',
                           SECTOR_CARTERA == "10.Comercio"~'Comercio',
                           SECTOR_CARTERA == "11.Servicios"~'Servicios',
                           SECTOR_CARTERA == "12.Consumo"~'Consumo',)) %>% 
    # mutate(rangom = case_when(MONTOUS<=2000~"1Hasta2000",
    #                           MONTOUS<=5000~"2Hasta5000",
    #                           MONTOUS<=10000~"3Hasta10000",
    #                           MONTOUS<=20000~"4Hasta20000",
    #                           MONTOUS<=50000~"5Hasta50000",
    #                           MONTOUS>50000~"6>50000",)) %>% 
    group_by(monDate,Desc) %>% 
    summarise(BOB = sum(BOB)) %>% 
    ungroup() %>% 
    select(Fecha = monDate, Desc, BOB) %>% 
    left_join(codSector, by="Desc") %>% 
    mutate(Fecha = as.Date(Fecha, frac=1))
  secList[[i]] <- bdc
}
secFull <- rbindlist(secList) %>% 
  relocate(Desc, BOB, .before = D01) %>% 
  arrange(Fecha,N01,N02,N03,N05,N06,N07,N08)

write_xlsx(secFull, "D:/!bso/Control_Presupuestario_Ene2023May2023.xlsx")
