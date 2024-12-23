####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
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
library(forcats)    # Working with factors/categorical data
library(openxlsx)
library(scales)
library(janitor)
library(ggrepel)
remove(list = ls())
options("encoding" = "UTF-8")
options(scipen = 999)

paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.5)


# Data in
bdcList <- list()
file_list <- list.files(path='D:/!bso/girCartera/rds_v3')
for (i in 1:length(file_list)) {
  print(i)
  print(file_list[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds_v3/',
                        file_list[i])) %>% 
    select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, 
           CALIFICACION, fbase, montous, FDESEMBOLSO, saldous, previus,
           AGENCIA, CIU, CAEDEC_DEST, tipoCred, DESC_SEGMERC,
           TIPOTASA, TASAFIJA, TASAVARIABLE, fdes, cosechaY,
           MONEDA, OBJETO_CRED, OPERACION_ORI_REF) %>% 
    mutate(CIU = as.character(CIU),
           CAEDEC_DEST = as.character(CAEDEC_DEST), 
           OPERACION_ORI_REF= as.integer(OPERACION_ORI_REF))
  bdcList[[i]] <- bdc
}
#quietly(gc())
bdcFull <- bind_rows(bdcList) %>%
  mutate(mon = substr(fbase,1,3)) %>%
  mutate(year = substr(fbase,4,7)) %>%
  mutate(mes = case_when(mon == 'Ene'~'jan',
                         mon == 'Feb'~'feb',
                         mon == 'Mar'~'mar',
                         mon == 'Abr'~'apr',
                         mon == 'May'~'may',
                         mon == 'Jun'~'jun',
                         mon == 'Jul'~'jul',
                         mon == 'Ago'~'aug',
                         mon == 'Sep'~'sep',
                         mon == 'Oct'~'oct',
                         mon == 'Nov'~'nov',
                         mon == 'Dic'~'dec',)) %>%
  mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>%
  mutate(monDate = as.yearmon(dayDate)) %>%
  select(-dayDate, -mon, -year, -mes) %>%
  arrange(CI, CTACLIENTE, OPERACION, monDate, FDESEMBOLSO) 
#quietly(gc())
bdcList <- NULL
nrowFull <- nrow(bdcFull)
nopsFull <- length(unique(bdcFull$OPERACION))
ncliFull <- length(unique(bdcFull$CI))
write_rds(bdcFull, 'D:/!bso/girCartera/bdcFullRangos_ene2023.rds')
write.csv(bdcFull, 'D:/!bso/girCartera/bdcFullRangos_ene2023.csv')
#===============================================================================
# Updating last month
remove(list = ls())
gc()
bdcFull_old <- readRDS('D:/!bso/girCartera/bdcFullRangos_feb2023.rds')
bdc_update <- readRDS('D:/!bso/girCartera/rds_v3/ec_Mar2023.rds') %>% 
  select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, 
         CALIFICACION, fbase, montous, FDESEMBOLSO, saldous, previus,
         AGENCIA, CIU, CAEDEC_DEST, tipoCred, DESC_SEGMERC,
         TIPOTASA, TASAFIJA, TASAVARIABLE, OPERACION_ORI_REF,
         MONEDA, OBJETO_CRED, fdes) %>% 
  mutate(CIU = as.character(CIU),
         CAEDEC_DEST = as.character(CAEDEC_DEST)) %>%
  mutate(mon = substr(fbase,1,3)) %>%
  mutate(year = substr(fbase,4,7)) %>%
  mutate(mes = case_when(mon == 'Ene'~'jan',
                         mon == 'Feb'~'feb',
                         mon == 'Mar'~'mar',
                         mon == 'Abr'~'apr',
                         mon == 'May'~'may',
                         mon == 'Jun'~'jun',
                         mon == 'Jul'~'jul',
                         mon == 'Ago'~'aug',
                         mon == 'Sep'~'sep',
                         mon == 'Oct'~'oct',
                         mon == 'Nov'~'nov',
                         mon == 'Dic'~'dec',)) %>%
  mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>%
  mutate(monDate = as.yearmon(dayDate)) %>%
  select(-dayDate, -mon, -year, -mes) %>%
  arrange(CI, CTACLIENTE, OPERACION, monDate, FDESEMBOLSO)

bdcFull_update <- bdcFull_old %>% 
  bind_rows(bdc_update) %>% 
  mutate(cosechaY = year(fdes))

write_rds(bdcFull_update, 'D:/!bso/girCartera/bdcFullRangos_mar2023.rds')

#===============================================================================
remove(list = ls())
gc()
bdcFull <- readRDS('C:/!bso/Cosecha/bdcFullRangos_mar2023.rds')
# fullFeb <- bdcFull_update %>% 
#   dplyr::filter(monDate == 'feb. 2023')

rangos <- bdcFull %>% 
  # mutate(rangos = case_when(saldous <= 500 ~'a. Menor a 500USD',
  #                           saldous > 500 & saldous <= 1000 ~'b. 500-1k',
  #                           saldous > 1000 & saldous <= 3000 ~'c. 1k-3k',
  #                           saldous > 3000 & saldous <= 5000 ~'d. 3k-5k',
  #                           saldous > 5000 & saldous <= 8000 ~'e. 5k-8k',
  #                           saldous > 8000 & saldous <= 10000 ~'f. 8k-10k',
  #                           saldous > 10000 & saldous <= 20000 ~'g. 10k-20k',
  #                           saldous > 20000 ~'h. Mayor a 20k')) %>%
  mutate(rangom = case_when(montous <=  1000 ~'a. Hasta 1k',
                            montous > 1000 & montous <= 3000 ~'b. 1k-3k',
                            montous > 3000 & montous <= 5000 ~'c. 3k-5k',
                            montous > 5000 & montous <= 8000 ~'d. 5k-8k',
                            montous > 8000 & montous <= 10000 ~'e. 8k-10k',
                            montous > 10000 & montous <= 20000 ~'f. 10k-20k',
                            montous > 20000 ~'g. Mayor a 20k')) %>%
  mutate(nops = ifelse(montous > 0, 1, 0)) %>% 
  select(montous, saldous, monDate, rangom, nops) %>% 
  ungroup() %>% 
  group_by(monDate, rangom) %>% 
  summarise_all(sum, na.rm = T) %>% 
  ungroup() %>% 
  group_by(monDate) %>% 
  mutate(totalDes = sum(montous),
         opsDes = sum(nops)) %>% 
  dplyr::filter(monDate >= 'ene. 2017' & rangom != 'a. Menor a 500USD') %>% 
  mutate(pctOps = nops/opsDes*100,
         pctMonto = montous/totalDes*100) %>% 
  dplyr::rename(`Rango Desembolso` = rangom) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1),
         promOD_cat = montous/nops,
         promOD_bso = totalDes/opsDes)
saveRDS(rangos, 'D:/!bso/cosechas/rangos/rangos_mar23.rds')
rangos <- readRDS('D:/!bso/cosechas/rangos/rangos_mar23.rds')
#======================================================================
# Updating rangos
rangos <- readRDS('D:/!bso/cosechas/rangos_mar23.rds')
rangos_ene <- rangos %>% 
  dplyr::filter(monDate < 'mar. 2023')
rangos_feb <- rangos %>% 
  dplyr::filter(monDate == 'mar. 2023')
rangos_update <-  readRDS('D:/!bso/girCartera/rds/ec_Abr2023.rds') %>% 
  select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, 
         CALIFICACION, fbase, montous, FDESEMBOLSO, saldous, previus,
         AGENCIA, CIU, CAEDEC_DEST, tipoCred, DESC_SEGMERC,
         TIPOTASA, TASAFIJA, TASAVARIABLE, OPERACION_ORI_REF,
         MONEDA, OBJETO_CRED, fdes) %>% 
  mutate(CIU = as.character(CIU),
         CAEDEC_DEST = as.character(CAEDEC_DEST)) %>%
  mutate(mon = substr(fbase,1,3)) %>%
  mutate(year = substr(fbase,4,7)) %>%
  mutate(mes = case_when(mon == 'Ene'~'jan',
                         mon == 'Feb'~'feb',
                         mon == 'Mar'~'mar',
                         mon == 'Abr'~'apr',
                         mon == 'May'~'may',
                         mon == 'Jun'~'jun',
                         mon == 'Jul'~'jul',
                         mon == 'Ago'~'aug',
                         mon == 'Sep'~'sep',
                         mon == 'Oct'~'oct',
                         mon == 'Nov'~'nov',
                         mon == 'Dic'~'dec',)) %>%
  mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>%
  mutate(monDate = as.yearmon(dayDate)) %>%
  select(-dayDate, -mon, -year, -mes) %>%
  arrange(CI, CTACLIENTE, OPERACION, monDate, FDESEMBOLSO) %>% 
  mutate(rangom = case_when(montous <=  1000 ~'a. Hasta 1k',
                            montous > 1000 & montous <= 3000 ~'b. 1k-3k',
                            montous > 3000 & montous <= 5000 ~'c. 3k-5k',
                            montous > 5000 & montous <= 8000 ~'d. 5k-8k',
                            montous > 8000 & montous <= 10000 ~'e. 8k-10k',
                            montous > 10000 & montous <= 20000 ~'f. 10k-20k',
                            montous > 20000 ~'g. Mayor a 20k')) %>%
  mutate(nops = ifelse(montous > 0, 1, 0)) %>% 
  select(montous, saldous, monDate, rangom, nops) %>% 
  ungroup() %>% 
  group_by(monDate, rangom) %>% 
  summarise_all(sum, na.rm = T) %>% 
  ungroup() %>% 
  group_by(monDate) %>% 
  mutate(totalDes = sum(montous),
         opsDes = sum(nops)) %>% 
  dplyr::filter(monDate >= 'ene. 2017' & rangom != 'a. Menor a 500USD') %>% 
  mutate(pctOps = nops/opsDes*100,
         pctMonto = montous/totalDes*100) %>% 
  dplyr::rename(`Rango Desembolso` = rangom) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1),
         promOD_cat = montous/nops,
         promOD_bso = totalDes/opsDes) %>% 
  bind_rows(rangos) %>% 
  mutate(monDate = as.Date(monDate,frac=1))

write_rds(rangos_update,'D:/!bso/cosechas/rangos_abr23.rds')
write.xlsx(rangos_update,'D:/!bso/cosechas/rangos_abr23.xlsx')
# updating rangos_ori
rangos_ori <- readRDS('D:/!bso/cosechas/rangos_ori_mar23.rds')
rangos_ori_ene <- rangos_ori %>% 
  dplyr::filter(monDate < 'mar. 2023')
rangos_ori_feb <- rangos_ori %>% 
  dplyr::filter(monDate == 'mar. 2023')
rangos_ori_update <-  readRDS('D:/!bso/girCartera/rds/ec_Abr2023.rds') %>% 
  select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, 
         CALIFICACION, fbase, montous, FDESEMBOLSO, saldous, previus,
         AGENCIA, CIU, CAEDEC_DEST, tipoCred, DESC_SEGMERC,
         TIPOTASA, TASAFIJA, TASAVARIABLE, OPERACION_ORI_REF,
         MONEDA, OBJETO_CRED, fdes, cosechaY) %>% 
  mutate(CIU = as.character(CIU),
         CAEDEC_DEST = as.character(CAEDEC_DEST)) %>%
  mutate(mon = substr(fbase,1,3)) %>%
  mutate(year = substr(fbase,4,7)) %>%
  mutate(mes = case_when(mon == 'Ene'~'jan',
                         mon == 'Feb'~'feb',
                         mon == 'Mar'~'mar',
                         mon == 'Abr'~'apr',
                         mon == 'May'~'may',
                         mon == 'Jun'~'jun',
                         mon == 'Jul'~'jul',
                         mon == 'Ago'~'aug',
                         mon == 'Sep'~'sep',
                         mon == 'Oct'~'oct',
                         mon == 'Nov'~'nov',
                         mon == 'Dic'~'dec',)) %>%
  mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>%
  mutate(monDate = as.yearmon(dayDate)) %>%
  select(-dayDate, -mon, -year, -mes) %>%
  arrange(CI, CTACLIENTE, OPERACION, monDate, fdes) %>% 
  ungroup() %>% 
  group_by(CTACLIENTE) %>% # check grouping
  mutate(hasRefin = sum(OPERACION_ORI_REF),
         yearRefin = ifelse(OPERACION_ORI_REF > 0, year(fdes), 0),
         maxRefin = max(yearRefin)) %>% # identificación de refinanciados
  #dplyr::filter(maxRefin == 2021) %>% 
  mutate(OPERACION_hist = ifelse(OPERACION_ORI_REF > 0, OPERACION_ORI_REF, OPERACION)) %>% # Identificación de # de operación original
  ungroup() %>% 
  group_by(OPERACION_hist) %>% 
  mutate(cosechaY_ori = min(cosechaY)) %>% # La cosecha original es la menor de las dos fechas (desembolso original y refinanciamiento)
  ungroup() %>%
  mutate(yearBase = year(as.Date(monDate, frac = 1))) %>% 
  mutate(montous_ori = ifelse(cosechaY_ori != yearBase, NA , montous)) %>% 
  mutate(rangom = case_when(montous <=  1000 ~'a. Hasta 1k',
                            montous > 1000 & montous <= 3000 ~'b. 1k-3k',
                            montous > 3000 & montous <= 5000 ~'c. 3k-5k',
                            montous > 5000 & montous <= 8000 ~'d. 5k-8k',
                            montous > 8000 & montous <= 10000 ~'e. 8k-10k',
                            montous > 10000 & montous <= 20000 ~'f. 10k-20k',
                            montous > 20000 ~'g. Mayor a 20k')) %>%
  mutate(nops = ifelse(montous > 0 & !is.na(montous), 1, 0)) %>% 
  mutate(nops_ori = ifelse(montous_ori > 0 & !is.na(montous), 1, 0)) %>% 
  select(montous_ori, saldous, monDate, rangom, nops_ori) %>% 
  ungroup() %>% 
  dplyr::rename(montous = montous_ori,
                nops = nops_ori) %>% 
  group_by(monDate, rangom) %>% 
  summarise_all(sum, na.rm = T) %>% 
  ungroup() %>% 
  group_by(monDate) %>% 
  mutate(totalDes = sum(montous),
         opsDes = sum(nops)) %>% 
  dplyr::filter(monDate >= 'ene. 2017' & rangom != 'a. Menor a 500USD') %>% 
  mutate(pctOps = nops/opsDes*100,
         pctMonto = montous/totalDes*100) %>% 
  dplyr::rename(`Rango Desembolso` = rangom) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1),
         promOD_cat = montous/nops,
         promOD_bso = totalDes/opsDes) %>% 
  bind_rows(rangos_ori) %>% 
  mutate(monDate = as.Date(monDate,frac=1))

write_rds(rangos_ori_update,'D:/!bso/cosechas/rangos_ori_abr23.rds')
Range <- list(Rangos = rangos_update, Rangos_ori=rangos_ori_update)
write.xlsx(Range,'D:/!bso/cosechas/Rangos_abr23_v2.xlsx')
#======================================================================

# Tablas
feb23 <- readRDS('D:/!bso/girCartera/rds_v3/ec_Feb2023.rds')
rangos <- readRDS('D:/!bso/cosechas/rangos_abr23.rds')
tab_pctMonto <- rangos %>% 
  ungroup() %>% 
  mutate(yearD = year(dayDate)) %>%
  group_by(yearD) %>% 
  mutate(montoY = sum(montous)) %>% 
  ungroup() %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  mutate(montoY_rango = sum(montous)) %>% 
  ungroup() %>% 
  select(yearD,  `Rango Desembolso`, montoY, montoY_rango) %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(pctMonto = montoY_rango/montoY) %>% 
  ungroup() %>% 
  select(yearD, pctMonto, `Rango Desembolso`) %>%
  pivot_wider(names_from = yearD, values_from = pctMonto) %>% 
  adorn_totals('row') %>% 
  mutate(difYear = `2023` - `2022`) 
  

pctMonto_2023<-rangos %>% 
  dplyr::filter(monDate>'dic. 2022') %>% 
  group_by(monDate) %>% 
  mutate(montoY = sum(montous)) %>% 
  ungroup() %>% 
  group_by(monDate,  `Rango Desembolso`) %>% 
  mutate(montoY_rango = sum(montous)) %>% 
  ungroup() %>% 
  select(monDate,  `Rango Desembolso`, montoY, montoY_rango) %>% 
  group_by(monDate,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(pctMonto = montoY_rango/montoY) %>% 
  ungroup() %>% 
  select(monDate, pctMonto, `Rango Desembolso`) %>% 
  pivot_wider(names_from = monDate, values_from = pctMonto) %>% 
  adorn_totals('row')


tab_pctNops <- rangos %>% 
  ungroup() %>% 
  mutate(yearD = year(dayDate)) %>%
  group_by(yearD) %>% 
  mutate(opY = sum(nops)) %>% 
  ungroup() %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  mutate(opY_rango = sum(nops)) %>% 
  ungroup() %>% 
  select(yearD,  `Rango Desembolso`, opY, opY_rango) %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(pctOpy = opY_rango/opY) %>% 
  ungroup() %>% 
  select(yearD, pctOpy, `Rango Desembolso`) %>% 
  pivot_wider(names_from = yearD, values_from = pctOpy) %>% 
  adorn_totals('row') %>% 
  mutate(difYear = `2023` - `2022`) 


pctNops_2023<-rangos %>% 
  dplyr::filter(monDate>'dic. 2022') %>% 
  group_by(monDate) %>% 
  mutate(opY = sum(nops)) %>% 
  ungroup() %>% 
  group_by(monDate,  `Rango Desembolso`) %>% 
  mutate(opY_rango = sum(nops)) %>% 
  ungroup() %>% 
  select(monDate,  `Rango Desembolso`, opY, opY_rango) %>% 
  group_by(monDate,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(pctOpy = opY_rango/opY) %>% 
  ungroup() %>% 
  select(monDate, pctOpy, `Rango Desembolso`) %>% 
  pivot_wider(names_from = monDate, values_from = pctOpy) %>% 
  adorn_totals('row')

tab_avgNops_0 <- rangos %>% 
  ungroup() %>% 
  mutate(yearD = year(dayDate)) %>%
  group_by(yearD) %>% 
  mutate(montoY = sum(montous),
         nopsY   = sum(nops)) %>% 
  ungroup() %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  mutate(montoY_rango = sum(montous),
         nopsY_rango   = sum(nops)) %>% 
  ungroup() %>% 
  select(yearD,  `Rango Desembolso`, nopsY, nopsY_rango, montoY, montoY_rango) %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(avgNops_rango = montoY_rango/nopsY_rango,
         avgNops = montoY/nopsY )%>% 
  ungroup() %>% 
  select(yearD, avgNops_rango, `Rango Desembolso`) %>% 
  pivot_wider(names_from = yearD, values_from = avgNops_rango) %>% 
  mutate(difYear = `2023` - `2022`) 


tab_avgNops_1 <- rangos %>% 
  ungroup() %>% 
  mutate(yearD = year(dayDate)) %>%
  group_by(yearD) %>% 
  mutate(montoY = sum(montous),
         nopsY   = sum(nops)) %>% 
  ungroup() %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  mutate(montoY_rango = sum(montous),
         nopsY_rango   = sum(nops)) %>% 
  ungroup() %>% 
  select(yearD,  `Rango Desembolso`, nopsY, nopsY_rango, montoY, montoY_rango) %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(avgNops_rango = montoY_rango/nopsY_rango,
         avgNops = montoY/nopsY )%>% 
  ungroup() %>% 
  select(yearD, avgNops, `Rango Desembolso`) %>% 
  pivot_wider(names_from = yearD, values_from = avgNops) %>% 
  mutate(difYear = `2023` - `2022`) %>% 
  dplyr::filter(row_number() == 1) %>% 
  mutate(`Rango Desembolso` = 'Total')


tab_avgNops <- tab_avgNops_0 %>% 
  bind_rows(tab_avgNops_1)

tab_avgNops2023<- rangos %>% 
  dplyr::filter(monDate>'dic. 2022') %>% 
  group_by(monDate) %>% 
  mutate(montoY = sum(montous),
         nopsY   = sum(nops)) %>% 
  ungroup() %>% 
  group_by(monDate,  `Rango Desembolso`) %>% 
  mutate(montoY_rango = sum(montous),
         nopsY_rango   = sum(nops)) %>% 
  ungroup() %>% 
  select(monDate,  `Rango Desembolso`, nopsY, nopsY_rango, montoY, montoY_rango) %>% 
  group_by(monDate,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(avgNops_rango = montoY_rango/nopsY_rango,
         avgNops = montoY/nopsY )%>% 
  ungroup() %>% 
  select(monDate, avgNops_rango, `Rango Desembolso`) %>% 
  pivot_wider(names_from = monDate, values_from = avgNops_rango)

tab_avgNops_2023_1 <- tab_avgNops2023 %>% 
  dplyr::filter(row_number() == 1) %>% 
  mutate(`Rango Desembolso` = 'Total')

 tabAv_2023 <- tab_avgNops2023 %>% 
  bind_rows(tab_avgNops_2023_1)

tab_4 <- rangos %>% 
  dplyr::filter(dayDate =='2017-12-31' | dayDate =='2018-12-31' | 
                dayDate =='2019-12-31' | dayDate =='2020-12-31' | 
                dayDate =='2021-12-31' | dayDate =='2022-3-31' | 
                dayDate =='2022-6-30'  | dayDate >='2022-9-30' ) %>% 
  select(monDate, pctMonto, `Rango Desembolso`) %>% 
  pivot_wider(names_from = monDate, values_from = pctMonto) %>% 
  adorn_totals('row') %>% 
  mutate(difYear = `nov. 2022` - `dic. 2021`,
         difMon  = `nov. 2022` - `oct. 2022` ) 
#write.xlsx(tab, 'C:/!bso/rangos/rangos_1_feb2023.xlsx')
#===============================================================================
#===============================================================================
#===============================================================================
# Tablas con cosecha original
rangos_ori <- bdcFull %>% 
  ungroup() %>% 
  group_by(CTACLIENTE) %>% # check grouping
  mutate(hasRefin = sum(OPERACION_ORI_REF),
         yearRefin = ifelse(OPERACION_ORI_REF > 0, year(fdes), 0),
         maxRefin = max(yearRefin)) %>% # identificación de refinanciados
  #dplyr::filter(maxRefin == 2021) %>% 
  mutate(OPERACION_hist = ifelse(OPERACION_ORI_REF > 0, OPERACION_ORI_REF, OPERACION)) %>% # Identificación de # de operación original
  ungroup() %>% 
  group_by(OPERACION_hist) %>% 
  mutate(cosechaY_ori = min(cosechaY)) %>% # La cosecha original es la menor de las dos fechas (desembolso original y refinanciamiento)
  ungroup() %>%
  mutate(yearBase = year(as.Date(monDate, frac = 1))) %>% 
  mutate(montous_ori = ifelse(cosechaY_ori != yearBase, NA , montous)) %>% 
  mutate(rangom = case_when(montous <=  1000 ~'a. Hasta 1k',
                            montous > 1000 & montous <= 3000 ~'b. 1k-3k',
                            montous > 3000 & montous <= 5000 ~'c. 3k-5k',
                            montous > 5000 & montous <= 8000 ~'d. 5k-8k',
                            montous > 8000 & montous <= 10000 ~'e. 8k-10k',
                            montous > 10000 & montous <= 20000 ~'f. 10k-20k',
                            montous > 20000 ~'g. Mayor a 20k')) %>%
  mutate(nops = ifelse(montous > 0 & !is.na(montous), 1, 0)) %>% 
  mutate(nops_ori = ifelse(montous_ori > 0 & !is.na(montous), 1, 0)) %>% 
  select(montous_ori, saldous, monDate, rangom, nops_ori) %>% 
  ungroup() %>% 
  dplyr::rename(montous = montous_ori,
                nops = nops_ori) %>% 
  group_by(monDate, rangom) %>% 
  summarise_all(sum, na.rm = T) %>% 
  ungroup() %>% 
  group_by(monDate) %>% 
  mutate(totalDes = sum(montous),
         opsDes = sum(nops)) %>% 
  dplyr::filter(monDate >= 'ene. 2017' & rangom != 'a. Menor a 500USD') %>% 
  mutate(pctOps = nops/opsDes*100,
         pctMonto = montous/totalDes*100) %>% 
  dplyr::rename(`Rango Desembolso` = rangom) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1),
         promOD_cat = montous/nops,
         promOD_bso = totalDes/opsDes)

saveRDS(rangos_ori, 'D:/!bso/cosechas/rangos/rangos_ori_mar23.rds')

rangos_ori <- readRDS('D:/!bso/cosechas/rangos_ori_abr23.rds')
# rangos_ori_ene <- readRDS('C:/!bso/rangos/rangos_ori_ene23.rds')
tab_pctMonto_ori <- rangos_ori %>% 
  ungroup() %>% 
  mutate(yearD = year(dayDate)) %>%
  group_by(yearD) %>% 
  mutate(montoY = sum(montous)) %>% 
  ungroup() %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  mutate(montoY_rango = sum(montous)) %>% 
  ungroup() %>% 
  select(yearD,  `Rango Desembolso`, montoY, montoY_rango) %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(pctMonto = montoY_rango/montoY) %>% 
  ungroup() %>% 
  select(yearD, pctMonto, `Rango Desembolso`) %>% 
  pivot_wider(names_from = yearD, values_from = pctMonto) %>% 
  adorn_totals('row') %>% 
  mutate(difYear = `2023` - `2022`) 


tabMontoOri_2023<-rangos_ori %>% 
  dplyr::filter(monDate>'dic. 2022') %>% 
  group_by(monDate) %>% 
  mutate(montoY = sum(montous)) %>% 
  ungroup() %>% 
  group_by(monDate,  `Rango Desembolso`) %>% 
  mutate(montoY_rango = sum(montous)) %>% 
  ungroup() %>% 
  select(monDate,  `Rango Desembolso`, montoY, montoY_rango) %>% 
  group_by(monDate,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(pctMonto = montoY_rango/montoY) %>% 
  ungroup() %>% 
  select(monDate, pctMonto, `Rango Desembolso`) %>% 
  pivot_wider(names_from = monDate, values_from = pctMonto) %>% 
  adorn_totals('row') 


tab_pctNops_ori <- rangos_ori %>% 
  ungroup() %>% 
  mutate(yearD = year(dayDate)) %>%
  group_by(yearD) %>% 
  mutate(opY = sum(nops)) %>% 
  ungroup() %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  mutate(opY_rango = sum(nops)) %>% 
  ungroup() %>% 
  select(yearD,  `Rango Desembolso`, opY, opY_rango) %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(pctOpy = opY_rango/opY) %>% 
  ungroup() %>% 
  select(yearD, pctOpy, `Rango Desembolso`) %>% 
  pivot_wider(names_from = yearD, values_from = pctOpy) %>% 
  adorn_totals('row') %>% 
  mutate(difYear = `2023` - `2022`) 


tabNopsOri_2023<-rangos_ori %>% 
  dplyr::filter(monDate>'dic. 2022') %>%
  group_by(monDate) %>% 
  mutate(opY = sum(nops)) %>% 
  ungroup() %>% 
  group_by(monDate,  `Rango Desembolso`) %>% 
  mutate(opY_rango = sum(nops)) %>% 
  ungroup() %>% 
  select(monDate,  `Rango Desembolso`, opY, opY_rango) %>% 
  group_by(monDate,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(pctOpy = opY_rango/opY) %>% 
  ungroup() %>% 
  select(monDate, pctOpy, `Rango Desembolso`) %>% 
  pivot_wider(names_from = monDate, values_from = pctOpy) %>% 
  adorn_totals('row') 
 

tab_avgNops_ori_0 <- rangos_ori %>% 
  ungroup() %>% 
  mutate(yearD = year(dayDate)) %>%
  group_by(yearD) %>% 
  mutate(montoY = sum(montous),
         nopsY   = sum(nops)) %>% 
  ungroup() %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  mutate(montoY_rango = sum(montous),
         nopsY_rango   = sum(nops)) %>% 
  ungroup() %>% 
  select(yearD,  `Rango Desembolso`, nopsY, nopsY_rango, montoY, montoY_rango) %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(avgNops_rango = montoY_rango/nopsY_rango,
         avgNops = montoY/nopsY )%>% 
  ungroup() %>% 
  select(yearD, avgNops_rango, `Rango Desembolso`) %>% 
  pivot_wider(names_from = yearD, values_from = avgNops_rango) %>% 
  mutate(difYear = `2023` - `2022`) 


tab_avgNops_ori_1 <- rangos_ori %>% 
  ungroup() %>% 
  mutate(yearD = year(dayDate)) %>%
  group_by(yearD) %>% 
  mutate(montoY = sum(montous),
         nopsY   = sum(nops)) %>% 
  ungroup() %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  mutate(montoY_rango = sum(montous),
         nopsY_rango   = sum(nops)) %>% 
  ungroup() %>% 
  select(yearD,  `Rango Desembolso`, nopsY, nopsY_rango, montoY, montoY_rango) %>% 
  group_by(yearD,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(avgNops_rango = montoY_rango/nopsY_rango,
         avgNops = montoY/nopsY )%>% 
  ungroup() %>% 
  select(yearD, avgNops, `Rango Desembolso`) %>% 
  pivot_wider(names_from = yearD, values_from = avgNops) %>% 
  mutate(difYear = `2023` - `2022`) %>% 
  dplyr::filter(row_number() == 1) %>%
  mutate(`Rango Desembolso` = 'Total')

#write.xlsx(tab, 'C:/!bso/rangos/rangos_avgNops_acum_2_feb2023.xlsx')
tab_avgNops_ori <- tab_avgNops_ori_0 %>% 
  bind_rows(tab_avgNops_ori_1)


tabAv_Ori2023<-rangos_ori %>% 
  dplyr::filter(monDate>'dic. 2022') %>% 
  group_by(monDate) %>% 
  mutate(montoY = sum(montous),
         nopsY   = sum(nops)) %>% 
  ungroup() %>% 
  group_by(monDate,  `Rango Desembolso`) %>% 
  mutate(montoY_rango = sum(montous),
         nopsY_rango   = sum(nops)) %>% 
  ungroup() %>% 
  select(monDate,  `Rango Desembolso`, nopsY, nopsY_rango, montoY, montoY_rango) %>% 
  group_by(monDate,  `Rango Desembolso`) %>% 
  summarise_all(mean) %>% 
  mutate(avgNops_rango = montoY_rango/nopsY_rango,
         avgNops = montoY/nopsY )%>% 
  ungroup() %>% 
  select(monDate, avgNops_rango, `Rango Desembolso`) %>% 
  pivot_wider(names_from = monDate, values_from = avgNops_rango)

tabAvOri_1_2023<-tabAv_Ori2023 %>% 
  dplyr::filter(row_number() == 1) %>%
  mutate(`Rango Desembolso` = 'Total')

tab_avg2023 <- tabAv_Ori2023 %>% 
  bind_rows(tabAvOri_1_2023)


tabList = list(tab_pctMonto_SA = tab_pctMonto, tab_pctMonto2023=pctMonto_2023, tab_pctNops_SA = tab_pctNops, 
               tab_pctNops2023 = pctNops_2023, tab_avgNops_SA = tab_avgNops, tabAv = tabAv_2023, 
               tabMontoOri = tabMontoOri_2023, tab_pctMonto_CA = tab_pctMonto_ori,
               tab_pctNops_CA = tab_pctNops_ori, tabNopsOri = tabNopsOri_2023,
               tab_avgNops_CA = tab_avgNops_ori, tabAvOri_2023 = tab_avg2023)

write.xlsx(tabList, 'D:/!bso/cosechas/rangos/ppt_Rangos.xlsx')

####____CON LISTA DE FEATURES____####
OpsBSO <- readRDS('D:/!bso/features/Clientes_Ene15Abr23_v5.rds') %>% 
  group_by(CTACLIENTE) %>% 
  arrange(desc(monDate)) %>% 
  mutate(CI=CI[row_number()==1]) %>% 
  ungroup()

tabMonto_avg <- OpsBSO %>% 
  mutate(FechaReprog = as.Date(FechaReprog)) %>%
  mutate(fdes = if_else(!is.na(FechaReprog) & FechaReprog>fdes,FechaReprog,fdes)) %>%
  mutate(fdes = as.Date(fdes)) %>%
  mutate(FechaRefin = as.Date(FechaRefin)) %>%
  mutate(fdes = if_else(!is.na(FechaRefin) & FechaRefin>fdes,FechaRefin,fdes)) %>%
  mutate(fdes = as.Date(fdes)) %>%  
  dplyr::filter(year(fdes)>=2020) %>% 
  mutate(monDate=as.yearmon(fdes)) %>% 
  mutate(rangos = case_when(MONTOUS<=1e3~'a. Hasta 1k',
                            MONTOUS<=3e3~'b. 1k-3k',
                            MONTOUS<=5e3~'c.3k-5k',
                            MONTOUS<=8e3~'d. 5k-8k',
                            MONTOUS<=10e3~'e. 8k-10k',
                            MONTOUS<=20e3~'f. 10k-20k',
                            MONTOUS>20e3~'g. Mayor a 20k')) %>% 
  mutate(cortes = case_when(year(fdes)<=2022~as.character(year(fdes)),
                            year(fdes)>2022~as.character(as.yearmon(fdes)),)) %>%
  group_by(monDate) %>% 
  mutate(TotalDes=sum(MONTOUS),
         OpsDes=n()) %>% 
  ungroup() %>% 
  group_by(monDate,rangos) %>% 
  summarise(MontoProm = mean(MONTOUS),MontoDes=sum(MONTOUS),Nops=n(),NN=n_distinct(OPERACION),
            TotalDes=max(TotalDes),OpsDes=max(OpsDes)) %>% 
  # pivot_wider(names_from = cortes,values_from = MontoProm) %>% 
  glimpse()

tail(tabMonto_avg %>% group_by(monDate) %>% summarise(MontoDes=sum(MontoDes),n=sum(NN)))

x <- tabMonto_avg %>% dplyr::filter(as.yearmon(fdes)=="Abr. 2023")
y <- rangos_update %>% dplyr::filter(as.yearmon(fdes)=="Abr. 2023") %>% 
  mutate(MONTOUS=MONTO/6.86)
z <- y$OPERACION[!y$OPERACION %in% x$OPERACION]
z <- x %>% 
  left_join(y,by=c("CTACLIENTE","OPERACION"),suffix = c("_x","_y")) %>% 
  dplyr::filter(abs(MONTOUS_x-MONTOUS_y)>2)
