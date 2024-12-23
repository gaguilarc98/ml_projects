
library(dplyr)
library(foreign)
library(reshape)
library(reshape2)
library(stringr)
library(lubridate)
library(Hmisc)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(forecast)
library(quantmod)
library(astsa)
library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(timeSeries)
library(tseries)
library(xtable)
library(openxlsx)
library(hrbrthemes)
library(viridis)
library(scales)
library(janitor)
library(RColorBrewer)
library(paletteer)
library(plotly)
library(kableExtra)
library(glmnet)
remove(list = ls())
options("encoding" = "UTF-8")
#Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
#===============================================================================
# Data in
bdcList <- list()
file_list <- list.files(path='D:/!bso/girCartera/rds_v3')
for (i in 1:length(file_list)) {
  
  #print(file_list[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds_v3/',
                        file_list[i])) %>% 
    select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, 
           CALIFICACION, fbase, montous, FDESEMBOLSO, saldous)
  bdcList[[i]] <- bdc
}
#quietly(gc())

#-------------------------------
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
write_rds(bdcFull, 'D:/!bso/vipRC/bdcFull_nov2022.rds')
#===============================================================================
#  Update last month
bdcFull_old <- readRDS('D:/!bso/vipRC/bdcFull_VIP_feb2023.rds')
bdc_new <- readRDS('D:/!bso/girCartera/rds_v3/ec_Mar2023.rds') %>% 
  select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, 
         CALIFICACION, fbase, montous, FDESEMBOLSO, saldous) %>%
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

bdc_update <- bdcFull_old %>% 
  bind_rows(bdc_new)

write_rds(bdc_update, 'D:/!bso/vipRC/bdcFull_VIP_mar2023.rds')
#===============================================================================
# Primeros filtros
remove(list =ls())
bdcFull <- readRDS('D:/!bso/vipCartera/bdcFull_VIP_feb2023.rds')
bdcVIP2 <- bdcFull %>% 
  dplyr::filter(monDate <= 'mar. 2020'| monDate >= 'ene. 2021') %>% 
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  dplyr::filter(fdes>=as.Date("2015-01-01")) %>% 
  select(OPERACION, CTACLIENTE, CI, DIASMORA, montous, fdes, monDate, GENERO) %>%
  mutate(eom = as.Date(monDate, frac = 1)) %>%  
  mutate(loanDays = as.integer(eom - fdes)) %>% # check measure
  group_by(CTACLIENTE) %>% 
  arrange(desc(fdes)) %>% 
  mutate(CI = CI[row_number()==1]) %>% 
  ungroup() %>% 
  group_by(CTACLIENTE, CI) %>% 
  mutate(totalLoanDays = sum(loanDays, na.rm = T)) %>% # check measure
  mutate(moraAcum = sum(DIASMORA, na.rm = T)) %>% # check measure
  mutate(moraMax = max(DIASMORA, na.rm = T)) %>% # check measure
  mutate(totalMonto = sum(montous, na.rm = T)) %>% 
  mutate(maxMonto = max(montous, na.rm = T)) %>% # check measure
  mutate(minMonto = min(montous, na.rm = T)) %>% # check measure
  mutate(montoRat = maxMonto/minMonto) %>% # CHECK MEASURE
  mutate(pct_maxDM_acumDM = ifelse(moraAcum > 0, moraMax/moraAcum, 0)) %>% # check measure
  mutate(pct_acumDM_TLD = ifelse(totalLoanDays > 0, moraAcum/totalLoanDays, 0))
  
  mutate(pct_acumDM_TLD = ifelse(pct_acumDM_TLD > 1, 1, pct_acumDM_TLD)) %>% # check measure
  mutate(totalNops = sum(nops)) %>%# check measure
  select(-montous, -eom, -DIASMORA, -fdes, -monDate, -OPERACION, -nops) %>% 
  summarise_all(max, na.rm = T) %>% # check grouping
  glimpse() %>% 
  #dplyr::filter(totalLoanDays > 360) %>% 
  mutate(moraMaxBin = case_when(moraMax == 0~'1. 0 días',
                                moraMax > 0 & moraMax <= 5~'2. 1-5 días',
                                moraMax > 5 & moraMax <= 10~'3. 6-10 días',
                                moraMax > 10 & moraMax <= 15~'4. 11-15 días',
                                moraMax > 15 & moraMax <= 20~'5. 16-20 días',
                                moraMax > 20 ~'6. 20+ días')) %>% 
  mutate(moraAcumBin = case_when(moraAcum == 0~'1. 0 días',
                                 moraAcum > 0 & moraAcum <= 5~'2. 1-5 días',
                                 moraAcum > 5 & moraAcum <= 10~'3. 6-10 días',
                                 moraAcum > 10 & moraAcum <= 15~'4. 11-15 días',
                                 moraAcum > 15 & moraAcum <= 20~'5. 16-20 días',
                                 moraAcum > 20 ~'6. 20+ días')) %>% 
  mutate(pctMaxBin = case_when(pct_maxDM_acumDM > 0.9~'1. 90% +',
                               pct_maxDM_acumDM > 0.8 & pct_maxDM_acumDM <= 0.9~'2. 80%-90%',
                               pct_maxDM_acumDM > 0.7 & pct_maxDM_acumDM <= 0.8~'3. 70%-80%',
                               pct_maxDM_acumDM <= 0.7 & pct_maxDM_acumDM > 0 ~'4. 1-70%',
                               pct_maxDM_acumDM == 0~'5. 0 (sin morosidad)')) %>% 
  mutate(pctAcumBin = case_when(pct_acumDM_TLD == 0~'1. 0%',
                                pct_acumDM_TLD > 0 & pct_acumDM_TLD <= 0.03~'2. 1%-3%',
                                pct_acumDM_TLD > 0.03 & pct_acumDM_TLD <= 0.05~'3. 3%-5%',
                                pct_acumDM_TLD > 0.05 & pct_acumDM_TLD <= 0.07~'4. 5%-7%',
                                pct_acumDM_TLD > 0.07~'5. 7% +')) %>% 
  mutate(tldBin =case_when(totalLoanDays <= 360 ~ '0. Menos de 360 días',
                           totalLoanDays > 360 & totalLoanDays <=1080 ~'1. 360-1080 días',
                           totalLoanDays > 1080 & totalLoanDays <=2160 ~'2. 1080-2160 días',
                           totalLoanDays > 2160 & totalLoanDays <=3240 ~'3. 2160-3240 días',
                           totalLoanDays > 3240~'4. 3240 +')) %>% 
  mutate(montoRatBin = case_when(montoRat == 1 ~ '1. Sin crecimiento',
                                 montoRat > 1 & montoRat <= 2 ~ '2. Hasta 2x',
                                 montoRat > 2 & montoRat <= 4 ~ '3. 2x-4x',
                                 montoRat > 4~ '4. 4x +',)) %>% 
  ungroup() %>% 
  select(ends_with('Bin'), CI, CTACLIENTE, totalMonto, minMonto, maxMonto, totalNops) 

bdcVIP <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CI, DIASMORA, monDate, montous, FDESEMBOLSO) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  dplyr::filter(monDate <= 'mar. 2020'| monDate >= 'ene. 2021') %>% 
  glimpse()  %>% 
  ungroup() %>%  
  select(OPERACION, CTACLIENTE, CI, DIASMORA, montous, fdes, monDate) %>%
  mutate(eom = as.Date(monDate, frac = 1)) %>%  
  mutate(loanDays = as.integer(eom - fdes)) %>% # check measure
  group_by(CTACLIENTE, CI) %>% 
  mutate(totalLoanDays = sum(loanDays, na.rm = T)) %>% # check measure
  mutate(moraAcum = sum(DIASMORA, na.rm = T)) %>% # check measure
  mutate(moraMax = max(DIASMORA, na.rm = T)) %>% # check measure
  ungroup() %>% 
  group_by(OPERACION) %>% 
  mutate(montous = ifelse(row_number() == 1, montous, NA)) %>% 
  mutate(nops = ifelse(!is.na(montous), 1, 0)) %>% 
  ungroup() %>% 
  group_by(CI) %>% # check grouping
  dplyr::filter(!is.na(montous)) %>%
  dplyr::filter(montous > 0) %>% 
  mutate(totalMonto = sum(montous, na.rm = T)) %>% 
  mutate(maxMonto = max(montous, na.rm = T)) %>% # check measure
  mutate(minMonto = min(montous, na.rm = T)) %>% # check measure
  mutate(montoRat = maxMonto/minMonto) %>% # CHECK MEASURE
  mutate(pct_maxDM_acumDM = ifelse(moraAcum > 0, moraMax/moraAcum, 0)) %>% # check measure
  mutate(pct_acumDM_TLD = ifelse(totalLoanDays > 0, moraAcum/totalLoanDays, 0)) %>% # check measure
  mutate(pct_acumDM_TLD = ifelse(pct_acumDM_TLD > 1, 1, pct_acumDM_TLD)) %>% # check measure
  mutate(totalNops = sum(nops)) %>%# check measure
  select(-montous, -eom, -DIASMORA, -fdes, -monDate, -OPERACION, -nops) %>% 
  summarise_all(max, na.rm = T) %>% # check grouping
  glimpse() %>% 
  #dplyr::filter(totalLoanDays > 360) %>% 
  mutate(moraMaxBin = case_when(moraMax == 0~'1. 0 días',
                                moraMax > 0 & moraMax <= 5~'2. 1-5 días',
                                moraMax > 5 & moraMax <= 10~'3. 6-10 días',
                                moraMax > 10 & moraMax <= 15~'4. 11-15 días',
                                moraMax > 15 & moraMax <= 20~'5. 16-20 días',
                                moraMax > 20 ~'6. 20+ días')) %>% 
  mutate(moraAcumBin = case_when(moraAcum == 0~'1. 0 días',
                                 moraAcum > 0 & moraAcum <= 5~'2. 1-5 días',
                                 moraAcum > 5 & moraAcum <= 10~'3. 6-10 días',
                                 moraAcum > 10 & moraAcum <= 15~'4. 11-15 días',
                                 moraAcum > 15 & moraAcum <= 20~'5. 16-20 días',
                                 moraAcum > 20 ~'6. 20+ días')) %>% 
  mutate(pctMaxBin = case_when(pct_maxDM_acumDM > 0.9~'1. 90% +',
                               pct_maxDM_acumDM > 0.8 & pct_maxDM_acumDM <= 0.9~'2. 80%-90%',
                               pct_maxDM_acumDM > 0.7 & pct_maxDM_acumDM <= 0.8~'3. 70%-80%',
                               pct_maxDM_acumDM <= 0.7 & pct_maxDM_acumDM > 0 ~'4. 1-70%',
                               pct_maxDM_acumDM == 0~'5. 0 (sin morosidad)')) %>% 
  mutate(pctAcumBin = case_when(pct_acumDM_TLD == 0~'1. 0%',
                                pct_acumDM_TLD > 0 & pct_acumDM_TLD <= 0.03~'2. 1%-3%',
                                pct_acumDM_TLD > 0.03 & pct_acumDM_TLD <= 0.05~'3. 3%-5%',
                                pct_acumDM_TLD > 0.05 & pct_acumDM_TLD <= 0.07~'4. 5%-7%',
                                pct_acumDM_TLD > 0.07~'5. 7% +')) %>% 
  mutate(tldBin =case_when(totalLoanDays <= 360 ~ '0. Menos de 360 días',
                           totalLoanDays > 360 & totalLoanDays <=1080 ~'1. 360-1080 días',
                           totalLoanDays > 1080 & totalLoanDays <=2160 ~'2. 1080-2160 días',
                           totalLoanDays > 2160 & totalLoanDays <=3240 ~'3. 2160-3240 días',
                           totalLoanDays > 3240~'4. 3240 +')) %>% 
  mutate(montoRatBin = case_when(montoRat == 1 ~ '1. Sin crecimiento',
                                 montoRat > 1 & montoRat <= 2 ~ '2. Hasta 2x',
                                 montoRat > 2 & montoRat <= 4 ~ '3. 2x-4x',
                                 montoRat > 4~ '4. 4x +',)) %>% 
  ungroup() %>% 
  select(ends_with('Bin'), CI, CTACLIENTE, totalMonto, minMonto, maxMonto, totalNops) 
write_rds(bdcVIP, 'D:/!bso/vipRC/bdcVIP_mar2023.rds')
bdcVIP <- readRDS('D:/!bso/vipRC/bdcVIP_feb2023.rds')
#===============================================================================
# bdc agosto
bdcLast <- fread('D:/!bso/BaseCarteraFeb2023.txt', 
                 encoding = 'Latin-1', fill = T)
bdcAgo <- bdcLast %>% 
  dplyr::filter(MODULO != 131) %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  mutate(activoAgo = 1) %>% 
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(intus = saldous * TASAACT/100) %>% 
  group_by(CI) %>% 
  mutate(totalSaldoAgo = sum(saldous)) %>% 
  mutate(totalMontoAgo = sum(montous)) %>% 
  mutate(totalInt = sum(intus)) %>% 
  mutate(tppa = totalInt/totalSaldoAgo*100) %>% 
  mutate(pctResto = totalSaldoAgo/totalMontoAgo) %>% 
  mutate(pctRestoBin = case_when(pctResto <= 0.2 ~ '1. Hasta 20%',
                                 pctResto > 0.2 & pctResto <= 0.5 ~ '2. 20% - 50%',
                                 pctResto > 0.5 & pctResto <= 0.8 ~ '3. 50% - 80%',
                                 pctResto > 0.8 ~ '4. 20% - 50%',)) %>% 
  mutate(categ = case_when(SECTOR_CARTERA == '6.Vivienda Controlada' ~ 'viviendaTC',
                           SECTOR_CARTERA ==  '1.Prod. Agropec. Controlada' |
                             SECTOR_CARTERA ==  '2.Otra prod. Controlada'|
                             SECTOR_CARTERA ==  '3.C2.Sector Turismo'|
                             SECTOR_CARTERA ==  '4.C3.Prod Intelectual'|
                             SECTOR_CARTERA ==  '5.C4.Fab,Ens.,Vent.MaqAutHib'~ 'productivoTC',
                           SECTOR_CARTERA ==  '7.Prod.Agropec.No Controlada' |
                             SECTOR_CARTERA ==  '8.Otra Prod.No Controlada' | 
                             SECTOR_CARTERA ==  '9.Vivienda No Controlada'~ 'productivoTNC',))  %>%
  mutate(categ = ifelse(is.na(categ), 'Otros', categ)) %>% 
  mutate(tipoCred = case_when(substr(TIPO_CREDITO, 1,1) == 'M'~'Micro',
                              substr(TIPO_CREDITO, 1,1) == 'H'~'Vivienda',
                              substr(TIPO_CREDITO, 1,1) == 'N'~'Consumo',
                              substr(TIPO_CREDITO, 1,1) == 'P'~'PyMe',)) %>% 
  mutate(Sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
  mutate(Sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', Sucursal)) %>% 
  mutate(Sucursal = case_when(Sucursal == '1' ~ 'Chuquisaca',
                              Sucursal == '10' ~ 'El Alto',
                              Sucursal == '2' ~ 'La Paz',
                              Sucursal == '3' ~ 'Cochabamba',
                              Sucursal == '4' ~ 'Oruro',
                              Sucursal == '5' ~ 'Potosí',
                              Sucursal == '6' ~ 'Tarija',
                              Sucursal == '7' ~ 'Santa Cruz',
                              Sucursal == '8' ~ 'Beni',
                              Sucursal == '9' ~ 'Pando',)) %>% 
  mutate(rango = ifelse(montous < 20000, 'menos20k', '20k+')) %>% 
  mutate(CAEDEC_DEST = as.character(CAEDEC_DEST)) %>% 
  mutate(CAEDEC_DEST = ifelse(str_length(CAEDEC_DEST) == 4, 
                              paste0('0', CAEDEC_DEST), CAEDEC_DEST)) %>% 
  mutate(divCaedec = substr(CAEDEC_DEST,1,2)) %>% 
  mutate(grupoCaedec = case_when(divCaedec == '01'~'A',
                                 divCaedec == '02'~'B',
                                 divCaedec == '03'~'B',
                                 divCaedec == '05'~'B',
                                 divCaedec == '11'~'C',
                                 divCaedec == '10'~'D',
                                 divCaedec == '11'~'D',
                                 divCaedec == '12'~'D',
                                 divCaedec == '13'~'D',
                                 divCaedec == '14'~'D',
                                 divCaedec == '15'~'E',
                                 divCaedec == '16'~'E',
                                 divCaedec == '17'~'E',
                                 divCaedec == '18'~'E',
                                 divCaedec == '19'~'E',
                                 divCaedec == '20'~'E',
                                 divCaedec == '21'~'E',
                                 divCaedec == '22'~'E',
                                 divCaedec == '23'~'E',
                                 divCaedec == '24'~'E',
                                 divCaedec == '25'~'E',
                                 divCaedec == '26'~'E',
                                 divCaedec == '27'~'E',
                                 divCaedec == '28'~'E',
                                 divCaedec == '29'~'E',
                                 divCaedec == '30'~'E',
                                 divCaedec == '31'~'E',
                                 divCaedec == '32'~'E',
                                 divCaedec == '33'~'E',
                                 divCaedec == '34'~'E',
                                 divCaedec == '35'~'E',
                                 divCaedec == '36'~'E',
                                 divCaedec == '37'~'E',
                                 divCaedec == '40'~'F',
                                 divCaedec == '41'~'F',
                                 divCaedec == '45'~'G',
                                 divCaedec == '50'~'H',
                                 divCaedec == '51'~'H',
                                 divCaedec == '52'~'H',
                                 divCaedec == '55'~'I',
                                 divCaedec == '60'~'J',
                                 divCaedec == '61'~'J',
                                 divCaedec == '62'~'J',
                                 divCaedec == '63'~'J',
                                 divCaedec == '64'~'J',
                                 divCaedec == '65'~'K',
                                 divCaedec == '66'~'K',
                                 divCaedec == '67'~'K',
                                 divCaedec == '70'~'L',
                                 divCaedec == '71'~'L',
                                 divCaedec == '72'~'L',
                                 divCaedec == '73'~'L',
                                 divCaedec == '74'~'L',
                                 divCaedec == '75'~'M',
                                 divCaedec == '80'~'N',
                                 divCaedec == '85'~'O',
                                 divCaedec == '90'~'O',
                                 divCaedec == '91'~'O',
                                 divCaedec == '92'~'O',
                                 divCaedec == '93'~'O',
                                 divCaedec == '95'~'P',
                                 divCaedec == '98'~'Q',
                                 divCaedec == '99'~'Z',)) %>% 
  mutate(caedec3d = case_when(grupoCaedec == 'A'~'5. Agropecuario',
                              grupoCaedec == 'B'~'5. Agropecuario',
                              grupoCaedec == 'C'~'4. Productivo',
                              grupoCaedec == 'D'~'4. Productivo',
                              grupoCaedec == 'E'~'5. Ind. Manufacturera',
                              grupoCaedec == 'F'~'4. Productivo',
                              grupoCaedec == 'G'~'3. Construcción',
                              grupoCaedec == 'H'~'2.Comercio',
                              grupoCaedec == 'I'~'1.Servicios',
                              grupoCaedec == 'J'~'1.Servicios',
                              grupoCaedec == 'K'~'1.Servicios',
                              grupoCaedec == 'L'~'1.Servicios',
                              grupoCaedec == 'M'~'1.Servicios',
                              grupoCaedec == 'N'~'1.Servicios',
                              grupoCaedec == 'O'~'1.Servicios',
                              grupoCaedec == 'P'~'1.Servicios',
                              grupoCaedec == 'Q'~'1.Servicios',
                              grupoCaedec == 'Z'~'1.Servicios',)) 
# %>% 
#   select(CI, CTACLIENTE, OPERACION, saldous, previus, totalSaldoAgo, montous, totalMontoAgo,
#          pctResto, pctRestoBin, CALIFICACION, ESTADO, FDESEMBOLSO, FFINALIZA, CPOP,
#          ASESOR, AGENCIA, ends_with('_TIT'), categ, tipoCred, Sucursal,
#          rango, intus, caedec3d, totalInt)

bdcAgoCli <- bdcAgo %>% 
  select(CI, CTACLIENTE, totalSaldoAgo, totalMontoAgo, totalInt,
         pctResto, pctRestoBin, CALIFICACION, ends_with('_TIT')) %>% 
  ungroup() %>% 
  group_by(CI, CTACLIENTE) %>% 
  summarise_all(max, na.rm = T)

bdcAgoVip <- bdcVIP %>% 
  left_join(bdcAgoCli, by = c('CTACLIENTE'))
bdcAgoVipNoMiss <- bdcAgoVip %>% 
  dplyr::filter(!is.na(moraMaxBin))

write_rds(bdcAgoVipNoMiss, 'D:/!bso/vipRC/bdcAgoVipNoMiss_mar2023.rds')
write_rds(bdcAgo, 'D:/!bso/vipRC/bdcAgo_mar2023.rds')
bdcAgoVipNoMiss <- readRDS('D:/!bso/vipRC/bdcAgoVipNoMiss_feb2023.rds')
bdcAgo <- readRDS('D:/!bso/vipRC/bdcAgo_feb2023.rds')
#===============================================================================
# infocred
infoRaw <- fread('C:/!bso/Cargamensual_Infocred/BSO202301_utf8.txt', encoding = 'UTF-8')
nrowInfo <- nrow(infoRaw)

infoPerf <- infoRaw %>% 
  mutate(CI = paste0(`NRO DOCUMENTO`, EXT)) %>% 
  mutate(opBSO_ = ifelse(`SIGLA SBEF` == 'BSO', NumeroOp, '-')) %>% 
  mutate(dBSO_ = ifelse(`SIGLA SBEF` == 'BSO' & str_detect(`TIPO OBLIGADO SBEF`, 'A -'), 1, 0)) %>%
  mutate(isBSO_ = ifelse(`SIGLA SBEF` == 'BSO', 1, 0)) %>%
  group_by(CI) %>% 
  mutate(dBSO_total = sum(dBSO_),
         isBSO = max(isBSO_),
         isBSO_total = sum(isBSO_)) %>%
  ungroup() %>% 
  #dplyr::filter(dBSO == 1) %>%
  mutate(idBSO = opBSO_) %>%
  separate_wider_delim(opBSO_, delim = '-',
                       names = c('CTACLIENTE_all', 'OPERACION'),
                       too_few = 'align_start',
                       too_many = 'merge') %>% 
  mutate(CTACLIENTE_d_ = ifelse(`SIGLA SBEF` == 'BSO' & str_detect(`TIPO OBLIGADO SBEF`, 'A -'), 
                                as.integer(CTACLIENTE_all), 0)) %>% 
  group_by(CI) %>% 
  mutate(CTACLIENTE = max(CTACLIENTE_d_, na.rm = T)) %>% 
  ungroup() %>% 
  select(CI, CTACLIENTE, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`, 
         `ENTIDAD SBEF`,  `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
         MontoOriginal, MonedaOrigen) %>% 
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A -')) %>% 
  dplyr::filter(`SIGLA SBEF` != 'BSO') %>% # check filter
  mutate(MontoOriginal = ifelse(MonedaOrigen == 'MN', as.numeric(MontoOriginal)/6.86, as.numeric(MontoOriginal))) %>% 
  mutate(saldoVig = ifelse(MonedaOrigen == 'MN',  as.numeric(`SBEF VIGENTE`)/6.86,   as.numeric(`SBEF VIGENTE`))) %>% 
  mutate(histStr = as.character(HISTORICO)) %>% # HISTORY CONSTRUCTION
  mutate(histStr2 = substr(histStr, 1, 16)) %>% 
  mutate(histStr2 = ifelse(is.na(histStr2), '0', histStr2)) %>% 
  mutate(badCredit = ifelse((str_detect(histStr2, '2') | str_detect(histStr2, '3') |
                             str_detect(histStr2, '4')),1,0)) %>% # CHECK MEASURE
  mutate(worstCredit = ifelse(str_detect(histStr2, '4'),1,0)) %>% # CHECK MEASURE
  ungroup() %>% 
  group_by(CTACLIENTE) %>% # CHECK GROUPING
  # mutate(totalBC = sum(badCredit, na.rm = T),
  #        totalWC = sum(worstCredit, na.rm = T)) %>%  
  mutate(totalBC = str_count(histStr2, "2") + str_count(histStr2, "3") + 
           str_count(histStr2, "4"),
         totalWC = str_count(histStr2, "4")) %>%  
  arrange(CTACLIENTE) %>% 
  mutate(badCreditBin = case_when(totalBC == 0  ~ '1. 0',
                                  totalBC == 1  ~ '2. 1',
                                  totalBC == 2 ~ '3. 2',
                                  totalBC > 2 ~ '4. 2+',
                                  totalWC > 0  & totalWC < NA ~ '5. Tuvo castigo')) %>% 
  select(CI, CTACLIENTE, ends_with('Bin'), totalBC, totalWC, histStr, histStr2,
         badCredit, worstCredit) %>% 
  summarise_all(max)

bdcAgoInfo <- bdcAgoVipNoMiss %>% 
  dplyr::filter(!is.na(CTACLIENTE)) %>% 
  left_join(infoPerf, by = 'CTACLIENTE') %>% 
  select(-CI.x, -CI.y)


bdcAgoInfoCL <- bdcAgoInfo
write_rds(bdcAgoInfoCL, 'D:/!bso/vipRC/bdcAgoInfoCL_feb2023_v2.rds')
bdcAgoInfoCL <- readRDS('D:/!bso/vipRC/bdcAgoInfoCL_feb2023.rds')
#===============================================================================
# daily delinquency data
dailyMM <- readRDS('C:/!bso/vipCartera/dailyMM_feb2023.rds') %>%
  glimpse() %>% 
  ungroup() %>% 
  select(-OPERACION, -maxMoraIM_op) %>% 
  group_by(CTACLIENTE) %>% 
  select(-CI) %>% 
  summarise_all(max)

bdcAgoInfoCL_dailyVal <- bdcAgoInfoCL %>% 
  left_join(dailyMM, by = 'CTACLIENTE') %>% 
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl > 0 & maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl > 5 & maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl > 10 & maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl > 15 & maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días')) %>% 
  glimpse()
#===============================================================================
# Lista excelentes para Negocios
# bus <- read.delim('D:/!bso/vipCartera/Clientes excelentes 202303.csv',
#                   sep = '|') %>% 
bus <- read.xlsx('D:/!bso/vipCartera/Clientes Excelentes Febrero 2023.xlsx',
                  sep = '|') %>% 
  dplyr::rename(CTACLIENTE = Cuenta) %>% 
  select(CTACLIENTE) %>% 
  distinct() %>% 
  mutate(excelenteNegocios = 1)
length(unique(bus$CTACLIENTE))

finalVIPdf <- bdcAgoInfoCL_dailyVal %>% 
  left_join(bus, by = 'CTACLIENTE') %>% 
  mutate(excelenteNegocios = ifelse(is.na(CTACLIENTE), NA, excelenteNegocios))
write.xlsx(finalVIPdf, 'D:/!bso/vipRC/finalVIPdf_histStr2_feb2023_v2.xlsx')
#===============================================================================
# data for score selection
bdcVIPscore <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CI, DIASMORA, monDate, montous, FDESEMBOLSO) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  dplyr::filter(monDate <= 'mar. 2020'| monDate >= 'ene. 2021') %>% 
  glimpse()  %>% 
  ungroup() %>%  
  select(OPERACION, CTACLIENTE, CI, DIASMORA, montous, fdes, monDate) %>%
  mutate(eom = as.Date(monDate, frac = 1)) %>%  # check measure
  mutate(loanDays = as.integer(eom - fdes)) %>% # check measure
  group_by(CTACLIENTE) %>% 
  mutate(totalLoanDays = sum(loanDays, na.rm = T)) %>% 
  mutate(moraAcum = sum(DIASMORA, na.rm = T)) %>% 
  mutate(moraMax = max(DIASMORA, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(OPERACION) %>% 
  mutate(montous = ifelse(row_number() == 1, montous, NA)) %>% 
  mutate(nops = ifelse(!is.na(montous), 1, 0)) %>% 
  ungroup() %>% 
  group_by(CTACLIENTE) %>% # check grouping
  dplyr::filter(!is.na(montous)) %>%
  dplyr::filter(montous > 0) %>% 
  mutate(totalMonto = sum(montous, na.rm = T)) %>% 
  mutate(maxMonto = max(montous, na.rm = T)) %>% 
  mutate(minMonto = min(montous, na.rm = T)) %>% 
  mutate(montoRat = maxMonto/minMonto) %>% # CHECK MEASURE
  mutate(pct_maxDM_acumDM = ifelse(moraAcum > 0, moraMax/moraAcum, 0)) %>% 
  mutate(pct_acumDM_TLD = ifelse(totalLoanDays > 0, moraAcum/totalLoanDays, 0)) %>% 
  mutate(pct_acumDM_TLD = ifelse(pct_acumDM_TLD > 1, 1, pct_acumDM_TLD)) %>% 
  mutate(totalNops = sum(nops)) %>%
  select(-montous, -eom, -DIASMORA, -fdes, -monDate, -OPERACION, -nops) %>% 
  summarise_all(max, na.rm = T) %>%
  #dplyr::filter(totalLoanDays > 360) %>% 
  mutate(moraMaxBin = case_when(moraMax == 0~'1. 0 días',
                                moraMax > 0 & moraMax <= 5~'2. 1-5 días',
                                moraMax > 5 & moraMax <= 10~'3. 6-10 días',
                                moraMax > 10 & moraMax <= 15~'4. 11-15 días',
                                moraMax > 15 & moraMax <= 20~'5. 16-20 días',
                                moraMax > 20 ~'6. 20+ días')) %>% 
  mutate(moraAcumBin = case_when(moraAcum == 0~'1. 0 días',
                                 moraAcum > 0 & moraAcum <= 5~'2. 1-5 días',
                                 moraAcum > 5 & moraAcum <= 10~'3. 6-10 días',
                                 moraAcum > 10 & moraAcum <= 15~'4. 11-15 días',
                                 moraAcum > 15 & moraAcum <= 20~'5. 16-20 días',
                                 moraAcum > 20 ~'6. 20+ días')) %>% 
  mutate(pctMaxBin = case_when(pct_maxDM_acumDM > 0.9~'1. 90% +',
                               pct_maxDM_acumDM > 0.8 & pct_maxDM_acumDM <= 0.9~'2. 80%-90%',
                               pct_maxDM_acumDM > 0.7 & pct_maxDM_acumDM <= 0.8~'3. 70%-80%',
                               pct_maxDM_acumDM <= 0.7 & pct_maxDM_acumDM > 0 ~'4. 1-70%',
                               pct_maxDM_acumDM == 0~'5. 0 (sin morosidad)')) %>% 
  mutate(pctAcumBin = case_when(pct_acumDM_TLD == 0~'1. 0%',
                                pct_acumDM_TLD > 0 & pct_acumDM_TLD <= 0.03~'2. 1%-3%',
                                pct_acumDM_TLD > 0.03 & pct_acumDM_TLD <= 0.05~'3. 3%-5%',
                                pct_acumDM_TLD > 0.05 & pct_acumDM_TLD <= 0.07~'4. 5%-7%',
                                pct_acumDM_TLD > 0.07~'5. 7% +')) %>% 
  mutate(tldBin =case_when(totalLoanDays <= 360 ~ '0. Menos de 360 días',
                           totalLoanDays > 360 & totalLoanDays <=1080 ~'1. 360-1080 días',
                           totalLoanDays > 1080 & totalLoanDays <=2160 ~'2. 1080-2160 días',
                           totalLoanDays > 2160 & totalLoanDays <=3240 ~'3. 2160-3240 días',
                           totalLoanDays > 3240~'4. 3240 +')) %>% 
  mutate(montoRatBin = case_when(montoRat == 1 ~ '1. Sin crecimiento',
                                 montoRat > 1 & montoRat <= 2 ~ '2. Hasta 2x',
                                 montoRat > 2 & montoRat <= 4 ~ '3. 2x-4x',
                                 montoRat > 4~ '4. 4x +',)) %>% 
  ungroup() %>% 
  select(ends_with('Bin'), CI, CTACLIENTE, totalMonto, minMonto, maxMonto, totalNops,
         montoRat, totalLoanDays, pct_acumDM_TLD, pct_maxDM_acumDM, 
         moraAcum, moraMax) %>% 
  left_join(infoPerf, by = 'CTACLIENTE') %>% 
  left_join(dailyMM, by = 'CTACLIENTE') %>% 
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl > 0 & maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl > 5 & maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl > 10 & maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl > 15 & maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días')) %>% 
  left_join(bus, by = 'CTACLIENTE') %>% 
  mutate(excelenteNegocios = ifelse(is.na(CTACLIENTE), NA, excelenteNegocios)) %>% 
  glimpse()

bdcVIPscore <- bdcVIPscore %>% 
  dplyr::filter(!is.na(CTACLIENTE)) %>% 
  select(-CI.x, -CI.y)

write_rds(bdcVIPscore, 'D:/!bso/vipRC/bdcVIPscore_feb2023_v2.rds')
bdcVIPscore <- readRDS('D:/!bso/vipRC/bdcVIPscore_mar2023.rds')

#==============================================================================
# DEFINICIÓN DE CLIENTE VIP
bdcVIPScore_t <- bdcVIPscore %>% 
  mutate(target = ifelse(((pctMaxBin == '5. 0 (sin morosidad)' | pctMaxBin == '1. 90% +') &
                            (pctAcumBin == '1. 0%') &
                            (tldBin == '4. 3240 +' | tldBin == '3. 2160-3240 días' | 
                             tldBin == '2. 1080-2160 días') &
                            (montoRatBin == '4. 4x +' | montoRatBin == '3. 2x-4x') &
                            (is.na(badCreditBin) | badCreditBin == '1. 0' | badCreditBin == '2. 1') &
                            (is.na(maxMoraIMclBin) | maxMoraIMclBin == '1. 0 días')), 1, 0 )) %>% # Este es el ifelse que identifica al cliente VIP
  select(-ends_with('Bin'), -starts_with('histStr'), -ends_with('Credit')) %>% 
  mutate(totalBC = ifelse(is.na(totalBC), 0, totalBC)) %>% 
  mutate(totalWC = ifelse(is.na(totalWC), 0, totalWC)) %>% 
  mutate(maxMoraIM_cl = ifelse(is.na(maxMoraIM_cl), 0, maxMoraIM_cl)) %>% 
  mutate(excelenteNegocios = ifelse(is.na(excelenteNegocios), 0, excelenteNegocios)) %>% 
  glimpse()

table(bdcVIPScore_t$target)
glimpse(bdcVIPScore_t)
write.csv(bdcVIPScore_t, 'D:/!bso/vipRC/dataScore_feb2023_v2.csv', row.names = F)
length(unique(bdcVIPScore_t$CTACLIENTE))
################################################################################
# Here we go to python to estimate scores
################################################################################
#===============================================================================
# Table export
bdcVIPScore <- readRDS('D:/!bso/vipRC/bdcVIPScore_feb2023_v2.rds')
bdcVIPScore_exp <- bdcVIPScore %>% 
  mutate(target = ifelse(((pctMaxBin == '5. 0 (sin morosidad)' | pctMaxBin == '1. 90% +') &
                            (pctAcumBin == '1. 0%') &
                            (tldBin == '4. 3240 +' | tldBin == '3. 2160-3240 días' | 
                               tldBin == '2. 1080-2160 días') &
                            (montoRatBin == '4. 4x +' | montoRatBin == '3. 2x-4x') &
                            (is.na(badCreditBin) | badCreditBin == '1. 0' | badCreditBin == '2. 1') &
                            (is.na(maxMoraIMclBin) | maxMoraIMclBin == '1. 0 días')), 1, 0 )) %>% 
  #select(-ends_with('Bin'), -CTACLIENTE, -starts_with('histStr'), -ends_with('Credit')) %>% 
  mutate(totalBC = ifelse(is.na(totalBC), 0, totalBC)) %>% 
  mutate(totalWC = ifelse(is.na(totalWC), 0, totalWC)) %>% 
  mutate(maxMoraIM_cl = ifelse(is.na(maxMoraIM_cl), 0, maxMoraIM_cl)) %>% 
  mutate(excelenteNegocios = ifelse(is.na(excelenteNegocios), 0, excelenteNegocios)) %>% 
  glimpse()

# Reading probabilities from python
dfScore <- read.csv('D:/!bso/vipRC/vipList_wScores_feb2023_v2.csv') %>% 
  select(CTACLIENTE, prob_GC, goodClient)

bdcAgoExp <- bdcAgo %>%
  ungroup() %>% 
  select(CTACLIENTE, OPERACION, saldous, montous, CALIFICACION, ESTADO, tipoCred,
         CPOP, Sucursal, rango, categ, pctResto, pctRestoBin,
         FFINALIZA, ends_with('_TIT'), caedec3d)

vipListExp <- dfScore %>% 
  left_join(bdcVIPScore_exp, by = 'CTACLIENTE') %>% 
  #left_join(bdcAgoExp, by = 'CI') %>% 
  glimpse()
length(unique(vipListExp$CTACLIENTE))
# This is the dataset that goes into python
write.csv(vipListExp, 'D:/!bso/vipRC/vipListRC_Final_feb2023_v2.csv', row.names = F)
write.xlsx(vipListExp, 'D:/!bso/vipRC/vipListRC_Final_feb2023_v2.xlsx')

#==============================================================================
# final list
vipList <- read.csv('D:/!bso/vipRC/vipListRC_Final_feb2023_v2.csv')
#vipList <- vipListExp
table(vipList$target)
# bdcLast <- fread('D:/!bso/BaseCarteraOct2022.txt', 
#                  encoding = 'Latin-1', fill = T)
# bdcAgo <- bdcLast %>% 
#   dplyr::filter(MODULO != 131) %>% 
#   dplyr::filter(ESTADO != 'CASTIGADA') %>% 
#   mutate(activoAgo = 1) %>% 
#   mutate(fdes = dmy(FDESEMBOLSO)) %>% 
#   mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
#   mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
#   mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
#   mutate(intus = saldous * TASAACT/100) 

# vipAgo <- bdcAgo %>% 
#   left_join(vipList, by = 'CI') %>%
#   ungroup() %>% 
#   group_by(CI) %>% 
#   mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
#   select(target, excelenteNegocios, montous, saldous, ncli) %>% 
#   group_by(target, excelenteNegocios) %>% 
#   summarise(monto = sum(montous), saldo = sum(saldous), nops = n(),
#             ncli = sum(ncli)) %>% 
#   glimpse()
# table(vipAgo$target)
# write.xlsx(vipAgo, 'D:/!bso/vipRC/vipAgo_final_oct2022.xlsx')
# 
# vipAgoFull <- bdcAgo %>% 
#   left_join(vipList, by = 'CI') %>%
#   ungroup() %>% 
#   group_by(CI) %>% 
#   mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
#   ungroup() %>% 
#   group_by(CI) %>% 
#   mutate(pctSaldo = sum(saldous)/sum(montous)) %>% 
#   mutate(vipViable = ifelse(target == 1 & pctSaldo < 0.8, 1, 0)) %>% 
#   mutate(oportunity = ifelse(target == 1, montous - saldous, 0)) %>% 
#   mutate(oportunityViable = ifelse(vipViable == 1, oportunity, 0))
# sum(vipAgoFull$oportunity, na.rm = T)
# sum(vipAgoFull$oportunityViable, na.rm = T)
# 
# vipSingle <- vipAgoFull %>%
#   ungroup() %>% 
#   mutate(exclGroup = ifelse(EXCLUSIVO != '0', 'No 0', '0')) %>% 
#   group_by(CI) %>% 
#   mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
#   ungroup() %>% 
#   mutate(nops = 1) %>% 
#   select(vipViable, excelenteNegocios, saldous, montous, oportunity,
#          oportunityViable, target, exclGroup, ncli) %>% 
#   group_by(vipViable, target, excelenteNegocios, exclGroup) %>% 
#   summarise_all(sum, na.rm = T) %>% 
#   dplyr::filter(!is.na(target)) %>% 
#   glimpse()
# write.xlsx(vipSingle, 'D:/!bso/vipRC/vipSingle_oct2022.xlsx')

#===============================================================================
# Descriptives on bdcAgo
# bdcAgoDes <-fread('D:/!bso/BaseCarteraOct2022.txt', encoding = 'Latin-1', fill = T) %>% 
#   dplyr::filter(ESTADO != 'CASTIGADA') %>% 
#   dplyr::filter(MODULO != 131)
# 
# vipTipoCred <- bdcAgo %>% 
#   left_join(vipList, by = 'CI') %>%
#   ungroup() %>% 
#   group_by(CI) %>% 
#   mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
#   ungroup() %>% 
#   group_by(CI) %>% 
#   mutate(pctSaldo = sum(saldous)/sum(montous)) %>% 
#   mutate(vipViable = ifelse(target == 1 & pctSaldo < 0.8, 1, 0)) %>% 
#   mutate(oportunity = ifelse(target == 1, montous - saldous, 0)) %>% 
#   mutate(oportunityViable = ifelse(vipViable == 1, oportunity, 0)) %>% 
#   ungroup() %>% 
#   dplyr::filter(target == 1 & vipViable == 1) %>% 
#   mutate(rango = ifelse(saldous < 20000, 'menos20k', '20k+')) %>% 
#   mutate(tipoCred = case_when(substr(TIPO_CREDITO, 1,1) == 'M'~'Micro',
#                               substr(TIPO_CREDITO, 1,1) == 'H'~'Vivienda',
#                               substr(TIPO_CREDITO, 1,1) == 'N'~'Consumo',
#                               substr(TIPO_CREDITO, 1,1) == 'P'~'PyMe',)) %>% 
#   mutate(sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
#   mutate(Sucursal = case_when(sucursal == '1' ~ 'Chuquisaca',
#                               sucursal == '10' ~ 'El Alto',
#                               sucursal == '2' ~ 'La Paz',
#                               sucursal == '3' ~ 'Cochabamba',
#                               sucursal == '4' ~ 'Oruro',
#                               sucursal == '5' ~ 'Potosí',
#                               sucursal == '6' ~ 'Tarija',
#                               sucursal == '7' ~ 'Santa Cruz',
#                               sucursal == '8' ~ 'Beni',
#                               sucursal == '9' ~ 'Pando',)) %>% 
#   select(excelenteNegocios, montous, saldous, ncli, tipoCred, intus,
#          oportunityViable, target, vipViable, Sucursal, SECTOR_CARTERA) %>% 
#   group_by(tipoCred, excelenteNegocios, vipViable, Sucursal, SECTOR_CARTERA) %>% 
#   summarise(monto = sum(montous), int = sum(intus), saldo = sum(saldous), nops = n(),
#             ncli = sum(ncli), oport = sum(oportunityViable)) %>% 
#   adorn_totals('row') %>% 
#   mutate(tppa = int/saldo*100) %>%  
#   select( -int) %>% 
#   arrange(excelenteNegocios) %>% 
#   glimpse()
# write.xlsx(vipTipoCred, 'D:/!bso/vipRC/vipDescriptives_oct2022.xlsx')
# 
# vipSucursal <- bdcAgoDes %>% 
#   left_join(vipList, by = 'CI') %>%
#   ungroup() %>% 
#   group_by(CI) %>% 
#   mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
#   dplyr::filter(target == 1) %>% 
#   mutate(Sucursal = case_when(sucursal == '1' ~ 'Chuquisaca',
#                               sucursal == '10' ~ 'El Alto',
#                               sucursal == '2' ~ 'La Paz',
#                               sucursal == '3' ~ 'Cochabamba',
#                               sucursal == '4' ~ 'Oruro',
#                               sucursal == '5' ~ 'Potosí',
#                               sucursal == '6' ~ 'Tarija',
#                               sucursal == '7' ~ 'Santa Cruz',
#                               sucursal == '8' ~ 'Beni',
#                               sucursal == '9' ~ 'Pando',)) %>% 
#   select(excelenteNegocios, montous, saldous, ncli, Sucursal, intus) %>% 
#   group_by(Sucursal, excelenteNegocios) %>% 
#   summarise(monto = sum(montous), int = sum(intus), saldo = sum(saldous), nops = n(),
#             ncli = sum(ncli)) %>% 
#   adorn_totals('row') %>% 
#   mutate(tppa = int/saldo*100) %>%  
#   glimpse() %>% 
#   arrange(excelenteNegocios)
# write.xlsx(vipSucursal, 'D:/!bso/vipRC/vipSucursal.xlsx')
# 
# vipCaedec3d <- bdcAgoDes %>% 
#   left_join(vipList, by = 'CI') %>%
#   ungroup() %>% 
#   group_by(CI) %>% 
#   mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
#   dplyr::filter(target == 1) %>% 
#   select(excelenteNegocios, montous, saldous, ncli, caedec3dC, intus) %>% 
#   group_by( caedec3dC, excelenteNegocios) %>% 
#   summarise(monto = sum(montous), int = sum(intus), saldo = sum(saldous), nops = n(),
#             ncli = sum(ncli)) %>% 
#   adorn_totals('row') %>% 
#   mutate(tppa = int/saldo*100) %>%  
#   arrange(excelenteNegocios) %>% 
#   glimpse()
# write.xlsx(vipCaedec3d, 'D:/!bso/vipRC/vipCaedec3d.xlsx')
# 
# vipUnique <- bdcAgoDes %>% 
#   left_join(vipList, by = 'CI') %>%
#   ungroup() %>% 
#   group_by(CI) %>% 
#   mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
#   dplyr::filter(target == 1) %>% 
#   select(excelenteNegocios, montous, saldous, ncli, EXCLUSIVO, intus) %>% 
#   group_by(EXCLUSIVO, excelenteNegocios) %>% 
#   summarise(monto = sum(montous), int = sum(intus), saldo = sum(saldous), nops = n(),
#             ncli = sum(ncli)) %>% 
#   adorn_totals('row') %>% 
#   mutate(tppa = int/saldo*100) %>%  
#   glimpse()
# write.xlsx(vipUnique, 'D:/!bso/vipRC/vipUnique.xlsx')
# #---------
# # single table
# vipSingle <- bdcAgoDes %>% 
#   left_join(vipList, by = 'CI') %>%
#   ungroup() %>% 
#   group_by(CI) %>% 
#   mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
#   dplyr::filter(target == 1) %>% 
#   ungroup() %>% 
#   group_by(CI) %>% 
#   mutate(pctSaldo = sum(saldous)/sum(montous)) %>% 
#   mutate(vipViable = ifelse(target == 1 & pctSaldo < 0.8, 1, 0)) %>% 
#   mutate(oportunity = ifelse(target == 1, montous - saldous, 0)) %>% 
#   mutate(oportunityViable = ifelse(vipViable == 1, oportunity, 0)) %>% 
#   ungroup() %>% 
#   mutate(exclGroup = ifelse(EXCLUSIVO != '0', 'No 0', '0')) %>% 
#   select(excelenteNegocios, montous, saldous, ncli, exclGroup, intus, vipViable,
#          oportunityViable, oportunity) %>% 
#   group_by(exclGroup, excelenteNegocios, vipViable) %>% 
#   summarise(int = sum(intus), saldo = sum(saldous), nops = n(),
#             ncli = sum(ncli), oportunity = sum(oportunity), 
#             oportunityViable = sum(oportunityViable)) %>% 
#   adorn_totals('row') %>% 
#   mutate(tppa = int/saldo*100) %>%  
#   glimpse()
# write.xlsx(vipSingle, 'D:/!bso/vipRC/vipSingle.xlsx')
# #-----------------------------------------------------------------------------
# Final descriptives
vipDescFull_1 <- bdcAgo %>% 
  left_join(vipList, by = 'CTACLIENTE') %>%
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
  mutate(totalMonto = sum(montous)) %>% 
  mutate(totalSaldo = sum(saldous)) %>% 
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(pctSaldo = sum(saldous)/sum(montous)) %>% 
  mutate(vipViable = ifelse(target == 1 & pctSaldo < 0.8, 1, 0)) %>% 
  mutate(oportunity = ifelse(target == 1, totalMonto - totalSaldo, 0)) %>% 
  mutate(oportunityViable = ifelse(vipViable == 1, oportunity, 0)) %>% 
  ungroup() %>% 
  #dplyr::filter(target == 1 & vipViable == 1) %>% 
  mutate(rango = ifelse(saldous < 20000, 'menos20k', '20k+')) %>% 
  # mutate(tipoCred = case_when(substr(TIPO_CREDITO, 1,1) == 'M'~'Micro',
  #                             substr(TIPO_CREDITO, 1,1) == 'H'~'Vivienda',
  #                             substr(TIPO_CREDITO, 1,1) == 'N'~'Consumo',
  #                             substr(TIPO_CREDITO, 1,1) == 'P'~'PyMe',)) %>% 
  mutate(Sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
  mutate(Sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', Sucursal)) %>% 
  mutate(Sucursal = case_when(Sucursal == '1' ~ 'Chuquisaca',
                              Sucursal == '10' ~ 'El Alto',
                              Sucursal == '2' ~ 'La Paz',
                              Sucursal == '3' ~ 'Cochabamba',
                              Sucursal == '4' ~ 'Oruro',
                              Sucursal == '5' ~ 'Potosí',
                              Sucursal == '6' ~ 'Tarija',
                              Sucursal == '7' ~ 'Santa Cruz',
                              Sucursal == '8' ~ 'Beni',
                              Sucursal == '9' ~ 'Pando',)) %>% 
  mutate(ctileProb = ntile(prob_GC, n = 100)) %>% 
  mutate(dtileProb = ntile(prob_GC, n = 10)) %>%
  mutate(rangos = case_when(saldous < 500 ~'1. <500USD',
                            saldous > 500 & saldous <= 1000 ~'2. 500-1k',
                            saldous > 1000 & saldous <= 5000 ~'3. 1k-5k',
                            saldous > 5000 & saldous <= 10000 ~'4. 5k-10k',
                            saldous > 10000 & saldous <= 15000 ~'5. 10k-15k',
                            saldous > 15000 & saldous <= 20000 ~'6. 15k-20k',
                            saldous > 20000 ~'7. >20k')) %>%
  mutate(rangom = case_when(montous < 500 ~'1. <500USD',
                            montous > 500 & montous <= 1000 ~'2. 500-1k',
                            montous > 1000 & montous <= 5000 ~'3. 1k-5k',
                            montous > 5000 & montous <= 10000 ~'4. 5k-10k',
                            montous > 10000 & montous <= 15000 ~'5. 10k-15k',
                            montous > 15000 & montous <= 20000 ~'6. 15k-20k',
                            montous > 20000 ~'7. >20k')) %>%
  mutate(Lista_GNN = ifelse(excelenteNegocios == 1, 'En Lista', 'Fuera de Lista'),
         VIP_RC = ifelse(target==1, 'VIP-RC', 'Regular'),
         VIP_Viable = ifelse(vipViable == 1, 'Viable', 'No viable'),
         Grupo_Exclusivo = ifelse(EXCLUSIVO == 0, 'Exclusivo', 'No Exclusivo')) %>%
  select(OPERACION, CI, CTACLIENTE, NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT, NOMBRE_ASESOR, AGENCIA,
         Lista_GNN, montous, caedec3d, CPOP, prob_GC, ctileProb, Grupo_Exclusivo,
         saldous, tipoCred, intus, pctSaldo, oportunity,dtileProb, SECTOR_CARTERA,
         oportunityViable, VIP_RC, VIP_Viable, Sucursal, categ, rangom,
         moraMaxBin, moraAcumBin, pctMaxBin, pctAcumBin, minMonto, maxMonto,
         totalLoanDays, pct_acumDM_TLD, pct_maxDM_acumDM, moraAcum, moraMax,
         badCreditBin, totalBC, totalWC, histStr, histStr2) %>% 
  mutate(oportunityViable = montous - saldous) %>% 
  glimpse() %>% 
  dplyr::filter(VIP_RC == 'VIP-RC') %>% # filtro de VIPs
  glimpse() 
 
# VIPs que se mantienen al último cierre
length(unique(vipDescFull_1$CTACLIENTE)) 
# Operaciones que mantienen los VIPs activos al último cierre
nrow(vipDescFull_1)
  
vipDescFull_2 <- vipDescFull_1 %>% 
  dplyr::filter(oportunityViable > 1000) %>% # filtro de préstamo mínimo del BSO
  glimpse() %>% 
  dplyr::rename(Monto_USD = montous,
                Saldo_USD = saldous,
                CAEDEC_DC = caedec3d,
                Percentil_Prob = ctileProb,
                Decil_Prob = dtileProb,
                Tipo_Crédito = tipoCred,
                Interés_USD = intus,
                Saldo_s_Monto = pctSaldo,
                Oportunidad_X_Total = oportunity,
                Oportunidad_Viable = oportunityViable,
                Rango_Monto = rangom,
                Categoría = categ,
                Probabilidad_VIP = prob_GC) %>% # Rename the rest of variables to pretty names
  mutate(Score = Probabilidad_VIP * Oportunidad_Viable,
         Decil_Score = ntile(Score, n = 10),
         Amortización = 1-Saldo_s_Monto,
         Grupo_Amortización = case_when(Amortización <= 0.2 ~ '1. 20%-',
                                        Amortización > 0.2 & Amortización <= 0.4 ~ '2. 20%-40%',
                                        Amortización > 0.4 & Amortización <= 0.6 ~ '3. 40%-60%',
                                        Amortización > 0.6 & Amortización <= 0.8 ~ '4. 60%-80%',
                                        Amortización > 0.8 ~ '5. 80%+',))

# Operaciones viables (>1,000$us)
nrow(vipDescFull_2)

# Operaciones viables (>1,000$us)
nrow(vipDescFull_2[vipDescFull_2$Grupo_Amortización != '1. 20%-', ])

# This is the final list/dataframe
write.xlsx(vipDescFull_2, 'D:/!bso/vipRC/vipDescriptives_CompleteList_Feb2023_allVars_v2.xlsx')
write_rds(vipDescFull_2, 'D:/!bso/vipRC/vipDescriptives_CompleteList_Feb2023_allVars_v2.rds')
#-------------------------------------------------------------------------------
# This is where we make the final quarto report
# First time only: Read excel file and work on quarto report
vipDescFull <- read_xlsx('bla bla bla')

# vipDescFull_Long <- vipDescFull_2 %>%  
#   select(Lista_GNN, Monto_USD, Saldo_USD, Tipo_Crédito, Interés_USD,
#          Oportunidad_Viable, VIP_RC, VIP_Viable, Sucursal) %>% 
#   group_by(Tipo_Crédito, Lista_GNN, VIP_Viable, Sucursal) %>% 
#   summarise(Monto_USD = sum(Monto_USD), Interés_USD = sum(Interés_USD), Saldo_USD = sum(Saldo_USD), nops = n(),
#             Oportunidad_Viable = sum(Oportunidad_Viable)) %>% # Add unique count of CI to get number of unique clients
#   adorn_totals('row') %>% 
#   mutate(TPPA = Interés_USD/Saldo_USD*100) %>%  
#   #select( -int) %>% 
#   #arrange(excelenteNegocios) %>% 
#   glimpse()

vipDescFull_Sucursal <- vipDescFull_2 %>%  
  select(Lista_GNN, Monto_USD, Saldo_USD, Tipo_Crédito, Interés_USD,
         Oportunidad_Viable, VIP_RC, VIP_Viable, Sucursal, NOMBRE_ASESOR) %>% 
  dplyr::filter(VIP_Viable == 'Viable') %>% 
  # group_by(NOMBRE_ASESOR) %>% 
  # mutate(Nops_x_Asesor = n())
  group_by(Lista_GNN, Sucursal) %>% 
  summarise(Monto_USD = sum(Monto_USD), Interés_USD = sum(Interés_USD), Saldo_USD = sum(Saldo_USD), 
            N_Ops_Viables = n(),
            Oportunidad_Viable = sum(Oportunidad_Viable),
            Clientes = 'Unique count of CI',
            Ops_Viables_X_Asesor = mean(Nops_x_Asesor)) %>% # Add unique count of CI to get number of unique clients, Add Ops Viables x Asesor
  adorn_totals('row') %>% 
  mutate(TPPA = Interés_USD/Saldo_USD*100) %>%  
  select(-Interés_USD) %>% 
  glimpse()

# export to tex for quarto report
# Sucursal, Rango, Tipo-Credito, TipoCreditoXRango (filtrar menores a 10K para micro y consumo)
#===============================================================================
# Detailed payment history for viable vips
vipViable <- vipDescFull_2 %>% 
  dplyr::filter(Grupo_Amortización != '1. 20%-') %>% 
  left_join(bdcFull, by = 'OPERACION')
  
