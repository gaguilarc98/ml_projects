
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

# Data in
bdcList <- list()
file_list <- list.files(path='D:/!bso/girCartera/rds')
for (i in 1:length(file_list)) {
  
  #print(file_list[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds/',
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
write_rds(bdcFull, 'D:/!bso/vipRC/bdcFull_sep2022.rds')
#===============================================================================
# Primeros filtros
bdcFull <- readRDS('D:/!bso/vipRC/bdcFull_sep2022.rds')
bdcVIP <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CI, DIASMORA, monDate, montous, FDESEMBOLSO) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  dplyr::filter(monDate <= 'mar. 2020'| monDate >= 'ene. 2021') %>% 
  glimpse()  %>% 
  ungroup() %>%  
  select(OPERACION, CTACLIENTE, CI, DIASMORA, montous, fdes, monDate) %>%
  mutate(eom = as.Date(monDate, frac = 1)) %>%  # check measure
  mutate(loanDays = as.integer(eom - fdes)) %>% # check measure
  group_by(CTACLIENTE, CI) %>% 
  mutate(totalLoanDays = sum(loanDays, na.rm = T)) %>% 
  mutate(moraAcum = sum(DIASMORA, na.rm = T)) %>% 
  mutate(moraMax = max(DIASMORA, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(OPERACION) %>% 
  mutate(montous = ifelse(row_number() == 1, montous, NA)) %>% 
  mutate(nops = ifelse(!is.na(montous), 1, 0)) %>% 
  ungroup() %>% 
  group_by(CI) %>% # check grouping
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
  select(ends_with('Bin'), CI, totalMonto, minMonto, maxMonto, totalNops) 
write_rds(bdcVIP, 'D:/!bso/vipRC/bdcVIP_sep2022.rds')
bdcVIP <- readRDS('D:/!bso/vipRC/bdcVIP_sep2022.rds')
#===============================================================================
# bdc agosto
bdcLast <- fread('D:/!bso/BaseCarteraSep2022.txt', 
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
                              grupoCaedec == 'Z'~'1.Servicios',)) %>% 
  select(CI, CTACLIENTE, OPERACION, saldous, previus, totalSaldoAgo, montous, totalMontoAgo,
         pctResto, pctRestoBin, CALIFICACION, ESTADO, FDESEMBOLSO, FFINALIZA, CPOP,
         ASESOR, AGENCIA, ends_with('_TIT'), categ, tipoCred, Sucursal,
         rango, intus, caedec3d, totalInt)

bdcAgoCli <- bdcAgo %>% 
  select(CI, CTACLIENTE, totalSaldoAgo, totalMontoAgo, totalInt,
         pctResto, pctRestoBin, CALIFICACION, ends_with('_TIT')) %>% 
  ungroup() %>% 
  group_by(CI, CTACLIENTE) %>% 
  summarise_all(max, na.rm = T)

bdcAgoVip <- bdcVIP %>% 
  left_join(bdcAgoCli, by = c('CI'))
bdcAgoVipNoMiss <- bdcAgoVip %>% 
  dplyr::filter(!is.na(moraMaxBin))

write_rds(bdcAgoVipNoMiss, 'D:/!bso/vipRC/bdcAgoVipNoMiss_sep2022.rds')
#===============================================================================
# infocred
infoRaw <- fread('D:/!bso/BSO202208_utf8.txt', encoding = 'UTF-8')
nrowInfo <- nrow(infoRaw)

infoPerf <- infoRaw %>% 
  glimpse() %>% 
  mutate(CI = paste0(`NRO DOCUMENTO`, EXT)) %>% 
  select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`, 
         `ENTIDAD SBEF`,  `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
         MontoOriginal, MonedaOrigen) %>% 
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'DEUDOR')) %>% 
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
  group_by(CI) %>% # CHECK GROUPING
  mutate(totalBC = sum(badCredit, na.rm = T),
         totalWC = sum(worstCredit, na.rm = T)) %>%  
  arrange(CI) %>% 
  mutate(badCreditBin = case_when(totalBC == 0  ~ '1. 0',
                                  totalBC == 1  ~ '2. 1',
                                  totalBC == 2 ~ '3. 2',
                                  totalBC > 2 ~ '4. 2+',
                                  totalWC > 0  & totalWC < NA ~ '5. Tuvo castigo')) %>% 
  select(CI, ends_with('Bin'), totalBC, totalWC, histStr, histStr2, badCredit, worstCredit) %>% 
  summarise_all(max)

bdcAgoInfo <- bdcAgoVipNoMiss %>% 
  left_join(infoPerf, by = 'CI')


bdcAgoInfoCL <- bdcAgoInfo

#===============================================================================
# daily delinquency data
dailyMM <- readRDS('D:/!bso/vipCartera/dailyMM.rds') %>%
  glimpse() %>% 
  ungroup() %>% 
  select(-OPERACION, -maxMoraIM_op) %>% 
  group_by(CI, CTACLIENTE) %>% 
  summarise_all(max)

bdcAgoInfoCL_dailyVal <- bdcAgoInfoCL %>% 
  left_join(dailyMM, by = c('CI', 'CTACLIENTE')) %>% 
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl > 0 & maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl > 5 & maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl > 10 & maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl > 15 & maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días')) %>% 
  glimpse()
#===============================================================================
bus <- read.csv('D:/!bso/vipCartera/Clientes excelentes septiembre 2022.csv', sep = '|') %>% 
  dplyr::rename(CTACLIENTE = Cuenta) %>% 
  select(CTACLIENTE) %>% 
  distinct() %>% 
  mutate(excelenteNegocios = 1)
length(unique(bus$CTACLIENTE))

finalVIPdf <- bdcAgoInfoCL_dailyVal %>% 
  left_join(bus, by = 'CTACLIENTE') %>% 
  mutate(excelenteNegocios = ifelse(is.na(CTACLIENTE), NA, excelenteNegocios))
write.xlsx(finalVIPdf, 'D:/!bso/vipRC/finalVIPdf_histStr2_sep2022.xlsx')
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
  group_by(CTACLIENTE, CI) %>% 
  mutate(totalLoanDays = sum(loanDays, na.rm = T)) %>% 
  mutate(moraAcum = sum(DIASMORA, na.rm = T)) %>% 
  mutate(moraMax = max(DIASMORA, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(OPERACION) %>% 
  mutate(montous = ifelse(row_number() == 1, montous, NA)) %>% 
  mutate(nops = ifelse(!is.na(montous), 1, 0)) %>% 
  ungroup() %>% 
  group_by(CI) %>% # check grouping
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
  left_join(infoPerf, by = 'CI') %>% 
  left_join(dailyMM, by = c('CI', 'CTACLIENTE')) %>% 
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl > 0 & maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl > 5 & maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl > 10 & maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl > 15 & maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días')) %>% 
  left_join(bus, by = 'CTACLIENTE') %>% 
  mutate(excelenteNegocios = ifelse(is.na(CTACLIENTE), NA, excelenteNegocios)) %>% 
  glimpse()

write_rds(bdcVIPscore, 'D:/!bso/vipRC/bdcVIPscore_sep2022.rds')


bdcVIPScore_t <- bdcVIPscore %>% 
  mutate(target = ifelse(((pctMaxBin == '5. 0 (sin morosidad)' | pctMaxBin == '1. 90% +') &
                            (pctAcumBin == '1. 0%') &
                            (tldBin == '4. 3240 +' | tldBin == '3. 2160-3240 días' | 
                             tldBin == '2. 1080-2160 días') &
                            (montoRatBin == '4. 4x +' | montoRatBin == '3. 2x-4x') &
                            (is.na(badCreditBin) | badCreditBin == '1. 0' | badCreditBin == '2. 1') &
                            (is.na(maxMoraIMclBin) | maxMoraIMclBin == '1. 0 días')), 1, 0 )) %>% 
  select(-ends_with('Bin'), -CTACLIENTE, -starts_with('histStr'), -ends_with('Credit')) %>% 
  mutate(totalBC = ifelse(is.na(totalBC), 0, totalBC)) %>% 
  mutate(totalWC = ifelse(is.na(totalWC), 0, totalWC)) %>% 
  mutate(maxMoraIM_cl = ifelse(is.na(maxMoraIM_cl), 0, maxMoraIM_cl)) %>% 
  mutate(excelenteNegocios = ifelse(is.na(excelenteNegocios), 0, excelenteNegocios)) %>% 
  glimpse()

table(bdcVIPScore_t$target)
glimpse(bdcVIPScore_t)
write.csv(bdcVIPScore_t, 'D:/!bso/vipRC/dataScore_sep2022.csv', row.names = F)

#===============================================================================
# Table export
bdcVIPScore <- readRDS('D:/!bso/vipRC/bdcVIPScore_sep2022.rds')
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

dfScore <- read.csv('D:/!bso/vipRC/vipList_wScores_sep2022.csv') %>% 
  select(CI, prob_GC, goodClient)

bdcAgoExp <- bdcAgo %>% 
  select(CI, OPERACION, saldous, montous, CALIFICACION, ESTADO, tipoCred,
         CPOP, Sucursal, rango, categ, pctResto, pctRestoBin,
         FFINALIZA, ends_with('_TIT'), caedec3d)

vipListExp <- dfScore %>% 
  left_join(bdcVIPScore_exp, by = 'CI') %>% 
  #left_join(bdcAgoExp, by = 'CI') %>% 
  glimpse()
write.csv(vipListExp, 'D:/!bso/vipRC/vipListRC_Final_sep2022.csv', row.names = F)
write.xlsx(vipListExp, 'D:/!bso/vipRC/vipListRC_Final_sep2022.xlsx')

#==============================================================================
# final list
vipList <- read.csv('D:/!bso/vipRC/vipListRC_Final_sep2022.csv')
table(vipList$target)
bdcLast <- fread('D:/!bso/BaseCarteraSep2022.txt', 
                 encoding = 'Latin-1', fill = T)
bdcAgo <- bdcLast %>% 
  dplyr::filter(MODULO != 131) %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  mutate(activoAgo = 1) %>% 
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(intus = saldous * TASAACT/100) 

vipAgo <- bdcAgo %>% 
  left_join(vipList, by = 'CI') %>%
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
  select(target, excelenteNegocios, montous, saldous, ncli) %>% 
  group_by(target, excelenteNegocios) %>% 
  summarise(monto = sum(montous), saldo = sum(saldous), nops = n(),
            ncli = sum(ncli)) %>% 
  glimpse()
table(vipAgo$target)
write.xlsx(vipAgo, 'D:/!bso/vipRC/vipAgo_final_sep2022.xlsx')

vipAgoFull <- bdcAgo %>% 
  left_join(vipList, by = 'CI') %>%
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(pctSaldo = sum(saldous)/sum(montous)) %>% 
  mutate(vipViable = ifelse(target == 1 & pctSaldo < 0.8, 1, 0)) %>% 
  mutate(oportunity = ifelse(target == 1, montous - saldous, 0)) %>% 
  mutate(oportunityViable = ifelse(vipViable == 1, oportunity, 0))
sum(vipAgoFull$oportunity, na.rm = T)
sum(vipAgoFull$oportunityViable, na.rm = T)

vipSingle <- vipAgoFull %>%
  ungroup() %>% 
  mutate(exclGroup = ifelse(EXCLUSIVO != '0', 'No 0', '0')) %>% 
  group_by(CI) %>% 
  mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
  ungroup() %>% 
  mutate(nops = 1) %>% 
  select(vipViable, excelenteNegocios, saldous, montous, oportunity,
         oportunityViable, target, exclGroup, ncli) %>% 
  group_by(vipViable, target, excelenteNegocios, exclGroup) %>% 
  summarise_all(sum, na.rm = T) %>% 
  dplyr::filter(!is.na(target)) %>% 
  glimpse()
write.xlsx(vipSingle, 'D:/!bso/vipRC/vipSingle_sep2022.xlsx')

#===============================================================================
# Descriptives on bdcAgo
bdcAgoDes <- readRDS('D:/!bso/girCartera/rds/ec_Sep2022.rds') %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  dplyr::filter(MODULO != 131)

vipTipoCred <- bdcAgo %>% 
  left_join(vipList, by = 'CI') %>%
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(pctSaldo = sum(saldous)/sum(montous)) %>% 
  mutate(vipViable = ifelse(target == 1 & pctSaldo < 0.8, 1, 0)) %>% 
  mutate(oportunity = ifelse(target == 1, montous - saldous, 0)) %>% 
  mutate(oportunityViable = ifelse(vipViable == 1, oportunity, 0)) %>% 
  ungroup() %>% 
  dplyr::filter(target == 1 & vipViable == 1) %>% 
  mutate(rango = ifelse(saldous < 20000, 'menos20k', '20k+')) %>% 
  mutate(tipoCred = case_when(substr(TIPO_CREDITO, 1,1) == 'M'~'Micro',
                              substr(TIPO_CREDITO, 1,1) == 'H'~'Vivienda',
                              substr(TIPO_CREDITO, 1,1) == 'N'~'Consumo',
                              substr(TIPO_CREDITO, 1,1) == 'P'~'PyMe',)) %>% 
  mutate(sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
  mutate(Sucursal = case_when(sucursal == '1' ~ 'Chuquisaca',
                              sucursal == '10' ~ 'El Alto',
                              sucursal == '2' ~ 'La Paz',
                              sucursal == '3' ~ 'Cochabamba',
                              sucursal == '4' ~ 'Oruro',
                              sucursal == '5' ~ 'Potosí',
                              sucursal == '6' ~ 'Tarija',
                              sucursal == '7' ~ 'Santa Cruz',
                              sucursal == '8' ~ 'Beni',
                              sucursal == '9' ~ 'Pando',)) %>% 
  select(excelenteNegocios, montous, saldous, ncli, tipoCred, intus,
         oportunityViable, target, vipViable, Sucursal, SECTOR_CARTERA) %>% 
  group_by(tipoCred, excelenteNegocios, vipViable, Sucursal, SECTOR_CARTERA) %>% 
  summarise(monto = sum(montous), int = sum(intus), saldo = sum(saldous), nops = n(),
            ncli = sum(ncli), oport = sum(oportunityViable)) %>% 
  adorn_totals('row') %>% 
  mutate(tppa = int/saldo*100) %>%  
  select( -int) %>% 
  arrange(excelenteNegocios) %>% 
  glimpse()
write.xlsx(vipTipoCred, 'D:/!bso/vipRC/vipDescriptives_sep2022.xlsx')

vipSucursal <- bdcAgoDes %>% 
  left_join(vipList, by = 'CI') %>%
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
  dplyr::filter(target == 1) %>% 
  mutate(Sucursal = case_when(sucursal == '1' ~ 'Chuquisaca',
                              sucursal == '10' ~ 'El Alto',
                              sucursal == '2' ~ 'La Paz',
                              sucursal == '3' ~ 'Cochabamba',
                              sucursal == '4' ~ 'Oruro',
                              sucursal == '5' ~ 'Potosí',
                              sucursal == '6' ~ 'Tarija',
                              sucursal == '7' ~ 'Santa Cruz',
                              sucursal == '8' ~ 'Beni',
                              sucursal == '9' ~ 'Pando',)) %>% 
  select(excelenteNegocios, montous, saldous, ncli, Sucursal, intus) %>% 
  group_by(Sucursal, excelenteNegocios) %>% 
  summarise(monto = sum(montous), int = sum(intus), saldo = sum(saldous), nops = n(),
            ncli = sum(ncli)) %>% 
  adorn_totals('row') %>% 
  mutate(tppa = int/saldo*100) %>%  
  glimpse() %>% 
  arrange(excelenteNegocios)
write.xlsx(vipSucursal, 'D:/!bso/vipRC/vipSucursal.xlsx')

vipCaedec3d <- bdcAgoDes %>% 
  left_join(vipList, by = 'CI') %>%
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
  dplyr::filter(target == 1) %>% 
  select(excelenteNegocios, montous, saldous, ncli, caedec3dC, intus) %>% 
  group_by( caedec3dC, excelenteNegocios) %>% 
  summarise(monto = sum(montous), int = sum(intus), saldo = sum(saldous), nops = n(),
            ncli = sum(ncli)) %>% 
  adorn_totals('row') %>% 
  mutate(tppa = int/saldo*100) %>%  
  arrange(excelenteNegocios) %>% 
  glimpse()
write.xlsx(vipCaedec3d, 'D:/!bso/vipRC/vipCaedec3d.xlsx')

vipUnique <- bdcAgoDes %>% 
  left_join(vipList, by = 'CI') %>%
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
  dplyr::filter(target == 1) %>% 
  select(excelenteNegocios, montous, saldous, ncli, EXCLUSIVO, intus) %>% 
  group_by(EXCLUSIVO, excelenteNegocios) %>% 
  summarise(monto = sum(montous), int = sum(intus), saldo = sum(saldous), nops = n(),
            ncli = sum(ncli)) %>% 
  adorn_totals('row') %>% 
  mutate(tppa = int/saldo*100) %>%  
  glimpse()
write.xlsx(vipUnique, 'D:/!bso/vipRC/vipUnique.xlsx')
#---------
# single table
vipSingle <- bdcAgoDes %>% 
  left_join(vipList, by = 'CI') %>%
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
  dplyr::filter(target == 1) %>% 
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(pctSaldo = sum(saldous)/sum(montous)) %>% 
  mutate(vipViable = ifelse(target == 1 & pctSaldo < 0.8, 1, 0)) %>% 
  mutate(oportunity = ifelse(target == 1, montous - saldous, 0)) %>% 
  mutate(oportunityViable = ifelse(vipViable == 1, oportunity, 0)) %>% 
  ungroup() %>% 
  mutate(exclGroup = ifelse(EXCLUSIVO != '0', 'No 0', '0')) %>% 
  select(excelenteNegocios, montous, saldous, ncli, exclGroup, intus, vipViable,
         oportunityViable, oportunity) %>% 
  group_by(exclGroup, excelenteNegocios, vipViable) %>% 
  summarise(int = sum(intus), saldo = sum(saldous), nops = n(),
            ncli = sum(ncli), oportunity = sum(oportunity), 
            oportunityViable = sum(oportunityViable)) %>% 
  adorn_totals('row') %>% 
  mutate(tppa = int/saldo*100) %>%  
  glimpse()
write.xlsx(vipSingle, 'D:/!bso/vipRC/vipSingle.xlsx')
