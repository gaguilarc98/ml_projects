
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
paleta <- colorRampPalette(c("slateblue4","purple4","slateblue3","darkorchid3","red3","tan2","yellow2","white"),bias=1.5)
#===============================================================================
# Data in
# NO CORRER!!!
# Proceso de bases mensuales, mismas bases que se usan para la tabla GIR
# y todos los otros reportes de riesgos
# Esto ya no sería necesario dado el procedimiento de actualización líneas abajo
bdcList <- list()
file_list <- list.files(path='D:/!bso/girCartera/rds_v3')
for (i in 1:length(file_list)) {
  
  #print(file_list[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds_v3/',
                        file_list[i])) %>% 
    select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, 
           CALIFICACION, fbase, montous, FDESEMBOLSO, saldous, previus,
           AGENCIA, CIU, CAEDEC_DEST, tipoCred, DESC_SEGMERC,
           ING_VENTAS, PATRIMONIO, PER_OCUPADO,
           PLAZODIAS, EXCLUSIVO, TIPOTASA, TASAFIJA, TASAVARIABLE,
           MONEDA, OBJETO_CRED) %>% 
    mutate(CIU = as.character(CIU),
           CAEDEC_DEST = as.character(CAEDEC_DEST))
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
write_rds(bdcFull, 'D:/!bso/vipRC/bdcFullCollectScore_dic2022.rds')
#===============================================================================
# CORRER ESTO!!!
# Updating last month
remove(list = ls())
gc()
bdcFull_old <- readRDS('D:/!bso/vipRC/bdcFullCollectScore_Feb2023.rds') # actualizar mes

bdc_update <- readRDS('D:/!bso/girCartera/rds_v3/ec_Mar2023.rds') %>% # actualizar mes
  select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, 
         CALIFICACION, fbase, montous, FDESEMBOLSO, saldous, previus,
         AGENCIA, CIU, CAEDEC_DEST, tipoCred, DESC_SEGMERC,
         ING_VENTAS, PATRIMONIO, PER_OCUPADO,
         PLAZODIAS, EXCLUSIVO, TIPOTASA, TASAFIJA, TASAVARIABLE,
         MONEDA, OBJETO_CRED) %>% 
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
  bind_rows(bdc_update)

write_rds(bdcFull_update, 'D:/!bso/vipRC/bdcFullCollectScore_mar2023.rds') # actualizar mes
#===============================================================================
remove(list = ls())
gc()
bdcFull <- readRDS('D:/!bso/vipRC/bdcFullCollectScore_mar2023.rds') # actualizar mes

bdcVIPscore <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CI, DIASMORA, monDate, montous, FDESEMBOLSO, 
         saldous, CALIFICACION, previus,
         AGENCIA, CIU, CAEDEC_DEST, tipoCred, DESC_SEGMERC,
         ING_VENTAS, PATRIMONIO, PER_OCUPADO,
         PLAZODIAS, EXCLUSIVO, TIPOTASA, TASAFIJA, TASAVARIABLE,
         MONEDA, OBJETO_CRED, CALIFICACION) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  mutate(mdes = as.yearmon(fdes)) %>% 
  dplyr::filter(mdes >= 'mar. 2015') %>% 
  dplyr::filter(monDate <= 'mar. 2020'| monDate >= 'ene. 2021') %>% 
  glimpse()  %>% 
  ungroup() %>%  
  select(OPERACION, CTACLIENTE, CI, DIASMORA, montous, monDate,fdes, mdes, 
         saldous, CALIFICACION, previus,
         AGENCIA, CIU, CAEDEC_DEST, tipoCred, DESC_SEGMERC,
         ING_VENTAS, PATRIMONIO, PER_OCUPADO,
         PLAZODIAS, EXCLUSIVO, TIPOTASA, TASAFIJA, TASAVARIABLE,
         MONEDA, OBJETO_CRED, CALIFICACION) %>%
  mutate(eom = as.Date(monDate, frac = 1)) %>%  # check measure
  mutate(loanDays = as.integer(eom - fdes)) %>% # check measure
  group_by(OPERACION) %>% 
  arrange(OPERACION, fdes) %>% 
  mutate(totalLoanDays = cumsum(loanDays)) %>% 
  mutate(moraAcum = cumsum(DIASMORA)) %>% 
  mutate(moraMax = cummax(DIASMORA)) %>%
  mutate(montous = sum(montous, na.rm = T)) %>% 
  # ungroup() %>% 
  # group_by(OPERACION) %>% 
  #mutate(montous = ifelse(row_number() == 1, montous, NA)) %>% 
  #mutate(nops = ifelse(!is.na(montous), 1, 0)) %>% 
  ungroup() %>% 
  # group_by(CI) %>% # check grouping
  # dplyr::filter(!is.na(montous)) %>%
  # dplyr::filter(montous > 0) %>% 
  # mutate(totalMonto = sum(montous, na.rm = T)) %>% 
  # mutate(maxMonto = max(montous, na.rm = T)) %>% 
  # mutate(minMonto = min(montous, na.rm = T)) %>% 
  # mutate(montoRat = maxMonto/minMonto) %>% # CHECK MEASURE
  mutate(pct_maxDM_acumDM = ifelse(moraAcum > 0, round(moraMax/moraAcum, 8), 0)) %>% 
  mutate(pct_acumDM_TLD = ifelse(totalLoanDays > 0,  round(moraAcum/totalLoanDays, 8), 0)) %>% 
  mutate(pct_acumDM_TLD = ifelse(pct_acumDM_TLD > 1, 1, pct_acumDM_TLD)) %>%
  group_by(OPERACION) %>% 
  #mutate(montous = sum(montous, na.rm = T)) %>% 
  mutate(det = ifelse(CALIFICACION > dplyr::lag(CALIFICACION), 1, 0),
         det_3 = ifelse(CALIFICACION > dplyr::lag(CALIFICACION, 3), 1, 0),
         det_6 = ifelse(CALIFICACION > dplyr::lag(CALIFICACION), 6, 0),
         det_12 = ifelse(CALIFICACION > dplyr::lag(CALIFICACION), 12, 0)) %>% 
  mutate(pct_saldo = saldous/montous) %>% 
  mutate(l_moraAcum = dplyr::lag(moraAcum),
         l_totalLoanDays = dplyr::lag(totalLoanDays),
         l_moraMax = dplyr::lag(moraMax),
         l_pct_maxDM_acumDM = dplyr::lag(pct_maxDM_acumDM),
         l_pct_acumDM_TLD = dplyr::lag(pct_acumDM_TLD),
         l_pct_acumDM_TLD = dplyr::lag(pct_acumDM_TLD),
         l_pct_saldo = dplyr::lag(pct_saldo),
         l_saldous = dplyr::lag(saldous),
         l_previus = dplyr::lag(previus),
         l_calif = dplyr::lag(CALIFICACION),
         l_moraAcum_3 = dplyr::lag(moraAcum, 3),
         l_totalLoanDays_3 = dplyr::lag(totalLoanDays, 3),
         l_moraMax_3 = dplyr::lag(moraMax, 3),
         l_pct_maxDM_acumDM_3 = dplyr::lag(pct_maxDM_acumDM, 3),
         l_pct_acumDM_TLD_3 = dplyr::lag(pct_acumDM_TLD, 3),
         l_pct_acumDM_TLD_3 = dplyr::lag(pct_acumDM_TLD, 3),
         l_pct_saldo_3 = dplyr::lag(pct_saldo, 3),
         l_saldous_3 = dplyr::lag(saldous, 3),
         l_previus_3 = dplyr::lag(previus, 3),
         l_calif_3 = dplyr::lag(CALIFICACION, 3),
         l_moraAcum_6 = dplyr::lag(moraAcum, 6),
         l_totalLoanDays_6 = dplyr::lag(totalLoanDays, 6),
         l_moraMax_6 = dplyr::lag(moraMax, 6),
         l_pct_maxDM_acumDM_6 = dplyr::lag(pct_maxDM_acumDM, 6),
         l_pct_acumDM_TLD_6 = dplyr::lag(pct_acumDM_TLD, 6),
         l_pct_acumDM_TLD_6 = dplyr::lag(pct_acumDM_TLD, 6),
         l_pct_saldo_6 = dplyr::lag(pct_saldo, 6),
         l_saldous_6 = dplyr::lag(saldous, 6),
         l_previus_6 = dplyr::lag(previus, 6),
         l_calif_6 = dplyr::lag(CALIFICACION, 6),
         l_moraAcum_12 = dplyr::lag(moraAcum, 6),
         l_totalLoanDays_12 = dplyr::lag(totalLoanDays, 6),
         l_moraMax_12 = dplyr::lag(moraMax, 12),
         l_pct_maxDM_acumDM_12 = dplyr::lag(pct_maxDM_acumDM, 12),
         l_pct_acumDM_TLD_12 = dplyr::lag(pct_acumDM_TLD, 12),
         l_pct_acumDM_TLD_12 = dplyr::lag(pct_acumDM_TLD, 12),
         l_pct_saldo_12 = dplyr::lag(pct_saldo, 12),
         l_saldous_12 = dplyr::lag(saldous, 12),
         l_previus_12 = dplyr::lag(previus, 12),
         l_calif_12 = dplyr::lag(CALIFICACION, 12)) %>% 
  #dplyr::filter(l_calif == 'A') %>% 
  #dplyr::filter(!is.na(det)) %>% 
  select(mdes, monDate, OPERACION, montous, saldous, starts_with('l_'), starts_with('det'),
         previus, starts_with('pct_'), previus, moraAcum, moraMax, totalLoanDays,
         AGENCIA, CIU, CAEDEC_DEST, tipoCred, DESC_SEGMERC,
         ING_VENTAS, PATRIMONIO, PER_OCUPADO,
         PLAZODIAS, EXCLUSIVO, TIPOTASA, TASAFIJA, TASAVARIABLE,
         MONEDA, OBJETO_CRED, CALIFICACION) %>% 
  mutate(tr = case_when(mdes <= 'dic. 2019' & monDate <= 'dic. 2019' ~ 1,
                        mdes > 'dic. 2019' & mdes <= 'ago. 2022' & 
                        monDate > 'dic. 2019' & monDate <= 'ago. 2022'~ 2,
                        mdes > 'ago. 2022' & monDate > 'ago. 2022' ~ 3)) %>% 
  ungroup() 

write_rds(bdcVIPscore, 'D:/!bso/alertaDeterioro/raw_data_score_cobranza_mar2023_Aonly_allDets.rds') # actualizar mes
#=====================================================================================================
 
bdcVIPscore <- readRDS('D:/!bso/alertaDeterioro/raw_data_score_cobranza_ene2023_Aonly_allDets.rds') # actualizar mes
bdcVIPscore2 <- bdcVIPscore %>% 
  #dplyr::filter(l_calif == 'A') %>%
  mutate(tr = case_when(monDate <= 'dic. 2021' ~ 1,
                        monDate == 'ene. 2022'~ 2,
                        monDate == 'feb. 2022'~ 3,
                        monDate == 'mar. 2022'~ 4,
                        monDate == 'abr. 2022'~ 5,
                        monDate == 'may. 2022'~ 6,
                        monDate == 'jun. 2022'~ 7,
                        monDate == 'jul. 2022'~ 8,
                        monDate == 'ago. 2022'~ 9,
                        monDate == 'sep. 2022'~ 10,
                        monDate == 'oct. 2022'~ 11,
                        monDate == 'nov. 2022'~ 12,
                        TRUE ~ 1)) %>% 
  #select(-l_pct_saldo) %>% 
  #na.omit() %>% 
  select(starts_with('l_'), starts_with('det'), tr, 
         AGENCIA, tipoCred, DESC_SEGMERC, OPERACION,
         ING_VENTAS, PATRIMONIO, PER_OCUPADO,
         PLAZODIAS, EXCLUSIVO, TIPOTASA, TASAFIJA, TASAVARIABLE,
         MONEDA, OBJETO_CRED, montous) %>% 
  # dplyr::filter(l_pct_acumDM_TLD >= 0 & l_pct_acumDM_TLD <= 1) %>% 
  # dplyr::filter(l_pct_maxDM_acumDM >= 0 & l_pct_maxDM_acumDM <= 1) %>% 
  # dplyr::filter(l_pct_saldo >= 0 & l_pct_saldo <= 1) %>% 
  #dplyr::filter(montous > 0) %>% 
  #dplyr::filter(l_saldous > 0) %>%
  mutate(PATRIMONIO = ifelse(MONEDA == 0, PATRIMONIO/6.86, PATRIMONIO)) %>% 
  mutate(ING_VENTAS = ifelse(MONEDA == 0, ING_VENTAS/6.86, ING_VENTAS)) %>% 
  mutate(OBJETO_CRED = as.character(OBJETO_CRED),
         AGENCIA = as.character(AGENCIA)) %>%
  #na.omit() %>% 
  glimpse()
  
table(bdcVIPscore2$tr)
table(bdcVIPscore2$det)
write.csv(bdcVIPscore2, 'D:/!bso/alertaDeterioro/data_score_cobranza_feb2023_Aonly.csv', row.names = F) # actualizar mes
write_rds(bdcVIPscore2, 'D:/!bso/alertaDeterioro/data_score_cobranza_feb2023_Aonly.rds') # actualizar mes

bdcCollect <- readRDS('C:/!bso/alertaDeterioro/data_score_cobranza_feb2023_Aonly.rds') # actualizar mes


#=========================================================================================================
# prediction dataset
bdcPred <- bdcVIPscore %>% 
  dplyr::filter(monDate == 'feb. 2023') %>% # Filtramos el mes que deseamos usar para la predicción
  dplyr::filter(mdes < 'feb. 2023') %>% # No monitoreamos deterioros en operaciones recientemente desembolsadas porque no tienen historial 
  dplyr::filter(CALIFICACION == 'A') %>% # check filter
  select(starts_with('pct_'), moraAcum, totalLoanDays, moraMax,
         det, tr, AGENCIA, tipoCred, DESC_SEGMERC, montous,
         ING_VENTAS, PATRIMONIO, PER_OCUPADO, PLAZODIAS, EXCLUSIVO, TIPOTASA, OPERACION,
         TASAFIJA, TASAVARIABLE, MONEDA, OBJETO_CRED, saldous, previus) %>% 
  dplyr::filter(pct_acumDM_TLD >= 0 & pct_acumDM_TLD <= 1) %>% # consistencia de porcentajes 
  dplyr::filter(pct_maxDM_acumDM >= 0 & pct_maxDM_acumDM <= 1) %>%  # consistencia de porcentajes 
  dplyr::filter(pct_saldo >= 0 & pct_saldo <= 1) %>%  # consistencia de porcentajes 
  #dplyr::filter(montous > 0) %>% 
  dplyr::filter(saldous > 0) %>% # consistencia de cifras
  mutate(PATRIMONIO = ifelse(MONEDA == 0, PATRIMONIO/6.86, PATRIMONIO)) %>% 
  mutate(ING_VENTAS = ifelse(MONEDA == 0, ING_VENTAS/6.86, ING_VENTAS)) %>% 
  mutate(OBJETO_CRED = as.character(OBJETO_CRED),
         AGENCIA = as.character(AGENCIA)) %>%
  select(-tr, -det) %>% # estas variables no ingresan a la predicción (una es el target y otra denota si el mes es de training o testing)
  na.omit()  # limpiamos missings
# glimpse()

write.csv(bdcPred, 'D:/!bso/alertaDeterioro/fcast_score_cobranza_feb2023_Aonly.csv', row.names = F)


# Processing forecasts
# Estos resultados se generan desde python! (i.e. en python se genera el csv que se lee abajo)
fcast <- read.csv('D:/!bso/alertaDeterioro/detForecast_Febrero2023_Aonly.csv')

# leemos la base de cartera del cierre para añadir columnas como dias mora, calificacion, etc.
bdc <- readRDS('D:/!bso/girCartera/rds_v3/ec_Feb2023.rds') %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  dplyr::filter(MODULO != 131) %>% 
  select(CTACLIENTE, OPERACION, saldous, previus, AGENCIA, CALIFICACION, DIASMORA) %>% 
  dplyr::rename(CALIFICACION_cierre = CALIFICACION,
                DIASMORA_cierre = DIASMORA,
                saldous_cierre = saldous,
                previus_cierre = previus)

# Unimos bdc con los resultados de python
finalFcast <- fcast %>% 
  left_join(bdc, by = 'OPERACION')

write_rds(finalFcast, 'D:/!bso/alertaDeterioro/finalDetFcast_Febrero2023_Aonly.rds')

write.xlsx(finalFcast, 'D:/!bso/alertaDeterioro/finalDetFcast_Febrero2023_Aonly.xlsx')
#write.csv(finalFcast, 'D:/!bso/alertaDeterioro/finalDetFcast_Diciembre2022_Aonly.csv')

# current check: Leemos la base de un día para evaluar si la predicción va bien o no
bdcHoy <- fread('D:/!bso/data/BaseCartera_20230312.txt', encoding = 'Latin-1')

bdcToday <- bdcHoy %>% 
  dplyr::filter(MODULO != 131) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION, DIASMORA) %>% 
  dplyr::rename(CALIFICACION_hoy = CALIFICACION,
                DIASMORA_hoy = DIASMORA)

# Realizamos la evaluacion
fcastEval <- finalFcast %>% 
  left_join(bdcToday, by = c('OPERACION', 'CTACLIENTE')) %>% 
  mutate(detObs = ifelse(CALIFICACION_hoy > CALIFICACION_cierre, 1, 0), # id de operacion deteriorada hoy, que no estaba deteriorada en el cierre pasado
         peligro_DiasMora = ifelse(DIASMORA_hoy > DIASMORA_cierre & !is.na(DIASMORA_hoy), 1, 0)) %>% # alerta de op que tiene hoy, un amora superior a la del anterior cierre.
  mutate(one = 1) %>% 
  dplyr::filter(!is.na(CALIFICACION_hoy))

write.xlsx(fcastEval, 'D:/!bso/alertaDeterioro/evalDetFcast_Febrero2023_Aonly.xlsx')

# No actulizar después de esta línea
#=========================================================================================================
# Tarea para chapel

# 1) Tomar bdcCollect actualizado a feb-2023.
# 2) Procesar base infocred: Filtras sigla != 'BSO', creas mejorCalif_2, peorCalif_2, 
# Nro de creds en el sistema, suma saldo en SF, max saldo en el SF, min saldo en SF. Todo
# esto en una única fila por CI.
# 3) Join bdcCollect por CI con base procesada de infocred a ene-2023
# 4) Join la base anterior con pagos tardios por operacion (instancias_UR e instancias_AR)
# 5) Grabar esta base como bdcCollect_final_ene2023.csv


#=========================================================================================================
scoreData_1 <- bdcVIPscore2 %>% 
  select(-ends_with('_3'), -ends_with('_6'), -ends_with('_12')) %>% 
  dplyr::filter(l_calif == 'A') %>% 
  dplyr::filter(!is.na(det)) %>%
  dplyr::filter(l_pct_acumDM_TLD >= 0 & l_pct_acumDM_TLD <= 1) %>%
  dplyr::filter(l_pct_maxDM_acumDM >= 0 & l_pct_maxDM_acumDM <= 1) %>%
  dplyr::filter(l_pct_saldo >= 0 & l_pct_saldo <= 1) %>%
  # select(-starts_with('l_pct_saldo', -ends_with('_3'), -ends_with('_6'),
  #                     -ends_with('_12'))) %>% 
  #na.omit() %>% 
  dplyr::filter(montous > 0) %>%
  dplyr::filter(l_saldous > 0) %>% 
  na.omit() %>% 
  glimpse()
write_rds(scoreData_1, 'D:/!bso/alertaDeterioro/data_score_cobranza_nov2022_Aonly_1.rds')
write.csv(scoreData_1, 'D:/!bso/alertaDeterioro/data_score_cobranza_nov2022_Aonly_1.csv', row.names = F)

scoreData_1 <- bdcVIPscore2 %>% 
  select(-ends_with('_3'), -ends_with('_6'), -ends_with('_12')) %>% 
  dplyr::filter(l_calif == 'A') %>% 
  dplyr::filter(!is.na(det)) %>%
  dplyr::filter(l_pct_acumDM_TLD >= 0 & l_pct_acumDM_TLD <= 1) %>%
  dplyr::filter(l_pct_maxDM_acumDM >= 0 & l_pct_maxDM_acumDM <= 1) %>%
  dplyr::filter(l_pct_saldo >= 0 & l_pct_saldo <= 1) %>%
  # select(-starts_with('l_pct_saldo', -ends_with('_3'), -ends_with('_6'),
  #                     -ends_with('_12'))) %>% 
  #na.omit() %>% 
  dplyr::filter(montous > 0) %>%
  dplyr::filter(l_saldous > 0) %>% 
  na.omit() %>% 
  glimpse()

scoreData_3 <- bdcVIPscore2 %>% 
  select(-ends_with('_1'), -ends_with('_6'), -ends_with('_12')) %>% 
  dplyr::filter(l_calif_3 == 'A') %>% 
  dplyr::filter(!is.na(det_3)) %>%
  dplyr::filter(l_pct_acumDM_TLD_3 >= 0 & l_pct_acumDM_TLD_3 <= 1) %>%
  dplyr::filter(l_pct_maxDM_acumDM_3 >= 0 & l_pct_maxDM_acumDM_3 <= 1) %>%
  dplyr::filter(l_pct_saldo_3 >= 0 & l_pct_saldo_3 <= 1) %>%
  # select(-starts_with('l_pct_saldo', -ends_with('_3'), -ends_with('_6'),
  #                     -ends_with('_12'))) %>% 
  #na.omit() %>% 
  dplyr::filter(montous > 0) %>%
  dplyr::filter(l_saldous_3 > 0) %>% 
  na.omit() %>% 
  glimpse()
write_rds(scoreData_3, 'D:/!bso/alertaDeterioro/data_score_cobranza_nov2022_Aonly_3.rds')
write.csv(scoreData_3, 'D:/!bso/alertaDeterioro/data_score_cobranza_nov2022_Aonly_3.csv', row.names = F)

scoreData_6 <- bdcVIPscore2 %>% 
  select(-ends_with('_1'), -ends_with('_3'), -ends_with('_12')) %>% 
  dplyr::filter(l_calif_6 == 'A') %>% 
  dplyr::filter(!is.na(det_6)) %>%
  dplyr::filter(l_pct_acumDM_TLD_6 >= 0 & l_pct_acumDM_TLD_6 <= 1) %>%
  dplyr::filter(l_pct_maxDM_acumDM_6 >= 0 & l_pct_maxDM_acumDM_6 <= 1) %>%
  dplyr::filter(l_pct_saldo_6 >= 0 & l_pct_saldo_6 <= 1) %>%
  # select(-starts_with('l_pct_saldo', -ends_with('_3'), -ends_with('_6'),
  #                     -ends_with('_12'))) %>% 
  #na.omit() %>% 
  dplyr::filter(montous > 0) %>%
  dplyr::filter(l_saldous_6 > 0) %>% 
  na.omit() %>% 
  glimpse()
write_rds(scoreData_6, 'D:/!bso/alertaDeterioro/data_score_cobranza_nov2022_Aonly_6.rds')
write.csv(scoreData_6, 'D:/!bso/alertaDeterioro/data_score_cobranza_nov2022_Aonly_6.csv', row.names = F)


scoreData_12 <- bdcVIPscore2 %>% 
  select(-ends_with('_1'), -ends_with('_3'), -ends_with('_6')) %>% 
  dplyr::filter(l_calif_12 == 'A') %>% 
  dplyr::filter(!is.na(det_12)) %>%
  dplyr::filter(l_pct_acumDM_TLD_12 >= 0 & l_pct_acumDM_TLD_12 <= 1) %>%
  dplyr::filter(l_pct_maxDM_acumDM_12 >= 0 & l_pct_maxDM_acumDM_12 <= 1) %>%
  dplyr::filter(l_pct_saldo_12 >= 0 & l_pct_saldo_12 <= 1) %>%
  # select(-starts_with('l_pct_saldo', -ends_with('_3'), -ends_with('_6'),
  #                     -ends_with('_12'))) %>% 
  #na.omit() %>% 
  dplyr::filter(montous > 0) %>%
  dplyr::filter(l_saldous_12 > 0) %>% 
  na.omit() %>% 
  glimpse()
write_rds(scoreData_12, 'D:/!bso/alertaDeterioro/data_score_cobranza_nov2022_Aonly_12.rds')
write.csv(scoreData_12, 'D:/!bso/alertaDeterioro/data_score_cobranza_nov2022_Aonly_12.csv', row.names = F)


table(scoreData_1$tr)
table(scoreData_1$det)
table(scoreData_3$tr)
table(scoreData_3$det)
table(scoreData_6$tr)
table(scoreData_6$det)
table(scoreData_12$tr)
table(scoreData_12$det)
#===============================================================================
# prediction dataset
bdcPred <- bdcVIPscore %>% 
  dplyr::filter(monDate == 'ene. 2023') %>% 
  dplyr::filter(CALIFICACION == 'A') %>%
  select(starts_with('pct_'), moraAcum, totalLoanDays, moraMax,
         det, tr, AGENCIA, tipoCred, DESC_SEGMERC,
  ING_VENTAS, PATRIMONIO, PER_OCUPADO, PLAZODIAS, EXCLUSIVO, TIPOTASA, OPERACION,
  TASAFIJA, TASAVARIABLE, MONEDA, OBJETO_CRED, saldous, previus) %>% 
  dplyr::filter(pct_acumDM_TLD >= 0 & pct_acumDM_TLD <= 1) %>% 
  dplyr::filter(pct_maxDM_acumDM >= 0 & pct_maxDM_acumDM <= 1) %>% 
  dplyr::filter(pct_saldo >= 0 & pct_saldo <= 1) %>% 
  #dplyr::filter(montous > 0) %>% 
  dplyr::filter(saldous > 0) %>%
  mutate(PATRIMONIO = ifelse(MONEDA == 0, PATRIMONIO/6.86, PATRIMONIO)) %>% 
  mutate(ING_VENTAS = ifelse(MONEDA == 0, ING_VENTAS/6.86, ING_VENTAS)) %>% 
  mutate(OBJETO_CRED = as.character(OBJETO_CRED),
         AGENCIA = as.character(AGENCIA)) %>%
  select(-tr, -det) %>% 
  dplyr::rename(l_pct_acumDM_TLD = pct_acumDM_TLD,
                l_pct_maxDM_acumDM = pct_maxDM_acumDM,
                l_moraAcum = moraAcum,
                l_moraMax = moraMax,
                l_totalLoanDays = totalLoanDays,
                l_saldous = saldous,
                l_previus = previus,
                l_pct_saldo = pct_saldo) %>% 
  #na.omit() %>% 
  glimpse()

write.csv(bdcPred, 'D:/!bso/alertaDeterioro/fcast_score_cobranza_ene2023_Aonly.csv', row.names = F)
#==============================================================================
# describing det
gph <- bdcVIPscore %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  dplyr::filter(monDate > 'dic. 2021') %>%
  ungroup() %>% 
  group_by(OPERACION) %>% 
  dplyr::filter(dplyr::lag(CALIFICACION) == 'A' & det == 1) %>% 
  ungroup() %>% 
  group_by(monDate) %>% 
  tally()

write.xlsx(gph, 'D:/!bso/alertaDeterioro/testing_obs.xlsx')
#==============================================================================
# describing backtest
btest <- read.csv('D:/!bso/alertaDeterioro/btest_Sep2022_Aonly.csv')

bdcAgo <- readRDS('D:/!bso/girCartera/rds/ec_Ago2022.rds') %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  dplyr::filter(MODULO != 131) %>% 
  select(OPERACION, saldous, previus, CALIFICACION) %>% 
  dplyr::rename(califAgo2022 = CALIFICACION,
                saldoAgo2022 = saldous,
                previAgo2022 = previus)
bdcSep <- readRDS('D:/!bso/girCartera/rds/ec_Sep2022.rds') %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  dplyr::filter(MODULO != 131) %>% 
  select(OPERACION, saldous, previus, CALIFICACION) %>% 
  dplyr::rename(califSep2022 = CALIFICACION,
                saldoSep2022 = saldous,
                previSep2022 = previus)

finalBtest <- btest %>% 
  left_join(bdcAgo, by = 'OPERACION') %>% 
  left_join(bdcSep, by = 'OPERACION') %>% 
  mutate(det2 = ifelse(califSep2022 > califAgo2022, 1, 0))
sum(finalBtest$det)
sum(finalBtest$pred_det)
sum(finalBtest$det2, na.rm = T)
sum(finalBtest[finalBtest$det == 1 & finalBtest$pred_det == 1,]$det, na.rm = T)

califSum <- finalBtest %>% 
  select(califAgo2022, pred_det, det, previSep2022, previAgo2022) %>% 
  dplyr::filter(!is.na(det) & !is.na(pred_det) & !is.na(califAgo2022)) %>%
  group_by(califAgo2022, det, pred_det) %>%
  summarise(n = n(), previAgo2022 = sum(previAgo2022, na.rm = T),
            previSep2022 = sum(previSep2022, na.rm = T)) %>% 
  mutate(difPrev = previSep2022 - previAgo2022)
write.xlsx(califSum, 'D:/!bso/alertaDeterioro/backTest_Sep2022_96.xlsx')

#------------------------------------------------------------------------------
# 76% threshold 
btest76 <- read.csv('D:/!bso/alertaDeterioro/btest_Sep2022_76.csv')

finalBtest76 <- btest76 %>% 
  left_join(bdcAgo, by = 'OPERACION') %>% 
  left_join(bdcSep, by = 'OPERACION') %>% 
  mutate(det2 = ifelse(califSep2022 > califAgo2022, 1, 0))
sum(finalBtest76$det)
sum(finalBtest76$pred_det)
sum(finalBtest76$det2, na.rm = T)
sum(finalBtest76[finalBtest76$det == 1 & finalBtest76$pred_det == 1,]$det, na.rm = T)

califSum76 <- finalBtest76 %>% 
  select(califAgo2022, pred_det, det, previSep2022, previAgo2022) %>% 
  dplyr::filter(!is.na(det) & !is.na(pred_det) & !is.na(califAgo2022)) %>%
  group_by(califAgo2022, det, pred_det) %>%
  summarise(n = n(), previAgo2022 = sum(previAgo2022, na.rm = T),
            previSep2022 = sum(previSep2022, na.rm = T)) %>% 
  mutate(difPrev = previSep2022 - previAgo2022)
write.xlsx(califSum76, 'D:/!bso/alertaDeterioro/backTest_Sep2022_76.xlsx')
#==============================================================================
# Processing forecasts
fcast <- read.csv('D:/!bso/alertaDeterioro/detForecast_Septiembre2022_Aonly.csv')
bdc <- readRDS('D:/!bso/girCartera/rds/ec_Sep2022.rds') %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  dplyr::filter(MODULO != 131) %>% 
  select(OPERACION, saldous, previus, AGENCIA, CALIFICACION)
finalFcast <- fcast %>% 
  left_join(bdc, by = 'OPERACION')
write.xlsx(finalFcast, 'D:/!bso/alertaDeterioro/finalDetFcast_Septiembre2022_Aonly.xlsx')
write.csv(finalFcast, 'D:/!bso/alertaDeterioro/finalDetFcast_Septiembre2022_Aonly.csv')

# current check
bdcHoy <- fread('D:/!bso/BaseCartera_20221023.txt', 
                encoding = 'Latin-1')
bdcToday <- bdcHoy %>% 
  dplyr::filter(MODULO != 131) %>% 
  select(OPERACION, CALIFICACION, DIASMORA) %>% 
  dplyr::rename(calif1023 = CALIFICACION,
                dm1023 = DIASMORA)

fcastEval <- finalFcast %>% 
  left_join(bdcToday, by = 'OPERACION') %>% 
  mutate(detObs = ifelse(calif1023 > CALIFICACION, 1, 0)) %>% 
  mutate(one = 1)

correct <- fcastEval %>% 
  dplyr::filter(pred_det==1 & detObs == 1)
#===============================================================================
# Monthly check

lastList <- fread('D:/!bso/alertaDeterioro/predDet_sep2022.csv')
bdcPost <- readRDS('D:/!bso/girCartera/rds/ec_Oct2022.rds') %>% 
  select(OPERACION, CALIFICACION, DIASMORA) %>% 
  dplyr::rename(califPost = CALIFICACION)
bdcCheck <- lastList %>% 
  left_join(bdcPost, by = 'OPERACION') %>% 
  mutate(caught = ifelse(califPost > CALIFICACION, 1, 0)) %>% 
  glimpse()
table(bdcCheck$caught)

#===============================================================================
# Simulating imbalance
bdcVIPscore <- NULL
bdcVIPscore2 <- NULL
bdcVIPscore_tr <- readRDS('D:/!bso/alertaDeterioro/data_score_cobranza_nov2022_Aonly_12.rds') %>% 
  dplyr::filter(tr == 1) %>% 
  group_by(det)
table(bdcVIPscore_tr$det)
det1 <- group_split(bdcVIPscore_tr)[[2]]
det0 <- group_split(bdcVIPscore_tr)[[1]]
imbCurrent <- nrow(det1)/(nrow(det1) + nrow(det0))
imbParam <- nrow(det1)
imbReq <- (imbParam/0.1)-imbParam
imbReq + imbParam
# Don't forget to run set.seed to reproduce samples
set.seed(1234)
dfImb <- slice_sample(det0, n = imbReq) %>% 
  glimpse()
bdcVIPscore_te <- readRDS('D:/!bso/alertaDeterioro/data_score_cobranza_nov2022_Aonly_12.rds') %>% 
  dplyr::filter(tr > 1)
table(bdcVIPscore_te$det)
dfScore <- dfImb %>% 
  bind_rows(det1) %>% 
  bind_rows(bdcVIPscore_te)
write.csv(dfScore, 'D:/!bso/alertaDeterioro/data_score_cobranza_nov2022_Aonly_synthetic_12.csv', row.names = F)
#==============================================================================
# Result post-processing. This comes after python
resdfs_rec <- read.csv('D:/!bso/alertaDeterioro/resDF_collect_dic2022.csv') %>% 
  select(X, ends_with('_50'), auc) %>% 
  arrange(desc(recall_50)) %>% 
  dplyr::rename(Modelo = X) %>% 
  mutate(Total = tp_50 + fp_50 + tn_50 + fn_50) %>% 
  select(Modelo, recall_50, precision_50, accuracy_50, auc, flagRate_50, Total) %>% 
  mutate(Eficiencia = recall_50/flagRate_50)  %>% 
  mutate(across(2:7, round, 2)) %>% 
  dplyr::rename(Sensibilidad = recall_50,
                `Tasa de Positividad` = precision_50,
                `Precision Total` = accuracy_50,
                AUC = auc,
                `Tasa de Alerta` = flagRate_50) %>% 
  mutate(Alertas = round( `Tasa de Alerta`/100 * Total)) %>% 
  select(-Total)

resdfs_eff <- read.csv('D:/!bso/alertaDeterioro/resDF_collect_dic2022.csv') %>% 
  select(X, ends_with('_50'), auc) %>% 
  arrange(desc(recall_50)) %>% 
  dplyr::rename(Modelo = X) %>% 
  mutate(Total = tp_50 + fp_50 + tn_50 + fn_50) %>% 
  select(Modelo, recall_50, precision_50, accuracy_50, auc, flagRate_50, Total) %>% 
  mutate(Eficiencia = round(recall_50/flagRate_50,1)) %>% 
  arrange(desc(Eficiencia)) %>% 
  mutate(across(2:7, round, 2))%>% 
  dplyr::rename(Sensibilidad = recall_50,
                `Tasa de Positividad` = precision_50,
                `Precision Total` = accuracy_50,
                AUC = auc,
                `Tasa de Alerta` = flagRate_50)%>% 
  mutate(Alertas = round( `Tasa de Alerta`/100 * Total))%>% 
  select(-Total) 

write.xlsx(resdfs_rec, 'D:/!bso/alertaDeterioro/resDFs_dic2022.xlsx')
kable(resdfs_rec, format = 'latex', booktabs = TRUE) 

kable(resdfs_eff, format = 'latex', booktabs = TRUE) 

cm <- read.csv('D:/!bso/alertaDeterioro/resDF_collect_dic2022.csv') %>% 
  select(X, ends_with('_50'), auc) %>% 
  arrange(desc(recall_50)) %>% 
  dplyr::rename(Modelo = X) %>% 
  select(Modelo, tn_50, tp_50, fn_50, fp_50, flagRate_50, recall_50) %>% 
  dplyr::filter(Modelo == 'full_easyEnsemble' | Modelo == 'xgb_full' |
                  Modelo == 'full_balancedbaggingTree') %>% 
  mutate(Eficiencia = recall_50/flagRate_50) %>% 
  arrange(desc(Eficiencia)) %>% 
  mutate(across(2:8, round, 2)) %>% 
  mutate(Alertas = round((flagRate_50/100)*(tp_50 + tn_50 + fp_50 + fn_50))) %>% 
  mutate(Deterioros = tp_50 + fn_50) %>% 
  select(Modelo, recall_50, tp_50, Alertas, Deterioros, Eficiencia, flagRate_50) %>% 
  dplyr::rename(Sensibilidad = recall_50,
                Detecciones = tp_50, 
                `Tasa de Alerta` = flagRate_50) 

kable(cm, format = 'latex', booktabs = TRUE) 

fdt <- read.csv('D:/!bso/alertaDeterioro/fdtDFs_dic2022.csv') %>% 
  select(X, tp.1, tp.6, tp.9, rej.1, rej.6, rej.9) %>% 
  dplyr::filter(row_number()>1)

ggplot(fdt, aes(x = as.numeric(rej.9), y = as.numeric(tp.9))) + 
  geom_line(group = 1, size = 1.25, color = 'navy') + 
  theme_minimal() + xlab('% Monitoreo del Portafolio') +
  ylab('Detecciones') + scale_y_continuous(breaks = seq(0,600,50)) +
  scale_x_continuous(breaks = seq(0,100,10)) + 
  geom_hline(yintercept = 579)

ggsave('D:/!bso/alertaDeterioro/tex/roc_xgb_full.png')

gph <- fdt %>% 
  dplyr::filter(row_number()>1) %>% 
  select(X, tp.1, tp.6, tp.8, rej.1, rej.6, rej.8) %>% 
  pivot_longer(!X) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(Modelo = case_when(substr(name, nchar(name)-1, nchar(name)) ==  '.1' ~ 'XGBoost FS',
                           substr(name, nchar(name)-1, nchar(name)) ==  '.6' ~ 'BalancedBaggingTree FS',
                           substr(name, nchar(name)-1, nchar(name)) ==  '.8' ~ 'EasyEnsemble FS',)) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(tp = case_when(!is.na(tp.1)~tp.1,
                        !is.na(tp.6)~tp.6,
                        !is.na(tp.8)~tp.8,)) %>% 
  mutate(rej = case_when(!is.na(rej.1)~rej.1,
                        !is.na(rej.6)~rej.6,
                        !is.na(rej.8)~rej.8,)) %>%
  select(X, Modelo, rej, tp)

ggplot(gph, aes(x = rej, y = tp, color = Modelo)) + 
  geom_line(size = 1.25) + 
  theme_minimal() + xlab('% Monitoreo del Portafolio') +
  ylab('Detecciones') + scale_y_continuous(breaks = seq(0,600,50)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  theme(legend.position = 'bottom') + 
  geom_vline(xintercept = 22, linetype = 'dotted', size = 1.25) +
  geom_vline(xintercept = 52, linetype = 'dashed', size = 1.25) + 
  geom_hline(yintercept = 579)
ggsave('D:/!bso/alertaDeterioro/tex/roc_allModels.png')

fdt <- read.csv('D:/!bso/alertaDeterioro/fdtDFs_dic2022.csv')
gph <- fdt %>% 
  dplyr::filter(row_number()>1)%>% 
  select(X, starts_with('tp.'), starts_with('rej.')) %>% 
  pivot_longer(!X) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(Modelo = case_when(substr(name, nchar(name)-1, nchar(name)) ==  '.1' ~ '1',
                            substr(name, nchar(name)-1, nchar(name)) ==  '.2' ~ '2',
                            substr(name, nchar(name)-1, nchar(name)) ==  '.3' ~ '3',
                            substr(name, nchar(name)-1, nchar(name)) ==  '.4' ~ '4',
                            substr(name, nchar(name)-1, nchar(name)) ==  '.5' ~ '5',
                            substr(name, nchar(name)-1, nchar(name)) ==  '.6' ~ '6',
                            substr(name, nchar(name)-1, nchar(name)) ==  '.7' ~ '7',)) %>% 
                            # substr(name, nchar(name)-1, nchar(name)) ==  '.8' ~ '8',
                            # substr(name, nchar(name)-1, nchar(name)) ==  '.9' ~ '9',)) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(tp = case_when(!is.na(tp.1)~tp.1,
                        !is.na(tp.2)~tp.2,
                        !is.na(tp.3)~tp.3,
                        !is.na(tp.4)~tp.4,
                        !is.na(tp.5)~tp.5,
                        !is.na(tp.6)~tp.6,
                        !is.na(tp.7)~tp.7,
                        # !is.na(tp.8)~tp.8,
                        # !is.na(tp.9)~tp.9,
                       )) %>% 
  mutate(rej = case_when(!is.na(rej.1)~rej.1,
                         !is.na(rej.2)~rej.2,
                         !is.na(rej.3)~rej.3,
                         !is.na(rej.4)~rej.4,
                         !is.na(rej.5)~rej.5,
                         !is.na(rej.6)~rej.6,
                         !is.na(rej.7)~rej.7,
                         # !is.na(rej.8)~rej.8,
                         # !is.na(rej.9)~rej.9,
                        )) %>%
  dplyr::filter(!is.na(tp)) %>% 
  select(X, Modelo, rej, tp)

ggplot(gph, aes(x = rej, y = tp, color = Modelo)) + 
  geom_line(size = 1.25) + 
  theme_minimal() + xlab('% Monitoreo del Portafolio') +
  ylab('Detecciones') + scale_y_continuous(breaks = seq(0,800,50)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  theme(legend.position = 'bottom') + 
  geom_vline(xintercept = 22, linetype = 'dotted', size = 1.25) +
  geom_vline(xintercept = 52, linetype = 'dashed', size = 1.25) + 
  geom_hline(yintercept = 579)+ guides(colour = guide_legend(nrow = 1)) +
  scale_color_manual(values = paleta(8))
ggsave('D:/!bso/alertaDeterioro/roc_allModels_dic2022.png')

dfLim <- gph %>% 
  dplyr::filter(rej>=20 & rej <= 22) %>% 
  mutate(alertas = rej/100*314555,
         recall = tp/579*100)

# valuation
bkTest <- fread('D:/!bso/alertaDeterioro/backTest_preds_2x_dic2022.csv') %>% 
  glimpse()
bkTest_full <- readRDS('D:/!bso/alertaDeterioro/data_score_cobranza_dic2022_Aonly_1.rds') %>% 
  dplyr::filter(tr > 1) %>% 
  left_join(bkTest, by = c('OPERACION', 'tr')) %>% 
  mutate(month = case_when(tr == 2 ~ 'Ene',
         tr == 3  ~ 'Feb',
         tr == 4  ~ 'Mar',
         tr == 5  ~ 'Abr',
         tr == 6  ~ 'May',
         tr == 7  ~ 'Jun',
         tr == 8  ~ 'Jul',
         tr == 9  ~ 'Ago',
         tr == 10 ~ 'Sep',
         tr == 11 ~ 'Oct',
         tr == 12 ~ 'Nov',
         tr == 13 ~ 'Dic',)) %>% 
  glimpse()

shortList <- c('Ene2022', 'Feb2022', 'Mar2022', 'Abr2022', 'May2022', 
               'Jun2022', 'Jul2022', 'Ago2022', 'Sep2022', 'Oct2022',
               'Nov2022', 'Dic2022')
bdcList <- list()
for(i in 1:length(shortList)) {
  print(i)
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds_v3/ec_', shortList[i], '.rds')) %>% 
    dplyr::filter(MODULO != 131) %>% 
    dplyr::filter(ESTADO != 'CASTIGADA') %>% 
    mutate(activoAgo = 1) %>% 
    mutate(fdes = dmy(FDESEMBOLSO)) %>% 
    mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
    mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
    select(OPERACION, CALIFICACION, previus, montous, saldous, DIASMORA) %>% 
    mutate(tr = i+1)
  bdcList[[i]] <- bdc
}

bdcTest <- bind_rows(bdcList) %>% 
  glimpse()

testdf <- bkTest_full %>% 
  mutate(pred_det = ifelse(prob_delinq > 0.8, 'c. 2 ops./Asesor',
                     ifelse(prob_delinq <= 0.8 & prob_delinq > 0.7, 'b. 6 ops./Asesor',
                     ifelse(prob_delinq <= 0.7 & prob_delinq > 0.45, 'a. 20% del total', 'No detectado')))) %>% 
  left_join(bdcTest, by = c('OPERACION', 'tr'))
# table(testdf$det, testdf$pred_det)

checkSet <- testdf %>% 
  dplyr::filter(det == 1)
checkSet_clean <- testdf %>% 
  dplyr::filter(det == 1) %>% 
  dplyr::filter(!is.na(DIASMORA))%>% 
  dplyr::filter(CALIFICACION != 'A') %>% 
  mutate(likelyPrev = 0.825 * saldous.x,
         abPrev = 0.175 * saldous.x)
# sum(checkSet_clean$previus)
# sum(checkSet_clean$saldous)
# likelyPrev <- sum(checkSet_clean$saldous)*0.825

gph <- checkSet_clean %>%
  select(tr, saldous.x, previus,  pred_det, det) %>% 
  group_by(tr, pred_det) %>% 
  summarise_all(sum) %>%
  arrange(tr) %>% 
  mutate(tr = tr) %>% 
  dplyr::rename(Monitoreo = pred_det) %>% 
  dplyr::filter(Monitoreo != 'No detectado') %>% 
  arrange(tr, det) %>% 
  ungroup() %>% 
  group_by(tr) %>% 
  mutate(cumDet = cumsum(det))
  
 ggplot(gph, aes(x = tr, y = det, fill = Monitoreo)) + 
  geom_bar(stat = 'identity', position = 'stack') +
  theme_minimal() + xlab('Mes en 2022') +
  ylab('Operaciones') +
  scale_y_continuous(breaks = seq(50,800,50), 
                     labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
   scale_x_continuous(breaks = seq(1,12))+
   theme(legend.position = 'bottom',
         panel.grid.minor = element_blank(),
         plot.title = element_text(size = 25))+ 
   geom_text(aes(label = round(det)), size = 4.25, 
             position = position_stack(vjust = 0.5))+
   scale_fill_manual(values= paleta(4))
 
 ggsave('D:/!bso/alertaDeterioro/bktest_opsDetectadas_dic2022.png')
 
 
 ggplot(gph, aes(x = tr, y = saldous.x, fill = Monitoreo)) + 
   geom_bar(stat = 'identity', position = 'stack') +
   theme_minimal() + xlab('Mes en 2022') +
   ylab('Saldo (M de USD)') +
   scale_y_continuous(
                      labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
   scale_x_continuous(breaks = seq(1,12))+
   theme(legend.position = 'bottom',
         panel.grid.minor = element_blank(),
         plot.title = element_text(size = 25))+ 
   geom_text(aes(label = round(saldous.x/1000,1)), size = 3, 
             position = position_stack(vjust = 0.5))+
   scale_fill_manual(values= paleta(4))
 
 ggsave('D:/!bso/alertaDeterioro/bktest_saldoDetectado_dic2022.png')
 
 ggplot(gph, aes(x = tr, y = abPrev, fill = Monitoreo)) + 
   geom_bar(stat = 'identity', position = 'stack') +
   theme_minimal() + xlab('Mes en 2022') +
   ylab('Ahorro en previsión USD') +
   scale_y_continuous(
     labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
   scale_x_continuous(breaks = seq(1,10))+
   theme(legend.position = 'bottom',
         panel.grid.minor = element_blank())
 
gph <- testdf %>%
  select(tr, pred_det, det) %>% 
  mutate(correct = ifelse(pred_det != 'No detectado' & det == 1, 1, 0)) %>%
  mutate(wrong = ifelse(pred_det != 'No detectado' & det == 0, 1, 0)) %>%
  mutate(one = 1) %>% 
  group_by(tr, pred_det) %>% 
  summarise_all(sum) %>%
  arrange(tr, pred_det) %>%
  ungroup() %>% 
  group_by(tr) %>% 
  mutate(denom = sum(det)) %>% 
  mutate(recall = det/denom*100,
         flagRate = (wrong + correct)/sum(one)*100,
         tr = tr) %>% 
  dplyr::filter(pred_det != 'No detectado') %>% 
  dplyr::rename(Monitoreo = pred_det)

ggplot(gph, aes(x = tr, y = recall, fill = Monitoreo)) + 
  geom_bar(stat = 'identity', position = 'stack') +
  theme_minimal() + xlab('Mes en 2022') +
  ylab('Sensibilidad (%)') + ylim(0,100)+
  scale_x_continuous(breaks = seq(1,12))+
  scale_y_continuous(breaks = seq(0,100, 10)) +
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 25))+ 
  geom_text(aes(label = round(recall, 1)), size = 4.25, 
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = paleta(4))

ggsave('D:/!bso/alertaDeterioro/bktest_recall_dic2022.png')

ggplot(gph, aes(x = tr, y = flagRate, fill = Monitoreo)) + 
  geom_bar(stat = 'identity', position = 'stack') +
  theme_minimal() + xlab('Mes en 2022') +
  ylab('Monitoreo del portafolio (%)') + ylim(0,25)  + 
  scale_x_continuous(breaks = seq(1,10))+
  scale_y_continuous(breaks = seq(0, 25, 2.5))+
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 25)) + 
  geom_text(aes(label = round(flagRate, 1)), size = 5.25, 
            position = position_stack(vjust = 0.5)) +
  ggtitle('Volumen de Monitoreo (% del portafolio)')
  
ggsave('D:/!bso/alertaDeterioro/tex/bktest_flagrate.png')

#==============================================================================
nameAG <- read.csv('D:/!bso/nombreAgencia.csv', sep = ';')
bdcOct <- readRDS('D:/!bso/girCartera/rds/ec_Oct2022.rds') %>% 
  left_join(nameAG, by = 'AGENCIA') %>% 
  dplyr::filter(ESTADO!='CASTIGADA') %>% 
  dplyr::filter(MODULO != 131)
bdcAct <- bdcOct %>% 
  dplyr::filter(!str_detect(Nombre_Agencia, 'Normal'))
table(bdcAct$Nombre_Agencia)
length(unique(bdcAct$ASESOR))
