
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
 
bdcVIPscore <- readRDS('D:/!bso/alertaDeterioro/raw_data_score_cobranza_mar2023_Aonly_allDets.rds') # actualizar mes
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
  dplyr::filter(monDate == 'mar. 2023') %>% # Filtramos el mes que deseamos usar para la predicción
  dplyr::filter(mdes < 'mar. 2023') %>% # No monitoreamos deterioros en operaciones recientemente desembolsadas porque no tienen historial 
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

write.csv(bdcPred, 'D:/!bso/alertaDeterioro/fcast_score_cobranza_mar2023_Aonly.csv', row.names = F)


# Processing forecasts
# Estos resultados se generan desde python! (i.e. en python se genera el csv que se lee abajo)
fcast <- read.csv('D:/!bso/alertaDeterioro/detForecast_Marzo2023_Aonly.csv')

# leemos la base de cartera del cierre para añadir columnas como dias mora, calificacion, etc.
bdc <- readRDS('D:/!bso/girCartera/rds_v3/ec_Mar2023.rds') %>% 
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

write_rds(finalFcast, 'D:/!bso/alertaDeterioro/finalDetFcast_Marzo2023_Aonly.rds')

write.xlsx(finalFcast, 'D:/!bso/alertaDeterioro/finalDetFcast_Marzo2023_Aonly.xlsx')
#write.csv(finalFcast, 'D:/!bso/alertaDeterioro/finalDetFcast_Diciembre2022_Aonly.csv')

# current check: Leemos la base de un día para evaluar si la predicción va bien o no
bdcHoy <- fread('D:/!bso/data/BaseCartera_20230405.txt', encoding = 'Latin-1')

bdcToday <- bdcHoy %>% 
  dplyr::filter(MODULO != 131) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION, DIASMORA, FVEN_ULTPAGO) %>% 
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

#=========================================================================================================
# Date slotting
bdc <- readRDS('D:/!bso/girCartera/rds_v3/ec_Mar2023.rds') %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  dplyr::filter(MODULO != 131) %>% 
  select(CTACLIENTE, OPERACION, saldous, previus, AGENCIA, CALIFICACION, DIASMORA,
         FVEN_PROXPAGO) %>% 
  dplyr::rename(CALIFICACION_cierre = CALIFICACION,
                DIASMORA_cierre = DIASMORA,
                saldous_cierre = saldous,
                previus_cierre = previus) %>% 
  mutate(fproxpago = as.Date(FVEN_PROXPAGO, format = '%d/%m/%y'))

# Unimos bdc con los resultados de python
finalFcast <- fcast %>% 
  left_join(bdc, by = 'OPERACION') %>% 
  dplyr::filter(year(fproxpago) == 2023 & month(fproxpago) == 4) %>% 
  mutate(dayPago = day(fproxpago),
         dayNotice = dayPago - 2,
         dateNotice = fproxpago - 2) %>% 
  dplyr::filter(fproxpago >=  '2023-04-10') %>% 
  left_join(bdcToday, by = c('OPERACION', 'CTACLIENTE')) %>% 
  # mutate(detObs = ifelse(CALIFICACION_hoy > CALIFICACION_cierre, 1, 0), # id de operacion deteriorada hoy, que no estaba deteriorada en el cierre pasado
  #        peligro_DiasMora = ifelse(DIASMORA_hoy > DIASMORA_cierre & !is.na(DIASMORA_hoy), 1, 0)) %>% # alerta de op que tiene hoy, un amora superior a la del anterior cierre.
  # mutate(one = 1) %>% 
  dplyr::filter(!is.na(CALIFICACION_hoy)) %>% 
  mutate(fultpago = dmy(FVEN_ULTPAGO)) %>% 
  select(-ends_with('hoy'), -FVEN_ULTPAGO, -FVEN_PROXPAGO, -pred_det, -dayPago,
         -dayNotice) %>% 
  dplyr::rename(Probabilidad_Deterioro = prob_det,
                Saldo_USD_Cierre = saldous_cierre,
                Prevision_USD_Cierre = previus_cierre,
                Fecha_Proximo_Pago = fproxpago,
                Fecha_Ultimo_Pago = fultpago,
                Fecha_Notificacion = dateNotice,) %>% 
  relocate(CTACLIENTE, OPERACION) %>% 
  dplyr::filter(month(Fecha_Ultimo_Pago) != 4)
write.xlsx(finalFcast, 'D:/!bso/alertaDeterioro/Lista_Notificacion_Cobranza_Abril2023.xlsx')

