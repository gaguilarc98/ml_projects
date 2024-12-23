
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
library(bdscale)
remove(list = ls())
options("encoding" = "UTF-8")
#Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 9)
cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
paleta <- colorRampPalette(c("slateblue4","purple4","slateblue3","darkorchid3","red3","tan2","yellow2","white"),bias=1.5)
nameAG <- fread('C:/!bso/codigos_agencia_v2.csv') %>% 
  mutate(AGENCIA = as.character(AGENCIA))
#========================
# Testing one month
infocred_dic <- fread(paste0('C:/!bso/Cargamensual_infocred/BSO202212_utf8.txt'), encoding = 'UTF-8', fill = T)

infoList <- list()
file_list <- list.files(path='C:/!bso/Cargamensual_infocred/rds/')
for (i in 1:length(file_list)) {
  print(file_list[[i]])
  infoRaw <- readRDS(paste0('C:/!bso/Cargamensual_infocred/rds/', file_list[[i]])) %>% 
    mutate(CI = paste0(`NRO DOCUMENTO`, EXT)) %>% 
    mutate(opBSO_ = ifelse(`SIGLA SBEF` == 'BSO', NumeroOp, '-')) %>% 
    mutate(dBSO_ = ifelse(`SIGLA SBEF` == 'BSO' & str_detect(`TIPO OBLIGADO SBEF`, 'A -'), 1, 0)) %>%
    mutate(isBSO_ = ifelse(`SIGLA SBEF` == 'BSO', 1, 0)) %>%
    group_by(CI) %>% 
    mutate(dBSO_total = sum(dBSO_),
           isBSO = max(isBSO_),
           isBSO_total = sum(isBSO_)) %>%
    ungroup() %>% 
    mutate(idBSO = opBSO_) %>%
    # separate_wider_delim(opBSO_, delim = '-',
    #                      names = c('CTACLIENTE_all', 'OPERACION'),
    #                      too_few = 'align_start',
    #                      too_many = 'merge') %>% 
    mutate(CTACLIENTE_d_ = ifelse(`SIGLA SBEF` == 'BSO' & str_detect(`TIPO OBLIGADO SBEF`, 'A -'), 
                                  as.integer(CTACLIENTE_all), 0)) %>% 
    group_by(CI) %>% 
    mutate(CTACLIENTE = max(CTACLIENTE_d_, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(CTACLIENTE = as.integer(CTACLIENTE),
           OPERACION = as.integer(OPERACION)) %>% 
    mutate(ESTADO = case_when(`SBEF VIGENTE` > 0 ~ 'VIGENTE',
                              `SBEF VENCIDO` > 0 ~ 'VENCIDO',
                              `SBEF EJECUCION` > 0 ~ 'EJECUCION',
                              `SBEF CASTIGADO` > 0 ~ 'CASTIGADO',
                              TRUE ~ 'NA'),
           SALDO = `SBEF VIGENTE` + `SBEF VENCIDO` + `SBEF EJECUCION`) %>% 
    select(CTACLIENTE, OPERACION, CI, 
           HISTORICO, 
           `FECHA NAC`, LOCALIDAD, CANTON, SECCION, PROVINCIA, DEPARTAMENTO,
           `FECHA DECLARACION`, `ENTIDAD SBEF`,`SIGLA SBEF`, 
           FECHA, `TIPO OBLIGADO SBEF`, `TIPO CREDITO SBEF`, `FECHA INICIO OPERACION`,
           MonedaOrigen, MontoOriginal, `SBEF CALIFICACION`, SALDO, `SBEF CONTINGENTE`,
           DiasMora, FECHAVTO, `PERIODO PAGO`, idBSO) %>%
    mutate(HISTORICO = round(as.double(HISTORICO))) %>% 
    mutate(HISTORICO = as.character(HISTORICO)) %>% 
    mutate(fechaReporte = substr(file_list[[i]], 4,9))
  
 infoList[[i]] <- infoRaw
  
}

infoFull <- bind_rows(infoList)
write_rds(infoFull, 'C:/!bso/Cargamensual_Infocred/infoFull_feb23.rds')
glimpse(infoFull)
length(unique(infoFull$CTACLIENTE))
table(infoFull$fechaReporte)
#===============================================================================
# All disbursements
bdcList22 <- list()
file_list <- list.files(path='D:/!bso/girCartera/rds_v3', pattern = '2022')
for (i in 1:length(file_list)) {
  print(i)
  print(file_list[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds_v3/',
                        file_list[i])) %>%
    dplyr::filter(MODULO != 131) %>% 
    dplyr::filter(ESTADO != 'CASTIGADA') %>% 
    dplyr::filter(year(fdes) == 2022 & month(fdes) == month(as.Date(monDate, frac = 1))) %>% 
    select(CTACLIENTE, OPERACION, CI, montous, fdes, MODULO, monDate) %>% 
    dplyr::rename(CIbso = CI) %>% 
    ungroup() %>% 
    group_by(CIbso) %>% 
    mutate(ndisb = max(row_number()))
  
  bdcList22[[i]] <- bdc
    
}

bdcList20 <- list()
file_list <- list.files(path='D:/!bso/girCartera/rds_v3', pattern = '2020')
for (i in 1:length(file_list)) {
  print(i)
  print(file_list[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds_v3/',
                        file_list[i])) %>%
    dplyr::filter(MODULO != 131) %>% 
    dplyr::filter(ESTADO != 'CASTIGADA') %>% 
    dplyr::filter(year(fdes) == 2020 & month(fdes) == month(as.Date(monDate, frac = 1))) %>% 
    select(CTACLIENTE, OPERACION, CI, montous, fdes, MODULO, monDate) %>% 
    dplyr::rename(CIbso = CI) %>% 
    ungroup() %>% 
    group_by(CIbso) %>% 
    mutate(ndisb = max(row_number()))
  
  bdcList20[[i]] <- bdc
  
}

bdcFull20 <- bind_rows(bdcList20)
length(unique(bdcFull20$CIbso))

bdcFull22 <- bind_rows(bdcList22)
length(unique(bdcFull22$CIbso))

bdcList21 <- list()
file_list <- list.files(path='D:/!bso/girCartera/rds_v3', pattern = '2021')
for (i in 1:length(file_list)) {
  print(i)
  print(file_list[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds_v3/',
                        file_list[i])) %>%
    dplyr::filter(MODULO != 131) %>% 
    dplyr::filter(ESTADO != 'CASTIGADA') %>% 
    dplyr::filter(year(fdes) == 2021 & month(fdes) == month(as.Date(monDate, frac = 1))) %>% 
    select(CTACLIENTE, OPERACION, CI, montous, fdes, MODULO, monDate) %>% 
    dplyr::rename(CIbso = CI) %>% 
    ungroup() %>% 
    group_by(CIbso) %>% 
    mutate(ndisb = max(row_number()))
  
  bdcList21[[i]] <- bdc
  
}

bdcFull21 <- bind_rows(bdcList21)
length(unique(bdcFull21$CIbso))

bdcList23 <- list()
file_list <- list.files(path='D:/!bso/girCartera/rds_v3', pattern = '2023')
for (i in 1:length(file_list)) {
  print(i)
  print(file_list[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds_v3/',
                        file_list[i])) %>%
    dplyr::filter(MODULO != 131) %>% 
    dplyr::filter(ESTADO != 'CASTIGADA') %>% 
    dplyr::filter(year(fdes) == 2023 & month(fdes) == month(as.Date(monDate, frac = 1))) %>% 
    select(CTACLIENTE, OPERACION, CI, montous, fdes, MODULO, monDate) %>% 
    dplyr::rename(CIbso = CI) %>% 
    ungroup() %>% 
    group_by(CIbso) %>% 
    mutate(ndisb = max(row_number()))
  
  bdcList23[[i]] <- bdc
  
}

bdcFull23 <- bind_rows(bdcList23)
length(unique(bdcFull23$CIbso))

bdcFull <- bdcFull21 %>% 
  bind_rows(bdcFull22) %>%
  bind_rows(bdcFull23) %>% 
  bind_rows(bdcFull20)
length(unique(bdcFull$CIbso))
       
#===============================================================================
# Join
disbJoin <- bdcFull %>% 
  dplyr::rename(opBSO = OPERACION) %>% 
  left_join(infoFull, by = 'CTACLIENTE') %>% 
  mutate(`FECHA INICIO OPERACION` = dmy(`FECHA INICIO OPERACION`)) %>% 
  arrange(CTACLIENTE, `FECHA INICIO OPERACION`) %>% 
  dplyr::rename(fechaNacimiento = `FECHA NAC`,
                fechaDeclaracion = `FECHA DECLARACION`,
                entidad = `ENTIDAD SBEF`,
                siglaEntidad = `SIGLA SBEF`,
                tipoObligado = `TIPO OBLIGADO SBEF`,
                tipoCredito = `TIPO CREDITO SBEF`,
                fechaInicioOperacion = `FECHA INICIO OPERACION`,
                calificacion = `SBEF CALIFICACION`,
                contingente = `SBEF CONTINGENTE` ,
                periodoPago = `PERIODO PAGO` )
write_rds(disbJoin, 'D:/!bso/accionScoring/tabla4.rds')
write.xlsx(disbJoin_nov, 'D:/!bso/accionScoring/disbJoin_nov.xlsx')
write.xlsx(disbJoin_dic, 'D:/!bso/accionScoring/disbJoin_dic.xlsx')
write.xlsx(disbJoin_ene, 'D:/!bso/accionScoring/disbJoin_ene.xlsx')

sampleJoin <- disbJoin %>% 
  dplyr::filter(row_number() <= 50000)
  
joinFinal <- disbJoin %>% 
  dplyr::filter(fdes != fechaInicioOperacion) %>% 
  select(-montous, -fdes, -ndisb)

length(unique(disb$CTACLIENTE))
length(unique(disbJoin_dic$CTACLIENTE))
length(unique(disbJoin_ene$CTACLIENTE))




