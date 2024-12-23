####____CARGA DE PAQUETES____####
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
library(tseries)
library(scales)
library(openxlsx)
library(janitor)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____BASES DE CRUCE____####
base <- readRDS("D:/!bso/girCartera/rds/ec_Jun2023.rds") %>% 
  dplyr::filter(!MODULO %in% c(131,29)) %>% 
  dplyr::filter(ESTADO!="CASTIGADA") %>% 
  dplyr::filter(!(MODULO==118 | str_detect(TIPO_OPER,"MIGR"))) %>% 
  dplyr::filter(opDes==1)

baseNoRepNoRefin <- base %>% 
  dplyr::filter(OPERACION_ORI_REF==0) %>% 
  dplyr::filter(!ctaCont %in% c('135','136','137'))
sum(baseNoRepNoRefin$montous)
n_distinct(baseNoRepNoRefin$OPERACION)

ctas <- read_excel('D:/!bso/bases/excel/ctasTipoCredito.xlsx', sheet='cta') %>% 
  rename(subCtaCont = subCta) %>% 
  mutate(subCtaCont = as.character(subCtaCont))

prod <- read_excel("D:/!bso/bases/excel/CodSecProd.xlsx") %>% 
  mutate(caecn = as.integer(caecn)) %>% 
  select(CAEDEC = caecn, GrupoActEcon = cgaec, Sector, FechaVigencia)

prorrogas <- read_excel('D:/!bso/prorrogas/Prorrogas_Jun2023.xlsx') %>% 
  select(CTACLIENTE, OPERACION) %>% 
  mutate(OBSERVACION = 'Con prórroga')
####___CRITERIOS DE VALIDACION____####
flist <- c("ec_Dic2022")
i <- 1
for(i in 1:length(flist)){
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds/', flist[i], '.rds')) %>% 
    dplyr::filter(MODULO!=131) %>% 
    dplyr::filter(ESTADO!='CASTIGADA') 
  
  
  baseP <- bdc %>% 
    mutate(CAEDEC_DEST = as.integer(CAEDEC_DEST)) %>% 
    left_join(prod,by=c("CAEDEC_DEST"="CAEDEC")) %>% 
    mutate(ES_PROD_CHECK = ifelse(tipoCred %in% c('Micro','PyMe') & !is.na(GrupoActEcon), 1, 0)) %>% 
    mutate(SEC_PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & CAEDEC_DEST %in% codProd$CAEDEC,'PROD','NO PROD')) %>% 
    mutate(ES_PROD_CHECK = ifelse(ES_PROD_CHECK==1 & Sector=='TURISMO' & !str_detect(DESC_OBJCRED, "INVERSION"), 0, ES_PROD_CHECK)) %>% 
    mutate(ES_PROD_CHECK = case_when(ES_PROD_CHECK==1 & Sector=='TURISMO' 
                                     & FDESEMBOLSO<FechaVigencia ~ 0,
                                     ES_PROD_CHECK==1 & Sector=='TURISMO' 
                                     & FDESEMBOLSO>=FechaVigencia & !str_detect(DESC_OBJCRED, "INVERSION")~ 0,
                                     ES_PROD_CHECK==1 & Sector=='TURISMO' 
                                     & FDESEMBOLSO>=FechaVigencia & str_detect(DESC_OBJCRED, "INVERSION")
                                     & MONEDA==101~ 0,
                                     ES_PROD_CHECK==1 & Sector=='H&E'
                                     & FDESEMBOLSO<FechaVigencia ~ 0,
                                     TRUE~ ES_PROD_CHECK))
  
  baseP <- bdc %>% 
    mutate(CAEDEC_DEST = as.integer(CAEDEC_DEST)) %>% 
    left_join(prod,by=c("CAEDEC_DEST"="CAEDEC")) %>% 
    # left_join(cicJoin, by=c("CTACLIENTE", "OPERACION")) %>% 
    mutate(SEC_PROD_NORMA = ifelse(tipoCred %in% c('Micro','PyMe') & CAEDEC_DEST %in% codProd$CAEDEC,'PROD','NO PROD')) %>% 
    mutate(ES_PROD_CHECK = ifelse(tipoCred %in% c('Micro','PyMe') 
                                  & grupoCaedecD %in% c('A','B','C','D','E','F','G'), 1, 0)) %>% #PARA PROD GRUPOS A-G
    mutate(ES_PROD_CHECK = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'TURISMO' 
                                  & FDESEMBOLSO>FechaVigencia & MONEDA ==0 & str_detect(DESC_OBJCRED, "INVERSION"), 1, ES_PROD_CHECK)) %>% #SECTOR TURISMO
    mutate(ES_PROD_CHECK = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'PRODUCCION INTELECTUAL' 
                                  & FDESEMBOLSO>FechaVigencia & MONEDA ==0, 1, ES_PROD_CHECK)) %>% #PRODUCCION INTELECTUAL
    mutate(ES_PROD_CHECK = ifelse(tipoCred %in% c('Micro','PyMe') & Sector == 'H&E' 
                                  & FDESEMBOLSO>FechaVigencia & MONEDA ==0, 1, ES_PROD_CHECK)) %>% #ELÉCTRICOS E HÍBRIDOS
    glimpse()
    
  table(baseP$SECTOR_CARTERA, baseP$ES_PROD_CHECK)
    mutate(ES_PROD_CHECK = ifelse(ES_PROD_CHECK==1 & Sector=='TURISMO' & !str_detect(DESC_OBJCRED, "INVERSION"), 0, ES_PROD_CHECK)) %>% 
    mutate(ES_PROD_CHECK = case_when(ES_PROD_CHECK==1 & Sector=='TURISMO' 
                                     & FDESEMBOLSO<FechaVigencia ~ 0,
                                     ES_PROD_CHECK==1 & Sector=='TURISMO' 
                                     & FDESEMBOLSO>=FechaVigencia & !str_detect(DESC_OBJCRED, "INVERSION")~ 0,
                                     ES_PROD_CHECK==1 & Sector=='TURISMO' 
                                     & FDESEMBOLSO>=FechaVigencia & str_detect(DESC_OBJCRED, "INVERSION")
                                     & MONEDA==101~ 0,
                                     ES_PROD_CHECK==1 & Sector=='H&E'
                                     & FDESEMBOLSO<FechaVigencia ~ 0,
                                     TRUE~ ES_PROD_CHECK))
    
  x <- baseP %>% dplyr::filter(Sector=="H&E")#Correctos
  x <- baseP %>% dplyr::filter(SECTOR_CARTERA=="11.Servicios" & ES_PROD_CHECK==1)
  x <- baseP %>% dplyr::filter(SECTOR_CARTERA=="3.C2.Sector Turismo" & ES_PROD_CHECK==0)
  x <- baseP %>% dplyr::filter(SECTOR_CARTERA=="4.C3.Prod Intelectual" & ES_PROD_CHECK==0)
  baseA <- bdc %>% 
    mutate(across(c(FDESEMBOLSO,FFINALIZA,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FALTACLI),
                  ~as.Date(.x,"%d/%m/%y"))) %>%
    select(CTACLIENTE, OPERACION, FVEN_PROXPAGO, FVEN_ULTPAGO, ESTADO, DIASMORA) %>%
    dplyr::filter(FVEN_PROXPAGO < FVEN_ULTPAGO)
  
  #No se puede replicar validación porque no se cuenta con información en FULT_PAGO
  
  baseB <- bdc %>% 
    mutate(across(c(FDESEMBOLSO,FFINALIZA,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FALTACLI),
                  ~as.Date(.x,"%d/%m/%y"))) %>%
    dplyr::filter(TIPO_CREDITO %in% c('H0', 'H1', 'H2', 'H3', 'H4', 'M0', 'M1',
                                      'M2', 'M7', 'M8', 'N0', 'N1', 'N2', 'P1',
                                      'P3', 'P9')) %>% 
    mutate(ultimoDia=as.Date(monDate, frac=1)) %>% 
    mutate(moraReal=as.numeric(ultimoDia-FVEN_ULTPAGO)) %>% 
    left_join(prorrogas, by=c("CTACLIENTE","OPERACION")) %>% 
    mutate(moraReal = ifelse(!is.na(OBSERVACION),0,moraReal)) %>% 
    mutate(OBSERVACION = ifelse(is.na(OBSERVACION),'Inconsistencia',OBSERVACION)) %>% 
    dplyr::filter(FVEN_PROXPAGO<FULT_PAGO & !(DIASMORA==moraReal)) %>% 
    select(CTACLIENTE, OPERACION, FVEN_PROXPAGO, FVEN_ULTPAGO, ESTADO, 
           DIASMORA, moraReal, FULT_PAGO, OBSERVACION)
  
  
  baseC <- bdc %>%
    anti_join(ctas, by= c('TIPO_CREDITO', 'subCtaCont')) %>%
    dplyr::filter(subCtaCont!= 86501) %>% 
    select(CTACLIENTE, OPERACION, TIPO_CREDITO, subCtaCont, ESTADO) %>% 
    mutate(OBSERVACION = ifelse(TIPO_CREDITO=='P9','Pyme calificado por Días Mora debidamente garantizado con garantía real','Inconsistencia'))
  
  #Inconsistencia encontrada
  #TIPO_CREDITO P9 no existe en el Manual de Cuentas CIC.
  
  exp <- data.frame(Inconsistencias=c("InconsistenciaA","InconsistenciaB","InconsistenciaC"),
                    Descripción=c("Inconsistencia: Fecha de próximo pago es menor a la fecha de último pago",
                                  "Por tipo de crédito los días de mora son diferentes a los días de incumplimiento considerados en la base","Subcuentas incorrectas por tipo de crédito"))
  
  #   res<-data.frame(Resumen="Tomando como referencia el informe de riesgo crediticio, referente a la
  # Evaluación de la base de cartera de créditos al 30 de noviembre de 2022, se realizó la validación de
  # cartera para cada mes desde enero 2022 a febrero del 2023, desde los incisos a-d. La validación a partir del inciso e no se
  #                   pudo realizar debido a que la base de cartera de Bantotal no cuenta con información referente a garantía real,
  #                   si el crédito es productivo o no y fecha de ingreso a estado")
  
  objeto <- list(InconsistenciaA=baseA, InconsistenciaB=baseB, InconsistenciaCyD=baseC, Diccionario=exp)
  write.xlsx(objeto, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/ValidacionBDCBantotal/output/Validacion_',substr(long_list[i],4,10),'.xlsx'))
}