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
library(openxlsx)
library(ggplot2)
library(ggrepel)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____LECTURA CODIGOS CAEDEC____####
codCaedec <- read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx",sheet='RPT004') %>% 
  select(CAEDEC = caecn, GrupoActEcon = cgaec) %>% 
  glimpse()
codProd <- read_excel("D:/!bso/bases/excel/CodSecProd.xlsx",sheet='RPT004') %>% 
  rename(CAEDEC = caecn, GrupoActEcon = cgaec) %>% 
  glimpse()
####____LECTURA DE DICIEMBRES____####
#BANTOTAL CIERRE MENSUAL
bdcBantotalM <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
                    'Dic2022','.txt'), encoding = 'Latin-1', fill = T)
bdcBantotalM <- bdcBantotalM %>% 
  remove_empty("cols") %>% 
  dplyr::filter(MODULO !=131, ESTADO!='CASTIGADA') %>%
  mutate(tipoCred = case_when(substr(TIPO_CREDITO,1,1)=='M'~'Micro',
                              substr(TIPO_CREDITO,1,1)=='P'~'Pyme',
                              substr(TIPO_CREDITO,1,1)=='N'~'Consumo',
                              substr(TIPO_CREDITO,1,1)=='H'~'Vivienda',)) %>% 
  mutate(ctaCont = substr(as.character(RUBRO),1,3)) %>% 
  select(OPERACION, CTACLIENTE, ctaCont, ESTADO, MONEDA, MONTO, SALDO, tipoCred, TIPO_CREDITO, FDESEMBOLSO, 
         CIU, CAEDEC_DEST, PRODUCTIVO, SECTOR_CARTERA)
  
#BANTOTAL CIERRE DIARIO DIC31
bdcBantotalD <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/06_Base_Cartera_Diaria/BaseCartera_', 
                            '20221231','.txt'), encoding = 'UTF-8', fill = T)
bdcBantotalD <- bdcBantotalD %>% 
  dplyr::filter(MODULO !=131, ESTADO!='CASTIGADA') %>% 
  select(OPERACION, CTACLIENTE, PRODUCTIVO)
#BASEASFI DIC 
bdcASFI <- readRDS('D:/!bso/bases/rds/REPORTE_ASFI_DIC2022_DEP.rds') %>% 
  dplyr::filter(TIPO_REL %in% c("1A","4A","5A","6A","7A")) %>% 
  dplyr::filter(ESTADO %in% c("Vigente","Vencido","Ejecución","Boleta de Garantía")) %>% 
  separate_wider_delim(OPERACION, delim='-', names = c('CTACLIENTE','OPERACION'),
                       too_few = 'align_start', too_many = 'drop') %>% 
  mutate(across(c(OPERACION,CTACLIENTE),~as.integer(.x))) %>% 
  select(CTACLIENTE, OPERACION, califASFI, SEC_PROD)
glimpse(bdcASFI)
#BASE CIC DIC
workdir <- "C:/CIC/20221231/"
namesk <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-K") %>% 
  select(1:2)
cick <- fread(paste0(workdir,"CR20221231K.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = namesk$NAME)
namesh <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-H") %>% 
  select(1:2)
cich <- fread(paste0(workdir,"CR20221231H.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = namesh$NAME)

cicJoin <- cick %>% 
  left_join(cich,by=c("FechaCorte","CodEnvio","IdOperacion","FechaInicio"),suffix = c('_k','_h')) %>% 
  separate_wider_delim(IdOperacion,delim='-',names = c('CTACLIENTE','OPERACION'),
                       too_few = 'align_start',too_many = 'drop') %>% 
  mutate(across(c(CTACLIENTE, OPERACION),~as.integer(.x))) %>% 
  mutate(CtaCont = paste0(CodCapituloCuenta,CodGrupoCuenta,CodCuenta)) %>% 
  dplyr::filter(CtaCont %in% c('131','133','134','135','136','137','623')) %>% 
  dplyr::filter(is.na(CodTipoCancelacion)) %>% 
  select(CTACLIENTE, OPERACION, MontoComputable, CodTipoCredito, CodActividadEconomica,
         CodObjetoCredito, CodMonedaCuenta, CredProdNoProd)

####____JOIN_ALL____####
bdcAll <- bdcBantotalM %>% 
  left_join(bdcBantotalD, by=c("CTACLIENTE", "OPERACION"),suffix=c('_bdcM','bdcD')) %>% 
  left_join(bdcASFI, by=c("CTACLIENTE", "OPERACION")) %>% 
  left_join(cicJoin, by=c("CTACLIENTE", "OPERACION")) %>% 
  left_join(codCaedec, by= c("CAEDEC_DEST"="CAEDEC")) %>% 
  mutate(SEC_PROD_NORMA = ifelse(tipoCred %in% c('Micro','Pyme') & CAEDEC_DEST %in% codProd$CAEDEC,'PROD','NO PROD'))

write_xlsx(bdcAll, "D:/!bso/girCartera/bdcDicSectorProductivo.xlsx")
