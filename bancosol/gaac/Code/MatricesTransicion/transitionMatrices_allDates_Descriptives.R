#-------------------------
####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
#library(foreign)
#library(reshape)
#library(reshape2)
#library(stringr)
library(lubridate)
#library(Hmisc)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
#library(forecast)
library(quantmod)
#library(astsa)
#library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
#library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
#library(timeSeries)
#library(tseries)
#library(xtable)
#library(openxlsx)
#library(hrbrthemes)
#library(viridis)
#library(scales)
#library(janitor)
#library(RColorBrewer)
# library(paletteer)
# library(plotly)

Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
paleta <- colorRampPalette(c("blue2","slateblue4","slateblue3","violetred3","red3","tan2","yellow3","yellow2"),bias=1.5)
cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
#__________________________________________________________
# Data in
bdcList <- list()
file_list <- list.files(path='D:/!bso/girCartera/rds')
for (i in 1:length(file_list)) {
  
  print(file_list[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds/',
                        file_list[i])) %>% 
    #select(-MODULO) %>% 
    #dplyr::filter(substr(AGENCIA, 1, 1) == '6') %>% 
    # mutate(RUBRO = as.character(RUBRO)) %>% 
    # mutate(CALIFICACION = as.character(CALIFICACION)) %>% 
    # mutate(SALDO = as.double(SALDO)) %>% 
    # mutate(CAEDEC_DEST = as.character(CAEDEC_DEST)) %>% 
    # mutate(fbase = substr(file_list[i], 4, 10)) %>% 
    select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, 
          CALIFICACION, fbase, montous, tipoCred, sucursal,
          caedec3d) %>% 
    mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION),
           rangom = ifelse(montous > 20000, '20k+', 'under20k'))
  bdcList[[i]] <- bdc
}
gc()

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
  arrange(CI, CTACLIENTE, OPERACION, monDate) %>% 
  glimpse()
gc()
bdcList <- NULL
write.csv(bdcFull, 'D:/!bso/transMat/bdcFull_desc.csv')

#---------------------------------------------------------
bdcFull <- fread('D:/!bso/transMat/bdcFull_desc.csv')
bdcFull <- bdcFull %>% 
  arrange(OPERACION, FDESEMBOLSO)
#---------------------------------------------------------
# Tipo Crédito
# plots
bdcTrans <- bdcFull %>% 
  select(OPERACION, CALIFICACION, monDate, tipoCred) %>% 
  #mutate(monDate = zoo::as.yearmon(dayDate)) %>% 
  #select(-dayDate) %>%
  #dplyr::filter(monDate >= 'Jan 2017') %>% 
  group_by(OPERACION) %>%
  arrange(OPERACION, monDate) %>%
  dplyr::rename(cmt = CALIFICACION) %>% 
  mutate(cm1 = dplyr::lag(cmt, 1)) %>% 
  ungroup() %>% 
  mutate(trans = ifelse(cm1 == 'A' & cmt == 'A' , 'AA',
                 ifelse(cm1 == 'A' & cmt == 'B' , 'AB',
                 ifelse(cm1 == 'A' & cmt == 'C' , 'AC',
                 ifelse(cm1 == 'A' & cmt == 'D' , 'AD',
                 ifelse(cm1 == 'A' & cmt == 'E' , 'AE',
                 ifelse(cm1 == 'A' & cmt == 'F' , 'AF',
                 ifelse(cm1 == 'A' & cmt == 'S' , 'AS',
                 ifelse(cm1 == 'B' & cmt == 'A' , 'BA',
                 ifelse(cm1 == 'B' & cmt == 'B' , 'BB',
                 ifelse(cm1 == 'B' & cmt == 'C' , 'BC',
                 ifelse(cm1 == 'B' & cmt == 'D' , 'BD',
                 ifelse(cm1 == 'B' & cmt == 'E' , 'BE',
                 ifelse(cm1 == 'B' & cmt == 'F' , 'BF',
                 ifelse(cm1 == 'B' & cmt == 'S' , 'BS',
                 ifelse(cm1 == 'C' & cmt == 'A' , 'CA',
                 ifelse(cm1 == 'C' & cmt == 'B' , 'CB',
                 ifelse(cm1 == 'C' & cmt == 'C' , 'CC',
                 ifelse(cm1 == 'C' & cmt == 'D' , 'CD',
                 ifelse(cm1 == 'C' & cmt == 'E' , 'CE',
                 ifelse(cm1 == 'C' & cmt == 'F' , 'CF',
                 ifelse(cm1 == 'C' & cmt == 'S' , 'CS',
                 ifelse(cm1 == 'D' & cmt == 'A' , 'DA',
                 ifelse(cm1 == 'D' & cmt == 'B' , 'DB',
                 ifelse(cm1 == 'D' & cmt == 'C' , 'DC',
                 ifelse(cm1 == 'D' & cmt == 'D' , 'DD',
                 ifelse(cm1 == 'D' & cmt == 'E' , 'DE',
                 ifelse(cm1 == 'D' & cmt == 'F' , 'DF',
                 ifelse(cm1 == 'D' & cmt == 'S' , 'DS',
                 ifelse(cm1 == 'E' & cmt == 'A' , 'EA',
                 ifelse(cm1 == 'E' & cmt == 'B' , 'EB',
                 ifelse(cm1 == 'E' & cmt == 'C' , 'EC',
                 ifelse(cm1 == 'E' & cmt == 'D' , 'ED',
                 ifelse(cm1 == 'E' & cmt == 'E' , 'EE',
                 ifelse(cm1 == 'E' & cmt == 'F' , 'EF',
                 ifelse(cm1 == 'E' & cmt == 'S' , 'ES',
                 ifelse(cm1 == 'F' & cmt == 'A' , 'FA',
                 ifelse(cm1 == 'F' & cmt == 'B' , 'FB',
                 ifelse(cm1 == 'F' & cmt == 'C' , 'FC',
                 ifelse(cm1 == 'F' & cmt == 'D' , 'FD',
                 ifelse(cm1 == 'F' & cmt == 'E' , 'FE',
                 ifelse(cm1 == 'F' & cmt == 'F' , 'FF',
                 ifelse(cm1 == 'F' & cmt == 'S' , 'FS',
                 ifelse(cm1 == 'S' & cmt == 'S' , 'SS',
                 NA)))))))))))))))))))))))))))))))))))))))))))) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1, monDate) %>% 
  mutate(rowTot = n()) %>% 
  ungroup() %>% 
  group_by(OPERACION, tipoCred) %>% 
  arrange(tipoCred, OPERACION, monDate) %>% 
  mutate(det = ifelse(cm1 < cmt,1,0)) %>% 
  glimpse()
table(bdcTrans[bdcTrans$monDate == 'ago. 2022' & bdcTrans$tipoCred == 'Micro',]$trans)
write.csv(bdcTrans, 'D:/!bso/transMat/bdcTrans_tipoCred.csv')

# sucursal
# plots
bdcTrans <- bdcFull %>% 
  select(OPERACION, CALIFICACION, monDate, sucursal) %>% 
  #mutate(monDate = zoo::as.yearmon(dayDate)) %>% 
  #select(-dayDate) %>%
  #dplyr::filter(monDate >= 'Jan 2017') %>% 
  group_by(OPERACION) %>%
  arrange(OPERACION, monDate) %>%
  dplyr::rename(cmt = CALIFICACION) %>% 
  mutate(cm1 = dplyr::lag(cmt, 1)) %>% 
  ungroup() %>% 
  mutate(trans = ifelse(cm1 == 'A' & cmt == 'A' , 'AA',
                        ifelse(cm1 == 'A' & cmt == 'B' , 'AB',
                               ifelse(cm1 == 'A' & cmt == 'C' , 'AC',
                                      ifelse(cm1 == 'A' & cmt == 'D' , 'AD',
                                             ifelse(cm1 == 'A' & cmt == 'E' , 'AE',
                                                    ifelse(cm1 == 'A' & cmt == 'F' , 'AF',
                                                           ifelse(cm1 == 'A' & cmt == 'S' , 'AS',
                                                                  ifelse(cm1 == 'B' & cmt == 'A' , 'BA',
                                                                         ifelse(cm1 == 'B' & cmt == 'B' , 'BB',
                                                                                ifelse(cm1 == 'B' & cmt == 'C' , 'BC',
                                                                                       ifelse(cm1 == 'B' & cmt == 'D' , 'BD',
                                                                                              ifelse(cm1 == 'B' & cmt == 'E' , 'BE',
                                                                                                     ifelse(cm1 == 'B' & cmt == 'F' , 'BF',
                                                                                                            ifelse(cm1 == 'B' & cmt == 'S' , 'BS',
                                                                                                                   ifelse(cm1 == 'C' & cmt == 'A' , 'CA',
                                                                                                                          ifelse(cm1 == 'C' & cmt == 'B' , 'CB',
                                                                                                                                 ifelse(cm1 == 'C' & cmt == 'C' , 'CC',
                                                                                                                                        ifelse(cm1 == 'C' & cmt == 'D' , 'CD',
                                                                                                                                               ifelse(cm1 == 'C' & cmt == 'E' , 'CE',
                                                                                                                                                      ifelse(cm1 == 'C' & cmt == 'F' , 'CF',
                                                                                                                                                             ifelse(cm1 == 'C' & cmt == 'S' , 'CS',
                                                                                                                                                                    ifelse(cm1 == 'D' & cmt == 'A' , 'DA',
                                                                                                                                                                           ifelse(cm1 == 'D' & cmt == 'B' , 'DB',
                                                                                                                                                                                  ifelse(cm1 == 'D' & cmt == 'C' , 'DC',
                                                                                                                                                                                         ifelse(cm1 == 'D' & cmt == 'D' , 'DD',
                                                                                                                                                                                                ifelse(cm1 == 'D' & cmt == 'E' , 'DE',
                                                                                                                                                                                                       ifelse(cm1 == 'D' & cmt == 'F' , 'DF',
                                                                                                                                                                                                              ifelse(cm1 == 'D' & cmt == 'S' , 'DS',
                                                                                                                                                                                                                     ifelse(cm1 == 'E' & cmt == 'A' , 'EA',
                                                                                                                                                                                                                            ifelse(cm1 == 'E' & cmt == 'B' , 'EB',
                                                                                                                                                                                                                                   ifelse(cm1 == 'E' & cmt == 'C' , 'EC',
                                                                                                                                                                                                                                          ifelse(cm1 == 'E' & cmt == 'D' , 'ED',
                                                                                                                                                                                                                                                 ifelse(cm1 == 'E' & cmt == 'E' , 'EE',
                                                                                                                                                                                                                                                        ifelse(cm1 == 'E' & cmt == 'F' , 'EF',
                                                                                                                                                                                                                                                               ifelse(cm1 == 'E' & cmt == 'S' , 'ES',
                                                                                                                                                                                                                                                                      ifelse(cm1 == 'F' & cmt == 'A' , 'FA',
                                                                                                                                                                                                                                                                             ifelse(cm1 == 'F' & cmt == 'B' , 'FB',
                                                                                                                                                                                                                                                                                    ifelse(cm1 == 'F' & cmt == 'C' , 'FC',
                                                                                                                                                                                                                                                                                           ifelse(cm1 == 'F' & cmt == 'D' , 'FD',
                                                                                                                                                                                                                                                                                                  ifelse(cm1 == 'F' & cmt == 'E' , 'FE',
                                                                                                                                                                                                                                                                                                         ifelse(cm1 == 'F' & cmt == 'F' , 'FF',
                                                                                                                                                                                                                                                                                                                ifelse(cm1 == 'F' & cmt == 'S' , 'FS',
                                                                                                                                                                                                                                                                                                                       ifelse(cm1 == 'S' & cmt == 'S' , 'SS',
                                                                                                                                                                                                                                                                                                                              NA)))))))))))))))))))))))))))))))))))))))))))) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1, monDate) %>% 
  mutate(rowTot = n()) %>% 
  ungroup() %>% 
  group_by(OPERACION, sucursal) %>% 
  arrange(sucursal, OPERACION, monDate) %>% 
  mutate(det = ifelse(cm1 < cmt,1,0)) %>% 
  glimpse()
#table(bdcTrans[bdcTrans$monDate == 'ago. 2022' & bdcTrans$tipoCred == 'Micro',]$trans)
write.csv(bdcTrans, 'D:/!bso/transMat/bdcTrans_sucursal.csv')

# caedec3d
# plots
bdcTrans <- bdcFull %>% 
  select(OPERACION, CALIFICACION, monDate, caedec3d) %>% 
  #mutate(monDate = zoo::as.yearmon(dayDate)) %>% 
  #select(-dayDate) %>%
  #dplyr::filter(monDate >= 'Jan 2017') %>% 
  group_by(OPERACION) %>%
  arrange(OPERACION, monDate) %>%
  dplyr::rename(cmt = CALIFICACION) %>% 
  mutate(cm1 = dplyr::lag(cmt, 1)) %>% 
  ungroup() %>% 
  mutate(trans = ifelse(cm1 == 'A' & cmt == 'A' , 'AA',
                        ifelse(cm1 == 'A' & cmt == 'B' , 'AB',
                               ifelse(cm1 == 'A' & cmt == 'C' , 'AC',
                                      ifelse(cm1 == 'A' & cmt == 'D' , 'AD',
                                             ifelse(cm1 == 'A' & cmt == 'E' , 'AE',
                                                    ifelse(cm1 == 'A' & cmt == 'F' , 'AF',
                                                           ifelse(cm1 == 'A' & cmt == 'S' , 'AS',
                                                                  ifelse(cm1 == 'B' & cmt == 'A' , 'BA',
                                                                         ifelse(cm1 == 'B' & cmt == 'B' , 'BB',
                                                                                ifelse(cm1 == 'B' & cmt == 'C' , 'BC',
                                                                                       ifelse(cm1 == 'B' & cmt == 'D' , 'BD',
                                                                                              ifelse(cm1 == 'B' & cmt == 'E' , 'BE',
                                                                                                     ifelse(cm1 == 'B' & cmt == 'F' , 'BF',
                                                                                                            ifelse(cm1 == 'B' & cmt == 'S' , 'BS',
                                                                                                                   ifelse(cm1 == 'C' & cmt == 'A' , 'CA',
                                                                                                                          ifelse(cm1 == 'C' & cmt == 'B' , 'CB',
                                                                                                                                 ifelse(cm1 == 'C' & cmt == 'C' , 'CC',
                                                                                                                                        ifelse(cm1 == 'C' & cmt == 'D' , 'CD',
                                                                                                                                               ifelse(cm1 == 'C' & cmt == 'E' , 'CE',
                                                                                                                                                      ifelse(cm1 == 'C' & cmt == 'F' , 'CF',
                                                                                                                                                             ifelse(cm1 == 'C' & cmt == 'S' , 'CS',
                                                                                                                                                                    ifelse(cm1 == 'D' & cmt == 'A' , 'DA',
                                                                                                                                                                           ifelse(cm1 == 'D' & cmt == 'B' , 'DB',
                                                                                                                                                                                  ifelse(cm1 == 'D' & cmt == 'C' , 'DC',
                                                                                                                                                                                         ifelse(cm1 == 'D' & cmt == 'D' , 'DD',
                                                                                                                                                                                                ifelse(cm1 == 'D' & cmt == 'E' , 'DE',
                                                                                                                                                                                                       ifelse(cm1 == 'D' & cmt == 'F' , 'DF',
                                                                                                                                                                                                              ifelse(cm1 == 'D' & cmt == 'S' , 'DS',
                                                                                                                                                                                                                     ifelse(cm1 == 'E' & cmt == 'A' , 'EA',
                                                                                                                                                                                                                            ifelse(cm1 == 'E' & cmt == 'B' , 'EB',
                                                                                                                                                                                                                                   ifelse(cm1 == 'E' & cmt == 'C' , 'EC',
                                                                                                                                                                                                                                          ifelse(cm1 == 'E' & cmt == 'D' , 'ED',
                                                                                                                                                                                                                                                 ifelse(cm1 == 'E' & cmt == 'E' , 'EE',
                                                                                                                                                                                                                                                        ifelse(cm1 == 'E' & cmt == 'F' , 'EF',
                                                                                                                                                                                                                                                               ifelse(cm1 == 'E' & cmt == 'S' , 'ES',
                                                                                                                                                                                                                                                                      ifelse(cm1 == 'F' & cmt == 'A' , 'FA',
                                                                                                                                                                                                                                                                             ifelse(cm1 == 'F' & cmt == 'B' , 'FB',
                                                                                                                                                                                                                                                                                    ifelse(cm1 == 'F' & cmt == 'C' , 'FC',
                                                                                                                                                                                                                                                                                           ifelse(cm1 == 'F' & cmt == 'D' , 'FD',
                                                                                                                                                                                                                                                                                                  ifelse(cm1 == 'F' & cmt == 'E' , 'FE',
                                                                                                                                                                                                                                                                                                         ifelse(cm1 == 'F' & cmt == 'F' , 'FF',
                                                                                                                                                                                                                                                                                                                ifelse(cm1 == 'F' & cmt == 'S' , 'FS',
                                                                                                                                                                                                                                                                                                                       ifelse(cm1 == 'S' & cmt == 'S' , 'SS',
                                                                                                                                                                                                                                                                                                                              NA)))))))))))))))))))))))))))))))))))))))))))) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1, monDate) %>% 
  mutate(rowTot = n()) %>% 
  ungroup() %>% 
  group_by(OPERACION, caedec3d) %>% 
  arrange(caedec3d, OPERACION, monDate) %>% 
  mutate(det = ifelse(cm1 < cmt,1,0)) %>% 
  glimpse()
#table(bdcTrans[bdcTrans$monDate == 'ago. 2022' & bdcTrans$tipoCred == 'Micro',]$trans)
write.csv(bdcTrans, 'D:/!bso/transMat/bdcTrans_caedec3d.csv')

# Tipo Crédito
# plots
bdcTrans <- bdcFull %>% 
  select(OPERACION, CALIFICACION, monDate, rangom) %>% 
  #mutate(monDate = zoo::as.yearmon(dayDate)) %>% 
  #select(-dayDate) %>%
  #dplyr::filter(monDate >= 'Jan 2017') %>% 
  group_by(OPERACION) %>%
  arrange(OPERACION, monDate) %>%
  dplyr::rename(cmt = CALIFICACION) %>% 
  mutate(cm1 = dplyr::lag(cmt, 1)) %>% 
  ungroup() %>% 
  mutate(trans = ifelse(cm1 == 'A' & cmt == 'A' , 'AA',
                        ifelse(cm1 == 'A' & cmt == 'B' , 'AB',
                               ifelse(cm1 == 'A' & cmt == 'C' , 'AC',
                                      ifelse(cm1 == 'A' & cmt == 'D' , 'AD',
                                             ifelse(cm1 == 'A' & cmt == 'E' , 'AE',
                                                    ifelse(cm1 == 'A' & cmt == 'F' , 'AF',
                                                           ifelse(cm1 == 'A' & cmt == 'S' , 'AS',
                                                                  ifelse(cm1 == 'B' & cmt == 'A' , 'BA',
                                                                         ifelse(cm1 == 'B' & cmt == 'B' , 'BB',
                                                                                ifelse(cm1 == 'B' & cmt == 'C' , 'BC',
                                                                                       ifelse(cm1 == 'B' & cmt == 'D' , 'BD',
                                                                                              ifelse(cm1 == 'B' & cmt == 'E' , 'BE',
                                                                                                     ifelse(cm1 == 'B' & cmt == 'F' , 'BF',
                                                                                                            ifelse(cm1 == 'B' & cmt == 'S' , 'BS',
                                                                                                                   ifelse(cm1 == 'C' & cmt == 'A' , 'CA',
                                                                                                                          ifelse(cm1 == 'C' & cmt == 'B' , 'CB',
                                                                                                                                 ifelse(cm1 == 'C' & cmt == 'C' , 'CC',
                                                                                                                                        ifelse(cm1 == 'C' & cmt == 'D' , 'CD',
                                                                                                                                               ifelse(cm1 == 'C' & cmt == 'E' , 'CE',
                                                                                                                                                      ifelse(cm1 == 'C' & cmt == 'F' , 'CF',
                                                                                                                                                             ifelse(cm1 == 'C' & cmt == 'S' , 'CS',
                                                                                                                                                                    ifelse(cm1 == 'D' & cmt == 'A' , 'DA',
                                                                                                                                                                           ifelse(cm1 == 'D' & cmt == 'B' , 'DB',
                                                                                                                                                                                  ifelse(cm1 == 'D' & cmt == 'C' , 'DC',
                                                                                                                                                                                         ifelse(cm1 == 'D' & cmt == 'D' , 'DD',
                                                                                                                                                                                                ifelse(cm1 == 'D' & cmt == 'E' , 'DE',
                                                                                                                                                                                                       ifelse(cm1 == 'D' & cmt == 'F' , 'DF',
                                                                                                                                                                                                              ifelse(cm1 == 'D' & cmt == 'S' , 'DS',
                                                                                                                                                                                                                     ifelse(cm1 == 'E' & cmt == 'A' , 'EA',
                                                                                                                                                                                                                            ifelse(cm1 == 'E' & cmt == 'B' , 'EB',
                                                                                                                                                                                                                                   ifelse(cm1 == 'E' & cmt == 'C' , 'EC',
                                                                                                                                                                                                                                          ifelse(cm1 == 'E' & cmt == 'D' , 'ED',
                                                                                                                                                                                                                                                 ifelse(cm1 == 'E' & cmt == 'E' , 'EE',
                                                                                                                                                                                                                                                        ifelse(cm1 == 'E' & cmt == 'F' , 'EF',
                                                                                                                                                                                                                                                               ifelse(cm1 == 'E' & cmt == 'S' , 'ES',
                                                                                                                                                                                                                                                                      ifelse(cm1 == 'F' & cmt == 'A' , 'FA',
                                                                                                                                                                                                                                                                             ifelse(cm1 == 'F' & cmt == 'B' , 'FB',
                                                                                                                                                                                                                                                                                    ifelse(cm1 == 'F' & cmt == 'C' , 'FC',
                                                                                                                                                                                                                                                                                           ifelse(cm1 == 'F' & cmt == 'D' , 'FD',
                                                                                                                                                                                                                                                                                                  ifelse(cm1 == 'F' & cmt == 'E' , 'FE',
                                                                                                                                                                                                                                                                                                         ifelse(cm1 == 'F' & cmt == 'F' , 'FF',
                                                                                                                                                                                                                                                                                                                ifelse(cm1 == 'F' & cmt == 'S' , 'FS',
                                                                                                                                                                                                                                                                                                                       ifelse(cm1 == 'S' & cmt == 'S' , 'SS',
                                                                                                                                                                                                                                                                                                                              NA)))))))))))))))))))))))))))))))))))))))))))) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1, monDate) %>% 
  mutate(rowTot = n()) %>% 
  ungroup() %>% 
  group_by(OPERACION, rangom) %>% 
  arrange(rangom, OPERACION, monDate) %>% 
  mutate(det = ifelse(cm1 < cmt,1,0)) %>% 
  glimpse()
#table(bdcTrans[bdcTrans$monDate == 'ago. 2022' & bdcTrans$tipoCred == 'Micro',]$trans)
write.csv(bdcTrans, 'D:/!bso/transMat/bdcTrans_rangom.csv')
#===============================================================================
#===============================================================================
#===============================================================================
# transition matrices and plots
bdcTrans <- fread('D:/!bso/transMat/bdcTrans_tipoCred.csv')
bdcCancel <- readRDS('D:/!bso/transMat/matCancel_tipoCred.rds') 
tm <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate, tipoCred) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, tipoCred, trans) %>%
  summarise_all(sum) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(bdcCancel) %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% 
  group_by(monDate, tipoCred, cm1) %>% 
  mutate(rowTot = sum(one)) %>% 
  mutate(prob = round(one/rowTot*100,2)) %>% 
  arrange(cm1, monDate, tipoCred) %>% 
  mutate(Deterioro = case_when(cm1 == 'A' ~ sum(prob[cmt != 'A' & cmt != 'Z']), 
                             cm1 == 'B' ~ sum(prob[cmt != 'A' & cmt != 'B' & cmt != 'Z']),
                             cm1 == 'C' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C' & cmt != 'Z']),
                             cm1 == 'D' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D' & cmt != 'Z']),
                             cm1 == 'E' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E' & cmt != 'Z']),
                             cm1 == 'F' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E'& cmt != 'F' & cmt != 'Z']),
                             TRUE ~ 0)) %>% 
  mutate(Recuperacion = case_when(cm1 == 'A' ~ 0,
                                  cm1 == 'Z' ~ 0,
                             cm1 == 'B' ~ sum(prob[cmt == 'A' | cmt == 'Z']),
                             cm1 == 'C' ~ sum(prob[cmt == 'A' | cmt == 'B' | cmt == 'Z']),
                             cm1 == 'D' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C' | cmt == 'Z']),
                             cm1 == 'E' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D' | cmt == 'Z']),
                             cm1 == 'F' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D'| cmt == 'E' | cmt == 'Z']),
                             cm1 == 'S' ~ sum(prob[cmt == 'Z']),
                             TRUE ~ 0)) %>% 
  mutate(Permanencia = case_when(cm1 == 'A' ~ sum(prob[cmt == 'A']), 
                             cm1 == 'B' ~ sum(prob[cmt == 'B']),
                             cm1 == 'C' ~ sum(prob[cmt == 'C']),
                             cm1 == 'D' ~ sum(prob[cmt == 'D']),
                             cm1 == 'E' ~ sum(prob[cmt == 'E']),
                             cm1 == 'F' ~ sum(prob[cmt == 'F']),
                             cm1 == 'S' ~ sum(prob[cmt == 'S']),
                             cm1 == 'Z' ~ sum(prob[cmt == 'Z']),
                             TRUE ~ 0)) %>% 
  glimpse()

gphT <- tm %>% 
  dplyr::filter(monDate > 'dic. 2015') %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  mutate(eom = as.Date(monDate, frac = 1)) %>%
  dplyr::filter(!is.na(tipoCred)) %>% 
  dplyr::filter(tipoCred != 'PyMe') %>% 
  glimpse()

gph <- gphT %>% 
  ungroup() %>% 
  dplyr::filter(cm1 == 'B') %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  select(monDate, Recuperacion, Permanencia, Deterioro, panel, tipoCred) %>%
  group_by(monDate, panel, tipoCred) %>% 
  summarise_all(mean) %>% 
  pivot_longer(!c('monDate', 'panel', 'tipoCred'))  %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>%  
  dplyr::rename(Destino = name) %>%
  ungroup() %>%
  group_by(monDate) %>% 
  mutate(ptot = sum(value)) %>% 
  glimpse()

cbp3 <- c( "#D43B1B","#4198B5", "#246D94",
          "#E96732", "#FB9263")
ggplot(gph, aes(x=Fecha, y=value, fill=Destino)) + 
  geom_area() + theme_minimal() +
  #facet_grid( ~ panel, scales = "free_x", space = "free_x") +
  facet_grid(tipoCred ~ panel, scales = "free_x", space = "free_x") +
  scale_x_date(date_breaks = "4 month", labels=date_format("%b%Y")) +
  theme(axis.text.x = element_text(size = 5),
        panel.spacing = unit(-0.5, "lines")) +
  xlab('') + ylab('Prob. de transicion (%)')  +
  theme(strip.text.x = element_blank(),
        legend.position = 'bottom', 
        axis.text.y = element_text(size=8),
        panel.spacing = unit(1, "mm", data = NULL)) +
  scale_fill_manual(values = cbp3) +
  geom_vline(xintercept = as.Date('2020-03-31'), linetype="dashed", color = "black") +
  geom_vline(xintercept = as.Date('2021-11-30'), linetype="dashed", color = "black")
ggsave(filename ='D:/!bso/transMat/transicion_desde_B_tipoCred.png')

#===============================================================================
# caedec3d
bdcTrans <- fread('D:/!bso/transMat/bdcTrans_caedec3d.csv')
bdcCancel <- readRDS('D:/!bso/transMat/matCancel_caedec3d.rds') 
tm <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate, caedec3d) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, caedec3d, trans) %>%
  summarise_all(sum) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(bdcCancel) %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% 
  group_by(monDate, caedec3d, cm1) %>% 
  mutate(rowTot = sum(one)) %>% 
  mutate(prob = round(one/rowTot*100,2)) %>% 
  arrange(cm1, monDate, caedec3d) %>% 
  mutate(Deterioro = case_when(cm1 == 'A' ~ sum(prob[cmt != 'A' & cmt != 'Z']), 
                               cm1 == 'B' ~ sum(prob[cmt != 'A' & cmt != 'B' & cmt != 'Z']),
                               cm1 == 'C' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C' & cmt != 'Z']),
                               cm1 == 'D' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D' & cmt != 'Z']),
                               cm1 == 'E' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E' & cmt != 'Z']),
                               cm1 == 'F' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E'& cmt != 'F' & cmt != 'Z']),
                               TRUE ~ 0)) %>% 
  mutate(Recuperacion = case_when(cm1 == 'A' ~ sum(prob[cmt == 'A']),
                                  cm1 == 'Z' ~ 0,
                                  cm1 == 'B' ~ sum(prob[cmt == 'A' | cmt == 'Z']),
                                  cm1 == 'C' ~ sum(prob[cmt == 'A' | cmt == 'B' | cmt == 'Z']),
                                  cm1 == 'D' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C' | cmt == 'Z']),
                                  cm1 == 'E' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D' | cmt == 'Z']),
                                  cm1 == 'F' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D'| cmt == 'E' | cmt == 'Z']),
                                  cm1 == 'S' ~ sum(prob[cmt == 'Z']),
                                  TRUE ~ 0)) %>% 
  mutate(Permanencia = case_when(cm1 == 'A' ~ sum(prob[cmt == 'A']), 
                                 cm1 == 'B' ~ sum(prob[cmt == 'B']),
                                 cm1 == 'C' ~ sum(prob[cmt == 'C']),
                                 cm1 == 'D' ~ sum(prob[cmt == 'D']),
                                 cm1 == 'E' ~ sum(prob[cmt == 'E']),
                                 cm1 == 'F' ~ sum(prob[cmt == 'F']),
                                 cm1 == 'S' ~ sum(prob[cmt == 'S']),
                                 cm1 == 'Z' ~ sum(prob[cmt == 'Z']),
                                 TRUE ~ 0)) %>% 
  glimpse()

gphT <- tm %>% 
  dplyr::filter(monDate > 'dic. 2015') %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  mutate(eom = as.Date(monDate, frac = 1)) %>%
  dplyr::filter(!is.na(caedec3d)) %>% 
  dplyr::filter(caedec3d != '4.Productivo GDE') %>% 
  glimpse()

gph <- gphT %>% 
  ungroup() %>% 
  dplyr::filter(cm1 == 'B') %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  select(monDate, Recuperacion, Permanencia, Deterioro, panel, caedec3d) %>%
  group_by(monDate, panel, caedec3d) %>% 
  summarise_all(mean) %>% 
  pivot_longer(!c('monDate', 'panel', 'caedec3d'))  %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>%  
  dplyr::rename(Destino = name) %>%
  ungroup() %>%
  group_by(monDate) %>% 
  mutate(ptot = sum(value)) %>%
  mutate(caedec3d = case_when(caedec3d == '1.Servicios' ~ 'Servicios',
                              caedec3d == '2.Comercio' ~ 'Comercio',
                              caedec3d == '3.Productivo VIV'~ 'Construccion',
                              caedec3d == '5.Productivo GDI'~ 'Productivo')) %>%
  glimpse()

cbp3 <- c( "#D43B1B","#4198B5", "#246D94",
           "#E96732", "#FB9263")
ggplot(gph, aes(x=Fecha, y=value, fill=Destino)) + 
  geom_area() + theme_minimal() +
  #facet_grid( ~ panel, scales = "free_x", space = "free_x") +
  facet_grid(caedec3d ~ panel, scales = "free_x", space = "free_x") +
  scale_x_date(date_breaks = "4 month", labels=date_format("%b%Y")) +
  theme(axis.text.x = element_text(size = 5),
        panel.spacing = unit(-0.3, "lines")) +
  xlab('') + ylab('Prob. de transicion (%)')  +
  theme(strip.text.x = element_blank(),
        legend.position = 'bottom', 
        axis.text.y = element_text(size=8),
        panel.spacing = unit(1, "mm", data = NULL)) +
  scale_fill_manual(values = cbp3) +
  geom_vline(xintercept = as.Date('2020-03-31'), linetype="dashed", color = "black") +
  geom_vline(xintercept = as.Date('2021-11-30'), linetype="dashed", color = "black")
ggsave(filename ='D:/!bso/transMat/transicion_desde_B_caedec3d.png')

#===============================================================================
# sucursal
bdcTrans <- fread('D:/!bso/transMat/bdcTrans_sucursal.csv')
bdcCancel <- readRDS('D:/!bso/transMat/matCancel_sucursal.rds') 
tm <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate, sucursal) %>% 
  mutate(sucursal = as.character(sucursal)) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, sucursal, trans) %>%
  summarise_all(sum) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(bdcCancel) %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% 
  group_by(monDate, sucursal, cm1) %>% 
  mutate(rowTot = sum(one)) %>% 
  mutate(prob = round(one/rowTot*100,2)) %>% 
  arrange(cm1, monDate, sucursal) %>% 
  mutate(Deterioro = case_when(cm1 == 'A' ~ sum(prob[cmt != 'A' & cmt != 'Z']), 
                               cm1 == 'B' ~ sum(prob[cmt != 'A' & cmt != 'B' & cmt != 'Z']),
                               cm1 == 'C' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C' & cmt != 'Z']),
                               cm1 == 'D' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D' & cmt != 'Z']),
                               cm1 == 'E' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E' & cmt != 'Z']),
                               cm1 == 'F' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E'& cmt != 'F' & cmt != 'Z']),
                               TRUE ~ 0)) %>% 
  mutate(Recuperacion = case_when(cm1 == 'A' ~ 0,
                                  cm1 == 'Z' ~ 0,
                                  cm1 == 'B' ~ sum(prob[cmt == 'A' | cmt == 'Z']),
                                  cm1 == 'C' ~ sum(prob[cmt == 'A' | cmt == 'B' | cmt == 'Z']),
                                  cm1 == 'D' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C' | cmt == 'Z']),
                                  cm1 == 'E' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D' | cmt == 'Z']),
                                  cm1 == 'F' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D'| cmt == 'E' | cmt == 'Z']),
                                  cm1 == 'S' ~ sum(prob[cmt == 'Z']),
                                  TRUE ~ 0)) %>% 
  mutate(Permanencia = case_when(cm1 == 'A' ~ sum(prob[cmt == 'A']), 
                                 cm1 == 'B' ~ sum(prob[cmt == 'B']),
                                 cm1 == 'C' ~ sum(prob[cmt == 'C']),
                                 cm1 == 'D' ~ sum(prob[cmt == 'D']),
                                 cm1 == 'E' ~ sum(prob[cmt == 'E']),
                                 cm1 == 'F' ~ sum(prob[cmt == 'F']),
                                 cm1 == 'S' ~ sum(prob[cmt == 'S']),
                                 cm1 == 'Z' ~ sum(prob[cmt == 'Z']),
                                 TRUE ~ 0)) %>% 
  glimpse()

gphT <- tm %>% 
  dplyr::filter(monDate > 'dic. 2015') %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  mutate(eom = as.Date(monDate, frac = 1)) %>%
  dplyr::filter(!is.na(sucursal)) %>% 
  dplyr::filter(sucursal != '4.Productivo GDE') %>% 
  glimpse()

gph <- gphT %>% 
  ungroup() %>% 
  dplyr::filter(cm1 == 'B') %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  select(monDate, Recuperacion, Permanencia, Deterioro, panel, sucursal) %>%
  group_by(monDate, panel, sucursal) %>% 
  summarise_all(mean) %>% 
  pivot_longer(!c('monDate', 'panel', 'sucursal'))  %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>%  
  dplyr::rename(Destino = name) %>%
  ungroup() %>%
  group_by(monDate) %>% 
  mutate(ptot = sum(value)) %>% 
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
  #dplyr::filter(sucursal == '9' | sucursal == '10' | sucursal == '7') %>% 
  glimpse()

ggplot(gph[gph$sucursal == '9' | gph$sucursal == '10' | gph$sucursal == '7',], aes(x=Fecha, y=value, fill=Destino)) + 
  geom_area() + theme_minimal() +
  #facet_grid( ~ panel, scales = "free_x", space = "free_x") +
  facet_grid(Sucursal ~ panel, scales = "free_x", space = "free_x") +
  scale_x_date(date_breaks = "4 month", labels=date_format("%b%Y")) +
  theme(axis.text.x = element_text(size = 5),
        panel.spacing = unit(-0.3, "lines")) +
  xlab('') + ylab('Prob. de transicion (%)')  +
  theme(strip.text.x = element_blank(),
        legend.position = 'bottom', 
        axis.text.y = element_text(size=8),
        panel.spacing = unit(1, "mm", data = NULL)) +
  scale_fill_manual(values = cbp3) +
  geom_vline(xintercept = as.Date('2020-03-31'), linetype="dashed", color = "black") +
  geom_vline(xintercept = as.Date('2021-11-30'), linetype="dashed", color = "black")
ggsave(filename ='D:/!bso/transMat/transicion_desde_B_sucursal_1.png')


ggplot(gph[gph$sucursal == '2' | gph$sucursal == '3' | gph$sucursal == '4',], aes(x=Fecha, y=value, fill=Destino)) + 
  geom_area() + theme_minimal() +
  #facet_grid( ~ panel, scales = "free_x", space = "free_x") +
  facet_grid(Sucursal ~ panel, scales = "free_x", space = "free_x") +
  scale_x_date(date_breaks = "4 month", labels=date_format("%b%Y")) +
  theme(axis.text.x = element_text(size = 5),
        panel.spacing = unit(-0.3, "lines")) +
  xlab('') + ylab('Prob. de transicion (%)')  +
  theme(strip.text.x = element_blank(),
        legend.position = 'bottom', 
        axis.text.y = element_text(size=8),
        panel.spacing = unit(1, "mm", data = NULL)) +
  scale_fill_manual(values = cbp3) +
  geom_vline(xintercept = as.Date('2020-03-31'), linetype="dashed", color = "black") +
  geom_vline(xintercept = as.Date('2021-11-30'), linetype="dashed", color = "black")
ggsave(filename ='D:/!bso/transMat/transicion_desde_B_sucursal_2.png')


ggplot(gph[gph$sucursal == '1' | gph$sucursal == '5' | gph$sucursal == '6' | gph$sucursal == '8',], aes(x=Fecha, y=value, fill=Destino)) + 
  geom_area() + theme_minimal() +
  #facet_grid( ~ panel, scales = "free_x", space = "free_x") +
  facet_grid(Sucursal ~ panel, scales = "free_x", space = "free_x") +
  scale_x_date(date_breaks = "4 month", labels=date_format("%b%Y")) +
  theme(axis.text.x = element_text(size = 5),
        panel.spacing = unit(-0.3, "lines")) +
  xlab('') + ylab('Prob. de transicion (%)')  +
  theme(strip.text.x = element_blank(),
        legend.position = 'bottom', 
        axis.text.y = element_text(size=8),
        panel.spacing = unit(1, "mm", data = NULL)) +
  scale_fill_manual(values = cbp3) +
  geom_vline(xintercept = as.Date('2020-03-31'), linetype="dashed", color = "black") +
  geom_vline(xintercept = as.Date('2021-11-30'), linetype="dashed", color = "black")
ggsave(filename ='D:/!bso/transMat/transicion_desde_B_sucursal_3.png')

gphT <- tm %>% 
  dplyr::filter(monDate > 'dic. 2015') %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  mutate(eom = as.Date(monDate, frac = 1)) %>%
  dplyr::filter(!is.na(sucursal)) %>% 
  dplyr::filter(sucursal != '4.Productivo GDE') %>% 
  glimpse()

#=========================================================================
# Aggregate transition matrices
bdcTrans <- fread('D:/!bso/transMat/bdcTrans.csv')
bdcCancel <- readRDS('D:/!bso/transMat/matCancel.rds') 
tm <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, trans) %>%
  summarise_all(sum) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(bdcCancel) %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% 
  group_by(monDate, cm1) %>% 
  mutate(rowTot = sum(one)) %>% 
  mutate(prob = round(one/rowTot*100,2)) %>% 
  arrange(cm1, monDate) %>% 
  mutate(Deterioro = case_when(cm1 == 'A' ~ sum(prob[cmt != 'A' & cmt != 'Z']), 
                               cm1 == 'B' ~ sum(prob[cmt != 'A' & cmt != 'B' & cmt != 'Z']),
                               cm1 == 'C' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C' & cmt != 'Z']),
                               cm1 == 'D' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D' & cmt != 'Z']),
                               cm1 == 'E' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E' & cmt != 'Z']),
                               cm1 == 'F' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E'& cmt != 'F' & cmt != 'Z']),
                               TRUE ~ 0)) %>% 
  mutate(Recuperacion = case_when(cm1 == 'A' ~ sum(prob[cmt == 'Z']),
                                  cm1 == 'Z' ~ 0,
                                  cm1 == 'B' ~ sum(prob[cmt == 'A' | cmt == 'Z']),
                                  cm1 == 'C' ~ sum(prob[cmt == 'A' | cmt == 'B' | cmt == 'Z']),
                                  cm1 == 'D' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C' | cmt == 'Z']),
                                  cm1 == 'E' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D' | cmt == 'Z']),
                                  cm1 == 'F' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D'| cmt == 'E' | cmt == 'Z']),
                                  cm1 == 'S' ~ sum(prob[cmt == 'Z']),
                                  TRUE ~ 0)) %>% 
  mutate(Permanencia = case_when(cm1 == 'A' ~ sum(prob[cmt == 'A']), 
                                 cm1 == 'B' ~ sum(prob[cmt == 'B']),
                                 cm1 == 'C' ~ sum(prob[cmt == 'C']),
                                 cm1 == 'D' ~ sum(prob[cmt == 'D']),
                                 cm1 == 'E' ~ sum(prob[cmt == 'E']),
                                 cm1 == 'F' ~ sum(prob[cmt == 'F']),
                                 cm1 == 'S' ~ sum(prob[cmt == 'S']),
                                 cm1 == 'Z' ~ sum(prob[cmt == 'Z']),
                                 TRUE ~ 0)) %>% 
  glimpse()

gphT <- tm %>% 
  dplyr::filter(monDate > 'dic. 2015') %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  mutate(eom = as.Date(monDate, frac = 1)) %>% 
  glimpse()


gph <- gphT %>% 
  ungroup() %>% 
  dplyr::filter(cm1 == 'B') %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  select(monDate, Recuperacion, Permanencia, Deterioro, panel) %>%
  group_by(monDate, panel) %>% 
  summarise_all(mean) %>% 
  pivot_longer(!c('monDate', 'panel'))  %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>%  
  dplyr::rename(Destino = name) %>%
  ungroup() %>%
  group_by(monDate) %>% 
  mutate(ptot = sum(value)) %>% 
  glimpse()

filename = 'D:/!bso/transMat/transicion_desde_B_2.png'
ggplot(gph, aes(x=Fecha, y=value, fill=Destino)) + 
  geom_area() + theme_minimal() +
  facet_grid( ~ panel, scales = "free_x", space = "free_x") +
  scale_x_date(date_breaks = "4 month", labels=date_format("%b%Y")) +
  theme(axis.text.x = element_text(size = 5),
        panel.spacing = unit(-0.5, "lines")) +
  xlab('') + ylab('Prob. de transicion (%)') +
  #ggtitle('Evolucion de la probabilidad mensual de transición desde B') +
  theme(strip.text.x = element_blank(),
        legend.position = 'bottom') +
  scale_fill_manual(values = cbp3) +
  geom_vline(xintercept = as.Date('2020-03-31'), linetype="dashed", color = "black") +
  geom_vline(xintercept = as.Date('2021-11-30'), linetype="dashed", color = "black")
ggsave(filename)

gph <- gphT %>% 
  ungroup() %>% 
  dplyr::filter(cm1 == 'A') %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate > 'oct. 2021') %>% 
  mutate(panel = ifelse( monDate <= 'mar. 2020',1,2)) %>% 
  select(monDate, Recuperacion, Permanencia, Deterioro, panel) %>%
  group_by(monDate, panel) %>% 
  summarise_all(mean) %>% 
  pivot_longer(!c('monDate', 'panel'))  %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>%  
  dplyr::rename(Destino = name) %>%
  ungroup() %>%
  group_by(monDate) %>% 
  mutate(ptot = sum(value)) %>% 
  glimpse()

filename = 'D:/!bso/transMat/transicion_desde_A_2.png'
ggplot(gph, aes(x=Fecha, y=value, fill=Destino)) + 
  geom_area() + theme_minimal() +
  facet_grid( ~ panel, scales = "free_x", space = "free_x") +
  scale_x_date(date_breaks = "4 month", labels=date_format("%b%Y")) +
  theme(axis.text.x = element_text(size = 5),
        panel.spacing = unit(-0.5, "lines")) +
  xlab('') + ylab('Prob. de transicion (%)') +
  #ggtitle('Evolucion de la probabilidad mensual de transición desde B') +
  theme(strip.text.x = element_blank(),
        legend.position = 'bottom') +
  scale_fill_manual(values = cbp3) +
  geom_vline(xintercept = as.Date('2020-03-31'), linetype="dashed", color = "black") +
  geom_vline(xintercept = as.Date('2021-11-30'), linetype="dashed", color = "black")
ggsave(filename)

# average transition
gph <- tm %>% 
  #dplyr::filter(cm1 == 'B') %>% 
  #dplyr::filter(monDate == 'ago. 2022') %>% 
  arrange(trans, monDate) %>%
  ungroup() %>% 
  select(trans, prob, cm1, cmt) %>% 
  group_by(trans, cm1, cmt) %>%
  summarise_all(mean) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1) %>% 
  mutate(ptot = sum(prob)) %>% 
  mutate(excess = 100-ptot) %>% 
  mutate(pct = prob/ptot) %>% 
  mutate(prob = prob + (excess*pct)) %>% 
  mutate(ptot = sum(prob)) %>% 
  glimpse()

tmExp <- gph %>% 
  ungroup() %>% 
  select(cm1, cmt, prob) %>% 
  group_by(cm1, cmt) %>% 
  summarise_all(mean) %>%
  dplyr::filter(!is.na(cm1)) %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  ungroup() %>% 
  #group_by(cm1) %>% 
  arrange(cm1) %>% 
  #mutate(cumProb = cumsum(prob))  %>%
  select(cm1, cmt, prob) %>%
  mutate(prob = prob/100) %>%
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  select(-cm1) %>%
  add_row(A = 0, B=0, C=0, D=0, E=0, F=0, S=0, Z=1) %>% 
  glimpse()
M <-data.matrix(tmExp)
cm1v <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')
M

#===============================================================================
# Just checking
# bdcLast <- fread('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraAgo2022.txt', 
#                  encoding = 'Latin-1', fill = T)
bdcLast <- readRDS('D:/!bso/girCartera/rds/ec_Ago2022.rds')
bdcAgo <- bdcLast %>% 
  dplyr::filter(MODULO != 131) %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  mutate(activoAgo = 1) %>% 
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(intus = saldous * TASAACT/100) 
sum(bdcAgo$previus)
summary(bdcAgo$saldous)
quantile(bdcAgo$saldous, probs = c(0.1, 0.25, 0.5, 0.75, 0.8, 0.85, 0.9, 0.95, 0.99))
sd(bdcAgo$saldous)
# bdcLast <- fread('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraAgo2021.txt', 
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
# sum(bdcAgo$previus)
stateAgo <- bdcLast %>% 
  dplyr::filter(MODULO != 131) %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  mutate(activoAgo = 1) %>% 
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(intus = saldous * TASAACT/100) %>% 
  mutate(nops = ifelse(ESTADO != 'CASTIGADA', 1, 0)) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  mutate(previus = ifelse(ESTADO == 'CASTIGADA', saldous, previus)) %>% 
  select(CALIFICACION, previus, nops) %>% 
  group_by(CALIFICACION) %>% 
  summarise(previ = sum(previus), nops = sum(nops)) %>% 
  add_row(CALIFICACION = 'Z', nops = 990443, previ = 0) %>% # n comes from  sum(bdcCancel$one)
  glimpse()
write.xlsx(stateAgo, 'D:/!bso/transMat/currDescriptives.xlsx')
#===============================================================================
# Current state
bdcLast <- fread('D:/!bso/BaseCarteraAgo2022.txt', 
                 encoding = 'Latin-1', fill = T)
stateAgo <- bdcLast %>% 
  dplyr::filter(MODULO != 131) %>% 
  #dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  mutate(activoAgo = 1) %>% 
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(intus = saldous * TASAACT/100) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  select(CALIFICACION) %>% 
  group_by(CALIFICACION) %>% 
  tally() %>% 
  add_row(CALIFICACION = 'Z', n = 990443) %>% # n comes from  sum(bdcCancel$one)
  select(-CALIFICACION) %>% 
  glimpse()
transMat <- matrix(M, nrow = 8, ncol = 8)
transMat
S <-data.matrix(stateAgo)
currState <- matrix(S, nrow = 8, ncol = 1)
currState

futureState <- t(round(t(S) %*% M))
futureState
currState

M12 <- M %^% 12
M12
futureState12 <- round(t(t(S) %*% M12))
futureState12
currState

# provision
prevAgo <- bdcLast %>% 
  dplyr::filter(MODULO != 131) %>% 
  #dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  mutate(activoAgo = 1) %>% 
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(intus = saldous * TASAACT/100) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  mutate(previus = ifelse(CALIFICACION == 'S', saldous, previus)) %>% 
  select(CALIFICACION, previus) %>% 
  group_by(CALIFICACION) %>% 
  summarise_all(sum) %>% 
  add_row(CALIFICACION = 'Z', previus = 0) %>% 
  glimpse()

stateMat <- as_tibble(cbind(currState, futureState12, prevAgo)) %>% 
  mutate(prevNops = previus/currState) %>% 
  mutate(newPrev = prevNops*n) %>% 
  adorn_totals('row') %>% 
  mutate(difPrev = newPrev - previus) %>% 
  glimpse()
stateMat
write.xlsx(stateMat, 'D:/!bso/transMat/futureState.xlsx')

#===============================================================================
#===============================================================================
# descriptives
bdcLast <- readRDS('D:/!bso/girCartera/rds/ec_Ago2022.rds')
des_tipoCred <- bdcLast %>% 
  dplyr::filter(MODULO != 131) %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  mutate(activoAgo = 1) %>% 
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(saldoMora = ifelse(DIASMORA > 0, saldous, 0)) %>% 
  mutate(intus = saldous * TASAACT/100) %>% 
  select(tipoCred, previus, saldous, saldoMora) %>% 
  group_by(tipoCred) %>% 
  summarise_all(sum) %>% 
  mutate(moraRel = saldoMora/saldous*100) %>% 
  glimpse()

write.xlsx(des_tipoCred, 'D:/!bso/transMat/des_tipoCred.xlsx')

des_sucursal <- bdcLast %>% 
  dplyr::filter(MODULO != 131) %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  mutate(activoAgo = 1) %>% 
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(saldoMora = ifelse(DIASMORA > 0, saldous, 0)) %>% 
  mutate(intus = saldous * TASAACT/100) %>% 
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
  select(Sucursal, previus, saldous, saldoMora) %>% 
  group_by(Sucursal) %>% 
  summarise_all(sum) %>% 
  mutate(moraRel = saldoMora/saldous*100) %>% 
  glimpse()

write.xlsx(des_sucursal, 'D:/!bso/transMat/des_sucursal.xlsx')

des_caedec3d <- bdcLast %>% 
  dplyr::filter(MODULO != 131) %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  mutate(activoAgo = 1) %>% 
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(saldoMora = ifelse(DIASMORA > 0, saldous, 0)) %>% 
  mutate(intus = saldous * TASAACT/100) %>% 
  select(caedec3d, previus, saldous, saldoMora) %>% 
  group_by(caedec3d) %>% 
  summarise_all(sum) %>% 
  mutate(moraRel = saldoMora/saldous*100) %>% 
  glimpse()

write.xlsx(des_caedec3d, 'D:/!bso/transMat/des_caedec3d.xlsx')
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
# PPT figures
# Provision distribution
prevDist <- read.csv('D:/!bso/transMat/ytprev_12avg.csv') %>% 
  mutate(newF = fdetPrev - oldfPrev) %>%
  mutate(netFlow = castPrev + fdetPrev + detPrev - exitPrev - ffPrev) %>% 
  glimpse()

prevMean <- mean(prevDist$newPrev) # 40,870,250
prevq25 <- quantile(prevDist$newPrev, probs = 0.01) # 39,672,950
prevq99 <- quantile(prevDist$newPrev, probs = 0.995) # 42,147,882
ggplot(prevDist, aes(x = newPrev)) + 
  geom_density(color = 'navy', fill = 'blue', alpha = 0.25) +
  theme_minimal() +
  geom_vline(xintercept = prevMean, size = 1.25) +
  geom_vline(xintercept = prevq25, color = 'navy', size = 1.25) + 
  geom_vline(xintercept = prevq99, color = 'red', size = 1.25) +
  xlab('Previsión USD') +
  ylab('Densidad') +
  scale_x_continuous(label=comma) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(panel.grid = element_blank()) +
  annotate("text", x=40700000, y=6E-7, label= "40.9M") +
  annotate("text", x=42300000, y=6E-7, label= "42.1M", color = 'red')+
  annotate("text", x=39800000, y=4E-7, label= "39.7M", color = 'blue')


prevMean <- mean(prevDist$netFlow) # 18,923,275
prevq25 <- quantile(prevDist$netFlow, probs = 0.01) # 17,544,666
prevq99 <- quantile(prevDist$netFlow, probs = 0.995) # 20,394,176
ggplot(prevDist, aes(x = netFlow)) + 
  geom_density(color = 'navy', fill = 'blue', alpha = 0.25) +
  theme_minimal() +
  geom_vline(xintercept = prevMean, size = 1.25) +
  geom_vline(xintercept = prevq25, color = 'navy', size = 1.25) + 
  geom_vline(xintercept = prevq99, color = 'red', size = 1.25) +
  xlab('Previsión USD') +
  ylab('Densidad') +
  scale_x_continuous(label=comma) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(panel.grid = element_blank()) +
  annotate("text", x=23200000, y=7E-7, label= "23.3M") +
  annotate("text", x=24600000, y=6E-7, label= "24.7M", color = 'red')+
  annotate("text", x=22100000, y=4E-7, label= "22.2M", color = 'blue')

# Excel table for letter
prevMean <- mean(prevDist$newPrev) # 40,870,250
prevTiles <- as_tibble(quantile(prevDist$newPrev, probs = c(0.5, 0.9, 0.95, 0.99, 0.995)))
prev90 <- prevTiles$value[2]
prev95 <- prevTiles$value[3]
prev99 <- prevTiles$value[4]
prev995 <- prevTiles$value[5]
#--------
calDist <- read.csv('D:/!bso/transMat/dpprev_12avg.csv') %>% 
  dplyr::rename(sumPrevius = previus,
                nPrevius = previus.1,
                sumNewPrev = newPrev,
                nNewPrev = newPrev.1,
                sumDifPrev = difPrev,
                nDifPrev = difPrev.1,
                calif = X,
                califPot = X.1) %>% 
  glimpse()

calDist <- calDist[-1,]
calDist <- calDist[-1,]
calDist <- calDist %>% 
  mutate(sumPrevius = as.numeric(sumPrevius),
                  nPrevius =  as.numeric(nPrevius),
                  sumNewPrev =  as.numeric(sumNewPrev),
                  nNewPrev =  as.numeric(nNewPrev),
                  sumDifPrev =  as.numeric(sumDifPrev),
                  nDifPrev =  as.numeric(nDifPrev)) %>% 
  mutate(trans = paste0(calif, califPot)) %>% 
  group_by(trans) %>% 
  mutate(iter = row_number()) %>% 
  arrange(trans, iter) %>% 
  ungroup() %>% 
  glimpse()

meanDist <- calDist %>%
  select(-starts_with('calif'), -iter) %>% 
  group_by(trans) %>% 
  summarise_all(mean) %>% 
  glimpse()
write.xlsx(meanDist, 'D:/!bso/transMat/meanDist_avg.xlsx')

npDist <- calDist %>% 
  select(iter, sumDifPrev) %>% 
  group_by(iter) %>% 
  summarise_all(sum) %>% 
  mutate(Escenario = 'MT Promedio') %>% 
  glimpse()

prevMean <- mean(npDist$sumDifPrev) # 19,399,459
prevq25 <- quantile(npDist$sumDifPrev, probs = 0.01) # 18,262,359
prevq99 <- quantile(npDist$sumDifPrev, probs = 0.995) # 20,755,643

ggplot(npDist, aes(x = sumDifPrev)) + 
  geom_density(color = 'navy', fill = 'blue', alpha = 0.25) +
  theme_minimal() +
  geom_vline(xintercept = prevMean, size = 1.25) +
  geom_vline(xintercept = prevq25, color = 'navy', size = 1.25) + 
  geom_vline(xintercept = prevq99, color = 'red', size = 1.25) +
  xlab('Dif. Previsión USD') +
  ylab('Densidad') +
  scale_x_continuous(label=comma, breaks = seq(17500000, 21500000, 1000000)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(panel.grid = element_blank()) +
  annotate("text", x=19200000, y=8E-7, label= "19.4M") +
  annotate("text", x=20900000, y=6E-7, label= "20.7M", color = 'red')+
  annotate("text", x=18400000, y=4E-7, label= "18.2M", color = 'blue')
ggsave('D:/!bso/transMat/difPrevDist_avg12.png')

write.xlsx(calDist, 'D:/!bso/transMat/calDist_avg.xlsx')
#-------
# stressed scenarios
# worst castigo by 20%
calDistw20 <- read.csv('D:/!bso/transMat/dpprev_12sensW20.csv') %>% 
  dplyr::rename(sumPrevius = previus,
                nPrevius = previus.1,
                sumNewPrev = newPrev,
                nNewPrev = newPrev.1,
                sumDifPrev = difPrev,
                nDifPrev = difPrev.1,
                calif = X,
                califPot = X.1) %>% 
  glimpse()

calDistw20 <- calDistw20[-1,]
calDistw20 <- calDistw20[-1,]
calDistw20 <- calDistw20 %>% 
  mutate(sumPrevius = as.numeric(sumPrevius),
         nPrevius =  as.numeric(nPrevius),
         sumNewPrev =  as.numeric(sumNewPrev),
         nNewPrev =  as.numeric(nNewPrev),
         sumDifPrev =  as.numeric(sumDifPrev),
         nDifPrev =  as.numeric(nDifPrev)) %>% 
  mutate(trans = paste0(calif, califPot)) %>% 
  group_by(trans) %>% 
  mutate(iter = row_number()) %>% 
  arrange(trans, iter) %>% 
  ungroup() %>% 
  glimpse()

meanDistw20 <- calDistw20 %>%
  select(-starts_with('calif'), -iter) %>% 
  group_by(trans) %>% 
  summarise_all(mean) %>% 
  glimpse()
write.xlsx(meanDistw20, 'D:/!bso/transMat/meanDist_W20.xlsx')

npDistw20 <- calDistw20 %>% 
  select(iter, sumDifPrev) %>% 
  group_by(iter) %>% 
  summarise_all(sum) %>% 
  mutate(Escenario = '+20% Castigo desde F') %>% 
  glimpse()

prevMean <- mean(npDistw20$sumDifPrev) # 19,399,459
prevq25 <- quantile(npDistw20$sumDifPrev, probs = 0.01) # 18,262,359
prevq99 <- quantile(npDistw20$sumDifPrev, probs = 0.995) # 20,755,643

dens <- npDist %>% 
  bind_rows(npDistw20) %>% 
  glimpse()

ggplot(dens, aes(x = sumDifPrev, fill = Escenario)) + 
  geom_density(alpha = 0.45) +
  theme_minimal() +
  geom_vline(xintercept = prevMean, size = 1.25) +
  geom_vline(xintercept = prevq25, color = 'navy', size = 1.25) + 
  geom_vline(xintercept = prevq99, color = 'red', size = 1.25) +
  xlab('Dif. Previsión USD') +
  ylab('Densidad') +
  scale_x_continuous(label=comma, breaks = seq(17500000, 23500000, 1000000)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(panel.grid = element_blank(),
        legend.position = 'bottom') +
  scale_fill_brewer(palette = 'Set1') +
  annotate("text", x=20200000, y=8E-7, label= "20.4M") +
  annotate("text", x=21200000, y=6E-7, label= "21.4M", color = 'red')+
  annotate("text", x=19200000, y=4E-7, label= "19.4M", color = 'blue')
ggsave('D:/!bso/transMat/difPrevDist_sens20.png')

# better ab
calDistab <- read.csv('D:/!bso/transMat/dpprev_12sensab2.csv') %>% 
  dplyr::rename(sumPrevius = previus,
                nPrevius = previus.1,
                sumNewPrev = newPrev,
                nNewPrev = newPrev.1,
                sumDifPrev = difPrev,
                nDifPrev = difPrev.1,
                calif = X,
                califPot = X.1) %>% 
  glimpse()

calDistab <- calDistab[-1,]
calDistab <- calDistab[-1,]
calDistab <- calDistab %>% 
  mutate(sumPrevius = as.numeric(sumPrevius),
         nPrevius =  as.numeric(nPrevius),
         sumNewPrev =  as.numeric(sumNewPrev),
         nNewPrev =  as.numeric(nNewPrev),
         sumDifPrev =  as.numeric(sumDifPrev),
         nDifPrev =  as.numeric(nDifPrev)) %>% 
  mutate(trans = paste0(calif, califPot)) %>% 
  group_by(trans) %>% 
  mutate(iter = row_number()) %>% 
  arrange(trans, iter) %>% 
  ungroup() %>% 
  glimpse()

meanDistab <- calDistab %>%
  select(-starts_with('calif'), -iter) %>% 
  group_by(trans) %>% 
  summarise_all(mean) %>% 
  glimpse()
write.xlsx(meanDistab, 'D:/!bso/transMat/meanDist_ab2.xlsx')

npDistab <- calDistab %>% 
  select(iter, sumDifPrev) %>% 
  group_by(iter) %>% 
  summarise_all(sum) %>% 
  mutate(Escenario = '+0.05% de permanencia en A') %>% 
  glimpse()

prevMean <- mean(npDistab$sumDifPrev) # 19,399,459
prevq25 <- quantile(npDistab$sumDifPrev, probs = 0.01) # 18,262,359
prevq99 <- quantile(npDistab$sumDifPrev, probs = 0.995) # 20,755,643

dens <- npDistab %>% 
  glimpse()

ggplot(dens, aes(x = sumDifPrev)) + 
  geom_density(alpha = 0.45, fill = 'navy') +
  theme_minimal() +
  geom_vline(xintercept = prevMean, size = 1.25) +
  geom_vline(xintercept = prevq25, color = 'navy', size = 1.25) + 
  geom_vline(xintercept = prevq99, color = 'red', size = 1.25) +
  xlab('Dif. Previsión USD') +
  ylab('Densidad') +
  scale_x_continuous(label=comma, breaks = seq(11000000, 17000000, 1000000)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(panel.grid = element_blank(),
        legend.position = 'bottom') +
  annotate("text", x=14100000, y=10E-7, label= "14.2M") +
  annotate("text", x=15300000, y=6E-7, label= "15.4M", color = 'red')+
  annotate("text", x=13400000, y=4E-7, label= "13.2M", color = 'blue')
ggsave('D:/!bso/transMat/difPrevDist_sensab.png')

# worst ab by 0.5%
calDistwab <- read.csv('D:/!bso/transMat/dpprev_12sensWab.csv') %>% 
  dplyr::rename(sumPrevius = previus,
                nPrevius = previus.1,
                sumNewPrev = newPrev,
                nNewPrev = newPrev.1,
                sumDifPrev = difPrev,
                nDifPrev = difPrev.1,
                calif = X,
                califPot = X.1) %>% 
  glimpse()

calDistwab <- calDistwab[-1,]
calDistwab <- calDistwab[-1,]
calDistwab <- calDistwab %>% 
  mutate(sumPrevius = as.numeric(sumPrevius),
         nPrevius =  as.numeric(nPrevius),
         sumNewPrev =  as.numeric(sumNewPrev),
         nNewPrev =  as.numeric(nNewPrev),
         sumDifPrev =  as.numeric(sumDifPrev),
         nDifPrev =  as.numeric(nDifPrev)) %>% 
  mutate(trans = paste0(calif, califPot)) %>% 
  group_by(trans) %>% 
  mutate(iter = row_number()) %>% 
  arrange(trans, iter) %>% 
  ungroup() %>% 
  glimpse()

meanDistwab <- calDistwab %>%
  select(-starts_with('calif'), -iter) %>% 
  group_by(trans) %>% 
  summarise_all(mean) %>% 
  glimpse()
write.xlsx(meanDistwab, 'D:/!bso/transMat/meanDist_Wab.xlsx')

npDistwab <- calDistwab %>% 
  select(iter, sumDifPrev) %>% 
  group_by(iter) %>% 
  summarise_all(sum) %>% 
  mutate(Escenario = '-0.05% de permanencia en A') %>% 
  glimpse()

prevMean <- mean(npDistwab$sumDifPrev) # 19,399,459
prevq25 <- quantile(npDistwab$sumDifPrev, probs = 0.01) # 18,262,359
prevq99 <- quantile(npDistwab$sumDifPrev, probs = 0.995) # 20,755,643


dens <- npDist %>% 
  bind_rows(npDistw20) %>% 
  bind_rows(npDistwab) %>% 
  glimpse()

ggplot(dens, aes(x = sumDifPrev, fill = Escenario)) + 
  geom_density(alpha = 0.45) +
  theme_minimal() +
  geom_vline(xintercept = prevMean, size = 1.25, color = 'red') +
  geom_vline(xintercept = 19400000, color = 'green', size = 1.25) + 
  geom_vline(xintercept = 20500000, color = 'blue', size = 1.25) +
  xlab('Dif. Previsión USD') +
  ylab('Densidad') +
  scale_x_continuous(label=comma, breaks = seq(17500000, 26000000, 1000000)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(panel.grid = element_blank(),
        legend.position = 'bottom') +
  scale_fill_brewer(palette = 'Set1') +
  annotate("text", x=20200000, y=8E-7, label= "20.4M") +
  annotate("text", x=19400000, y=8E-7, label= "19.4M") +
  annotate("text", x=24200000, y=7E-7, label= "24.5M", color = 'red')
ggsave('D:/!bso/transMat/difPrevDist_sensAll.png')

#===============================================================================
# Aux figures
# average
gph <- calDist %>% 
  group_by(trans, calif, califPot) %>% 
  summarise_all(mean) %>% 
  dplyr::filter(sumDifPrev != 0)
ggplot(gph, aes(x = trans, y = sumDifPrev)) +
  geom_bar(stat = 'identity', fill = 'navy') + theme_minimal() +
  scale_y_continuous(label = comma, breaks = seq(-5000000,15000000,2500000)) + 
  xlab('Transición') + 
  ylab('Dif. Previsión (USD)') + geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(size = 6),
        panel.grid = element_blank())
ggsave('D:/!bso/transMat/avgProvisionFlow.png')
write.csv(gph, 'D:/!bso/transMat/calDist_avg.csv')
# worst 20
calDistw20 <- read.csv('D:/!bso/transMat/dpprev_12sensW20.csv') %>% 
  dplyr::rename(sumPrevius = previus,
                nPrevius = previus.1,
                sumNewPrev = newPrev,
                nNewPrev = newPrev.1,
                sumDifPrev = difPrev,
                nDifPrev = difPrev.1,
                calif = X,
                califPot = X.1) %>% 
  glimpse()

calDistw20 <- calDistw20[-1,]
calDistw20 <- calDistw20[-1,]
calDistw20 <- calDistw20 %>% 
  mutate(sumPrevius = as.numeric(sumPrevius),
         nPrevius =  as.numeric(nPrevius),
         sumNewPrev =  as.numeric(sumNewPrev),
         nNewPrev =  as.numeric(nNewPrev),
         sumDifPrev =  as.numeric(sumDifPrev),
         nDifPrev =  as.numeric(nDifPrev)) %>% 
  mutate(trans = paste0(calif, califPot)) %>% 
  group_by(trans) %>% 
  mutate(iter = row_number()) %>% 
  arrange(iter, trans) %>% 
  ungroup() %>% 
  glimpse()

gph <- calDist %>% 
  select(trans, sumDifPrev) %>% 
  group_by(trans) %>% 
  summarise_all(mean) %>% 
  dplyr::filter(sumDifPrev != 0) %>% 
  mutate(Escenario = 'MT Promedio')

gphw20 <- calDistw20 %>% 
  select(trans, sumDifPrev) %>% 
  group_by(trans) %>% 
  summarise_all(mean) %>% 
  dplyr::filter(sumDifPrev != 0) %>% 
  mutate(Escenario = '+20% Castigo desde F')

gph2 <- gph %>% 
  bind_rows(gphw20)

ggplot(gph2, aes(x = trans, y = sumDifPrev, fill = Escenario)) +
  geom_bar(stat = 'identity', position = 'dodge') + theme_minimal() +
  scale_y_continuous(label = comma, breaks = seq(-5000000,15000000,2500000))  +
  xlab('Transición') + 
  ylab('Dif. Previsión (USD)') + geom_hline(yintercept = 0) +
  scale_fill_manual(values = cbp3)+
  theme(axis.text.x = element_text(size = 6),
        panel.grid = element_blank(),
        legend.position = 'bottom')
ggsave('D:/!bso/transMat/w20ProvisionFlow.png')

gphw20 <- calDistw20 %>% 
  group_by(trans) %>% 
  summarise_all(mean) 
write.csv(gphw20, 'D:/!bso/transMat/calDist_w20.csv')

# better ab
calDistab <- read.csv('D:/!bso/transMat/dpprev_12sensab.csv') %>% 
dplyr::rename(sumPrevius = previus,
              nPrevius = previus.1,
              sumNewPrev = newPrev,
              nNewPrev = newPrev.1,
              sumDifPrev = difPrev,
              nDifPrev = difPrev.1,
              calif = X,
              califPot = X.1) %>% 
  glimpse()

calDistab <- calDistab[-1,]
calDistab <- calDistab[-1,]
calDistab <- calDistab %>% 
  mutate(sumPrevius = as.numeric(sumPrevius),
         nPrevius =  as.numeric(nPrevius),
         sumNewPrev =  as.numeric(sumNewPrev),
         nNewPrev =  as.numeric(nNewPrev),
         sumDifPrev =  as.numeric(sumDifPrev),
         nDifPrev =  as.numeric(nDifPrev)) %>% 
  mutate(trans = paste0(calif, califPot)) %>% 
  group_by(trans) %>% 
  mutate(iter = row_number()) %>% 
  arrange(iter, trans) %>% 
  ungroup() %>% 
  glimpse()

gphab <- calDistab %>% 
  select(trans, sumDifPrev) %>% 
  group_by(trans) %>% 
  summarise_all(mean) %>% 
  dplyr::filter(sumDifPrev != 0) %>% 
  mutate(Escenario = '+0.0005% Permanencia en A')

gph3 <- gph %>% 
  bind_rows(gphw20) %>% 
  bind_rows(gphab) %>% 
  dplyr::filter(substr(trans, 1,1) == 'A')

ggplot(gph3, aes(x = trans, y = sumDifPrev, fill = Escenario)) +
  geom_bar(stat = 'identity', position = 'dodge') + theme_minimal() +
  scale_y_continuous(label = comma, breaks = seq(-5000000,15000000,2500000))  +
  xlab('Transición') + 
  ylab('Dif. Previsión (USD)') + geom_hline(yintercept = 0) +
   scale_fill_manual(values = cbp3)+
  theme(axis.text.x = element_text(size = 14),
        panel.grid = element_blank(),
        legend.position = 'bottom')
ggsave('D:/!bso/transMat/abProvisionFlow.png')

gphab <- calDistab %>% 
  group_by(trans) %>% 
  summarise_all(mean) 
write.csv(gphab, 'D:/!bso/transMat/calDist_ab.csv')
