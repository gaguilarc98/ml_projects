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

################################################################################
# Data in
bdcList <- list()
file_list <- list.files(path='C:/!bso/girCartera/rds')
for (i in 1:length(file_list)) {
  
  #print(file_list[i])
  bdc <- readRDS(paste0('C:/!bso/girCartera/rds/',
                        file_list[i])) %>% 
    select(CI, saldous, previus, mon, year, mes, dayDate, fbase, GENERO)
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
  arrange(CI, GENERO) 
#quietly(gc())
bdcList <- NULL
nrowFull <- nrow(bdcFull)
nopsFull <- length(unique(bdcFull$OPERACION))
ncliFull <- length(unique(bdcFull$CI))
write_rds(bdcFull, 'C:/!bso/califClientes/output/bdcFull_tabla.rds')


bdcBSO_full <- readRDS('C:/!bso/califClientes/output/bdcFull_tabla.rds') %>% 
  dplyr::filter(monDate<2023)

long_list <- c('BSO202208_utf8.txt', 'BSO202209_utf8.txt', 'BSO202210_utf8.txt', 'BSO202211_utf8.txt', 'BSO202212_utf8.txt' )

# Lista de resultados intermedios
resList_1<- list()

# Loop sobre bases de infocred
for(i in 1:length(long_list)) {
  #print(long_list[i])
  infoRaw <- fread(paste0('C:/!bso/Cargamensual_infocred/utf/', long_list[i]),  encoding = 'UTF-8', fill = T)
  nrowInfo <- nrow(infoRaw)
  
  infoCheck <- infoRaw %>% 
    mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
           saldo = `SBEF VIGENTE` + `SBEF VENCIDO` + `SBEF EJECUCION`,
           saldoMora = `SBEF VENCIDO` + `SBEF EJECUCION`, 
           saldoMMC = `SBEF VENCIDO` + `SBEF EJECUCION` + `SBEF CASTIGADO`) %>%
    select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`,
           `ENTIDAD SBEF`,  `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
           MontoOriginal, MonedaOrigen, `SBEF CALIFICACION`, `FR CALIFICACION`,
           saldo, saldoMora, saldoMMC) %>%
    #dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A -')) %>% 
    #dplyr::filter(`SIGLA SBEF` != 'BSO') %>% # check filter
    mutate(MontoOriginal = ifelse(MonedaOrigen == 'MN', as.numeric(MontoOriginal)/6.86,  as.numeric(MontoOriginal))) %>% 
    mutate(saldoVig = ifelse(MonedaOrigen == 'MN',  as.numeric(`SBEF VIGENTE`)/6.86,   as.numeric(`SBEF VIGENTE`))) %>% 
    mutate(saldo = ifelse(MonedaOrigen == 'MN',  saldo/6.86, saldo)) %>%
    mutate(saldoMora = ifelse(MonedaOrigen == 'MN',  saldoMora/6.86, saldoMora)) %>% 
    mutate(fbase=substr(long_list[i], 4,9)) %>% 
    mutate(Mes=as.yearmon(as.Date(paste0(fbase, "01"), "%Y%m%d")))
  resList_1[[i]] <- infoCheck
}

# Apilación de lista en un único DF
infoFull <-bind_rows(resList_1)

# Almacenado en disco de DF con datos de Infocred para todas las bases procesadas
write_rds(infoFull, 'C:/!bso/califClientes/output/infoFull.rds')

bdcBSO <- bdcBSO_full %>% 
  select(CI, saldous, monDate, previus, GENERO) %>% 
  group_by(monDate, CI, GENERO) %>% 
  summarise_all(sum, na.rm=T) %>% 
  dplyr::rename(Fecha=monDate)

write_rds(bdcBSO, 'C:/!bso/califClientes/bdcBSO.rds')
bdcBSO<-readRDS('C:/!bso/califClientes/bdcBSO.rds')


bdcBSO_tabcli <- bdcBSO %>% 
  group_by(Fecha) %>% 
  select(-saldous) %>% 
  summarise_all(n_distinct)

infoFull <- readRDS('D:/!bso/califClientes/infoFull.rds')

infoPerf_1 <- infoFull %>% 
  select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`,
         `ENTIDAD SBEF`,  `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
         MontoOriginal, MonedaOrigen, `SBEF CALIFICACION`, `FR CALIFICACION`, fbase) %>%
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% # buscar solamente tipos de DEUDOR
  #dplyr::filter(`SIGLA SBEF` != 'BSO') %>% # check filter
  mutate(MontoOriginal = ifelse(MonedaOrigen == 'MN', as.numeric(MontoOriginal)/6.86, as.numeric(MontoOriginal))) %>% 
  mutate(saldoVig = ifelse(MonedaOrigen == 'MN',  as.numeric(`SBEF VIGENTE`)/6.86,   as.numeric(`SBEF VIGENTE`))) %>% 
  mutate(histStr = as.character(HISTORICO)) %>% # HISTORY CONSTRUCTION
  arrange(CI) %>%
  ungroup() %>% 
  group_by(CI, fbase) %>% # Agrupación por CI y fecha para establecer máximos
  dplyr::filter(max(row_number()) > 1) %>% # Filtro para quedar solamente con compartidos
  mutate(califBSO = ifelse(`SIGLA SBEF` == 'BSO', `SBEF CALIFICACION`, '_'),
         califBSO_2 = max(califBSO, na.rm = T),
         distCalif = n_distinct(`SBEF CALIFICACION`)) %>% # Máx. calificación en BSO
  dplyr::filter(califBSO_2 != '_') %>% 
  select(-califBSO) %>% 
  mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
         califSF_2 = max(califSF, na.rm = T)) %>% # Máx. calificación en SF
  dplyr::filter(califSF_2 != '_') %>% 
  select(-califSF) %>% 
  #dplyr::filter(CI == '10020373LP') # Just an example for garante
  ungroup() %>% 
  select(CI, califBSO_2, califSF_2, fbase) %>% 
  group_by(CI, fbase) %>%
  summarise_all(max) %>% 
  ungroup() %>% 
  select(califBSO_2, califSF_2, fbase) %>% 
  group_by(califBSO_2, califSF_2, fbase) %>% # Agrupamos por calificaciones para hacer el recuento 
  tally() %>% # Creamos el recuento
  pivot_wider(names_from = califSF_2, values_from = n) %>% # Ponemos calif en el SF en las columnas
  adorn_totals('row', 'col') %>% # Acá creamos mejor/igual/peor calificación
  mutate(igualCalif=case_when(califBSO_2 == 'A'~ A,
                              califBSO_2 == 'B'~ B,
                              califBSO_2 == 'C'~ C,
                              califBSO_2 == 'D'~ D,
                              califBSO_2 == 'E'~ E,
                              califBSO_2 == 'F'~ `F`)) %>% 
  
  mutate(peorCalif=case_when(califBSO_2 == 'A'~ (B+C+D+E+`F`),
                             califBSO_2 == 'B'~ (C+D+E+`F`),
                             califBSO_2 == 'C'~ (D+E+`F`),
                             califBSO_2 == 'D'~ (E+`F`),
                             califBSO_2 == 'E'~ (`F`),)) %>% 
  
  mutate(mejorCalif=case_when(califBSO_2 == 'B'~ (A),
                              califBSO_2 == 'C'~ (A+B),
                              califBSO_2 == 'D'~ (A+B+C),
                              califBSO_2 == 'E'~ (A+B+C+D),
                              califBSO_2 == 'F'~ (A+B+C+D+E))) %>% 
  
  mutate(TotalFila=(A+B+C+D+E+`F`)) %>%
  rowwise() %>% 
  mutate(TotalCalif= sum(c(igualCalif, peorCalif, mejorCalif), na.rm = T)) %>% 
  mutate(across(A:TotalCalif, ~replace_na(. , 0))) %>% 
  mutate(Fecha=as.yearmon(as.Date(paste0(fbase, "01"), "%Y%m%d"))) %>% 
  #select(-fbase) %>% 
  relocate(Fecha) 

head(infoPerf_1, n = 12)

tabClientes_a <- infoPerf_1 %>%
  select(Fecha, starts_with('peor'), starts_with('mejor'), starts_with('igual'),
         TotalCalif) %>%
  group_by(Fecha) %>% 
  summarise(mejorCalif = sum(mejorCalif), igualCalif = sum(igualCalif),
            peorCalif = sum(peorCalif), totalCalif = sum(TotalCalif)) %>% 
  left_join(bdcBSO_tabcli, by = 'Fecha')%>% 
  mutate(pct_SF_BSO = totalCalif/CI) %>% 
  dplyr::rename(Mes = Fecha,
                `Mejor en SF` = mejorCalif,
                `Peor en SF` = peorCalif,
                `Misma Calificación (A)` = igualCalif,
                `Total Clientes BSO` = CI,
                `% Clientes Compartidos` = pct_SF_BSO,
                `Total Compartidos` = totalCalif) %>% 
  dplyr::filter(!is.na(Mes)) %>% 
  select(Mes, `Total Compartidos`, `% Clientes Compartidos`)

# Tabla final para publicación
tabClientes_final <- tabClientes %>% 
  left_join(tabClientes_a, by = 'Mes') %>% 
  mutate(`% Clientes A en BSO y SF` = `Misma Calificación (A)`/`Misma Calificación`) %>% 
  relocate(Mes,`Misma Calificación`, `Misma Calificación (A)`, `% Clientes A en BSO y SF`,
           `Mejor en SF`, `Peor en SF`, `Total Compartidos`, `Total Clientes BSO`,
           `% Clientes Compartidos`) %>% 
  mutate(across(2:3, ~formatC(., format = 'd', big.mark = ','))) %>% 
  mutate(across(5:8, ~formatC(., format = 'd', big.mark = ',')))




infoPerf_2 <- infoFull %>% 
  select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`,
         `ENTIDAD SBEF`,  `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
         MontoOriginal, MonedaOrigen, `SBEF CALIFICACION`, `FR CALIFICACION`,
         saldo, saldoMora, saldoMMC, fbase) %>% # entra saldo al select
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% 
  mutate(Fecha=as.yearmon(as.Date(paste0(fbase, "01"), "%Y%m%d"))) %>% 
  select(-fbase) %>% 
  left_join(bdcBSO, by = c('CI', 'Fecha')) 
infoPerf_3<-infoPerf_2 %>% 
  mutate(MontoOriginal = ifelse(MonedaOrigen == 'MN', as.numeric(MontoOriginal)/6.86, as.numeric(MontoOriginal))) %>% 
  mutate(saldoVig = ifelse(MonedaOrigen == 'MN',  as.numeric(`SBEF VIGENTE`)/6.86,   as.numeric(`SBEF VIGENTE`))) %>% 
  mutate(saldo = ifelse(MonedaOrigen == 'MN',  saldo/6.86, saldo)) %>%
  mutate(saldoMora = ifelse(MonedaOrigen == 'MN',  saldoMora/6.86, saldoMora)) %>% 
  mutate(histStr = as.character(HISTORICO)) %>% # HISTORY CONSTRUCTION
  arrange(CI) %>% 
  ungroup() %>% 
  group_by(CI, Fecha, GENERO) %>% # Agrupación por CI y fecha
  dplyr::filter(max(row_number()) > 1) %>% # filtro para quedar solo con clientes compartidos
  mutate(saldoBSO = ifelse(`SIGLA SBEF` == 'BSO', saldo, 0),
         saldoBSO_2 = sum(saldoBSO, na.rm = T)) %>% 
  select(-saldoBSO) %>% # saldo en bso
  mutate(saldoMoraBSO = ifelse(`SIGLA SBEF` == 'BSO', saldoMora, 0),
         saldoMoraBSO_2 = sum(saldoMoraBSO, na.rm = T)) %>% 
  select(-saldoMoraBSO) %>% # saldo en mora en BSO
  mutate(saldoMMCBSO = ifelse(`SIGLA SBEF` == 'BSO', saldoMMC, 0),
         saldoMMCBSO_2 = sum(saldoMMCBSO, na.rm = T)) %>% 
  select(-saldoMMCBSO) %>% # saldo en mora + castigado en BSO
  mutate(saldoSF = ifelse(`SIGLA SBEF` != 'BSO', saldo, 0),
         saldoSF_2 = sum(saldoSF, na.rm = T)) %>% 
  select(-saldoSF) %>% # saldo en SF
  mutate(saldoMoraSF = ifelse(`SIGLA SBEF` != 'BSO', saldoMora, 0),
         saldoMoraSF_2 = sum(saldoMoraSF, na.rm = T)) %>% 
  select(-saldoMoraSF) %>% # saldo en mora en SF
  mutate(saldoMMCSF = ifelse(`SIGLA SBEF` != 'BSO', saldoMMC, 0),
         saldoMMCSF_2 = sum(saldoMMCSF, na.rm = T)) %>% 
  select(-saldoMMCSF) %>% # saldo en mora + castigado en SF
  mutate(califBSO = ifelse(`SIGLA SBEF` == 'BSO', `SBEF CALIFICACION`, '_'),
         califBSO_2 = max(califBSO, na.rm = T)) %>% # Max. calificación en BSO
  dplyr::filter(califBSO_2 != '_') %>% 
  select(-califBSO) %>%
  mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
         califSF_2 = max(califSF, na.rm = T)) %>% # Max calificación en SF
  dplyr::filter(califSF_2 != '_') %>% 
  select(-califSF) %>% 
  ungroup() %>% 
  select(CI, starts_with('saldo'), califBSO_2, califSF_2, Fecha, GENERO) %>% # entra saldo al select
  group_by(CI, califBSO_2, califSF_2, Fecha, GENERO) %>% #Este es el group_by importante
  summarise_all(max) %>% 
  ungroup() %>% 
  select(-CI) %>% 
  group_by(califBSO_2, califSF_2, Fecha, GENERO) %>% 
  summarise_all(sum, na.rm = T) %>% # ya no es recuento, es suma
  select(califBSO_2, califSF_2, saldo, saldoMora, Fecha, saldous, GENERO) %>% 
  relocate(Fecha) %>% 
  pivot_wider(names_from = califSF_2, values_from = c('saldo', 'saldoMora', 'saldous')) %>% 
  mutate(across(saldo_A:saldoMora_F, ~replace_na(. , 0))) %>% 
  mutate(igualCalif=case_when(califBSO_2 == 'A'~ saldo_A,
                              califBSO_2 == 'B'~ saldo_B,
                              califBSO_2 == 'C'~ saldo_C,
                              califBSO_2 == 'D'~ saldo_D,
                              califBSO_2 == 'E'~ saldo_E,
                              califBSO_2 == 'F'~ saldo_F)) %>% 
  
  mutate(peorCalif=case_when(califBSO_2 == 'A'~ (saldo_B+saldo_C+saldo_D+saldo_E+saldo_F),
                             califBSO_2 == 'B'~ (saldo_C+saldo_D+saldo_E+saldo_F),
                             califBSO_2 == 'C'~ (saldo_D+saldo_E+saldo_F),
                             califBSO_2 == 'D'~ (saldo_E+saldo_F),
                             califBSO_2 == 'E'~ (saldo_F),)) %>% 
  
  mutate(mejorCalif=case_when(califBSO_2 == 'B'~ (saldo_A),
                              califBSO_2 == 'C'~ (saldo_A+saldo_B),
                              califBSO_2 == 'D'~ (saldo_A+saldo_B+saldo_C),
                              califBSO_2 == 'E'~ (saldo_A+saldo_B+saldo_C+saldo_D),
                              califBSO_2 == 'F'~ (saldo_A+saldo_B+saldo_C+saldo_D+saldo_E))) %>% 
  

  mutate(TotalFila=(saldo_A+saldo_B+saldo_C+saldo_D+saldo_E+saldo_F)) %>%
  rowwise() %>% 
  mutate(TotalCalif= sum(c(igualCalif, peorCalif, mejorCalif), na.rm = T)) %>% 
  # con saldous
  mutate(igualCalif_us=case_when(califBSO_2 == 'A'~ saldous_A,
                                 califBSO_2 == 'B'~ saldous_B,
                                 califBSO_2 == 'C'~ saldous_C,
                                 califBSO_2 == 'D'~ saldous_D,
                                 califBSO_2 == 'E'~ saldous_E,
                                 califBSO_2 == 'F'~ saldous_F)) %>% 
  
  mutate(peorCalif_us=case_when(califBSO_2 == 'A'~ (saldous_B+saldous_C+saldous_D+saldous_E+saldous_F),
                                califBSO_2 == 'B'~ (saldous_C+saldous_D+saldous_E+saldous_F),
                                califBSO_2 == 'C'~ (saldous_D+saldous_E+saldous_F),
                                califBSO_2 == 'D'~ (saldous_E+saldous_F),
                                califBSO_2 == 'E'~ (saldous_F),)) %>% 
  
  mutate(mejorCalif_us=case_when(califBSO_2 == 'B'~ (saldous_A),
                                 califBSO_2 == 'C'~ (saldous_A+saldous_B),
                                 califBSO_2 == 'D'~ (saldous_A+saldous_B+saldous_C),
                                 califBSO_2 == 'E'~ (saldous_A+saldous_B+saldous_C+saldous_D),
                                 califBSO_2 == 'F'~ (saldous_A+saldous_B+saldous_C+saldous_D+saldous_E))) %>% 

  mutate(TotalFila_us=(saldous_A+saldous_B+saldous_C+saldous_D+saldous_E+saldous_F)) %>%
  rowwise() %>% 
  mutate(TotalCalif_us= sum(c(igualCalif_us, peorCalif_us, mejorCalif_us), na.rm = T)) %>%
  
  mutate(across(saldo_A:TotalCalif_us, ~replace_na(. , 0))) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  mutate(totalSaldo = sum(TotalCalif)) %>% 
  mutate(totalSaldo_us = sum(TotalCalif_us)) %>% 
  ungroup() %>% 
  mutate(across(starts_with('saldo'), ~formatC(., format = 'd', big.mark = ','))) 
head(infoPerf_2, n = 12)

tabSaldo <- infoPerf_2 %>%
  select(Fecha, starts_with('peor'), starts_with('mejor'), starts_with('igual'),
         starts_with('totalS')) %>%
  left_join(bdcBSO_tab, by = 'Fecha') %>% 
  group_by(Fecha) %>% 
  summarise(mejorCalif = sum(mejorCalif), igualCalif = sum(igualCalif),
            peorCalif = sum(peorCalif), mejorCalif_us = sum(mejorCalif_us), 
            igualCalif_us = sum(igualCalif_us), peorCalif_us = sum(peorCalif_us),
            totalSaldo = mean(totalSaldo), totalSaldo_us = mean(totalSaldo_us)) %>% 
  mutate(pct_SF_BSO = totalSaldo/totalSaldo_us,
         pct_SF_totalBSO = totalSaldo/saldous,
         pct_BSO_totalBSO = totalSaldo_us/saldous) %>% 
  select(Fecha, starts_with('pct'), ends_with('_us'), saldous) %>% 
  dplyr::rename(Mes = Fecha,
                `Mejor en SF` = mejorCalif_us,
                `Peor en SF` = peorCalif_us,
                `Misma Calificación` = igualCalif_us,
                `Total Saldo USD BSO` = saldous,
                `% Saldo Compartido` = pct_SF_BSO,
                `Total Saldo Compartido` = totalSaldo_us) %>% 
  dplyr::filter(!is.na(Mes)) %>% 
  relocate(Mes,`Misma Calificación`, 
           `Mejor en SF`, `Peor en SF`, `Total Saldo Compartido`, `Total Saldo USD BSO`,
           `% Saldo Compartido`)