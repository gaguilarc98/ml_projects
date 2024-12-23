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
library(ggplot2)
library(fastDummies)
library(openxlsx)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
################################################################################
#####____READING BDCBSO____####
list <- c("ec_Ago2022.rds","ec_Sep2022.rds","ec_Oct2022.rds","ec_Nov2022.rds","ec_Dic2022.rds")
list <- c("Ago2022.txt","Sep2022.txt","Oct2022.txt","Nov2022.txt","Dic2022.txt")
bdcList <- list()
for (i in 1:length(list)) {
  bdcBSO <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
                           list[i]),encoding = 'Latin-1', fill = T) %>% 
    dplyr::filter(MODULO != 131) %>% 
    mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
    mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
    mutate(Fecha = paste0(str_to_lower(substr(list[i],1,3)),'. ',substr(list[i],4,7))) %>% 
    mutate(Fecha = as.yearmon(Fecha)) %>% 
    mutate(ctaCont = substr(RUBRO,1,3)) %>% 
    mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                 ctaCont == '134'~saldous,
                                 ctaCont == '136'~saldous,
                                 ctaCont == '137'~saldous,
                                 TRUE ~ 0)) %>%
    mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
    mutate(par0 = ifelse(DIASMORA >0, saldous, 0)) %>%
    mutate(par30 = ifelse(DIASMORA >30, saldous, 0)) %>%
    arrange(CI, GENERO) %>% 
    select(CI, saldous, Fecha, previus, GENERO, par0, saldoMora, par30,ESTADO,MODULO) %>% 
    group_by(Fecha, CI, GENERO, ESTADO) %>% 
    summarise_all(sum, na.rm=T)
  bdcList[[i]] <- bdcBSO
}

#bsoFull sale de los RDS (sin castigados ni lineas de crédito)
#bsoFull2 sale de los txt
bdcBSO <- bind_rows(bdcList)
write_rds(bdcBSO ,'D:/!bso/bsoFull.rds')

bsoFull <- readRDS('D:/!bso/bsoFull.rds') %>% 
  mutate(carnet = str_extract(CI,pattern = "[0-9]+")) %>%
  mutate(carnet = as.numeric(carnet)) %>% 
  mutate(CI_2 = gsub("[^0-9.-]", "", CI)) %>%
  mutate(CI_2 = gsub("[^[:alnum:] ]", "", CI_2)) %>%
  mutate(CI_2 = as.numeric(CI_2)) %>%
  glimpse()

remove(bdc)
####____READING INFOFULL____####
infoF <- readRDS('D:/!bso/califClientes/infoFull.rds') %>% 
  glimpse()

infoFull <- infoF %>% 
  mutate(Fecha=as.yearmon(as.Date(paste0(fbase, "01"), "%Y%m%d"))) %>% 
  dplyr::filter(fbase %in% c("202208","202209","202210","202211","202212")) %>% 
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - '))

remove(infoF)

info <- infoFull %>% 
  mutate(CI_2 = gsub("[^0-9.-]", "", CI)) %>%
  mutate(CI_2 = gsub("[^[:alnum:] ]", "", CI_2)) %>%
  mutate(CI_2 = as.numeric(CI_2)) %>%
  # mutate(carnet = str_extract(CI,pattern = "[0-9]+")) %>% 
  # mutate(carnet = as.numeric(carnet)) %>% 
  mutate(MontoOriginal = ifelse(MonedaOrigen == 'MN', as.numeric(MontoOriginal)/6.86, as.numeric(MontoOriginal))) %>% 
  mutate(saldoVig = ifelse(MonedaOrigen == 'MN',  as.numeric(`SBEF VIGENTE`)/6.86,   as.numeric(`SBEF VIGENTE`))) %>% 
  mutate(histStr = as.character(HISTORICO)) %>% # HISTORY CONSTRUCTION
  mutate(esBSO=ifelse(`SIGLA SBEF`=='BSO',1,0)) %>%
  group_by(CI,Fecha) %>%
  dplyr::filter(max(row_number())>1) %>% 
  # mutate(tieneBSO = max(esBSO),cuentas=max(row_number())) %>%
  ungroup() %>%
  # dplyr::filter(tieneBSO>=1) %>%
  group_by(CI,CI_2,Fecha) %>% # Agrupación por CI y fecha para establecer máximos
  mutate(califBSO = ifelse(`SIGLA SBEF` == 'BSO', `SBEF CALIFICACION`, '_'),
         califBSO_2 = max(califBSO, na.rm = T),
         distCalif = n_distinct(`SBEF CALIFICACION`)) %>% # Máx. calificación en BSO
  mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
         califSF_2 = max(califSF, na.rm = T)) %>% # Máx. calificación en SF
  # dplyr::filter(cuentas > 1) %>%
  dplyr::filter(`SIGLA SBEF`=='BSO') %>% 
  summarise(saldo=sum(saldo),saldoVig=sum(saldoVig),Mora=sum(saldoMora),
            MontoOriginal=sum(MontoOriginal),saldoVig=sum(saldoVig),
            califBSO=max(califBSO),califSF=max(califSF)) %>% 
  ungroup() %>% 
  glimpse()


infoCheck <- info %>% 
  # mutate(saldo = ifelse(MonedaOrigen == 'MN',  as.numeric(saldo)/6.86,   as.numeric(saldo))) %>% 
  arrange(CI,Fecha) %>% 
  group_by(Fecha) %>% 
  summarise(saldo=sum(saldo),CIS=n_distinct(CI))

bdcBSO_tab <- bsoFull %>% 
  ungroup() %>% 
  select(-GENERO,-CI,-carnet) %>% 
  group_by(Fecha) %>% 
  summarise_all(sum)
  
bdcBSO_tabcli <- bsoFull %>% 
  select(-GENERO) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  select(-saldous,-previus) %>% 
  summarise_all(n_distinct)

################################################################################
####____SIN GENERO____####
info <- info %>% 
  dplyr::filter(Fecha=="dic. 2022")
bsoFull <- bsoFull %>% 
  # rename(saldo=saldous) %>% 
  dplyr::filter(Fecha=="dic. 2022")
infoPerf1 <- info %>% 
  left_join((bsoFull %>% select(-CI_2)),by=c('CI','Fecha','saldo'))

infoPerf1 %>% 
  group_by(Fecha) %>% 
  summarise(saldo=sum(saldo),CIS=n_distinct(CI))

ops <- which(is.na(infoPerf1$GENERO))

infoPerf1[-ops,] %>% 
  group_by(Fecha) %>% 
  summarise(saldo=sum(saldo),CIS=n_distinct(CI))
infoPerf <- infoPerf1[ops,] %>%
  # group_by(CI_2,Fecha) %>%
  # mutate(pos=max(row_number())) %>%
  # ungroup() %>%
  select(-GENERO,-ESTADO,-previus,-par0,-saldoMora,-par30,-MODULO) %>% 
  left_join((bsoFull %>% select(-CI)),by=c('CI_2','Fecha')) %>% 
  select(-CI.y) %>% 
  rename(CI=CI.x)

infoPerf1 <- infoPerf1[-ops,] %>% 
  bind_rows(infoPerf)

infoPerf1 %>% 
  group_by(Fecha) %>% 
  summarise(saldo=sum(saldo,na.rm = T),CIS=n_distinct(CI))

infox <- infoPerf1 %>% 
  dplyr::filter(Fecha=="dic. 2022") %>% 
  dplyr::filter(`ESTADO!="CASTIGADA"```)

infox %>% 
  group_by(Fecha) %>% 
  summarise(saldo=sum(saldo),CIS=n_distinct(CI))

infoPerf1 %>% 
  dplyr::filter(ESTADO!="CASTIGADA") %>% 
  group_by(Fecha) %>% 
  summarise(saldo=sum(saldo),CIS=n_distinct(CI))

infoPerf2 %>% 
  dplyr::filter(`SIGLA SBEF`=='BSO') %>% 
  # mutate(saldo = ifelse(MonedaOrigen == 'MN',  as.numeric(saldo)/6.86,   as.numeric(saldo))) %>% 
  arrange(CI,Fecha) %>% 
  group_by(CI,Fecha) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  summarise(saldo=sum(saldo),CIS=n_distinct(CI))

infoPerf2 <- infoPerf1 %>% 
  bind_rows(infoPerf) %>% 
  
  dplyr::filter(`SIGLA SBEF`=='BSO') %>%
  ungroup() %>% 
  select(CI, califBSO_2, califSF_2, fbase,saldo) %>%
  group_by(CI, fbase) %>%
  summarise_all(max) %>% 
  
  dplyr::filter(`SIGLA SBEF`=="BSO") %>% 
  group_by(Fecha,CI) %>% 
  summarise(n=n(), m=n_distinct(CI_2)) %>% 
  arrange(desc(n),desc(m))
notFound <- infoPerf
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% # buscar solamente tipos de DEUDOR
  #dplyr::filter(`SIGLA SBEF` != 'BSO') %>% # check filter
  mutate(MontoOriginal = ifelse(MonedaOrigen == 'MN', as.numeric(MontoOriginal)/6.86, as.numeric(MontoOriginal))) %>% 
  mutate(saldoVig = ifelse(MonedaOrigen == 'MN',  as.numeric(`SBEF VIGENTE`)/6.86,   as.numeric(`SBEF VIGENTE`))) %>% 
  mutate(histStr = as.character(HISTORICO)) %>% # HISTORY CONSTRUCTION
  arrange(CI) %>%
  ungroup() %>% 
  group_by(CI, fbase) %>% # Agrupación por CI y fecha para establecer máximos
  dplyr::filter(max(row_number()) > 1) %>% 
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
  summarise(n=n()) %>% 
  pivot_wider(names_from = califSF_2, values_from = n) %>% # Ponemos calif en el SF en las columnas
  mutate(across(A:`F`,~as.double(.x))) %>% 
  adorn_totals('row', 'col') %>% 
  rowwise() %>% 
  mutate(igualCalif=case_when(califBSO_2 == 'A'~ A,
                              califBSO_2 == 'B'~ B,
                              califBSO_2 == 'C'~ C,
                              califBSO_2 == 'D'~ D,
                              califBSO_2 == 'E'~ E,
                              califBSO_2 == 'F'~ `F`)) %>% 
  mutate(peorCalif=case_when(califBSO_2 == 'A'~ sum(B,C,D,E,`F`,na.rm = T),
                             califBSO_2 == 'B'~ sum(C,D,E,`F`,na.rm = T),
                             califBSO_2 == 'C'~ sum(D,E,`F`,na.rm = T),
                             califBSO_2 == 'D'~ sum(E,`F`,na.rm = T),
                             califBSO_2 == 'E'~ (`F`),)) %>% 
  mutate(mejorCalif=case_when(califBSO_2 == 'B'~ (A),
                              califBSO_2 == 'C'~ sum(A,B,na.rm = T),
                              califBSO_2 == 'D'~ sum(A,B,C,na.rm = T),
                              califBSO_2 == 'E'~ sum(A,B,C,D,na.rm = T),
                              califBSO_2 == 'F'~ sum(A,B,C,D,E,na.rm = T))) %>% 
  mutate(TotalFila=sum(A,B,C,D,E,`F`,na.rm = T)) %>%
  rowwise() %>% 
  mutate(TotalCalif= sum(c(igualCalif, peorCalif, mejorCalif), na.rm = T)) %>% 
  mutate(across(A:TotalCalif, ~replace_na(. , 0))) %>% 
  mutate(Fecha=as.yearmon(as.Date(paste0(fbase, "01"), "%Y%m%d"))) %>% 
  #select(-fbase) %>% 
  relocate(Fecha) 

tabClientes_a <- infoPerf1 %>%
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
################################################################################
####____CON GENERO____####
infoPerf2 <- infoFull %>% 
  left_join(bsoFull,by=c('CI','Fecha')) %>% 
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% # buscar solamente tipos de DEUDOR
  #dplyr::filter(`SIGLA SBEF` != 'BSO') %>% # check filter
  mutate(MontoOriginal = ifelse(MonedaOrigen == 'MN', as.numeric(MontoOriginal)/6.86, as.numeric(MontoOriginal))) %>% 
  mutate(saldoVig = ifelse(MonedaOrigen == 'MN',  as.numeric(`SBEF VIGENTE`)/6.86,   as.numeric(`SBEF VIGENTE`))) %>% 
  mutate(histStr = as.character(HISTORICO)) %>% # HISTORY CONSTRUCTION
  arrange(CI) %>%
  ungroup() %>% 
  group_by(CI, fbase,GENERO) %>% # Agrupación por CI y fecha para establecer máximos
  dplyr::filter(max(row_number()) > 1) %>% 
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
  select(CI, califBSO_2, califSF_2, fbase,GENERO) %>%
  group_by(CI, fbase,GENERO) %>%
  summarise_all(max) %>% 
  ungroup() %>% 
  select(califBSO_2, califSF_2, fbase,GENERO) %>% 
  group_by(califBSO_2, califSF_2, fbase,GENERO) %>% # Agrupamos por calificaciones para hacer el recuento 
  summarise(n=n()) %>% 
  pivot_wider(names_from = califSF_2, values_from = n) %>% # Ponemos calif en el SF en las columnas
  mutate(across(A:`F`,~as.double(.x))) %>% 
  adorn_totals('row', 'col') %>% # Acá creamos mejor/igual/peor calificación
  rowwise() %>% 
  mutate(igualCalif=case_when(califBSO_2 == 'A'~ A,
                              califBSO_2 == 'B'~ B,
                              califBSO_2 == 'C'~ C,
                              califBSO_2 == 'D'~ D,
                              califBSO_2 == 'E'~ E,
                              califBSO_2 == 'F'~ `F`)) %>% 
  mutate(peorCalif=case_when(califBSO_2 == 'A'~ sum(B,C,D,E,`F`,na.rm = T),
                             califBSO_2 == 'B'~ sum(C,D,E,`F`,na.rm = T),
                             califBSO_2 == 'C'~ sum(D,E,`F`,na.rm = T),
                             califBSO_2 == 'D'~ sum(E,`F`,na.rm = T),
                             califBSO_2 == 'E'~ (`F`),)) %>% 
  mutate(mejorCalif=case_when(califBSO_2 == 'B'~ (A),
                              califBSO_2 == 'C'~ sum(A,B,na.rm = T),
                              califBSO_2 == 'D'~ sum(A,B,C,na.rm = T),
                              califBSO_2 == 'E'~ sum(A,B,C,D,na.rm = T),
                              califBSO_2 == 'F'~ sum(A,B,C,D,E,na.rm = T))) %>% 
  mutate(TotalFila=sum(A,B,C,D,E,`F`,na.rm = T)) %>%
  mutate(TotalCalif= sum(c(igualCalif, peorCalif, mejorCalif), na.rm = T)) %>% 
  mutate(across(A:TotalCalif, ~replace_na(. , 0))) %>% 
  mutate(Fecha=as.yearmon(as.Date(paste0(fbase, "01"), "%Y%m%d"))) %>% 
  #select(-fbase) %>% 
  relocate(Fecha) 

infoPerf3 <- infoPerf2 %>% 
  select(-GENERO) %>% 
  group_by(Fecha,fbase,califBSO_2) %>% 
  summarise_all(sum,na.rm=T)

tabClientes_b <- infoPerf2 %>%
  select(Fecha, starts_with('peor'), starts_with('mejor'), starts_with('igual'),
         TotalCalif,GENERO) %>%
  group_by(Fecha,GENERO) %>% 
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
  dplyr::filter(!is.na(Mes))

write.xlsx(tabClientes_b,'D:/!bso/tabClientes.xlsx')
####____INFO PERF SALDO____####
infoPerf_2 <- infoFull %>% 
  select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`,
         `ENTIDAD SBEF`,  `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
         MontoOriginal, MonedaOrigen, `SBEF CALIFICACION`, `FR CALIFICACION`,
         saldo, saldoMora, saldoMMC, fbase) %>% # entra saldo al select
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% 
  mutate(Fecha=as.yearmon(as.Date(paste0(fbase, "01"), "%Y%m%d"))) %>% 
  select(-fbase) %>% 
  left_join(bsoFull, by = c('CI', 'Fecha')) %>% 
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
  rowwise() %>% 
  mutate(igualCalif=case_when(califBSO_2 == 'A'~ saldo_A,
                              califBSO_2 == 'B'~ saldo_B,
                              califBSO_2 == 'C'~ saldo_C,
                              califBSO_2 == 'D'~ saldo_D,
                              califBSO_2 == 'E'~ saldo_E,
                              califBSO_2 == 'F'~ saldo_F)) %>% 
  mutate(peorCalif=case_when(califBSO_2 == 'A'~ sum(saldo_B,saldo_C,saldo_D,saldo_E,saldo_F,na.rm = T),
                             califBSO_2 == 'B'~ sum(saldo_C,saldo_D,saldo_E,saldo_F,na.rm = T),
                             califBSO_2 == 'C'~ sum(saldo_D,saldo_E,saldo_F,na.rm = T),
                             califBSO_2 == 'D'~ sum(saldo_E,saldo_F,na.rm = T),
                             califBSO_2 == 'E'~ (saldo_F),)) %>% 
  
  mutate(mejorCalif=case_when(califBSO_2 == 'B'~ (saldo_A),
                              califBSO_2 == 'C'~ sum(saldo_A,saldo_B,na.rm = T),
                              califBSO_2 == 'D'~ sum(saldo_A,saldo_B,saldo_C,na.rm = T),
                              califBSO_2 == 'E'~ sum(saldo_A,saldo_B,saldo_C,saldo_D,na.rm = T),
                              califBSO_2 == 'F'~ sum(saldo_A,saldo_B,saldo_C,saldo_D,saldo_E,na.rm = T))) %>% 
  mutate(TotalFila=sum(saldo_A,saldo_B,saldo_C,saldo_D,saldo_E,saldo_F,na.rm = T)) %>%
  mutate(TotalCalif= sum(c(igualCalif, peorCalif, mejorCalif), na.rm = T)) %>% 
  mutate(igualCalif_us=case_when(califBSO_2 == 'A'~ saldous_A,
                                 califBSO_2 == 'B'~ saldous_B,
                                 califBSO_2 == 'C'~ saldous_C,
                                 califBSO_2 == 'D'~ saldous_D,
                                 califBSO_2 == 'E'~ saldous_E,
                                 califBSO_2 == 'F'~ saldous_F)) %>% 
  mutate(peorCalif_us=case_when(califBSO_2 == 'A'~ sum(saldous_B,saldous_C,saldous_D,saldous_E,saldous_F,na.rm = T),
                                califBSO_2 == 'B'~ sum(saldous_C,saldous_D,saldous_E,saldous_F,na.rm = T),
                                califBSO_2 == 'C'~ sum(saldous_D,saldous_E,saldous_F,na.rm = T),
                                califBSO_2 == 'D'~ sum(saldous_E,saldous_F,na.rm = T),
                                califBSO_2 == 'E'~ (saldous_F),)) %>% 
  mutate(mejorCalif_us=case_when(califBSO_2 == 'B'~ (saldous_A),
                                 califBSO_2 == 'C'~ sum(saldous_A,saldous_B,na.rm = T),
                                 califBSO_2 == 'D'~ sum(saldous_A,saldous_B,saldous_C,na.rm = T),
                                 califBSO_2 == 'E'~ sum(saldous_A,saldous_B,saldous_C,saldous_D,na.rm = T),
                                 califBSO_2 == 'F'~ sum(saldous_A,saldous_B,saldous_C,saldous_D,saldous_E,na.rm = T))) %>% 
  mutate(TotalFila_us=sum(saldous_A,saldous_B,saldous_C,saldous_D,saldous_E,saldous_F,na.rm = T)) %>%
  mutate(TotalCalif_us= sum(c(igualCalif_us, peorCalif_us, mejorCalif_us), na.rm = T)) %>%
  mutate(across(saldo_A:TotalCalif_us, ~replace_na(. , 0))) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  mutate(totalSaldo = sum(TotalCalif)) %>% 
  mutate(totalSaldo_us = sum(TotalCalif_us)) %>% 
  ungroup() %>% 
  mutate(across(starts_with('saldo'), ~formatC(., format = 'd', big.mark = ','))) 


tabSaldo <- infoPerf_2 %>%
  select(Fecha, starts_with('peor'), starts_with('mejor'), starts_with('igual'),
         starts_with('totalS'),TotalFila_us,GENERO) %>%
  left_join(bdcBSO_tab, by = 'Fecha') %>% 
  group_by(Fecha,GENERO) %>% 
  summarise(mejorCalif = sum(mejorCalif), igualCalif = sum(igualCalif),
            peorCalif = sum(peorCalif), mejorCalif_us = sum(mejorCalif_us), 
            igualCalif_us = sum(igualCalif_us), peorCalif_us = sum(peorCalif_us),
            totalSaldo = mean(totalSaldo), totalSaldo_us = sum(TotalFila_us),
            saldous=max(saldous)) %>% 
  mutate(pct_SF_BSO = totalSaldo/totalSaldo_us,
         pct_SF_totalBSO = totalSaldo/saldous,
         pct_BSO_totalBSO = totalSaldo_us/saldous) %>% 
  select(Fecha, starts_with('pct'), ends_with('_us'), saldous,GENERO) %>% 
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
write.xlsx(tabSaldo,'D:/!bso/tabSaldo.xlsx')
