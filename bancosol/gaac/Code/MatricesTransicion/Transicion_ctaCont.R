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
library(openxlsx)
# require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

cases <- function(quant,levs,values){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- rep(NA,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
####____READING HISTORICO____####
bdcFull <- readRDS("D:/!bso/features/Historial_Operaciones.rds") %>% 
  glimpse()
####____TRANSICIONES CON LAG VARIABLE____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
lagList <- c(1,2,3,6,12,24)
# dlist <- list()
# codAge <- read_excel("D:/!bso/bases/excel/CodAgeSucReg.xlsx")
i <- 1
for (lag in lagList) {
  clist <- list()
  for(i in 1:(length(myrds)-lag)) {
    tryCatch({
      print(i)
      print(myrds[i])
      k <- i + lag
      print(myrds[i+lag])
      
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i], '.rds')) %>%  #se abre mes anterior
        dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>% 
        mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
        select(OPERACION, CTACLIENTE, ctaCont, monDate, saldous, previus)
      
      df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[k], '.rds')) %>%  #se abre mes posterior 
        dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>%
        mutate(saldous=ifelse(saldoCast>0, saldoCast, saldous)) %>% 
        select(OPERACION, CTACLIENTE, ctaCont, monDate, saldous, previus)
      
      dfCancel <- df1 %>% 
        anti_join(df2,by=c("CTACLIENTE","OPERACION")) %>% 
        mutate(ctaCont = "999") %>% 
        mutate(saldous = 0) %>% 
        mutate(monDate = monDate+1/12) %>% 
        bind_rows(df2)
      
      dfTrans <- df1 %>% 
        left_join(dfCancel, by=c("CTACLIENTE","OPERACION"), suffix = c("_ini","_fin")) %>% 
        mutate(trans = paste(ctaCont_ini, ctaCont_fin,sep="-")) %>% 
        mutate(difPrev = previus_fin - previus_ini) %>% 
        mutate(opTot = 1) %>% 
        select(monDate = monDate_ini, trans, ctaCont_ini, ctaCont_fin, saldous_ini, saldous_fin,
               opTot, difPrev) %>% 
        group_by(monDate, trans, ctaCont_fin, ctaCont_ini) %>% 
        summarise_all(sum,na.rm=T) %>% 
        ungroup() %>% 
        group_by(monDate, ctaCont_ini) %>% 
        mutate(prob = opTot/sum(opTot)) %>% 
        mutate(probS = saldous_fin/sum(saldous_fin)) %>% 
        mutate(probS_ini = saldous_ini/sum(saldous_ini)) %>% 
        mutate(ctaCont_ini = case_when(ctaCont_ini == "131"~"131 Vigente",
                                       ctaCont_ini == "133"~"133 Vencida",
                                       ctaCont_ini == "134"~"134 Ejecución",
                                       ctaCont_ini == "135"~"135 Repro Vigente",
                                       ctaCont_ini == "136"~"136 Repro Vencida",
                                       ctaCont_ini == "137"~"137 Repro Ejecución",
                                       ctaCont_ini == "865"~"865 Castigada",
                                       ctaCont_ini == "999"~"999 Cancelada",)) %>% 
        mutate(ctaCont_fin = case_when(ctaCont_fin == "131"~"131 Vigente",
                                       ctaCont_fin == "133"~"133 Vencida",
                                       ctaCont_fin == "134"~"134 Ejecución",
                                       ctaCont_fin == "135"~"135 Repro Vigente",
                                       ctaCont_fin == "136"~"136 Repro Vencida",
                                       ctaCont_fin == "137"~"137 Repro Ejecución",
                                       ctaCont_fin == "865"~"865 Castigada",
                                       ctaCont_fin == "999"~"999 Cancelada",))
      
      clist[[i]] <- dfTrans
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  cFull <- rbindlist(clist)
  saveRDS(cFull, paste0("D:/!bso/transMat/ctaCont/Trans_ctaCont_",lag,"_v2.rds"))
}
####____MATRICES____####
files <- c(1,2,3,6,12,24)
for (i in 1:length(files)) {
  trans_lag <- readRDS(paste0("D:/!bso/transMat/ctaCont/Trans_ctaCont_Lag",files[i],".rds")) %>% 
    mutate(ctaCont_fin = ifelse(ctaCont_fin=="131 Repro Ejecución","137 Repro Ejecución",ctaCont_fin)) %>% 
    mutate(ctaCont_ini = ifelse(ctaCont_ini=="131 Repro Ejecución","137 Repro Ejecución",ctaCont_ini))
  saveRDS(trans_lag, paste0("D:/!bso/transMat/ctaCont/Trans_ctaCont_Lag",files[i],".rds"))
}

files <- c(1,2,3,6,12,24)
for (i in 1:length(files)) {
  trans_lag <- readRDS(paste0("D:/!bso/transMat/ctaCont/Trans_ctaCont_Lag",files[i],"_v2.rds")) %>% 
    mutate(monDate = monDate + files[i]/12) %>% 
    mutate(monDate = as.Date(monDate,frac=1))
  
  trans_lag1_prob <- trans_lag %>% 
    select(monDate, ctaCont_ini, ctaCont_fin, prob) %>% 
    pivot_wider(names_from = ctaCont_fin, values_from = prob, values_fill = 0) 
  trans_lag1_ops <- trans_lag %>% 
    select(monDate, ctaCont_ini, ctaCont_fin, opTot) %>% 
    pivot_wider(names_from = ctaCont_fin, values_from = opTot, values_fill = 0) 
  trans_lag1_saldo <- trans_lag %>% 
    select(monDate, ctaCont_ini, ctaCont_fin, saldous_ini) %>% 
    pivot_wider(names_from = ctaCont_fin, values_from = saldous_ini, values_fill = 0) 
  trans_lag1_probSaldo <- trans_lag %>% 
    select(monDate, ctaCont_ini, ctaCont_fin, probS_ini) %>% 
    pivot_wider(names_from = ctaCont_fin, values_from = probS_ini, values_fill = 0) 
  Informe <- list(prob = trans_lag1_prob, ops = trans_lag1_ops, saldo = trans_lag1_saldo, probS = trans_lag1_probSaldo)
  write_xlsx(Informe, paste0("D:/!bso/transMat/ctacont/Trans_ctaCont_Lag",files[i],"_v2.xlsx"))
}

#####____ADDING A MONTH TO BDC Trans cont____####
cFull <- readRDS("D:/!bso/transMat/ctaCont/Trans_ctaCont_Lag1_v2.rds")
myrds <- c("May2023","Jun2023")

df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[1], '.rds')) %>%  #se abre mes anterior
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>% 
  mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
  select(OPERACION, CTACLIENTE, ctaCont, monDate, saldous, previus)

df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[2], '.rds')) %>%  #se abre mes posterior 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>%
  mutate(saldous=ifelse(saldoCast>0, saldoCast, saldous)) %>% 
  select(OPERACION, CTACLIENTE, ctaCont, monDate, saldous, previus)

dfCancel <- df1 %>% 
  anti_join(df2,by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(ctaCont = "999") %>% 
  mutate(saldous = 0) %>% 
  mutate(monDate = monDate+1/12) %>% 
  bind_rows(df2)

dfTrans <- df1 %>% 
  left_join(dfCancel, by=c("CTACLIENTE","OPERACION"), suffix = c("_ini","_fin")) %>% 
  mutate(trans = paste(ctaCont_ini, ctaCont_fin,sep="-")) %>% 
  mutate(difPrev = previus_fin - previus_ini) %>% 
  mutate(opTot = 1) %>% 
  select(monDate = monDate_ini, trans, ctaCont_ini, ctaCont_fin, saldous_ini, saldous_fin,
         opTot, difPrev) %>% 
  group_by(monDate, trans, ctaCont_fin, ctaCont_ini) %>% 
  summarise_all(sum,na.rm=T) %>% 
  ungroup() %>% 
  group_by(monDate, ctaCont_ini) %>% 
  mutate(prob = opTot/sum(opTot)) %>% 
  mutate(probS = saldous_fin/sum(saldous_fin)) %>% 
  mutate(probS_ini = saldous_ini/sum(saldous_ini)) %>% 
  mutate(ctaCont_ini = case_when(ctaCont_ini == "131"~"131 Vigente",
                                 ctaCont_ini == "133"~"133 Vencida",
                                 ctaCont_ini == "134"~"134 Ejecución",
                                 ctaCont_ini == "135"~"135 Repro Vigente",
                                 ctaCont_ini == "136"~"136 Repro Vencida",
                                 ctaCont_ini == "137"~"137 Repro Ejecución",
                                 ctaCont_ini == "865"~"865 Castigada",
                                 ctaCont_ini == "999"~"999 Cancelada",)) %>% 
  mutate(ctaCont_fin = case_when(ctaCont_fin == "131"~"131 Vigente",
                                 ctaCont_fin == "133"~"133 Vencida",
                                 ctaCont_fin == "134"~"134 Ejecución",
                                 ctaCont_fin == "135"~"135 Repro Vigente",
                                 ctaCont_fin == "136"~"136 Repro Vencida",
                                 ctaCont_fin == "137"~"137 Repro Ejecución",
                                 ctaCont_fin == "865"~"865 Castigada",
                                 ctaCont_fin == "999"~"999 Cancelada",))

tail(cFull %>% group_by(monDate) %>% summarise(sum(saldous_ini)))
cFull <- cFull %>% 
  bind_rows(dfTrans)
saveRDS(cFull,"D:/!bso/transMat/ctaCont/Trans_ctaCont_Lag1_v2.rds")
####____CREATING BDC TRANS____####
#Se hace el recuento de transiciones por mes
lastmonth <- "Abr. 2023"
shortmonth <- str_replace(lastmonth,". ","")

bdcCancel <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, ctaCont, monDate, saldous, previus) %>% 
  dplyr::filter(monDate >= 'Ene. 2015') %>%
  group_by(OPERACION, CTACLIENTE) %>%
  arrange(desc(monDate)) %>%
  dplyr::filter(row_number()==1) %>% 
  dplyr::filter(monDate != lastmonth) %>% 
  mutate(monDate = monDate+1/12) %>% 
  mutate(saldous = 0) %>% 
  mutate(previus = 0) %>% 
  mutate(ctaCont = "999")
  
bdcTrans <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, ctaCont, monDate, saldous, previus) %>% 
  bind_rows(bdcCancel) %>% 
  group_by(OPERACION, CTACLIENTE) %>%
  arrange(monDate) %>%
  dplyr::rename(cmt = ctaCont) %>% 
  mutate(cm1 = dplyr::lag(cmt, 1)) %>% 
  mutate(cm2 = dplyr::lag(cmt, 2)) %>% 
  mutate(cm3 = dplyr::lag(cmt, 3)) %>% 
  mutate(cm6 = dplyr::lag(cmt, 6)) %>% 
  mutate(cm12 = dplyr::lag(cmt, 12)) %>% 
  mutate(cm24 = dplyr::lag(cmt, 24)) %>% 
  ungroup() 

bdcOps <- bdcTrans %>% 
  mutate(trans1 = ifelse(cm1 %in% c('131','133','135','136','137','865','999') &
                          cmt %in% c('131','133','135','136','137','865','999'),
                        paste(cm1, cmt, sep="-"), NA)) %>% 
  mutate(trans2 = ifelse(cm2 %in% c('131','133','135','136','137','865','999') &
                           cmt %in% c('131','133','135','136','137','865','999'),
                         paste(cm2, cmt, sep="-"), NA)) %>% 
  mutate(trans3 = ifelse(cm3 %in% c('131','133','135','136','137','865','999') &
                           cmt %in% c('131','133','135','136','137','865','999'),
                         paste(cm3, cmt, sep="-"), NA)) %>% 
  mutate(trans6 = ifelse(cm6 %in% c('131','133','135','136','137','865','999') &
                           cmt %in% c('131','133','135','136','137','865','999'),
                         paste(cm6, cmt, sep="-"), NA)) %>% 
  mutate(trans12 = ifelse(cm12 %in% c('131','133','135','136','137','865','999') &
                           cmt %in% c('131','133','135','136','137','865','999'),
                         paste(cm12, cmt, sep="-"), NA)) %>% 
  mutate(trans24 = ifelse(cm24 %in% c('131','133','135','136','137','865','999') &
                           cmt %in% c('131','133','135','136','137','865','999'),
                         paste(cm24, cmt, sep="-"), NA)) %>% 
  dplyr::filter(!is.na(trans)) %>%
  #dplyr::filter(!is.na(DiasMoraMes) & !(DiasMoraMes>31)) %>% 
  group_by(cm1, monDate) %>% #Se agrupa por calificacion inicial y por mes
  mutate(rowTot = n()) %>% 
  ungroup()


tm_ops <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate,saldous, previus, difPrev) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, trans) %>%
  summarise_all(sum, na.rm=T) %>% 
  
saveRDS(bdcTrans, "D:/!bso/transMat/ctaCont/")
##NEW VERSION
bdcTrans <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CALIFICACION, monDate, DIASMORA, saldous, previus) %>% 
  dplyr::filter(monDate >= 'ene. 2015') %>%
  group_by(OPERACION, CTACLIENTE) %>%
  arrange(OPERACION, CTACLIENTE, monDate) %>%
  dplyr::rename(cm1 = CALIFICACION) %>% 
  mutate(cmt = dplyr::lead(cm1, 1)) %>% #Antes se hacía con lag
  mutate(difPrev = dplyr::lead(previus)-previus) %>% 
  mutate(DiasMoraMes= dplyr::lead(DIASMORA,1)-DIASMORA) %>% 
  ungroup() 
bdcTrans <- bdcTrans_old %>% #Colocar bdcTrans2 para correr y y z
  mutate(trans = ifelse(cm1 %in% c('A','B','C','D','E','F') &
                          cmt %in% c('A','B','C','D','E','F','S'),paste0(cm1,cmt),NA)) %>% 
  mutate(trans = ifelse(cm1=='S'& cmt=='S',paste0(cm1,cmt),trans)) %>% 
  dplyr::filter(!is.na(trans)) %>%
  #dplyr::filter(!is.na(DiasMoraMes) & !(DiasMoraMes>31)) %>% 
  ungroup() %>% 
  group_by(cm1, monDate) %>% #Se agrupa por calificacion inicial y por mes
  mutate(rowTot = n()) %>% 
  ungroup()

y <- bdcTrans %>% 
  dplyr::filter(is.na(trans)) %>% 
  group_by(monDate) %>% 
  summarise(Saldo=sum(saldous),n=n())
z <- bdcTrans %>% 
  # dplyr::filter(is.na(trans)) %>% 
  group_by(monDate) %>% 
  summarise(Saldo=sum(saldous),n=n())
write_rds(bdcTrans, 'D:/!bso/transMat/bdcTrans.rds') #Se guarda el recuento de transiciones por mes
bdcTrans <- readRDS("D:/!bso/transMat/bdcTrans.rds")

tail(bdcTrans %>%
       group_by(monDate) %>% 
       summarise(Saldo= sum(saldous),nOps=n())) # Resumen informativo de transiciones
####____CREATING TM OPS____####
#Se agrega a la base transiciones las transiciones hacia el estado de cancelado
tm_ops <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate,saldous, previus, difPrev) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, trans) %>%
  summarise_all(sum, na.rm=T) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(mutate(bdcCancel,monDate=monDate-1/12)) %>% #Aquí se añade la tabla resumen de cancelados
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% #Delimitación temporal
  group_by(monDate, cm1) %>% 
  mutate(rowTot = sum(one)) %>% #Transiciones en número de operaciones
  mutate(prob = round(one/rowTot*100,2)) %>% 
  mutate(saldoTot = sum(saldous, na.rm = T)) %>% #Transiciones en saldo
  mutate(probS = round(saldous/saldoTot*100,2)) %>% 
  mutate(previTot = sum(previus, na.rm = T)) %>% #Transiciones en previsión
  mutate(probP = round(previus/previTot*100,2)) %>% 
  mutate(difPrevTot = sum(difPrev, na.rm = T)) %>% #Transiciones en diferencia de previsión
  mutate(probDP = round(difPrev/difPrevTot*100,2)) %>% 
  arrange(cm1, monDate) %>% 
  mutate(Deterioro = case_when(cm1 == 'A' ~ sum(prob[!(cmt %in% c('A','Z'))]), 
                               cm1 == 'B' ~ sum(prob[!(cmt %in% c('A','B','Z'))]),
                               cm1 == 'C' ~ sum(prob[!(cmt %in% c('A','B','C','Z'))]),
                               cm1 == 'D' ~ sum(prob[!(cmt %in% c('A','B','C','D','Z'))]),
                               cm1 == 'E' ~ sum(prob[!(cmt %in% c('A','B','C','D','E','Z'))]),
                               cm1 == 'F' ~ sum(prob[!(cmt %in% c('A','B','C','D','E','F','Z'))]),
                               TRUE ~ 0)) %>% 
  mutate(Recuperacion = case_when(cm1 == 'A' ~ sum(prob[cmt == 'Z']),
                                  cm1 == 'Z' ~ 0,
                                  cm1 == 'B' ~ sum(prob[cmt %in% c('A','Z')]),
                                  cm1 == 'C' ~ sum(prob[cmt %in% c('A','B','Z')]),
                                  cm1 == 'D' ~ sum(prob[cmt %in% c('A','B','C','Z')]),
                                  cm1 == 'E' ~ sum(prob[cmt %in% c('A','B','C','D','Z')]),
                                  cm1 == 'F' ~ sum(prob[cmt %in% c('A','B','C','D','E','Z')]),
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

tail(tm_ops %>%
       group_by(monDate) %>% 
       summarise(Saldo=sum(saldous),n=sum(one)))