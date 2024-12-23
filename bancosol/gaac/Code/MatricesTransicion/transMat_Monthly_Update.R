####____CARGA DE PAQUETES y FUNCIONES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(lubridate)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(quantmod)
library(scales)
library(stringr)
library(forcats)
library(tseries)
library(scales)
library(janitor)
library(ggplot2)
library(openxlsx)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)

cases <- function(quant,levs,values,default=NA){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- rep(default,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
####____CREATING BDC CANCEL____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
lag <- 1
clist <- list()
# dlist <- list()
# codAge <- read_excel("D:/!bso/bases/excel/CodAgeSucReg.xlsx")
for(i in 1:(length(myrds)-lag)) {
  tryCatch({
    print(i)
    print(myrds[i])
    k <- i + lag
    print(myrds[i+lag])
    if(i==1){
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i], '.rds')) #se abre mes anterior
      
      df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[k], '.rds')) #se abre mes posterior 
    }else{
      df1 <- df2
      df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[k], '.rds')) #se abre mes posterior   
    }
    
    #Se obtienen los cancelados como aquellos que están en el cierre del mes anterior pero no el posterior
    df3 <- df1 %>% 
      anti_join(df2, by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(saldous=ifelse(saldoCast>0,saldoCast,saldous)) %>% 
      select(OPERACION, CI, CTACLIENTE, FDESEMBOLSO,fdes, MONTO, MONEDA, saldous, saldoCast, CALIFICACION, ESTADO,
             tipoCred, Sucursal, NOMBRE_AGENCIA) %>% 
      dplyr::filter(CALIFICACION %in% c("A","B","C","D","E","F","S")) %>% 
      mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
      mutate(mCancel = as.yearmon(paste0(substr(myrds[k],1,3),". ",substr(myrds[k],4,7)))) %>% 
      mutate(mSearch = as.yearmon(paste0(substr(myrds[i],1,3),". ",substr(myrds[i],4,7))))
    #Se almacenan los cancelado de cada mes en una lista
    clist[[i]] <- df3
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
#Se unifica la lista en una sola "tabla"
dfCancel <- rbindlist(clist) %>% 
  dplyr::filter(!is.na(mCancel)) %>% 
  mutate(yearCancel = year(mCancel)) %>% 
  mutate(mesCancel = month(mCancel)) %>%
  select(OPERACION,CTACLIENTE,CI,fdes,mCancel,yearCancel,mesCancel,saldous,MONTO,MONEDA,Sucursal,NOMBRE_AGENCIA)

write_rds(dfCancel,"D:/!bso/Consultas/dfCancelEne15Abr23.rds") #Se guarda la base de cancelados
dfCancel <- readRDS("D:/!bso/Consultas/dfCancelEne15Abr23.rds")
print(dfCancel %>% 
        dplyr::filter(mCancel > 'dic. 2021') %>% 
        group_by(mCancel) %>% 
        summarise(saldo=sum(saldous),nOps=n())) #Resumen informativo de cancelados por mes

#Se construye una tabla resumen de los cancelados haciendo un recuento de su calificación en el mes anterior
dfCancelSum <- rbindlist(clist) %>% 
  dplyr::filter(!is.na(mCancel)) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  dplyr::rename(cm1 = CALIFICACION,
                monDate = mCancel) %>%
  mutate(cmt='Z') %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(cm1, cmt, trans, monDate, saldous) %>%
  group_by(cm1, cmt, trans, monDate) %>% 
  summarise(n=n(),saldous=sum(saldous)) %>% 
  dplyr::rename(one = n) %>% 
  glimpse()

write_rds(dfCancelSum, 'D:/!bso/transMat/matCancel.rds') #Se almacena la tabla resumen

bdcCancel <- readRDS('D:/!bso/transMat/matCancel.rds')
glimpse(bdcCancel)
####____ADD A MONTH TO BDC CANCEL____####
dfCancel <- readRDS("D:/!bso/Consultas/dfCancelEne15Abr23.rds")
bdcCancel <- readRDS('D:/!bso/transMat/matCancel.rds')
myrds <- c('Abr2023','Mar2023') #Colocar nombre MonthYYYY del mes previo y mes actual
tryCatch({
  print(myrds[1])
  print(myrds[2])
  
  df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[1], '.rds')) #se abre mes anterior
  df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[2], '.rds')) #se abre mes posterior
  
  #Se obtienen los cancelados como aquellos que están en el cierre del mes anterior pero no en el posterior
  df3 <- df1 %>% 
    anti_join(df2, by=c("CTACLIENTE","OPERACION")) %>% 
    mutate(saldous=ifelse(saldoCast>0,saldoCast,saldous)) %>% 
    select(OPERACION, CI, CTACLIENTE, FDESEMBOLSO,fdes, MONTO, MONEDA, saldous, saldoCast, CALIFICACION, ESTADO,
           tipoCred, Sucursal, NOMBRE_AGENCIA) %>% 
    dplyr::filter(CALIFICACION %in% c("A","B","C","D","E","F","S")) %>% 
    mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(mCancel = as.yearmon(paste0(substr(myrds[2],1,3),". ",substr(myrds[2],4,7)))) %>% 
    mutate(mSearch = as.yearmon(paste0(substr(myrds[1],1,3),". ",substr(myrds[1],4,7))))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

dfCancel <- dfCancel %>% 
  bind_rows(df3 %>% 
              mutate(yearCancel = year(mCancel)) %>% 
              mutate(mesCancel = month(mCancel)) %>%
              select(OPERACION,CTACLIENTE,CI,fdes,mCancel,yearCancel,mesCancel,saldous,
                     MONTO,MONEDA,Sucursal,NOMBRE_AGENCIA))
write_rds(dfCancel,"D:/!bso/Consultas/dfCancelEne15Abr23.rds") #change name of file

tail(dfCancel %>% 
  group_by(mCancel) %>% 
  summarise(nOps=n()) %>% 
  ungroup())
df3 <- df3 %>% 
  dplyr::filter(!is.na(mCancel)) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  dplyr::rename(cm1 = CALIFICACION,
                monDate = mCancel) %>%
  mutate(cmt='Z') %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(cm1, cmt, trans, monDate) %>%
  group_by(cm1, cmt, trans, monDate) %>% 
  summarise(one=n()) %>% 
  glimpse()

bdcCancel <- bdcCancel %>% 
  bind_rows(df3)
write_rds(bdcCancel, 'D:/!bso/transMat/matCancel.rds')

####____CREATING BDC FULL____####
#Con la misma lista de meses-años se consolida una base de operaciones y sus respectivas calificaciones
bdcList <- list()
for (i in 1:length(myrds)) {
  print(myrds[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_', myrds[i], '.rds')) %>% 
    select(OPERACION, CI, CTACLIENTE, saldous, CALIFICACION, ESTADO, DIASMORA, 
           montous, saldous, previus, saldoCast, tipoCred, SECTOR_CARTERA) %>% 
    mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
    mutate(monDate = as.yearmon(paste0(substr(myrds[i],1,3),". ",substr(myrds[i],4,7)))) %>% 
    mutate(saldous = ifelse(CALIFICACION == 'S', saldoCast, saldous)) %>% 
    select(-saldoCast)
  #Se almacena cada base de operacion, calificación y otros en una lista
  bdcList[[i]] <- bdc
}
gc()

#Se unifica la lista en una sola 'tabla'
bdcFull <- bind_rows(bdcList) %>% 
  arrange(CI, CTACLIENTE, OPERACION, monDate) %>% 
  glimpse()

bdcList <- NULL
write_rds(bdcFull, 'D:/!bso/transMat/bdcFull.rds') #Se almacena la base de transiciones

bdcFull <- readRDS('D:/!bso/transMat/bdcFull.rds')

####____ADD A MONTH TO BDC FULL____####
myrds <- 'Abr2023' #Colocar MonthYYYY del mes actual
print(myrds)
bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds, '.rds')) %>% 
  select(OPERACION, CI, CTACLIENTE, saldous, CALIFICACION, ESTADO, DIASMORA, 
         montous, saldous, previus, saldoCast, tipoCred, SECTOR_CARTERA) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION)) %>% 
  mutate(monDate = as.yearmon(paste0(substr(myrds,1,3),". ",substr(myrds,4,7)))) %>% 
  mutate(saldous = ifelse(CALIFICACION == 'S', saldoCast, saldous)) %>% 
  select(-saldoCast)

bdcFull <- bdcFull %>% 
  bind_rows(bdc)
write_rds(bdcFull,'D:/!bso/transMat/bdcFull.rds')

bdcSummary <- bdcFull %>% 
  dplyr::filter(monDate> 'dic. 2021') %>% 
  group_by(monDate) %>% 
  summarise(saldo=sum(saldous,na.rm=T),nOps=n()) %>% 
  ungroup()
write_rds(bdcSummary,'D:/!bso/transMat/bdcSummary.rds')

print(bdcSummary) #Resumen informativo de transiciones por mes
####____CREATING BDC TRANS____####
#Se hace el recuento de transiciones por mes
##OLD VERSION
bdcTrans_old <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CALIFICACION, monDate, DIASMORA, saldous, previus) %>% 
  dplyr::filter(monDate >= 'ene. 2015') %>%
  group_by(OPERACION, CTACLIENTE) %>%
  arrange(OPERACION, CTACLIENTE, monDate) %>%
  dplyr::rename(cmt = CALIFICACION) %>% 
  mutate(cm1 = dplyr::lag(cmt, 1)) %>% #Antes se hacía con lag
  mutate(difPrev = previus-dplyr::lag(previus)) %>% 
  mutate(DiasMoraMes= DIASMORA-dplyr::lag(DIASMORA,1)) %>% 
  ungroup() 
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
#Se almacena la base de recuento de todas las transiciones
write.csv(tm_ops,"D:/!bso/transMat/Oreports/tmAll_Abr23.csv",row.names = F)
####____CHECKS DE REPORTE____####
#ESTA SECCIÓN NO ES NECESARIA PUESTO QUE LOS RESULTADOS QUE SE OBTIENEN A PARTIR
#DE AQUÍ SE MUESTRAN EN EL REPORTE, PERO SE DEJA AL USUARIO COMPILAR O MODIFICAR
#ESTA SECCIÓN EN LUGAR DE SIMPLEMENTE VER EL REPORTE
tm_ops <- read.csv("D:/!bso/transMat/Oreports/tmAll_Abr23.csv") %>% 
  mutate(monDate=as.yearmon(monDate))
lastmonth <- "Abr. 2023"
lastmonth12 <- "Abr. 2022"
shortmonth <- str_replace(lastmonth,". ","")
shortmonth12 <- str_replace(lastmonth12,". ","")

tm2022_prob <- tm_ops %>% 
  dplyr::filter(monDate >= lastmonth12 & monDate< lastmonth) %>% #Se filtra los meses que se mostrarán en el reporte
  select(prob, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>% #Se coloca en formato de matriz
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,`F`,S,Z)

tm2022_ops <- tm_ops %>% 
  dplyr::filter(monDate >= lastmonth12 & monDate< lastmonth) %>% #Se filtra los meses que se mostrarán en el reporte
  mutate(one=as.double(one)) %>% 
  arrange(monDate) %>% 
  select(one, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = one, values_fill = 0) %>% #Formato de matriz
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,`F`,S,Z)
  
tm2022_saldo <- tm_ops %>% 
  dplyr::filter(monDate >= lastmonth12 & monDate< lastmonth) %>% #Se filtra los meses que se mostrarán en el reporte
  mutate(monDate=as.yearmon(monDate)) %>% 
  arrange(monDate) %>% 
  select(saldous, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = saldous, values_fill = 0) %>% #Formato de matriz
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,`F`,S,Z) %>% 
  # select(-Z) %>% #Uncomment if the criteria is following operations forward
  glimpse

tm2022_difPrev <- tm_ops %>% 
  dplyr::filter(monDate >= lastmonth12 & monDate< lastmonth) %>% #Se filtra los meses que se mostrarán en el reporte
  arrange(monDate) %>% 
  select(difPrev, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = difPrev,values_fill = 0) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,`F`,S,Z) %>% 
  select(-Z)

MatTrans <- list(prob=tm2022_prob,ops=tm2022_ops,saldo=tm2022_saldo,difPrev=tm2022_difPrev)
write.xlsx(MatTrans,paste0('D:/!bso/transMat/tm_',shortmonth,'_v2.xlsx'))

tm_cumprob <- tm2022_prob %>% 
  group_by(monDate) %>% 
  mutate(perm = case_when(cm1=="A"~A,cm1=="B"~B,cm1=="C"~C,cm1=="D"~D,
                          cm1=="E"~E,cm1=="F"~`F`,cm1=="S"~S,)) %>% #Se calcula la probabilidad de permanencia
  mutate(deter = case_when(cm1=="A"~B+C+D+E+`F`+S,
                           cm1=="B"~C+D+E+`F`+S,
                           cm1=="C"~D+E+`F`+S,
                           cm1=="D"~E+`F`+S,
                           cm1=="E"~`F`+S,
                           cm1=="F"~S,
                           cm1=="S"~0,)) %>% #Se calcula la probabilidad de deterioro
  mutate(recup = case_when(cm1=="A"~Z,
                           cm1=="B"~A+Z,
                           cm1=="C"~A+B+Z,
                           cm1=="D"~A+B+C+Z,
                           cm1=="E"~A+B+C+D+Z,
                           cm1=="F"~A+B+C+D+E+Z,
                           cm1=="S"~Z,)) %>% #Se calcula la probabilidad de recuperación
  select(recup,deter,perm) %>%
  ungroup()

tm_cumops <- tm2022_ops %>% 
  group_by(monDate) %>% 
  mutate(perm = case_when(cm1=="A"~A,cm1=="B"~B,cm1=="C"~C,cm1=="D"~D,
                          cm1=="E"~E,cm1=="F"~`F`,cm1=="S"~S,)) %>% #Se calcula la permanencia de operaciones
  mutate(deter = case_when(cm1=="A"~B+C+D+E+`F`+S,
                           cm1=="B"~C+D+E+`F`+S,
                           cm1=="C"~D+E+`F`+S,
                           cm1=="D"~E+`F`+S,
                           cm1=="E"~`F`+S,
                           cm1=="F"~S,
                           cm1=="S"~0,)) %>% #Se calcula la cantidad de deterioros
  mutate(recup = case_when(cm1=="A"~Z,
                           cm1=="B"~A+Z,
                           cm1=="C"~A+B+Z,
                           cm1=="D"~A+B+C+Z,
                           cm1=="E"~A+B+C+D+Z,
                           cm1=="F"~A+B+C+D+E+Z,
                           cm1=="S"~Z,)) %>% #Se calcula la cantidad de recuperaciones
  select(recup,deter,perm) %>%
  ungroup()

tm_cumsaldo <- tm2022_saldo %>% 
  group_by(monDate) %>% 
  mutate(perm = case_when(cm1=="A"~A,cm1=="B"~B,cm1=="C"~C,cm1=="D"~D,
                          cm1=="E"~E,cm1=="F"~`F`,cm1=="S"~S,)) %>% #Se calcula la permanencia en saldo
  mutate(deter = case_when(cm1=="A"~B+C+D+E+`F`+S,
                           cm1=="B"~C+D+E+`F`+S,
                           cm1=="C"~D+E+`F`+S,
                           cm1=="D"~E+`F`+S,
                           cm1=="E"~`F`+S,
                           cm1=="F"~S,
                           cm1=="S"~0,)) %>% #Se calcula el deterioro en saldo 
  mutate(recup = case_when(cm1=="A"~0,
                           cm1=="B"~A,
                           cm1=="C"~A+B,
                           cm1=="D"~A+B+C,
                           cm1=="E"~A+B+C+D,
                           cm1=="F"~A+B+C+D+E,
                           cm1=="S"~0,)) %>% #Se calcula la recuperación en saldo
  select(recup,deter,perm) %>%
  ungroup()
####____DETERIOROS DESDE A____####
tm_A <- tm_ops %>% 
  dplyr::filter(monDate >= 'ene. 2022' & monDate <= 'dic. 2022') %>% 
  select(monDate, cm1, cmt, one, saldous) %>% 
  mutate(one=as.double(one)) %>% 
  dplyr::filter(cm1=='A') %>% 
  group_by(monDate) %>% 
  mutate(Ops_A = sum(one)) %>% 
  mutate(Ops_det = sum(one[cmt %in% c('B','C','D','E','F','S')])) %>%
  mutate(Saldo_det = sum(saldous[cmt %in% c('B','C','D','E','F','S')])) %>% 
  ungroup() %>% 
  select(-cm1,-cmt,-saldous,-one) %>% 
  group_by(monDate) %>% 
  summarise_all(max) %>% 
  ungroup() %>% 
  mutate(monDate=as.Date(monDate))
write.xlsx(tm_A,'D:/!bso/transMat/tmA_Ene23.xlsx')  
  