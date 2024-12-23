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
####____CREATING BDC CANCEL____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
lag <- 1
clist <- list()
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
dfCancel <- readRDS("D:/!bso/Consultas/dfCancelEne15May23.rds")
bdcCancel <- readRDS('D:/!bso/transMat/matCancel.rds')
myrds <- c('May2023','Jun2023') #Colocar nombre MonthYYYY del mes previo y mes actual
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
saveRDS(dfCancel,"D:/!bso/Consultas/dfCancelEne15Jun23.rds") #change name of file

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
####____TRANSICIONES CON LAG VARIABLE____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
lag <- 1
clist <- list()
# dlist <- list()
# codAge <- read_excel("D:/!bso/bases/excel/CodAgeSucReg.xlsx")
i <- 99
for(i in 1:(length(myrds)-lag)) {
  tryCatch({
    print(i)
    print(myrds[i])
    k <- i + lag
    print(myrds[i+lag])
    
    df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i], '.rds')) %>%  #se abre mes anterior
      dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>% 
      mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
      mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S", CALIFICACION)) %>% 
      select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus)
    
    df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[k], '.rds')) %>%  #se abre mes posterior 
      dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>%
      mutate(saldous=ifelse(saldoCast>0, saldoCast, saldous)) %>% 
      mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S",CALIFICACION)) %>% 
      select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus)
    
    dfCancel <- df1 %>% 
      anti_join(df2,by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(CALIFICACION = "Z") %>% 
      mutate(saldous = 0) %>% 
      mutate(previus = 0) %>% 
      mutate(monDate = monDate+1/12) %>% 
      bind_rows(df2)
    
    dfTrans <- df1 %>% 
      left_join(dfCancel, by=c("CTACLIENTE","OPERACION"), suffix = c("_ini","_fin")) %>% 
      mutate(trans = paste(CALIFICACION_ini, CALIFICACION_fin,sep="-")) %>% 
      mutate(difPrev = previus_fin - previus_ini) %>% 
      mutate(difSaldo = saldous_fin - saldous_ini) %>% 
      mutate(one = 1) %>% 
      select(monDate = monDate_ini, trans, cm1 = CALIFICACION_ini, cmt = CALIFICACION_fin, 
             saldous_ini, saldous = saldous_fin, one, difPrev, difSaldo) %>% 
      group_by(monDate, trans, cm1, cmt) %>% 
      summarise_all(sum,na.rm=T) %>% 
      ungroup() %>% 
      group_by(monDate, cm1) %>% 
      mutate(prob = one/sum(one)) %>% 
      mutate(probS = saldous/sum(saldous))
    
    clist[[i]] <- dfTrans
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

tm_ops <- rbindlist(clist)

tail(tm_ops %>%
       group_by(monDate) %>% 
       summarise(Saldo=sum(saldous),n=sum(one)))
#Se almacena la base de recuento de todas las transiciones
write.csv(tm_ops, "D:/!bso/transMat/Oreports/tmAll_May23.csv", row.names = F)

####____AGREGAR UN MES A TM_OPS____####
tm_ops <- read.csv("D:/!bso/transMat/Oreports/tmAll_Jun23.csv") %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  # dplyr::filter(monDate!="Abr. 2023") %>% 
  glimpse()
myrds <- c("Jul2023","Ago2023") #Colocar los meses adecuados de acuerdo con el lag

df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[1], '.rds')) %>%  #se abre mes anterior
  # dplyr::filter(!(MODULO==118 | (MODULO ==121 & str_detect(TIPO_OPER,"MIGR")))) %>% #Sin Cartera Migrada
  dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>% 
  mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S", CALIFICACION)) %>%
  select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus)

df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[2], '.rds')) %>%  #se abre mes posterior 
  # dplyr::filter(!(MODULO==118 | (MODULO ==121 & str_detect(TIPO_OPER,"MIGR")))) %>% #Sin Cartera Migrada
  dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>%
  mutate(saldous=ifelse(saldoCast>0, saldoCast, saldous)) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S",CALIFICACION)) %>% 
  select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus)

dfCancel <- df1 %>% 
  anti_join(df2,by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(CALIFICACION = "Z") %>% 
  mutate(saldous = 0) %>% 
  mutate(previus = 0) %>% 
  mutate(monDate = monDate+1/12) %>% 
  bind_rows(df2)

dfTrans <- df1 %>% 
  left_join(dfCancel, by=c("CTACLIENTE","OPERACION"), suffix = c("_ini","_fin")) %>% 
  mutate(trans = paste(CALIFICACION_ini, CALIFICACION_fin,sep="-")) %>% 
  mutate(difPrev = previus_fin - previus_ini) %>% 
  mutate(difSaldo = saldous_fin - saldous_ini) %>% 
  mutate(one = 1) %>% 
  select(monDate = monDate_ini, trans, cm1 = CALIFICACION_ini, cmt = CALIFICACION_fin, 
         saldous_ini, saldous = saldous_fin, one, difPrev, difSaldo) %>% 
  group_by(monDate, trans, cm1, cmt) %>% 
  summarise_all(sum,na.rm=T) %>% 
  ungroup() %>% 
  group_by(monDate, cm1) %>% 
  mutate(prob = one/sum(one)) %>% 
  mutate(probS = saldous/sum(saldous))

tm_ops <- tm_ops %>% 
  bind_rows(dfTrans)
tail(tm_ops %>%
       group_by(monDate) %>% 
       summarise(Saldo=sum(saldous),n=sum(one)))
write.csv(tm_ops, "D:/!bso/transMat/Oreports/tmAll_Ago23.csv", row.names = F)
# saveRDS(cFull, "D:/!bso/transMat/ctaCont/Trans_ctaCont.rds")
####____CHECKS DE REPORTE____####
#ESTA SECCIÓN NO ES NECESARIA PUESTO QUE LOS RESULTADOS QUE SE OBTIENEN A PARTIR
#DE AQUÍ SE MUESTRAN EN EL REPORTE, PERO SE DEJA AL USUARIO COMPILAR O MODIFICAR
#ESTA SECCIÓN EN LUGAR DE SIMPLEMENTE VER EL REPORTE
tm_ops <- read.csv("D:/!bso/transMat/Oreports/tmAll_Jun23.csv") %>% 
  mutate(monDate=as.yearmon(monDate)+1/12) #Para poner el mes de cancelación en lugar del mes de origen +1/12
lastmonth <- "Jun. 2023"
lastmonth12 <- "Jul. 2022"
shortmonth <- str_replace(lastmonth,". ","")
shortmonth12 <- str_replace(lastmonth12,". ","")

tm2022_prob <- tm_ops %>% 
  dplyr::filter(monDate >= lastmonth12 & monDate <= lastmonth) %>% #Se filtra los meses que se mostrarán en el reporte
  select(prob, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>% #Se coloca en formato de matriz
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,`F`,S,Z) %>% 
  mutate(recup = case_when(cm1=="A"~Z,
                           cm1=="B"~A+Z,
                           cm1=="C"~A+B+Z,
                           cm1=="D"~A+B+C+Z,
                           cm1=="E"~A+B+C+D+Z,
                           cm1=="F"~A+B+C+D+E+Z,
                           cm1=="S"~Z,)) %>% #Se calcula la probabilidad de recuperación
  mutate(perm = case_when(cm1=="A"~A,cm1=="B"~B,cm1=="C"~C,cm1=="D"~D,
                          cm1=="E"~E,cm1=="F"~`F`,cm1=="S"~S,)) %>% #Se calcula la probabilidad de permanencia
  mutate(deter = case_when(cm1=="A"~B+C+D+E+`F`+S,
                           cm1=="B"~C+D+E+`F`+S,
                           cm1=="C"~D+E+`F`+S,
                           cm1=="D"~E+`F`+S,
                           cm1=="E"~`F`+S,
                           cm1=="F"~S,
                           cm1=="S"~0,)) #Se calcula la probabilidad de deterioro

tm2022_ops <- tm_ops %>% 
  dplyr::filter(monDate >= lastmonth12 & monDate <= lastmonth) %>% #Se filtra los meses que se mostrarán en el reporte
  mutate(one=as.double(one)) %>% 
  arrange(monDate) %>% 
  select(one, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = one, values_fill = 0) %>% #Formato de matriz
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,`F`,S,Z) %>% 
  mutate(recup = case_when(cm1=="A"~Z,
                           cm1=="B"~A+Z,
                           cm1=="C"~A+B+Z,
                           cm1=="D"~A+B+C+Z,
                           cm1=="E"~A+B+C+D+Z,
                           cm1=="F"~A+B+C+D+E+Z,
                           cm1=="S"~Z,)) %>% #Se calcula la recuperación en operaciones
  mutate(perm = case_when(cm1=="A"~A,cm1=="B"~B,cm1=="C"~C,cm1=="D"~D,
                          cm1=="E"~E,cm1=="F"~`F`,cm1=="S"~S,)) %>% #Se calcula la permanencia en operaciones
  mutate(deter = case_when(cm1=="A"~B+C+D+E+`F`+S,
                           cm1=="B"~C+D+E+`F`+S,
                           cm1=="C"~D+E+`F`+S,
                           cm1=="D"~E+`F`+S,
                           cm1=="E"~`F`+S,
                           cm1=="F"~S,
                           cm1=="S"~0,)) #Se calcula el deterioro en operaciones

tm2022_saldo <- tm_ops %>% 
  dplyr::filter(monDate >= lastmonth12 & monDate <= lastmonth) %>% #Se filtra los meses que se mostrarán en el reporte
  mutate(monDate=as.yearmon(monDate)) %>% 
  arrange(monDate) %>% 
  select(saldous, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = saldous, values_fill = 0) %>% #Formato de matriz
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,`F`,S,Z) %>% 
  # select(-Z) %>% #Uncomment if the criteria is following operations forward
  mutate(recup = case_when(cm1=="A"~Z,
                           cm1=="B"~A+Z,
                           cm1=="C"~A+B+Z,
                           cm1=="D"~A+B+C+Z,
                           cm1=="E"~A+B+C+D+Z,
                           cm1=="F"~A+B+C+D+E+Z,
                           cm1=="S"~Z,)) %>% #Se calcula la recuperación en saldo
  mutate(perm = case_when(cm1=="A"~A,cm1=="B"~B,cm1=="C"~C,cm1=="D"~D,
                          cm1=="E"~E,cm1=="F"~`F`,cm1=="S"~S,)) %>% #Se calcula la permanencia en saldo
  mutate(deter = case_when(cm1=="A"~B+C+D+E+`F`+S,
                           cm1=="B"~C+D+E+`F`+S,
                           cm1=="C"~D+E+`F`+S,
                           cm1=="D"~E+`F`+S,
                           cm1=="E"~`F`+S,
                           cm1=="F"~S,
                           cm1=="S"~0,)) #Se calcula el deterioro en saldo

tm2022_difPrev <- tm_ops %>% 
  dplyr::filter(monDate >= lastmonth12 & monDate <= lastmonth) %>% #Se filtra los meses que se mostrarán en el reporte
  arrange(monDate) %>% 
  select(difPrev, cm1, cmt, monDate) %>% 
  pivot_wider(names_from = cmt, values_from = difPrev,values_fill = 0) %>% 
  arrange(monDate, cm1) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  relocate(monDate,cm1,A,B,C,D,E,`F`,S,Z) %>% 
  select(-Z) %>% 
  mutate(recup = case_when(cm1=="A"~0,
                           cm1=="B"~A,
                           cm1=="C"~A+B,
                           cm1=="D"~A+B+C,
                           cm1=="E"~A+B+C+D,
                           cm1=="F"~A+B+C+D+E,
                           cm1=="S"~0,)) %>% #Se calcula la recuperación en dif de prev
  mutate(perm = case_when(cm1=="A"~A,cm1=="B"~B,cm1=="C"~C,cm1=="D"~D,
                          cm1=="E"~E,cm1=="F"~`F`,cm1=="S"~S,)) %>% #Se calcula la permanencia en dif prev
  mutate(deter = case_when(cm1=="A"~B+C+D+E+`F`+S,
                           cm1=="B"~C+D+E+`F`+S,
                           cm1=="C"~D+E+`F`+S,
                           cm1=="D"~E+`F`+S,
                           cm1=="E"~`F`+S,
                           cm1=="F"~S,
                           cm1=="S"~0,)) #Se calcula el deterioro en dif prev


MatTrans <- list(prob=tm2022_prob,ops=tm2022_ops,saldo=tm2022_saldo,difPrev=tm2022_difPrev)
write.xlsx(MatTrans,paste0('D:/!bso/transMat/tm_',shortmonth,'_v2.xlsx'))
