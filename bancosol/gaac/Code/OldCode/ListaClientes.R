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
####____HISTORIAL____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2018)
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
clist <- list()
# agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")
# file_list <- list.files(path='D:/!bso/girCartera/rdsGAR')
bdcList <- list()
i <- 1
for (i in 1:(length(myrds)-1)) {
  tryCatch({
    k <- i+1
    print(myrds[i])
    print(myrds[k])
    if(i==1){
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                            myrds[i],'.rds')) %>% 
        mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
        select(CTACLIENTE,OPERACION,CI,GENERO,MONTOUS,MONEDA,fdes,AGENCIA,ASESOR,NOMBRE_ASESOR,
               monDate,CALIFICACION,DIASMORA,OPERACION_ORI_REF,ctaCont)
      dfTotal <- df1 %>% 
        mutate(moraMax = DIASMORA) %>% 
        mutate(peorCalif = CALIFICACION) %>% 
        mutate(fueRefin = if_else(OPERACION_ORI_REF!=0,1,0)) %>% 
        mutate(FechaRefin = if_else(fueRefin==1,fdes,as.Date(NA))) %>% 
        mutate(fueReprog = if_else(ctaCont %in% c('135','136','137'),1,0)) %>% 
        mutate(FechaReprog = if_else(fueReprog==1,fdes,as.Date(NA))) %>% 
        mutate(fueCast = if_else(ctaCont=='865',1,0)) %>% 
        mutate(FechaCast = if_else(fueCast==1,monDate-1/12,as.yearmon(NA)))
    }else{
      df1 <- df2
    }
    df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                          myrds[k],'.rds')) %>% 
      mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
      select(CTACLIENTE,OPERACION,CI,GENERO,MONTOUS,MONEDA,fdes,AGENCIA,ASESOR,NOMBRE_ASESOR,
             monDate,CALIFICACION,DIASMORA,OPERACION_ORI_REF,ctaCont)
    
    dfPrev <- df1 %>% 
      select(OPERACION,CTACLIENTE,monDate,CALIFICACION,DIASMORA)
    dfLast <- df2 %>% 
      select(OPERACION,CTACLIENTE,monDate,CALIFICACION,DIASMORA, OPERACION_ORI_REF,ctaCont)
    
    dfTotal <- dfTotal %>% #Aquí se actualiza la info del mes previo
      left_join(dfPrev,by=c("OPERACION","CTACLIENTE")) %>% 
      mutate(DIASMORA.y = if_else(is.na(DIASMORA.y),DIASMORA.x,DIASMORA.y)) %>% 
      mutate(CALIFICACION.y = if_else(is.na(CALIFICACION.y),CALIFICACION.x,CALIFICACION.y)) %>% 
      mutate(monDate.y = if_else(is.na(monDate.y),monDate.x,monDate.y)) %>%   
      mutate(monDate.y = as.yearmon(monDate.y)) %>% 
      dplyr::rename(CALIFICACION = CALIFICACION.y, 
             DIASMORA = DIASMORA.y,
             monDate = monDate.y) %>% 
      select(-DIASMORA.x,-CALIFICACION.x,-monDate.x)
    dfTotal <- dfTotal %>%  #Aquí se actualiza la info del mes actual
      left_join(dfLast,by=c("OPERACION","CTACLIENTE")) %>% 
      group_by(OPERACION,CTACLIENTE) %>% 
      mutate(moraMax = max(DIASMORA.x,DIASMORA.y,na.rm = T)) %>% 
      mutate(peorCalif = max(CALIFICACION.x,CALIFICACION.y,na.rm = T)) %>% 
      mutate(fueRefin = ifelse(OPERACION_ORI_REF.x==0 & OPERACION_ORI_REF.y!=0,1,0)) %>% 
      mutate(fueReprog = ifelse(ctaCont.x %in% c('131','133','134') & ctaCont.y %in% c('135','136','137'),1,0)) %>% 
      mutate(fueCast = ifelse(ctaCont.x %in% c('131','133','134') & ctaCont.y %in% c('135','136','137'),1,0)) %>% 
      mutate(FechaRefin = ifelse(OPERACION_ORI_REF.x==0 & OPERACION_ORI_REF.y!=0,as.character(monDate),NA)) %>% 
      mutate(FechaReprog = ifelse(ctaCont.x %in% c('131','133','134') & ctaCont.y %in% c('135','136','137'),as.character(monDate),NA)) %>% 
      ungroup() %>% 
      mutate(DIASMORA.y = ifelse(is.na(DIASMORA.y),DIASMORA.x,DIASMORA.y)) %>% 
      mutate(CALIFICACION.y = ifelse(is.na(CALIFICACION.y),CALIFICACION.x,CALIFICACION.y)) %>% 
      mutate(monDate.y = ifelse(is.na(monDate.y),monDate.x,monDate.y)) %>%   
      mutate(monDate.y = as.yearmon(monDate.y)) %>% 
      dplyr::rename(CALIFICACION = CALIFICACION.y, 
             DIASMORA = DIASMORA.y,
             monDate = monDate.y) %>% 
      select(-DIASMORA.x,-CALIFICACION.x,-monDate.x)
    OpsNuevas <- df2 %>%
      anti_join(dfTotal,by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(moraMax = DIASMORA) %>% 
      mutate(peorCalif = CALIFICACION) %>% 
      mutate(fueRefin = ifelse(OPERACION_ORI_REF!=0,1,0)) %>% 
      mutate(FechaRefin = ifelse(fueRefin==1,as.character(monDate),NA)) %>% 
      mutate(fueReprog = 0) %>% 
      mutate(FechaReprog = ifelse(fueRefin==1,as.character(monDate),NA))
    dfTotal <- dfTotal %>% 
      bind_rows(OpsNuevas)  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

n_distinct(dfTotal$ASESOR)
n_distinct(dfTotal$NOMBRE_ASESOR)

x <- dfTotal %>% 
  group_by(ASESOR) %>% 
  mutate(len = n_distinct(NOMBRE_ASESOR)) %>%
  ungroup() %>% 
  dplyr::filter(len>1)
dfTotal_fixed <- dfTotal %>% 
  group_by(ASESOR) %>% 
  mutate(NOMBRE_ASESOR = max(NOMBRE_ASESOR[which(nchar(NOMBRE_ASESOR)==max(nchar(NOMBRE_ASESOR)))],na.rm = T)) %>% 
  ungroup()

y <- dfTotal_fixed %>% 
  group_by(NOMBRE_ASESOR) %>% 
  mutate(len=n_distinct(ASESOR)) %>% 
  ungroup() %>% 
  dplyr::filter(len>1)
write_rds(dfTotal_fixed,'D:/!bso/features/Clientes_Ene15Abr23.rds')

dfTotal <- OpsBSO

####____FEATURES____####
####____HISTORIAL____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2022,2023)
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
clist <- list()
# agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")
# file_list <- list.files(path='D:/!bso/girCartera/rdsGAR')
bdcList <- list()
i <- 100
for (i in 1:(length(myrds))) {
  tryCatch({
    print(myrds[i])
    if(i==1){
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                            myrds[i],'.rds')) %>% 
        mutate(NOMBRE = paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT)) %>% 
        mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
        select(CTACLIENTE,OPERACION,CI,NOMBRE,GENERO,MONTOUS,MONEDA,fdes,AGENCIA,ASESOR,NOMBRE_ASESOR,
               monDate,CALIFICACION,DIASMORA,OPERACION_ORI_REF,ctaCont)
      dfTotal <- df1 %>% 
        mutate(moraMax = DIASMORA) %>% 
        mutate(peorCalif = CALIFICACION) %>% 
        mutate(fueRefin = if_else(OPERACION_ORI_REF!=0,1,0)) %>% 
        mutate(fueReprog = if_else(ctaCont %in% c('135','136','137'),1,0)) %>%
        mutate(fueCast = if_else(ctaCont %in% c('865'),1,0)) %>% 
        mutate(NReprog = fueReprog) %>% 
        mutate(FechaRefin = if_else(fueRefin==1,fdes,as.Date(NA))) %>% 
        mutate(FechaReprog = if_else(fueReprog==1,fdes,as.Date(NA))) %>% 
        mutate(FechaCast = if_else(fueCast==1,monDate-1/12,as.yearmon(NA))) 
    }else{
      df2 <- df1
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                            myrds[i],'.rds')) %>% 
        mutate(NOMBRE = paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT)) %>% 
        mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
        select(CTACLIENTE,OPERACION,CI,NOMBRE,GENERO,MONTOUS,MONEDA,fdes,AGENCIA,ASESOR,NOMBRE_ASESOR,
               monDate,CALIFICACION,DIASMORA,OPERACION_ORI_REF,ctaCont) 
      dfNew <- df1 %>% 
        anti_join(dfTotal,by=c("CTACLIENTE","OPERACION")) %>% 
        mutate(moraMax = DIASMORA) %>% 
        mutate(peorCalif = CALIFICACION) %>% 
        mutate(fueRefin = if_else(OPERACION_ORI_REF!=0,1,0)) %>% 
        mutate(fueReprog = if_else(ctaCont %in% c('135','136','137'),1,0)) %>%
        mutate(fueCast = if_else(ctaCont %in% c('865'),1,0)) %>% 
        mutate(NReprog = fueReprog) %>% 
        mutate(FechaRefin = if_else(fueRefin==1,fdes,as.Date(NA))) %>% 
        mutate(FechaReprog = if_else(fueReprog==1,fdes,as.Date(NA))) %>% 
        mutate(FechaCast = if_else(fueCast==1,monDate-1/12,as.yearmon(NA))) 
      dfUpdate <- df1 %>% 
        select(-CI,-NOMBRE,-GENERO,-MONTOUS,-MONEDA,-AGENCIA,-ASESOR,-NOMBRE_ASESOR)
      dfOld <- dfTotal %>% 
        inner_join(dfUpdate,by=c("CTACLIENTE","OPERACION")) %>% 
        mutate(DIASMORA.y = if_else(is.na(DIASMORA.y),DIASMORA.x,DIASMORA.y)) %>% 
        mutate(CALIFICACION.y = if_else(is.na(CALIFICACION.y),CALIFICACION.x,CALIFICACION.y)) %>% 
        group_by(CTACLIENTE,OPERACION) %>% 
        mutate(moraMax = max(DIASMORA.x,DIASMORA.y,na.rm = T)) %>% 
        mutate(peorCalif = max(CALIFICACION.x,CALIFICACION.y,na.rm = T)) %>% 
        ungroup() %>% 
        mutate(cambio = case_when(fueRefin==0 & OPERACION_ORI_REF.y!=0~1,
                                  fueRefin==1 & OPERACION_ORI_REF.y!=OPERACION_ORI_REF.x & OPERACION_ORI_REF.y!=0~1,
                                  fueRefin==1 & !(ctaCont.y %in% c('135','136','137')) & fdes.y>FechaRefin~1,
                                  TRUE~0)) %>% 
        mutate(fueRefin = if_else(fueRefin==0 & cambio==1,1,fueRefin)) %>% 
        mutate(FechaRefin = if_else(cambio==1,fdes.y,FechaRefin)) %>% 
        mutate(OPERACION_ORI_REF = if_else(cambio==1,OPERACION_ORI_REF.y,OPERACION_ORI_REF.x)) %>% 
        mutate(cambio = case_when(fueReprog==0 & ctaCont.y %in% c('135','136','137')~1,
                                  fueReprog==1 & ctaCont.y %in% c('135','136','137') & fdes.y>FechaReprog~1,
                                  TRUE~0)) %>% 
        mutate(fueReprog = if_else(fueReprog==0 & cambio==1,1,fueReprog)) %>% 
        mutate(FechaReprog = if_else(cambio==1, fdes.y,FechaReprog)) %>% 
        mutate(NReprog = if_else(cambio==1,NReprog+1,NReprog)) %>% 
        mutate(FechaCast = if_else(fueCast==0 & ctaCont.y =='865',monDate.y,FechaCast)) %>%
        mutate(fueCast = if_else(fueCast==0 & ctaCont.y =='865',1,fueCast)) %>%
        # mutate(FechaRefin = ifelse(fueRefin==0 & OPERACION_ORI_REF.y!=0,as.character(fdes.y),FechaRefin)) %>% 
        # mutate(FechaReprog = ifelse(fueReprog==0 & ctaCont.y %in% c('135','136','137'),as.character(fdes.y),FechaReprog)) %>% 
        # mutate(FechaCast = ifelse(fueCast==0 & (ctaCont.y =='865'),as.character(monDate.y),FechaCast)) %>% 
        # mutate(fueReprog = ifelse(fueReprog==0 & ctaCont.y %in% c('135','136','137'),1,fueReprog)) %>% 
        #Si una op refin es reprogramada luego, se elimina su op_ori_ref por eso nos quedamos con la primera op_ori_ref
        select(CTACLIENTE,OPERACION,CI,NOMBRE,GENERO,MONTOUS,MONEDA,AGENCIA,ASESOR,NOMBRE_ASESOR,
               moraMax,peorCalif,fueRefin,FechaRefin,fueReprog,FechaReprog,NReprog,fueCast,FechaCast,
               CALIFICACION = CALIFICACION.y,
               DIASMORA = DIASMORA.y,
               ctaCont = ctaCont.y,
               OPERACION_ORI_REF,
               monDate = monDate.y,
               fdes = fdes.x)
      dfCancel <- dfTotal %>% 
        anti_join(df1,by=c("CTACLIENTE","OPERACION"))
      
      dfTotal <- dfCancel %>% 
        bind_rows(dfOld) %>% 
        bind_rows(dfNew)
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

n_distinct(dfTotal$ASESOR)
n_distinct(dfTotal$NOMBRE_ASESOR)

x <- dfTotal %>% 
  group_by(ASESOR) %>% 
  mutate(len = n_distinct(NOMBRE_ASESOR)) %>%
  ungroup() %>% 
  dplyr::filter(len>1)
dfTotal_fixed <- dfTotal %>% 
  group_by(ASESOR) %>% 
  mutate(NOMBRE_ASESOR = max(NOMBRE_ASESOR[which(nchar(NOMBRE_ASESOR)==max(nchar(NOMBRE_ASESOR)))],na.rm = T)) %>% 
  ungroup()

y <- dfTotal_fixed %>% 
  group_by(NOMBRE_ASESOR) %>% 
  mutate(len=n_distinct(ASESOR)) %>% 
  ungroup() %>% 
  dplyr::filter(len>1)
write_rds(dfTotal_fixed,'D:/!bso/features/Clientes_Ene15Abr23_v5.rds')

dfTotal <- readRDS('D:/!bso/features/Clientes_Ene15Abr23_v3.rds')

CARNETS <- dfTotal %>% 
  group_by(CTACLIENTE) %>% 
  dplyr::filter(n_distinct(CI)>1) %>% 
  arrange(CTACLIENTE,monDate)
sapply(CARNETS, n_distinct)

CTACLIENTES <- dfTotal %>% 
  group_by(CTACLIENTE) %>% 
  arrange(desc(monDate)) %>% 
  mutate(CI=CI[row_number()==1]) %>% 
  ungroup()

CTASREP <- CTACLIENTES %>% 
  group_by(CI) %>% 
  dplyr::filter(n_distinct(CTACLIENTE)>1) %>% 
  arrange(CI,monDate)
sapply(CTASREP, n_distinct)

GENEROS <- dfTotal %>% 
  group_by(CTACLIENTE) %>% 
  dplyr::filter(n_distinct(GENERO)>1) %>% 
  arrange(CTACLIENTE,monDate)

REFINS_SIN_CAMBIO <- dfTotal %>% #fueron refinanciadas, pero no les cambiaron la operación
  dplyr::filter(fueRefin==1 & OPERACION_ORI_REF==0)
####____SECUENCIA DESDE 2015____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2020:2023)
myfecha <- as.vector(sapply(year,function(x){paste0(mes,". ",x)})) #lista de meses-años para abrir
myfecha <- myfecha[-c(which(myfecha=="Abr. 2023"):length(myfecha))]
secList <- list()
for (i in 1:length(myfecha)) {
  dfAux <- dfTotal %>% 
    mutate(mydes = as.yearmon(fdes)) %>% 
    dplyr::filter(mydes < as.yearmon(myfecha[i])) %>% 
    group_by(CTACLIENTE) %>% 
    dplyr::filter(fueRefin!=1) %>% 
    summarise(N_SEC = n_distinct(OPERACION))
  secList[[i]] <- dfAux
}


Sec <- dfTotal %>% 
  dplyr::filter(fdes < as.Date("2023-04-01")) %>% 
  group_by(CTACLIENTE) %>% 
  dplyr::filter(fueRefin!=1) %>% 
  summarise(N_SEC = n_distinct(OPERACION))

MasterKeyList <- readRDS("D:/!bso/accion/MasterKeyList.rds")

length(which(!MasterKeyList$OPERACION %in% dfTotal$OPERACION))
