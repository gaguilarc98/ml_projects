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
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____CANCELADOS Y CARTERA POR RANGO PLAZOS____####
####____TRANSICIONES CON LAG VARIABLE____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
lag <- 1
cancelList <- list()
# dlist <- list()
# codAge <- read_excel("D:/!bso/bases/excel/CodAgeSucReg.xlsx")
i <- 1
for(i in 1:(length(myrds)-lag)) {
  tryCatch({
    print(i)
    print(myrds[i])
    k <- i + lag
    print(myrds[i+lag])
    
    df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i], '.rds')) %>%  #se abre mes anterior
      mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
      # mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S", CALIFICACION)) %>% 
      mutate(MONTOUS = ifelse(MONEDA == 0, MONTO/6.86, MONTO)) %>% 
      mutate(MONTOUS = ifelse(MONTOUS < saldous, saldous, MONTOUS)) %>% 
      select(OPERACION, CI, CTACLIENTE, monDate, CALIFICACION, MONTOUS, saldous,
             OPERACION_ORI_REF, fdes, FFINALIZA, ctaCont, PLAZODIAS, DIASMORA)
    
    df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[k], '.rds')) %>% 
      mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
      # mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S", CALIFICACION)) %>% 
      mutate(MONTOUS = ifelse(MONEDA == 0, MONTO/6.86, MONTO)) %>% 
      mutate(MONTOUS = ifelse(MONTOUS < saldous, saldous, MONTOUS)) %>% 
      select(OPERACION, CI, CTACLIENTE, monDate, CALIFICACION, MONTOUS, saldous,
             OPERACION_ORI_REF, fdes, FFINALIZA, ctaCont, PLAZODIAS, DIASMORA)
    
    dfCancel <- df1 %>% 
      anti_join(df2, by = c("CTACLIENTE","OPERACION")) %>% 
      mutate(FueRefin = ifelse(OPERACION %in% df2$OPERACION_ORI_REF, 1, 0)) %>% 
      mutate(monDate = monDate+1/12) %>% 
      mutate(CALIFICACION = "Z") %>% 
      mutate(saldous = 0) %>% 
      mutate(previus = 0) %>% 
      mutate(TipoReduccion = case_when(as.yearmon(FFINALIZA)==monDate ~ '1. Regular',
                                       as.yearmon(FFINALIZA)>monDate ~ '2. Anticipado',
                                       as.yearmon(FFINALIZA)<monDate ~ '3. Intempestivo')) %>% 
      bind_rows(df2) %>% 
      select(-OPERACION_ORI_REF)
    
    dfTrans <- df1 %>% 
      left_join(dfCancel, by=c("CTACLIENTE","OPERACION"), suffix = c("_ini","_fin")) %>% 
      # mutate(trans = paste(CALIFICACION_ini, CALIFICACION_fin,sep="-")) %>% 
      # mutate(difPrev = previus_fin - previus_ini) %>% 
      mutate(FueRefin = ifelse(is.na(FueRefin), 0,FueRefin)) %>% 
      mutate(Fecha = as.Date(monDate_fin, frac=1)) %>% 
      mutate(PlazoRemanente = FFINALIZA_ini - Fecha) %>% 
      mutate(RangoPlazo = case_when(PlazoRemanente <=30 ~ '1. 30 días',
                                    PlazoRemanente <=60 ~ '2. 60 días',
                                    PlazoRemanente <=90 ~ '3. 90 días',
                                    PlazoRemanente <=180 ~ '4. 180 días',
                                    PlazoRemanente <=360 ~ '5. 360 días',
                                    PlazoRemanente <=720 ~ '6. 720 días',
                                    PlazoRemanente > 720 ~ '7. >720 días',
                                    TRUE ~ '0. Otros')) %>% 
      mutate(Operaciones = 1) %>% 
      group_by(Fecha, RangoPlazo) %>% 
      mutate(SaldoTotal = sum(saldous_ini),
             OpsTotal = sum(Operaciones)) %>% 
      ungroup() %>% 
      dplyr::filter(CALIFICACION_fin=='Z') %>% 
      group_by(Fecha, RangoPlazo, TipoReduccion) %>% 
      summarise(saldous_ini = sum(saldous_ini),
                ops_ini = sum(Operaciones),
                SaldoTotal = max(SaldoTotal),
                OpsTotal = max(OpsTotal)) %>% 
      ungroup()
    
    cancelList[[i]] <- dfTrans
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
cancelFull <- rbindlist(cancelList)
write_xlsx(cancelFull, "D:/!bso/calce/SustentoCancelados_Jun2023.xlsx")

####____TRANSICIONES POR CALIF____#####
tm_ops <- read.csv("D:/!bso/transMat/Oreports/tmAll_Jun23.csv") %>% 
  mutate(monDate = as.yearmon(monDate)+1/12) 
####____TRANSICIONES POR CTACONT____####
#CON CUENTA CONTABLE
tm_cont <- readRDS("D:/!bso/transMat/ctaCont/Trans_ctaCont_Lag1_v2.rds") %>% 
  mutate(monDate = monDate+1/12) %>% 
  mutate(Fecha = as.Date(monDate, frac=1)) %>% 
  mutate(ctaCont_ini = substr(ctaCont_ini,1,3)) %>% 
  dplyr::filter(ctaCont_ini!='865') %>% 
  group_by(Fecha, ctaCont_ini) %>% 
  summarise(Saldo = sum(saldous_ini), Ops = sum(opTot))

####____TRANSICIONES CON LAG VARIABLE____####
cancelFull <- readRDS("D:/!bso/Consultas/Amortizados_Ene2015Jun2023.rds")

Calce <- cancelFull %>%
  dplyr::filter(ctaCont_ini %in% c('131','133','134','135','136','137')) %>% 
  dplyr::filter(TipoReduccion == "2. Anticipado") %>% 
  mutate(Fecha = as.Date(myAmort, frac=1)) %>% 
  mutate(RangoPlazo = case_when(FFINALIZA_ini - Fecha <=30 ~ '1. 30 días',
                                FFINALIZA_ini - Fecha <=60 ~ '2. 60 días',
                                FFINALIZA_ini - Fecha <=90 ~ '3. 90 días',
                                FFINALIZA_ini - Fecha <=180 ~ '4. 180 días',
                                FFINALIZA_ini - Fecha <=360 ~ '5. 360 días',
                                FFINALIZA_ini - Fecha <=720 ~ '6. 720 días',
                                FFINALIZA_ini - Fecha > 720 ~ '7. >720 días',)) %>% 
  mutate(Operaciones = 1) %>% 
  select(Fecha, ctaCont_ini, RangoPlazo, saldous_ini, Operaciones) %>% 
  group_by(Fecha, ctaCont_ini, RangoPlazo) %>% 
  summarise_all(sum) %>% 
  ungroup()

Calce <- Calce %>% 
  left_join(tm_cont, by = c("Fecha","ctaCont_ini")) 
  
# write_xlsx(Calce, "D:/!bso/calce/SustentoCancelados.xlsx")

#SIN CUENTA CONTABLE
tm_cont <- readRDS("D:/!bso/transMat/ctaCont/Trans_ctaCont_Lag1_v2.rds") %>% 
  mutate(monDate = monDate+1/12) %>% 
  mutate(Fecha = as.Date(monDate, frac=1)) %>% 
  mutate(ctaCont_ini = substr(ctaCont_ini,1,3)) %>% 
  dplyr::filter(ctaCont_ini!='865') %>% 
  group_by(Fecha) %>% 
  summarise(Saldo = sum(saldous_ini), Ops = sum(opTot))

####____TRANSICIONES CON LAG VARIABLE____####
cancelFull <- readRDS("D:/!bso/Consultas/Amortizados_Ene2015Jun2023.rds")

Calce2 <- cancelFull %>%
  dplyr::filter(ctaCont_ini %in% c('131','133','134','135','136','137')) %>% 
  dplyr::filter(TipoReduccion == "2. Anticipado") %>% 
  mutate(Fecha = as.Date(myAmort, frac=1)) %>% 
  mutate(RangoPlazo = case_when(FFINALIZA_ini - Fecha <=30 ~ '1. 30 días',
                                FFINALIZA_ini - Fecha <=60 ~ '2. 60 días',
                                FFINALIZA_ini - Fecha <=90 ~ '3. 90 días',
                                FFINALIZA_ini - Fecha <=180 ~ '4. 180 días',
                                FFINALIZA_ini - Fecha <=360 ~ '5. 360 días',
                                FFINALIZA_ini - Fecha <=720 ~ '6. 720 días',
                                FFINALIZA_ini - Fecha > 720 ~ '7. >720 días',)) %>% 
  mutate(Operaciones = 1) %>% 
  select(Fecha, RangoPlazo, saldous_ini, Operaciones) %>% 
  group_by(Fecha, RangoPlazo) %>% 
  summarise_all(sum) %>% 
  ungroup()

Calce2 <- Calce2 %>% 
  left_join(tm_cont, by = c("Fecha"))

Sustento <- list(Calce_ctaCont = Calce, Calce_Total =Calce2)
write_xlsx(Sustento, "D:/!bso/calce/SustentoCancelados_Jun2023.xlsx")
