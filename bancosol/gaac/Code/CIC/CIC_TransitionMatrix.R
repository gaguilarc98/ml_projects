####____CARGA DE PAQUETES____####
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
library(expm)
library(quantmod)
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(janitor)
library(openxlsx)
require(XLConnect)

Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____ESTUDIO DE PROYECCION DE PREVISION____####
#EL OBJETIVO DE ESTE ESTUDIO ES DETERMINAR EL INCREMENTO EN PREVISION
#DEL STOCK DE CARTERA ACTUAL A UN AÑO
#PARA ELLO DEBEMOS CONSTRUIR LAS MATRICES DE TRANSICION A 12 MESES
#Y LA MATRIZ DE TRANSICIÓN MESNUAL PROYECTADA A 12 MESES
#ADEMÁS DEBEMOS CAPTURAR LA TASA DE AMORTIZACIÓN PROMEDIO POR TRANSICIÓN
#Y AGRUPANDO POR LOS CRITERIOS DE CONSTITUCIÓN DE PREVISIÓN

####____DETALLES METODOLÓGICOS____####
#PARA VALIDAR EL MODELO SE HARÁ EL CORTE HASTA ABRIL DE 2022 PARA PREDECIR EL AUMENTO DE
#PREVISION EN ABRIL 2023 Y REALIZAR LA COMPARACIÓN CON LOS VALORES REALES.
#CONSTRUIR LAS MATRICES PARA LOS SIGUIENTES CRITERIOS:
# * TIPO_CREDITO
# * SECTOR PRODUCTIVO
# * MONEDA (si la cantidad de observaciones es suficiente)
# * HIPOTECARIOS DE VIVIENDA O VIV DE INTERES SOCIAL (S GARANTIA)/SIN GARANTIA
# * DIRECTO/CONTINGENTE (si la cantidad de observaciones es suficiente)
# * FECHA DESEMBOLSO (si la cantidad de observaciones es suficiente)

####____TRANSICIONES CON LAG VARIABLE____####
codProd <- read_excel("D:/!bso/bases/excel/CodSecProd.xlsx",sheet='RPT004') %>% 
  rename(CAEDEC = caecn, GrupoActEcon = cgaec) %>% 
  glimpse()

mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
lag <- 1

i <- 99
for(i in 1:(length(myrds)-lag)) {
  tryCatch({
    print(i)
    print(myrds[i])
    k <- i + lag
    print(myrds[i+lag])
    
    df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i], '.rds')) %>%  #se abre mes anterior
      dplyr::filter(CuentaContable.y %in% c('131','133','134','135','136','137','623','865')) %>% 
      dplyr::filter(is.na(TipoCancelacion)) %>% 
      dplyr::filter(!(CuentaContable.y =='865' & SaldoCastigado==0)) %>% 
      dplyr::filter(Calificacion %in% c('A','B','C','D','E','F')) %>% 
      
      mutate(Saldo = case_when(SaldoBruto>0 ~ SaldoBruto,
                               SaldoCastigado>0 ~ SaldoBruto)) %>% 
      mutate(Calificacion = ifelse(CuentaContable.y=='865',"S", Calificacion)) %>% 
      select(monDate, CTACLIENTE, OPERACION, Calificacion, Saldo, PrevEspecifica,
             TipoCredito, )
    
    df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[k], '.rds')) %>%  #se abre mes posterior 
      dplyr::filter(CuentaContable.y %in% c('131','133','134','135','136','137','623','865')) %>% 
      dplyr::filter(is.na(TipoCancelacion)) %>% 
      dplyr::filter(!(CuentaContable.y =='865' & SaldoCastigado==0)) %>% 
      dplyr::filter(Calificacion %in% c('A','B','C','D','E','F')) %>% 
      mutate(Saldo = case_when(SaldoBruto>0 ~ SaldoBruto,
                               SaldoCastigado>0 ~ SaldoBruto)) %>% 
      mutate(Calificacion = ifelse(CuentaContable.y=='865',"S", Calificacion)) %>% 
      select(monDate, CTACLIENTE, OPERACION, Calificacion, Saldo, PrevEspecifica)
    
    dfCancel <- df1 %>% 
      anti_join(df2,by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(Calificacion = "Z") %>% 
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


bdc <- readRDS("D:/!bso/girCartera/rds/ec_Jul2023.rds")
cic <- readRDS("D:/!bso/CIC/rds/cic_Jul2023.rds")

bdcAll <- bdc %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','623','865'))

cicAll <- cic %>% 
  dplyr::filter(CuentaContable.y %in% c('131','133','134','135','136','137','623','865')) %>% 
  dplyr::filter(is.na(TipoCancelacion)) %>% 
  dplyr::filter(!(CuentaContable.y =='865' & SaldoCastigado==0))

cicX <- cicAll %>% 
  anti_join(bdcAll, by=c("CTACLIENTE","OPERACION"))

####____RDS____####
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Ago2023.rds")
bdcPrev <- readRDS("D:/!bso/girCartera/rds/ec_Jul2023.rds")
bdcCI <- bdc %>% 
  mutate(NDOC = str_replace(CI, "LP$|OR$|PO$|CB$|CH$|TJ$|SC$|BE$|PA$","")) %>% 
  select(CTACLIENTE, OPERACION, NDOC, CI, ends_with("TIT"), MONTO, ctaCont, SALDO, MONEDA,
         MODULO, TIPO_OPER)

bdcCIPrev <- bdcPrev %>% 
  mutate(NDOC = str_replace(CI, "LP$|OR$|PO$|CB$|CH$|TJ$|SC$|BE$|PA$","")) %>% 
  select(CTACLIENTE, OPERACION, NDOC, CI, ends_with("TIT"), MONTO, ctaCont, SALDO, MONEDA,
         MODULO, TIPO_OPER)

bdcJoin <- bdcCI %>% 
  left_join(bdcCIPrev, by=c("NDOC","OPERACION"))

x <- bdcJoin %>% 
  dplyr::filter(CTACLIENTE.x!=CTACLIENTE.y)
