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
library(openxlsx)
require(XLConnect)
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
####____MUESTRAS AGENCIAS____####
#Diciembre 2022
c(251,252,301,306,701,253,271,316,723,
  256,307,709,703,302,305,702,716)
#Enero 2022
c(101,103,201,202,203,205,206,208,210,254,262,263,267,270,276,303,
  309,312,313,314,318,326,327,704,705,706,711,715,721,725,727,728,730)
#Febrero 2023
c(222,220,212,211,216,266,273,106,325,328,105,724,
  321,322,324,323,731,726,732,274,219,218,275,107)
#Marzo 2023
c(204,217,221,902,272,260,261,601,605,603,320,329,
  501,608,734,401,409,801,802,803,411)
####____READING CIERRES____####
#La muestra es con la ultima fecha de corte y filtrando las fechas de desembolso
#La metodología indica considerar Ops distintas a linea de credito y boleta de garantia
#La metodología indica seleccionar Ops con montos desem <= 343000 BS
agen <- read.xlsx("D:/!bso/bases/excel/CodAgeSucReg.xlsx")
flist <- c('ec_Mar2023.rds')
bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/', flist)) %>% 
  left_join(agen,by="AGENCIA") %>% 
  dplyr::filter(MODULO != 131) %>% #Aqui se eliminan lineas de credito
  dplyr::filter(ctaCont!=623) %>% #Aquí se eliminan las operaciones cuya ctaCont es Boleta de garantía
  dplyr::filter(montous<=(343000/6.86)) #Aqui se eliminan desembolsos menores a 50000 Sus

base <- bdc %>% 
  dplyr::filter(cosechaM == "mar. 2023") %>%
  select(Regional,AGENCIA, CTACLIENTE) %>%
  group_by(Regional,AGENCIA) %>% 
  summarise(POBLACION=n()) %>% 
  glimpse()
#write.xlsx(base, 'D:/!bso/girCartera/Muestra_Dic2022.xlsx')

dfdraw <- bdc %>% 
  dplyr::filter(cosechaM == 'mar. 2023')

popExp <- dfdraw %>% 
  dplyr::filter(AGENCIA %in% c(204,217,221,902,272,260,261,601,605,603,320,329,
                               501,608,734,401,409,801,802,803,411)) %>% 
  arrange(AGENCIA) %>% 
  rename(SALDO_USD=saldous, SUCURSAL=Sucursal, CAEDEC_ACT_ECO=grupoCaedecC, 
         CAEDEC_DEST_CRED=grupoCaedecD, REGIONAL=Regional) %>%
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT, PATERNO_TIT,
         MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,
         REGIONAL,SUCURSAL,LINEA)
#write.xlsx(popExp, 'D:/!bso/girCartera/samples/Poblacion_RD_Dic2022.xlsx')
####____ESTABLISHING SEED AND SAMPLE____####
#Primera semilla (dic22) 1234
#Semilla (ene23) 123456
#Semilla (feb23) 123456
size <- 31
set.seed(123456)

lista <- dfdraw %>% 
  group_by(AGENCIA) %>% 
  arrange(MONTO) %>% 
  ungroup() %>% 
  mutate(draw = runif(nrow(dfdraw), min = 0, max = 1)) %>% 
  group_by(AGENCIA) %>% 
  arrange(AGENCIA, desc(draw)) %>% 
  mutate(pos = row_number()) %>% 
  dplyr::filter(pos <= 31) %>% 
  dplyr::filter(AGENCIA %in% c(204,217,221,902,272,260,261,601,605,603,320,329,
                               501,608,734,401,409,801,802,803,411)) %>% 
  rename(SALDO_USD=saldous, SUCURSAL=sucursal, CAEDEC_ACT_ECO=grupoCaedecC, 
         CAEDEC_DEST_CRED=grupoCaedecD, REGIONAL=Regional) %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT, PATERNO_TIT,
         MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,
         REGIONAL,SUCURSAL,LINEA)

lista2 <- dfdraw %>% 
  group_by(AGENCIA) %>% 
  arrange(MONTO) %>% 
  mutate(k=ifelse(n()<size,1,n()/size)) %>% 
  mutate(a = runif(1,min = 1, max = k)) %>% 
  dplyr::filter(row_number() %in% floor(seq(max(a),max(a)+max(k)*(size-1),length.out=size)))
  dplyr::filter(AGENCIA %in% c(204,217,221,902,272,260,261,601,605,603,320,329,
                               501,608,734,401,409,801,802,803,411)) %>% 
  rename(SALDO_USD=saldous, SUCURSAL=sucursal, CAEDEC_ACT_ECO=grupoCaedecC, 
         CAEDEC_DEST_CRED=grupoCaedecD, REGIONAL=Regional) %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT, PATERNO_TIT,
         MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,
         REGIONAL,SUCURSAL,LINEA)

ag <- unique(lista$AGENCIA)
muestralist <- list()
for (i in 1:length(ag)) {
  m <- lista %>% 
    dplyr::filter(AGENCIA==ag[i])
  muestralist[[i]] <- m
}

names(muestralist) <- ag
setwd('D:/!bso/girCartera/samples/MuestraDesembolsados202303')
Map(openxlsx::write.xlsx, muestralist, paste0(names(muestralist), '.xlsx'))
#write.xlsx(muestralist, 'D:/!bso/girCartera/samples/Muestra_Dic2022_consolidada_v2.xlsx')

l <- lista %>% 
  group_by(AGENCIA) %>%
  summarise(MUESTRA=n()) %>% 
  glimpse()

muestra <- base %>% 
  left_join(agen,by="AGENCIA") %>%
  select(-Regional.x,-Regional.y) %>% 
  left_join(l,by="AGENCIA") %>% 
  relocate(POBLACION,.before = MUESTRA)
write.xlsx(muestra,'D:/!bso/girCartera/samples/MuestraDesembolsados202303/Detalle_MuestraDesem_Mar23.xlsx')
####____BASE DE CARTERA PARA DAN____####
flist <- c('ec_Feb2023.rds')
agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")
bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/', flist)) %>% 
  mutate(REGIONAL=case_when(str_detect(sucursal,"La Paz|Oruro")~"Regional Occidente",
                            str_detect(sucursal,"Santa Cruz|Beni")~"Regional Oriente",
                            str_detect(sucursal,"Cochabamba|Tarija")~"Regional Centro",
                            str_detect(sucursal,"El Alto|Pando")~"Regional El Alto",
                            str_detect(sucursal,"Potosí-|Chuquisaca")~"Regional Sur")) %>% 
  left_join(agen,by="AGENCIA") %>% 
  rename(SALDO_USD=saldous, SUCURSAL=sucursal, CAEDEC_ACT_ECO=grupoCaedecC, 
         CAEDEC_DEST_CRED=grupoCaedecD) %>%
  mutate(across(c(FDESEMBOLSO,FFINALIZA,FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FALTACLI),~as.Date(.x,"%d/%m/%y"))) %>% 
  dplyr::filter(MODULO != 131) %>% #Aqui se eliminan lineas de credito
  select(-RUBRO,-V94,-NOMBRE_GRUPO,-ASESOR_PILOTO)
bdc2 <- bdc[,c(1:90,151,152)]
write.xlsx(bdc2,'D:/!bso/girCartera/samples/BaseCartera_Feb2023.xlsx')

select(CTACLIENTE,OPERACION,MODULO,REGIONAL,SUCURSAL,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT,
         PATERNO_TIT,MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,
         FULT_PAGO,FVEN_ULTPAGO,FVEN_PROXPAGO,FFINALIZA,ESTADO,RUBRO,DIASMORA,CALIFICACION,
         PREVCONST,REPRO,FONDO,SIB,TASAFIJA,TASAVARIABLE,TIPOTASA,GENERO,CIU,PLAZODIAS,
         EXCLUSIVO,DEUDORES,CODEUDORES,TIPO_CREDITO,TAM_ACTIV,INDICE_TAM,DESC_OBJCRED,
         CAEDEC_DEST,CAEDEC_ACT_ECO,CAEDEC_DEST_CRED,SECTOR_CARTERA,SEGM_MERCADO,DESC_SEGMERC,
         PRODUCTIVO,REFINANCIAMIENTO_GENUINO,TIPO_CLIENTE,OPERACION_ORI_REF,MONEDA_ORI_REF,
         MONTO_ORI_REF,TASAACT,INICIO_ACTIVIDAD,TIPO_ASESOR,CPOP,
         BENEFICIO_OTORGADO,SEGURO_CUOTA_PROTEGIDA,MONTO_SEGURO_CUOTA_PROTEGIDA,
         RUBRO_CAPITAL_DIFERIDO,SALDO_CAPITAL_DIFERIDO,RUBRO_INT_CAPITAL_DIFERIDO,
         SALDO_INT_CAPITAL_DIFERIDO,GRACIA_MESES,MONTO_CUOTA,SEGURO_DE_VIDA,MONTO_SEGURO_DE_VIDA,
         TIENE_AHORRO,MONTO_AHORRO)