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
####____AGENCIAS____####
#Diciembre 2022
agencias <- cc(251,252,301,306,701,253,271,316,723,
               256,307,709,703,302,305,702,716)
#Enero 2022
agencias <- cc(101,103,201,202,203,205,206,208,210,254,262,263,267,270,276,303,
               309,312,313,314,318,326,327,704,705,706,711,715,721,725,727,728,730)
#Febrero 2023
agencias <- cc(222,220,212,211,216,266,273,106,325,328,105,724,
               321,322,324,323,731,726,732,274,219,218,275,107)
#Marzo 2023
agencias <- c(204,217,221,902,272,260,261,601,605,603,320,329,
              501,608,734,401,409,801,802,803,411)
####____READING CIERRE____####
#La muestra es con la ultima fecha de corte y filtrando las fechas de desembolso
#La metodología indica considerar Ops distintas a linea de credito y boleta de garantia
#La metodología indica seleccionar Ops con montos desem <= 343000 BS
agen <- read.xlsx("D:/!bso/bases/excel/CodAgeSucReg.xlsx")
cosecha <- 'Mar. 2023' #cambiar para muestra
mes <- str_replace(cosecha,". ","")
bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',mes,'.rds')) %>% 
  left_join(agen,by="AGENCIA") %>% 
  dplyr::filter(MODULO != 131) %>% #Aqui se eliminan lineas de credito
  dplyr::filter(ctaCont!=623) %>% #Aquí se eliminan las operaciones cuya ctaCont es Boleta de garantía
  dplyr::filter(montous<=(343000/6.86)) #Aqui se eliminan desembolsos menores a 50000 Sus

####____DESEMBOLSOSO AND POPULATION____####
desembolsos <- bdc %>% 
  dplyr::filter(cosechaM == cosecha) %>%
  select(Regional,Sucursal, AGENCIA, NOMBRE_AGENCIA, OPERACION) %>%
  group_by(Regional,Sucursal,AGENCIA, NOMBRE_AGENCIA) %>% 
  summarise(POBLACION=n()) %>% 
  glimpse()

dfdraw <- bdc %>% 
  dplyr::filter(cosechaM == cosecha) %>% 
  dplyr::filter(AGENCIA %in% agencias)

popExp <- dfdraw %>% 
  arrange(AGENCIA) %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT, PATERNO_TIT,
         MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,
         REGIONAL=Regional,SUCURSAL=Sucursal,LINEA)
####____ESTABLISHING SEED POPULATION AND SAMPLE____####
#Primera semilla (dic22) 1234
#Semilla (ene23) 123456
#Semilla (feb23) 123456
size <- 31
set.seed(123456)

lista <- dfdraw %>% 
  group_by(AGENCIA) %>% 
  arrange(MONTO) %>% 
  mutate(k = ifelse(n()<size,1,n()/size)) %>% 
  mutate(a = runif(1,min = 1, max = k)) %>% 
  dplyr::filter(row_number() %in% floor(seq(max(a),max(a)+max(k)*(size-1),length.out=size))) %>% 
  ungroup() %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT, PATERNO_TIT,
         MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,
         REGIONAL=Regional,SUCURSAL=Sucursal,LINEA)

ag <- unique(lista$AGENCIA)
muestralist <- list()
for (i in 1:length(ag)) {
  m <- lista %>% 
    dplyr::filter(AGENCIA==ag[i])
  muestralist[[i]] <- m
}
names(muestralist) <- ag
dir.create(path = paste0("D:/!bso/girCartera/samples/MuestraDesembolsados",mes))
setwd(paste0('D:/!bso/girCartera/samples/MuestraDesembolsados',mes))
Map(openxlsx::write.xlsx, muestralist, paste0(names(muestralist), '.xlsx'))

l <- lista %>% 
  group_by(AGENCIA) %>%
  summarise(MUESTRA=n()) %>% 
  glimpse()

detalle <- desembolsos %>% 
  left_join(l,by="AGENCIA") %>% 
  relocate(POBLACION,.before = MUESTRA)
write.xlsx(detalle,paste0('D:/!bso/girCartera/samples/MuestraDesembolsados',mes,'Detalle_',mes,'.xlsx'))