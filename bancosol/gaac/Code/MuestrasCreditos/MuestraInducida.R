####____CARGA DE PAQUETES Y LIBRERIAS____####
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
####____CRITERIOS DE UNIVERSO____####
#Octubre 2023: Al corte de octubre 2023, TIPO_CREDITO todos, cuentas c("131","135") (vigentes)
#___sin restricción de sucursal, montos menores a 6000 USD, frange c("2022-09-01","2023-09-30")
#___de primera secuencia, desv 0.265. Sin operaciones migradas. Estratificado por Sucursal
#___edad del titular de 25 a 35 años, y día de desembolso posterior al 24 de cada mes.
#NOTA: La fecha de desembolso cambia cuando se reprograma o se refinancia
conCuentas <- c("131","135")
conEstado  <- c("VIGENTE")
rangoEdad <- c(25, 35)
sinModulo <- c(131,118) #118 para excluir migradas de FASSIL
desv <- 0.265
conf_level <- 0.95
sample_error <- 0.1
frange <- c("2022-09-01","2023-09-30")
####____POBLACION AL CORTE SOLICITADO____####
lastmonth <- "Oct. 2023"
shortmonth <- str_replace(lastmonth,". ","")
bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',shortmonth,'.rds'))
#SI SE HACE CON CIERRE DE ÚLTIMO DÍA EN LUGAR DE CIERRE MENSUAL
fn <- "20230531"
bdc <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/ec_', fn, '.rds'))

cic <- readRDS("D:/!bso/CIC/rds/cic_Sep2023.rds") %>% 
  mutate(Edad = floor(as.numeric(FechaCorte-FechaNacimiento)/365)) %>%
  group_by(CTACLIENTE) %>% 
  summarise(Edad=max(Edad)) %>% 
  ungroup()
dfAux <- readRDS('D:/!bso/features/Clientes_Ene15Oct23.rds') %>% 
  select(CTACLIENTE, OPERACION, Ciclo)

bdcUniverse <- bdc %>% 
  mutate(MontoDes = ifelse(MONEDA==0, MONTO/6.86, MONTO)) %>% 
  left_join(select(dfAux, CTACLIENTE, OPERACION, Ciclo), by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(cic, by=c("CTACLIENTE")) %>% 
  dplyr::filter(FDESEMBOLSO>=as.Date(frange[1]) & FDESEMBOLSO<=as.Date(frange[2])) %>%
  dplyr::filter(day(FDESEMBOLSO)>=24) %>%
  dplyr::filter((Edad>=rangoEdad[1] & Edad<=rangoEdad[2]) | is.na(Edad)) %>%
  dplyr::filter(ctaCont %in% conCuentas) %>% 
  dplyr::filter(ESTADO %in% conEstado) %>% 
  dplyr::filter(MontoDes <= 6000) %>% 
  dplyr::filter(Ciclo==1) %>% 
  dplyr::filter(!(MODULO==118 | str_detect(TIPO_OPER, "MIGR"))) 

bdcPop <- bdcUniverse %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT,PATERNO_TIT,
         MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,
         REGIONAL=Regional,SUCURSAL=Sucursal,LINEA, Edad, Ciclo) 
  
write_xlsx(bdcPop,paste0("D:/!bso/girCartera/samples/MuestraInducida/PoblacionInducida_",shortmonth,"_v2.xlsx"))
####____TAMAÑO DE MUESTRA POR SUCURSAL____####
tam <- bdcUniverse %>% 
  mutate(Nops = sum(opTot)) %>%
  group_by(Sucursal) %>% 
  summarise(Nh = sum(opTot), Wh = sum(opTot)/max(Nops),
            Ph = 1-desv)%>% #Aqui se aplica el nivel de desvios anterior
  ungroup() %>% 
  mutate(pph = Wh*Ph) %>% 
  mutate(Sh = sqrt(Nh*Ph*(1-Ph)/(Nh-1))) %>% 
  summarise(Nops = sum(Nh), sumSh = sum(Wh*Sh), sumSh2 = sum(Wh*Sh^2),
            z = qnorm(1-(1-conf_level)/2), e = sample_error*sum(pph)) %>% #Aqui se aplica el nivel de confianza y error
  mutate(n = ceiling(sumSh^2/(e^2/z^2+sumSh2/Nops)))
####____ASIGNACION DE MUESTRA POR SUCURSAL____####
#Seed for Oct23: 1234
#SI SE UTILIZA BDC DE CIERRE DIARIO SE CAMBIA TIPO_CLIENTE POR `TIPO DE CLIENTE`
set.seed(1234)#Oct23
bdcSample <- bdcUniverse %>%
  mutate(Nops = sum(opTot)) %>%
  group_by(Sucursal) %>%
  mutate(Wh = sum(opTot)/max(Nops)) %>% 
  sample_n(round(max(Wh)*tam$n)) %>%
  mutate(x=1) %>% 
  ungroup() %>% 
  select(-Wh,-x) %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT, PATERNO_TIT,
         MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,
         REGIONAL=Regional,SUCURSAL=Sucursal,LINEA, Edad,Ciclo, CTACONTABLE=ctaCont,SALDO_USD=saldous)
####____EXPORTANDO EL RESULTADO____####
table(bdcSample$SUCURSAL)
#Check para ver si operacion son únicas para añadir dummy al universo basado solo en operación
n_distinct(bdcUniverse$OPERACION) == nrow(bdcUniverse)
#Si es False se debe crear un id unico concatenando CTACLIENTE-OPERACION
RepTipoCred <- bdcUniverse %>% 
  group_by(Sucursal) %>% 
  summarise(Saldo=sum(saldous),Operaciones=sum(opTot)) %>% 
  ungroup()
RepRegional <- bdcUniverse %>% 
  group_by(Regional) %>% 
  summarise(Saldo=sum(saldous),Operaciones=sum(opTot)) %>% 
  ungroup()

Informe <- list(Muestra=bdcSample, DetalleSucursal=RepTipoCred, DetalleRegional=RepRegional)
write_xlsx(Informe,paste0("D:/!bso/girCartera/samples/MuestraInducida/MuestraInducida_",shortmonth,"_v2.xlsx"))
