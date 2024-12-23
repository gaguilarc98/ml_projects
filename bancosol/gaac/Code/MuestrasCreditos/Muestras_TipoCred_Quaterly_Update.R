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
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____CRITERIOS DE UNIVERSO____####
#Diciembre 2022: creds: c("H0","H1","H2","H3","H4") con estados vigente, suspenso, vencida y 
#___ejecución. Sin líneas de crédito, de todas las sucursales sin restricción de 
#___con nivel de desvío 13.6% y nivel de confianza 95% y error muestral de 5%
#___Operaciones desembolsadas, reprogramadas o refinanciadas desde el 01/05/2022 hasta el 30/04/2023
#Abril 2023: creds c("M0","M1","M2","M7","M8","N0","N1","N2") cuentas c("131","133","134","135","136","137")
#___sin modulo 131 ni estado "CASTIGADA". Sin líneas de crédito de todas las sucursales sin 
#___restricción de montos y sin líneas de crédito.
#Mayo 2023: creds c("M0","M1","M2","M7","M8","N0","N1","N2") cuentas c("131","133","134","135","136","137")
#___sin modulo 131 ni estado "CASTIGADA". Sin líneas de crédito, sin restricción de sucursal
##___sin restricción de monto, frange c("2022-05-01","2023-05-31") desv 0.112. Sin migradas de FSL
#NOTA: La fecha de desembolso cambia cuando se reprograma o se refinancia
conCreditos <- c("H0","H1","H2","H3","H4")
conCuentas <- c("131","133","134","135","136","137")
sinModulo <- c(131,118) #118 para excluir migradas de FASSIL
sinEstado <- c("CASTIGADA")
desv <- 0.136
conf_level <- 0.95
sample_error <- 0.05
frange <- c("2022-12-01","2023-09-30")
####____LEYENDO ULTIMO CORTE____####
lastmonth <- "Sep. 2023"
shortmonth <- str_replace(lastmonth,". ","")
bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',shortmonth,'.rds'))
#SI SE HACE CON CIERRE DE ÚLTIMO DÍA EN LUGAR DE CIERRE MENSUAL
fn <- "20230531"
bdc <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/ec_', fn, '.rds'))

bdcUniverse <- bdc %>% 
  dplyr::filter(fdes >= as.Date(frange[1]) & fdes<=as.Date(frange[2])) %>% 
  dplyr::filter(!MODULO %in% sinModulo) %>% 
  dplyr::filter(!ESTADO %in% sinEstado) %>% 
  dplyr::filter(ctaCont %in% conCuentas) %>% 
  dplyr::filter(TIPO_CREDITO %in% conCreditos) %>% 
  # dplyr::filter(!(str_detect(TIPO_OPER,"MIGR") & MODULO==121)) %>%  #Para excluir reprog de FASSIL
  ungroup()

#SI SE UTILIZA BDC DE CIERRE DIARIO SE CAMBIA TIPO_CLIENTE POR `TIPO DE CLIENTE`
bdcPop <- bdcUniverse %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT, PATERNO_TIT,
         MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_OPER,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,
         REGIONAL=Regional,SUCURSAL=Sucursal,LINEA,CTACONTABLE=ctaCont,SALDO_USD=saldous)
write_xlsx(bdcPop,paste0("D:/!bso/girCartera/samples/MuestraTipoCredito/PoblacionBDC_",shortmonth,".xlsx"))
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

####____ASIGNACION DE MUESTRA POR REGIONAL____####
#Seed for Dic22: 1234
#Seed for Abr22: 123456
#Seed for May22: 20230602
#SI SE UTILIZA BDC DE CIERRE DIARIO SE CAMBIA TIPO_CLIENTE POR `TIPO DE CLIENTE`
set.seed(12345)#Sep23
bdcSample <- bdcUniverse %>%
  mutate(Nops = sum(opTot)) %>%
  group_by(Regional) %>%
  mutate(Wh = sum(opTot)/max(Nops)) %>% 
  sample_n(round(max(Wh)*tam$n)) %>%
  mutate(x=1) %>% 
  ungroup() %>% 
  select(-Wh,-x) %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT, PATERNO_TIT,
         MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,
         REGIONAL=Regional,SUCURSAL=Sucursal,LINEA,CTACONTABLE=ctaCont,SALDO_USD=saldous)

####____EXPORTANDO EL RESULTADO____####
table(bdcSample$TIPO_CREDITO)
#Check para ver si operacion son únicas para añadir dummy al universo basado solo en operación
n_distinct(bdcUniverse$OPERACION) == nrow(bdcUniverse)
#Si es False se debe crear un id unico concatenando CTACLIENTE-OPERACION
RepTipoCred <- bdcUniverse %>% 
  group_by(TIPO_CREDITO) %>% 
  summarise(Saldo=sum(saldous),Operaciones=sum(opTot)) %>% 
  ungroup()
RepRegional <- bdcUniverse %>% 
  group_by(Regional) %>% 
  summarise(Saldo=sum(saldous),Operaciones=sum(opTot)) %>% 
  ungroup()

Informe <- list(Muestra=bdcSample, DetalleTipoCred=RepTipoCred, DetalleRegional=RepRegional)
write_xlsx(Informe,paste0("D:/!bso/girCartera/samples/MuestraTipoCredito/MuestraBDC_",shortmonth,".xlsx"))
