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
#Abril 2023
agencias <- c(607,503,410,269)
#Junio 2023
agencias <- c(206,254,316,302,301,701,709,702,208,202,251,306,307,305,716,
              723,205,256,253,314,303,706,715,703,271,201,318,309,705,711,728)
#Julio 2023
agencias <- c(203,263,313,217,328,721, 260,273,327,219,272,725,
              276,252,221,325,704,726, 261,204,322,324,724,732,
              270,212,329,321,731,220)
####____READING CIERRE____####
#La muestra es con la ultima fecha de corte y filtrando las fechas de desembolso
#La metodología indica considerar Ops distintas a linea de credito y boleta de garantia
#La metodología indica seleccionar Ops con montos desem <= 343000 BS
#Para junio 2023: Excluir cartera migrada de FSL, eso es !(str_detect(TIPO_OPER,"MIGR") & MODULO==121) | !MODULO==118
agen <- read_xlsx("D:/!bso/bases/excel/CodAgeSucReg.xlsx")
cosecha <- 'Jul. 2023' #cambiar para muestra
mes <- str_replace(cosecha,". ","")
bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mes,'.rds')) %>% 
  # left_join(agen,by="AGENCIA") %>% 
  dplyr::filter(!MODULO %in% c(131)) %>% #Aqui se eliminan lineas de credito y cartera FASSIL
  dplyr::filter(ctaCont!=623) %>% #Aquí se eliminan las operaciones cuya ctaCont es Boleta de garantía
  dplyr::filter(montous<=(343000/6.86))  #Aqui se eliminan desembolsos menores a 50000 Sus

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
#Semilla (mar23) 123456
#Semilla (abr23) 123456
#Semilla (jun23) 123456
#Semilla (jul23) 123456
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
openxlsx::write.xlsx(detalle,paste0('D:/!bso/girCartera/samples/MuestraDesembolsados',mes,'/Detalle_',mes,'.xlsx'))
openxlsx::write.xlsx(popExp,paste0('D:/!bso/girCartera/samples/MuestraDesembolsados',mes,'/Población_',mes,'.xlsx'))
openxlsx::write.xlsx(lista,paste0('D:/!bso/girCartera/samples/MuestraDesembolsados',mes,'/Muestra_',mes,'.xlsx'))
####____EN UNA SOLA HOJA____####
setwd(paste0('D:/!bso/girCartera/samples/MuestraDesembolsados',mes))
flist <- list.files()
MuestraList <- list()
for(i in 1:30){
  agencial <- read.xlsx(flist[i])
  MuestraList[[i]] <- agencial
}
MuestraFull <- rbindlist(MuestraList)
write.xlsx(MuestraFull, file = paste0('D:/!bso/girCartera/samples/MuestraDesembolsados',mes,'/Muestra_',mes,'.xlsx'))

####____JULIO 2023 REEMPLAZO DE OBS LA COLORADA____####
popExp <- read.xlsx(paste0('D:/!bso/girCartera/samples/MuestraDesembolsados',mes,'/Población_',mes,'.xlsx')) %>% 
  dplyr::filter(NOMBRE_AGENCIA == "La Colorada") %>% 
  mutate(MONTO_REEMP = MONTO[CTACLIENTE==850163 & OPERACION==4037038]) %>% 
  dplyr::filter(CTACLIENTE!=850163) %>% 
  mutate(MONTO_DIF = abs(MONTO- MONTO_REEMP)) %>% 
  arrange(MONTO_DIF) %>% 
  dplyr::filter(row_number()==1) %>% 
  select(-MONTO_DIF, -MONTO_REEMP)

MuestraFull <- read.xlsx(paste0('D:/!bso/girCartera/samples/MuestraDesembolsados',mes,'/Muestra_',mes,'.xlsx')) %>% 
  dplyr::filter(NOMBRE_AGENCIA == "La Colorada") %>% 
  dplyr::filter(CTACLIENTE!=850163) %>% 
  bind_rows(popExp) %>% 
  arrange(MONTO)
write.xlsx(MuestraFull, file = paste0('D:/!bso/girCartera/samples/MuestraDesembolsados',mes,'/Muestra_LaColorada',mes,'.xlsx'))

################################################################################
#____________________AGREGAR MEDIDAS DE RENDIMIENTO PARA RC____________________#
################################################################################
#TABLA ASESORES
asesores <- readRDS('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/TablaAsesores.rds')
  
asesores <- asesores %>% 
  group_by(ASESOR) %>% 
  mutate(NOMBRE_ASESOR = max(NOMBRE_ASESOR[which(nchar(NOMBRE_ASESOR)==max(nchar(NOMBRE_ASESOR)))],na.rm = T)) %>% 
  mutate(CI_Asesor = CI_Asesor[monDate==max(monDate)]) %>% 
  arrange(monDate) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  rename(PrimerCierreDesde2015 = monDate)

#TABLA SECUENCIA
dfTotal <- readRDS("D:/!bso/features/Clientes_20231028.rds")

dfAux <- dfTotal %>% 
  mutate(mydes = as.yearmon(fdes)) %>% 
  dplyr::filter(mydes <= "Oct. 2023") %>% 
  # mutate(fueRefin = ifelse((!is.na(FechaRefin) & as.Date(FechaRefin) <= as.Date("2015-01-01")),0,fueRefin)) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(esMontoMax = ifelse(MONTOUS == max(MONTOUS),1,0)) %>% 
  mutate(esMontoMin = ifelse(MONTOUS == min(MONTOUS),1,0)) %>% 
  summarise(MONTO_MAXIMO_USD = max(MONTOUS),
            MONTO_MINIMO_USD = min(MONTOUS),
            Fecha_Monto_Max = max(fdes[esMontoMax==1]),
            Fecha_Monto_Min = min(fdes[esMontoMin==1]),
            N_OPNOREFIN = length(which(fueRefin==0)), N_OPREFIN = length(which(fueRefin==1)), N_TOTAL = N_OPNOREFIN+N_OPREFIN) %>% 
  select(CTACLIENTE, MONTO_MAXIMO_USD, MONTO_MINIMO_USD, Fecha_Monto_Max,
         Fecha_Monto_Min, N_OPNOREFIN, N_OPREFIN, N_TOTAL) 

#TABLA EDADES
cic <- readRDS("D:/!bso/CIC/rds/cic_Sep2023.rds") %>% 
  mutate(Edad = floor(as.numeric(FechaCorte-FechaNacimiento)/365)) %>%
  group_by(CTACLIENTE) %>% 
  summarise(Edad=max(Edad)) %>% 
  ungroup()

fn <- "20231028"
bdc <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/ec_', fn, '.rds'))

bdcShort <- bdc %>% 
  mutate(PrimerDia = as.Date("2023-10-01")) %>% 
  mutate(UltimoDia = as.Date("2023-10-28")) %>% 
  mutate(MoraIntraMes = ifelse(!is.na(FULT_PAGO) & !is.na(FVEN_ULTPAGO) & FULT_PAGO>FVEN_ULTPAGO &
                                 FVEN_ULTPAGO>=PrimerDia & FVEN_PROXPAGO>UltimoDia,FULT_PAGO-FVEN_ULTPAGO,0)) %>% 
  mutate(maximaDPD = 0) %>% 
  mutate(maximaDPD = case_when(ctaCont %in% c('133','134','136','137','865') & DIASMORA>0 ~ DIASMORA,
                               ctaCont %in% c('133','134','136','137','865') & DIASMORA==0
                               & !is.na(FVEN_ULTPAGO) ~ as.numeric(UltimoDia-FVEN_ULTPAGO),
                               ctaCont %in% c('131','135') & DIASMORA!=0 ~ DIASMORA,
                               ctaCont %in% c('131','135') & DIASMORA==0 & !is.na(MoraIntraMes) ~ MoraIntraMes,
                               TRUE ~ maximaDPD)) %>% 
  mutate(maximaDPD_2 = ifelse(maximaDPD>(UltimoDia-PrimerDia),UltimoDia-PrimerDia,maximaDPD)) %>% 
  dplyr::filter(!ctaCont %in% c('623','865')) %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT, PATERNO_TIT,
         MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE = `TIPO DE CLIENTE`,
         REGIONAL=Regional,SUCURSAL=Sucursal,LINEA, DIASMORA, maximaDPD_2) %>% 
  left_join(select(asesores, ASESOR, PrimerCierreDesde2015), by="ASESOR") %>% 
  left_join(dfAux, by = "CTACLIENTE") %>% 
  left_join(cic, by = "CTACLIENTE")

write_xlsx(PTyCondJoin4, "D:/!bso/features/MetricasCierre_Sep2023.xlsx")

Metricas <- read_xlsx("D:/!bso/features/Metricas_Sep2023_old.xlsx", sheet = "Sheet1")

Metricas <- Metricas %>% 
  select(CTACLIENTE, OPERACION, starts_with("Cant"), NXN, MoraIntraMes, starts_with("tuvo"), 
         califPeorSF, ESTADOPeorSF, MaxDiasMoraSF, meanMoraIM, starts_with("Int"),
         PeorCalif, PeorEstadoHist, MesesNoVigenteSF, MesesCastigoSF)
bdcShort <- bdcShort %>% 
  left_join(Metricas, by=c("CTACLIENTE","OPERACION"))

write_xlsx(bdcShort, "D:/!bso/requests/Metricas_20231028_v2.xlsx")
glimpse(bdcShort)

####____POBLACION____####
cic <- readRDS("D:/!bso/CIC/rds/cic_Sep2023.rds") %>% 
  mutate(Edad = floor(as.numeric(FechaCorte-FechaNacimiento)/365)) %>%
  group_by(CTACLIENTE) %>% 
  summarise(Edad=max(Edad)) %>% 
  ungroup()

dfAux <- readRDS('D:/!bso/features/Clientes_Ene15Oct23.rds') %>% 
  group_by(CTACLIENTE) %>% 
  mutate(esMontoMax = ifelse(MONTOUS == max(MONTOUS),1,0)) %>% 
  mutate(esMontoMin = ifelse(MONTOUS == min(MONTOUS),1,0)) %>% 
  mutate(MONTO_MAXIMO_USD = max(MONTOUS),
         MONTO_MINIMO_USD = min(MONTOUS),
         Fecha_Monto_Max = max(fdes[esMontoMax==1]),
         Fecha_Monto_Min = min(fdes[esMontoMin==1])) %>% 
  ungroup() %>% 
  select(CTACLIENTE, OPERACION, MONTO_MAXIMO_USD, MONTO_MINIMO_USD, Fecha_Monto_Max,
         Fecha_Monto_Min, Ciclo)

bdc <- readRDS("D:/!bso/girCartera/rds/ec_Oct2023.rds") %>% 
  mutate(PrimerDia = as.Date("2023-10-01")) %>% 
  mutate(UltimoDia = as.Date("2023-10-28")) %>% 
  mutate(MoraIntraMes = ifelse(!is.na(FULT_PAGO) & !is.na(FVEN_ULTPAGO) & FULT_PAGO>FVEN_ULTPAGO &
                                 FVEN_ULTPAGO>=PrimerDia & FVEN_PROXPAGO>UltimoDia,FULT_PAGO-FVEN_ULTPAGO,0)) %>% 
  mutate(maximaDPD = 0) %>% 
  mutate(maximaDPD = case_when(ctaCont %in% c('133','134','136','137','865') & DIASMORA>0 ~ DIASMORA,
                               ctaCont %in% c('133','134','136','137','865') & DIASMORA==0
                               & !is.na(FVEN_ULTPAGO) ~ as.numeric(UltimoDia-FVEN_ULTPAGO),
                               ctaCont %in% c('131','135') & DIASMORA!=0 ~ DIASMORA,
                               ctaCont %in% c('131','135') & DIASMORA==0 & !is.na(MoraIntraMes) ~ MoraIntraMes,
                               TRUE ~ maximaDPD)) %>% 
  mutate(maximaDPD_2 = ifelse(maximaDPD>(UltimoDia-PrimerDia),UltimoDia-PrimerDia,maximaDPD)) %>% 
  dplyr::filter(!ctaCont %in% c('623','865')) %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT, PATERNO_TIT,
         MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,
         REGIONAL=Regional,SUCURSAL=Sucursal,LINEA, DIASMORA, maximaDPD_2) %>% 
  left_join(select(asesores, ASESOR, PrimerCierreDesde2015), by="ASESOR") %>% 
  left_join(dfTotal, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(cic, by = "CTACLIENTE")

bdcCriterios <- bdc %>% 
  dplyr::filter(FDESEMBOLSO>=as.Date("2021-10-01")) %>% 
  dplyr::filter(day(FDESEMBOLSO)>=24) %>% 
  dplyr::filter(Ciclo %in% c(1,2)) %>% 
  dplyr::filter(is.na(Edad) | (Edad>=25 & Edad<=35))
  
write_xlsx(bdcCriterios, "D:/!bso/requests/SegmentoBDC_Oct2023.xlsx")
