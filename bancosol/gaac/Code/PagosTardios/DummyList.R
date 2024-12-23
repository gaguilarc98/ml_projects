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
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)

####____READING AGENCIAS____####
agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")
####____CON P2FULL SIN CRITERIO DE 12 MESES____####
P2full <- readRDS('D:/!bso/mph/P2FullLast.rds')
P2uh <- P2full %>%
  mutate(hourUltDia = as.numeric(str_sub(Hora_UltDia, 1, 2))) %>%
  dplyr::filter(myPago != 'ene. 2023') %>% #Se actualiza para conservar los útlimos 12 meses
  group_by(myPago, yearPago, mesPago) %>% 
  mutate(maxDia = max(dayPago)) %>% 
  mutate(maxDia = ifelse(myPago=='feb. 2022',25,maxDia)) %>% #Para febrero se toma el 25 como último día
  mutate(maxDia = ifelse(myPago=='dic. 2022',30,maxDia)) %>% #Para diciembre se considera el 30 y 31
  ungroup() %>% 
  mutate(FechaPago = as.Date(paste(yearPago, mesPago, dayPago), format = '%Y%m%d')) %>% 
  mutate(appsH = ifelse(dayPago >= maxDia-2 & FechaPago > FechaPrevistaPago, 1, 0)) %>% 
  mutate(appsU = case_when(myPago=='dic. 2022' & dayPago>=maxDia & FechaPago > FechaPrevistaPago &
                             hourUltDia >= 12~1,
                           myPago!='dic. 2022' & dayPago==maxDia & FechaPago > FechaPrevistaPago &
                             hourUltDia >= 12~1,TRUE~0))
####____READING PAGOS TARDÍOS____####
P2uh <- read_rds('D:/!bso/mph/Pagos_conDummyTardio.rds')
P2uh_group <- P2uh %>% 
  #dplyr::filter(myPago!='ene. 2023') %>% #Eliminate last months to get results of previus months
  group_by(Operacion) %>% 
  mutate(appsH = sum(appsH))%>%
  mutate(appsU = sum(appsU))%>%
  ungroup() %>% 
  dplyr::filter(appsH >= 1 & appsU >= 1) %>% 
  group_by(Operacion) %>% 
  arrange(Operacion, desc(mesPago), dayPago) %>% 
  mutate(pos = row_number()) %>% 
  dplyr::filter(pos == 1) %>% 
  ungroup()
####____LAST CIERRE____####
lastCierre2 <- readRDS('D:/!bso/girCartera/rdsGAR/ec_ene2023.rds') %>% #Actualizar al último mes
  mutate(cosechaS=case_when(cosechaY<2020 ~ '< 2020',
                            cosechaY==2021 & cosechaM<='jun. 2021' ~ 'I/2021',
                            cosechaY==2021 & cosechaM>'jun. 2021' ~ 'II/2021',
                            TRUE~ as.character(cosechaY))) %>% 
  mutate(NOMBRE_CLIENTE=paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT,sep = " ")) %>% 
  left_join(agen,by="AGENCIA") %>% #Se añade el nombre de la agencia
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>% #Eliminamos las operaciones castigadas
  mutate(fdes = dmy(FDESEMBOLSO)) %>%
  rename(Operacion=OPERACION) %>% 
  mutate(Reprogramado=ifelse(saldoReprog>0,1,0)) %>% 
  mutate(Refinanciado=ifelse(saldoRef>0,1,0)) %>% 
  mutate(Diferido=ifelse(saldoDif>0,1,0))
idHR <- P2uh_group %>% 
  dplyr::filter(appsU>=1) %>% 
  select(Operacion, appsH, appsU,mesPago,
         dayPago,hourUltDia) %>%
  left_join(lastCierre2, by = 'Operacion') %>% 
  mutate(bin2 = case_when(dayPago==29~'1. Pagó antepenúltimo día',
                          dayPago==30~'2. Pagó penúltimo día',
                          dayPago==31 & hourUltDia<=12~'3. Pagó último día antes de 12',
                          dayPago==31 & hourUltDia>12 & hourUltDia<=14~'4. Pagó último día de 12 a 14',
                          dayPago==31 & hourUltDia>14~'5. Pagó último día de 14 en adelante',
                          TRUE~'0. Pagó antes del antepenúltimo día'
  )) %>% 
  select(Operacion,appsH,appsU)
lastCierre <- lastCierre2 %>% 
  left_join(idHR,by='Operacion') %>% 
  mutate(Saldo2M= ifelse(appsU>=2,saldous,0)) %>% 
  mutate(Op2M = ifelse(appsU>=2,1,0)) %>% 
  mutate(Saldo3M = ifelse(appsU>=3,saldous,0)) %>% 
  mutate(Op3M = ifelse(appsU>=3,1,0)) %>% 
  mutate(Saldo4M = ifelse(appsU>=4,saldous,0)) %>% 
  mutate(Op4M = ifelse(appsU>=4,1,0)) %>% 
  mutate(Saldo5M = ifelse(appsU>=5,saldous,0)) %>% 
  mutate(Op5M = ifelse(appsU>=5,1,0)) %>% 
  mutate(Saldo6M = ifelse(appsU>=6,saldous,0)) %>% 
  mutate(Op6M = ifelse(appsU>=6,1,0)) %>%
  dplyr::rename(Sucursal = sucursal,
                Agencia = NOMBRE_AGENCIA,
                Asesor = NOMBRE_ASESOR,
                Tipo_Credito = tipoCred,
                Actividad_Cliente = labGrupoC,
                Destino_Credito = labGrupoD,
                Estado = ESTADO,
                Prevision_USD = previus,
                Saldo_USD = saldous,
                Instancias_AR = appsH,
                Instancias_UR = appsU,
                Rango_Monto = rangom,
                Operaciones = opTot,
                Cartera_Reprogramada = saldoReprog,
                Saldo_Mora = saldoMora,
                PaR_30_Reprogramada = par0Reprog,
                PaR_0 = par0,
                PaR_30 = par30,
                PaR_0_Reprogramada = par30Reprog,
                Cartera_Diferida_ASFI = saldoDif,
                Cartera_Diferida_RC = saldoDifFranz,
                PaR_0_Refinanciada = par0Ref,
                PaR_30_Refinanciada = par30Ref,
                Cartera_Refinanciada = saldoRef,
                PaR_0_Diferida_ASFI = par0Dif,
                PaR_30_Diferida_ASFI = par30Dif ,
                PaR_0_Diferida_RC = par0DifFranz,
                PaR_30_Diferida_RC = par30DifFranz,
                Dias_Mora=DIASMORA)

####____READING CRUCES____####
base0 <- P2uh %>% 
  dplyr::filter(myPago=='dic. 2022') %>% 
  dplyr::filter(appsU==1) %>% 
  dplyr::rename(Ultimo_mes = appsU) %>% 
  select(Operacion,Ultimo_mes)
  
base1 <- readRDS("D:/!bso/bases/rds/joinPlain_dic2022.rds") %>% 
  select(-fbase)

base2 <- readRDS("D:/!bso/bases/rds/finalDetFcast_Diciembre2022_Aonly.rds") %>% 
  select(OPERACION,prob_det) %>% 
  rename(Operacion=OPERACION)

base3 <- readRDS("D:/!bso/bases/rds/dummyPrev_Dic22.rds")

base4 <- readRDS('D:/!bso/Consultas/ConsultasFullEne17Ene23.rds') %>% 
  dplyr::filter(myCon=='ene. 2023') %>% 
  group_by(CI) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  select(CI,Consultado,ENTIDAD)
  
base5 <- readRDS('D:/!bso/bases/rds/basesimpleVip.rds') %>% 
  rename(Operacion=OPERACION)

base6 <- readRDS('D:/!bso/bases/rds/compra_dic2022.rds') %>% 
  rename(Operacion=OPERACION)

dummy <- lastCierre %>% 
  dplyr::filter(Estado!='CASTIGADA') %>% 
  left_join(base0,by="Operacion") %>% 
  left_join(base1,by="CI") %>%
  left_join(base2,by="Operacion") %>% 
  left_join(base3,by="CI") %>% 
  left_join(base4,by="CI") %>% 
  left_join(base5,by=c("Operacion","CI")) %>% 
  left_join(base6,by=c("Operacion","CI")) %>% 
  replace_na(list(califBSO_2="NC",califSF_2="NC",Instancias_UR=0,
                  Ultimo_mes=0,Consultado=0,target=0)) %>%
  rename(Peor_Calificacion_BSO = califBSO_2,
         Peor_Calificacion_SF = califSF_2,
         Cta_Cliente = CTACLIENTE,
         Probabilidad_Deterioro = prob_det,
         `Nro_Pagos_Tardios` = Instancias_UR,
         VIP_RC = target,
         Percentil_Prob_VIP=Percentil_Prob,
         Score_VIP=Score,
         Decil_Score_VIP=Decil_Score)

write_rds(dummy,'D:/!bso/shared/FullJoin_Ene23.rds')
saveRDS(dummy,'D:/!bso/shared/FullJoin_Ene23_v2.rds',compress = T)

dummy2 <- dummy %>% 
  select(Operacion,Cta_Cliente,CI,Estado,ctaCont,Saldo_USD,Prevision_USD,Dias_Mora,Sucursal,
         Agencia,Asesor,Rango_Monto,NOMBRE_CLIENTE,Tipo_Credito,CALIFICACION,Nro_Pagos_Tardios,
         FDESEMBOLSO,cosechaY,Ultimo_mes,Peor_Calificacion_BSO,Peor_Calificacion_SF,
         Probabilidad_Deterioro,califPot,previusNew,Consultado,ENTIDAD,VIP_RC,Percentil_Prob_VIP,
         Probabilidad_VIP,Score_VIP,Decil_Score_VIP,Historial_SF,Calificacion_SF,Saldo_USD_SF,
         MontoOriginal_SF,Entidad_SF,Sigla_Entidad_SF,Tipo_Obligado_SF)
write.xlsx(dummy2,'D:/!bso/shared/FullJoin_Ene23.xlsx')  

dummy <- read.xlsx('D:/!bso/shared/FullJoin_Ene23.xlsx')
