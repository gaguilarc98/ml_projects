#-------------------------
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
library(quantmod)
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data

Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
paleta <- colorRampPalette(c("blue2","slateblue4","slateblue3","violetred3","red3","tan2","yellow3","yellow2"),bias=1.5)
cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
#__________________________________________________________
# last close
cierreSep <- read_rds('D:/!bso/output/rds/ec_Sep2022.rds') %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  dplyr::filter(DIASMORA > 0) %>% 
  group_by(AGENCIA) %>% 
  summarise(saldo = sum(saldous), nopsMora = n())
write.xlsx(cierreSep, 'D:/!bso/mph/descMoraXAge_sep2022.xlsx')

portSep <- read_rds('D:/!bso/output/rds/ec_Sep2022.rds') %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  group_by(AGENCIA) %>% 
  summarise(saldo = sum(saldous), nopsMora = n())
write.xlsx(portSep, 'D:/!bso/mph/descPortXAge_sep2022.xlsx')
#___________________________________________________________
#####_____LECTURA de BASE DIARIA____####
diaria0 <- fread('D:/!bso/girCartera/BaseCartera_20220129.txt',
                encoding = 'Latin-1', fill = T)
diaria1 <- fread('D:/!bso/girCartera/BaseCartera_20220130.txt',
                 encoding = 'Latin-1', fill = T)
cierre <- fread('D:/!bso/gircartera/BaseCarteraEne2022.txt',
                encoding = 'Latin-1', fill = T) %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(activoAgo = 1) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>%
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(par0 = ifelse(DIASMORA>0, saldous, 0)) %>% 
  #select(OPERACION, saldous, par0) %>% 
  rename(Operacion=OPERACION)

dia0 <- diaria0 %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(activoAgo = 1) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>%
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(par0 = ifelse(DIASMORA>0, saldous, 0)) %>% 
  select(OPERACION, saldous, par0, DIASMORA) %>% 
  rename(Operacion=OPERACION)

dia1 <- diaria1 %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(activoAgo = 1) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>%
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(par0 = ifelse(DIASMORA>0, saldous, 0)) %>% 
  select(OPERACION, saldous, par0, DIASMORA) %>% 
  rename(Operacion=OPERACION)

diaMora0 <- diaria0 %>% 
dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(activoAgo = 1) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>%
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(par0 = ifelse(DIASMORA>0, saldous, 0)) %>% 
  dplyr::filter(DIASMORA>0) %>% 
  select(OPERACION, saldous) %>% 
  rename(Operacion=OPERACION)

diaMora1 <- diaria1 %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(activoAgo = 1) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>%
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(par0 = ifelse(DIASMORA>0, saldous, 0)) %>% 
  dplyr::filter(DIASMORA>0) %>% 
  select(OPERACION, saldous) %>% 
  rename(Operacion=OPERACION)

diaMoraC <- cierre %>% 
  dplyr::filter(DIASMORA>0) %>% 
  select(Operacion, saldous) 

par0Init <- sum(diaMora$saldous)
saldoInit <- sum(dia$saldous)
par0Fin <- sum(cierre$par0)
saldoFin <- sum(cierre$saldous)

#newMora1 <- !(dia0[dia0$DIASMORA ==  ]$Operacion %in% diaMora1$Operacion
#---------------------------------------------------------
#####_____PARA ENERO_____####
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraDesdeEnero2022.csv')
P2 <- Pagos %>% 
  dplyr::filter(FechaPago>=as.Date('2022-01-30') & FechaPago<=as.Date('2022-01-31')) %>%
  select(Operacion, CapitalPagado, FechaPago, HoraPago) %>% 
  group_by(Operacion) %>% 
  summarise(HoraPago = max(HoraPago), FechaPago = max(FechaPago),
            CapitalPagado = sum(CapitalPagado)) %>% 
  left_join(dia0, by='Operacion')

diaPago <- diaMora0 %>% 
  left_join(P2, by='Operacion') %>% 
  dplyr::filter(!is.na(HoraPago)) %>% 
  mutate(hora=as.POSIXct(paste(FechaPago,HoraPago),format="%Y-%m-%d %H:%M:%S"))
par0Pagox <- sum(diaPago$saldous.x)
par0Pagoy <- sum(diaPago$saldous.y)

bins <- seq.POSIXt(min(diaPago$hora),(max(diaPago$hora)+60*60),by="60 min") #Add to max 60 times the size of by

diaPago <- diaPago %>% 
  mutate(horabinned=cut(hora,breaks=bins)) %>% 
  #mutate(saldous = ifelse(Moneda == 0, CapitalPagado/6.86, CapitalPagado)) %>%
  #mutate(saldous = CapitalPagado) %>% 
  select(horabinned, Operacion, saldous.x) %>% 
  group_by(horabinned) %>% 
  summarise(n = n(), saldous = sum(saldous.x, na.rm = T)) %>% 
  mutate(saldoAcum = cumsum(saldous),
         opsAcum = cumsum(n),
         saldoProm = saldous/n)

ggplot(diaPago,aes(x=horabinned,y=n))+
  geom_line(group=1,size=1.25)+
  labs(x="Fecha",y="Número de Pagos") + theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))
  
#___________________________________________________________
####____Lectura de Pagos diarios 012022 - 092022____####
#Summary por operacion, año, mes y dia de pago
PagosOct <- fread('D:/!bso/bases/csv/PagosCartera_Oct_2022.csv',
                  encoding = "Latin-1",fill = T,sep=',')
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraDesdeEnero2022.csv',
               encoding = "Latin-1",fill = T,sep=',') %>% 
  dplyr::filter(month(FechaPago)!=10) %>% 
  bind_rows(PagosOct)
  
PagosNov <- fread('D:/!bso/bases/csv/Bases Transacciones 202211.csv',
                  encoding = "Latin-1",fill = T,sep=',') %>% 
  dplyr::filter(month(FechaPago)==11)  

PagosNov$codAsesorAsignacion <- as.character(PagosNov$codAsesorAsignacion)
PagosNov$codRegional <- as.character(PagosNov$codRegional)
PagosNov$codAgencia <- as.character(PagosNov$codAgencia)

PagosDic <- fread('D:/!bso/bases/csv/PagosCarteraDic2022.csv',
                  encoding = "Latin-1",fill = T,sep=',') %>% 
  dplyr::filter(month(FechaPago)==12)  

Pagos2 <- fread('D:/!bso/bases/csv/PagosCarteraDesdeEne2022_Oct2022.csv',
                encoding = "Latin-1",fill = T,sep=',') %>% 
  dplyr::filter(month(FechaPago)!=11)

Pagos2 <- Pagos2 %>% bind_rows(PagosNov) %>% bind_rows(PagosDic)
####____BASE SANEADA____####
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraEne22Dic22.csv',
               encoding = "Latin-1",fill = T,sep=',')
P2full <- Pagos %>% 
  #dplyr::filter(FechaPago>=as.Date('2022-01-30') & FechaPago<=as.Date('2022-01-31')) %>%
  mutate(mesPago = month(FechaPago)) %>% 
  mutate(dayPago = day(FechaPago)) %>% 
  mutate(yearPago = year(FechaPago)) %>% 
  #mutate(Hora = hour(as.POSIXct(HoraPago,format="%H:%M:%S")))
  select(Operacion, CapitalPagado, FechaPago, HoraPago, mesPago, dayPago, 
         yearPago, FechaPrevistaPago) %>% 
  # group_by(Operacion, mesPago, dayPago, yearPago) %>% 
  # summarise(HoraPago = max(HoraPago), FechaPrevistaPago = max(FechaPrevistaPago)) %>%
  # arrange(Operacion, mesPago, yearPago) %>%
  # ungroup() %>%
  group_by(Operacion, mesPago, yearPago) %>% 
  mutate(Hora_UltDia = max(HoraPago[which(dayPago==max(dayPago))])) %>% 
  summarise(HoraPago = max(HoraPago), Hora_UltDia = max(Hora_UltDia),
            FechaPrevistaPago = max(FechaPrevistaPago),dayPago = max(dayPago)) %>% 
  ungroup() %>% 
  dplyr::filter(yearPago == 2022) %>% 
  group_by(Operacion) %>% 
  mutate(apps = max(row_number())) %>% 
  ungroup()

write.csv(P2full, "D:/!bso/mph/P2FullLast.csv",row.names = F)
P2full <- fread("D:/!bso/mph/P2FullLast.csv",
                encoding = "Latin-1",fill = T,sep=',')
####___PLOT NUMERO DE PERSONAS POR DIA DEL MES EN EL QUE PAGAN____####
gph <- P2full %>%
  select(dayPago, Operacion) %>% 
  group_by(dayPago) %>% 
  tally()
ggplot(gph, aes(x = as.factor(dayPago), y = n)) + geom_bar(stat = 'identity')

#------------------------------------------
####___PAGOS RIESGOSOS POR AÑO___####
P2rec <- P2full %>% 
  mutate(hour = as.numeric(str_sub(HoraPago, 1, 2))) %>% 
  group_by(mesPago) %>% 
  mutate(maxDia = max(dayPago)) %>%
  ungroup() %>% 
  mutate(FechaPago = as.Date(paste(yearPago, mesPago, dayPago), format = '%Y%m%d')) %>% 
  mutate(apps = ifelse(dayPago >= maxDia-2 & FechaPago > FechaPrevistaPago, 1, 0)) %>% 
  group_by(Operacion) %>% 
  mutate(apps = sum(apps)) %>% 
  dplyr::filter(apps > 1) %>% 
  group_by(Operacion) %>% 
  arrange(Operacion, mesPago, dayPago) %>% 
  mutate(pos = row_number()) %>% 
  dplyr::filter(pos == apps & dayPago >= 29) %>% 
  ungroup()

#NUMERO DE OPERACIONES RIESGOSAS AL AÑO
gph1 <- P2rec %>% 
  select(apps, Operacion) %>% 
  group_by(apps) %>% 
  tally()
  
ggplot(gph1, aes(x = as.factor(apps), y = n)) + geom_bar(stat = 'identity') +
  theme_minimal() + xlab('# de pagos riesgosos en 2022') +
  ylab('Operaciones') +
  geom_text(aes(label = n), vjust = 1.5, colour = "white") + 
  labs(caption = "Pago riesgoso: Pago posterior a la fecha de pago prevista en día 29 o posterior")
ggsave('D:/!bso/mph/OpsAltoRiesgo.png')

#------------------------------------------
####____PAGOS ULTRA RIESGOSOS POR AÑO____####
P2ultra <- P2full %>%
  mutate(hour = as.numeric(str_sub(HoraPago, 1, 2))) %>% 
  mutate(hourUltDia = as.numeric(str_sub(Hora_UltDia, 1, 2))) %>% 
  group_by(mesPago) %>% 
  mutate(maxDia = max(dayPago)) %>% 
  ungroup() %>% 
  mutate(FechaPago = as.Date(paste(yearPago, mesPago, dayPago), format = '%Y%m%d')) %>% 
  mutate(apps = ifelse(dayPago == maxDia & FechaPago > FechaPrevistaPago & 
                      hourUltDia >= 12, 1, 0)) %>% 
  group_by(Operacion) %>% 
  mutate(apps = sum(apps))%>% 
  ungroup() %>% 
  dplyr::filter(dayPago == maxDia) %>% 
  dplyr::filter(apps > 1) %>% 
  group_by(Operacion) %>% 
  arrange(Operacion, mesPago, dayPago) %>% 
  mutate(pos = row_number()) %>% 
  dplyr::filter(pos == apps) %>% 
  ungroup()

#NUMERO DE OPERACIONES ULTRA RIESGOSAS AL AÑO
gph2 <- P2ultra %>% 
  select(apps, Operacion) %>% 
  group_by(apps) %>% 
  tally()

ggplot(gph2, aes(x = as.factor(apps), y = n)) + geom_bar(stat = 'identity') +
  theme_minimal() + xlab('# de pagos riesgosos en 2022') +
  ylab('Operaciones') +
  geom_text(aes(label = n), vjust = 1.5, colour = "white") + 
  labs(caption = "Pago riesgoso: Pago posterior a la fecha de pago prevista en día 29 o posterior")
ggsave('D:/!bso/mph/OpsUltraRiesgo.png')

#_______________________________________________________________________________
####____PAGOS RIESGOSOS Y ULTRA RIESGOSOS____####
P2uh <- P2full %>%
  mutate(hour = as.numeric(str_sub(HoraPago, 1, 2))) %>% 
  mutate(hourUltDia = as.numeric(str_sub(Hora_UltDia, 1, 2))) %>% 
  group_by(mesPago) %>% 
  mutate(maxDia = max(dayPago)) %>% 
  mutate(maxDia = ifelse(mesPago==2,25,maxDia)) %>% 
  mutate(maxDia = ifelse(mesPago==12,30,maxDia)) %>% 
  ungroup() %>% 
  mutate(FechaPago = as.Date(paste(yearPago, mesPago, dayPago), format = '%Y%m%d')) %>% 
  mutate(appsH = ifelse(dayPago >= maxDia-2 & FechaPago > FechaPrevistaPago, 1, 0)) %>% 
  mutate(appsU = case_when(mesPago==12 & dayPago>=maxDia & FechaPago > FechaPrevistaPago &
                             hourUltDia >=12~1,
                           mesPago!=12 & dayPago==maxDia & FechaPago > FechaPrevistaPago &
                             hourUltDia>=12~1,TRUE~0)) %>% 
  # mutate(appsU = ifelse(dayPago == maxDia & FechaPago > FechaPrevistaPago & 
  #                         hourUltDia >= 12, 1, 0)) %>% 
  group_by(Operacion) %>% 
  mutate(appsH = sum(appsH))%>%
  mutate(appsU = sum(appsU))%>%
  ungroup() %>% 
  dplyr::filter(appsH >= 1 & appsU >= 1) %>% 
  group_by(Operacion) %>% 
  arrange(Operacion, mesPago, dayPago) %>% 
  mutate(pos = row_number()) %>% 
  dplyr::filter(pos == 1) %>% 
  ungroup()

table(P2uh$mesPago)
####____JOIN RIESGOSOS Y girCartera____####
agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")

lastCierre2 <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Dic2022.rds') %>% 
  mutate(NOMBRE_CLIENTE=paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT,sep = " ")) %>% 
  left_join(agen,by="AGENCIA") %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>%
  rename(Operacion=OPERACION) %>% 
  # mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>%
  # mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  # mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  # mutate(par1 = ifelse(DIASMORA>0, saldous, 0)) %>% 
  # mutate(tipoCred = case_when(substr(TIPO_CREDITO, 1,1) == 'M'~'Micro',
  #                             substr(TIPO_CREDITO, 1,1) == 'H'~'Vivienda',
  #                             substr(TIPO_CREDITO, 1,1) == 'N'~'Consumo',
  #                             substr(TIPO_CREDITO, 1,1) == 'P'~'PyMe',)) %>% 
  # mutate(sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
  # mutate(sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', sucursal)) %>% 
  # mutate(Sucursal = case_when(sucursal == '1' ~ 'Chuquisaca',
  #                             sucursal == '10' ~ 'El Alto',
  #                             sucursal == '2' ~ 'La Paz',
  #                             sucursal == '3' ~ 'Cochabamba',
  #                             sucursal == '4' ~ 'Oruro',
  #                             sucursal == '5' ~ 'Potosí',
  #                             sucursal == '6' ~ 'Tarija',
  #                             sucursal == '7' ~ 'Santa Cruz',
  #                             sucursal == '8' ~ 'Beni',
  #                             sucursal == '9' ~ 'Pando',)) %>% 
  mutate(Reprogramado=ifelse(saldoReprog>0,1,0)) %>% 
  mutate(Refinanciado=ifelse(saldoRef>0,1,0)) %>% 
  mutate(Diferido=ifelse(saldoDif>0,1,0)) %>% 
  mutate(Periodo_Gracia=ifelse(GRACIA_MESES>0,1,0)) %>% 
  mutate(Rango_Desembolso = ifelse(montous > 20000, '20K+', 'under20K')) %>% 
  select(Operacion,sucursal,NOMBRE_CLIENTE,NOMBRE_AGENCIA,NOMBRE_ASESOR,tipoCred,labGrupoC,
         labGrupoD,ESTADO,ctaCont,DIASMORA, Rango_Desembolso,rangom,previus,opTot,
         saldous,saldoMora,saldoDifFranz,saldoDif,saldoRef,saldoReprog,par0,par30,
         par0Reprog,par30Reprog,par0Ref,par30Ref,par0Dif,par30Dif,
         par0DifFranz,par30DifFranz,Reprogramado, Refinanciado, Diferido, Periodo_Gracia)

idHR <- P2uh %>% 
  select(Operacion, appsH, appsU,mesPago,
         dayPago,hourUltDia) %>%
  left_join(lastCierre2, by = 'Operacion') %>% 
  mutate(ESTADO = ifelse(is.na(saldous), 'CANCELADA', ESTADO))
table(idHR$ESTADO)
write_xlsx(idHR, 'D:/!bso/mph/descHR.xlsx')
bdc_hr <- lastCierre2 %>% 
  left_join(idfinal,by='Operacion')
#____________________________________________________________
####____LEFT JOIN bdc septiembre con idHR____####
# ahmed <- readRDS("D:/!bso/mph/opsHR.rds")
# which((idHR$Operacion%in%ahmed$Operacion)==F)
idHR <- idHR %>% 
  mutate(HR = 1) %>% 
  mutate(bin2 = case_when(dayPago==29~'1. Pagó antepenúltimo día',
                          dayPago==30~'2. Pagó penúltimo día',
                          dayPago==31 & hourUltDia<=12~'3. Pagó último día antes de 12',
                          dayPago==31 & hourUltDia>12 & hourUltDia<=14~'4. Pagó último día de 12 a 14',
                          dayPago==31 & hourUltDia>14~'5. Pagó último día de 14 en adelante',
                          TRUE~'0. Pagó antes del antepenúltimo día'
                          )) %>% 
  select(Operacion,appsH,appsU,dayPago,mesPago,hourUltDia,HR,bin2)
  

lastCierre <- lastCierre2 %>% 
  left_join(idHR,by='Operacion') %>% 
  mutate(HR = ifelse(is.na(HR),0,HR)) %>%
  mutate(Saldo_UR=ifelse(HR==1,saldous,0)) %>% 
  mutate(saldoDif=ifelse(saldoDif==0,NA,saldoDif)) %>% 
  mutate(saldoRef=ifelse(saldoRef==0,NA,saldoRef)) %>% 
  mutate(saldoDifFranz=ifelse(saldoDifFranz==0,NA,saldoDifFranz)) %>% 
  mutate(saldoReprog=ifelse(saldoReprog==0,NA,saldoReprog)) %>% 
  mutate(saldoMora=ifelse(saldoMora==0,NA,saldoMora)) %>% 
  mutate(saldoDif=ifelse(saldoDif==0,NA,saldoDif)) %>% 
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
                Mes_Pago = mesPago,
                Dia_Pago = dayPago,
                Hora_Ultimo_Dia= hourUltDia,
                Operacion_Riesgosa = HR,
                Pago_Ultimos_Dias = bin2,
                Rango_Monto = rangom,
                Operaciones_Totales = opTot,
                Cartera_Reprogramada = saldoReprog,
                Saldo_Mora = saldoMora,
                PaR_30_Reprogramada = par0Reprog,
                PaR_0_Bruta = par0,
                PaR_30_Bruta = par30,
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
  
convertNA <- function(vec){
  n <- ncol(vec)
  for (i in 1:n) {
    vec[[i]][which(vec[[i]]==0)] <- NA
  }
  return(vec)
}
lastCierre[,c(15:29,41)] <- convertNA(lastCierre[,c(15:29,41)])
write_xlsx(lastCierre, 'D:/!bso/mph/BaseCarteraDic2022_join_293031.xlsx')

lastCierre <- lastCierre2 %>% 
  left_join(idHR,by='Operacion') %>% 
  mutate(HR = ifelse(is.na(HR),0,HR)) %>%
  mutate(Saldo_UR=ifelse(HR==1,saldous,0)) %>% 
  mutate(saldoDif=ifelse(saldoDif==0,NA,saldoDif)) %>% 
  mutate(saldoRef=ifelse(saldoRef==0,NA,saldoRef)) %>% 
  mutate(saldoDifFranz=ifelse(saldoDifFranz==0,NA,saldoDifFranz)) %>% 
  mutate(saldoReprog=ifelse(saldoReprog==0,NA,saldoReprog)) %>% 
  mutate(saldoMora=ifelse(saldoMora==0,NA,saldoMora)) %>% 
  mutate(saldoDif=ifelse(saldoDif==0,NA,saldoDif)) %>% 
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
                Mes_Pago = mesPago,
                Dia_Pago = dayPago,
                Hora_Ultimo_Dia= hourUltDia,
                Operacion_UR = HR,
                Pago_Ultimos_Dias = bin2,
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
write.csv(lastCierre,"D:/!bso/mph/Oreports/lastCierrreUR_Dic2022_3031_v2.csv",row.names = F)
####____AÑADIENDO CTACONT####
lastCierre2 <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Nov2022.rds') %>% 
  select(OPERACION,ctaCont) %>% 
  rename(Operacion=OPERACION)
lastCierre <- read.csv("D:/!bso/mph/Oreports/lastCierrreUR_Nov2022.csv") %>% 
  left_join(lastCierre2,by="Operacion")
  
write.csv(lastCierre,"D:/!bso/mph/Oreports/lastCierrreUR_Nov2022.csv",row.names = F)
####____LISTA DE PAGOS TARDÍOS____####
lastCierre2 <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Dic2022.rds') %>% 
  select(OPERACION,CI,) %>% 
  rename(Operacion=OPERACION)
lastCierre <- read.csv("D:/!bso/mph/Oreports/lastCierrreUR_Dic2022_3031.csv")
listaTardios <- lastCierre %>% 
  select(Operacion, Instancias_UR) %>% 
  left_join(lastCierre2,by="Operacion") %>% 
  select(-Operacion)
write_xlsx(listaTardios,"D:/!bso/mph/202212ListaPagosRecurrentes.xlsx")
