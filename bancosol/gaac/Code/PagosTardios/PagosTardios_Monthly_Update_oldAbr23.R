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
library(scales)
library(stringr)
library(forcats)
library(tseries)
library(scales)
library(janitor)
library(ggplot2)
library(openxlsx)
require(XLConnect)
Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
####____READING PAGOS LAST MONTH____####
PagosMes <- fread('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/03_Mora_Hora/PagosCarteraFeb2023.csv',
                  encoding = "Latin-1",fill = T,sep=',')
PagosMes <- PagosMes %>% 
  dplyr::filter(month(FechaPago)==2) %>% 
  mutate(across(where(is.POSIXct),as.IDate))
####____CONSOLIDANDO PAGOS FULL____####
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraEne22Dic22.csv',
               encoding = "Latin-1",fill = T,sep=',')
Pagos <- Pagos %>% bind_rows(PagosMes)
write_excel_csv(Pagos,'D:/!bso/bases/csv/PagosCarteraEne22Ene23.csv')
Pagos <- read_csv('D:/!bso/bases/csv/PagosCarteraEne22Ene23.csv',
                  col_select = c(Operacion,FechaPrevistaPago,FechaPago,CapitalPagado,HoraPago))
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraEne22Ene23.csv',
               encoding = "Latin-1",fill=T,sep=',')
####____PAGOS FULL____####
P2full <- Pagos %>% 
  mutate(mesPago = month(FechaPago)) %>% 
  mutate(dayPago = day(FechaPago)) %>% 
  mutate(yearPago = year(FechaPago)) %>% 
  mutate(myPago = as.yearmon(FechaPago)) %>% 
  dplyr::filter(yearPago >= 2022) %>% 
  select(Operacion, CapitalPagado, FechaPago, myPago, yearPago, mesPago, dayPago, 
         HoraPago, FechaPrevistaPago) %>% 
  group_by(Operacion, myPago, yearPago, mesPago) %>% 
  mutate(Hora_UltDia = max(HoraPago[which(dayPago==max(dayPago))])) %>% 
  summarise(CapitalPagado = sum(CapitalPagado), Hora_UltDia = max(Hora_UltDia),
            FechaPrevistaPago = max(FechaPrevistaPago),dayPago = max(dayPago)) %>% 
  ungroup()

write_rds(P2full, 'D:/!bso/mph/P2FullLast.rds')
P2full <- readRDS('D:/!bso/mph/P2FullLast.rds') %>% 
  # dplyr::filter(myPago<="nov. 2022") %>% #Si se quiere replicar de meses anteriores se pone el filtro de mes
  glimpse()
# P2full <- P2full %>% 
#   dplyr::filter(myPago<'ene. 2023')
####____ADDING A MONTH TO PAGOS FULL____####
PagosMes <- fread('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/03_Mora_Hora/PagosCarteraMar2023.csv',
                  encoding = "UTF-8",fill = T,sep=',')
PagosMes <- PagosMes %>% 
  mutate(across(where(is.POSIXct),as.IDate)) %>% 
  dplyr::filter(month(FechaPago)==3)
P2fullMes <- PagosMes %>% 
  mutate(mesPago = month(FechaPago)) %>% 
  mutate(dayPago = day(FechaPago)) %>% 
  mutate(yearPago = year(FechaPago)) %>% 
  mutate(myPago = as.yearmon(FechaPago)) %>% 
  dplyr::filter(yearPago > 2022) %>% 
  select(Operacion, CapitalPagado, FechaPago, myPago, yearPago, mesPago, dayPago, 
         HoraPago, FechaPrevistaPago) %>% 
  group_by(Operacion, myPago, yearPago, mesPago) %>% 
  mutate(Hora_UltDia = max(HoraPago[which(dayPago==max(dayPago))])) %>% 
  summarise(CapitalPagado = sum(CapitalPagado), Hora_UltDia = max(Hora_UltDia),
            FechaPrevistaPago = max(FechaPrevistaPago),dayPago = max(dayPago)) %>% 
  ungroup()

P2full <- readRDS('D:/!bso/mph/P2FullLast.rds') %>% 
  glimpse()
P2full %>% #Check de capital y operaciones con cuota pagada por mes
  group_by(myPago) %>% 
  summarise(capital=sum(CapitalPagado),n=n_distinct(Operacion))
P2fullMes %>% #Check de capital y operaciones con cuota pagada por mes
  group_by(myPago) %>% 
  summarise(capital=sum(CapitalPagado),n=n_distinct(Operacion))

P2full <- P2full %>% 
  bind_rows(P2fullMes)

write_rds(P2full,'D:/!bso/mph/P2FullLast.rds')
P2full <- readRDS('D:/!bso/mph/P2FullLast.rds')
####____EXTRAYENDO PAGOS TARDÍOS____####
P2uh <- P2full %>%
  mutate(hourUltDia = as.numeric(str_sub(Hora_UltDia, 1, 2))) %>%
  dplyr::filter(myPago > 'mar. 2022') %>% #Se actualiza para conservar los últimos 12 meses
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
write_rds(P2uh, 'D:/!bso/mph/Pagos_conDummyTardio.rds')
P2uh <- read_rds('D:/!bso/mph/Pagos_conDummyTardio.rds')

check <- P2uh %>% #En este check aún se consideran los clientes que luego de pagar están en estado Z cancelado
  group_by(myPago) %>% 
  summarise(n=n(),Capital=sum(CapitalPagado),PagosAH=sum(appsH),PagosUH=sum(appsU)) %>% 
  ungroup()
####____PAGOS TARDÍOS EVOLUTIVO____####
P2Tardios <- P2uh %>% 
  dplyr::filter(appsU==1)
####____SALDO TARDIO EVOLUTIVO____####
codAge <- read.xlsx("D:/!bso/bases/excel/CodAgeSucReg.xlsx")
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
years <- c("2022","2023")
myrds <- as.vector(sapply(years, function(x){paste0(mes,x)}))
myrds <- myrds[c(which(myrds=="Abr2022"):which(myrds=="Mar2023"))] #Actualizar para los últimos 12 meses

bdcList <- list()
for (i in 1:length(myrds)) {
  tryCatch({
    print(myrds[i])
    Cierre <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds[i],'.rds')) %>% 
      # left_join(codAge,by="AGENCIA") %>% 
      dplyr::filter(MODULO != 131) %>%
      dplyr::filter(ESTADO != 'CASTIGADA') %>%
      mutate(cosechaS=case_when(cosechaY<2020 ~ '< 2020',
                                cosechaY==2021 & cosechaM<='jun. 2021' ~ 'I/2021',
                                cosechaY==2021 & cosechaM>'jun. 2021' ~ 'II/2021',
                                TRUE~ as.character(cosechaY))) %>% 
      rename(Operacion=OPERACION) %>% 
      mutate(Reprogramado=ifelse(saldoReprog>0,1,0)) %>% 
      mutate(Refinanciado=ifelse(saldoRef>0,1,0)) %>% 
      mutate(Diferido=ifelse(saldoDif>0,1,0)) %>% 
      mutate(Periodo_Gracia=ifelse(GRACIA_MESES>0,1,0)) %>% 
      mutate(Rango_Desembolso = ifelse(montous > 20000, '20K+', 'under20K')) %>% 
      mutate(myPago=as.yearmon(monDate)) %>% 
      group_by(myPago) %>% #Conservamos el saldo, par0 y par30 de todo el mes
      mutate(saldoTot=sum(saldous),
             par0Tot=sum(par0),
             par30Tot=sum(par30)) %>% 
      ungroup() %>% 
      select(myPago,Operacion,Sucursal,NOMBRE_AGENCIA,NOMBRE_ASESOR,tipoCred, FDESEMBOLSO,
             labGrupoC,labGrupoD,ESTADO,DIASMORA, Rango_Desembolso,rangom,previus,
             opTot,saldous,saldoMora,par0,par30,Reprogramado, Refinanciado, Diferido, 
             Periodo_Gracia,ctaCont, saldoTot,par0Tot,par30Tot, cosechaS) %>% 
      left_join(P2Tardios, by=c("Operacion","myPago")) %>%  #Unimos la base de cartera con la lista de pagos tardios
      mutate(appsU=ifelse(is.na(appsU),0,appsU)) %>% 
      dplyr::filter(appsU==1) %>% 
      mutate(Tipo_Cartera=case_when(ctaCont=="131"~"Vigente",
                                    ctaCont=="133"~"Vencida",
                                    ctaCont=="134"~"Ejecución",
                                    ctaCont=="135"~"Repro Vigente",
                                    ctaCont=="136"~"Repro Vencida",
                                    ctaCont=="137"~"Repro Ejecución"))
    
    bdcList[[i]] <- Cierre
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

bdcEvol <- bind_rows(bdcList) %>% 
  mutate(myPago=as.yearmon(myPago))
check2 <- bdcEvol %>% #Este check ya no contiene cancelados tras el pago
  group_by(myPago) %>% 
  summarise(saldo=sum(saldous),nOps=n())

write_rds(bdcEvol,'D:/!bso/mph/bdcEvolAbr22Mar23.rds')
bdcEvol <- readRDS('D:/!bso/mph/bdcEvolAbr22Mar23.rds')
####____ADD A MONTH TO SALDO TARDIO EVOLUTIVO____####
bdcEvol <- readRDS('D:/!bso/mph/bdcEvolFeb22Ene23.rds') #Leer consolidado hasta mes anterior
myrds <- "Ene2023"
tryCatch({
  print(myrds[i])
  Cierre <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',myrds,'.rds')) %>% 
    left_join(agen,by="AGENCIA") %>% 
    dplyr::filter(MODULO != 131) %>%
    dplyr::filter(ESTADO != 'CASTIGADA') %>%
    mutate(cosechaS=case_when(cosechaY<2020 ~ '< 2020',
                              cosechaY==2021 & cosechaM<='jun. 2021' ~ 'I/2021',
                              cosechaY==2021 & cosechaM>'jun. 2021' ~ 'II/2021',
                              TRUE~ as.character(cosechaY))) %>% 
    # mutate(fdes = dmy(FDESEMBOLSO)) %>%
    rename(Operacion=OPERACION) %>% 
    mutate(Reprogramado=ifelse(saldoReprog>0,1,0)) %>% 
    mutate(Refinanciado=ifelse(saldoRef>0,1,0)) %>% 
    mutate(Diferido=ifelse(saldoDif>0,1,0)) %>% 
    mutate(Periodo_Gracia=ifelse(GRACIA_MESES>0,1,0)) %>% 
    mutate(Rango_Desembolso = ifelse(montous > 20000, '20K+', 'under20K')) %>% 
    mutate(myPago=as.yearmon(monDate)) %>% 
    group_by(myPago) %>% #Conservamos el saldo, par0 y par30 de todo el mes
    mutate(saldoTot=sum(saldous),
           par0Tot=sum(par0),
           par30Tot=sum(par30)) %>% 
    ungroup() %>% 
    select(myPago,Operacion,sucursal,NOMBRE_AGENCIA,NOMBRE_ASESOR,tipoCred, FDESEMBOLSO,
           labGrupoC,labGrupoD,ESTADO,DIASMORA, Rango_Desembolso,rangom,previus,
           opTot,saldous,saldoMora,par0,par30,Reprogramado, Refinanciado,Diferido,
           Periodo_Gracia,ctaCont, saldoTot,par0Tot,par30Tot,cosechaS) %>% 
    left_join(P2Tardios, by=c("Operacion","myPago")) %>%  #Unimos la base de cartera con la lista de pagos tardios
    mutate(appsU=ifelse(is.na(appsU),0,appsU)) %>% 
    dplyr::filter(appsU==1) %>% 
    mutate(Tipo_Cartera=case_when(ctaCont=="131"~"Vigente",
                                  ctaCont=="133"~"Vencida",
                                  ctaCont=="134"~"Ejecución",
                                  ctaCont=="135"~"Repro Vigente",
                                  ctaCont=="136"~"Repro Vencida",
                                  ctaCont=="137"~"Repro Ejecución"))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

bdcEvol <- bdcEvol %>% 
  bind_rows(Cierre)
write_rds(bdcEvol,'D:/!bso/mph/bdcEvolFeb22Ene23.rds')
####____COUNTING PAGOS TARDÍOS POR OPERACION____####
P2uh_group <- P2uh %>%
  # dplyr::filter(myPago<="mar. 2023") %>% 
  dplyr::filter(appsH >= 1 & appsU >= 1) %>% 
  group_by(Operacion) %>% 
  mutate(appsH = sum(appsH))%>%
  mutate(appsU = sum(appsU))%>%
  ungroup() %>% 
  group_by(Operacion) %>% 
  mutate(Ultimo_mes=ifelse(myPago=="mar. 2023",1,0)) %>% # Identificando pagos tardios del último mes
  arrange(Operacion, desc(myPago)) %>% 
  mutate(pos = row_number()) %>% 
  dplyr::filter(pos == 1) %>% 
  ungroup()

sum(P2uh_group$Ultimo_mes==1,na.rm = T)
####____CRUZANDO CONTEO DE PAGOS TARDÍOS CON ÚLTIMO CIERRE####
lastCierre2 <- readRDS('D:/!bso/girCartera/rds/ec_Mar2023.rds') %>% #Actualizar al último mes
  mutate(cosechaS=case_when(cosechaY<2020 ~ '< 2020',
                            cosechaY==2021 & cosechaM<='jun. 2021' ~ 'I/2021',
                            cosechaY==2021 & cosechaM>'jun. 2021' ~ 'II/2021',
                            TRUE~ as.character(cosechaY))) %>% 
  mutate(NOMBRE_CLIENTE=paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT,sep = " ")) %>% 
  # left_join(agen,by="AGENCIA") %>% #Se añade el nombre de la agencia
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>% #Eliminamos las operaciones castigadas
  rename(Operacion=OPERACION) %>% 
  mutate(Reprogramado=ifelse(saldoReprog>0,1,0)) %>% 
  mutate(Refinanciado=ifelse(saldoRef>0,1,0)) %>% 
  mutate(Diferido=ifelse(saldoDif>0,1,0)) %>% 
  mutate(Periodo_Gracia=ifelse(GRACIA_MESES>0,1,0)) %>% 
  mutate(Rango_Desembolso = ifelse(montous > 20000, '20K+', 'under20K')) %>% 
  select(Operacion,CI,CTACLIENTE,Sucursal,NOMBRE_CLIENTE,NOMBRE_AGENCIA,NOMBRE_ASESOR,tipoCred,labGrupoC,
         labGrupoD,ESTADO,ctaCont,DIASMORA, Rango_Desembolso,rangom,previus,opTot, FDESEMBOLSO,
         saldous,saldoMora,saldoDifFranz,saldoDif,saldoRef,saldoReprog,CALIFICACION,par0,par30,
         par0Reprog,par30Reprog,par0Ref,par30Ref,par0Dif,par30Dif, cosechaY, cosechaS,
         par0DifFranz,par30DifFranz,Reprogramado, Refinanciado, Diferido, Periodo_Gracia)

idHR <- P2uh_group %>% 
  dplyr::filter(appsU>=1) %>% 
  select(Operacion, appsH, appsU,mesPago,
         dayPago,hourUltDia,Ultimo_mes) %>%
  left_join(lastCierre2, by = 'Operacion') %>% 
  mutate(bin2 = case_when(dayPago==29~'1. Pagó antepenúltimo día',
                          dayPago==30~'2. Pagó penúltimo día',
                          dayPago==31 & hourUltDia<=12~'3. Pagó último día antes de 12',
                          dayPago==31 & hourUltDia>12 & hourUltDia<=14~'4. Pagó último día de 12 a 14',
                          dayPago==31 & hourUltDia>14~'5. Pagó último día de 14 en adelante',
                          TRUE~'0. Pagó antes del antepenúltimo día'
  )) %>% 
  select(Operacion,appsH,appsU,bin2,Ultimo_mes)

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
  dplyr::rename(Agencia = NOMBRE_AGENCIA,
                Asesor = NOMBRE_ASESOR,
                Tipo_Credito = tipoCred,
                Actividad_Cliente = labGrupoC,
                Destino_Credito = labGrupoD,
                Estado = ESTADO,
                Prevision_USD = previus,
                Saldo_USD = saldous,
                Instancias_AR = appsH,
                Instancias_UR = appsU,
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

sum(lastCierre$Ultimo_mes==1,na.rm = T)
write_xlsx(lastCierre, 'D:/!bso/mph/BaseCarteraMar2023_join.xlsx')
write.csv(lastCierre,"D:/!bso/mph/Oreports/lastCierrreUR_Mar2023.csv",
          row.names = F)
####____LEYENDO LAST CIERRE____####
lastCierre2 <- lastCierre %>% 
  mutate(M4M=ifelse(Saldo_USD!=0,Saldo4M/sum(Saldo_USD),0)) %>% 
  group_by(Sucursal) %>% 
  summarise(M4M=sum(M4M,na.rm = T),saldo=sum(Saldo_USD)/2088293354,M4MCOR=M4M/saldo) %>% 
  adorn_totals("row")

lastCierre <- fread("D:/!bso/mph/Oreports/lastCierrreUR_Nov2022.csv",
                    encoding = "UTF-8",sep=",",fill=T)
lastCierre <- lastCierrre %>% 
  mutate(mora=ifelse(Saldo_USD!=0,PaR_30/Saldo_USD,0)) %>% 
  group_by(Sucursal) %>% 
  summarise(mora=sum(PaR_30)/sum(Saldo_USD))
