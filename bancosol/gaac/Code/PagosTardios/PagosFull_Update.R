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
# require(XLConnect)

Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options("encoding" = "UTF-8")
options(scipen = 999)
#####____FUNCIONES_____####
summarypagos <- function(x,fecha,flow,fhigh){
  x %>%
    mutate(mesPago = month({{fecha}})) %>%
    mutate(dayPago = day({{fecha}})) %>%
    mutate(yearPago = year({{fecha}})) %>%
    mutate(myPago = as.yearmon({{fecha}})) %>%
    dplyr::filter(myPago>=as.yearmon(flow) & myPago <=as.yearmon(fhigh)) %>%
    select(Cuenta, Operacion, myPago, yearPago, mesPago, dayPago, HoraPago, 
           FechaPrevistaPago, CapitalPagado, InteresPagado) %>%
    group_by(Cuenta, Operacion, myPago, yearPago, mesPago) %>%
    # arrange(desc(dayPago)) %>%
    # mutate(Hora_UltDia = HoraPago[row_number()==1]) %>%
    mutate(Hora_UltDia = max(HoraPago[which(dayPago==max(dayPago))])) %>%
    summarise(CapitalPagado = sum(CapitalPagado), InteresPagado = sum(InteresPagado),Hora_UltDia = max(Hora_UltDia),
              FechaPrevistaPago = max(FechaPrevistaPago),dayPago = max(dayPago)) %>%
    ungroup()
}

getTardios <- function(x){
  x %>%
    mutate(hourUltDia = as.numeric(str_sub(Hora_UltDia, 1, 2))) %>%
    # dplyr::filter(myPago < "mar. 2020" | myPago >= "dic. 2020") %>%
    group_by(myPago, yearPago, mesPago) %>% 
    mutate(maxDia = max(dayPago)) %>% 
    mutate(maxDia = ifelse(myPago=='may. 2018',30,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='sep. 2018',29,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='dic. 2018',29,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='jun. 2019',29,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='dic. 2019',30,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='dic. 2020',30,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='ene. 2021',30,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='feb. 2021',27,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='dic. 2021',30,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='feb. 2022',25,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='dic. 2022',30,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='abr. 2023',29,maxDia)) %>% 
    ungroup() %>% 
    mutate(FechaPago = as.Date(paste(yearPago, mesPago, dayPago), format = '%Y%m%d')) %>% 
    mutate(appsH = ifelse(dayPago >= maxDia-2 & FechaPago > FechaPrevistaPago, 1, 0)) %>% 
    mutate(appsU = case_when(FechaPago > FechaPrevistaPago & ((dayPago==maxDia & hourUltDia >= 12) | 
                               dayPago>maxDia) ~ 1,
                             TRUE~0))
}
####____CONSOLIDADO ENE22 OCT22____####
Pagos <- fread(paste0('D:/!bso/bases/csv/PagosCartera','HastaOct2022','.csv'),
                        encoding = "UTF-8",fill = T,sep=',')
PagosOct <- fread(paste0('D:/!bso/bases/csv/PagosCartera','Oct2022','.csv'),
                        encoding = "UTF-8",fill = T,sep=',')
Pagos <- Pagos %>% 
  dplyr::filter(FechaTrx < as.Date("2022-10-01")) %>% 
  mutate(across(c(FechaPrevistaPago,FechaPago,FechaTrx),~as.Date(.x))) %>% 
  bind_rows(PagosOct)

write_csv(Pagos,"D:/!bso/bases/csv/PagosCarteraEne2022_Oct2022.csv")
Pagos <- fread(paste0('D:/!bso/bases/csv/PagosCartera','Ene2022_Oct2022','.csv'),
               encoding = "UTF-8",fill = T,sep=',')
####____READING PAGOS FULL RAW____####
Pagos <- read_csv('D:/!bso/bases/csv/PagosCarteraEne22Ene23.csv',
                  col_select = c(Operacion,FechaPrevistaPago,FechaPago,CapitalPagado,HoraPago))
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraEne22Ene23.csv',
               encoding = "Latin-1",fill=T,sep=',')
bases <- c("Ene2018_Jun2018","Jul2018_Dic2018","Ene2019_Jun2019","Jul2019_Dic2019",
           "Ene2020_Jun2020","Jul2020_Dic2020","Ene2021_Jun2021","Jul2021_Dic2021",
           "Ene2022_Oct2022","Nov2022","Dic2022","Ene2023","Feb2023","Mar2023","Abr2023")
low <- c("ene. 2018","jul. 2018","ene. 2019","jul. 2019","ene. 2020","jul. 2020",
         "ene. 2021","jul. 2021","ene. 2022", "nov. 2022","dic. 2022",
         "ene. 2023","feb. 2023","mar. 2023", "abr. 2023")
high <- c("jun. 2018","dic. 2018","jun. 2019","dic. 2019","jun. 2020","dic. 2020",
          "jun. 2021","dic. 2021","oct. 2022","nov. 2022","dic. 2022",
          "ene. 2023","feb. 2023","mar. 2023", "abr. 2023")
####____LOOP PARA GENERAR RDS CONSOLIDADO PARA UN PAGO AL MES POR OPERACION____####
for (i in 1:length(bases)) {
  # Pagos <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/03_Mora_Hora/PagosCartera',bases[i],'.csv'),
  #                encoding = "UTF-8",fill = T,sep=',')
  print(paste(bases[i],low[i],high[i]))
  
  Pagos <- fread(paste0('D:/!bso/bases/csv/PagosCartera',bases[i],'.csv'),
                 encoding = "UTF-8",fill = T,sep=',')
  
  # print(table(as.yearmon(Pagos$FechaTrx)))
  # print(table(as.yearmon(Pagos$FechaPago)))
  
  P2full <- Pagos %>%
    summarypagos(fecha=FechaPago,flow=low[i],fhigh=high[i]) #Cambiar a FechaPago para obtener el corriente
  
  saveRDS(P2full,paste0("D:/!bso/mph/rds/PagosCartera_",bases[i],".rds"))
  #En /rds se encuentra con FechaPago, en /rds2 se encuentra con FechaTrx
}
####____FOR A SINGLE MONTH____####
base <- "Oct2023"
low <- c("Oct. 2023")
high <- c("Oct. 2023")

Pagos <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/03_Mora_Hora/PagosCartera',base,'.csv'),
               encoding = "UTF-8",fill = T,sep=',')
P2full <- Pagos %>%
  summarypagos(fecha=FechaTrx,flow = low,fhigh = high) #Cambiar a fecha=FechaPago para obtener el corriente

saveRDS(P2full,paste0("D:/!bso/mph/rds2/PagosCartera_",base,".rds")) #Cambiar a /rds si se usa FechaPago
####____PAGOS TARDIOS 2018-2022____####
bases <- c("Ene2018_Jun2018","Jul2018_Dic2018","Ene2019_Jun2019","Jul2019_Dic2019",
           "Ene2020_Jun2020","Jul2020_Dic2020","Ene2021_Jun2021","Jul2021_Dic2021",
           "Ene2022_Oct2022","Nov2022","Dic2022","Ene2023","Feb2023","Mar2023","Abr2023",
           "May2023","Jun2023","Jul2023","Ago2023")
PagosTList <- list()
i <- 6
for (i in 1:length(bases)){
  print(bases[i])
  P2full <- readRDS(paste0("D:/!bso/mph/rds2/PagosCartera_",bases[i],".rds"))
  P2uh <- P2full %>%
    getTardios()
  PTardios <- P2uh %>% 
    dplyr::filter(appsU==1)
  PagosTList[[i]] <- PTardios
}

PagosTFull <- rbindlist(PagosTList)

saveRDS(PagosTFull,"D:/!bso/firstTimes/PagosHist_Ene18Ago23.rds")
PagosTFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Jul23.rds")
check <- PagosTFull %>% #En este check aún se consideran los clientes que luego de pagar están en estado cancelado
  group_by(myPago) %>% 
  summarise(n=n(),Capital=sum(CapitalPagado),PagosAH=sum(appsH),PagosUH=sum(appsU),
            CapitalUH=sum(CapitalPagado[appsU==1]),maxDia=max(maxDia)) %>% 
  ungroup()
####____ADD A MONTH TO PAGOS FULL____####
PagosTFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Sep23.rds")
bases <- "Oct2023"
P2full <- readRDS(paste0("D:/!bso/mph/rds2/PagosCartera_",bases,".rds"))
P2uh <- P2full %>%
  getTardios()
PTardios <- P2uh %>% 
  dplyr::filter(appsU==1)
PagosTFull <- PagosTFull %>% 
  bind_rows(PTardios)
tail(PagosTFull %>% 
        group_by(myPago) %>% 
       summarise(CapitalPagado = sum(CapitalPagado), nOps=n_distinct(Operacion)))
saveRDS(PagosTFull,"D:/!bso/firstTimes/PagosHist_Ene18Oct23.rds")
################################################################################
####____REPRODUCING MONTHLY UPDATE____####
bases <- c("Ene2022_Oct2022","Nov2022","Dic2022","Ene2023","Feb2023","Mar2023","Abr2023","May2023","Jun2023","Jul2023")
P2uhList <- list()
for (i in 1:length(bases)) {
  P2uhFull <- readRDS(paste0("D:/!bso/mph/rds2/PagosCartera_",bases[i],".rds"))
  P2uhList[[i]] <- P2uhFull
}
P2full <- rbindlist(P2uhList)

lastmonth <- "Jul. 2023"
lastmonth12 <- "Ago. 2022"
shortmonth <- str_replace(lastmonth,". ","")
shortmonth12 <- str_replace(lastmonth12,". ","")
P2uh_v2 <- P2full %>% 
  dplyr::filter(myPago >= lastmonth12 & myPago <= lastmonth) %>% 
  getTardios()

saveRDS(P2uh, 'D:/!bso/mph/Pagos_conDummyTardio.rds')
P2uh <- read_rds('D:/!bso/mph/Pagos_conDummyTardio.rds')

check <- P2uh %>% #En este check aún se consideran los clientes que luego de pagar están en estado cancelado
  group_by(myPago) %>% 
  summarise(n=n(),Capital=sum(CapitalPagado),PagosAH=sum(appsH),PagosUH=sum(appsU)) %>% 
  ungroup()
####____PAGOS TARDÍOS EVOLUTIVO____####
P2Tardios <- P2uh %>% 
  dplyr::filter(appsU==1)
####____SALDO TARDIO EVOLUTIVO____####
codAge <- read_xlsx("D:/!bso/bases/excel/CodAgeSucReg.xlsx")
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
years <- c("2022","2023")
myrds <- as.vector(sapply(years, function(x){paste0(mes,x)}))
myrds <- myrds[c(which(myrds==shortmonth12):which(myrds==shortmonth))] #Actualizar para los últimos 12 meses

bdcList <- list()
for (i in 1:length(myrds)) {
  tryCatch({
    print(myrds[i])
    Cierre <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds[i],'.rds')) %>% 
      dplyr::filter(!MODULO %in% c(131,29)) %>% #Sin Modulo 29 desde Junio 2023
      dplyr::filter(ESTADO != 'CASTIGADA') %>%
      mutate(cosechaS=case_when(cosechaY<2020 ~ '< 2020',
                                cosechaY==2021 & cosechaM<='jun. 2021' ~ 'I/2021',
                                cosechaY==2021 & cosechaM>'jun. 2021' ~ 'II/2021',
                                TRUE~ as.character(cosechaY))) %>% 
      rename(Cuenta=CTACLIENTE,Operacion = OPERACION) %>% 
      mutate(Reprogramado = ifelse(saldoReprog>0,1,0)) %>% 
      mutate(Refinanciado = ifelse(saldoRef>0,1,0)) %>% 
      mutate(Diferido = ifelse(saldoDif>0,1,0)) %>% 
      mutate(Periodo_Gracia = ifelse(GRACIA_MESES>0,1,0)) %>% 
      mutate(myPago=as.yearmon(monDate)) %>% 
      group_by(myPago) %>% #Conservamos el saldo, par0 y par30 de todo el mes
      mutate(saldoTot=sum(saldous),
             par0Tot=sum(par0),
             par30Tot=sum(par30)) %>% 
      ungroup() %>% 
      select(myPago,Cuenta,Operacion,Sucursal,NOMBRE_AGENCIA,NOMBRE_ASESOR,tipoCred, FDESEMBOLSO,
             labGrupoC,labGrupoD,ESTADO,DIASMORA,rangom,previus,opTot,saldous,saldoMora,
             par0,par30,Reprogramado, Refinanciado, Diferido, Periodo_Gracia,ctaCont, 
             saldoTot,par0Tot,par30Tot, cosechaS) %>% 
      left_join(P2Tardios, by=c("Cuenta","Operacion","myPago")) %>%  #Unimos la base de cartera con la lista de pagos tardios
      replace_na(list(appsU=0)) %>% 
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

saveRDS(bdcEvol,paste0('D:/!bso/mph/bdcEvol',shortmonth12,shortmonth,'.rds'))
bdcEvol <- readRDS(paste0('D:/!bso/mph/bdcEvol',shortmonth12,shortmonth,'.rds'))
####____COUNTING PAGOS TARDÍOS POR OPERACION____####
P2uh_group <- P2uh %>%
  # dplyr::filter(myPago<=lastmonth) %>% #Si se quiere replicar para meses anteriores
  dplyr::filter(appsH >= 1 & appsU >= 1) %>% 
  group_by(Operacion) %>% 
  mutate(appsH = sum(appsH))%>%
  mutate(appsU = sum(appsU))%>%
  ungroup() %>% 
  group_by(Cuenta,Operacion) %>% 
  mutate(Ultimo_mes=ifelse(myPago==lastmonth,1,0)) %>% # Identificando pagos tardios del último mes
  arrange(Operacion, desc(myPago)) %>% 
  mutate(pos = row_number()) %>% 
  dplyr::filter(pos == 1) %>% 
  ungroup()

sum(P2uh_group$Ultimo_mes==1,na.rm = T)
####____CRUZANDO CONTEO DE PAGOS TARDÍOS CON ÚLTIMO CIERRE___####
lastCierre2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',shortmonth,'.rds')) %>% #Actualizar al último mes
  mutate(cosechaS=case_when(cosechaY<2020 ~ '< 2020',
                            cosechaY==2021 & cosechaM<='jun. 2021' ~ 'I/2021',
                            cosechaY==2021 & cosechaM>'jun. 2021' ~ 'II/2021',
                            TRUE~ as.character(cosechaY))) %>% 
  mutate(NOMBRE_CLIENTE=paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT,sep = " ")) %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>% #Eliminamos las operaciones castigadas
  rename(Cuenta=CTACLIENTE, Operacion=OPERACION) %>% 
  mutate(Reprogramado=ifelse(saldoReprog>0,1,0)) %>% 
  mutate(Refinanciado=ifelse(saldoRef>0,1,0)) %>% 
  mutate(Diferido=ifelse(saldoDif>0,1,0)) %>% 
  mutate(Periodo_Gracia=ifelse(GRACIA_MESES>0,1,0)) %>% 
  select(Cuenta,Operacion,CI,Sucursal,NOMBRE_CLIENTE,NOMBRE_AGENCIA,NOMBRE_ASESOR,FDESEMBOLSO,
         ESTADO,CALIFICACION,DIASMORA,ctaCont,tipoCred,labGrupoC,labGrupoD,Rango_Desembolso=rango,
         rangom,cosechaY, cosechaS,saldous,saldoMora,par0,par30,previus,opTot,saldoDifFranz,saldoDif,
         saldoRef,saldoReprog,par0Reprog,par30Reprog,par0Ref,par30Ref,par0Dif,par30Dif, 
         par0DifFranz,par30DifFranz,Reprogramado, Refinanciado, Diferido, Periodo_Gracia)

idHR <- P2uh_group %>% 
  dplyr::filter(appsU>=1) %>% 
  select(Cuenta,Operacion, appsH, appsU,mesPago,
         dayPago,maxDia,hourUltDia,Ultimo_mes) %>%
  left_join(lastCierre2, by = c('Cuenta','Operacion')) %>% 
  mutate(bin2 = case_when(dayPago==(maxDia-2)~'1. Pagó antepenúltimo día',
                          dayPago==(maxDia-1)~'2. Pagó penúltimo día',
                          dayPago>maxDia~'4. Pagó último día de 12 a 14',
                          dayPago==maxDia & hourUltDia<=12~'3. Pagó último día antes de 12',
                          dayPago==maxDia & hourUltDia>12 & hourUltDia<=14~'4. Pagó último día de 12 a 14',
                          dayPago==maxDia & hourUltDia>14~'5. Pagó último día de 14 en adelante',
                          TRUE~'0. Pagó antes del antepenúltimo día'
  )) %>% 
  select(Cuenta,Operacion,appsH,appsU,bin2,Ultimo_mes)

lastCierre <- lastCierre2 %>% 
  left_join(idHR,by=c('Cuenta','Operacion')) %>% 
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
# write_xlsx(lastCierre, paste0('D:/!bso/mph/BaseCartera',shortmonth,'_join.xlsx'))
write.csv(lastCierre,paste0('D:/!bso/mph/Oreports/lastCierrreUR_',shortmonth,'.txt'),
          row.names = F,fileEncoding = "utf-8", sep = "|",quote = FALSE)
write.table(lastCierre,file = paste0('D:/!bso/mph/Oreports/lastCierrreUR_',shortmonth,'.txt'),
            quote = FALSE,row.names = FALSE,sep = "|",fileEncoding = "utf-8")
fwrite(lastCierre, paste0('D:/!bso/mph/Oreports/lastCierrreUR_',shortmonth,'.txt'),
       row.names = F, sep='|', quote = FALSE)

lastCierre %>% 
  group_by(Sucursal) %>% 
  summarise(SaldoUSD=sum(Saldo_USD),SaldoRecurrente=sum(Saldo4M,na.rm = T),
            MoraRec = sum(Saldo4M,na.rm = T)/sum(Saldo_USD)*100) %>% 
  arrange(desc(MoraRec)) %>% 
  adorn_totals("row")
