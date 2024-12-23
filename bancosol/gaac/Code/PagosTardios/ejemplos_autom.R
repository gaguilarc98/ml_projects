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
library(forcats) 
library(tseries)
library(openxlsx)
library(ggplot2)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)

####____OBTENIENDO LAS OPERACIONES EJEMPLO____####
join <- read_xlsx("D:/!bso/mph/BaseCarteraNov2022_join.xlsx",sheet = 'Sheet1')
jj <- join %>% 
  dplyr::filter(Instancias_UR>=4) %>% 
  select(Operacion)
####____PARA OBTENER UNA MUESTRA____####
# set.seed(14112022)
# Ops_ejemplo <- sample(jj$Operacion,size = 1)
####____PARA OBTENER TODAS LAS OPERACIONES____####
Ops_ejemplo <- jj$Operacion
####____OBTENIENDO SUS PAGOS____####
PagosNov <- fread('D:/!bso/bases/csv/Bases Transacciones 202211.csv',
                  encoding = "Latin-1",fill = T,sep=',') %>% 
  dplyr::filter(month(FechaPago)==11)  

PagosNov$codAsesorAsignacion <- as.character(PagosNov$codAsesorAsignacion)
PagosNov$codRegional <- as.character(PagosNov$codRegional)
PagosNov$codAgencia <- as.character(PagosNov$codAgencia)

Pagos <- fread('D:/!bso/bases/csv/PagosCarteraDesdeEne2022_Oct2022.csv',
               encoding = "Latin-1",fill = T,sep=',') %>% 
  dplyr::filter(month(FechaPago)!=11 &year(FechaPago)>=2022)

Pagos <- Pagos %>% bind_rows(PagosNov)

P2full <- Pagos %>% 
  mutate(mesPago = month(FechaPago)) %>% 
  mutate(dayPago = day(FechaPago)) %>% 
  mutate(yearPago = year(FechaPago)) %>% 
  dplyr::filter(yearPago == 2022) %>% 
  group_by(Operacion, mesPago, yearPago) %>% 
  mutate(Hora_UltDia = max(HoraPago[which(dayPago==max(dayPago))])) %>% 
  summarise(HoraPago = max(HoraPago), Hora_UltDia = max(Hora_UltDia),
            FechaPrevistaPago = max(FechaPrevistaPago),dayPago = max(dayPago),
            CapitalPagado=sum(CapitalPagado)) %>% 
  ungroup() %>% 
  mutate(FechaPago=as.Date(paste0(yearPago,"-",mesPago,"-",dayPago)))

write.csv(P2full,"D:/!bso/mph/ProcessPagosNov22.csv",row.names = F)
####____READING PAGOS____####
P2full <- fread("D:/!bso/mph/ProcessPagosNov22.csv",encoding = 'Latin-1', fill = T)

lastday <- P2full %>%
  mutate(mesano=as.yearmon(as.Date(FechaPago))) %>% 
  group_by(mesano) %>% 
  summarise(maxDia = max(FechaPago)) %>% 
  ungroup() %>% 
  select(mesano,maxDia) %>% 
  mutate(maxDia=ifelse(mesano=="feb. 2022",as.Date("2022-02-25"),maxDia)) %>% 
  mutate(maxDia=as.Date(maxDia))

P2full <- P2full %>% 
  mutate(mesano=as.yearmon(FechaPago)) %>% 
  left_join(lastday, by='mesano') %>% 
  select(Operacion,FechaPago,Hora_UltDia,FechaPrevistaPago,maxDia)

ListaPagos <- P2full %>% 
  dplyr::filter(Operacion %in% Ops_ejemplo) %>% 
  dplyr::filter(FechaPago==maxDia & FechaPago > FechaPrevistaPago &
                  as.numeric(substr(Hora_UltDia,1,2))>=12) %>% 
  arrange(Operacion,FechaPago)
fverifprev <- unique(c(as.Date(ListaPagos$FechaPago)-1))
fveriflast <- unique(c(as.Date(ListaPagos$FechaPago)))

write.csv(ListaPagos,"D:/!bso/mph/ListaPagosNov22.csv",row.names = F)
#_______________________________________________________________________________
####___MONITOREO DE DIAS MORA____####
ListaPagos <- fread("D:/!bso/mph/ListaPagosNov22.csv",encoding = 'Latin-1', fill = T)
fverifprev <- sort(unique(c(as.Date(ListaPagos$FechaPago)-1)))
fveriflast <- sort(unique(c(as.Date(ListaPagos$FechaPago))))

ListaPagos <- ListaPagos %>% 
  select(-maxDia) %>% 
  mutate(FechaPago=as.Date(FechaPago),FechaPrevistaPago=as.Date(FechaPrevistaPago))
ejemplos <- data.frame(Operacion=as.numeric(),
                       Fecha=as.Date(character()),
                       DIASMORA=as.numeric(),
                       FechaPago=as.Date(as.numeric()),
                       Hora_UltDia=character(),
                       FechaPrevistaPago=as.Date(as.numeric()),
                       Sucursal=character(),
                       NOMBRE_AGENCIA=character(),
                       NOMBRE_ASESOR=character(),
                       SALDO_USD=numeric())

agen <- read.csv("D:/!bso/bases/csv/CodAgeSucReg.csv",sep = ";")
for (i in 1:length(fverifprev)){
  print(fverifprev[i])
  fecha_verif <- str_replace(str_replace(as.character(fverifprev[i]),"-",""),"-","")
  tryCatch({
    ListaDia <- ListaPagos %>% 
      dplyr::filter(FechaPago==fverifprev[i]+1)
    baseDia <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/06_Base_Cartera_Diaria/BaseCartera_',fecha_verif,'.txt'),
                     encoding = 'UTF-8', fill = T) %>% 
      left_join(agen,by="AGENCIA") %>% 
      dplyr::filter(OPERACION %in% ListaDia$Operacion) %>%
      mutate(SALDO_USD = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
      mutate(ctaCont = substr(RUBRO,1,3)) %>% 
      mutate(Reprogramada = case_when(ctaCont %in% c("135","136","137")~1,
                                      TRUE~0)) %>% 
      select(OPERACION,DIASMORA,Sucursal,NOMBRE_AGENCIA,NOMBRE_ASESOR,SALDO_USD,ctaCont,Reprogramada) %>% 
      dplyr::rename(Operacion=OPERACION) %>% 
      left_join(ListaDia,by="Operacion") %>% 
      mutate(Fecha=fverifprev[i]) %>% 
      relocate(Operacion,Fecha,DIASMORA,FechaPago,Hora_UltDia,FechaPrevistaPago)
    ejemplos <- ejemplos %>% bind_rows(baseDia)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
for (i in 1:length(fveriflast)){
  print(fveriflast[i])
  fecha_verif <- str_replace(str_replace(as.character(fveriflast[i]),"-",""),"-","")
  tryCatch({
    ListaDia <- ListaPagos %>% 
      dplyr::filter(FechaPago==fveriflast[i])
    baseDia <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/06_Base_Cartera_Diaria/BaseCartera_',fecha_verif,'.txt'),
                     encoding = 'UTF-8', fill = T) %>% 
      left_join(agen,by="AGENCIA") %>% 
      dplyr::filter(OPERACION %in% ListaDia$Operacion) %>% 
      mutate(SALDO_USD = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
      mutate(ctaCont = substr(RUBRO,1,3)) %>% 
      mutate(Reprogramada = case_when(ctaCont %in% c("135","136","137")~1,
                                      TRUE~0)) %>% 
      select(OPERACION,DIASMORA,Sucursal,NOMBRE_AGENCIA,NOMBRE_ASESOR,SALDO_USD,ctaCont,Reprogramada) %>% 
      dplyr::rename(Operacion=OPERACION) %>% 
      left_join(ListaDia,by="Operacion") %>% 
      mutate(Fecha=fveriflast[i]) %>% 
      relocate(Operacion,Fecha,DIASMORA,FechaPago,Hora_UltDia,FechaPrevistaPago)
    ejemplos <- ejemplos %>% bind_rows(baseDia)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
ejemplos <- ejemplos %>%
  rename(CTACONT=ctaCont, FECHA_BASE=Fecha,
         FECHA_PAGO=FechaPago, HORA_ULT_PAGO=Hora_UltDia,
         FECHA_PREVISTA_PAGO=FechaPrevistaPago,
         SUCURSAL = Sucursal, AGENCIA = NOMBRE_AGENCIA,
         ASESOR=NOMBRE_ASESOR, OPERACION=Operacion,
         REPROGRAMADA=Reprogramada) %>% 
  mutate(MES_PAGO=month(FECHA_PAGO)) %>% 
  mutate(HORA_PAGO=as.numeric(substr(HORA_ULT_PAGO,1,2))) %>% 
  mutate(DESC=case_when(CTACONT=="131"~"Vigente",
                        CTACONT=="133"~"Vencida",
                        CTACONT=="134"~"Ejecución",
                        CTACONT=="135"~"Repro Vigente",
                        CTACONT=="136"~"Repro Vencida",
                        CTACONT=="137"~"Repro Ejecución")) %>% 
  group_by(OPERACION,MES_PAGO) %>%
  arrange(FECHA_PAGO) %>% 
  mutate(OP_PENULT_DIA=ifelse(row_number()==1,1,0)) %>% 
  mutate(OP_ULT_DIA=ifelse(row_number()!=1,1,0)) %>% 
  ungroup() %>% 
  #mutate(`NRO_OP_TARDÍAS`= n()/2) %>%
  group_by(OPERACION) %>% 
  mutate(`NRO_OP_TARDÍAS`=sum(OP_PENULT_DIA)) %>% 
  relocate(SUCURSAL, AGENCIA, ASESOR, OPERACION, FECHA_BASE, DIASMORA, FECHA_PAGO,
           HORA_ULT_PAGO, FECHA_PREVISTA_PAGO, `NRO_OP_TARDÍAS`, SALDO_USD, CTACONT,
           REPROGRAMADA, DESC, MES_PAGO, HORA_PAGO, OP_PENULT_DIA, OP_ULT_DIA) %>% 
  arrange(OPERACION, FECHA_BASE)

write.csv(ejemplos,"D:/!bso/mph/202211_OpRecurrentesVerif.csv",
          fileEncoding = "utf-8",row.names = F)
write_xlsx(ejemplos,"D:/!bso/mph/202211_OpRecurrentesVerif.xlsx")
ejemplos2 <- read_xlsx("D:/!bso/mph/202211_OpRecurrentesVerif.xlsx")
length(unique(ejemplos2$OPERACION))

table(ejemplos$SUCURSAL)
