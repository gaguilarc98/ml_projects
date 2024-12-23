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
library(scales)
library(janitor)
library(ggplot2)

Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
paleta <- colorRampPalette(c("blue2","slateblue4","slateblue3","violetred3","red3","tan2","yellow3","yellow2"),bias=1.5)

####____LECTURA DE BASE DE PAGOS____####
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraDesdeEne2022_Oct2022.csv',
                encoding = "Latin-1",fill = T,sep=',') %>% 
  dplyr::filter(year(FechaPago)>=2022)

P2full <- Pagos %>% 
  #dplyr::filter(FechaPago>=as.Date('2022-01-30') & FechaPago<=as.Date('2022-01-31')) %>%
  mutate(mesPago = month(FechaPago)) %>% 
  mutate(dayPago = day(FechaPago)) %>% 
  mutate(yearPago = year(FechaPago)) %>% 
  select(Operacion, CapitalPagado, FechaPago, HoraPago, mesPago, dayPago, 
         yearPago, FechaPrevistaPago) %>% 
  group_by(Operacion, mesPago, yearPago) %>% 
  mutate(Hora_UltDia = max(HoraPago[which(dayPago==max(dayPago))])) %>% 
  summarise(HoraPago = max(HoraPago), Hora_UltDia = max(Hora_UltDia),
            FechaPrevistaPago = max(FechaPrevistaPago),dayPago = max(dayPago)) %>% 
  ungroup() %>% 
  dplyr::filter(yearPago == 2022) %>% 
  group_by(Operacion) %>% 
  mutate(apps = max(row_number())) %>% 
  ungroup()

####____PAGOS TARDÍOS____####
P2uh <- P2full %>%
  mutate(hour = as.numeric(str_sub(HoraPago, 1, 2))) %>% 
  mutate(hourUltDia = as.numeric(str_sub(Hora_UltDia, 1, 2))) %>% 
  group_by(mesPago) %>% 
  mutate(maxDia = max(dayPago)) %>% 
  ungroup() %>% 
  mutate(FechaPago = as.Date(paste(yearPago, mesPago, dayPago), format = '%Y%m%d')) %>% 
  mutate(appsH = ifelse(dayPago >= maxDia-2 & FechaPago > FechaPrevistaPago, 1, 0)) %>% 
  mutate(appsU = ifelse(dayPago == maxDia & FechaPago > FechaPrevistaPago & 
                          hour >= 12, 1, 0)) %>% 
  group_by(Operacion) %>% 
  mutate(appsH = sum(appsH))%>%
  mutate(appsU = sum(appsU))%>%
  ungroup() %>% 
  dplyr::filter(appsH > 1 & appsU > 1) %>% 
  group_by(Operacion) %>% 
  arrange(Operacion, mesPago, dayPago) %>% 
  mutate(pos = row_number()) %>% 
  dplyr::filter(pos == 1) %>% 
  ungroup()

####____JOIN girCartera CIERRE MES CON OPERACIONES TARDÍAS____####
agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")

lastCierre2 <- readRDS('D:/!bso/bases/rds/ec_Oct2022.rds') %>% 
  left_join(agen,by="AGENCIA") %>% 
  dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>%
  rename(Operacion=OPERACION) %>% 
  mutate(Reprogramado=ifelse(saldoReprog>0,1,0)) %>% 
  mutate(Refinanciado=ifelse(saldoRef>0,1,0)) %>% 
  mutate(Diferido=ifelse(saldoDif>0,1,0)) %>% 
  mutate(Periodo_Gracia=ifelse(GRACIA_MESES>0,1,0)) %>% 
  mutate(Rango_Desembolso = ifelse(montous > 20000, '20K+', 'under20K')) %>% 
  select(Operacion,sucursal,NOMBRE_AGENCIA,NOMBRE_ASESOR,tipoCred,labGrupoC,
         labGrupoD,ESTADO,DIASMORA, Rango_Desembolso,rangom,previus,opTot,
         saldous,saldoMora,saldoDifFranz,saldoDif,saldoRef,saldoReprog,par0,par30,
         par0Reprog,par30Reprog,par0Ref,par30Ref,par0Dif,par30Dif,
         par0DifFranz,par30DifFranz,Reprogramado, Refinanciado, Diferido, Periodo_Gracia)

idHR <- P2uh %>% 
  select(Operacion, appsH, appsU,mesPago,
         dayPago,hourUltDia) %>%
  left_join(lastCierre2, by = 'Operacion') %>% 
  mutate(ESTADO = ifelse(is.na(saldous), 'CANCELADA', ESTADO))
table(idHR$ESTADO)

idHR <- idHR %>% 
  mutate(Op_tardia_2meses = 1) %>% 
  mutate(Op_tardia_4meses = ifelse(appsU>=4,1,0)) %>% 
  mutate(Op_tardia_2_3meses = ifelse(apssU<4,1,0)) %>% 
  mutate(bin2 = case_when(dayPago==29~'1. Pagó antepenúltimo día',
                          dayPago==30~'2. Pagó penúltimo día',
                          dayPago==31 & hourUltDia<=12~'3. Pagó último día antes de 12',
                          dayPago==31 & hourUltDia>12 & hourUltDia<=14~'4. Pagó último día de 12 a 14',
                          dayPago==31 & hourUltDia>14~'5. Pagó último día de 14 en adelante',
                          TRUE~'0. Pagó antes del antepenúltimo día'
  )) %>% 
  select(Operacion,appsH,appsU,dayPago,mesPago,hourUltDia,Op_tardia_2meses,
         Op_tardia_2_3meses,Op_tardia_4meses,bin2)


lastCierre <- lastCierre2 %>% 
  left_join(idHR,by='Operacion') %>% 
  mutate(Op_tardia_2meses = ifelse(is.na(Op_tardia_2meses),0,Op_tardia_2meses)) %>%
  mutate(Saldo_tardio_2meses = ifelse(Op_tardia_2meses==1,saldous,0)) %>% 
  mutate(Saldo_tardio_2_3meses = ifelse(Op_tardia_2_3_meses==1,saldous,0)) %>% 
  mutate(Saldo_tardio_4meses = ifelse(Op_tardia_4meses==1,saldous,0)) %>% 
  mutate(saldoDif = ifelse(saldoDif==0,NA,saldoDif)) %>% 
  mutate(saldoRef = ifelse(saldoRef==0,NA,saldoRef)) %>% 
  mutate(saldoDifFranz = ifelse(saldoDifFranz==0,NA,saldoDifFranz)) %>% 
  mutate(saldoReprog = ifelse(saldoReprog==0,NA,saldoReprog)) %>% 
  mutate(saldoMora = ifelse(saldoMora==0,NA,saldoMora)) %>% 
  mutate(saldoDif = ifelse(saldoDif==0,NA,saldoDif)) %>% 
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
                Pago_Ultimos_Dias = bin2,
                Rango_Monto = rangom,
                Op_Totales = opTot,
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

write_xlsx(lastCierre, 'D:/!bso/mph/BaseCarteraOct2022_join.xlsx')
write.csv(lastCierre,"D:/!bso/mph/Oreports/lastCierrreUR_Oct2022.csv",sep = ",",row.names = F)
