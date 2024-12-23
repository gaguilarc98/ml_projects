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
####____FUNCIONES____####
process <- function(x, vstep){
  x %>% 
    select(Fecha, Cuenta, Operacion, CondCapInt_USD = `Total Cond cap + Int En $us`,
           CondInt_USD = `Cond Intereses En $us`,
           CondCap_USD = `Cond Capital En $us`,
           Cod_Asesor = Asesor,
           Nombre_Asesor = NombreAsesor,
           Nombre_Agencia= `NOMBRE DE AGENCIA`,
           AGENCIA = Sucursal_operacion,
           {{vstep}}) %>%
    left_join(codAge,by="AGENCIA") %>% 
    select(-Fecha, -Regional) %>% 
    rename(Cod_Agencia = AGENCIA) %>% 
    group_by({{vstep}}, Sucursal, Cod_Agencia, Nombre_Agencia, NOMBRE_AGENCIA, 
             Cod_Asesor, Nombre_Asesor, Cuenta, Operacion) %>% 
    summarise_all(sum) %>% 
    ungroup() %>% 
    mutate(Int_Condonado = ifelse(CondInt_USD>0,1,0)) %>% 
    mutate(Cap_Condonado = ifelse(CondCap_USD>0,1,0)) %>% 
    mutate(IntCap_Condonado = ifelse(CondCapInt_USD>0,1,0)) %>% 
    mutate(Key = paste(Cuenta,Operacion,sep="-")) 
}
getTardios <- function(x){
  x %>%
    mutate(hourUltDia = as.numeric(str_sub(Hora_UltDia, 1, 2))) %>%
    dplyr::filter(myPago < "mar. 2020" | myPago >= "dic. 2020") %>%
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
    ungroup() %>% 
    mutate(FechaPago = as.Date(paste(yearPago, mesPago, dayPago), format = '%Y%m%d')) %>% 
    mutate(appsH = ifelse(dayPago >= maxDia-2 & FechaPago > FechaPrevistaPago, 1, 0)) %>% 
    mutate(appsU = case_when(FechaPago > FechaPrevistaPago & (dayPago==maxDia & hourUltDia >= 12) | 
                               (dayPago>maxDia) ~ 1,
                             TRUE~0))
}
####____REGPROGRAMACIONES____####
codAge <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')
####____COND LAST X MONTHS____####
condFull <- readRDS('D:/!bso/condonaciones/condon/CondFull_Ene19Mar23.rds')
lastmonth <- "Mar. 2023"
lastmonth12 <- "Oct. 2022"
shortmonth <- str_replace(lastmonth,". ","")
shortmonth12 <- str_replace(lastmonth12,". ","")

gph <- condFull %>% 
  mutate(monDate = as.yearmon(Fecha)) %>% 
  process(vstep = monDate)
Cond_count <- gph %>% 
  dplyr::filter(monDate >= lastmonth12 & monDate<=lastmonth) %>% 
  mutate(Cond_ult_mes = ifelse(monDate==lastmonth,1,0)) %>% 
  group_by(Cuenta,Operacion) %>% 
  mutate(last3m = ifelse(monDate>as.yearmon(lastmonth)-3/12,1,0)) %>% 
  summarise(CapInt_6meses = sum(CondCapInt_USD),
            CapInt_3meses = sum(CondCapInt_USD*last3m),
            N_Cond_6meses = n(), N_Cond_3meses = sum(last3m)) %>% 
  ungroup() %>% 
  rename(CTACLIENTE=Cuenta, OPERACION=Operacion)

####____GETTING PAGOS TARDIOS____####
bases <- c("Ene2022_Oct2022","Nov2022","Dic2022","Ene2023","Feb2023","Mar2023")
P2uhList <- list()
for (i in 1:length(bases)) {
  P2uhFull <- readRDS(paste0("D:/!bso/mph/rds2/PagosCartera_",bases[i],".rds"))
  P2uhList[[i]] <- P2uhFull
}
P2full <- rbindlist(P2uhList)

P2uh <- P2full %>% 
  dplyr::filter(myPago >= lastmonth12 & myPago <= lastmonth) %>% 
  getTardios()

check <- P2uh %>% #En este check aún se consideran los clientes que luego de pagar están en estado cancelado
  group_by(myPago) %>% 
  summarise(n=n(),Capital=sum(CapitalPagado),PagosAH=sum(appsH),PagosUH=sum(appsU)) %>% 
  ungroup()

Tardios_count <- P2uh %>% 
  dplyr::filter(appsH >= 1 & appsU >= 1) %>% 
  group_by(Cuenta, Operacion) %>% 
  mutate(last3m = ifelse(myPago>as.yearmon(lastmonth)-3/12,1,0)) %>% 
  summarise(N_Tardios_6meses = sum(appsU), N_Tardios_3meses = sum(last3m)) %>% 
  ungroup() %>% 
  rename(CTACLIENTE = Cuenta, OPERACION = Operacion)

####____READING REPROGS
reprog <- readRDS("D:/!bso/reprog/reprogList_IM_mar2023_all.rds") %>% 
  glimpse()

bdc <- readRDS("D:/!bso/girCartera/rds/ec_Mar2023.rds") %>% 
  dplyr::filter(ESTADO!="CASTIGADA") %>% 
  dplyr::filter(ctaCont %in% c("131","133","134")) %>% 
  select(CTACLIENTE,OPERACION,MONTO,MONEDA,AGENCIA,NOMBRE_ASESOR,cosechaM,Saldo_USD=saldous,
         ctaCont,ESTADO) %>% 
  mutate(Monto_USD = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
  left_join(codAge,by="AGENCIA") %>% 
  mutate(Rat_Saldo_Monto = ifelse(Monto_USD!=0,Saldo_USD/Monto_USD,0)) %>% 
  select(-MONTO,-MONEDA,-Regional,-AGENCIA)

Reprog_Cond_Tardio <- reprog %>%
  ungroup() %>% 
  select(-counter_,-counterLast_,-DIASMORA) %>% 
  mutate(cvIM=ifelse(avgIM==0 & sdIM==0, 0,cvIM)) %>% 
  # dplyr::filter(!is.na(sdIM)) %>% 
  inner_join(bdc,by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(Cond_count,by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(Tardios_count,by=c("CTACLIENTE","OPERACION")) %>% 
  # replace_na(list(N_Tardios_6meses = 0,N_Tardios_3meses = 0,N_Cond_6meses = 0,N_Cond_3meses = 0,
  #                 CapInt_3meses = 0,CapInt_6meses = 0)) %>% 
  glimpse()
####____STANDARIZATION____####
Rep <- Reprog_Cond_Tardio %>% 
  relocate(maxMM,sdIM,avgIM,cvIM,.before = N_Cond_6meses) %>% 
  mutate(CapInt_6m_Rel = CapInt_6meses/Monto_USD) %>% 
  mutate(CapInt_3m_Rel = CapInt_3meses/Monto_USD) %>% 
  # mutate(across(maxMM:N_Tardios_3meses,~ .x/max(.x,na.rm = T))) %>%
  rename(Max_DiasIM = maxMM, Desv_Est_Max_DiasIM=sdIM, Prom_Max_DiasIM=avgIM,
         Coef_Var_Max_DiasIM=cvIM) %>% 
  select(-monDate,-counter,-counterLast3,-cosechaM,-NOMBRE_AGENCIA,-NOMBRE_ASESOR,
         -Sucursal,-Saldo_USD,-Monto_USD,-ESTADO,-ctaCont,-CapInt_6meses,-CapInt_3meses) %>% 
  ungroup() %>% 
  mutate(across(Max_DiasIM:CapInt_3m_Rel,~.x-min(.x,na.rm = T))) %>% 
  mutate(across(Max_DiasIM:CapInt_3m_Rel,~cuts(.x,levs=quantile(.x,seq(0.05,1,0.05),na.rm=T),
                                               values=seq(0.05,1,0.05),lowest = -0.1))) %>% 
  replace_na(list(N_Tardios_6meses = 0,N_Tardios_3meses = 0,N_Cond_6meses = 0,N_Cond_3meses = 0,
                  CapInt_3m_Rel=0,CapInt_6m_Rel=0))

RepExp <- Reprog_Cond_Tardio %>% 
  mutate(Fecha = as.Date(monDate,frac=1)) %>% 
  mutate(`Año_desembolso` = year(cosechaM)) %>%
  mutate(`Mes_desembolso` = month(cosechaM)) %>%
  select(Fecha, Sucursal, Agencia=NOMBRE_AGENCIA, Asesor=NOMBRE_ASESOR,
         `Año_desembolso`, `Mes_desembolso`,Cuenta=CTACLIENTE, Operacion=OPERACION, 
         ESTADO, CTACONTABLE=ctaCont,Saldo_USD, Monto_USD, 
         Max_DiasIM = maxMM, Desv_Est_Max_DiasIM=sdIM, Prom_Max_DiasIM=avgIM,
         Coef_Var_Max_DiasIM=cvIM, N_Cond_6meses, N_Cond_3meses, N_Tardios_6meses,
         N_Tardios_3meses, CapInt_3meses, CapInt_6meses, Rat_Saldo_Monto)

ListaReprogs <- list(Lista=RepExp,Medidas=Rep)
write.xlsx(ListaReprogs, "D:/!bso/reprog/Lista_Reprog_Mar23_v5.xlsx")

Reprog_Cond_Tardio <- Reprog_Cond_Tardio %>% 
  left_join(Rep,by=c("CTACLIENTE","OPERACION")) %>% 
  arrange(desc(Score)) 

####____ADDING SUCURSAL AGENCIA ASESOR MONTO Y SALDO____####
RepExp <- Reprog_Cond_Tardio %>% 
  mutate(Fecha = as.Date(monDate,frac=1)) %>% 
  inner_join(bdc,by=c("CTACLIENTE","OPERACION")) %>% 
  select(Fecha, Sucursal, Agencia=NOMBRE_AGENCIA, Asesor=NOMBRE_ASESOR,
         Cuenta=CTACLIENTE, Operacion=OPERACION, Saldo_USD, Monto_USD, Rat_Saldo_Monto,
         N_DiasIM_6meses=counter, N_DiasIM_3meses=counterLast3, Max_DiasIM = maxMM,
         Desv_Est_Max_DiasIM=sdIM, Prom_Max_DiasIM=avgIM, Coef_Var_Max_DiasIM=cvIM, 
         N_Tardios_6meses=N_TARDIOS, N_Tardios_3meses=N_TARDIOS_3m, N_Cond_6meses=NCONDONACIONES, 
         N_Cond_3meses=NCONDONACIONES_3m, CapIntCond_3meses=CapInt_3m, CapInt_6meses=CapInt_6m)

write_xlsx(RepExp, "D:/!bso/reprog/Lista_Reprog_Mar23_v2.xlsx")
