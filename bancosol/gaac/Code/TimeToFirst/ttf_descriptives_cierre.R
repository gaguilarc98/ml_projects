####____CARGA DE PAQUETES, FUNCIONES Y OTROS____####
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
library(ggrepel)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____PAGOS TARDÍOS____####
ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Oct23.rds")
checkpt <- ptFull %>% 
  group_by(myPago) %>% 
  summarise(NObs= n(),NOps=n_distinct(Operacion), check=NObs==NOps)

pt_grouped <- ptFull %>% 
  dplyr::filter(myPago>"Dic. 2018" & myPago <="Oct. 2023") %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(FechaFirstTardio = min(FechaPago), 
            CantPT = n()) %>% 
  ungroup()

saveRDS(pt_grouped, "D:/!bso/firstTimes/pt_grouped.rds")
####____CONDONACIONES____####
condFull <- read_parquet('D:/!bso/condonaciones/CondFull.parquet')
zeros <- condFull %>%  
  dplyr::filter(Total_Cond_Cap_Int==0) %>% 
  group_by(as.yearmon(Fecha)) %>% summarise(nOps=n())

cond_clean <- condFull %>% 
  # dplyr::filter(Fecha>as.Date()) %>% 
  select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int, CondCap_USD = Cond_Cap) %>%
  mutate(myCond = as.yearmon(Fecha)) %>% 
  group_by(myCond, Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(Fecha),
            across(starts_with("Cond"),~sum(.x))) %>% 
  ungroup()

checkcond <- cond_clean %>% group_by(myCond) %>% 
  summarise(NObs=n(), NOps=n_distinct(Operacion), check=NObs==NOps)  

cond_grouped <- cond_clean %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(FechaFirstCond),
            CantCond = n()) %>% 
  ungroup()
saveRDS(cond_grouped, "D:/!bso/firstTimes/cond_grouped.rds")
####____MORA AL CIERRE____####
dfOps <- readRDS('D:/!bso/features/Historial_Operaciones.rds') %>% 
  mutate(tuvoMora = ifelse(DIASMORA>0, 1, 0)) %>% 
  glimpse()

measuresMora <- dfOps %>% 
  select(CTACLIENTE, OPERACION, monDate, fdes, DIASMORA, MONTOUS, tuvoMora) %>% 
  group_by(CTACLIENTE, OPERACION) %>% #Se agrupa por operación de cada cliente
  summarise(meanMora = mean(DIASMORA, na.rm = T),
            IntMoraBSO = sum(tuvoMora)/n(),
            fdes_ori = min(fdes),#Fecha más antigua para reprogramadas
            lastM = as.Date(max(monDate), frac = 1)) %>% #último mes en las bdc
  ungroup() %>% 
  mutate(loanDays = as.integer(lastM - fdes_ori)) #días de préstamo

FirstMora <- dfOps %>% 
  dplyr::filter(fdes>=as.Date("2016-01-01")) %>% 
  select(monDate, CTACLIENTE, OPERACION, DIASMORA, tuvoMora) %>% 
  dplyr::filter(tuvoMora>0) %>% 
  group_by(CTACLIENTE, OPERACION) %>% #Se agrupa por operación de cada cliente
  summarise(myFirstPar0 = min(monDate)) 

remove(list=c("dfOps"))
saveRDS(FirstMora, "D:/!bso/firstTimes/moraCierre.rds")
####____MORA_IM_2018____####
moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Oct2023.rds")) %>% 
  select(monDate, CTACLIENTE, OPERACION, maximaDPD_2, starts_with("tuvo")) %>% 
  dplyr::filter(!is.na(maximaDPD_2)) 

measuresMoraIM <- moraIntraMes %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  summarise(meanMoraIM = mean(maximaDPD_2),
            mesesDesde18 = n(),
            IntDPD1 = sum(tuvoDPD1)/mesesDesde18,
            IntDPD5 = sum(tuvoDPD5)/mesesDesde18,
            IntDPD10 = sum(tuvoDPD10)/mesesDesde18,
            IntDPD15 = sum(tuvoDPD15)/mesesDesde18,
            IntDPD20 = sum(tuvoDPD20)/mesesDesde18,
            IntDPD25 = sum(tuvoDPD25)/mesesDesde18) %>% 
  ungroup()

dpd1 <- moraIntraMes %>% 
  dplyr::filter(tuvoDPD1>0) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  summarise(myFirst_DPD1 = min(monDate)) %>% 
  ungroup()
dpd5 <- moraIntraMes %>% 
  dplyr::filter(tuvoDPD5>0) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  summarise(myFirst_DPD5 = min(monDate)) %>% 
  ungroup()
dpd10 <- moraIntraMes %>% 
  dplyr::filter(tuvoDPD10>0) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  summarise(myFirst_DPD10 = min(monDate)) %>% 
  ungroup()
dpd15 <- moraIntraMes %>% 
  dplyr::filter(tuvoDPD15>0) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  summarise(myFirst_DPD15 = min(monDate)) %>% 
  ungroup()
dpd20 <- moraIntraMes %>% 
  dplyr::filter(tuvoDPD20>0) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  summarise(myFirst_DPD20 = min(monDate)) %>% 
  ungroup()
dpd25 <- moraIntraMes %>% 
  dplyr::filter(tuvoDPD25>0) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  summarise(myFirst_DPD25 = min(monDate)) %>% 
  ungroup()

saveRDS(measuresMoraIM,"d:/!bso/firstTimes/measures.rds")
saveRDS(dpd1,"d:/!bso/firstTimes/dpd1.rds")
saveRDS(dpd5,"d:/!bso/firstTimes/dpd5.rds")
saveRDS(dpd10,"d:/!bso/firstTimes/dpd10.rds")
saveRDS(dpd15,"d:/!bso/firstTimes/dpd15.rds")
saveRDS(dpd20,"d:/!bso/firstTimes/dpd20.rds")
saveRDS(dpd25,"d:/!bso/firstTimes/dpd25.rds")

####____JOINING EVERYTHING_____####
codMod <- read_xlsx("D:/!bso/bases/excel/CodModulo.xlsx")
pt_grouped <- readRDS("D:/!bso/firstTimes/pt_grouped.rds")
cond_grouped <- readRDS("D:/!bso/firstTimes/cond_grouped.rds")
measuresMoraIM <- readRDS("D:/!bso/firstTimes/measures.rds")
dpd1 <- readRDS("D:/!bso/firstTimes/dpd1.rds") %>% 
  dplyr::filter(myFirst_DPD1 <= "Dic. 2022")
dpd5 <- readRDS("D:/!bso/firstTimes/dpd5.rds") %>% 
  dplyr::filter(myFirst_DPD5 <= "Dic. 2022")
dpd10 <- readRDS("D:/!bso/firstTimes/dpd10.rds") %>% 
  dplyr::filter(myFirst_DPD10 <= "Dic. 2022")
dpd15 <- readRDS("D:/!bso/firstTimes/dpd15.rds") %>% 
  dplyr::filter(myFirst_DPD15 <= "Dic. 2022")
dpd20 <- readRDS("D:/!bso/firstTimes/dpd20.rds") %>% 
  dplyr::filter(myFirst_DPD20 <= "Dic. 2022")
dpd25 <- readRDS("D:/!bso/firstTimes/dpd25.rds") %>% 
  dplyr::filter(myFirst_DPD25 <= "Dic. 2022")
FirstMora <- readRDS("D:/!bso/firsttimes/moraCierre.rds") %>% 
  dplyr::filter(myFirstPar0 <= "Dic. 2022")

clientes <- readRDS('D:/!bso/features/ClientesTTF_Ene2015Oct2023.rds') %>% 
  select(CTACLIENTE, OPERACION, fdes_original = fdes)

#CHECK DE INTEGRIDAD DE OPERACIONES EN TTF_DISBURSEMENT
nrow(bdc %>% dplyr::filter(esFSL==0 & fdes_original>=as.Date("2019-01-01") & ctaCont!="623"))
##

bdc <- readRDS("D:/!bso/girCartera/rds/ec_Dic2022.rds") %>% #Sep2023, #Dic2022
  left_join(clientes, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(codMod, by="MODULO") %>%
  dplyr::filter(! ctaCont %in% c('623')) %>% 
  mutate(SaldoBruto = ifelse(ctaCont %in% c('131','133','134','135','136','137'), saldous, 0)) %>% 
  mutate(SaldoTotal = ifelse(ctaCont %in% c('131','133','134','135','136','137','865'), saldous+saldoCast, 0)) %>% 
  mutate(SaldoMora = ifelse(ctaCont %in% c('133','134','136','137'), saldous, 0)) %>% 
  mutate(SaldoMoraCast = ifelse(ctaCont %in% c('133','134','136','137','865'), saldous+saldoCast, 0)) %>% 
  mutate(SaldoPar0Cast = par0+saldoCast) %>% 
  mutate(OpsBruta = ifelse(ctaCont %in% c('131','133','134','135','136','137'), 1, 0)) %>% 
  mutate(OpsTotal = ifelse(ctaCont %in% c('131','133','134','135','136','137','865'), 1, 0)) %>% 
  mutate(OpsMora = ifelse(ctaCont %in% c('133','134','136','137'), 1, 0)) %>% 
  mutate(OpsPar0 = ifelse(par0>0, 1, 0)) %>% 
  mutate(OpsMoraCast = ifelse(ctaCont %in% c('133','134','136','137','865'), 1, 0)) %>% 
  mutate(OpsPar0Cast = ifelse(par0+saldoCast>0, 1, 0)) %>% 
  mutate(OpsCastigada = ifelse(saldoCast>0, 1, 0)) %>%
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER, "MIGR"), 1, 0)) %>%
  mutate(Antes2019 = ifelse(fdes_original<as.Date("2019-01-01"), 1, 0)) %>% 
  # mutate(Antes2018 = ifelse(fdes_original<as.Date("2018-01-01"), 1, 0)) %>% 
  mutate(Antes2017 = ifelse(fdes_original<as.Date("2017-01-01"), 1, 0)) %>% 
  mutate(Antes2016 = ifelse(fdes_original<as.Date("2016-01-01"), 1, 0)) %>% 
  mutate(Objeto = case_when(str_detect(DESC_OBJCRED,"INVERSION")~"INVERSION",
                            str_detect(DESC_OBJCRED,"OPERACION")~"OPERACION",
                            TRUE~"OTROS")) %>% 
  mutate(Tipo_Credito = case_when(substr(TIPO_CREDITO,1,1) =='P' ~ 'Pyme',
                                  substr(TIPO_CREDITO,1,1) =='M' ~ 'Micro',
                                  substr(TIPO_CREDITO,1,1) =='N' ~ 'Consumo',
                                  TIPO_CREDITO %in% c('H0','H1','H2') ~ 'Vivienda Normal',
                                  TIPO_CREDITO %in% c('H3','H4') ~ 'Vivienda Social',)) %>% 
  mutate(CreditoNuevo = ifelse(year(fdes_original) < 2018, '< 2018',
                               as.character(year(fdes_original)))) %>% 
  mutate(Tipo_Cartera = case_when(ctaCont %in% c('131','133','134') & OPERACION_ORI_REF==0 ~ 'Normal',
                                  ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                  OPERACION_ORI_REF!=0 ~ 'Refinanciada',)) %>%
  # mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
  mutate(PlazoAnios = ifelse(floor(PLAZODIAS/365)<=7,as.character(floor(PLAZODIAS/365)), "> 7")) %>% 
  select(monDate, CTACLIENTE, OPERACION, CI, GENERO, Sucursal, AGENCIA, NOMBRE_AGENCIA, 
         ASESOR, NOMBRE_ASESOR, NOMBRE_MODULO, fdes_original, rangos, rangom, Tipo_Credito, 
         Objeto, Sector_Destino, Sector_Actividad, ctaCont, CreditoNuevo, PlazoAnios, Tipo_Cartera, 
         esFSL, Antes2019, Antes2017, Antes2016, SaldoBruto, SaldoMora, Par0=par0, SaldoMoraCast,
         SaldoPar0Cast,  SaldoTotal, OpsBruta, OpsMora, OpsPar0, OpsMoraCast, OpsPar0Cast,
         OpsTotal)

bdc_wMeasures <- bdc %>% 
  left_join(pt_grouped, by=c("CTACLIENTE"="Cuenta", "OPERACION"="Operacion")) %>% 
  left_join(cond_grouped, by=c("CTACLIENTE"="Cuenta", "OPERACION"="Operacion")) %>% 
  left_join(measuresMoraIM, by=c("CTACLIENTE", "OPERACION")) %>% 
  left_join(dpd1,  by = c("CTACLIENTE", "OPERACION")) %>% 
  left_join(dpd5,  by = c("CTACLIENTE", "OPERACION")) %>% 
  left_join(dpd10,  by = c("CTACLIENTE", "OPERACION")) %>% 
  left_join(dpd15,  by = c("CTACLIENTE", "OPERACION")) %>% 
  left_join(dpd20,  by = c("CTACLIENTE", "OPERACION")) %>% 
  left_join(dpd25,  by = c("CTACLIENTE", "OPERACION")) %>% 
  left_join(FirstMora, by = c("CTACLIENTE", "OPERACION")) %>% 
  replace_na(list(MesesPagoTardio=0, CantPT=0, 
                  MesesCondonado=0, CantCond=0)) %>% 
  ungroup() %>% 
  # mutate(FechaFirstTardio = if_else(fdes_ori<as.Date("2018-06-01"), NA, FechaFirstTardio)) %>% 
  # mutate(FechaFirstCond = if_else(fdes_ori<as.Date("2019-01-01"), NA, FechaFirstCond)) %>% 
  # mutate(FechaFirstMora = if_else(fdes_ori<as.Date("2015-01-01"), NA, FechaFirstMora)) %>% 
  # mutate(FechaFirstMoraIM = if_else(is.na(FechaFirstMoraIM), MesFirstMoraIM, FechaFirstMoraIM)) %>% 
  # mutate(FechaFirstMoraIM = if_else(fdes_ori<as.Date("2018-01-01"), NA, FechaFirstMoraIM)) %>% 
  glimpse()

sapply(bdc_wMeasures, function(x){sum(is.na(x))})
saveRDS(bdc_wMeasures, "D:/!bso/firstTimes/bdc_measures_Ago2023.rds")  
bdc_wMeasures <- readRDS("D:/!bso/firstTimes/bdc_measures_Ago2023.rds")
####____MONTO Y OPS COMPROMETIDAS EN EVENTOS A TIEMPO VARIABLE____####
bdc_ttf <- bdc_wMeasures %>% 
  mutate(Ops=1) %>% 
  mutate(cosechaM = as.yearmon(fdes_original)) %>% 
  mutate(cosechaY = year(fdes_original)) %>% 
  mutate(myFirstTardio = as.yearmon(FechaFirstTardio),
         myFirstCond = as.yearmon(FechaFirstCond)) %>%
  mutate(monthsSinceDisb = round((monDate-cosechaM)*12+1)) %>% 
  mutate(across(starts_with("Fecha"), ~as.Date(.x))) %>% 
  mutate(meanMoraIM_bin = case_when(meanMoraIM==0 ~ '1. 0',
                                    meanMoraIM<5 ~ '2. <5',
                                    meanMoraIM<10 ~ '3. [5-10)',
                                    meanMoraIM<15 ~ '4. [10-15)',
                                    meanMoraIM<20 ~ '5. [15-20)',
                                    meanMoraIM>=20 ~ '6. >=20')) %>% 
  mutate(IntPT = CantPT/monthsSinceDisb) %>% 
  mutate(IntCond = CantCond/monthsSinceDisb) %>% 
  mutate(Months_to_first_par0 = case_when(is.na(myFirstPar0) ~ 999,
                                    cosechaM < "Ene. 2016"~ round((myFirstPar0-as.yearmon("Ene. 2016"))*12),
                                    TRUE ~ round((myFirstPar0-cosechaM)*12))) %>% 
  mutate(Months_to_first_cond = case_when(is.na(myFirstCond) ~ 999,
                                          cosechaM < "Ene. 2019"~ round((myFirstCond-as.yearmon("Ene. 2019"))*12),
                                          TRUE ~ round((myFirstCond-cosechaM)*12))) %>% 
  mutate(Months_to_first_tardio = case_when(is.na(myFirstTardio) ~ 999,
                                            cosechaM < "Ene. 2019"~ round((myFirstTardio-as.yearmon("Ene. 2019"))*12),
                                            TRUE ~ round((myFirstTardio-cosechaM)*12))) %>% 
  # mutate(Months_to_DPD1 = case_when(is.na(myFirst_DPD1) ~ 999,
  #                                   cosechaM < "Ene. 2018"~ round((myFirst_DPD1-as.yearmon("Ene. 2018"))*12),
  #                                   TRUE ~ round((myFirst_DPD1-cosechaM)*12))) %>% 
  mutate(Months_to_DPD5 = case_when(is.na(myFirst_DPD5) ~ 999,
                                    cosechaM < "Ene. 2016"~ round((myFirst_DPD5-as.yearmon("Ene. 2016"))*12),
                                    TRUE ~ round((myFirst_DPD5-cosechaM)*12))) %>% 
  mutate(Months_to_DPD10 = case_when(is.na(myFirst_DPD10) ~ 999,
                                     cosechaM < "Ene. 2016"~ round((myFirst_DPD10-as.yearmon("Ene. 2016"))*12),
                                     TRUE ~ round((myFirst_DPD10-cosechaM)*12))) %>% 
  # mutate(Months_to_DPD15 = case_when(is.na(myFirst_DPD15) ~ 999,
  #                                    cosechaM < "Ene. 2018"~ round((myFirst_DPD15-as.yearmon("Ene. 2018"))*12),
  #                                    TRUE ~ round((myFirst_DPD15-cosechaM)*12))) %>% 
  # mutate(Months_to_DPD20 = case_when(is.na(myFirst_DPD20) ~ 999,
  #                                    cosechaM < "Ene. 2018"~ round((myFirst_DPD20-as.yearmon("Ene. 2018"))*12),
  #                                    TRUE ~ round((myFirst_DPD20-cosechaM)*12))) %>%
  mutate(Months_DPD5_to_Par0 = case_when(is.na(myFirstPar0) & OpsPar0==1 ~ -1,
                                         is.na(myFirstPar0) & OpsPar0==0 ~ 999,
                                         !is.na(myFirstPar0) & myFirstPar0<myFirst_DPD5 ~ 0,
                                         TRUE ~ round((myFirstPar0 - myFirst_DPD5)*12))) %>% 
  # rowwise() %>% 
  # mutate(Months_to_Event5 = min(Months_to_first_cond, Months_to_first_tardio, Months_to_DPD5)) %>% 
  # mutate(Months_to_Event10 = min(Months_to_first_cond, Months_to_first_tardio, Months_to_DPD10)) %>% 
  # ungroup() %>% 
  # mutate(Saldo_PT_3 = ifelse(Months_to_first_tardio>=0 & Months_to_first_tardio<=3, SaldoBruto, 0)) %>% 
  mutate(Saldo_PT_6 = ifelse(Months_to_first_tardio>=0 & Months_to_first_tardio<=6, SaldoBruto, 0)) %>% 
  mutate(Saldo_PT_9 = ifelse(Months_to_first_tardio>=0 & Months_to_first_tardio<=9, SaldoBruto, 0)) %>% 
  # mutate(Saldo_Cond_3 = ifelse(Months_to_first_cond>=0 & Months_to_first_cond<=3, SaldoBruto, 0)) %>% 
  mutate(Saldo_Cond_6 = ifelse(Months_to_first_cond>=0 & Months_to_first_cond<=6, SaldoBruto, 0)) %>% 
  mutate(Saldo_Cond_9 = ifelse(Months_to_first_cond>=0 & Months_to_first_cond<=9, SaldoBruto, 0)) %>% 
  # mutate(Saldo_DPD5_3 = ifelse(Months_to_DPD5>=0 & Months_to_DPD5<=3, SaldoBruto, 0)) %>% 
  mutate(Saldo_DPD5_6 = ifelse(Months_to_DPD5>=0 & Months_to_DPD5<=6, SaldoBruto, 0)) %>% 
  mutate(Saldo_DPD5_9 = ifelse(Months_to_DPD5>=0 & Months_to_DPD5<=9, SaldoBruto, 0)) %>% 
  # mutate(Saldo_DPD10_3 = ifelse(Months_to_DPD10>=0 & Months_to_DPD10<=3, SaldoBruto, 0)) %>% 
  mutate(Saldo_DPD10_6 = ifelse(Months_to_DPD10>=0 & Months_to_DPD10<=6, SaldoBruto, 0)) %>% 
  mutate(Saldo_DPD10_9 = ifelse(Months_to_DPD10>=0 & Months_to_DPD10<=9, SaldoBruto, 0)) %>% 
  # mutate(Saldo_DPD15_3 = ifelse(Months_to_DPD15>=0 & Months_to_DPD15<=3, SaldoBruto, 0)) %>% 
  # mutate(Saldo_DPD15_6 = ifelse(Months_to_DPD15>=0 & Months_to_DPD15<=6, SaldoBruto, 0)) %>% 
  # mutate(Saldo_DPD15_9 = ifelse(Months_to_DPD15>=0 & Months_to_DPD15<=9, SaldoBruto, 0)) %>% 
  # mutate(Saldo_Event5_3 = ifelse(Months_to_Event5>=0 & Months_to_Event5<=3, SaldoBruto, 0)) %>% 
  # mutate(Saldo_Event5_6 = ifelse(Months_to_Event5>=0 & Months_to_Event5<=6, SaldoBruto, 0)) %>% 
  # mutate(Saldo_Event5_9 = ifelse(Months_to_Event5>=0 & Months_to_Event5<=9, SaldoBruto, 0)) %>% 
  # mutate(Saldo_Event10_3 = ifelse(Months_to_Event10>=0 & Months_to_Event10<=3, SaldoBruto, 0)) %>% 
  # mutate(Saldo_Event10_6 = ifelse(Months_to_Event10>=0 & Months_to_Event10<=6, SaldoBruto, 0)) %>% 
  # mutate(Saldo_Event10_9 = ifelse(Months_to_Event10>=0 & Months_to_Event10<=9, SaldoBruto, 0)) %>% 
  mutate(Ops_PT_6 = ifelse(Months_to_first_tardio>=0 & Months_to_first_tardio<=6, 1, 0)) %>% 
  mutate(Ops_PT_9 = ifelse(Months_to_first_tardio>=0 & Months_to_first_tardio<=9, 1, 0)) %>% 
  mutate(Ops_Cond_6 = ifelse(Months_to_first_cond>=0 & Months_to_first_cond<=6, 1, 0)) %>% 
  mutate(Ops_Cond_9 = ifelse(Months_to_first_cond>=0 & Months_to_first_cond<=9, 1, 0)) %>% 
  mutate(Ops_DPD5_6 = ifelse(Months_to_DPD5>=0 & Months_to_DPD5<=6, 1, 0)) %>% 
  mutate(Ops_DPD5_9 = ifelse(Months_to_DPD5>=0 & Months_to_DPD5<=9, 1, 0)) %>% 
  mutate(Ops_DPD10_6 = ifelse(Months_to_DPD10>=0 & Months_to_DPD10<=6, 1, 0)) %>% 
  mutate(Ops_DPD10_9 = ifelse(Months_to_DPD10>=0 & Months_to_DPD10<=9, 1, 0)) %>%
  # mutate(Ops_Event5_6 = ifelse(Months_to_Event5>=0 & Months_to_Event5<=6, 1, 0)) %>% 
  # mutate(Ops_Event5_9 = ifelse(Months_to_Event5>=0 & Months_to_Event5<=9, 1, 0)) %>% 
  # mutate(Ops_Event10_6 = ifelse(Months_to_Event10>=0 & Months_to_Event10<=6, 1, 0)) %>% 
  # mutate(Ops_Event10_9 = ifelse(Months_to_Event10>=0 & Months_to_Event10<=9, 1, 0)) %>% 
  mutate(across(c(monDate,cosechaM),~as.Date(.x,frac=1))) %>% 
  select(-starts_with("myFirst"),
          -starts_with("Int"), -starts_with("meanMora")) %>% 
  # mutate(Months_Event5_bin = case_when(Months_to_Event5==999~'0. Sin evento',
  #                                      Months_to_Event5<=9~'1. <= 9 meses',
  #                                      TRUE ~ '2. > 9 meses')) %>% 
  mutate(Months_DPD5_bin = case_when(Months_to_DPD5==999~'0. Sin evento',
                                       Months_to_DPD5<=9~'1. <= 9 meses',
                                       TRUE ~ '2. > 9 meses'))  %>% 
  mutate(Months_DPD10_bin = case_when(Months_to_DPD10==999~'0. Sin evento',
                                      Months_to_DPD10<=9~'1. <= 9 meses',
                                      TRUE ~ '2. > 9 meses'))
  
write_xlsx(bdc_ttf, "D:/!bso/firstTimes/bdc_ttf_Dic2022.xlsx")

####____DICIEMBRE VS SEPTIEMBRE____####
bdc_ttf_Sep <- read_xlsx("D:/!bso/firstTimes/bdc_ttf_Sep2023_v3.xlsx", sheet = "Detalle")

bdc_ttf_Sep_grouped <- bdc_ttf_Sep %>% 
  mutate(Sector_Actividad = case_when(Sector_Actividad %in% c("Asalariados", "K. Inter. Fin.", "L. Serv. Inmob.","O. Serv. Hosp + Otros","Q. Org. Extraterritoriales","Otros") ~ "Servicios",
                                      Sector_Actividad %in% c("C. Ext. Gas y Pet.", "D. Ext. Minerales", "F. Dist. EE y agua") ~ "Industria",
                                      Sector_Actividad == "H. Comercio" ~ "Comercio",
                                      Sector_Actividad == "I. Hoteles" ~ "Hoteles",
                                      Sector_Actividad == "E. Ind. y Manu" ~ "Manufactura",
                                      Sector_Actividad == "J. Transporte" ~ "Transporte",
                                      Sector_Actividad == "G. Construcción" ~ "Construcción",
                                      Sector_Actividad == "Agropecuario" ~ "Agropecuario",
                                      TRUE~"Servicios")) %>% 
  mutate(Sector_Destino = case_when(Sector_Destino %in% c("Asalariados", "K. Inter. Fin.", "L. Serv. Inmob.","O. Serv. Hosp + Otros","Q. Org. Extraterritoriales","Otros") ~ "Servicios",
                                    Sector_Destino %in% c("C. Ext. Gas y Pet.", "D. Ext. Minerales", "F. Dist. EE y agua") ~ "Industria",
                                    Sector_Destino == "H. Comercio" ~ "Comercio",
                                    Sector_Destino == "I. Hoteles" ~ "Hoteles",
                                    Sector_Destino == "E. Ind. y Manu" ~ "Manufactura",
                                    Sector_Destino == "J. Transporte" ~ "Transporte",
                                    Sector_Destino == "G. Construcción" ~ "Construcción",
                                    Sector_Destino == "Agropecuario" ~ "Agropecuario",
                                    TRUE~"Servicios")) %>% 
  mutate(PlazoAnios = case_when(PlazoAnios %in% c("0","1","2") ~ "2 años o menos",
                                PlazoAnios %in% c(">7") ~ "Más de 7 años",
                                TRUE~PlazoAnios)) %>% 
  mutate(Months_to_first_DPD5_bin = case_when(Months_to_DPD5==999~'8. Sin evento',
                                              Months_to_DPD5<=1~'1. [0, 1] meses',
                                              Months_to_DPD5<=3~'2. (1, 3] meses',
                                              Months_to_DPD5<=6~'3. (3, 6] meses',
                                              Months_to_DPD5<=9~'4. (6, 9] meses',
                                              Months_to_DPD5<=12~'5. (9, 12] meses',
                                              Months_to_DPD5<=24~'6. (12, 24] meses',
                                              Months_to_DPD5>24~'7. > 24 meses',)) %>% 
  mutate(Months_to_first_DPD10_bin = case_when(Months_to_DPD10==999~'8. Sin evento',
                                               Months_to_DPD10<=1~'1. [0, 1] meses',
                                               Months_to_DPD10<=3~'2. [2, 3] meses',
                                               Months_to_DPD10<=6~'3. (3, 6] meses',
                                               Months_to_DPD10<=9~'4. (6, 9] meses',
                                               Months_to_DPD10<=12~'5. (9, 12] meses',
                                               Months_to_DPD10<=24~'6. (12, 24] meses',
                                               Months_to_DPD10>24~'7. > 24 meses',)) %>% 
  select(monDate, Sucursal, NOMBRE_MODULO, Sector_Destino, Sector_Actividad,
         PlazoAnios, rangos, rangom, Tipo_Credito, Objeto, CreditoNuevo, Tipo_Cartera, 
         Months_to_first_DPD5_bin, Months_to_first_DPD10_bin, Antes2016,
         SaldoBruto, SaldoMora, Par0, OpsBruta, OpsMora, OpsPar0) %>% 
  group_by(monDate, Sucursal, NOMBRE_MODULO, Sector_Destino, Sector_Actividad,
           PlazoAnios, rangos, rangom, Tipo_Credito, Objeto, CreditoNuevo, Tipo_Cartera, 
           Months_to_first_DPD5_bin, Months_to_first_DPD10_bin, Antes2016) %>% 
  summarise_all(sum) %>% 
  ungroup()

bdc_ttf_grouped <- bdc_ttf %>% 
  mutate(Sector_Actividad = case_when(Sector_Actividad %in% c("Asalariados", "K. Inter. Fin.", "L. Serv. Inmob.","O. Serv. Hosp + Otros","Q. Org. Extraterritoriales","Otros") ~ "Servicios",
                                      Sector_Actividad %in% c("C. Ext. Gas y Pet.", "D. Ext. Minerales", "F. Dist. EE y agua") ~ "Industria",
                                      Sector_Actividad == "H. Comercio" ~ "Comercio",
                                      Sector_Actividad == "I. Hoteles" ~ "Hoteles",
                                      Sector_Actividad == "E. Ind. y Manu" ~ "Manufactura",
                                      Sector_Actividad == "J. Transporte" ~ "Transporte",
                                      Sector_Actividad == "G. Construcción" ~ "Construcción",
                                      Sector_Actividad == "Agropecuario" ~ "Agropecuario",
                                      TRUE~"Servicios")) %>% 
  mutate(Sector_Destino = case_when(Sector_Destino %in% c("Asalariados", "K. Inter. Fin.", "L. Serv. Inmob.","O. Serv. Hosp + Otros","Q. Org. Extraterritoriales","Otros") ~ "Servicios",
                                      Sector_Destino %in% c("C. Ext. Gas y Pet.", "D. Ext. Minerales", "F. Dist. EE y agua") ~ "Industria",
                                      Sector_Destino == "H. Comercio" ~ "Comercio",
                                      Sector_Destino == "I. Hoteles" ~ "Hoteles",
                                      Sector_Destino == "E. Ind. y Manu" ~ "Manufactura",
                                      Sector_Destino == "J. Transporte" ~ "Transporte",
                                      Sector_Destino == "G. Construcción" ~ "Construcción",
                                      Sector_Destino == "Agropecuario" ~ "Agropecuario",
                                      TRUE~"Servicios")) %>% 
  mutate(PlazoAnios = case_when(PlazoAnios %in% c("0","1","2") ~ "2 años o menos",
                                PlazoAnios %in% c(">7") ~ "Más de 7 años",
                                TRUE~PlazoAnios)) %>% 
  mutate(Months_to_first_DPD5_bin = case_when(Months_to_DPD5==999~'8. Sin evento',
                                              Months_to_DPD5<=1~'1. [0, 1] meses',
                                              Months_to_DPD5<=3~'2. (1, 3] meses',
                                              Months_to_DPD5<=6~'3. (3, 6] meses',
                                              Months_to_DPD5<=9~'4. (6, 9] meses',
                                              Months_to_DPD5<=12~'5. (9, 12] meses',
                                              Months_to_DPD5<=24~'6. (12, 24] meses',
                                              Months_to_DPD5>24~'7. > 24 meses',)) %>% 
  mutate(Months_to_first_DPD10_bin = case_when(Months_to_DPD10==999~'8. Sin evento',
                                               Months_to_DPD10<=1~'1. [0, 1] meses',
                                               Months_to_DPD10<=3~'2. (1, 3] meses',
                                               Months_to_DPD10<=6~'3. (3, 6] meses',
                                               Months_to_DPD10<=9~'4. (6, 9] meses',
                                               Months_to_DPD10<=12~'5. (9, 12] meses',
                                               Months_to_DPD10<=24~'6. (12, 24] meses',
                                               Months_to_DPD10>24~'7. > 24 meses',)) %>% 
  select(monDate, Sucursal, NOMBRE_MODULO, Sector_Destino, Sector_Actividad,
         PlazoAnios, rangos, rangom, Tipo_Credito, Objeto, CreditoNuevo, Tipo_Cartera, 
         Months_to_first_DPD5_bin, Months_to_first_DPD10_bin, Antes2016,
         SaldoBruto, SaldoMora, Par0, OpsBruta, OpsMora, OpsPar0) %>% 
  group_by(monDate, Sucursal, NOMBRE_MODULO, Sector_Destino, Sector_Actividad,
           PlazoAnios, rangos, rangom, Tipo_Credito, Objeto, CreditoNuevo, Tipo_Cartera, 
           Months_to_first_DPD5_bin, Months_to_first_DPD10_bin, Antes2016) %>% 
  summarise_all(sum) %>% 
  ungroup()

bdcFull <- bdc_ttf_grouped %>% 
  bind_rows(bdc_ttf_Sep_grouped)

write_xlsx(bdcFull, "D:/!bso/firstTimes/bdc_ttf_Evol_Sep2023_v2.xlsx")
####____EXPLORATORY ANALYSIS____#####
bdc_ttf <- bdc_wMeasures %>% 
  mutate(cosechaM = as.yearmon(fdes)) %>% 
  mutate(cosechaY = year(fdes)) %>% 
  mutate(myFirstTardio = as.yearmon(FechaFirstTardio),
         myFirstCond = as.yearmon(FechaFirstCond)) %>%
  mutate(monthsSinceDisb = round((monDate-cosechaM)*12+1)) %>% 
  mutate(across(starts_with("Fecha"), ~as.Date(.x))) %>% 
  mutate(meanMoraIM_bin = case_when(meanMoraIM==0 ~ '1. 0',
                                    meanMoraIM<5 ~ '2. <5',
                                    meanMoraIM<10 ~ '3. [5-10)',
                                    meanMoraIM<15 ~ '4. [10-15)',
                                    meanMoraIM<20 ~ '5. [15-20)',
                                    meanMoraIM>=20 ~ '6. >=20')) %>% 
  mutate(IntPT = CantPT/monthsSinceDisb) %>% 
  mutate(IntCond = CantCond/monthsSinceDisb) %>% 
  mutate(Months_to_first_cond = if_else(is.na(myFirstCond),-1,
                                        round((myFirstCond-cosechaM)*12))) %>% 
  mutate(Months_to_first_tardio = if_else(is.na(myFirstTardio),-1,
                                          round((myFirstTardio-cosechaM)*12))) %>% 
  mutate(Months_to_DPD1 = case_when(is.na(myFirst_DPD1) ~ -1,
                                    TRUE ~ round((myFirst_DPD1-cosechaM)*12+1))) %>% 
  mutate(Months_to_DPD5 = case_when(is.na(myFirst_DPD5) ~ -1,
                                    TRUE ~ round((myFirst_DPD5-cosechaM)*12+1))) %>% 
  mutate(Months_to_DPD10 = case_when(is.na(myFirst_DPD10) ~ -1,
                                     TRUE ~ round((myFirst_DPD10-cosechaM)*12+1))) %>% 
  mutate(Months_to_DPD15 = case_when(is.na(myFirst_DPD15) ~ -1,
                                     TRUE ~ round((myFirst_DPD15-cosechaM)*12+1))) %>% 
  mutate(Months_to_DPD20 = case_when(is.na(myFirst_DPD20) ~ -1,
                                     cosechaM<"Ene. 2018"~ round((myFirst_DPD20-as.yearmon("Dic. 2017"))*12),
                                     TRUE ~ round((myFirst_DPD20-cosechaM)*12+1))) %>% 
  mutate(Months_to_DPD25 = case_when(is.na(myFirst_DPD25) ~ -1,
                                     TRUE ~ round((myFirst_DPD25-cosechaM)*12+1))) 

bdcExp <- bdc_ttf %>% 
  select(-starts_with("myFirst")) %>% 
  mutate(Months_to_first_cond_bin = case_when(Months_to_first_cond<0~'0. Sin evento',
                                              Months_to_first_cond<=3~'1. <3 meses',
                                              Months_to_first_cond<=6~'2. (3, 6] meses',
                                              Months_to_first_cond<=12~'3. (6, 12] meses',
                                              Months_to_first_cond<=24~'4. (12, 24] meses',
                                              Months_to_first_cond>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_tardio_bin = case_when(Months_to_first_tardio<0~'0. Sin evento',
                                                Months_to_first_tardio<=3~'1. <3 meses',
                                                Months_to_first_tardio<=6~'2. (3, 6] meses',
                                                Months_to_first_tardio<=12~'3. (6, 12] meses',
                                                Months_to_first_tardio<=24~'4. (12, 24] meses',
                                                Months_to_first_tardio>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_DPD1_bin = case_when(Months_to_DPD1<0~'0. Sin evento',
                                              Months_to_DPD1<=3~'1. <3 meses',
                                              Months_to_DPD1<=6~'2. (3, 6] meses',
                                              Months_to_DPD1<=12~'3. (6, 12] meses',
                                              Months_to_DPD1<=24~'4. (12, 24] meses',
                                              Months_to_DPD1>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_DPD5_bin = case_when(Months_to_DPD5<0~'0. Sin evento',
                                              Months_to_DPD5<=3~'1. <3 meses',
                                              Months_to_DPD5<=6~'2. (3, 6] meses',
                                              Months_to_DPD5<=12~'3. (6, 12] meses',
                                              Months_to_DPD5<=24~'4. (12, 24] meses',
                                              Months_to_DPD5>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_DPD10_bin = case_when(Months_to_DPD10<0~'0. Sin evento',
                                               Months_to_DPD10<=3~'1. <3 meses',
                                               Months_to_DPD10<=6~'2. (3, 6] meses',
                                               Months_to_DPD10<=12~'3. (6, 12] meses',
                                               Months_to_DPD10<=24~'4. (12, 24] meses',
                                               Months_to_DPD10>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_DPD15_bin = case_when(Months_to_DPD15<0~'0. Sin evento',
                                               Months_to_DPD15<=3~'1. <3 meses',
                                               Months_to_DPD15<=6~'2. (3, 6] meses',
                                               Months_to_DPD15<=12~'3. (6, 12] meses',
                                               Months_to_DPD15<=24~'4. (12, 24] meses',
                                               Months_to_DPD15>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_DPD20_bin = case_when(Months_to_DPD20<0~'0. Sin evento',
                                               Months_to_DPD20<=3~'1. <3 meses',
                                               Months_to_DPD20<=6~'2. (3, 6] meses',
                                               Months_to_DPD20<=12~'3. (6, 12] meses',
                                               Months_to_DPD20<=24~'4. (12, 24] meses',
                                               Months_to_DPD20>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_DPD25_bin = case_when(Months_to_DPD25<0~'0. Sin evento',
                                               Months_to_DPD25<=3~'1. <3 meses',
                                               Months_to_DPD25<=6~'2. (3, 6] meses',
                                               Months_to_DPD25<=12~'3. (6, 12] meses',
                                               Months_to_DPD25<=24~'4. (12, 24] meses',
                                               Months_to_DPD25>24~'5. >24 meses',)) %>% 
  mutate(across(c(monDate,cosechaM),~as.Date(.x,frac=1))) %>% 
  mutate(IntPT_bin = cut(IntPT, breaks = c(-Inf,0,0.2,0.4,0.6,0.8,1), include.lowest=T,right=T,
                         labels = c('0','(0,0.2]','(0.2,0.4]','(0.4,0.6]','(0.6,0.8]','(0.8,1]'))) %>% 
  mutate(IntCond_bin = cut(IntCond, breaks = c(-Inf,0,0.2,0.4,0.6,0.8,1), include.lowest=T,right=T,
                           labels = c('0','(0,0.2]','(0.2,0.4]','(0.4,0.6]','(0.6,0.8]','(0.8,1]'))) %>% 
  mutate(IntDPD1_bin = cut(IntDPD1, breaks = c(-Inf,0,0.2,0.4,0.6,0.8,1), include.lowest=T,right=T,
                           labels = c('0','(0,0.2]','(0.2,0.4]','(0.4,0.6]','(0.6,0.8]','(0.8,1]'))) %>% 
  mutate(IntDPD5_bin = cut(IntDPD5, breaks = c(-Inf,0,0.2,0.4,0.6,0.8,1), include.lowest=T,right=T,
                           labels = c('0','(0,0.2]','(0.2,0.4]','(0.4,0.6]','(0.6,0.8]','(0.8,1]'))) %>% 
  mutate(IntDPD10_bin = cut(IntDPD10, breaks = c(-Inf,0,0.2,0.4,0.6,0.8,1), include.lowest=T,right=T,
                            labels = c('0','(0,0.2]','(0.2,0.4]','(0.4,0.6]','(0.6,0.8]','(0.8,1]'))) %>% 
  mutate(IntDPD15_bin = cut(IntDPD15, breaks = c(-Inf,0,0.2,0.4,0.6,0.8,1), include.lowest=T,right=T,
                            labels = c('0','(0,0.2]','(0.2,0.4]','(0.4,0.6]','(0.6,0.8]','(0.8,1]'))) %>% 
  mutate(IntDPD20_bin = cut(IntDPD20, breaks = c(-Inf,0,0.2,0.4,0.6,0.8,1), include.lowest=T,right=T,
                            labels = c('0','(0,0.2]','(0.2,0.4]','(0.4,0.6]','(0.6,0.8]','(0.8,1]'))) %>% 
  mutate(IntDPD25_bin = cut(IntDPD25, breaks = c(-Inf,0,0.2,0.4,0.6,0.8,1), include.lowest=T,right=T,
                            labels = c('0','(0,0.2]','(0.2,0.4]','(0.4,0.6]','(0.6,0.8]','(0.8,1]'))) %>% 
  mutate(across(ends_with("_bin"),~as.character(.x)))

bdcExp <- bdcExp %>% 
  left_join(codMod, by="MODULO") %>% 
  mutate(Tipo_Credito = case_when(substr(TIPO_CREDITO,1,1) =='P' ~ 'Pyme',
                                  substr(TIPO_CREDITO,1,1) =='M' ~ 'Micro',
                                  substr(TIPO_CREDITO,1,1) =='N' ~ 'Consumo',
                                  TIPO_CREDITO %in% c('H0','H1','H2') ~ 'Vivienda Normal',
                                  TIPO_CREDITO %in% c('H3','H4') ~ 'Vivienda Social',)) %>% 
  select(-NOMBRE, -CI, -MODULO, -fueRefin, -fueReprog, -fueCast, -NReprog, -fdesLast,
         -FechaRefin, -FechaReprog, -FechaCast) %>% 
  relocate(NOMBRE_MODULO, Tipo_Credito, .after = TIPO_CREDITO)

write_xlsx(bdcExp, "D:/!bso/firstTimes/ttf_Disbursement_Ago2023.xlsx")
