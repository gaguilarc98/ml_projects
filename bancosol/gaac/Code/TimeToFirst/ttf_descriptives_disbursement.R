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
ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Sep23.rds")
checkpt <- ptFull %>% 
  group_by(myPago) %>% 
  summarise(NObs= n(),NOps=n_distinct(Operacion), check=NObs==NOps)

pt_grouped <- ptFull %>% 
  dplyr::filter(myPago>"Dic. 2018" & myPago <="Sep. 2023") %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(FechaFirstTardio = min(FechaPago), 
            CantPT = n()) %>% 
  ungroup()
saveRDS(pt_grouped, "D:/!bso/firstTimes/pt_grouped.rds")
####____CONDONACIONES____####
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Sep2023.rds')
zeros <- condFull %>%  
  dplyr::filter(Total_Cond_Cap_Int==0) %>% 
  group_by(as.yearmon(Fecha)) %>% summarise(nOps=n())

cond_clean <- condFull %>% 
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
####____MORA_IM_2018____####
moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2016Sep2023.rds")) %>% 
  select(monDate, CTACLIENTE, OPERACION, maximaDPD_2, starts_with("tuvo")) %>% 
  dplyr::filter(!is.na(maximaDPD_2)) 

measuresMoraIM <- moraIntraMes %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  summarise(meanMoraIM = mean(maximaDPD_2),
            mesesDesde16 = n(),
            IntDPD1 = sum(tuvoDPD1)/mesesDesde16,
            IntDPD5 = sum(tuvoDPD5)/mesesDesde16,
            IntDPD10 = sum(tuvoDPD10)/mesesDesde16,
            IntDPD15 = sum(tuvoDPD15)/mesesDesde16,
            IntDPD20 = sum(tuvoDPD20)/mesesDesde16,
            IntDPD25 = sum(tuvoDPD25)/mesesDesde16) %>% 
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
pt_grouped <- readRDS("D:/!bso/firstTimes/pt_grouped.rds")
cond_grouped <- readRDS("D:/!bso/firstTimes/cond_grouped.rds")
codMod <- read_xlsx("D:/!bso/bases/excel/CodModulo.xlsx") 
measuresMoraIM <- readRDS("D:/!bso/firstTimes/measures.rds")
dpd1 <- readRDS("D:/!bso/firstTimes/dpd1.rds")
dpd5 <- readRDS("D:/!bso/firstTimes/dpd5.rds")
dpd10 <- readRDS("D:/!bso/firstTimes/dpd10.rds")
dpd15 <- readRDS("D:/!bso/firstTimes/dpd15.rds")
dpd20 <- readRDS("D:/!bso/firstTimes/dpd20.rds")
dpd25 <- readRDS("D:/!bso/firstTimes/dpd25.rds")

clientes <- readRDS('D:/!bso/features/ClientesTTF_Ene2015Sep2023.rds') %>% 
  group_by(ASESOR) %>% 
  mutate(NOMBRE_ASESOR = max(NOMBRE_ASESOR[which(nchar(NOMBRE_ASESOR)==max(nchar(NOMBRE_ASESOR)))],na.rm = T)) %>% 
  ungroup()
clientes18 <- clientes %>% 
  dplyr::filter(fdes>=as.Date("2017-01-01")) %>% 
  dplyr::filter(!is.na(esFSL)) #| esFSL==1, para los clientes sin FSL es 0 
remove(clientes)

bdc_wMeasures <- clientes18 %>% 
  dplyr::filter(ctaCont!='623') %>% 
  left_join(pt_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(cond_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(measuresMoraIM, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(dpd1,  by = c("CTACLIENTE","OPERACION")) %>% 
  left_join(dpd5,  by = c("CTACLIENTE","OPERACION")) %>% 
  left_join(dpd10,  by = c("CTACLIENTE","OPERACION")) %>% 
  left_join(dpd15,  by = c("CTACLIENTE","OPERACION")) %>% 
  left_join(dpd20,  by = c("CTACLIENTE","OPERACION")) %>% 
  left_join(dpd25,  by = c("CTACLIENTE","OPERACION")) %>% 
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
  left_join(codMod, by="MODULO") %>% 
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
  mutate(Months_to_first_cond = if_else(is.na(myFirstCond),999,
                                        round((myFirstCond-cosechaM)*12))) %>% 
  mutate(Months_to_first_tardio = if_else(is.na(myFirstTardio),999,
                                          round((myFirstTardio-cosechaM)*12))) %>% 
  # mutate(Months_to_DPD1 = case_when(is.na(myFirst_DPD1) ~ 999,
  #                                   TRUE ~ round((myFirst_DPD1-cosechaM)*12))) %>% 
  mutate(Months_to_DPD5 = case_when(is.na(myFirst_DPD5) ~ 999,
                                    TRUE ~ round((myFirst_DPD5-cosechaM)*12))) %>% 
  mutate(Months_to_DPD10 = case_when(is.na(myFirst_DPD10) ~ 999,
                                     TRUE ~ round((myFirst_DPD10-cosechaM)*12))) %>% 
  # mutate(Months_to_DPD15 = case_when(is.na(myFirst_DPD15) ~ 999,
  #                                    TRUE ~ round((myFirst_DPD15-cosechaM)*12))) %>% 
  # mutate(Months_to_DPD20 = case_when(is.na(myFirst_DPD20) ~ 999,
  #                                    TRUE ~ round((myFirst_DPD20-cosechaM)*12))) %>% 
  # rowwise() %>% 
  # mutate(Months_to_Event5 = min(Months_to_first_cond, Months_to_first_tardio, Months_to_DPD5)) %>% 
  # mutate(Months_to_Event10 = min(Months_to_first_cond, Months_to_first_tardio, Months_to_DPD10)) %>% 
  # ungroup() %>% 
  mutate(Ops_PT_3 = ifelse(Months_to_first_tardio>=0 & Months_to_first_tardio<=3, 1, 0)) %>% 
  mutate(Ops_PT_6 = ifelse(Months_to_first_tardio>=0 & Months_to_first_tardio<=6, 1, 0)) %>% 
  mutate(Ops_PT_9 = ifelse(Months_to_first_tardio>=0 & Months_to_first_tardio<=9, 1, 0)) %>% 
  mutate(Ops_Cond_3 = ifelse(Months_to_first_cond>=0 & Months_to_first_cond<=3, 1, 0)) %>% 
  mutate(Ops_Cond_6 = ifelse(Months_to_first_cond>=0 & Months_to_first_cond<=6, 1, 0)) %>% 
  mutate(Ops_Cond_9 = ifelse(Months_to_first_cond>=0 & Months_to_first_cond<=9, 1, 0)) %>% 
  mutate(Ops_DPD5_3 = ifelse(Months_to_DPD5>=0 & Months_to_DPD5<=3, 1, 0)) %>% 
  mutate(Ops_DPD5_6 = ifelse(Months_to_DPD5>=0 & Months_to_DPD5<=6, 1, 0)) %>% 
  mutate(Ops_DPD5_9 = ifelse(Months_to_DPD5>=0 & Months_to_DPD5<=9, 1, 0)) %>% 
  mutate(Ops_DPD10_3 = ifelse(Months_to_DPD10>=0 & Months_to_DPD10<=3, 1, 0)) %>% 
  mutate(Ops_DPD10_6 = ifelse(Months_to_DPD10>=0 & Months_to_DPD10<=6, 1, 0)) %>% 
  mutate(Ops_DPD10_9 = ifelse(Months_to_DPD10>=0 & Months_to_DPD10<=9, 1, 0)) %>% 
  # mutate(Ops_Event5_3 = ifelse(Months_to_Event5>=0 & Months_to_Event5<=3, 1, 0)) %>% 
  # mutate(Ops_Event5_6 = ifelse(Months_to_Event5>=0 & Months_to_Event5<=6, 1, 0)) %>% 
  # mutate(Ops_Event5_9 = ifelse(Months_to_Event5>=0 & Months_to_Event5<=9, 1, 0)) %>% 
  # mutate(Ops_Event10_3 = ifelse(Months_to_Event10>=0 & Months_to_Event10<=3, 1, 0)) %>% 
  # mutate(Ops_Event10_6 = ifelse(Months_to_Event10>=0 & Months_to_Event10<=6, 1, 0)) %>% 
  # mutate(Ops_Event10_9 = ifelse(Months_to_Event10>=0 & Months_to_Event10<=9, 1, 0)) %>% 
  # # mutate(Monto_PT_3 = ifelse(Months_to_first_tardio>=0 & Months_to_first_tardio<=3, MontoDes, 0)) %>% 
  # mutate(Monto_PT_6 = ifelse(Months_to_first_tardio>=0 & Months_to_first_tardio<=6, MontoDes, 0)) %>% 
  # mutate(Monto_PT_9 = ifelse(Months_to_first_tardio>=0 & Months_to_first_tardio<=9, MontoDes, 0)) %>% 
  # mutate(Monto_Cond_3 = ifelse(Months_to_first_cond>=0 & Months_to_first_cond<=3, MontoDes, 0)) %>% 
  # mutate(Monto_Cond_6 = ifelse(Months_to_first_cond>=0 & Months_to_first_cond<=6, MontoDes, 0)) %>% 
  # mutate(Monto_Cond_9 = ifelse(Months_to_first_cond>=0 & Months_to_first_cond<=9, MontoDes, 0)) %>% 
  # mutate(Monto_DPD5_3 = ifelse(Months_to_DPD5>=0 & Months_to_DPD5<=3, MontoDes, 0)) %>% 
  # mutate(Monto_DPD5_6 = ifelse(Months_to_DPD5>=0 & Months_to_DPD5<=6, MontoDes, 0)) %>% 
  # mutate(Monto_DPD5_9 = ifelse(Months_to_DPD5>=0 & Months_to_DPD5<=9, MontoDes, 0)) %>% 
  # mutate(Monto_DPD10_3 = ifelse(Months_to_DPD10>=0 & Months_to_DPD10<=3, MontoDes, 0)) %>% 
  # mutate(Monto_DPD10_6 = ifelse(Months_to_DPD10>=0 & Months_to_DPD10<=6, MontoDes, 0)) %>% 
  # mutate(Monto_DPD10_9 = ifelse(Months_to_DPD10>=0 & Months_to_DPD10<=9, MontoDes, 0)) %>% 
  # mutate(Monto_DPD15_3 = ifelse(Months_to_DPD15>=0 & Months_to_DPD15<=3, MontoDes, 0)) %>% 
  # mutate(Monto_DPD15_6 = ifelse(Months_to_DPD15>=0 & Months_to_DPD15<=6, MontoDes, 0)) %>% 
  # mutate(Monto_DPD15_9 = ifelse(Months_to_DPD15>=0 & Months_to_DPD15<=9, MontoDes, 0)) %>% 
  mutate(across(c(monDate,cosechaM),~as.Date(.x,frac=1))) %>% 
  mutate(Tipo_Credito = case_when(substr(TIPO_CREDITO,1,1) =='P' ~ 'Pyme',
                                  substr(TIPO_CREDITO,1,1) =='M' ~ 'Micro',
                                  substr(TIPO_CREDITO,1,1) =='N' ~ 'Consumo',
                                  TIPO_CREDITO %in% c('H0','H1','H2') ~ 'Vivienda Normal',
                                  TIPO_CREDITO %in% c('H3','H4') ~ 'Vivienda Social',)) %>% 
  mutate(lastMora = ifelse(ctaCont %in% c('133','134','136','137'), 1, 0)) %>% 
  mutate(lastMoraCast = ifelse(ctaCont %in% c('133','134','136','137','865'), 1, 0)) %>% 
  select(-CI, -MODULO, -fueRefin, -fueReprog, -fueCast, -NReprog, -fdesLast,
         -FechaRefin, -FechaReprog, -FechaCast, -starts_with("myFirst"),
         -mesesDesde16, -starts_with("Int"), -starts_with("meanMora")) %>% 
  relocate(NOMBRE_MODULO, Tipo_Credito, .after = TIPO_CREDITO)


bdcLast <- readRDS("D:/!bso/girCartera/rds/ec_Sep2023.rds") %>% 
  select(ASESOR) %>% 
  distinct_all() %>% 
  mutate(Vigente = 1)

bdc_ttf <- bdc_ttf %>% 
  left_join(bdcLast, by="ASESOR") %>% 
  replace_na(list(Vigente = 0))

# write_xlsx(bdc_ttf, "D:/!bso/firstTimes/ttf_Disbursement_Ago2023_esFSL.xlsx") #FASSIL PARA REVERSIÓN
write_xlsx(bdc_ttf, "D:/!bso/firstTimes/ttf_Disbursement_Sep2023_v2.xlsx")
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
