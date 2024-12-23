####___METRICAS DE CLIENTES CON EVENTOS DE INCUMPLIMIENTO____####
#Para considerar a una operación como irregular en su comportamiento de pago
#se proponen las siguientes métricas y clasificación de métricas:
# * A nivel de operación:
#   * Si el último mes tuvo pago tardío.
#   * Cantidad de Pagos Tardíos en los últimos 12 meses.
#   * Fecha de primer pago tardío (solo desembolsos desde enero 2019)
#   * Cantidad de Meses con Pago Tardio/Cantidad de Meses en BSO desde enero 2019 (opcional)
#   * Si el último mes tuvo condonación.
#   * Cantidad de Condonaciones en los últimos 12 meses.
#   * Fecha de primera condonación (solo desembolsos desde enero 2019)
#   * Cantidad de Meses con Condonacion/Cantidad de Meses en BSO desde enero 2019 (opcional)
#   * Fecha de primer incumplimiento (solo desembolsos desde enero 2018, primera FVEN_ULTPAGO incumplida)
#   * Promedio de mora intra-mes
#   * Cantidad de meses con moraIM/Cantidad de Meses en BSO desde enero 2018
#   * Fecha de primer cierre con mora (solo desembolsos desde enero 2015)
#   * Cantidad de meses con mora al cierre/Cantidad de Meses en BSO desde enero 2015
# * A nivel de cliente:
#   * Si el último mes tuvo días mora en sistema.
#   * Si estuvo no vigente en el sistema.
#   * Cantidad de meses no vigentes/Cantidad de Meses Compartido desde enero 2018
#   * Cantidad de meses desde el incumplimiento

#Puntos a considerar:
# 1. Para que estas métricas funcionen se debe colocar correctamente la FECHA DE DESEMBOLSO
#a cada operación, es decir, corregir la fecha de desembolso a las operaciones reprogramadas.
# 2. En cada operación se debe regularizar el CI y/o la cuenta cliente para poder agrupar
#y contar a los clientes de manera única.
# 3. Como descriptores de cada operación conservar: Sucursal, NOMBRE_AGENCIA, TipoCrédito
#Monto (al último cierre), Monto original (al momento de desembolso), Rango de desembolso, CAEDEC_DEST, CIU,
#Sector_Destino, Sector_Actividad, PLAZODIAS, CALIFICACION, DIASMORA, MODULO, TIPO_OPER,
#OBJETO_CRED, DESC_SEGMERC, CPOP, SECTOR_CARTERA
# 4. Además de forma obligatoria filtrar solo la Cartera Bruta e incluir TASAACT, ctaCont, MONEDA, saldous.
# 5. A implementar se encuentra el plazo original al momento del desembolso, asesor vigente
#Y asesor al momento del desembolso.
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
####____LECTURA DE BASE DE CARTERA____####
longmonth <- "Jul. 2023"
shortmonth <- str_replace(longmonth,". ","")
bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",shortmonth,".rds")) %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>%
  mutate(Sector_Actividad = case_when(Sector_Actividad=='H. Comercio' & divCaedecC %in% c('50','51')~'H1. Ventas al por mayor',
                                      Sector_Actividad=='H. Comercio' & divCaedecC %in% c('52')~'H2. Ventas al por menor',
                                      TRUE~Sector_Actividad)) %>% 
  mutate(PLAZODIAS_bin = case_when(floor(PLAZODIAS/365)==0 ~'0. < 1 año',
                                   floor(PLAZODIAS/365)==1 ~'1. 1 año',
                                   floor(PLAZODIAS/365)==2 ~'2. 2 años',
                                   floor(PLAZODIAS/365)==3 ~'3. 3 años',
                                   floor(PLAZODIAS/365)==4 ~'4. 4 años',
                                   floor(PLAZODIAS/365)>4 ~'5. > 4 años',)) %>% 
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER, "MIGR"), 1, 0)) %>% 
  select(monDate, CTACLIENTE, OPERACION, CI, MODULO, TIPO_OPER, Sucursal, NOMBRE_AGENCIA, 
         tipoCred, MONTO, rangom, CAEDEC_DEST, CIU, Sector_Actividad, Sector_Destino, PLAZODIAS, PLAZODIAS_bin,
         FDESEMBOLSO, CALIFICACION, DIASMORA, OBJETO_CRED, DESC_SEGMERC, CPOP, SECTOR_CARTERA, 
         ctaCont, saldous, montous, par0, saldoMora, esFSL) 
####____PAGOS TARDIOS____####
# lastCierre <- fread(paste0('D:/!bso/mph/Oreports/lastCierrreUR_',shortmonth,'.txt'),
#                     sep='|', quote = FALSE) %>% 
#   select(CTACLIENTE=Cuenta, OPERACION=Operacion, UltMesPagoTardio=Ultimo_mes,
#          CantPagosTardios=Instancias_UR)

ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Jul23.rds")
checkpt <- ptFull %>% 
  group_by(myPago) %>% 
  summarise(NObs= n(),NOps=n_distinct(Operacion), check=NObs==NOps)

pt_grouped <- ptFull %>% 
  dplyr::filter(myPago>"May. 2018" & myPago <="Jul. 2023") %>% 
  mutate(UltMesPagoTardio = ifelse(myPago == "Jul. 2023",1,0)) %>% 
  mutate(Ult12MesesPagoTardio = ifelse(myPago<="Jul. 2023" & myPago >= "Ago. 2022",1,0)) %>% 
  group_by(Operacion) %>% 
  mutate(Ult12MesesPagoTardio = sum(Ult12MesesPagoTardio)) %>% 
  ungroup() %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(FechaFirstTardio = min(FechaPago),
            MesesPagoTardio = n(),
            UltMesPagoTardio = max(UltMesPagoTardio),
            Ult12MesesPagoTardio = max(Ult12MesesPagoTardio)) %>% 
  ungroup()

# bdcPt <- bdc %>% 
#   inner_join(pt_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
#   dplyr::filter(Ult12MesesPagoTardio>0)
####____CONDONACIONES____####
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Jul2023.rds')
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
  mutate(UltMesCondonado = ifelse(myCond == "Jul. 2023",1,0)) %>% 
  mutate(Ult12MesesCondonado = ifelse(myCond<="Jul. 2023" & myCond >= "Ago. 2022",1,0)) %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(FechaFirstCond),
            MesesCondonado = n(),
            UltMesCondonado = max(UltMesCondonado),
            Ult12MesesCondonado = sum(Ult12MesesCondonado)) %>% 
  ungroup()
####____HISTORIA EN SF____####
HistCalif <- readRDS("D:/!bso/vipCartera/historic/HistCalif_Jun2023.rds")
checkSF <- HistCalif %>% 
  group_by(FECHA) %>% 
  summarise(NObs=n(), NClie=n_distinct(CTACLIENTE), check=NObs==NClie)  

sf_grouped <- HistCalif %>% 
  # dplyr::filter(FECHA <= "may. 2023") %>% #Filter to reproduce previous months comment to keep all data
  select(FECHA, CTACLIENTE, CALIFICACION_SF, ESTADO_HIST_SF, ESTADO_LAST_SF) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(UltMesNoVigSF = ifelse(FECHA=="jun. 2023" & ESTADO_LAST_SF %in% c(2,3,4), 1, 0)) %>% 
  mutate(badCredit = ifelse(ESTADO_HIST_SF %in% c(2,3,4),1,0)) %>% 
  # mutate(worstCredit = ifelse(ESTADO_HIST_SF == 4,1,0)) %>% 
  summarise(UltMesNoVigSF = max(UltMesNoVigSF),
            VecesNoVigSF = sum(badCredit), 
            MesesSF = n(),
            IntMoraSF = VecesNoVigSF/MesesSF) %>% 
  mutate(FueCompartido=1)#Revisar que pasa si un cliente tiene más de un crédito

remove(list=c("ptFull","condFull","HistCalif"))
####____MORA_IM_2018____####
moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Jun2023.rds")) %>% 
  dplyr::filter(!is.na(maximaDPD)) %>% 
  mutate(tuvoMora = ifelse(maximaDPD>0, 1, 0)) 

measuresMoraIM <- moraIntraMes %>% 
  # dplyr::filter(monDate!="jun. 2023") %>% #Filtro para reproducir meses anteriores
  group_by(CTACLIENTE, OPERACION) %>% 
  # mutate(mesDiasMora = if_else(maximaDPD==0, 2014, year(monDate))) %>%
  summarise(meanMoraIM = mean(maximaDPD),
            VecesMoraIM = sum(tuvoMora),
            MesesIM = n(),
            IntMoraIM = VecesMoraIM/MesesIM) %>% #lastMoraIM_cl = max(mesDiasMora), maxMoraIM_cl = max(maximaDPD)
  ungroup()

FirstMoraIM <- moraIntraMes %>% 
  group_by(CTACLIENTE, OPERACION) %>% #Se agrupa por operación de cada cliente
  dplyr::filter(tuvoMora>0) %>% 
  mutate(monDate = as.Date(monDate, frac=1))
sapply(FirstMoraIM, function(x){length(which(is.na(x)))})
FirstMoraIM <- FirstMoraIM %>% 
  mutate(UltMesMoraIM = ifelse(as.yearmon(monDate) == "Jun. 2023" & tuvoMora==1,1,0)) %>% 
  group_by(CTACLIENTE, OPERACION) %>%
  # mutate(FVEN_ULTPAGO = ifelse(is.na(FVEN_ULTPAGO), monDate, FVEN_ULTPAGO)) %>% 
  summarise(FechaFirstMoraIM = min(FVEN_ULTPAGO),
            MesFirstMoraIM = min(monDate)) %>% 
  ungroup()


measuresMoraIM <- readRDS("D:/!bso/firstTimes/measures.rds")
dpd1 <- readRDS("D:/!bso/firstTimes/dpd1.rds")
dpd5 <- readRDS("D:/!bso/firstTimes/dpd5.rds")
dpd10 <- readRDS("D:/!bso/firstTimes/dpd10.rds")
dpd15 <- readRDS("D:/!bso/firstTimes/dpd15.rds")
dpd20 <- readRDS("D:/!bso/firstTimes/dpd20.rds")
dpd25 <- readRDS("D:/!bso/firstTimes/dpd25.rds")
####____JOIN EVERYTHING____####
glimpse(pt_grouped)
glimpse(cond_grouped)
glimpse(sf_grouped)
glimpse(measuresMoraIM)
glimpse(FirstMoraIM)
glimpse(dpd1)
glimpse(dpd25)
dfTotal <- readRDS("D:/!bso/features/Clientes_Ene15Jul23_v2.rds") %>% 
  select(CTACLIENTE, OPERACION, fdes_ori=fdes)

bdc_wMeasures <- bdc %>% 
  left_join(dfTotal, by=c("CTACLIENTE", "OPERACION")) %>% 
  left_join(pt_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(cond_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(sf_grouped, by="CTACLIENTE") %>% 
  left_join(measuresMoraIM, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(dpd1,  by = c("CTACLIENTE","OPERACION")) %>% 
  left_join(dpd5,  by = c("CTACLIENTE","OPERACION")) %>% 
  left_join(dpd10,  by = c("CTACLIENTE","OPERACION")) %>% 
  left_join(dpd15,  by = c("CTACLIENTE","OPERACION")) %>% 
  left_join(dpd20,  by = c("CTACLIENTE","OPERACION")) %>% 
  left_join(dpd25,  by = c("CTACLIENTE","OPERACION")) %>% 
  replace_na(list(MesesPagoTardio=0, UltMesPagoTardio=0, Ult12MesesPagoTardio=0, 
                  MesesCondonado=0, UltMesCondonado=0, Ult12MesesCondonado=0, 
                  UltMesNoVigSF=0, VecesNoVigSF=0, MesesSF=0, IntMoraSF=0, FueCompartido=0)) %>% 
  ungroup() %>% 
  # mutate(FechaFirstTardio = if_else(fdes_ori<as.Date("2018-06-01"), NA, FechaFirstTardio)) %>% 
  # mutate(FechaFirstCond = if_else(fdes_ori<as.Date("2019-01-01"), NA, FechaFirstCond)) %>% 
  # mutate(FechaFirstMora = if_else(fdes_ori<as.Date("2015-01-01"), NA, FechaFirstMora)) %>% 
  # mutate(FechaFirstMoraIM = if_else(is.na(FechaFirstMoraIM), MesFirstMoraIM, FechaFirstMoraIM)) %>% 
  # mutate(FechaFirstMoraIM = if_else(fdes_ori<as.Date("2018-01-01"), NA, FechaFirstMoraIM)) %>% 
  glimpse()

sapply(bdc_wMeasures, function(x){sum(is.na(x))})
saveRDS(bdc_wMeasures, "D:/!bso/firstTimes/bdc_worstCredit_Jun2023.rds")  

####____EXPLORATORY ANALYSIS____#####
bdc_ttf <- bdc_wMeasures %>% 
  mutate(cosechaM = as.yearmon(fdes_ori)) %>% 
  mutate(cosechaY = year(fdes_ori)) %>% 
  mutate(myFirstTardio = as.yearmon(FechaFirstTardio),
         myFirstCond = as.yearmon(FechaFirstCond)) %>%
  mutate(across(starts_with("Fecha"),~as.Date(.x))) %>% 
  mutate(mesesDesde18 = case_when(esFSL==1 ~ mesesDesde18,
                                  fdes_ori>=as.Date("2018-01-01") ~ round((monDate-cosechaM)*12+1),
                                  TRUE ~ round((monDate-as.yearmon("Dic. 2017"))*12))) %>% 
  mutate(meanMoraIM_bin = case_when(meanMoraIM==0 ~ '1. 0',
                                    meanMoraIM<5 ~ '2. <5',
                                    meanMoraIM<10 ~ '3. [5-10)',
                                    meanMoraIM<15 ~ '4. [10-15)',
                                    meanMoraIM<20 ~ '5. [15-20)',
                                    meanMoraIM>=20 ~ '6. >=20')) %>% 
  mutate(Months_to_first_cond = round((myFirstCond-cosechaM)*12)) %>% 
  mutate(Months_to_first_tardio = round((myFirstTardio-cosechaM)*12)) %>% 
  mutate(Months_to_DPD1 = case_when(esFSL==1 & round((myFirst_DPD1-as.yearmon("May. 2023"))*12)>mesesDesde18 ~ mesesDesde18,
                                    esFSL==1 ~ round((myFirst_DPD1-as.yearmon("May. 2023"))*12),
                                    cosechaM<"Ene. 2018" ~ round((myFirst_DPD1-as.yearmon("Dic. 2017"))*12),
                                    TRUE ~ round((myFirst_DPD1-cosechaM)*12+1))) %>% 
  mutate(Months_to_DPD5 = case_when(esFSL==1 & round((myFirst_DPD5-as.yearmon("May. 2023"))*12)>mesesDesde18 ~ mesesDesde18,
                                    esFSL==1 ~ round((myFirst_DPD5-as.yearmon("May. 2023"))*12),
                                    cosechaM<"Ene. 2018"~ round((myFirst_DPD5-as.yearmon("Dic. 2017"))*12),
                                    TRUE ~ round((myFirst_DPD5-cosechaM)*12+1))) %>% 
  mutate(Months_to_DPD10 = case_when(esFSL==1 & round((myFirst_DPD10-as.yearmon("May. 2023"))*12)>mesesDesde18 ~ mesesDesde18,
                                    esFSL==1 ~ round((myFirst_DPD10-as.yearmon("May. 2023"))*12),
                                    cosechaM<"Ene. 2018"~ round((myFirst_DPD10-as.yearmon("Dic. 2017"))*12),
                                    TRUE ~ round((myFirst_DPD10-cosechaM)*12+1))) %>% 
  mutate(Months_to_DPD15 = case_when(esFSL==1 & round((myFirst_DPD15-as.yearmon("May. 2023"))*12)>mesesDesde18 ~ mesesDesde18,
                                    esFSL==1 ~ round((myFirst_DPD15-as.yearmon("May. 2023"))*12),
                                    cosechaM<"Ene. 2018"~ round((myFirst_DPD15-as.yearmon("Dic. 2017"))*12),
                                    TRUE ~ round((myFirst_DPD15-cosechaM)*12+1))) %>% 
  mutate(Months_to_DPD20 = case_when(esFSL==1 & round((myFirst_DPD20-as.yearmon("May. 2023"))*12)>mesesDesde18 ~ mesesDesde18,
                                    esFSL==1 ~ round((myFirst_DPD20-as.yearmon("May. 2023"))*12),
                                    cosechaM<"Ene. 2018"~ round((myFirst_DPD20-as.yearmon("Dic. 2017"))*12),
                                    TRUE ~ round((myFirst_DPD20-cosechaM)*12+1))) %>% 
  mutate(Months_to_DPD25 = case_when(esFSL==1 & round((myFirst_DPD25-as.yearmon("May. 2023"))*12)>mesesDesde18 ~ mesesDesde18,
                                    esFSL==1 ~ round((myFirst_DPD25-as.yearmon("May. 2023"))*12),
                                    cosechaM<"Ene. 2018"~ round((myFirst_DPD25-as.yearmon("Dic. 2017"))*12),
                                    TRUE ~ round((myFirst_DPD25-cosechaM)*12+1)))

bdcExp <- bdc_ttf %>% 
  select(-starts_with("myFirst")) %>% 
  mutate(Months_to_first_cond_bin = case_when(Months_to_first_cond<=3~'1. <3 meses',
                                              Months_to_first_cond<=6~'2. (3, 6] meses',
                                              Months_to_first_cond<=12~'3. (6, 12] meses',
                                              Months_to_first_cond<=24~'4. (12, 24] meses',
                                              Months_to_first_cond>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_tardio_bin = case_when(Months_to_first_tardio<=3~'1. <3 meses',
                                                Months_to_first_tardio<=6~'2. (3, 6] meses',
                                                Months_to_first_tardio<=12~'3. (6, 12] meses',
                                                Months_to_first_tardio<=24~'4. (12, 24] meses',
                                                Months_to_first_tardio>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_DPD1_bin = case_when(Months_to_DPD1<=3~'1. <3 meses',
                                                Months_to_DPD1<=6~'2. (3, 6] meses',
                                                Months_to_DPD1<=12~'3. (6, 12] meses',
                                                Months_to_DPD1<=24~'4. (12, 24] meses',
                                                Months_to_DPD1>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_DPD5_bin = case_when(Months_to_DPD5<=3~'1. <3 meses',
                                              Months_to_DPD5<=6~'2. (3, 6] meses',
                                              Months_to_DPD5<=12~'3. (6, 12] meses',
                                              Months_to_DPD5<=24~'4. (12, 24] meses',
                                              Months_to_DPD5>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_DPD10_bin = case_when(Months_to_DPD10<=3~'1. <3 meses',
                                              Months_to_DPD10<=6~'2. (3, 6] meses',
                                              Months_to_DPD10<=12~'3. (6, 12] meses',
                                              Months_to_DPD10<=24~'4. (12, 24] meses',
                                              Months_to_DPD10>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_DPD15_bin = case_when(Months_to_DPD15<=3~'1. <3 meses',
                                              Months_to_DPD15<=6~'2. (3, 6] meses',
                                              Months_to_DPD15<=12~'3. (6, 12] meses',
                                              Months_to_DPD15<=24~'4. (12, 24] meses',
                                              Months_to_DPD15>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_DPD20_bin = case_when(Months_to_DPD20<=3~'1. <3 meses',
                                              Months_to_DPD20<=6~'2. (3, 6] meses',
                                              Months_to_DPD20<=12~'3. (6, 12] meses',
                                              Months_to_DPD20<=24~'4. (12, 24] meses',
                                              Months_to_DPD20>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_DPD25_bin = case_when(Months_to_DPD25<=3~'1. <3 meses',
                                               Months_to_DPD25<=6~'2. (3, 6] meses',
                                               Months_to_DPD25<=12~'3. (6, 12] meses',
                                               Months_to_DPD25<=24~'4. (12, 24] meses',
                                               Months_to_DPD25>24~'5. >24 meses',)) %>% 
  mutate(across(c(monDate,cosechaM),~as.Date(.x,frac=1))) %>% 
  mutate(IntMoraSF_bin = cut(IntMoraSF, breaks = c(-Inf,0,0.2,0.4,0.6,0.8,1), include.lowest=T,right=T,
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
  mutate(across(ends_with("_bin"),~as.character(.x))) %>% 
  mutate(Alerta = case_when(Ult12MesesCondonado>=12 & Ult12MesesPagoTardio>=12 ~ '12x12',
                            Ult12MesesCondonado>=11 & Ult12MesesPagoTardio>=11 ~ '11x11',
                            Ult12MesesCondonado>=10 & Ult12MesesPagoTardio>=10 ~ '10x10',
                            Ult12MesesCondonado>=9 & Ult12MesesPagoTardio>=9 ~ '9x9',
                            Ult12MesesCondonado>=8 & Ult12MesesPagoTardio>=8 ~ '8x8',
                            Ult12MesesCondonado>=7 & Ult12MesesPagoTardio>=7 ~ '7x7',
                            Ult12MesesCondonado>=6 & Ult12MesesPagoTardio>=6 ~ '6x6',
                            Ult12MesesCondonado>=5 & Ult12MesesPagoTardio>=5 ~ '5x5',
                            Ult12MesesCondonado>=4 & Ult12MesesPagoTardio>=4 ~ '4x4',
                            Ult12MesesCondonado>=3 & Ult12MesesPagoTardio>=3 ~ '3x3',
                            Ult12MesesCondonado>=2 & Ult12MesesPagoTardio>=2 ~ '2x2',
                            Ult12MesesCondonado>=1 & Ult12MesesPagoTardio>=1 ~ '1x1',)) %>% 
  mutate(OpsMora = ifelse(DIASMORA>0, 1, 0))

bdcExp %>% dplyr::filter(Ult12MesesCondonado>=4 & Ult12MesesPagoTardio>=4) %>% 
  summarise(n(), sum(saldous))
bdcExp <- bdcExp %>% 
  select(-CAEDEC_DEST, -CIU, -CPOP, -OBJETO_CRED)
write_xlsx(bdcExp, "D:/!bso/firstTimes/bdc_ttf_Jun2023_v5.xlsx")

