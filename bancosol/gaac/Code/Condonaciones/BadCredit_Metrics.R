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
longmonth <- "Jun. 2023"
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
  select(monDate, CTACLIENTE, OPERACION, CI, MODULO, TIPO_OPER, Sucursal, NOMBRE_AGENCIA, 
         tipoCred, MONTO, rangom, CAEDEC_DEST, CIU, Sector_Actividad, Sector_Destino, PLAZODIAS, PLAZODIAS_bin,
         FDESEMBOLSO, CALIFICACION, DIASMORA, OBJETO_CRED, DESC_SEGMERC, CPOP, SECTOR_CARTERA, 
         ctaCont, saldous, montous, par0) 
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
  dplyr::filter(myPago>"May. 2018" & myPago <="Jun. 2023") %>% 
  mutate(UltMesPagoTardio = ifelse(myPago == "Jun. 2023",1,0)) %>% 
  mutate(Ult12MesesPagoTardio = ifelse(myPago<="Jun. 2023" & myPago >= "Jul. 2022",1,0)) %>% 
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
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Jun2023.rds')
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
  mutate(UltMesCondonado = ifelse(myCond == "Jun. 2023",1,0)) %>% 
  mutate(Ult12MesesCondonado = ifelse(myCond<="Jun. 2023" & myCond >= "Jul. 2022",1,0)) %>% 
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
  mutate(UltMesNoVigSF = ifelse(FECHA=="may. 2023" & ESTADO_LAST_SF %in% c(2,3,4), 1, 0)) %>% 
  mutate(badCredit = ifelse(ESTADO_HIST_SF %in% c(2,3,4),1,0)) %>% 
  # mutate(worstCredit = ifelse(ESTADO_HIST_SF == 4,1,0)) %>% 
  summarise(UltMesNoVigSF = max(UltMesNoVigSF),
            VecesNoVigSF = sum(badCredit), 
            MesesSF = n(),
            IntMoraSF = VecesNoVigSF/MesesSF)#Revisar que pasa si un cliente tiene mpas de un crédito

remove(list=c("ptFull","condFull","HistCalif"))
####____HISTORIA EN BSO____####
dfOps <- readRDS('D:/!bso/features/Historial_Operaciones.rds') %>% 
  mutate(tuvoMora = ifelse(DIASMORA>0, 1, 0)) %>% 
  glimpse()

checkCierre <- dfOps %>% 
  group_by(monDate) %>% 
  summarise(NObs=n(), NOps=n_distinct(OPERACION), check=NObs==NOps)  

measuresMora <- dfOps %>% 
  select(CTACLIENTE, OPERACION, monDate, fdes, DIASMORA, MONTOUS, tuvoMora) %>% 
  group_by(CTACLIENTE, OPERACION) %>% #Se agrupa por operación de cada cliente
  summarise(meanMora = mean(DIASMORA, na.rm = T),
            VecesMoraBSO = sum(tuvoMora),
            MesesBSO = n(),
            IntMoraBSO = VecesMoraBSO/MesesBSO,
            fdes_ori = min(fdes),#Fecha más antigua para reprogramadas (o refin de diferidas)
            lastM = as.Date(max(monDate), frac = 1)) %>% #último mes en las bdc
  ungroup() %>% 
  mutate(loanDays = as.integer(lastM - fdes_ori)) #días de préstamo

FirstMora <- dfOps %>% 
  select(monDate, CTACLIENTE, OPERACION, DIASMORA, tuvoMora) %>% 
  dplyr::filter(tuvoMora>0) %>% 
  group_by(CTACLIENTE, OPERACION) %>% #Se agrupa por operación de cada cliente
  summarise(FechaFirstMora = min(monDate)) 

remove(list=c("dfOps"))
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

####____JOIN EVERYTHING____####
glimpse(pt_grouped)
glimpse(cond_grouped)
glimpse(sf_grouped)
glimpse(measuresMora)
glimpse(FirstMora)
glimpse(measuresMoraIM)
glimpse(FirstMoraIM)

bdc_wMeasures <- bdc %>% 
  left_join(pt_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(cond_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(sf_grouped, by="CTACLIENTE") %>% 
  left_join(measuresMora, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(FirstMora, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(measuresMoraIM, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(FirstMoraIM, by=c("CTACLIENTE","OPERACION")) %>% 
  replace_na(list(UltMesPagoTardio=0, Ult12MesesPagoTardio=0, UltMesCondonado=0, 
                  Ult12MesesCondonado=0, UltMesNoVigSF=0, totalBC=0, totalWC=0,
                  IntMoraIM = 0, IntMoraSF = 0, IntMoraBSO=0, meanMora=0, meanMoraIM_cl=0, 
                  tuvoMora_pct=0)) %>% 
  ungroup() %>% 
  mutate(FechaFirstTardio = if_else(fdes_ori<as.Date("2018-06-01"), NA, FechaFirstTardio)) %>% 
  mutate(FechaFirstCond = if_else(fdes_ori<as.Date("2019-01-01"), NA, FechaFirstCond)) %>% 
  mutate(FechaFirstMora = if_else(fdes_ori<as.Date("2015-01-01"), NA, FechaFirstMora)) %>% 
  mutate(FechaFirstMoraIM = if_else(is.na(FechaFirstMoraIM), MesFirstMoraIM, FechaFirstMoraIM)) %>% 
  mutate(FechaFirstMoraIM = if_else(fdes_ori<as.Date("2018-01-01"), NA, FechaFirstMoraIM)) %>% 
  select(-MesFirstMoraIM)
  
sapply(bdc_wMeasures, function(x){sum(is.na(x))})
saveRDS(bdc_wMeasures, "D:/!bso/firstTimes/bdc_worstCredit_Jun2023.rds")  

####____EXPLORATORY ANALYSIS____#####
bdc_ttf <- bdc_wMeasures %>% 
  mutate(cosechaM = as.yearmon(fdes_ori)) %>% 
  mutate(cosechaY = year(fdes_ori)) %>% 
  mutate(myFirstTardio = as.yearmon(FechaFirstTardio),
         myFirstCond = as.yearmon(FechaFirstCond),
         myFirstMora = as.yearmon(FechaFirstMora),
         myFirstMoraIM = as.yearmon(FechaFirstMoraIM)) %>%
  mutate(across(starts_with("Fecha"),~as.Date(.x))) %>% 
  mutate(Days_to_first_cond = as.numeric(FechaFirstCond-fdes_ori)) %>% 
  # mutate(dif =(myFirstCond-cosechaM)*12) %>% 
  mutate(Months_to_first_cond = round((myFirstCond-cosechaM)*12)) %>% 
  mutate(Days_to_first_tardio = as.numeric(FechaFirstTardio-fdes_ori)) %>% 
  mutate(Months_to_first_tardio = round((myFirstTardio-cosechaM)*12)) %>% 
  mutate(Days_to_first_mora = as.numeric(FechaFirstMora-fdes_ori)) %>% 
  mutate(Days_to_first_mora_IM = as.numeric(FechaFirstMoraIM-fdes_ori)) 

length(which(bdc_ttf$Days_to_first_mora<bdc_ttf$Days_to_first_mora_IM)) #Fix these as they come from missing FVEN_ULPAGO 
bdc_ttf <- bdc_ttf %>% 
  mutate(Days_to_first_mora_IM = ifelse(Days_to_first_mora<Days_to_first_mora_IM, Days_to_first_mora, Days_to_first_mora_IM)) %>% 
  mutate(Months_to_first_mora = round((myFirstMora-cosechaM)*12)) %>% 
  mutate(Months_to_first_moraIM = round((myFirstMoraIM-cosechaM)*12)) 

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
  mutate(Months_to_first_mora_bin = case_when(Months_to_first_mora<=3~'1. <3 meses',
                                              Months_to_first_mora<=6~'2. (3, 6] meses',
                                              Months_to_first_mora<=12~'3. (6, 12] meses',
                                              Months_to_first_mora<=24~'4. (12, 24] meses',
                                              Months_to_first_mora>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_moraIM_bin = case_when(Months_to_first_moraIM<=3~'1. <3 meses',
                                                Months_to_first_moraIM<=6~'2. (3, 6] meses',
                                                Months_to_first_moraIM<=12~'3. (6, 12] meses',
                                                Months_to_first_moraIM<=24~'4. (12, 24] meses',
                                                Months_to_first_moraIM>24~'5. >24 meses',)) %>% 
  mutate(across(c(monDate,FechaFirstMora, cosechaM),~as.Date(.x,frac=1))) %>% 
  mutate(IntMoraSF_bin = cut(IntMoraSF, breaks = c(0,0.2,0.4,0.6,0.8,1), include.lowest=T,right=T)) %>% 
  mutate(IntMoraBSO_bin = cut(IntMoraBSO, breaks = c(0,0.2,0.4,0.6,0.8,1), include.lowest=T,right=T)) %>% 
  mutate(IntMoraIM_bin = cut(IntMoraIM, breaks = c(0,0.2,0.4,0.6,0.8,1), include.lowest=T,right=T)) %>% 
  mutate(across(starts_with("IntMora"),~as.character(.x))) %>% 
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
                            Ult12MesesCondonado>=1 & Ult12MesesPagoTardio>=1 ~ '1x1',))

bdcExp %>% dplyr::filter(Ult12MesesCondonado>=4 & Ult12MesesPagoTardio>=4) %>% 
  summarise(n(), sum(saldous))
write_xlsx(bdcExp, "D:/!bso/firstTimes/bdc_ttf_Jun2023_v2.xlsx")

pt_gph <- bdc_ttf %>% 
  dplyr::filter()
ggplot(bdc_ttf, aes(x=))  
  