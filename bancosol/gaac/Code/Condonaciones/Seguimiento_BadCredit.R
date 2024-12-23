####____CARGA DE LIBRERIAS Y FUNCIONES_____####
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

cases <- function(quant,levs,values){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- rep(NA,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.5)
####____SEGUIMIENTO DE ALERTA ROJA____#####
#Partimos de la base de alertas rojas previa
mybdc <- c("May2023")
RA <- read_xlsx(paste0('D:/!bso/mph/condonados/TardioCond_',mybdc,'_v3.xlsx'), sheet = "Datos") 
RedAlert <- RA %>% 
  # dplyr::filter(Alerta == "4. Roja") %>% 
  select(CTACLIENTE = Cuenta, OPERACION=Operacion, Alerta,  
         UltimoMesPagoTardio, UltimoMesCondonado = COND_ULT_MES,
         Instancias_UR = CantidadPagosTardios, NCONDONACIONES) %>% 
  mutate(NXN = case_when(NCONDONACIONES>=12 & Instancias_UR>=12 ~ '12x12',
                         NCONDONACIONES>=11 & Instancias_UR>=11 ~ '11x11',
                         NCONDONACIONES>=10 & Instancias_UR>=10 ~ '10x10',
                         NCONDONACIONES>=9 & Instancias_UR>=9 ~ '9x9',
                         NCONDONACIONES>=8 & Instancias_UR>=8 ~ '8x8',
                         NCONDONACIONES>=7 & Instancias_UR>=7 ~ '7x7',
                         NCONDONACIONES>=6 & Instancias_UR>=6 ~ '6x6',
                         NCONDONACIONES>=5 & Instancias_UR>=5 ~ '5x5',
                         NCONDONACIONES>=4 & Instancias_UR>=4 ~ '4x4',
                         NCONDONACIONES>=3 & Instancias_UR>=3 ~ '3x3',
                         NCONDONACIONES>=2 & Instancias_UR>=2 ~ '2x2',
                         NCONDONACIONES>=1 & Instancias_UR>=1 ~ '1x1',))
bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mybdc,'.rds')) %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>% 
  mutate(ESTADO = case_when(ctaCont %in% c('131','135')~'VIGENTE',
                            ctaCont %in% c('133','136')~'VENCIDA',
                            ctaCont %in% c('134','137')~'EJECUCION',
                            ctaCont == '865'~'CASTIGADA',
                            TRUE~'OTROS')) %>% 
  mutate(EnMora = ifelse(DIASMORA>0, 'En Mora', 'Pendiente')) %>% 
  select(monDate,CTACLIENTE, OPERACION, CI, saldous, ESTADO, ctaCont, FDESEMBOLSO, 
         OPERACION_ORI_REF, DIASMORA, EnMora)

RA2 <- bdc %>% 
  left_join(RedAlert, by=c("CTACLIENTE","OPERACION")) 

#####____BUSQUEDA EN NUEVA BDC____####
mynewbdc <- c("Jul2023")
RA_new <- read_xlsx(paste0('D:/!bso/mph/condonados/TardioCond_',mynewbdc,'.xlsx')) 
RedAlert_new <- RA_new %>% 
  # dplyr::filter(Alerta == "4. Roja") %>% 
  select(CTACLIENTE = Cuenta, OPERACION=Operacion, Alerta,  
         UltimoMesPagoTardio=Ultimo_mes, UltimoMesCondonado = COND_ULT_MES,
         Instancias_UR, NCONDONACIONES) %>% 
  mutate(NXN = case_when(NCONDONACIONES>=12 & Instancias_UR>=12 ~ '12x12',
                         NCONDONACIONES>=11 & Instancias_UR>=11 ~ '11x11',
                         NCONDONACIONES>=10 & Instancias_UR>=10 ~ '10x10',
                         NCONDONACIONES>=9 & Instancias_UR>=9 ~ '9x9',
                         NCONDONACIONES>=8 & Instancias_UR>=8 ~ '8x8',
                         NCONDONACIONES>=7 & Instancias_UR>=7 ~ '7x7',
                         NCONDONACIONES>=6 & Instancias_UR>=6 ~ '6x6',
                         NCONDONACIONES>=5 & Instancias_UR>=5 ~ '5x5',
                         NCONDONACIONES>=4 & Instancias_UR>=4 ~ '4x4',
                         NCONDONACIONES>=3 & Instancias_UR>=3 ~ '3x3',
                         NCONDONACIONES>=2 & Instancias_UR>=2 ~ '2x2',
                         NCONDONACIONES>=1 & Instancias_UR>=1 ~ '1x1',))
  
bdcNew <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mynewbdc,'.rds')) %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>% 
  mutate(ESTADO = case_when(ctaCont %in% c('131','135')~'VIGENTE',
                            ctaCont %in% c('133','136')~'VENCIDA',
                            ctaCont %in% c('134','137')~'EJECUCION',
                            ctaCont == '865'~'CASTIGADA',
                            TRUE~'OTROS')) %>% 
  mutate(EnMora = ifelse(DIASMORA>0, 'En Mora', 'Pendiente')) %>% 
  select(monDate,CTACLIENTE, OPERACION, CI, saldous, ESTADO, ctaCont, FDESEMBOLSO, 
         OPERACION_ORI_REF, DIASMORA, EnMora)

RA2_new <- bdcNew %>% 
  left_join(RedAlert_new, by=c("CTACLIENTE","OPERACION")) 

AutenticosDesembolsos <- bdcNew %>% 
  dplyr::filter(as.yearmon(FDESEMBOLSO)==monDate) %>% 
  dplyr::filter(ctaCont=='131') %>% 
  dplyr::filter(OPERACION_ORI_REF==0)

followRA2 <- dplyr::filter(RA2, !is.na(NXN)) %>% 
  left_join(RA2_new, by=c("CTACLIENTE","OPERACION"), 
            suffix=c("_old","_new")) %>% 
  mutate(OpCancelada = ifelse(is.na(monDate_new), 1, 0)) %>% 
  mutate(CambioFechaDes = ifelse(FDESEMBOLSO_old!=FDESEMBOLSO_new & OpCancelada==0, 1, 0)) %>% 
  mutate(FueReprog = ifelse(CambioFechaDes==1 & ctaCont_new %in% c('131','135') & OpCancelada==0, 1, 0)) %>% 
  mutate(FueRefin = ifelse(OPERACION %in% bdcNew$OPERACION_ORI_REF, 1, 0)) %>% 
  mutate(FueCast = ifelse(ctaCont_old!='865' & ctaCont_new=='865' & OpCancelada==0, 1, 0)) %>% 
  mutate(NuevoDesembolso = ifelse(CTACLIENTE %in% AutenticosDesembolsos$CTACLIENTE, 1, 0)) %>% 
  mutate(across(starts_with("monDate"),~as.Date(.x, frac=1))) 
#Si hubiera cambios de fecha de Desembolso que no se explican por reprog, averiguar a qué se debe
table(followRA2$CambioFechaDes, followRA2$FueReprog)
#El siguiente check debería tener 0 observaciones.
checkReprog <- followRA2 %>% 
  dplyr::filter((CambioFechaDes==0 & FueReprog==1) | (CambioFechaDes==1 & FueReprog==0))

write_xlsx(followRA2, "D:/!bso/condonaciones/Seguimiento_BadCredit_Jul2023_v3.xlsx")

####____SEGUIMIENTO ANCLAJE MAYO 2023____####
mybdc <- c("May2023")
RA <- read_xlsx(paste0('D:/!bso/mph/condonados/TardioCond_',mybdc,'_v3.xlsx'), sheet = "Datos") 
RedAlert <- RA %>% 
  # dplyr::filter(Alerta == "4. Roja") %>% 
  rename(CTACLIENTE = Cuenta, OPERACION=Operacion,   
         UltimoMesCondonado = COND_ULT_MES,
         Instancias_UR = CantidadPagosTardios) %>% 
  mutate(NXN = case_when(NCONDONACIONES>=12 & Instancias_UR>=12 ~ '12x12',
                             NCONDONACIONES>=11 & Instancias_UR>=11 ~ '11x11',
                             NCONDONACIONES>=10 & Instancias_UR>=10 ~ '10x10',
                             NCONDONACIONES>=9 & Instancias_UR>=9 ~ '9x9',
                             NCONDONACIONES>=8 & Instancias_UR>=8 ~ '8x8',
                             NCONDONACIONES>=7 & Instancias_UR>=7 ~ '7x7',
                             NCONDONACIONES>=6 & Instancias_UR>=6 ~ '6x6',
                             NCONDONACIONES>=5 & Instancias_UR>=5 ~ '5x5',
                             NCONDONACIONES>=4 & Instancias_UR>=4 ~ '4x4',
                             NCONDONACIONES>=3 & Instancias_UR>=3 ~ '3x3',
                             NCONDONACIONES>=2 & Instancias_UR>=2 ~ '2x2',
                             NCONDONACIONES>=1 & Instancias_UR>=1 ~ '1x1',))

bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mybdc,'.rds')) %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>% 
  mutate(ESTADO = case_when(ctaCont %in% c('131','135')~'VIGENTE',
                            ctaCont %in% c('133','136')~'VENCIDA',
                            ctaCont %in% c('134','137')~'EJECUCION',
                            ctaCont == '865'~'CASTIGADA',
                            TRUE~'OTROS')) %>% 
  mutate(EnMora = ifelse(DIASMORA>0, 'En Mora', 'Pendiente')) %>% 
  select(monDate,CTACLIENTE, OPERACION, saldous, ESTADO, FDESEMBOLSO, OPERACION_ORI_REF,
         DIASMORA, EnMora)

RA2 <- bdc %>% 
  left_join(RedAlert, by=c("CTACLIENTE","OPERACION"))

ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Jul23.rds")
checkpt <- ptFull %>% 
  group_by(myPago) %>% 
  summarise(NObs= n(),NOps=n_distinct(Operacion), check=NObs==NOps)

pt_grouped <- ptFull %>% 
  dplyr::filter(myPago>"May. 2018" & myPago <="Jul. 2023") %>% 
  mutate(UltMesPagoTardio = ifelse(myPago == "Jul. 2023",1,0)) %>% 
  mutate(Ult12MesesPagoTardio = ifelse(myPago<="Jul. 2023" & myPago >= "Jun. 2022",1,0)) %>% 
  group_by(Operacion) %>% 
  mutate(Ult12MesesPagoTardio = sum(Ult12MesesPagoTardio)) %>% 
  ungroup() %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(FechaFirstTardio = min(FechaPago),
            MesesPagoTardio = n(),
            UltMesPagoTardio = max(UltMesPagoTardio),
            UltMesesPagoTardio = max(Ult12MesesPagoTardio)) %>% 
  ungroup()

condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Jul2023.rds')

cond_clean <- condFull %>% 
  select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int, CondCap_USD = Cond_Cap) %>%
  mutate(myCond = as.yearmon(Fecha)) %>% 
  group_by(myCond, Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(Fecha),
            across(starts_with("Cond"),~sum(.x))) %>% 
  ungroup()

cond_grouped <- cond_clean %>% 
  mutate(UltMesCondonado = ifelse(myCond == "Jul. 2023",1,0)) %>% 
  mutate(Ult12MesesCondonado = ifelse(myCond<="Jul. 2023" & myCond >= "Jun. 2022",1,0)) %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(FechaFirstCond),
            MesesCondonado = n(),
            UltMesCondonado = max(UltMesCondonado),
            UltMesesCondonado = sum(Ult12MesesCondonado)) %>% 
  ungroup()

RedAlert2 <- followRA2 %>% 
  left_join(pt_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(cond_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  mutate(NXN_anchored = case_when(UltMesesCondonado>=14 & UltMesesPagoTardio>=14 ~ '14x14',
                         UltMesesCondonado>=13 & UltMesesPagoTardio>=13 ~ '13x13',
                         UltMesesCondonado>=12 & UltMesesPagoTardio>=12 ~ '12x12',
                         UltMesesCondonado>=11 & UltMesesPagoTardio>=11 ~ '11x11',
                         UltMesesCondonado>=10 & UltMesesPagoTardio>=10 ~ '10x10',
                         UltMesesCondonado>=9 & UltMesesPagoTardio>=9 ~ '9x9',
                         UltMesesCondonado>=8 & UltMesesPagoTardio>=8 ~ '8x8',
                         UltMesesCondonado>=7 & UltMesesPagoTardio>=7 ~ '7x7',
                         UltMesesCondonado>=6 & UltMesesPagoTardio>=6 ~ '6x6',
                         UltMesesCondonado>=5 & UltMesesPagoTardio>=5 ~ '5x5',
                         UltMesesCondonado>=4 & UltMesesPagoTardio>=4 ~ '4x4',
                         UltMesesCondonado>=3 & UltMesesPagoTardio>=3 ~ '3x3',
                         UltMesesCondonado>=2 & UltMesesPagoTardio>=2 ~ '2x2',
                         UltMesesCondonado>=1 & UltMesesPagoTardio>=1 ~ '1x1',))
RedAlert2 <- RedAlert2 %>% 
  select(-starts_with("TOTAL"))
write_xlsx(RedAlert2, "D:/!bso/condonaciones/Seguimiento_badCredit_May23_a_Jul23.xlsx")
