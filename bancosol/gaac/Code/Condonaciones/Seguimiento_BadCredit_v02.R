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
library(arrow)
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
#####____REPRODUCING BAD CREDIT WITH HISTORIC PT AND COND____#####
Clientes <- readRDS("D:/!bso/features/Clientes_Ene15Oct23.rds") %>% 
  select(CTACLIENTE, OPERACION, fdesOri=fdes)
ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Oct23.rds")
condFull <- read_parquet('D:/!bso/condonaciones/CondFull.parquet')
cond_clean <- condFull %>% 
  dplyr::filter(Fecha>=as.Date("2019-01-01")) %>% 
  select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int, CondCap_USD = Cond_Cap) %>%
  mutate(myCond = as.yearmon(Fecha)) %>% 
  group_by(myCond, Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(Fecha),
            across(starts_with("Cond"), ~sum(.x))) %>% 
  ungroup()
generatePTk <- function(x, mini, mfin){
  x %>% 
    dplyr::filter(myPago>=as.yearmon(mini) & myPago<=mfin) %>% 
    group_by(Operacion) %>% 
    mutate(Ult12MesesPagoTardio = n()) %>% 
    ungroup() %>% 
    mutate(Cierre = as.yearmon(mfin)) %>% 
    group_by(Cierre, Cuenta, Operacion) %>% 
    summarise(CantPT12Meses = max(Ult12MesesPagoTardio)) %>% 
    ungroup()
}
generateCondk <- function(x, mini, mfin){
  x <- cond_clean %>% 
    dplyr::filter(myCond>=as.yearmon(mini) & myCond<=mfin) %>% 
    mutate(Cierre = as.yearmon(mfin)) %>% 
    group_by(Cierre, Cuenta, Operacion) %>% 
    summarise(CantCond12Meses = n()) %>% 
    ungroup()
}

pt_grouped <- generatePTk(ptFull, "Jun. 2022", "May. 2023")
cond_grouped <- generateCondk(cond_clean, "Jun. 2022", "May. 2023")

bdc <- readRDS("D:/!bso/girCartera/rds/ec_May2023.rds")

bdcOld <- bdc %>% 
  left_join(Clientes, by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER, "MIGR"), 1, 0)) %>% 
  mutate(fdesOri = if_else(esFSL==1 , as.Date("2023-05-31"), fdesOri)) %>% 
  mutate(cosechaOri = ifelse(year(fdesOri)<2017, "<2017", as.character(year(fdesOri)))) %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  mutate(ESTADO = case_when(ctaCont %in% c('131','135')~'VIGENTE',
                            ctaCont %in% c('133','136')~'VENCIDA',
                            ctaCont %in% c('134','137')~'EJECUCION',
                            ctaCont == '865'~'CASTIGADA',
                            TRUE~'OTROS')) %>% 
  select(monDate, CTACLIENTE, OPERACION, CI, Sucursal, saldous, ESTADO, ctaCont, 
         FDESEMBOLSO, cosechaOri, fdesOri, esFSL, OPERACION_ORI_REF, DIASMORA) %>% 
  mutate(EnMora = ifelse(DIASMORA>0, 'En Mora', 'Pendiente')) %>% 
  left_join(pt_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(cond_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  select(-starts_with("Cierre")) %>% 
  mutate(NXN = case_when(CantPT12Meses>=12 & CantCond12Meses>=12 ~ '12x12',
                         CantPT12Meses>=11 & CantCond12Meses>=11 ~ '11x11',
                         CantPT12Meses>=10 & CantCond12Meses>=10 ~ '10x10',
                         CantPT12Meses>=9 & CantCond12Meses>=9 ~ '09x09',
                         CantPT12Meses>=8 & CantCond12Meses>=8 ~ '08x08',
                         CantPT12Meses>=7 & CantCond12Meses>=7 ~ '07x07',
                         CantPT12Meses>=6 & CantCond12Meses>=6 ~ '06x06',
                         CantPT12Meses>=5 & CantCond12Meses>=5 ~ '05x05',
                         CantPT12Meses>=4 & CantCond12Meses>=4 ~ '04x04',
                         CantPT12Meses>=3 & CantCond12Meses>=3 ~ '03x03',
                         CantPT12Meses>=2 & CantCond12Meses>=2 ~ '02x02',
                         CantPT12Meses>=1 & CantCond12Meses>=1 ~ '01x01',))
#Cuadre
bdcOld %>% 
  dplyr::filter(saldous!=0) %>% 
  group_by(NXN) %>% 
  summarise(S=sum(saldous),N=n()) %>% 
  adorn_totals("row")

saveRDS(bdcOld, "D:/!bso/mph/condonados/TardioCond_May2023.rds")
write_xlsx(bdcOld,"D:/!bso/mph/condonados/TardioCond_May2023_v6.xlsx")
bdcOld <- read_xlsx("D:/!bso/mph/condonados/TardioCond_May2023_v6.xlsx") %>% 
  mutate(monDate=as.yearmon(monDate))
####____RED ALERT FOR NEW MONTH____####
longmonth <- "Oct. 2023"
longmonth12 <- "Nov. 2022"
shortmonth <- str_replace(longmonth, ". ", "")
shortmonth12 <- str_replace(longmonth12, ". ", "")
pt_grouped_anchored <- generatePTk(ptFull, "Jun. 2022", longmonth)
cond_grouped_anchored <- generateCondk(ptFull, "Jun. 2022", longmonth)

Seguimiento <-  bdcOld %>% 
  left_join(pt_grouped_anchored, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion"),
            suffix=c("","_anchor")) %>% 
  left_join(cond_grouped_anchored, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion"),
            suffix=c("","_anchor")) %>%  
  mutate(NXN_anchored = case_when(CantPT12Meses_anchor>=13 & CantCond12Meses_anchor>=13 ~ '13x13',
                         CantPT12Meses_anchor>=12 & CantCond12Meses_anchor>=12 ~ '12x12',
                         CantPT12Meses_anchor>=11 & CantCond12Meses_anchor>=11 ~ '11x11',
                         CantPT12Meses_anchor>=10 & CantCond12Meses_anchor>=10 ~ '10x10',
                         CantPT12Meses_anchor>=9 & CantCond12Meses_anchor>=9 ~ '09x09',
                         CantPT12Meses_anchor>=8 & CantCond12Meses_anchor>=8 ~ '08x08',
                         CantPT12Meses_anchor>=7 & CantCond12Meses_anchor>=7 ~ '07x07',
                         CantPT12Meses_anchor>=6 & CantCond12Meses_anchor>=6 ~ '06x06',
                         CantPT12Meses_anchor>=5 & CantCond12Meses_anchor>=5 ~ '05x05',
                         CantPT12Meses_anchor>=4 & CantCond12Meses_anchor>=4 ~ '04x04',
                         CantPT12Meses_anchor>=3 & CantCond12Meses_anchor>=3 ~ '03x03',
                         CantPT12Meses_anchor>=2 & CantCond12Meses_anchor>=2 ~ '02x02',
                         CantPT12Meses_anchor>=1 & CantCond12Meses_anchor>=1 ~ '01x01',)) %>% 
  mutate(PermDet = ifelse(NXN==NXN_anchored, 'Permanencia','Deterioro')) %>% 
  select(-starts_with("Cierre"), -ends_with("_anchor"))
#READ ALERT FOR NEW MONTH
pt_groupedNew <- generatePTk(ptFull, longmonth12, longmonth)
cond_groupedNew <- generateCondk(ptFull, longmonth12, longmonth)

bdcNew <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",shortmonth,".rds")) %>% 
  left_join(Clientes, by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER, "MIGR"), 1, 0)) %>% 
  mutate(fdesOri = if_else(esFSL==1 , as.Date("2023-05-31"), fdesOri)) %>% 
  mutate(cosechaOri = ifelse(year(fdesOri)<2017, "<2017", as.character(year(fdesOri)))) %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>% #SE CONSIDERAN CASTIGOS
  mutate(ESTADO = case_when(ctaCont %in% c('131','135')~'VIGENTE',
                            ctaCont %in% c('133','136')~'VENCIDA',
                            ctaCont %in% c('134','137')~'EJECUCION',
                            ctaCont == '865'~'CASTIGADA',
                            TRUE~'OTROS')) %>%
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER, "MIGR"), 1 ,0)) %>% 
  select(monDate, CTACLIENTE, OPERACION, CI, Sucursal, AGENCIA, NOMBRE_AGENCIA, ASESOR, NOMBRE_ASESOR,
         saldous, ESTADO, ctaCont, esFSL,
         FDESEMBOLSO, cosechaOri, fdesOri, esFSL, OPERACION_ORI_REF, DIASMORA) %>% #PARA VERSION ANTERIOR QUITAR AGENCIA Y NOMBRE_AGENCIA
  mutate(EnMora = ifelse(DIASMORA>0, 'En Mora', 'Pendiente')) %>% 
  left_join(pt_groupedNew, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(cond_groupedNew, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  select(-starts_with("Cierre")) %>% 
  mutate(NXN = case_when(CantPT12Meses>=12 & CantCond12Meses>=12 ~ '12x12',
                         CantPT12Meses>=11 & CantCond12Meses>=11 ~ '11x11',
                         CantPT12Meses>=10 & CantCond12Meses>=10 ~ '10x10',
                         CantPT12Meses>=9 & CantCond12Meses>=9 ~ '09x09',
                         CantPT12Meses>=8 & CantCond12Meses>=8 ~ '08x08',
                         CantPT12Meses>=7 & CantCond12Meses>=7 ~ '07x07',
                         CantPT12Meses>=6 & CantCond12Meses>=6 ~ '06x06',
                         CantPT12Meses>=5 & CantCond12Meses>=5 ~ '05x05',
                         CantPT12Meses>=4 & CantCond12Meses>=4 ~ '04x04',
                         CantPT12Meses>=3 & CantCond12Meses>=3 ~ '03x03',
                         CantPT12Meses>=2 & CantCond12Meses>=2 ~ '02x02',
                         CantPT12Meses>=1 & CantCond12Meses>=1 ~ '01x01',)) %>% 
  mutate(SaldoAlerta = ifelse(NXN %in% c('04x04','05x05','06x06','07x07','08x08','09x09',
                                         '10x10','11x11','12x12'), saldous, 0)) %>% 
  mutate(OpsAlerta = ifelse(NXN %in% c('04x04','05x05','06x06','07x07','08x08','09x09',
                                         '10x10','11x11','12x12'), 1, 0))
# write_xlsx(bdcNew, "D:/!bso/mph/condonados/TardioCond_Sep2023.xlsx")
write_xlsx(bdcNew, paste0("D:/!bso/mph/condonados/TardioCond_",shortmonth,".xlsx"))

AutenticosDesembolsos <- bdcNew %>% 
  dplyr::filter(as.yearmon(FDESEMBOLSO)==monDate) %>% 
  dplyr::filter(ctaCont=='131') %>% 
  dplyr::filter(OPERACION_ORI_REF==0)

# OPSREF <- bdcNew %>% 
#   dplyr::filter(OPERACION_ORI_REF!=0) %>% 
#   select(OPERACION_ORI_REF, OP=OPERACION)
Seguimiento2 <- Seguimiento %>% 
  left_join(bdcNew, by=c("CTACLIENTE","OPERACION"), suffix=c("_old","_new")) %>%
  # left_join(OPSREF, by=c("OPERACION"="OPERACION_ORI_REF")) %>% 
  mutate(OpCancelada = ifelse(is.na(monDate_new), 1, 0)) %>% 
  mutate(CambioFechaDes = ifelse(FDESEMBOLSO_old!=FDESEMBOLSO_new & OpCancelada==0, 1, 0)) %>% 
  mutate(FueReprog = ifelse(CambioFechaDes==1 & ctaCont_new %in% c('135','136','137') & OpCancelada==0, 1, 0)) %>% #ANTES ESTABA ctaCont_new %in% c('131','135')
  # mutate(FueRefin = ifelse(OPERACION != OP | (OPERACION==OP & OPERACION_ORI_REF_old!=OP), 1, 0)) %>% #Ante: única condición OPERACION %in% bdcNew$OPERACION_ORI_REF
  mutate(FueRefin = case_when(OPERACION_ORI_REF_old==0 & OPERACION %in% bdcNew$OPERACION_ORI_REF ~ 1,
                              OPERACION_ORI_REF_old!=0 & OPERACION %in% bdcNew$OPERACION_ORI_REF
                              & !(OPERACION %in% bdcNew$OPERACION) ~ 1,
                              OPERACION_ORI_REF_old!=0 & OPERACION_ORI_REF_old!=OPERACION
                              & OPERACION %in% bdcNew$OPERACION_ORI_REF ~1,
                              TRUE~0)) %>% 
  mutate(FueCast = ifelse(ctaCont_old!='865' & ctaCont_new=='865' & OpCancelada==0, 1, 0)) %>% 
  mutate(NuevoDesembolso = ifelse(CTACLIENTE %in% AutenticosDesembolsos$CTACLIENTE, 1, 0)) %>% 
  mutate(across(starts_with("monDate"),~as.Date(.x, frac=1))) 

#Si hubiera cambios de fecha de Desembolso que no se explican por reprog, averiguar a qué se debe
table(Seguimiento2$CambioFechaDes, Seguimiento2$FueReprog, Seguimiento2$FueRefin)

x <- Seguimiento2 %>% 
  dplyr::filter(CambioFechaDes==1 & FueReprog==0 & FueRefin==0) 
#El siguiente check debería tener 0 observaciones.
checkReprog <- Seguimiento %>% 
  dplyr::filter((CambioFechaDes==0 & FueReprog==1) | (CambioFechaDes==1 & FueReprog==0))
  
write_xlsx(Seguimiento2, "D:/!bso/condonaciones/Seguimiento_BadCredit_Oct2023.xlsx")
