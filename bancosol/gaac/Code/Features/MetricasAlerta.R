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

####____FIXING MONTHS____####
longmonth <- "Dic. 2022"
longmonth1 <- "Nov. 2022"
longmonth12 <- "Ene. 2022"
shortmonth <- str_replace(longmonth,". ","")
shortmonth1 <- str_replace(longmonth,". ","")
shortmonth12 <- str_replace(longmonth12,". ","")

####____PAGOS TARDIOS Y CONDONACIONES____####
ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Sep23.rds")
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Sep2023.rds')
cond_clean <- condFull %>% 
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

pt_grouped <- ptFull %>% 
  dplyr::filter(myPago>"May. 2018" & myPago <=longmonth) %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(CantMesesPT = n()) %>% 
  ungroup()

cond_grouped <- cond_clean %>% 
  dplyr::filter(myCond>"May. 2018" & myCond <=longmonth) %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(CantMesesCond = n()) %>% 
  ungroup()
saveRDS(pt_grouped, "D:/!bso/firstTimes/pt_grouped.rds")
saveRDS(cond_grouped, "D:/!bso/firstTimes/cond_grouped.rds")

pt_last12 <- generatePTk(ptFull, longmonth12, longmonth)
cond_last12 <- generateCondk(cond_clean, longmonth12, longmonth)
####____HISTORICO EN EL SF____####
HistCalif <- readRDS(paste0("D:/!bso/vipCartera/historic/HistCalif_",shortmonth1,".rds"))
HistSummary <- HistCalif %>% 
  dplyr::filter(FECHA < longmonth) %>% #Filter to reproduce previous months comment to keep all data
  select(CTACLIENTE, CALIFICACION_SF, ESTADO_HIST_SF) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(badCredit = ifelse(ESTADO_HIST_SF %in% c(2,3,4), 1, 0)) %>% 
  mutate(worstCredit = ifelse(ESTADO_HIST_SF == 4, 1, 0)) %>% 
  summarise(PeorCalif = max(CALIFICACION_SF), PeorEstadoHist = max(ESTADO_HIST_SF),
            totalBC = sum(badCredit), totalWC = sum(worstCredit)) 

####____ESTADO ULTIMO MES EN EL SF____####
infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',shortmonth1,'.rds'))

infoClean <- infoCheck %>% 
  dplyr::filter(REGULADO=="SBEF") %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
  mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
  mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
  mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>%
  group_by(CI) %>%
  dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  mutate(califPeorSF = max(CALIFICACION[esBSO == 0])) %>% 
  mutate(ESTADOPeorSF = max(ESTADO[esBSO == 0])) %>% 
  mutate(MaxDiasMoraSF = max(DiasMora[esBSO == 0])) %>% 
  ungroup() %>% 
  dplyr::filter(califPeorSF!="_" & SIGLA=="BSO") %>% 
  select(CTACLIENTE, OPERACION, califPeorSF, ESTADOPeorSF, MaxDiasMoraSF)
####____MORA INTRAMES____####
moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Sep2023.rds")) %>% 
  select(monDate, CTACLIENTE, OPERACION, maximaDPD_2, starts_with("tuvo")) %>% 
  dplyr::filter(!is.na(maximaDPD_2)) 

measuresMoraIM <- moraIntraMes %>% 
  dplyr::filter(monDate <= longmonth) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  summarise(PromMoraIM = mean(maximaDPD_2),
            SumMoraIM = sum(maximaDPD_2),
            MesesEnBDC = n(),
            MesesDPD1 = sum(tuvoDPD1),
            MesesDPD5 = sum(tuvoDPD5),
            MesesDPD10 = sum(tuvoDPD10),
            MesesDPD15 = sum(tuvoDPD15),
            MesesDPD20 = sum(tuvoDPD20),
            MesesDPD25 = sum(tuvoDPD25)) %>% 
  ungroup()

####____JOIN TO BDC____####
Clientes <- readRDS("D:/!bso/features/Clientes_Ene15Sep23.rds") %>% 
  select(CTACLIENTE, OPERACION, fdes_original=fdes)

bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",shortmonth,".rds")) %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  select(monDate, CTACLIENTE, OPERACION, CI, CAEDEC_DEST, CIU, Sucursal, ctaCont,
         saldous, par0)

bdcMetrics <- bdc %>% 
  left_join(Clientes, by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(cosechaM = as.yearmon(fdes_original)) %>% 
  mutate(cosechaY = year(fdes_original)) %>% 
  left_join(infoClean, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(measuresMoraIM, by=c("CTACLIENTE", "OPERACION")) %>% 
  left_join(pt_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(cond_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  select(-starts_with("Cierre")) %>% 
  replace_na(list(CantPT12Meses=0, CantCond12Meses=0)) %>% 
  mutate(MesesDesdeDesembolso = round((monDate-cosechaM)*12+1)) %>% 
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
  mutate(IntDPD1 = MesesDPD1/MesesEnBDC) %>% 
  mutate(IntDPD5 = MesesDPD5/MesesEnBDC) %>% 
  mutate(IntDPD10 = MesesDPD10/MesesEnBDC) %>% 
  mutate(IntDPD15 = MesesDPD15/MesesEnBDC) %>% 
  mutate(IntDPD20 = MesesDPD20/MesesEnBDC) %>% 
  mutate(IntDPD25 = MesesDPD25/MesesEnBDC) %>% 
  mutate(monDate = as.Date(monDate, frac=1))

write_xlsx(bdcMetrics, paste0("D:/!bso/features/Metricas_",shortmonth,".xlsx"))

####____OLD VERSION_____####
moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Sep2023.rds")) %>% 
  select(monDate, CTACLIENTE, OPERACION, maximaDPD_2, starts_with("tuvo")) %>% 
  dplyr::filter(!is.na(maximaDPD_2)) 

moraIntraMesLast <- moraIntraMes %>% 
  dplyr::filter(monDate=="Sep. 2023")
####____METRICAS____####
PTyCondJoin <- PTyCond %>% 
  left_join(moraIntraMesLast, by=c("CTACLIENTE","OPERACION"))

####____SISTEMA____####
infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_Ago2023.rds'))

infoClean <- infoCheck %>% 
  dplyr::filter(REGULADO=="SBEF") %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
  mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
  mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
  mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>%
  group_by(CI) %>%
  dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  ungroup() %>% 
  group_by(CI) %>%
  mutate(califPeorSF = max(CALIFICACION[esBSO == 0])) %>% 
  mutate(ESTADOPeorSF = max(ESTADO[esBSO == 0])) %>% 
  mutate(MaxDiasMoraSF = max(DiasMora[esBSO == 0])) %>% 
  ungroup() %>% 
  dplyr::filter(califPeorSF!="_" & SIGLA=="BSO") %>% 
  select(CTACLIENTE, OPERACION, califPeorSF, ESTADOPeorSF, MaxDiasMoraSF)

PTyCondJoin2 <- PTyCondJoin %>% 
  left_join(infoClean, by=c("CTACLIENTE","OPERACION"))

####____MEASURES____####  
pt_grouped <- readRDS("D:/!bso/firstTimes/pt_grouped.rds")
cond_grouped <- readRDS("D:/!bso/firstTimes/cond_grouped.rds")
measuresMoraIM <- readRDS("D:/!bso/firstTimes/measures.rds")
clientes <- readRDS('D:/!bso/features/ClientesTTF_Ene2015Sep2023.rds') %>% 
  select(CTACLIENTE, OPERACION, fdes_original = fdes)

PTyCondJoin3 <- PTyCondJoin2 %>% 
  left_join(clientes, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(pt_grouped, by=c("CTACLIENTE"="Cuenta", "OPERACION"="Operacion")) %>% 
  left_join(cond_grouped, by=c("CTACLIENTE"="Cuenta", "OPERACION"="Operacion")) %>% 
  left_join(measuresMoraIM, by=c("CTACLIENTE", "OPERACION")) %>% 
  mutate(cosechaY = year(fdes_original)) %>% 
  mutate(cosechaM = as.yearmon(fdes_original)) %>% 
  mutate(monthsSinceDisb = round((monDate-cosechaM)*12+1)) %>% 
  select(-mesesDesde16,-starts_with("FechaFirst")) %>% 
  rename(MoraIntraMes = maximaDPD_2, Fecha = monDate) %>% 
  mutate(Fecha = as.Date(Fecha, frac=1))

####____MORA EN SF DESDE 2018____####
HistCalif <- readRDS("D:/!bso/vipCartera/historic/HistCalif_Ago2023.rds")
HistSummary <- HistCalif %>% 
  # dplyr::filter(FECHA <= "may. 2023") %>% #Filter to reproduce previous months comment to keep all data
  select(CTACLIENTE, CALIFICACION_SF, ESTADO_HIST_SF) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(badCredit = ifelse(ESTADO_HIST_SF %in% c(2,3,4),1,0)) %>% 
  mutate(worstCredit = ifelse(ESTADO_HIST_SF == 4,1,0)) %>% 
  summarise(PeorCalif = max(CALIFICACION_SF), PeorEstadoHist = max(ESTADO_HIST_SF),
            totalBC = sum(badCredit), totalWC = sum(worstCredit)) %>% 
  # mutate(PeorCalif = cases(PeorCalif,levs = c(0,1,2,3,4,5,6), 
  #                          values = c('_','A','B','C','D','E','F'))) %>% 
  # mutate(PeorEstadoHist = cases(PeorEstadoHist,levs = c(-1,0,1,2,3,4), 
  #                               values = c('SIN DATOS','CONTINGENTE','VIGENTE','VENCIDA','EJECUCION','CASTIGADA'))) %>% 
  mutate(badCreditBin = case_when(!is.na(totalWC) & totalWC > 0 ~ '5. Tuvo castigo',
                                  totalBC > 2 ~ '4. 2+',
                                  totalBC == 2 ~ '3. 2',
                                  totalBC == 1  ~ '2. 1',
                                  totalBC == 0  ~ '1. 0',)) 
PTyCondJoin4 <- PTyCondJoin3 %>% 
  left_join(HistSummary, by="CTACLIENTE") %>% 
  rename(MesesNoVigenteSF = totalBC,
         MesesCastigoSF = totalWC,
         CatMesesNoVigente = badCreditBin)

write_xlsx(PTyCondJoin4, "D:/!bso/features/MetricasCierre_Sep2023.xlsx")

####____ADDING INFO DE CARTERA____####
codMod <- read_xlsx("D:/!bso/bases/excel/CodModulo.xlsx")
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Sep2023.rds") %>% 
  select(CTACLIENTE, OPERACION, TIPO_CREDITO, NOMBRE_AGENCIA, GENERO, Sector_Destino,
         Sector_Actividad, tipoCred, MODULO, TIPO_OPER, CALIFICACION,
         PLAZODIAS, SECTOR_CARTERA)
lastCierreSep <- read_xlsx("D:/!bso/features/Metricas_Sep2023.xlsx")

lastCierreSep <- lastCierreSep %>% 
  left_join(bdc, by=c("CTACLIENTE","OPERACION"))
  
lastCierreSep <- lastCierreSep %>% 
  left_join(codMod, by="MODULO")

write_xlsx(lastCierreSep, "D:/!bso/features/Metricas_Sep2023.xlsx")

bdc <- readRDS("D:/!bso/girCartera/rds/ec_Dic2022.rds") %>% 
  select(CTACLIENTE, OPERACION, TIPO_CREDITO, NOMBRE_AGENCIA, GENERO, Sector_Destino,
         Sector_Actividad, tipoCred, MODULO, TIPO_OPER, CALIFICACION,
         PLAZODIAS, SECTOR_CARTERA)
lastCierreDic <- read_xlsx("D:/!bso/features/Metricas_Dic2022.xlsx")

lastCierreDic <- lastCierreDic %>% 
  left_join(bdc, by=c("CTACLIENTE","OPERACION"))

lastCierreDic <- lastCierreDic %>% 
  left_join(codMod, by="MODULO")

write_xlsx(lastCierreDic, "D:/!bso/features/Metricas_Dic2022.xlsx")
###____CUADRES____####
lastBase <- read_xlsx("D:/!bso/features/Metricas_Sep2023.xlsx")
infoCheck <- infoCheck %>% 
  mutate(NDOC = str_replace(CI, "LP$|OR$|PO$|CB$|CH$|TJ$|SC$|BE$|PA$","")) 

lastBase <- lastBase %>% 
  mutate(NDOC = str_replace(CI, "LP$|OR$|PO$|CB$|CH$|TJ$|SC$|BE$|PA$","")) 

x <- lastBase %>% 
  dplyr::filter(califPeorSF=="A" & ESTADOPeorSF=="2. VENCIDA")

lastBaseJoin <- infoCheck %>% 
  semi_join(x, by="NDOC")
