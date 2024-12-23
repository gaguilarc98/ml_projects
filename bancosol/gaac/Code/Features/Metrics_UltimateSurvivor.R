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
library(arrow)
library(openxlsx)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____FIXING MONTHS____####
longmonth <- "Oct. 2023"
longmonth1 <- "Sep. 2023"
longmonth12 <- "Sep. 2022"
shortmonth <- str_replace(longmonth,". ","")
shortmonth1 <- str_replace(longmonth1,". ","")
shortmonth12 <- str_replace(longmonth12,". ","")

####____PAGOS TARDIOS Y CONDONACIONES____####
ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Oct23.rds")
condFull <- read_parquet('D:/!bso/condonaciones/CondFull.parquet')
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
  summarise(FechaFirstTardio = min(FechaPago),
            CantMesesPT = n()) %>% 
  ungroup()

cond_grouped <- cond_clean %>% 
  dplyr::filter(myCond>"May. 2018" & myCond <=longmonth) %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(FechaFirstCond),
            CantMesesCond = n()) %>% 
  ungroup()
saveRDS(pt_grouped, "D:/!bso/firstTimes/pt_grouped.rds")
saveRDS(cond_grouped, "D:/!bso/firstTimes/cond_grouped.rds")
pt_grouped <- readRDS("D:/!bso/firstTimes/pt_grouped.rds")
cond_grouped <- readRDS("D:/!bso/firstTimes/cond_grouped.rds")
pt_last12 <- generatePTk(ptFull, longmonth12, longmonth)
cond_last12 <- generateCondk(cond_clean, longmonth12, longmonth)
####____HISTORICO EN EL SF____####
HistCalif <- readRDS(paste0("D:/!bso/vipCartera/historic/HistCalif_",shortmonth1,".rds"))
HistSummary <- HistCalif %>% 
  dplyr::filter(FECHA < longmonth) %>% #Filter to reproduce previous months comment to keep all data
  select(FECHA, CTACLIENTE, CALIFICACION_SF, ESTADO_HIST_SF) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(badCredit = ifelse(ESTADO_HIST_SF %in% c(2,3,4), 1, 0)) %>% 
  mutate(worstCredit = ifelse(ESTADO_HIST_SF == 4, 1, 0)) %>% 
  summarise(PeorCalifHistSF = max(CALIFICACION_SF), PeorEstadoHistSF = max(ESTADO_HIST_SF),
            MesesMoraHistSF = sum(badCredit), MesesCastHistSF = sum(worstCredit), MesesEnSF = n_distinct(FECHA)) 

remove(HistCalif)
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
moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Oct2023.rds")) %>% 
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

saveRDS(measuresMoraIM,"D:/!bso/firstTimes/measures.rds")
measuresMoraIM <- readRDS("D:/!bso/firstTimes/measures.rds")
####____FEATURES DESEMBOLSO____####
Clientes <- readRDS("D:/!bso/features/ClientesTTF_Ene2015Oct2023.rds") %>% 
  select(CTACLIENTE, OPERACION, fdes_original=fdes, Ciclo)
####____JOIN TO BDC____####
bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",shortmonth,".rds")) %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  mutate(FechaCorte = as.Date(monDate, frac=1)) %>% 
  mutate(PrimerDia = as.Date(monDate)) %>% 
  mutate(UltimoDia = as.Date(monDate,frac=1)) %>% 
  mutate(MoraIntraMes = ifelse(!is.na(FULT_PAGO) & !is.na(FVEN_ULTPAGO) & FULT_PAGO>FVEN_ULTPAGO &
                                 FVEN_ULTPAGO>=PrimerDia & FVEN_PROXPAGO>UltimoDia,FULT_PAGO-FVEN_ULTPAGO,0)) %>% 
  mutate(maximaDPD = 0) %>% 
  mutate(maximaDPD = case_when(ctaCont %in% c('133','134','136','137','865') & DIASMORA>0 ~ DIASMORA,
                               ctaCont %in% c('133','134','136','137','865') & DIASMORA==0
                               & !is.na(FVEN_ULTPAGO) ~ as.numeric(UltimoDia-FVEN_ULTPAGO),
                               ctaCont %in% c('131','135') & DIASMORA!=0 ~ DIASMORA,
                               ctaCont %in% c('131','135') & DIASMORA==0 & !is.na(MoraIntraMes) ~ MoraIntraMes,
                               TRUE ~ maximaDPD)) %>% 
  mutate(MoraIntraMes = ifelse(maximaDPD>(UltimoDia-PrimerDia),UltimoDia-PrimerDia,maximaDPD)) %>% 
  mutate(Migrado = ifelse(MODULO==118 | str_detect(TIPO_OPER, "MIGR"), 1, 0)) %>% 
  select(FechaCorte, monDate, CTACLIENTE, OPERACION, Migrado, ctaCont, saldous, 
         ESTADO, CALIFICACION, DIASMORA, MoraIntraMes)

bdcMetrics <- bdc %>% 
  left_join(Clientes, by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(cosechaM = as.yearmon(fdes_original)) %>% 
  mutate(cosechaY = year(fdes_original)) %>% 
  left_join(infoClean, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(HistSummary, by=c("CTACLIENTE")) %>% 
  left_join(measuresMoraIM, by=c("CTACLIENTE", "OPERACION")) %>% 
  left_join(pt_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(cond_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(pt_last12, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(cond_last12, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  select(-starts_with("Cierre"), -starts_with("FechaFirst")) %>% 
  replace_na(list(CantPT12Meses=0, CantCond12Meses=0)) %>% 
  mutate(MesesDesdeDesembolso = round((monDate-cosechaM)*12+1)) %>% 
  mutate(MesesDesde2018 = case_when(cosechaM<"Ene. 2018" ~ round((monDate-as.yearmon("Ene. 2018"))*12+1),
                                    cosechaM>="Ene. 2018"~ round((monDate-cosechaM)*12+1)),) %>%
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
  mutate(IntMoraSF = MesesMoraHistSF/MesesEnSF) %>% 
  select(-monDate)

write_parquet(bdcMetrics, paste0("D:/!bso/features/Metricas_",shortmonth,".parquet"), compression = "GZIP")
# x <- bdcMetrics %>% dplyr::filter(MesesDesde2018 != MesesEnBDC) %>% dplyr::filter(Migrado==0)
####____ADDING SHIT DE RF____####
codMod <- read_xlsx("D:/!bso/bases/excel/CodModulo.xlsx")
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Oct2023.rds") %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  left_join(codMod, by="MODULO") %>% 
  select(monDate, CTACLIENTE, OPERACION, CI, CAEDEC_DEST, CIU, Sucursal, ctaCont, 
         saldous, par0, TIPO_CREDITO, NOMBRE_AGENCIA, GENERO, Sector_Destino,
         Sector_Actividad, tipoCred, MODULO, TIPO_OPER, CALIFICACION,
         PLAZODIAS, SECTOR_CARTERA, NOMBRE_MODULO)

bdcMetricsSelect <- bdcMetrics %>% 
  select(CTACLIENTE, OPERACION, fdes_original, cosechaM, cosechaY, califPeorSF,
         ESTADOPeorSF, MaxDiasMoraSF, PromMoraIM, SumMoraIM, MesesEnBDC, MesesDPD1,
         MesesDPD5, MesesDPD10, MesesDPD15, MesesDPD20, MesesDPD25, CantPT12Meses,
         CantCond12Meses, MesesDesdeDesembolso, NXN, IntDPD1, IntDPD5, IntDPD10, 
         IntDPD15, IntDPD20, IntDPD25)
bdcExp <- bdc %>% 
  left_join(bdcMetricsSelect, by=c("CTACLIENTE", "OPERACION")) %>% 
  relocate(fdes_original, cosechaM, cosechaY, califPeorSF,
           ESTADOPeorSF, MaxDiasMoraSF, PromMoraIM, SumMoraIM, MesesEnBDC, MesesDPD1,
           MesesDPD5, MesesDPD10, MesesDPD15, MesesDPD20, MesesDPD25, CantPT12Meses,
           CantCond12Meses, MesesDesdeDesembolso, NXN, IntDPD1, IntDPD5, IntDPD10, 
           IntDPD15, IntDPD20, IntDPD25, .after = par0)

write_xlsx(bdcExp, "D:/!bso/features/Metricas_Oct2023.xlsx")

####____EJEMPLOS OPERACIONES EN ALERTA_____####
#5 de 4x4 y 5 de 9x9
bdcExp <- read_xlsx("D:/!bso/features/Metricas_Oct2023.xlsx")


bdcExample <- bdcExp %>% 
  # dplyr::filter(ctaCont!="865") %>% 
  mutate(FechaCorte = as.Date("2023-10-31")) %>% 
  select(FechaCorte, CTACLIENTE, OPERACION)
  
pt <- ptFull %>% 
  dplyr::filter(myPago >= "Nov. 2022" & myPago<= "Oct. 2023") %>% 
  group_by(Cuenta, Operacion) %>% 
  mutate(CantPT12Meses = n()) %>% 
  ungroup() %>% 
  mutate(FechaHoraPago = as.POSIXct(paste(FechaPago, Hora_UltDia), format = "%Y-%m-%d %H:%M:%S")) %>% 
  select(Cuenta, Operacion, CantPT12Meses, myPago, FechaHoraPago, CapitalPagado) %>% 
  pivot_wider(names_from = myPago, values_from = c(FechaHoraPago, CapitalPagado))

cond <- cond_clean %>% 
  dplyr::filter(myCond >= "Nov. 2022" & myCond<= "Oct. 2023") %>% 
  group_by(Cuenta, Operacion) %>% 
  mutate(CantCond12Meses = n()) %>% 
  ungroup() %>% 
  select(Cuenta, Operacion,CantCond12Meses, myCond, CondCapInt_USD, CondCap_USD) %>% 
  pivot_wider(names_from = myCond, values_from = c(CondCapInt_USD, CondCap_USD))

bdcFinal <- bdcExample %>% 
  left_join(pt, by=c("CTACLIENTE"="Cuenta", "OPERACION"="Operacion")) %>% 
  left_join(cond, by=c("CTACLIENTE"="Cuenta", "OPERACION"="Operacion")) %>% 
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
  group_by(NXN) %>% 
  dplyr::filter(NXN %in% c("04x04", "09x09")) %>% 
  dplyr::filter(row_number()<=5) %>% 
  relocate(CantCond12Meses, NXN, .after=CantPT12Meses)
  
write.xlsx(bdcFinal, "D:/!bso/requests/EjemplosAlertaNXN_Oct2023.xlsx")
