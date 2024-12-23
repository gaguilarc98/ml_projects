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
####____LECTURA DE COSAS____####
PTyCond <- read_xlsx(paste0("D:/!bso/mph/condonados/TardioCond_Sep2023.xlsx")) %>% 
  replace_na(list(CantPT12Meses=0, CantCond12Meses=0)) %>% 
  select(CTACLIENTE, OPERACION, ctaCont, ESTADO, DIASMORA, CantPT12Meses, 
         CantCond12Meses, NXN)
####____MORA INTRAMES____####
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
  