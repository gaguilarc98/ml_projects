
library(dplyr)
library(foreign)
library(reshape)
library(reshape2)
library(stringr)
library(lubridate)
library(Hmisc)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(forecast)
library(quantmod)
library(astsa)
library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(timeSeries)
library(tseries)
library(xtable)
library(openxlsx)
library(hrbrthemes)
library(viridis)
library(scales)
library(janitor)
library(RColorBrewer)
library(paletteer)
library(plotly)
library(kableExtra)
library(glmnet)
remove(list = ls())
options("encoding" = "UTF-8")
#Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)

paleta <- colorRampPalette(c("slateblue4","purple4","slateblue3",
                             "darkorchid3","red3","tan2","yellow2",
                             "white"),bias=1.5)
#===============================================================================
# Data in
bdcList <- list()
file_list <- list.files(path='C:/!bso/girCartera/rds')
for (i in 1:length(file_list)) {
  
  #print(file_list[i])
  bdc <- readRDS(paste0('C:/!bso/girCartera/rds/',
                        file_list[i])) %>% 
    select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, 
           CALIFICACION, fbase, montous, FDESEMBOLSO, saldous, GENERO)
  bdcList[[i]] <- bdc
}
#quietly(gc())

#-------------------------------
bdcFull <- bind_rows(bdcList) %>%
  mutate(mon = substr(fbase,1,3)) %>%
  mutate(year = substr(fbase,4,7)) %>%
  mutate(mes = case_when(mon == 'Ene'~'jan',
                         mon == 'Feb'~'feb',
                         mon == 'Mar'~'mar',
                         mon == 'Abr'~'apr',
                         mon == 'May'~'may',
                         mon == 'Jun'~'jun',
                         mon == 'Jul'~'jul',
                         mon == 'Ago'~'aug',
                         mon == 'Sep'~'sep',
                         mon == 'Oct'~'oct',
                         mon == 'Nov'~'nov',
                         mon == 'Dic'~'dec',)) %>%
  mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>%
  mutate(monDate = as.yearmon(dayDate)) %>%
  select(-dayDate, -mon, -year, -mes) %>%
  arrange(CI, CTACLIENTE, OPERACION, monDate, FDESEMBOLSO, GENERO) 
#quietly(gc())
bdcList <- NULL
nrowFull <- nrow(bdcFull)
nopsFull <- length(unique(bdcFull$OPERACION))
ncliFull <- length(unique(bdcFull$CI))
write_rds(bdcFull, 'C:/!bso/vipCartera/output/bdcFull_eneFeb20223.rds')
#===============================================================================
# Primeros filtros
bdcFull <- readRDS('D:/!backupRBS/vip/bdcFull_eneFeb20223.rds')
bdcVIP_ene <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CI, DIASMORA, monDate, montous, FDESEMBOLSO, GENERO) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  dplyr::filter(monDate <= 'mar. 2020'| monDate >= 'ene. 2021') %>% 
  glimpse()  %>% 
  ungroup() %>%  
  select(OPERACION, CTACLIENTE, CI, DIASMORA, montous, fdes, monDate, GENERO) %>%
  mutate(eom = as.Date(monDate, frac = 1)) %>%  
  mutate(loanDays = as.integer(eom - fdes)) %>% # check measure
  group_by(CTACLIENTE, CI) %>% 
  mutate(totalLoanDays = sum(loanDays, na.rm = T)) %>% # check measure
  mutate(moraAcum = sum(DIASMORA, na.rm = T)) %>% # check measure
  mutate(moraMax = max(DIASMORA, na.rm = T)) %>% # check measure
  ungroup() %>% 
  group_by(OPERACION) %>% 
  mutate(montous = ifelse(row_number() == 1, montous, NA)) %>% 
  mutate(nops = ifelse(!is.na(montous), 1, 0)) %>% 
  ungroup() %>% 
  group_by(CI, CTACLIENTE) %>% # check grouping
  dplyr::filter(!is.na(montous)) %>%
  dplyr::filter(montous > 0) %>% 
  mutate(totalMonto = sum(montous, na.rm = T)) %>% 
  mutate(maxMonto = max(montous, na.rm = T)) %>% # check measure
  mutate(minMonto = min(montous, na.rm = T)) %>% # check measure
  mutate(montoRat = maxMonto/minMonto) %>% # CHECK MEASURE
  mutate(pct_maxDM_acumDM = ifelse(moraAcum > 0, moraMax/moraAcum, 0)) %>% # check measure
  mutate(pct_acumDM_TLD = ifelse(totalLoanDays > 0, moraAcum/totalLoanDays, 0)) %>% # check measure
  mutate(pct_acumDM_TLD = ifelse(pct_acumDM_TLD > 1, 1, pct_acumDM_TLD)) %>% # check measure
  mutate(totalNops = sum(nops)) %>%# check measure
  select(-montous, -eom, -DIASMORA, -fdes, -monDate, -OPERACION, -nops) %>% 
  summarise_all(max, na.rm = T) %>% # check grouping
  glimpse() %>% 
  #dplyr::filter(totalLoanDays > 360) %>% 
  mutate(moraMaxBin = case_when(moraMax == 0~'1. 0 días',
                                moraMax > 0 & moraMax <= 5~'2. 1-5 días',
                                moraMax > 5 & moraMax <= 10~'3. 6-10 días',
                                moraMax > 10 & moraMax <= 15~'4. 11-15 días',
                                moraMax > 15 & moraMax <= 20~'5. 16-20 días',
                                moraMax > 20 ~'6. 20+ días')) %>% 
  mutate(moraAcumBin = case_when(moraAcum == 0~'1. 0 días',
                                 moraAcum > 0 & moraAcum <= 5~'2. 1-5 días',
                                 moraAcum > 5 & moraAcum <= 10~'3. 6-10 días',
                                 moraAcum > 10 & moraAcum <= 15~'4. 11-15 días',
                                 moraAcum > 15 & moraAcum <= 20~'5. 16-20 días',
                                 moraAcum > 20 ~'6. 20+ días')) %>% 
  mutate(pctMaxBin = case_when(pct_maxDM_acumDM > 0.9~'1. 90% +',
                               pct_maxDM_acumDM > 0.8 & pct_maxDM_acumDM <= 0.9~'2. 80%-90%',
                               pct_maxDM_acumDM > 0.7 & pct_maxDM_acumDM <= 0.8~'3. 70%-80%',
                               pct_maxDM_acumDM <= 0.7 & pct_maxDM_acumDM > 0 ~'4. 1-70%',
                               pct_maxDM_acumDM == 0~'5. 0 (sin morosidad)')) %>% 
  mutate(pctAcumBin = case_when(pct_acumDM_TLD == 0~'1. 0%',
                                pct_acumDM_TLD > 0 & pct_acumDM_TLD <= 0.03~'2. 1%-3%',
                                pct_acumDM_TLD > 0.03 & pct_acumDM_TLD <= 0.05~'3. 3%-5%',
                                pct_acumDM_TLD > 0.05 & pct_acumDM_TLD <= 0.07~'4. 5%-7%',
                                pct_acumDM_TLD > 0.07~'5. 7% +')) %>% 
  mutate(tldBin =case_when(totalLoanDays <= 360 ~ '0. Menos de 360 días',
                           totalLoanDays > 360 & totalLoanDays <=1080 ~'1. 360-1080 días',
                           totalLoanDays > 1080 & totalLoanDays <=2160 ~'2. 1080-2160 días',
                           totalLoanDays > 2160 & totalLoanDays <=3240 ~'3. 2160-3240 días',
                           totalLoanDays > 3240~'4. 3240 +')) %>% 
  mutate(montoRatBin = case_when(montoRat == 1 ~ '1. Sin crecimiento',
                                 montoRat > 1 & montoRat <= 2 ~ '2. Hasta 2x',
                                 montoRat > 2 & montoRat <= 4 ~ '3. 2x-4x',
                                 montoRat > 4~ '4. 4x +',)) %>% 
  ungroup() %>% 
  select(ends_with('Bin'), CI, totalMonto, minMonto, maxMonto, totalNops, CTACLIENTE) 
write_rds(bdcVIP_ene, 'D:/!backupRBS/vip/bdcVIP_ene2023_vAE.rds')
bdcVIPEne <- readRDS('D:/!backupRBS/vip/bdcVIP_ene2023_vAE.rds')
bdcVIPFeb <- readRDS('D:/backupRBS/vip/bdcVIP_feb2023_vAE.rds')

#===============================================================================
# bdc noviembre
bdcLastEne<- readRDS('C:/!bso/girCartera/rds/ec_Ene2023.rds')
bdcNovEne <- bdcLastEne %>% 
  dplyr::filter(MODULO != 131) %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  mutate(activoAgo = 1) %>% 
  select(fdes, saldous, montous, previus, intus, categ, tipoCred, sucursal,
         rango, grupoCaedecC, grupoCaedecD, CI, CTACLIENTE, CALIFICACION,
         ends_with('_TIT'), GENERO) %>% 
  group_by(CI) %>% 
  mutate(totalSaldoNov = sum(saldous)) %>% 
  mutate(totalMontoNov = sum(montous)) %>% 
  mutate(totalInt = sum(intus)) %>% 
  mutate(tppa = totalInt/totalSaldoNov*100) %>% 
  mutate(pctResto = totalSaldoNov/totalMontoNov) %>% 
  mutate(pctRestoBin = case_when(pctResto <= 0.2 ~ '1. Hasta 20%',
                                 pctResto > 0.2 & pctResto <= 0.5 ~ '2. 20% - 50%',
                                 pctResto > 0.5 & pctResto <= 0.8 ~ '3. 50% - 80%',
                                 pctResto > 0.8 ~ '4. 20% - 50%',)) 
  
  
bdcNovCli_ene <- bdcNovEne %>% 
  select(CI, CTACLIENTE, totalSaldoNov, totalMontoNov, totalInt,
         pctResto, pctRestoBin, CALIFICACION, GENERO, ends_with('_TIT')) %>% 
  ungroup() %>% 
  group_by(CI, CTACLIENTE, GENERO) %>% 
  summarise_all(max, na.rm = T)

bdcNovVip_ene <- bdcVIPEne %>% 
  left_join(bdcNovCli_ene, by = c('CTACLIENTE'))
bdcNovVipNoMissEne <- bdcNovVip_ene %>% 
  dplyr::filter(!is.na(moraMaxBin))

write_rds(bdcNovVipNoMissEne, 'D:/!backupRBS/vip/bdcDicVipNoMiss_ene2023.rds')
write_rds(bdcNovVipNoMissFeb, 'D:/backupRBS/vip/bdcDicVipNoMiss_feb2023.rds')
write_rds(bdcNovEne, 'D:/!backupRBS/vip/bdcNov_ene2023.rds')
write_rds(bdcNovFeb, 'D:/backupRBS/vip/bdcNov_feb2023.rds')
bdcNovVipNoMissEne <- readRDS('D:/!backupRBS/vip/bdcDicVipNoMiss_ene2023.rds')
bdcNovVipNoMissFeb <- readRDS('D:/!backupRBS/vip/bdcDicVipNoMiss_feb2023.rds')
bdcNovEne <- readRDS('D:/!backupRBS/vip/bdcNov_ene2023.rds')
bdcNovFeb <- readRDS('D:/!backupRBS/vip/bdcNov_feb2023.rds')
#===============================================================================
# infocred
infoRaw <- fread('C:/!bso/Cargamensual_infocred/utf/BSO202212_utf8.txt', encoding = 'UTF-8')
nrowInfo <- nrow(infoRaw)

infoCheck <- infoRaw %>%
  mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
         saldo = `SBEF VIGENTE` + `SBEF VENCIDO` + `SBEF EJECUCION`,
         saldoMora = `SBEF VENCIDO` + `SBEF EJECUCION`,
         saldoMMC = `SBEF VENCIDO` + `SBEF EJECUCION` + `SBEF CASTIGADO`) %>%
  select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`,
         `ENTIDAD SBEF`, `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
         MontoOriginal, MonedaOrigen, `SBEF CALIFICACION`, `FR CALIFICACION`,
         saldo, saldoMora, saldoMMC, `SBEF CASTIGADO`,`NumeroOp`) %>%
  separate(NumeroOp,into = c("CTACLIENTE","OPERACION"),sep="-") %>% 
  mutate(CTACLIENTE=as.numeric(CTACLIENTE),
         OPERACION=as.numeric(OPERACION)) %>% 
  rename(saldoCastInfo=`SBEF CASTIGADO`) %>% 
  mutate(saldoCastInfo = as.numeric(saldoCastInfo)/6.86) %>% #Todas las cantidades están en bolivianos
  mutate(MontoOriginal = as.numeric(MontoOriginal)/6.86) %>% #sin importar el valor de moneda
  mutate(saldoVig = as.numeric(`SBEF VIGENTE`)/6.86) %>%
  mutate(saldo = saldo/6.86) %>%
  mutate(saldoMora = saldoMora/6.86) %>%
  rename(saldoMoraInfo=saldoMora)

infoCleanA <- infoCheck %>% 
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% 
  mutate(esBSO=ifelse(`SIGLA SBEF`=='BSO',1,0)) %>%
  mutate(noesBSO=ifelse(`SIGLA SBEF`!='BSO',1,0)) %>%
  mutate(`SBEF CALIFICACION` = ifelse(is.na(`SBEF CALIFICACION`),"_",`SBEF CALIFICACION`)) %>% 
  group_by(CI) %>%
  dplyr::filter(max(row_number())>1) %>%
  mutate(califBSO = ifelse(`SIGLA SBEF` == 'BSO', `SBEF CALIFICACION`, '_'),
         califBSO_2 = max(califBSO, na.rm = T)) %>% # Max. calificación en BSO
  mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
         califSF_2 = max(califSF, na.rm = T)) %>%  # Max calificación en SF
  mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
         califSF_2 = max(califSF, na.rm = T)) %>%  # Max calificación en SF
  mutate(saldoNoBSO = sum(saldo*noesBSO,na.rm=T)) %>% 
  mutate(saldoMoraNoBSO = sum(saldoMoraInfo*noesBSO,na.rm=T)) %>% 
  mutate(saldoMMCNoBSO = sum(saldoMMC*noesBSO,na.rm=T)) %>% 
  mutate(entidadSF = ifelse(`SBEF CALIFICACION` == califSF_2, `SIGLA SBEF`, '_'),
         entidadSF_2 = max(entidadSF[which(entidadSF!='BSO')], na.rm = T)) %>%  # Max entidad en SF
  dplyr::filter(califBSO_2==califBSO & `SIGLA SBEF`=='BSO') %>% 
  select(-califBSO,-califSF,-`SBEF CALIFICACION`) %>% 
  ungroup() %>% 
  dplyr::filter(califSF_2!="_")

infoJoin <- infoCleanA %>% 
  rename(CI_info=CI) %>% 
  left_join(bdcNovEne,by=c("CTACLIENTE","OPERACION")) %>%
  mutate(histStr = as.character(HISTORICO)) %>% # HISTORY CONSTRUCTION
  mutate(histStr2 = substr(histStr, 1, 16)) %>% 
  mutate(histStr2 = ifelse(is.na(histStr2), '0', histStr2)) %>% 
  mutate(badCredit = ifelse((str_detect(histStr2, '2') | str_detect(histStr2, '3') |
                               str_detect(histStr2, '4')),1,0)) %>% # CHECK MEASURE
  mutate(worstCredit = ifelse(str_detect(histStr2, '4'),1,0)) %>% # CHECK MEASURE
  ungroup() %>% 
  group_by(CI) %>%
  mutate(totalBC = str_count(histStr2, "2") + str_count(histStr2, "3") +
           str_count(histStr2, "4"),
         totalWC = str_count(histStr2, "4")) %>%  # CHECK GROUPING
  arrange(CI) %>% 
  mutate(badCreditBin = case_when(totalBC == 0  ~ '1. 0',
                                  totalBC == 1  ~ '2. 1',
                                  totalBC == 2 ~ '3. 2',
                                  totalBC > 2 ~ '4. 2+',
                                  totalWC > 0  & totalWC < NA ~ '5. Tuvo castigo')) %>% 
  select(CI, ends_with('Bin'), totalBC, totalWC, histStr, histStr2, badCredit, worstCredit,CTACLIENTE, OPERACION) %>% 
  summarise_all(max)


infoPerf <- infoRaw %>% 
  glimpse() %>% 
  mutate(CI = paste0(`NRO DOCUMENTO`, EXT)) %>% 
  mutate(idBso_=ifelse(`SIGLA SBEF`=='BSO', NumeroOp, '-')) %>%
  mutate(dBso_=ifelse(`SIGLA SBEF`=='BSO' & str_detect(`TIPO OBLIGADO SBEF`, 'A -'), 1, 0)) %>%
  group_by(CI) %>%
  mutate(dBso=max(dBso_)) %>% 
  mutate(idBso=max(idBso_)) %>% 
  ungroup() %>% 
  select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`, 
         `ENTIDAD SBEF`,  `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
         MontoOriginal, MonedaOrigen, idBso, dBso) %>% 
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A -')) %>% 
  dplyr::filter(`SIGLA SBEF` != 'BSO') %>% # check filter
  dplyr::filter(dBso==1) %>% 
  mutate(MontoOriginal = ifelse(MonedaOrigen == 'MN', as.numeric(MontoOriginal)/6.86, as.numeric(MontoOriginal))) %>% 
  mutate(saldoVig = ifelse(MonedaOrigen == 'MN',  as.numeric(`SBEF VIGENTE`)/6.86,   as.numeric(`SBEF VIGENTE`))) %>% 
  mutate(histStr = as.character(HISTORICO)) %>% # HISTORY CONSTRUCTION
  mutate(histStr2 = substr(histStr, 1, 16)) %>% 
  mutate(histStr2 = ifelse(is.na(histStr2), '0', histStr2)) %>% 
  mutate(badCredit = ifelse((str_detect(histStr2, '2') | str_detect(histStr2, '3') |
                             str_detect(histStr2, '4')),1,0)) %>% # CHECK MEASURE
  mutate(worstCredit = ifelse(str_detect(histStr2, '4'),1,0)) %>% # CHECK MEASURE
  ungroup() %>% 
  group_by(CI) %>%
  mutate(totalBC = str_count(histStr2, "2") + str_count(histStr2, "3") +
           str_count(histStr2, "4"),
         totalWC = str_count(histStr2, "4")) %>%  # CHECK GROUPING
  arrange(CI) %>% 
  mutate(badCreditBin = case_when(totalBC == 0  ~ '1. 0',
                                  totalBC == 1  ~ '2. 1',
                                  totalBC == 2 ~ '3. 2',
                                  totalBC > 2 ~ '4. 2+',
                                  totalWC > 0  & totalWC < NA ~ '5. Tuvo castigo')) %>% 
  select(CI, ends_with('Bin'), totalBC, totalWC, histStr, histStr2, badCredit, worstCredit, idBso) %>%
  mutate(idBso2=idBso) %>% 
  separate(idBso, sep='-', into = c('CTACLIENTE', 'OPERACION')) %>%
  mutate(CTACLIENTE=as.integer(CTACLIENTE)) %>% 
  summarise_all(max) %>% 
  dplyr::filter(idBso2!='-') %>% 
  dplyr::rename(CI_info=CI)

###JOIN ARREGLADO##############################################################
bdcNovInfo_1_ene <- bdcNovVipNoMissEne %>% 
  left_join(infoPerf, by = 'CTACLIENTE') 
################################################################################
bdcAgoInfoCL <- bdcNovInfo_1_ene
################################################################################
#===============================================================================
# daily delinquency data
#Con script vipClients_Dataprep_Daily
dailyMM <- readRDS('C:/!bso/vipCartera/output/dailyMM_ene2023.rds') %>%
  glimpse() %>% 
  ungroup() %>% 
  select(-OPERACION, -maxMoraIM_op) %>% 
  group_by(CI, CTACLIENTE) %>% 
  summarise_all(max)

bdcAgoInfoCL_dailyVal <- bdcAgoInfoCL %>% 
  left_join(dailyMM, by = c('CI', 'CTACLIENTE')) %>% 
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl > 0 & maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl > 5 & maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl > 10 & maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl > 15 & maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días')) %>% 
  glimpse()
#===============================================================================
# Lista excelentes para Negocios
bus <- read.csv('C:/!bso/vipCartera/lista_clientes/Clientes excelentes enero 2023.csv', sep='|') %>% 
  dplyr::rename(CTACLIENTE = Cuenta) %>% 
  select(CTACLIENTE) %>% 
  distinct() %>% 
  mutate(excelenteNegocios = 1)
length(unique(bus$CTACLIENTE))

finalVIPdf <- bdcAgoInfoCL_dailyVal %>% 
  left_join(bus, by = 'CTACLIENTE') %>% 
  mutate(excelenteNegocios = ifelse(is.na(CTACLIENTE), NA, excelenteNegocios))
write.xlsx(finalVIPdf, 'D:/backupRBS/vip/finalVIPdf_histStr2_ene2023.xlsx')
#===============================================================================
# data for score selection
bdcFull<-readRDS('C:/!bso/vipCartera/output/bdcFull_eneFeb20223.rds')
bdcVIPscore_ene <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CI, DIASMORA, monDate, montous, FDESEMBOLSO, GENERO) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  dplyr::filter(monDate <= 'mar. 2020'| monDate >= 'ene. 2021') %>% 
  glimpse()  %>% 
  ungroup() %>%  
  select(OPERACION, CTACLIENTE, CI, DIASMORA, montous, fdes, monDate, GENERO) %>%
  mutate(eom = as.Date(monDate, frac = 1)) %>%  # check measure
  mutate(loanDays = as.integer(eom - fdes)) %>% # check measure
  group_by(CTACLIENTE, CI) %>% 
  mutate(totalLoanDays = sum(loanDays, na.rm = T)) %>% 
  mutate(moraAcum = sum(DIASMORA, na.rm = T)) %>% 
  mutate(moraMax = max(DIASMORA, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(OPERACION) %>% 
  mutate(montous = ifelse(row_number() == 1, montous, NA)) %>% 
  mutate(nops = ifelse(!is.na(montous), 1, 0)) %>% 
  ungroup() %>% 
  group_by(CI) %>% # check grouping
  dplyr::filter(!is.na(montous)) %>%
  dplyr::filter(montous > 0) %>% 
  mutate(totalMonto = sum(montous, na.rm = T)) %>% 
  mutate(maxMonto = max(montous, na.rm = T)) %>% 
  mutate(minMonto = min(montous, na.rm = T)) %>% 
  mutate(montoRat = maxMonto/minMonto) %>% # CHECK MEASURE
  mutate(pct_maxDM_acumDM = ifelse(moraAcum > 0, moraMax/moraAcum, 0)) %>% 
  mutate(pct_acumDM_TLD = ifelse(totalLoanDays > 0, moraAcum/totalLoanDays, 0)) %>% 
  mutate(pct_acumDM_TLD = ifelse(pct_acumDM_TLD > 1, 1, pct_acumDM_TLD)) %>% 
  mutate(totalNops = sum(nops)) %>%
  select(-montous, -eom, -DIASMORA, -fdes, -monDate, -OPERACION, -nops) %>% 
  summarise_all(max, na.rm = T) %>%
  #dplyr::filter(totalLoanDays > 360) %>% 
  mutate(moraMaxBin = case_when(moraMax == 0~'1. 0 días',
                                moraMax > 0 & moraMax <= 5~'2. 1-5 días',
                                moraMax > 5 & moraMax <= 10~'3. 6-10 días',
                                moraMax > 10 & moraMax <= 15~'4. 11-15 días',
                                moraMax > 15 & moraMax <= 20~'5. 16-20 días',
                                moraMax > 20 ~'6. 20+ días')) %>% 
  mutate(moraAcumBin = case_when(moraAcum == 0~'1. 0 días',
                                 moraAcum > 0 & moraAcum <= 5~'2. 1-5 días',
                                 moraAcum > 5 & moraAcum <= 10~'3. 6-10 días',
                                 moraAcum > 10 & moraAcum <= 15~'4. 11-15 días',
                                 moraAcum > 15 & moraAcum <= 20~'5. 16-20 días',
                                 moraAcum > 20 ~'6. 20+ días')) %>% 
  mutate(pctMaxBin = case_when(pct_maxDM_acumDM > 0.9~'1. 90% +',
                               pct_maxDM_acumDM > 0.8 & pct_maxDM_acumDM <= 0.9~'2. 80%-90%',
                               pct_maxDM_acumDM > 0.7 & pct_maxDM_acumDM <= 0.8~'3. 70%-80%',
                               pct_maxDM_acumDM <= 0.7 & pct_maxDM_acumDM > 0 ~'4. 1-70%',
                               pct_maxDM_acumDM == 0~'5. 0 (sin morosidad)')) %>% 
  mutate(pctAcumBin = case_when(pct_acumDM_TLD == 0~'1. 0%',
                                pct_acumDM_TLD > 0 & pct_acumDM_TLD <= 0.03~'2. 1%-3%',
                                pct_acumDM_TLD > 0.03 & pct_acumDM_TLD <= 0.05~'3. 3%-5%',
                                pct_acumDM_TLD > 0.05 & pct_acumDM_TLD <= 0.07~'4. 5%-7%',
                                pct_acumDM_TLD > 0.07~'5. 7% +')) %>% 
  mutate(tldBin =case_when(totalLoanDays <= 360 ~ '0. Menos de 360 días',
                           totalLoanDays > 360 & totalLoanDays <=1080 ~'1. 360-1080 días',
                           totalLoanDays > 1080 & totalLoanDays <=2160 ~'2. 1080-2160 días',
                           totalLoanDays > 2160 & totalLoanDays <=3240 ~'3. 2160-3240 días',
                           totalLoanDays > 3240~'4. 3240 +')) %>% 
  mutate(montoRatBin = case_when(montoRat == 1 ~ '1. Sin crecimiento',
                                 montoRat > 1 & montoRat <= 2 ~ '2. Hasta 2x',
                                 montoRat > 2 & montoRat <= 4 ~ '3. 2x-4x',
                                 montoRat > 4~ '4. 4x +',)) %>% 
  ungroup() %>% 
  select(ends_with('Bin'), CI, CTACLIENTE, totalMonto, minMonto, maxMonto, totalNops,
         montoRat, totalLoanDays, pct_acumDM_TLD, pct_maxDM_acumDM, 
         moraAcum, moraMax, GENERO) %>% 
  left_join(infoPerf, by = 'CTACLIENTE') %>% 
  left_join(dailyMM, by = c('CI', 'CTACLIENTE')) %>% 
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl > 0 & maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl > 5 & maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl > 10 & maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl > 15 & maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días')) %>% 
  left_join(bus, by = 'CTACLIENTE') %>% 
  mutate(excelenteNegocios = ifelse(is.na(CTACLIENTE), NA, excelenteNegocios)) %>% 
  glimpse()

write_rds(bdcVIPscore_ene, 'D:/backupRBS/vip/bdcVIPscore_ene2023_Vae.rds')
bdcVIPscoreEne <- readRDS('D:/!backupRBS/vip/bdcVIPscore_ene2023_Vae.rds')

#==============================================================================
# DEFINICIÓN DE CLIENTE VIP
bdcVIPScore_t <- bdcVIPscoreEne %>% 
  mutate(target = ifelse(((pctMaxBin == '5. 0 (sin morosidad)' | pctMaxBin == '1. 90% +') &
                            (pctAcumBin == '1. 0%') &
                            (tldBin == '4. 3240 +' | tldBin == '3. 2160-3240 días' | 
                             tldBin == '2. 1080-2160 días') &
                            (montoRatBin == '4. 4x +' | montoRatBin == '3. 2x-4x') &
                            (is.na(badCreditBin) | badCreditBin == '1. 0' | badCreditBin == '2. 1') &
                            (is.na(maxMoraIMclBin) | maxMoraIMclBin == '1. 0 días')), 1, 0 )) %>% # Este es el ifelse que identifica al cliente VIP
  select(-ends_with('Bin'), -CTACLIENTE, -starts_with('histStr'), -ends_with('Credit')) %>% 
  mutate(totalBC = ifelse(is.na(totalBC), 0, totalBC)) %>% 
  mutate(totalWC = ifelse(is.na(totalWC), 0, totalWC)) %>% 
  mutate(maxMoraIM_cl = ifelse(is.na(maxMoraIM_cl), 0, maxMoraIM_cl)) %>% 
  mutate(excelenteNegocios = ifelse(is.na(excelenteNegocios), 0, excelenteNegocios)) %>% 
  glimpse()

table(bdcVIPScore_t$target)
glimpse(bdcVIPScore_t)
write.csv(bdcVIPScore_t, 'D:/backupRBS/vip/dataScore_ene2023.csv', row.names = F)
################################################################################
# Here we go to python to estimate scores
################################################################################
#===============================================================================
# Table export
bdcVIPScoreFeb <- readRDS('D:/!backupRBS/vip/bdcVIPscore_feb2023_Vae.rds')
bdcVIPScore_exp <- bdcVIPScoreFeb %>% 
  mutate(target = ifelse(((pctMaxBin == '5. 0 (sin morosidad)' | pctMaxBin == '1. 90% +') &
                            (pctAcumBin == '1. 0%') &
                            (tldBin == '4. 3240 +' | tldBin == '3. 2160-3240 días' | 
                               tldBin == '2. 1080-2160 días') &
                            (montoRatBin == '4. 4x +' | montoRatBin == '3. 2x-4x') &
                            (is.na(badCreditBin) | badCreditBin == '1. 0' | badCreditBin == '2. 1') &
                            (is.na(maxMoraIMclBin) | maxMoraIMclBin == '1. 0 días')), 1, 0 )) %>% 
  #select(-ends_with('Bin'), -CTACLIENTE, -starts_with('histStr'), -ends_with('Credit')) %>% 
  mutate(totalBC = ifelse(is.na(totalBC), 0, totalBC)) %>% 
  mutate(totalWC = ifelse(is.na(totalWC), 0, totalWC)) %>% 
  mutate(maxMoraIM_cl = ifelse(is.na(maxMoraIM_cl), 0, maxMoraIM_cl)) %>% 
  mutate(excelenteNegocios = ifelse(is.na(excelenteNegocios), 0, excelenteNegocios)) %>% 
  glimpse()

# Reading probabilities from python
dfScoreFeb <- read.csv('D:/!backupRBS/vip/vipList_wScores_feb2023.csv') %>% 
  select(CI, prob_GC, goodClient)

bdcNovExp <- bdcNovFeb %>% 
  select(CI, OPERACION, saldous, montous, CALIFICACION, ESTADO, tipoCred,
         CPOP, Sucursal, rango, categ, pctResto, pctRestoBin,
         FFINALIZA, ends_with('_TIT'), caedec3d, GENERO)

vipListExp <- dfScoreFeb %>% 
  left_join(bdcVIPScore_exp, by = 'CI') %>% 
  #left_join(bdcAgoExp, by = 'CI') %>% 
  glimpse()
# This is the dataset that goes into python
write.csv(vipListExp, 'D:/backupRBS/vip/vipListRC_Final_ene2023.csv', row.names = F)
write.xlsx(vipListExp, 'C:/!bso/vipCartera/output/vipListRC_Final_ene2023.xlsx')

#==============================================================================
# final list
vipListEne <- read.csv('D:/!backupRBS/vip/vipListRC_Final_feb2023.csv')
#vipList <- vipListExp
# #-----------------------------------------------------------------------------
# Final descriptives
vipDescFull_1 <- bdcNovFeb %>% 
  select(-GENERO) %>% 
  left_join(vipListFeb, by = c('OPERACION')) %>%
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
  mutate(totalMonto = sum(montous)) %>% 
  mutate(totalSaldo = sum(saldous)) %>% 
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(pctSaldo = sum(saldous)/sum(montous)) %>% 
  mutate(vipViable = ifelse(target == 1 & pctSaldo < 0.8, 1, 0)) %>% 
  mutate(oportunity = ifelse(target == 1, totalMonto - totalSaldo, 0)) %>% 
  mutate(oportunityViable = ifelse(vipViable == 1, oportunity, 0)) %>% 
  ungroup() %>% 
  #dplyr::filter(target == 1 & vipViable == 1) %>% 
  mutate(rango = ifelse(saldous < 20000, 'menos20k', '20k+')) %>% 
  # mutate(tipoCred = case_when(substr(TIPO_CREDITO, 1,1) == 'M'~'Micro',
  #                             substr(TIPO_CREDITO, 1,1) == 'H'~'Vivienda',
  #                             substr(TIPO_CREDITO, 1,1) == 'N'~'Consumo',
  #                             substr(TIPO_CREDITO, 1,1) == 'P'~'PyMe',)) %>% 
  mutate(Sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
  mutate(Sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', Sucursal)) %>% 
  mutate(Sucursal = case_when(Sucursal == '1' ~ 'Chuquisaca',
                              Sucursal == '10' ~ 'El Alto',
                              Sucursal == '2' ~ 'La Paz',
                              Sucursal == '3' ~ 'Cochabamba',
                              Sucursal == '4' ~ 'Oruro',
                              Sucursal == '5' ~ 'Potosí',
                              Sucursal == '6' ~ 'Tarija',
                              Sucursal == '7' ~ 'Santa Cruz',
                              Sucursal == '8' ~ 'Beni',
                              Sucursal == '9' ~ 'Pando',)) %>% 
  mutate(ctileProb = ntile(prob_GC, n = 100)) %>% 
  mutate(dtileProb = ntile(prob_GC, n = 10)) %>%
  mutate(rangos = case_when(saldous < 500 ~'1. <500USD',
                            saldous > 500 & saldous <= 1000 ~'2. 500-1k',
                            saldous > 1000 & saldous <= 5000 ~'3. 1k-5k',
                            saldous > 5000 & saldous <= 10000 ~'4. 5k-10k',
                            saldous > 10000 & saldous <= 15000 ~'5. 10k-15k',
                            saldous > 15000 & saldous <= 20000 ~'6. 15k-20k',
                            saldous > 20000 ~'7. >20k')) %>%
  mutate(rangom = case_when(montous < 500 ~'1. <500USD',
                            montous > 500 & montous <= 1000 ~'2. 500-1k',
                            montous > 1000 & montous <= 5000 ~'3. 1k-5k',
                            montous > 5000 & montous <= 10000 ~'4. 5k-10k',
                            montous > 10000 & montous <= 15000 ~'5. 10k-15k',
                            montous > 15000 & montous <= 20000 ~'6. 15k-20k',
                            montous > 20000 ~'7. >20k')) %>%
  mutate(Lista_GNN = ifelse(excelenteNegocios == 1, 'En Lista', 'Fuera de Lista'),
         VIP_RC = ifelse(target==1, 'VIP-RC', 'Regular'),
         VIP_Viable = ifelse(vipViable == 1, 'Viable', 'No viable'),
         Grupo_Exclusivo = ifelse(EXCLUSIVO == 0, 'Exclusivo', 'No Exclusivo')) %>%
  select(OPERACION, CI, NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT, NOMBRE_ASESOR, AGENCIA,
         Lista_GNN, montous, caedec3d, CPOP, prob_GC, ctileProb, Grupo_Exclusivo,
         saldous, tipoCred, intus, pctSaldo, oportunity,dtileProb, SECTOR_CARTERA,
         oportunityViable, VIP_RC, VIP_Viable, Sucursal, categ, rangom,
         moraMaxBin, moraAcumBin, pctMaxBin, pctAcumBin, minMonto, maxMonto,
         totalLoanDays, pct_acumDM_TLD, pct_maxDM_acumDM, moraAcum, moraMax,
         badCreditBin, totalBC, totalWC, histStr, histStr2, GENERO) %>% 
  mutate(oportunityViable = montous - saldous) %>% 
  glimpse() %>% 
  dplyr::filter(VIP_RC == 'VIP-RC') %>% # filtro de VIPs
  glimpse() 
 
# VIPs que se mantienen al último cierre
length(unique(vipDescFull_1$CI)) 
# Operaciones que mantienen los VIPs activos al último cierre
nrow(vipDescFull_1)
  
vipDescFull_2 <- vipDescFull_1 %>% 
  dplyr::filter(oportunityViable > 1000) %>% # filtro de préstamo mínimo del BSO
  glimpse() %>% 
  dplyr::rename(Monto_USD = montous,
                Saldo_USD = saldous,
                CAEDEC_DC = caedec3d,
                Percentil_Prob = ctileProb,
                Decil_Prob = dtileProb,
                Tipo_Crédito = tipoCred,
                Interés_USD = intus,
                Saldo_s_Monto = pctSaldo,
                Oportunidad_X_Total = oportunity,
                Oportunidad_Viable = oportunityViable,
                Rango_Monto = rangom,
                Categoría = categ,
                Probabilidad_VIP = prob_GC) %>% # Rename the rest of variables to pretty names
  mutate(Score = Probabilidad_VIP * Oportunidad_Viable,
         Decil_Score = ntile(Score, n = 10),
         Amortización = 1-Saldo_s_Monto,
         Grupo_Amortización = case_when(Amortización <= 0.2 ~ '1. 20%-',
                                        Amortización > 0.2 & Amortización <= 0.4 ~ '2. 20%-40%',
                                        Amortización > 0.4 & Amortización <= 0.6 ~ '3. 40%-60%',
                                        Amortización > 0.6 & Amortización <= 0.8 ~ '4. 60%-80%',
                                        Amortización > 0.8 ~ '5. 80%+',))

# Operaciones viables (>1,000$us)
nrow(vipDescFull_2)

# Operaciones viables (>1,000$us)
nrow(vipDescFull_2[vipDescFull_2$Grupo_Amortización != '1. 20%-', ])

# This is the final list/dataframe
write.xlsx(vipDescFull_2, 'D:/backupRBS/vip/vipDescriptives_CompleteList_ene2023_vAE_allVars.xlsx')

#-------------------------------------------------------------------------------
################################################################################
#COMPRA DE CARTERA
vipDescFull_cc <- read.xlsx('D:/!backupRBS/vip/vipDescriptives_CompleteList_ene2023_vAE_allVars.xlsx') 
#===============================================================================
# infocred
bdcPrevEne <- fread('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraEne2023.txt', 
                    encoding = 'Latin-1', fill = T) %>% 
  dplyr::filter(MODULO!=131) %>%
  dplyr::filter(ESTADO!="CASTIGADA") %>%
  select(CI, OPERACION, CTACLIENTE) %>% 
  distinct_all()

infoRaw <- fread('C:/!bso/Cargamensual_Infocred/utf/BSO202212_utf8.txt', encoding = 'UTF-8')
nrowInfo <- nrow(infoRaw)

infoPerf_long <- infoRaw %>% 
  glimpse() %>% 
  mutate(carnet = paste0(`NRO DOCUMENTO`, EXT)) %>% 
  separate(NumeroOp,into = c("CTACLIENTE","OPERACION"),sep="-") %>% 
  mutate(CTACLIENTE=as.numeric(CTACLIENTE),
         OPERACION=as.numeric(OPERACION)) %>% 
  left_join(bdcPrevEne, by=c('CTACLIENTE', 'OPERACION')) %>%
  group_by(carnet) %>% 
  mutate(CI=max(CI,na.rm = T)) %>%
  ungroup() %>% 
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>%
  dplyr::filter(!str_detect(`TIPO OBLIGADO SBEF`, '4A - ')) %>%
  dplyr::filter(`SBEF CALIFICACION`=='A') %>% 
  dplyr::filter(`SIGLA SBEF` != 'BSO') %>% # check filter
  mutate(MontoOriginal =as.numeric(MontoOriginal)/6.86) %>% 
  mutate(saldoVig = as.numeric(`SBEF VIGENTE`)/6.86) %>% 
  mutate(histStr = as.character(HISTORICO)) %>% # HISTORY CONSTRUCTION
  mutate(histStr2 = substr(histStr, 1, 16)) %>% 
  mutate(histStr2 = ifelse(is.na(histStr2), '0', histStr2)) %>% 
  mutate(badCredit = ifelse((str_detect(histStr2, '2') | str_detect(histStr2, '3') |
                               str_detect(histStr2, '4')),1,0)) %>% # CHECK MEASURE
  mutate(worstCredit = ifelse(str_detect(histStr2, '4'),1,0)) %>% # CHECK MEASURE
  ungroup() %>% 
  group_by(CI) %>% # CHECK GROUPING
  mutate(totalBC = sum(badCredit, na.rm = T),
         totalWC = sum(worstCredit, na.rm = T)) %>%  
  arrange(CI) %>% 
  mutate(badCreditBin = case_when(totalBC == 0  ~ '1. 0',
                                  totalBC == 1  ~ '2. 1',
                                  totalBC == 2 ~ '3. 2',
                                  totalBC > 2 ~ '4. 2+',
                                  totalWC > 0  & totalWC < NA ~ '5. Tuvo castigo')) %>% 
  select(CI, ends_with('Bin'), totalBC, totalWC, histStr, histStr2, badCredit, worstCredit, 
         `ENTIDAD SBEF`, `SIGLA SBEF`,`TIPO CREDITO SBEF`, `TIPO OBLIGADO SBEF` ,
         `SBEF VIGENTE` , `SBEF CALIFICACION`, `FECHA DECLARACION`, MontoOriginal,
         OPERACION, CTACLIENTE) %>% 
  ungroup() %>% 
  select(CI, `ENTIDAD SBEF`, `SIGLA SBEF`,`TIPO CREDITO SBEF`, `TIPO OBLIGADO SBEF` ,
         `SBEF VIGENTE`, MontoOriginal, `SBEF CALIFICACION`, `FECHA DECLARACION` ) %>% 
  group_by(CI) %>% # CHECK GROUPING
  summarise_all(max)  

write_rds(infoPerf_long, 'D:/!backupRBS/vip/perfLongEne23.rds')  
infoPerf_long <- readRDS('D:/!backupRBS/vip/perfLongEne23.rds')  
cc <- vipDescFull_cc %>% 
  left_join(infoPerf_long, by = 'CI') %>% 
  #dplyr::filter(!is.na(`SBEF VIGENTE`)) %>% 
  dplyr::filter(`SBEF VIGENTE` > 0) %>% 
  dplyr::filter(VIP_RC == 'VIP-RC') %>% 
  group_by(CI) %>%
  arrange(desc(`SBEF VIGENTE`)) %>% 
  dplyr::filter(row_number()==1) %>%
  ungroup() %>%
  dplyr::rename(Saldo_USD_SF = `SBEF VIGENTE`,
                Calificacion_SF = `SBEF CALIFICACION`,
                Entidad = `ENTIDAD SBEF`,
                Tipo_Credito_SF = `TIPO CREDITO SBEF`,
                Sigla_Entidad_SF = `SIGLA SBEF`) %>%
  mutate(Saldo_USD_SF = Saldo_USD_SF/6.86) %>%
  dplyr::filter(Calificacion_SF == 'A')

sum(cc$Saldo_USD_SF)
table(cc$Lista_GNN)
table(cc$Calificacion_SF)

write_rds(cc, 'D:/!backupRBS/vip/tablacc.rds')

#===============================================================================
# tables
compra <- cc %>% 
  select(Lista_GNN, Saldo_USD_SF, Saldo_USD, GENERO) %>% 
  group_by(Lista_GNN, GENERO) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF), Saldo_USD = sum(Saldo_USD),
            Operaciones = n()) %>% 
  arrange(Lista_GNN, desc(Saldo_USD_SF)) %>% 
  adorn_totals('row') %>% 
  mutate(Promedio = Saldo_USD_SF/Operaciones) %>% 
  glimpse()


tipoCred <- cc %>% 
  select(Lista_GNN, VIP_Viable, Tipo_Credito_SF,  Saldo_USD_SF, Saldo_USD) %>% 
  mutate(Tipo_Credito_SF = case_when(substr(Tipo_Credito_SF,1,1) == 'M'~'Micro',
                                     substr(Tipo_Credito_SF,1,1) == 'P'~'PyMe',
                                     substr(Tipo_Credito_SF,1,1) == 'H'~'Hipotecario',
                                     substr(Tipo_Credito_SF,1,1) == 'N'~'Consumo',)) %>% 
  group_by(Lista_GNN, VIP_Viable, Tipo_Credito_SF) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF), Saldo_USD = sum(Saldo_USD),
            Operaciones = n()) %>% 
  adorn_totals('row') %>%
  mutate(Promedio = Saldo_USD_SF/Operaciones) %>% 
  arrange(Lista_GNN, VIP_Viable, desc(Saldo_USD_SF)) %>% 
  glimpse()

################################################################################
#GRÄFICOS
ent_s <- cc %>% 
  select( Sigla_Entidad_SF,  Saldo_USD_SF, Saldo_USD, Sucursal, VIP_Viable) %>%
  dplyr::filter(VIP_Viable=='Viable') %>% 
  group_by(Sigla_Entidad_SF, Sucursal) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF), Saldo_USD = sum(Saldo_USD),
            Operaciones = n()) %>% 
  dplyr::filter(Operaciones > 10) %>% 
  arrange(desc(Operaciones)) %>% 
  arrange(desc(Saldo_USD_SF)) %>% 
  select(Sigla_Entidad_SF, Saldo_USD_SF, Sucursal) %>% 
  ungroup() %>% 
  mutate(Entidad2 = ifelse(Saldo_USD_SF < 1000000, 'Otras',Sigla_Entidad_SF)) %>% 
  ungroup() %>% 
  select(-Sigla_Entidad_SF) %>% 
  group_by(Entidad2) %>% 
  mutate(saldoTot = sum(Saldo_USD_SF)) %>% 
  glimpse()

ggplot(ent_s[ent_s$Entidad2 != 'Otras',], aes(x = reorder(Entidad2, saldoTot), y = Saldo_USD_SF/1000, fill = Sucursal)) + 
  geom_bar(stat = 'identity') + coord_flip() + theme_minimal() +
  xlab('Entidad') + ylab('Saldo USD en Miles') +
  scale_y_continuous(label=comma) + scale_fill_manual(values = paleta(12)) + 
  geom_label(aes(label=comma(round(Saldo_USD_SF/1000))), color='white', size=3,
             position = position_stack(vjust = 0.5), show.legend = F) +
  scale_y_continuous(label=comma) + 
  scale_fill_manual(values = paleta(9))+ 
  theme(legend.position = 'bottom')+ guides(fill = guide_legend(nrow = 1)) 

ggsave('D:/!backupRBS/vip/oportXSuc.png')


tipoCred <- cc %>% 
  select( Sigla_Entidad_SF,  Saldo_USD_SF, Saldo_USD, Tipo_Credito_SF, VIP_Viable) %>% 
  dplyr::filter(VIP_Viable=='Viable') %>% 
  group_by(Sigla_Entidad_SF, Tipo_Credito_SF) %>% 
  mutate(Tipo_Credito_SF = case_when(substr(Tipo_Credito_SF,1,1) == 'M'~'Micro',
                                     substr(Tipo_Credito_SF,1,1) == 'P'~'PyMe',
                                     substr(Tipo_Credito_SF,1,1) == 'H'~'Hipotecario',
                                     substr(Tipo_Credito_SF,1,1) == 'N'~'Consumo',)) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF), Saldo_USD = sum(Saldo_USD),
            Operaciones = n()) %>%
  dplyr::filter(Operaciones > 10) %>% 
  arrange(desc(Operaciones)) %>% 
  arrange(desc(Saldo_USD_SF)) %>% 
  select(Sigla_Entidad_SF, Saldo_USD_SF, Tipo_Credito_SF) %>% 
  ungroup() %>% 
  mutate(Entidad2 = ifelse(Saldo_USD_SF < 1000000, 'Otras',Sigla_Entidad_SF)) %>% 
  ungroup() %>% 
  select(-Sigla_Entidad_SF) %>% 
  group_by(Entidad2, Tipo_Credito_SF) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(Entidad2) %>% 
  mutate(saldoTot = sum(Saldo_USD_SF)) %>% 
  glimpse()

ggplot(tipoCred, aes(x = reorder(Entidad2, saldoTot), y = Saldo_USD_SF/1000, fill = Tipo_Credito_SF)) + 
  geom_bar(stat = 'identity') + coord_flip() + theme_minimal() + 
  geom_label(aes(label=comma(round(Saldo_USD_SF/1000))), color='white', size=3,
             position = position_stack(vjust = 0.5),  show.legend = FALSE) +
  xlab('Entidad') + ylab('Saldo USD en Miles') +
  scale_y_continuous(label=comma) + 
  scale_fill_manual(values = paleta(12))+ 
  theme(legend.position = 'bottom')+ guides(fill = guide_legend(nrow = 1)) 


ggsave('D:/!backupRBS/vip/oportXEntidad_tipoCred.png')

ent_g <- cc %>% 
  select( Sigla_Entidad_SF,  Saldo_USD_SF, Saldo_USD, VIP_Viable) %>% 
  dplyr::filter(VIP_Viable=='Viable') %>% 
  mutate(Sigla_Entidad_SF=ifelse(!(Sigla_Entidad_SF %in% c('FIE', 'FSL', 'BME', 'BUN',
                                                           'FPR', 'FEF', 'BNB', 'BEC', 'IDI')), 'Otros', Sigla_Entidad_SF)) %>%   
  group_by(Sigla_Entidad_SF) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF), Saldo_USD = sum(Saldo_USD),
            Operaciones = n()) %>%
  dplyr::filter(Operaciones > 10) %>% 
  arrange(desc(Operaciones)) %>% 
  arrange(desc(Saldo_USD_SF)) %>% 
  select(Sigla_Entidad_SF, Saldo_USD_SF) %>% 
  glimpse()

ggplot(ent_g, aes(x = reorder(Sigla_Entidad_SF, Saldo_USD_SF), y = Saldo_USD_SF/1000)) + 
  geom_bar(stat = 'identity', fill=paleta(12)[2]) + coord_flip() + theme_minimal() +
  geom_label(aes(label=comma(Saldo_USD_SF/1000),  show.legend = FALSE, fontface='bold'), color=paleta(12)[2], size=3) +
  xlab('Entidad') + ylab('Saldo USD en Miles') +
  scale_y_continuous(label=comma)
ggsave('D:/backupRBS/vip/oportXEntidad.png')
#=============================================================================
# Joining vip & lead sets
# base sexo
bdc_I_Wanna_Have_Sex <- readRDS('C:/!bso/girCartera/rds/ec_Ene2023.rds') %>% 
  select(CI,CTACLIENTE, GENERO) %>% 
  dplyr::rename(Genero = GENERO) %>% 
  distinct_all()

ccExp <- cc %>% 
  select(CI,NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT,NOMBRE_ASESOR,
         Lista_GNN,Calificacion_SF, Saldo_USD_SF, MontoOriginal_SF=MontoOriginal,
         Entidad_SF=Entidad,Sigla_Entidad_SF, Tipo_Obligado_SF=`TIPO OBLIGADO SBEF`,
         VIP_Viable) %>%
  glimpse()


vipLead_final <- ccExp %>%
  left_join(bdc_I_Wanna_Have_Sex, by = 'CI') %>%
  glimpse()

vipPreAP <- vipDescFull_cc %>%
  left_join(ccExp, by="CI") %>% 
  left_join(bdc_I_Wanna_Have_Sex, by = 'CI') %>%
  dplyr::filter(VIP_Viable=='Viable') %>% 
  select(OPERACION,CI,NOMBRE_TIT.x,PATERNO_TIT.x,MATERNO_TIT.x,NOMBRE_ASESOR.x,
         AGENCIA, Lista_GNN=Lista_GNN.x,Monto_USD,CAEDEC_DC,CPOP, Saldo_USD,
         Tipo_Crédito,SECTOR_CARTERA, Oportunidad_Viable,Sucursal,
         Categoría,Rango_Monto,GENERO,CTACLIENTE,Score)
  
excel<-list(ClientesPreAp=vipPreAP, compraCartera=vipLead_final)


write.xlsx(vipLead_final, 'C:/!bso/vipCartera/compraCartera/output/AllLeads_final_feb23.xlsx')
write.xlsx(excel, 'D:/!backupRBS/vip/leadsEnefeb.xlsx')

leadsJoin <- vipLead_final %>% 
  select(CI, OPERACION, ends_with('_SF'))
write_rds(leadsJoin, 'C:/!bso/vipCartera/compraCartera/output/compra_dic2022.rds')

# Checking sums and table by gender
vipLead_final<-read.xlsx('C:/!bso/vipCartera/compraCartera/output/AllLeads_final_ene2023.xlsx')
check <- vipLead_final %>% 
  ungroup() %>% 
  group_by(CI) %>% 
  arrange(desc(CI)) %>% 
  mutate(Saldo_USD_SF = ifelse(row_number()>1, 0, Saldo_USD_SF)) %>% 
  ungroup() %>% 
  select(Saldo_USD_SF, Oportunidad_Viable, VIP_Viable, Genero) %>% 
  mutate(Op_Viable = ifelse(Oportunidad_Viable>0,1,0),
         Op_Compra = ifelse(Saldo_USD_SF>0,1,0)) %>% 
  group_by(VIP_Viable, Genero) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF, na.rm = T),
            Oportunidad_Viable = sum(Oportunidad_Viable, na.rm = T),
            N_Ops_Preaprobadas = sum(Op_Viable, na.rm = T),
            N_Ops_Compra = sum(Op_Compra, na.rm = T)) %>% 
  adorn_totals('row') %>% 
  glimpse()

write.xlsx(check, 'C:/!bso/vipCartera/compraCartera/output/tabla_genero_ene2023.xlsx')

check2 <- vipLead_final %>% 
  ungroup() %>% 
  group_by(CI) %>% 
  arrange(desc(CI)) %>% 
  mutate(Saldo_USD_SF = ifelse(row_number()>1, 0, Saldo_USD_SF)) %>% 
  ungroup() %>% 
  select(Saldo_USD_SF, Oportunidad_Viable, VIP_Viable, Genero) %>% 
  mutate(Op_Viable = ifelse(Oportunidad_Viable>0,1,0),
         Op_Compra = ifelse(Saldo_USD_SF>0,1,0)) %>% 
  group_by(VIP_Viable, Genero) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF, na.rm = T),
            Oportunidad_Viable = sum(Oportunidad_Viable, na.rm = T),
            N_Ops_Preaprobadas = sum(Op_Viable, na.rm = T),
            N_Ops_Compra = sum(Op_Compra, na.rm = T)) %>% 
  adorn_totals('row') %>% 
  glimpse()

write.xlsx(check, 'C:/!bso/vipCartera/compraCartera/output/tabla_genero_ene2023.xlsx')
################################################################################
allLeads<-readRDS('C:/!bso/vipCartera/compraCartera/output/AllLeads_final_ene23.rds')

all_1<-vipLead_final %>% 
  dplyr::filter(VIP_Viable=='Viable') %>% 
  select(-VIP_RC, -VIP_Viable, -Probabilidad_VIP, -Percentil_Prob, -Grupo_Exclusivo,
         -Interés_USD,	-Saldo_s_Monto,	-Oportunidad_X_Total,	-Decil_Prob,
         -moraMaxBin,	-moraAcumBin,	-pctMaxBin,	-pctAcumBin,	-minMonto,
         -maxMonto,	-totalLoanDays,	-pct_acumDM_TLD,	-pct_maxDM_acumDM,	
         -moraAcum,	-moraMax,	-badCreditBin,	-totalBC,	-totalWC,	-histStr,
         -Decil_Score,	-Amortización,	-Grupo_Amortización,		-Saldo_USD_SF,
  )

all_2 <- vipLead_final %>% 
  dplyr::filter(Saldo_USD_SF!= 'NA') %>% 
  dplyr::select(CI,	NOMBRE_TIT,	PATERNO_TIT,	MATERNO_TIT,	NOMBRE_ASESOR,
                Lista_GNN, ends_with('_SF'), Genero) %>% 
  group_by(CI) %>% 
  summarise_all(max, na.rm=T)

all<-list(aprobViables=all_1, LeadsCompra=all_2)
write.xlsx(all, 'C:/!bso/vipCartera/compraCartera/output/TablasFeb23.xlsx')

compraTab <- vipLead_final %>% 
  dplyr::filter(Saldo_USD_SF!= 'NA') %>% 
  dplyr::select(CI,	NOMBRE_TIT,	PATERNO_TIT,	MATERNO_TIT,	NOMBRE_ASESOR,
                Lista_GNN, ends_with('_SF'), Genero, Saldo_USD, Saldo_USD_SF) %>% 
  group_by(CI, Genero) %>% 
  summarise_all(max, na.rm=T) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  select(Genero, Saldo_USD, Saldo_USD_SF) %>% 
  group_by(Genero) %>%
  summarise(Saldo_USD=sum(Saldo_USD),
            Saldo_USD_SF=sum(Saldo_USD_SF),
            Operaciones=n()) %>% 
  adorn_totals('row') %>% 
  mutate(Promedio=Saldo_USD_SF/Operaciones)

write.xlsx(compraTab,'C:/!bso/vipCartera/reportes/output/compraTabEne.xlsx')

################################################################################
###GRAFICOS
gph_1 <- cc %>% 
  select(Lista_GNN, Tipo_Credito_SF,  Saldo_USD_SF, Saldo_USD, GENERO, VIP_Viable) %>%
  dplyr::filter(VIP_Viable='Viable') %>% 
  mutate(Tipo_Credito_SF = case_when(substr(Tipo_Credito_SF,1,1) == 'M'~'Micro',
                                     substr(Tipo_Credito_SF,1,1) == 'P'~'PyMe',
                                     substr(Tipo_Credito_SF,1,1) == 'H'~'Hipotecario',
                                     substr(Tipo_Credito_SF,1,1) == 'N'~'Consumo',)) %>% 
  group_by(Lista_GNN, GENERO, Tipo_Credito_SF) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF), Saldo_USD = sum(Saldo_USD),
            Operaciones = n()) %>% 
  adorn_totals('row') %>%
  mutate(Promedio = Saldo_USD_SF/Operaciones) %>% 
  arrange(Lista_GNN, desc(Saldo_USD_SF)) %>% 
  glimpse()
kable(gph_1, row.names = F, format = 'latex', booktabs = T,
      digits = 0, format.args = list(decimal.mark = ".", big.mark = ","))
write.xlsx(gph_1, 'C:/!bso/vipCartera/compraCartera/output/compra_tab_2.xlsx')

ent_g <- cc %>% 
  select( Sigla_Entidad_SF,  Saldo_USD_SF, Saldo_USD, VIP_Viable) %>% 
  dplyr::filter(VIP_Viable=='Viable') %>% 
  mutate(Sigla_Entidad_SF=ifelse(!(Sigla_Entidad_SF %in% c('FIE', 'FSL', 'BME', 'BUN',
                                                           'FPR', 'FEF', 'BNB', 'BEC', 'IDI')), 'Otros', Sigla_Entidad_SF)) %>%   
  group_by(Sigla_Entidad_SF) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF), Saldo_USD = sum(Saldo_USD),
            Operaciones = n()) %>%
  dplyr::filter(Operaciones > 10) %>% 
  arrange(desc(Operaciones)) %>% 
  arrange(desc(Saldo_USD_SF)) %>% 
  select(Sigla_Entidad_SF, Saldo_USD_SF) %>% 
  glimpse()
write.xlsx(gph, 'C:/!bso/vipCartera/entidad_g.xlsx')

ggplot(ent_g, aes(x = reorder(Sigla_Entidad_SF, Saldo_USD_SF), y = Saldo_USD_SF/1000)) + 
  geom_bar(stat = 'identity', fill=paleta(12)[2]) + coord_flip() + theme_minimal() +
  geom_label(aes(label=comma(Saldo_USD_SF/1000),  show.legend = FALSE, fontface='bold'), color=paleta(12)[2], size=3) +
  xlab('Entidad') + ylab('Saldo USD en Miles') +
  scale_y_continuous(label=comma)

ggsave('D:/!backupRBS/vip/oportXEntidad.png')
################################################################################
#variacion mensual
vipReport_1 <- read_xlsx('C:/!bso/vipCartera/output/vipDescriptives_CompleteList_Nov2022_allVars.xlsx') 
vipReport_2 <- read_xlsx('C:/!bso/vipCartera/output/vipDescriptives_CompleteList_Dic2022_allVars.xlsx') 


var_exit<-vipReport_1 %>% 
  dplyr::filter(!(OPERACION %in% vipReport_2$OPERACION)) %>%
  dplyr::filter(VIP_Viable=='Viable') %>%
  select(Rango_Monto, Sucursal, Lista_GNN, Tipo_Crédito, CI, OPERACION)

write.csv(var_exit, 'C:/!bso/excel/TablaSalida.csv')
write.xlsx(var_exit, 'C:/!bso/excel/TablaSalida.xlsx')


var_entry<-vipReport_2 %>% 
  dplyr::filter(!(OPERACION %in% vipReport_1$OPERACION))

################################################################################
#===============================================================================
# Implementar un esquema de monitoreo y seguimiento

# Reading probabilities from python
dfScore_dic <- read.csv('C:/!bso/vipCartera/output/vipList_wScores_ene2023.csv') %>% 
  mutate(mes=as.Date('2023-12-31')) %>% 
  mutate(Rango_Monto= case_when(totalMonto < 5000~ '1. 0K-5k',
                                totalMonto >= 5000 & totalMonto < 10000~'2. 5k-10k',
                                totalMonto >= 10000 & totalMonto < 15000~'3. 10k-15k',
                                totalMonto >= 15000 & totalMonto < 20000~'4. 15k-20k',
                                totalMonto >=20000 ~'5. Mayor a 20k'))

dfScore_nov <- read.csv('C:/!bso/vipCartera/output/vipList_wScores_dic2022.csv')%>% 
  mutate(mes=as.Date('2022-12-31')) %>% 
  mutate(Rango_Monto= case_when(totalMonto < 5000~ '1. 0K-5k',
                                totalMonto >= 5000 & totalMonto < 10000~'2. 5k-10k',
                                totalMonto >= 10000 & totalMonto < 15000~'3. 10k-15k',
                                totalMonto >= 15000 & totalMonto < 20000~'4. 15k-20k',
                                totalMonto >=20000 ~'5. Mayor a 20k'))
delta<-dfScore_dic %>% 
  bind_rows(dfScore_nov) %>% 
  select(CI, target, mes, Rango_Monto) %>% 
  group_by(CI) %>% 
  mutate(cambio=sum(target)) %>% 
  dplyr::filter(cambio==1) %>% 
  arrange(CI, mes) %>% 
  mutate(tipo=ifelse(dplyr::lag(target)>target, 'Salida', 'Entrada')) %>% 
  dplyr::filter(!is.na(tipo))

table(delta$tipo)

leavers<-delta %>% 
  dplyr::filter(tipo=='Salida')  

leavers_1 <-dfScore_dic %>% 
  bind_rows(dfScore_nov) %>% 
  dplyr::filter(CI %in% leavers$CI) %>% 
  group_by(CI) %>% 
  arrange(CI, mes) %>% 
  mutate(moraIM=ifelse(dplyr::lag(maxMoraIM_cl)<maxMoraIM_cl, 1, 0), 
         moraIM=sum(moraIM, na.rm = T))

summary<-delta %>% 
  select(Rango_Monto, tipo, CI) %>% 
  group_by(Rango_Monto, tipo) %>% 
  tally() %>% 
  adorn_totals('row')
  
write.csv(summary, 'C:/!bso/vipCartera/reportes/output/TablaClientes.csv')

table(listaTotal$moraIM)
# 0    1 
# 740 3113 
# 3113/3853 clientes salieron de la lista VIP por tener mora intra mes

leavers_2 <-leavers_1 %>% 
  dplyr::filter(moraIM==0) %>% 
  dplyr::filter(min(montoRat) <2)
nrow(leavers_2)/2

leavers_3 <-leavers_1 %>% 
  dplyr::filter(moraIM==0) %>% 
  dplyr::filter(min(montoRat) > 2) %>% 
  mutate(moraSF=ifelse(dplyr::lag(totalBC)<totalBC, 1, 0), 
         moraSF=sum(moraSF, na.rm = T)) %>% 
  dplyr::filter(moraSF==1)

nrow(leavers_3)/2

entries<-delta %>% 
  dplyr::filter(tipo=='entrada')

entries_1 <-dfScore_dic %>% 
  bind_rows(dfScore_nov) %>% 
  dplyr::filter(CI %in% entries$CI) %>% 
  group_by(CI) %>% 
  arrange(CI, mes)%>% 
  mutate(moraIM=ifelse(dplyr::lag(maxMoraIM_cl)<maxMoraIM_cl, 1, 0), 
         moraIM=sum(moraIM, na.rm = T))
################################################################################
##CLIENTES VIP INACTIVOS
###ESTO VA AL INFORME
#lista de VIPS no activos al ultimo cierre
dfCancelEne<-readRDS('C:/!bso/vipCartera/dfCancelEne15Ene23.rds') %>%  #Me la pasa el Gabo
mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO)))

vipListEne <- read.csv('D:/backupRBS/vip/vipListRC_Final_ene2023.csv')

vipMissEne<-vipListEne %>% 
  dplyr::filter(target==1) %>% 
  dplyr::filter(!(CI %in% bdcNovEne$CI)) %>% 
  left_join(dfCancelFeb, by=c('CI', 'CTACLIENTE')) %>% 
  group_by(CI) %>% 
  arrange(CI,fdes) %>% 
  dplyr::filter(yearCancel>=2022) %>% 
  select(CI, fdes, montous, sucursal) %>% 
  arrange(CI, desc(fdes)) %>% 
  dplyr::filter(row_number()==1) %>% 
  dplyr::rename(lastMonto=montous) %>% 
  left_join(vipListEne, by='CI') %>% 
  mutate(Rango_Monto= case_when(lastMonto < 5000~ '1. 0K-5k',
                                lastMonto >= 5000 & lastMonto < 10000~'2. 5k-10k',
                                lastMonto >= 10000 & lastMonto < 15000~'3. 10k-15k',
                                lastMonto >= 15000 & lastMonto < 20000~'4. 15k-20k',
                                lastMonto >=20000 ~'5. Mayor a 20k')) %>% 
  mutate(lastMonto=lastMonto/1000, 
         totalMonto=totalMonto/1000)

tabF<-vipMissEne %>% 
  ungroup() %>% 
  select(excelenteNegocios, GENERO, totalMonto, totalNops, lastMonto) %>%
  group_by(excelenteNegocios, GENERO) %>% 
  summarise_all(sum,na.rm=T) %>%
  mutate(excelenteNegocios=ifelse(excelenteNegocios==1,'En Lista', 'Fuera de Lista')) %>% 
  arrange(excelenteNegocios, GENERO) %>%
  mutate(GENERO=ifelse(GENERO=='F', 'Femenino', 'Masculino')) %>% 
  adorn_totals('row')

write.xlsx(tabF, 'D:/!backupRBS/vipCartera/vipCanceladosFeb.xlsx')

tabR<-vipMissEne %>% 
  ungroup() %>% 
  select(excelenteNegocios, GENERO, totalMonto, totalNops, lastMonto, Rango_Monto) %>%
  group_by(excelenteNegocios, GENERO, Rango_Monto) %>% 
  summarise_all(sum,na.rm=T) %>%
  mutate(excelenteNegocios=ifelse(excelenteNegocios==1,'En Lista', 'Fuera de Lista')) %>% 
  arrange(excelenteNegocios, Rango_Monto, GENERO) %>%
  mutate(GENERO=ifelse(GENERO=='F', 'Femenino', 'Masculino')) 
  #adorn_totals('row') %>% 

gph<-tabR %>% 
  ungroup() %>% 
  select(totalMonto, GENERO, Rango_Monto) %>% 
  group_by(Rango_Monto, GENERO) %>% 
  summarise_all(sum,na.rm=T) 
  
ggplot(gph, aes(x = reorder(Rango_Monto, totalMonto), y = totalMonto, fill=GENERO)) + 
  geom_bar(stat = 'identity') + coord_flip() + theme_minimal() +
  xlab('Rango Monto') + ylab('Monto Total USD en Miles') +
  scale_y_continuous(label=comma) + scale_fill_manual(values = paleta(12)) + 
  geom_label(aes(label=comma(totalMonto)), color='white', size=3,
             position = position_stack(vjust = 0.5),  show.legend = FALSE) +
  scale_y_continuous(label=comma) + 
  scale_fill_manual(values = paleta(9))+ 
  theme(legend.position = 'bottom')+ guides(fill = guide_legend(nrow = 1)) 

ggsave('D:/!backupRBS/vip/NoactivosRango.png')

write_rds(tabR, 'C:/!bso/vipCartera/output/vipRangoCancelados.rds')

tabSu<-vipMissEne %>% 
  ungroup() %>% 
  select(excelenteNegocios, GENERO, totalMonto, totalNops, lastMonto, sucursal) %>%
  group_by(excelenteNegocios, GENERO, sucursal) %>% 
  summarise_all(sum,na.rm=T) %>%
  mutate(excelenteNegocios=ifelse(excelenteNegocios==1,'En Lista', 'Fuera de Lista')) %>% 
  arrange(excelenteNegocios, sucursal, GENERO) %>%
  mutate(GENERO=ifelse(GENERO=='F', 'Femenino', 'Masculino')) %>%
  adorn_totals('row') %>% 
  select(-excelenteNegocios)

gphSuc<-tabSu %>%
  dplyr::filter(GENERO!='-') %>% 
  ungroup() %>% 
  select(totalMonto, GENERO, sucursal) %>% 
  group_by(sucursal, GENERO) %>% 
  summarise_all(sum,na.rm=T) 

ggplot(gphSuc, aes(x = reorder(sucursal, totalMonto), y = totalMonto, fill=GENERO)) + 
  geom_bar(stat = 'identity') + coord_flip() + theme_minimal() +
  xlab('Sucursal') + ylab('Monto Total USD en Miles') +
  scale_y_continuous(label=comma) + scale_fill_manual(values = paleta(12)) + 
  geom_label(aes(label=comma(totalMonto)), color='white', size=3,
             position = position_stack(vjust = 0.3), show.legend = FALSE) +
  scale_y_continuous(label=comma) + 
  scale_fill_manual(values = paleta(7))+ 
  theme(legend.position = 'bottom')+ guides(fill = guide_legend(nrow = 1)) 

ggsave('D:/!backupRBS/vip/NoactivosSuc.png')


write_rds(tabSu, 'C:/!bso/vipCartera/output/vipSucCancelados.rds')

################################################################################
vipListEne_2<-vipListEne %>% 
  mutate(mes='Ene')

vipListFeb_2<-vipListFeb %>% 
  select(CI) %>% 
  mutate(mes='Feb') %>% 
  right_join(vipListEne, by='CI')

