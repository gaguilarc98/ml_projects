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
library(openxlsx)
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
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____READING FEATURES____####
dfTotal <- readRDS('D:/!bso/features/Clientes_Ene15Jun23.rds') %>% 
  glimpse()
dfOps <- readRDS('D:/!bso/features/Historial_Operaciones.rds') %>% 
  glimpse()
#Obteniendo un CI y GENERO único por CTACLIENTE
dfTotal <- dfTotal %>% 
  group_by(CTACLIENTE) %>% 
  arrange(desc(monDate)) %>% 
  mutate(CI = CI[row_number()==1]) %>% 
  mutate(GENERO = GENERO[row_number()==1]) %>% 
  ungroup() %>% 
  # dplyr::filter(fdes < as.Date("2023-03-01")) %>% 
  select(CTACLIENTE, CI, GENERO) %>% 
  distinct_all() %>% 
  glimpse() 

####____MEASURES OF PERFORMANCE BY CLIENT____####
lastmonth <- "Jun. 2023"
shortmonth <- str_replace(lastmonth,". ","")

n_distinct(dfOps[dfOps$fdes>=as.Date("2015-01-01") & dfOps$monDate==lastmonth,]$CTACLIENTE)
dfMeasures <- dfOps %>% 
  select(CTACLIENTE, OPERACION, monDate, fdes, DIASMORA, MONTOUS) %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate >= 'ene. 2021') %>% 
  dplyr::filter(monDate <= lastmonth) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  arrange(desc(monDate)) %>% 
  mutate(fdes = min(fdes)) %>% #Se cambia la fecha de desembolso a la más antigua
  # dplyr::filter(fdes>=as.Date("2015-01-01")) %>% #Filtro de desembolsos a partir de 2015 YA NO APLICA
  mutate(moraMax = max(DIASMORA, na.rm = T)) %>% 
  mutate(MONTOUS = sum(unique(MONTOUS), na.rm = T)) %>% 
  dplyr::filter(row_number()==1) %>% #Nos quedamos con su último mes
  ungroup() %>% 
  mutate(lastM = as.Date(monDate, frac = 1)) %>% #último mes en las bdc
  mutate(loanDays = as.integer(lastM - fdes)) %>% #días de préstamo
  mutate(isinLast = ifelse(monDate==lastmonth,1,0)) %>% #está en el último mes
  glimpse() %>% 
  group_by(CTACLIENTE) %>% 
  mutate(totalLoanDays = sum(loanDays, na.rm = T)) %>% #Total de días de préstamo 
  mutate(moraAcum = sum(moraMax, na.rm = T)) %>% #Mora acumulada
  mutate(moraMax = max(moraMax, na.rm = T)) %>% #Mora máxima
  mutate(totalMonto = sum(MONTOUS, na.rm = T)) %>% #Monto total desembolsado
  mutate(maxMonto = max(MONTOUS, na.rm = T)) %>% #Máximo monto
  mutate(minMonto = min(MONTOUS, na.rm = T)) %>% #Mínimo monto
  mutate(fdesMax_Min = ifelse(max(fdes[MONTOUS==maxMonto])>min(fdes[MONTOUS==minMonto]),1,0)) %>% #Si el monto máximo fue desembolsado antes del mínimo
  # mutate(firstMonto = min(MONTOUS[fdes==min(fdes)])) %>% #Monto de primer desembolso
  # mutate(lastMonto = max(MONTOUS[fdes==max(fdes)])) %>% #Monto de último desembolso
  # mutate(montoRat = lastMonto/firstMonto) %>% #Ratio de monto último y primero
  mutate(montoRat = ifelse(minMonto>0, maxMonto/minMonto, 1)) %>% # OLD VERSION OF montoRat
  mutate(pct_maxDM_acumDM = ifelse(moraAcum > 0, moraMax/moraAcum, 0)) %>% #Porcentaje de días mora acumulados
  mutate(pct_acumDM_TLD = ifelse(totalLoanDays > 0, moraAcum/totalLoanDays, 0)) %>% #Porcentaje de días mora acumulados respecto de TLD
  mutate(pct_acumDM_TLD = ifelse(pct_acumDM_TLD > 1, 1, pct_acumDM_TLD)) %>% #Arreglo de la medida anterior
  mutate(totalNops = n_distinct(OPERACION)) %>% #Total de operaciones con BSO
  select(-lastM, -DIASMORA, -fdes, -monDate, -OPERACION, -MONTOUS, -loanDays) %>% 
  summarise_all(max, na.rm = T) %>% # check grouping
  ungroup() %>% 
  mutate(moraMaxBin = cut(moraMax,breaks=c(-Inf, 0, 5, 10, 15, 20, Inf),
                          labels=c('1. 0 días','2. 1-5 días','3. 6-10 días','4. 11-15 días','5. 16-20 días','6. 20+ días'))) %>% 
  mutate(moraAcumBin = cut(moraAcum,breaks=c(-Inf, 0, 5, 10, 15, 20, Inf),
                           labels=c('1. 0 días','2. 1-5 días','3. 6-10 días','4. 11-15 días','5. 16-20 días','6. 20+ días'))) %>% 
  mutate(pctMaxBin = cut(pct_maxDM_acumDM, breaks = c(-Inf, 0, 0.7, 0.8, 0.9, Inf),
                         labels = c('5. 0 (sin morosidad)','4. 1%-70%','3. 70%-80%','2. 80%-90%','1. 90% +'))) %>% 
  mutate(pctAcumBin = cut(pct_acumDM_TLD, breaks = c(-Inf, 0, 0.03, 0.05, 0.07, Inf),
                          labels = c('1. 0%', '2. 1%-3%', '3. 3%-5%', '4. 5%-7%', '5. 7% +'))) %>% 
  mutate(tldBin = cut(totalLoanDays, breaks = c(-Inf, 360, 1080, 2160, 3240, Inf),
                      labels = c('0. Menos de 360 días', '1. 360-1080 días', '2. 1080-2160 días', '3. 2160-3240 días', '4. 3240 +'))) %>% 
  mutate(montoRatBin = cut(montoRat, breaks = c(-Inf, 1, 2, 4, Inf),
                           labels = c('1. Sin crecimiento', '2. Hasta 2x', '3. 2x-4x', '4. 4x +'))) %>% 
  mutate(across(ends_with('Bin'), ~as.character(.x))) 

dfVIP <- dfMeasures %>% 
  left_join(dfTotal, by=c("CTACLIENTE"))

remove(dfMeasures)
saveRDS(dfMeasures,paste0("D:/!bso/vipCartera/dfVIP_",shortmonth,".rds"))
dfVIP <- readRDS(paste0("D:/!bso/vipCartera/dfVIP_",shortmonth,".rds"))
####____AGREGANDO DATOS DE INFOCRED____####
lastcred <- "Abr. 2023" #Se selecciona un mes anterior al cierre de cartera
shortcred <- str_replace(lastcred,". ","")
#La última información de infocred tiene un mes de rezago

infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',shortcred,'.rds'))
infoCheck2 <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/califClientes/process/comp_',shortcred,'.rds'))
#Se obtienen medidas de rendimiento del cliente con bases en la historia de 
#sus créditos fuera del banco
infoClientes <- infoCheck %>% 
  dplyr::filter(REGULADO=="SBEF") %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>%  #Para conservar solo deudores
  mutate(esBSO = ifelse(SIGLA=='BSO',1,0)) %>%
  mutate(noesBSO = ifelse(SIGLA!='BSO',1,0)) %>%
  mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
  group_by(CI) %>%
  dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  mutate(CTACLIENTE = max(CTACLIENTE[esBSO==1])) %>% #Le asignamos la CTACLIENTE dentro de BSO a los créditos fuera de BSO
  ungroup() %>% 
  group_by(CTACLIENTE, esBSO) %>%
  mutate(maxSaldo = ifelse(saldo==max(saldo),1,0)) %>% 
  mutate(peorCalif = ifelse(CALIFICACION==max(CALIFICACION),1,0)) %>% 
  mutate(totalBC = str_count(histLast16, "2") + str_count(histLast16, "3") + str_count(histLast16, "4")) %>% 
  mutate(totalWC = str_count(histLast16, "4")) %>%  
  # mutate(worstLast = max(histLast1)) %>% 
  mutate(badCreditBin = case_when(totalBC == 0  ~ '1. 0',
                                  totalBC == 1  ~ '2. 1',
                                  totalBC == 2 ~ '3. 2',
                                  totalBC > 2 ~ '4. 2+',
                                  !is.na(totalWC) & totalWC > 0 ~ '5. Tuvo castigo')) %>% 
  mutate(across(c(totalBC:badCreditBin),~max(.x))) %>% 
  ungroup() %>% 
  group_by(CTACLIENTE) %>%
  mutate(totalBC = max(totalBC[esBSO==0], na.rm=T)) %>% 
  mutate(totalWC = max(totalWC[esBSO==0], na.rm=T)) %>% 
  mutate(badCreditBin = max(badCreditBin[esBSO==0], na.rm=T)) %>% 
  mutate(Peor_Calificacion_SF = ifelse(peorCalif==1 & esBSO==0, CALIFICACION,'_')) %>% #Obtenemos la peor calificación del cliente 
  mutate(N_Cred_SF = sum(noesBSO)) %>% 
  mutate(Sigla_SF = ifelse(maxSaldo==1 & esBSO==0, SIGLA,'_')) %>% 
  mutate(Entidad_SF = ifelse(maxSaldo==1 & esBSO==0, ENTIDAD,'_')) %>% 
  mutate(Tipo_Obligado_SF = ifelse(maxSaldo==1 & esBSO==0, TIPO_OBLIGADO, '_')) %>%
  mutate(Saldo_USD_SF = sum(saldo*noesBSO, na.rm=T)) %>% 
  mutate(Saldo_Vig_USD_SF = sum(saldoVig*noesBSO, na.rm=T)) %>% 
  mutate(MontoOriginal_SF = sum(MontoOriginal*noesBSO, na.rm=T)) %>%
  mutate(across(totalBC:Tipo_Obligado_SF, ~ max(.x, na.rm=T))) %>% 
  dplyr::filter(Peor_Calificacion_SF != '_' & SIGLA=="BSO") %>% 
  mutate(across(Saldo_USD_SF:MontoOriginal_SF,~.x/n())) %>% 
  ungroup() %>% 
  select(OPERACION, CTACLIENTE, badCreditBin_old = badCreditBin) %>% 
  # mutate(EXCLUSIVO = 0) %>% 
  glimpse()

infoClean  <- infoClientes %>% 
  select(CTACLIENTE, badCreditBin_old) %>% 
  group_by(CTACLIENTE) %>% 
  summarise_all(max)

HistCalif <- readRDS("D:/!bso/vipCartera/historic/HistCalif_May2023.rds")
HistSummary <- HistCalif %>% 
  dplyr::filter(FECHA <= "may. 2023") %>% 
  select(CTACLIENTE, CALIFICACION_SF, ESTADO_HIST_SF) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(badCredit = ifelse(ESTADO_HIST_SF %in% c(2,3,4),1,0)) %>% 
  mutate(worstCredit = ifelse(ESTADO_HIST_SF == 4,1,0)) %>% 
  summarise(PeorCalif = max(CALIFICACION_SF), PeorEstadoHist = max(ESTADO_HIST_SF),
            totalBC = sum(badCredit), totalWC = sum(worstCredit)) %>% 
  mutate(PeorCalif = cases(PeorCalif,levs = c(0,1,2,3,4,5,6), 
                           values = c('_','A','B','C','D','E','F'))) %>% 
  mutate(PeorEstadoHist = cases(PeorEstadoHist,levs = c(-1,0,1,2,3,4), 
                                values = c('SIN DATOS','CONTINGENTE','VIGENTE','VENCIDA','EJECUCION','CASTIGADA'))) %>% 
  mutate(badCreditBin = case_when(!is.na(totalWC) & totalWC > 0 ~ '5. Tuvo castigo',
                                  totalBC > 2 ~ '4. 2+',
                                  totalBC == 2 ~ '3. 2',
                                  totalBC == 1  ~ '2. 1',
                                  totalBC == 0  ~ '1. 0',)) 
  
####____JOIN INFOCRED WITH BDCVIP____####
VIPJoin <- dfMeasures %>% 
  left_join(HistSummary,by = "CTACLIENTE")
remove(HistCalif)
gc()
####____DAILY DELINQUENCY DATA____####
#CI, OPERACION, CTACLIENTE,maxMoraIM_cl,maxMoraIM_op
####PARA JUNIO SE TRABAJO CON SF18, pero con MIM de las bases de cartera diarias
system.time(moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Jun2023.rds")) %>% 
              dplyr::filter(monDate!="jun. 2023") %>% 
  dplyr::filter(!is.na(maximaDPD)) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(mesDiasMora = if_else(maximaDPD==0, 2014, year(monDate))) %>%
  mutate(tuvoMora = if_else(maximaDPD>0, 1, 0)) %>% 
  summarise(maxMoraIM_cl = max(maximaDPD),
            meanMoraIM_cl = mean(maximaDPD),
            tuvoMora_pct = sum(tuvoMora)/n(),
            lastMoraIM_cl = max(mesDiasMora)) %>% #lastMoraIM_cl = max(mesDiasMora)
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días')) %>% 
  ungroup() %>% 
    mutate(meanMoraIMclBin = case_when(meanMoraIM_cl == 0 ~ '1. 0 días',
                                       meanMoraIM_cl <= 1 ~ '2. <= 1 día',
                                       meanMoraIM_cl <= 5 ~ '3. 1 - 5 días',
                                       meanMoraIM_cl <= 10 ~ '4. 5 - 10 días',
                                       meanMoraIM_cl <= 30 ~ '5. 10 - 30 días',
                                       meanMoraIM_cl > 30 ~ '6. >30 días',)) %>% 
    mutate(tuvoMoraIMclBin = case_when(tuvoMora_pct == 0 ~ '1. 0 %',
                                       tuvoMora_pct <= 0.05 ~ '2. 1-5 %',
                                       tuvoMora_pct <= 0.10 ~ '3. 5-10 %',
                                       tuvoMora_pct <= 0.15 ~ '4. 10-15 %',
                                       tuvoMora_pct <= 0.20 ~ '5. 15-20 %',
                                       tuvoMora_pct > 0.20 ~ '6. >20%')) %>% 
  glimpse()
)

# daily <- readRDS(paste0("D:/!bso/vipCartera/dailyMM/dailyMM_",shortmonth,".rds"))

VIPJoin_DailyVal <- VIPJoin %>% 
  left_join(moraIntraMes, by = 'CTACLIENTE') %>% 
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días',)) %>%
  mutate(meanMoraIMclBin = case_when(meanMoraIM_cl == 0 ~ '1. 0 días',
                                     meanMoraIM_cl <= 1 ~ '2. <= 1 día',
                                     meanMoraIM_cl <= 5 ~ '3. 1 - 5 días',
                                     meanMoraIM_cl <= 10 ~ '4. 5 - 10 días',
                                     meanMoraIM_cl <= 30 ~ '5. 10 - 30 días',
                                     meanMoraIM_cl > 30 ~ '6. >30 días',)) %>% 
  glimpse()

saveRDS(VIPJoin_DailyVal, "D:/!bso/vipCartera/vipJoin_dailyVal.rds")
VIPJoin_DailyVal <- readRS("D:/!bso/vipCartera/vipJoin_dailyVal.rds")
####____LISTA DE CLIENTES EXCELENTES DE NEGOCIOS____####
# Lista excelentes para Negocios
bus <- read.delim('D:/!bso/vipCartera/excelentes/Clientes_excelentes_202306.csv',
                  sep = '|') %>%
  dplyr::rename(CTACLIENTE = Cuenta) %>% 
  select(CTACLIENTE) %>% 
  distinct() %>% 
  mutate(excelenteNegocios = 1)
length(unique(bus$CTACLIENTE))

####____FILLING MISSING VALUES, DEFINING TARGET AND EXPORTING TO PYTHON____####
finalVIP <- VIPJoin_DailyVal %>% 
  left_join(bus, by = 'CTACLIENTE') %>% 
  replace_na(list(totalBC=0, totalWC=0, maxMoraIM_cl=0, excelenteNegocios=0, EXCLUSIVO=1)) %>% 
  #isinLast==1 es cliente activo con historia desde 2015 tomando su fdes más antigua. YA NO APLICA
  mutate(CR_1_Mora = ifelse(moraMax==0,1,0)) %>% 
  mutate(CR_2_MoraIM = ifelse(is.na(maxMoraIMclBin) | maxMoraIMclBin %in% c("1. 0 días"),1,0)) %>% 
  mutate(CR_3_Hist = ifelse(is.na(badCreditBin) | badCreditBin %in% c("1. 0"),1,0)) %>% 
  mutate(CR_4_LoanDays = ifelse(tldBin %in% c("2. 1080-2160 días", "3. 2160-3240 días", "4. 3240 +"),1,0)) %>% 
  mutate(CR_5_Monto = ifelse(montoRatBin %in% c("3. 2x-4x","4. 4x +") & fdesMax_Min==1,1,0)) %>% 
  mutate(target = ifelse(isinLast==1 & totalNops > 1 &
                           CR_1_Mora==1 & CR_2_MoraIM==1 & CR_3_Hist==1 & CR_4_LoanDays==1 & 
                           CR_5_Monto==1, 1, 0 )) %>%  # Este es el ifelse que identifica al cliente VIP
  select(-ends_with('Bin'), -starts_with('histStr'), -ends_with('Credit'))

bdcVIPScore <- finalVIP %>% 
  select(CTACLIENTE, totalMonto, minMonto, maxMonto, totalNops, montoRat, totalLoanDays,
         pct_acumDM_TLD, pct_maxDM_acumDM, moraAcum, moraMax, totalBC, totalWC,
         maxMoraIM_cl, excelenteNegocios, target) %>% 
  glimpse()

table(bdcVIPScore$target)

write.csv(bdcVIPScore, paste0('D:/!bso/vipCartera/dataScore_',shortmonth,'.csv'), row.names = F)
#CON ESTE RESULTADO SE CALCULAN LOS SCORES EN PYTHON
#UNA VEZ SE EJECUTE EL RESULTADO EN PYTHON SE RETOMA A PARTIR DE AQUÍ

dfScore <- fread(paste0('D:/!bso/vipCartera/scores/vipList_wScores_',shortmonth,'.csv'), sep = ',', fill = T) %>% 
  select(CTACLIENTE, prob_GC, goodClient)

vipListExp <- dfScore %>% 
  left_join(finalVIP, by = 'CTACLIENTE') %>% 
  glimpse()
saveRDS(vipListExp, paste0("D:/!bso/vipCartera/vipListwScores_",shortmonth,".rds"))
vipListExp <- readRDS(paste0("D:/!bso/vipCartera/vipListwScores_",shortmonth,".rds"))

####____LEADS PREAPROBADOS CON EL ÚLTIMO CIERRE MENSUAL____####
bdcLast <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',shortmonth,'.rds')) %>% 
  dplyr::filter(!MODULO %in% c(131,29)) %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  mutate(montous = ifelse(MONEDA==0,as.numeric(MONTO)/6.86,as.numeric(MONTO))) %>% 
  mutate(montous = ifelse(montous<saldous, saldous, montous)) %>% 
  mutate(NOMBRE_CLIENTE = paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT)) %>% 
  mutate(CAEDEC_DC = cases(grupoCaedecC,c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'),
                           c('5. Agropecuario','5. Agropecuario','4. Productivo','4. Productivo','5. Ind. Manufacturera',
                             '4. Productivo','3. Construcción','2. Comercio','1. Servicios','1. Servicios','1. Servicios','1. Servicios',
                             '1. Servicios','1. Servicios','1. Servicios','1. Servicios','1. Servicios','1. Servicios'))) %>% 
  mutate(GENERO = ifelse(!GENERO %in% c('F', 'M'),'J',GENERO)) %>% 
  select(OPERACION, CI, CTACLIENTE, NOMBRE_CLIENTE, GENERO, NOMBRE_ASESOR, NOMBRE_AGENCIA, 
         FDESEMBOLSO, CAEDEC_DC, CPOP, SECTOR_CARTERA, DESC_SEGMERC,`Categoría` = categ, 
         `Tipo_Crédito` = tipoCred, Sucursal, Rango_Monto = rangom, MONTO_USD = montous,
         SALDO_USD = saldous,ctaCont)

vipDescFull <- bdcLast %>% 
  left_join(VIPJoin_DailyVal, by = 'CTACLIENTE') %>%
  left_join(bus, by = 'CTACLIENTE') %>% 
  replace_na(list(totalBC=0, totalWC=0, maxMoraIM_cl=0, excelenteNegocios=0, EXCLUSIVO=1)) %>% 
  #isinLast==1 es cliente activo con historia desde 2015 tomando su fdes más antigua. YA NO APLICA
  mutate(CR_1_Mora = ifelse(moraMax==0,1,0)) %>% 
  mutate(CR_2_MoraIM = ifelse(is.na(maxMoraIMclBin) | maxMoraIMclBin %in% c("1. 0 días"),1,0)) %>% 
  mutate(CR_3_Hist = ifelse(is.na(badCreditBin) | badCreditBin %in% c("1. 0"),1,0)) %>% 
  mutate(CR_4_LoanDays = ifelse(tldBin %in% c("2. 1080-2160 días", "3. 2160-3240 días", "4. 3240 +"),1,0)) %>% 
  mutate(CR_5_Monto = ifelse(montoRatBin %in% c("3. 2x-4x","4. 4x +") & fdesMax_Min==1,1,0)) %>% 
  mutate(target = ifelse(isinLast==1 & totalNops > 1 &
                           CR_1_Mora==1 & CR_2_MoraIM==1 & CR_3_Hist==1 & CR_4_LoanDays==1 & 
                           CR_5_Monto==1, 1, 0 )) %>%  # Este es el ifelse que identifica al cliente VIP
  left_join(select(vipListExp, CTACLIENTE, prob_GC), by = 'CTACLIENTE') %>%
  mutate(pctSaldo = SALDO_USD/MONTO_USD) %>% 
  mutate(Amortización = 1 - pctSaldo) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(vipViable = ifelse(target == 1 & pctSaldo < 0.8, 1, 0)) %>% #Criterio de amortización de al menos el 20%
  mutate(oportunity_raw = MONTO_USD-SALDO_USD) %>% 
  mutate(oportunity = ifelse(target == 1, MONTO_USD - SALDO_USD, 0)) %>% 
  mutate(oportunityViable = ifelse(vipViable == 1, oportunity, 0)) %>% 
  mutate(Cliente_Viable = max(vipViable)) %>% 
  ungroup() %>% 
  #dplyr::filter(target == 1 & vipViable == 1) %>% 
  # dplyr::filter(oportunityViable > 1000) %>% #Filtro de préstamo mínimo del BSO y amortización
  glimpse() %>% 
  mutate(ctileProb = ntile(prob_GC, n = 100)) %>% 
  mutate(dtileProb = ntile(prob_GC, n = 10)) %>%
  mutate(Lista_GNN = ifelse(excelenteNegocios == 1, 'En Lista', 'Fuera de Lista'),
         VIP_RC = ifelse(target==1, 'VIP-RC', 'Regular'),
         VIP_Viable = ifelse(vipViable == 1, 'Viable', 'No viable'),
         Cliente_Viable = ifelse(Cliente_Viable == 1, "Cliente Viable", "Cliente No Viable")) %>%
  # dplyr::filter(VIP_RC == 'VIP-RC') %>% # filtro de VIPs
  glimpse() %>% 
  select(OPERACION, CI, CTACLIENTE, GENERO, NOMBRE_CLIENTE, NOMBRE_ASESOR, Sucursal, ctaCont,
         NOMBRE_AGENCIA, FDESEMBOLSO, CAEDEC_DC, CPOP, SECTOR_CARTERA, `Categoría`, `Tipo_Crédito`,
         Rango_Monto, MONTO_USD, SALDO_USD, Lista_GNN, `Amortización`, Probabilidad_VIP = prob_GC, 
         Decil_Prob = dtileProb, Percentil_Prob = ctileProb, Cliente_Viable, 
         VIP_Viable, Oportunidad_cruda = oportunity_raw,Oportunidad = oportunity, Oportunidad_Viable = oportunityViable, VIP_RC, moraMax,
         maxMoraIM_cl, meanMoraIM_cl, maxMoraIMclBin,meanMoraIMclBin,totalBC, totalWC,  minMonto, maxMonto, montoRat, 
         totalLoanDays,  
         starts_with("CR_")) %>%
  mutate(Score = Probabilidad_VIP * Oportunidad_Viable) %>% 
  glimpse() 

nrow(vipDescFull)
n_distinct(vipDescFull$CTACLIENTE)
table(vipDescFull$VIP_Viable)
table(vipDescFull$Lista_GNN,vipDescFull$VIP_Viable)

write_xlsx(vipDescFull, "D:/!bso/vipCartera/Criterios_Leads_May2023.xlsx")
####____LEADS Y COMPRA CON CUADRE____####
leadsCompra <- vipDescFull %>% 
  left_join(select(infoClientes, CTACLIENTE, OPERACION,Peor_Calificacion_SF, Sigla_SF, Entidad_SF, Tipo_Obligado_SF, Saldo_Vig_USD_SF, MontoOriginal_SF, N_Cred_SF),
            by = c('CTACLIENTE','OPERACION')) %>% 
  # dplyr::filter(!is.na(Saldo_Vig_USD_SF)) %>% 
  # dplyr::filter(Saldo_Vig_USD_SF > 0) %>% 
  # dplyr::filter(VIP_RC == 'VIP-RC') %>% 
  # group_by(CTACLIENTE) %>% 
  # dplyr::filter(row_number()==1) %>% 
  # ungroup() %>% 
  # dplyr::filter(Peor_Calificacion_SF == 'A') %>% 
  glimpse()

sum(leadsCompra$Saldo_Vig_USD_SF,na.rm = T)
table(leadsCompra$Lista_GNN)
table(leadsCompra$Peor_Calificacion_SF)
table(leadsCompra$GENERO)

x <- infoClientes %>% anti_join(vipDescFull,by=c("CTACLIENTE","OPERACION")) #Para ver qué clientes dejaron de ser compartidos
#Esto es porque cancelaron sus créditos en BSO o sus créditos afuera, o ambos.
sum(infoClientes$Saldo_Vig_USD_SF)
sum(compraCartera$Saldo_Vig_USD_SF,na.rm = T)+sum(x$Saldo_Vig_USD_SF) #Check de saldo vigente compartido 

write.xlsx(leadsCompra,paste0("D:/!bso/vipCartera/Leads_PreAprobados_CompraCartera_",shortmonth,".xlsx"))

####____EXPORTING LEADS AND COMPRA____####
vipFiltrada <- vipDescFull %>% 
  dplyr::filter(VIP_RC=="VIP-RC") %>% 
  select(-VIP_RC, -starts_with("CR_"), -Cliente_Viable)

infoClientes <- infoCheck %>% 
  dplyr::filter(REGULADO=="SBEF") %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>%  #Para conservar solo deudores
  mutate(esBSO = ifelse(SIGLA=='BSO',1,0)) %>%
  mutate(noesBSO = ifelse(SIGLA!='BSO',1,0)) %>%
  mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
  group_by(CI) %>%
  dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  mutate(CTACLIENTE = max(CTACLIENTE[esBSO==1])) %>% #Le asignamos la CTACLIENTE dentro de BSO a los créditos fuera de BSO %>% 
  dplyr::filter(esBSO==0) %>% 
  ungroup() %>% 
  dplyr::filter(ESTADO != "0. CONTINGENTE") %>% 
  select(CTACLIENTE, NOMBRE, DEPARTAMENTO, TIPO_OBLIGADO, SIGLA, ENTIDAD, 
         TIPO_CREDITO, CALIFICACION, ESTADO, DiasMora, FECHAINICIO, FECHAVTO, MontoOriginal, 
         Saldo_Vig_USD_SF = saldoVig, Periodo_Pago, HISTORICO = histLast16)

compraCartera <- vipDescFull %>% 
  dplyr::filter(VIP_RC=="VIP-RC") %>% 
  select(CTACLIENTE, CI, GENERO, Lista_GNN, Probabilidad_VIP, Decil_Prob, Percentil_Prob, Cliente_Viable) %>% 
  distinct_all() %>% 
  inner_join(infoClientes,by="CTACLIENTE")

ListasVIP <- list(PreAprobados = vipFiltrada, CompraCartera=compraCartera)

writexl::write_xlsx(ListasVIP,paste0("D:/!bso/vipCartera/Entrega_Leads_PreAprobados_CompraCartera_",shortmonth,".xlsx"))

####____ADDING DESC_SEG_MERC____####
bdcNC <- readRDS("D:/!bso/girCartera/rds/ec_May2023.rds") %>% 
  select(CTACLIENTE,OPERACION,DESC_SEGMERC) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(Seg_Merc_Asalariado = ifelse(DESC_SEGMERC=="Asalariado",1,0)) %>% 
  mutate(Seg_Merc_Asalariado = max(Seg_Merc_Asalariado)) %>% 
  ungroup() %>% 
  mutate(Seg_Merc_Asalariado = ifelse(Seg_Merc_Asalariado==1,"Es asalariado","No es asalariado")) %>% 
  select(-DESC_SEGMERC)

excel_sheets("D:/!bso/vipCartera/Entrega_Leads_PreAprobados_CompraCartera_May2023_v2.xlsx")
PreAp <- readxl::read_xlsx("D:/!bso/vipCartera/Entrega_Leads_PreAprobados_CompraCartera_May2023_v2.xlsx",sheet = "PreAprobados") %>% 
  left_join(bdcNC,by=c("CTACLIENTE","OPERACION"))

bdcNC <- bdcNC %>% 
  select(CTACLIENTE,Seg_Merc_Asalariado) %>% 
  distinct_all()
  
Compr <- readxl::read_xlsx("D:/!bso/vipCartera/Entrega_Leads_PreAprobados_CompraCartera_May2023_v2.xlsx",sheet = "CompraCartera") %>% 
  left_join(bdcNC,by="CTACLIENTE")

ListasVIP <- list(PreAprobados = PreAp, CompraCartera=Compr)

writexl::write_xlsx(ListasVIP,paste0("D:/!bso/vipCartera/Entrega_Leads_PreAprobados_CompraCartera_May2023_v3.xlsx"))
####____LEADS____####
VIP_old <- read_xlsx("D:/!bso/vipCartera/Leads_PreAprobados_CompraCartera_May2023.xlsx",sheet = "Sheet 1")
VIP_old <- VIP_old %>% 
  select(CTACLIENTE, OPERACION, CR1Mora_old = CR_1_Mora, CR2MoraIM_old = CR_2_MoraIM, 
         CR3Hist_old = CR_3_Hist, CR4LoanDays_old = CR_4_LoanDays, CR5Monto_old = CR_5_Monto)

vipDescFull_wOld <- vipDescFull %>% 
  left_join(VIP_old, by = c("CTACLIENTE","OPERACION"))

write_xlsx(vipDescFull_wOld, "D:/!bso/vipCartera/Criterios_Leads_May2023.xlsx")
####____CRITERIOS MAYO ADD LAST EPISODE OF MORAIM____####
vipList <- read_xlsx("D:/!bso/vipCartera/Criterios_Leads_May2023_v2.xlsx")
system.time(moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Jun2023.rds")) %>% 
              dplyr::filter(monDate!="jun. 2023") %>% 
              dplyr::filter(!is.na(maximaDPD)) %>% 
              group_by(CTACLIENTE) %>% 
              mutate(mesDiasMora = if_else(maximaDPD==0, as.numeric(as.yearmon("Dic. 2014")), as.numeric(monDate))) %>% 
              summarise(maxMoraIM_cl = max(maximaDPD),
                        meanMoraIM_cl = mean(maximaDPD),
                        lastMoraIM_cl = max(mesDiasMora)) %>% #lastMoraIM_cl = max(mesDiasMora)
              mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                                maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                                maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                                maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                                maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                                maxMoraIM_cl > 20 ~ '6. 20+ días')) %>% 
              ungroup() %>% 
              glimpse()
)
moraIntraMes <- moraIntraMes %>% 
  # mutate(lastMoraIM_cl = as.yearmon(lastMoraIM_cl)) %>% 
  # mutate(UltMoraIM = as.Date(lastMoraIM_cl,frac=1)) %>% 
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días',)) %>%
  mutate(meanMoraIMclBin = case_when(meanMoraIM_cl == 0 ~ '1. 0 días',
                                     meanMoraIM_cl <= 1 ~ '2. <= 1 día',
                                     meanMoraIM_cl <= 5 ~ '3. 1 - 5 días',
                                     meanMoraIM_cl <= 10 ~ '4. 5 - 10 días',
                                     meanMoraIM_cl <= 30 ~ '5. 10 - 30 días',
                                     meanMoraIM_cl > 30 ~ '6. >30 días',)) %>% 
  glimpse()

vipList <- vipList %>% 
  select(-maxMoraIM_cl,	-meanMoraIM_cl,	-maxMoraIMclBin,	-meanMoraIMclBin) %>% 
  left_join(select(moraIntraMes,-lastMoraIM_cl), by="CTACLIENTE")
write_xlsx(vipList, "D:/!bso/vipCartera/Criterios_Leads_May2023_v3.xlsx")
####____TUVO CONDONACIONES____####
codAge <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Jun2023.rds') %>% 
  glimpse()

process <- function(x, vstep){
  x %>% 
    select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
           CondInt_USD = Cond_Int,
           CondCap_USD = Cond_Cap,
           Cod_Asesor = Asesor,
           Nombre_Asesor = NombreAsesor,
           AGENCIA = Sucursal_operacion,
           {{vstep}}) %>%
    left_join(codAge,by="AGENCIA") %>% 
    select(-Fecha, -Regional) %>% 
    rename(Cod_Agencia = AGENCIA) %>% 
    group_by({{vstep}}, Sucursal, Cod_Agencia, NOMBRE_AGENCIA, 
             Cod_Asesor, Nombre_Asesor, Cuenta, Operacion) %>% 
    # group_by({{vstep}}, Sucursal, Cod_Agencia, Nombre_Agencia, NOMBRE_AGENCIA, 
    #          Cod_Asesor, Nombre_Asesor, Cuenta, Operacion) %>% 
    summarise_all(sum) %>% 
    ungroup() %>% 
    mutate(Int_Condonado = ifelse(CondInt_USD>0,1,0)) %>% 
    mutate(Cap_Condonado = ifelse(CondCap_USD>0,1,0)) %>% 
    mutate(IntCap_Condonado = ifelse(CondCapInt_USD>0,1,0)) %>% 
    mutate(Key = paste(Cuenta,Operacion,sep="-")) 
}
gph <- condFull %>% 
  mutate(monDate = as.yearmon(Fecha)) %>% 
  process(vstep = monDate) %>% 
  group_by(Cuenta) %>% 
  summarise(NCond=n()) %>% 
  mutate(tuvoCond = 1)

vipList <- vipList %>% 
  left_join(gph,by=c("CTACLIENTE"="Cuenta")) %>% 
  replace_na(list(NCond=0,tuvoCond=0))

write_xlsx(vipList, "D:/!bso/vipCartera/Criterios_Leads_May2023_v3.xlsx")
