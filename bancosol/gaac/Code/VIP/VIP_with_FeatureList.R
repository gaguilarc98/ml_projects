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
dfTotal <- readRDS('D:/!bso/features/Clientes_Ene15Mar23.rds') %>% 
  glimpse()
#Convirtiendo CI a único por CTACLIENTE
dfTotal <- dfTotal %>% 
  group_by(CTACLIENTE) %>% 
  arrange(desc(monDate)) %>% 
  mutate(CI = CI[row_number()==1]) %>% 
  ungroup() %>% 
  # dplyr::filter(fdes < as.Date("2023-03-01")) %>% 
  glimpse()

####____MEASURES OF PERFORMANCE BY CLIENT____####
lastmonth <- "Mar. 2023"
shortmonth <- str_replace(lastmonth,". ","")

dfVIP <-  dfTotal %>% 
  # dplyr::filter(monDate <= 'mar. 2020'| monDate >= 'ene. 2021') %>% 
  dplyr::filter(fdes>=as.Date("2015-01-01")) %>% 
  select(OPERACION, CTACLIENTE, CI, DIASMORA, MONTOUS, fdes, monDate, GENERO,
         moraMax, peorCalif,ctaCont) %>%
  mutate(lastMonth = as.Date(monDate, frac = 1)) %>% #último mes en las bdc
  mutate(loanDays = as.integer(lastMonth - fdes)) %>% #días de préstamo
  mutate(isinLast = ifelse(monDate==lastmonth,1,0)) %>% #está en el último mes
  mutate(lastESTADO = ifelse(ctaCont %in% c('131','135'),1,0)) %>% 
  group_by(CTACLIENTE, CI) %>% #Recuento de tiempo de préstamo por cliente y su mora Max
  mutate(GENERO = max(GENERO[fdes==max(fdes)])) %>% #Para uniformizar genero con el último en la base
  mutate(totalLoanDays = sum(loanDays, na.rm = T)) %>% #Total de días de préstamo 
  mutate(moraAcum = sum(moraMax, na.rm = T)) %>% #Mora acumulada
  mutate(moraMax = max(moraMax, na.rm = T)) %>% #Mora máxima
  mutate(totalMonto = sum(MONTOUS, na.rm = T)) %>% #Monto total desembolsado
  mutate(maxMonto = max(MONTOUS, na.rm = T)) %>% #Máximo monto
  mutate(minMonto = min(MONTOUS, na.rm = T)) %>% #Mínimo monto
  mutate(firstMonto = min(MONTOUS[fdes==min(fdes)])) %>% #Monto de primer desembolso
  mutate(lastMonto = max(MONTOUS[fdes==max(fdes)])) %>% #Monto de último desembolso
  mutate(montoRat = lastMonto/firstMonto) %>% #Ratio de monto último y primero
  mutate(montoRat_old = maxMonto/minMonto) %>% # OLD VERSION OF montoRat
  mutate(pct_maxDM_acumDM = ifelse(moraAcum > 0, moraMax/moraAcum, 0)) %>% #Porcentaje de días mora acumulados
  mutate(pct_acumDM_TLD = ifelse(totalLoanDays > 0, moraAcum/totalLoanDays, 0)) %>% #Porcentaje de días mora acumulados respecto de TLD
  mutate(pct_acumDM_TLD = ifelse(pct_acumDM_TLD > 1, 1, pct_acumDM_TLD)) %>% #Arreglo de la medida anterior
  mutate(totalNops = n_distinct(OPERACION)) %>% #Total de operaciones con BSO
  select(-lastMonth, -DIASMORA, -fdes, -monDate, -OPERACION, -MONTOUS, -loanDays,-ctaCont) %>% 
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
  
write_rds(dfVIP,paste0("D:/!bso/vipCartera/dfVIP_",shortmonth,".rds"))

####____AGREGANDO DATOS DE INFOCRED____####
lastcred <- "Feb. 2023" #Se selecciona un mes anterior al cierre de cartera
shortcred <- str_replace(lastcred,". ","")
#La última información de infocred tiene un mes de rezago

infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',shortcred,'.rds'))
#Se obtienen medidas de rendimiento del cliente con bases en la historia de 
#sus créditos fuera del banco
infoClean <- infoCheck %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>%  #Para conservar solo deudores
  mutate(esBSO = ifelse(SIGLA=='BSO',1,0)) %>%
  mutate(noesBSO = ifelse(SIGLA!='BSO',1,0)) %>%
  mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
  group_by(CI) %>%
  dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  mutate(CTACLIENTE = max(CTACLIENTE[esBSO==1])) %>% #Le asignamos la CTACLIENTE dentro de BSO a los créditos fuera de BSO
  mutate(worstBSOhistLast = max(histLast1[esBSO==1])) %>% #Le asignamos la CTACLIENTE dentro de BSO a los créditos fuera de BSO
  dplyr::filter(esBSO!=1) %>% #Filtramos a los créditos fuera  de BSO
  mutate(CalifPeorSF = max(CALIFICACION,na.rm=T)) %>% #Obtenemos la peor calificación del cliente 
  ungroup() %>% 
  group_by(CTACLIENTE) %>% 
  arrange(CTACLIENTE) %>% 
  mutate(totalBC = str_count(histLast16, "2") + str_count(histLast16, "3") + str_count(histLast16, "4")) %>% 
  mutate(totalWC = str_count(histLast16, "4")) %>%  
  mutate(worstSFhistLast = max(histLast1[esBSO==0])) %>% 
  mutate(badCreditBin = case_when(totalBC == 0  ~ '1. 0',
                                  totalBC == 1  ~ '2. 1',
                                  totalBC == 2 ~ '3. 2',
                                  totalBC > 2 ~ '4. 2+',
                                  !is.na(totalWC) & totalWC > 0 ~ '5. Tuvo castigo')) %>% 
  select(CI, CTACLIENTE, histLast16, badCredit, worstCredit, totalBC, totalWC, CalifPeorSF,
         badCreditBin, worstBSOhistLast, worstSFhistLast) %>% 
  summarise_all(max) %>% 
  mutate(EXCLUSIVO = 0)
    
####____JOIN INFOCRED WITH BDCVIP____####
VIPJoin <- dfVIP %>% 
  left_join(infoClean,by="CTACLIENTE") %>% 
  select(-CI.x, -CI.y)
####____DAILY DELINQUENCY DATA____####
#CI, OPERACION, CTACLIENTE,maxMoraIM_cl,maxMoraIM_op
dailyMM <- readRDS('D:/!bso/vipCartera/ClientesDaily_Ene22Abr23.rds') %>%
  group_by(CTACLIENTE) %>% 
  mutate(maxMoraIM_cl = max(moraMax,na.rm = T)) %>% 
  ungroup() %>% 
  select(CTACLIENTE,maxMoraIM_cl) %>% 
  group_by(CTACLIENTE) %>% 
  summarise_all(max)

VIPJoin_DailyVal <- VIPJoin %>% 
  left_join(dailyMM, by = 'CTACLIENTE') %>% 
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días')) %>% 
  glimpse()
####____LISTA DE CLIENTES EXCELENTES DE NEGOCIOS____####
# Lista excelentes para Negocios
bus <- read.delim('D:/!bso/vipCartera/excelentes/Clientes_excelentes_202303.csv',
                        sep = '|') %>%
  dplyr::rename(CTACLIENTE = Cuenta) %>% 
  select(CTACLIENTE) %>% 
  distinct() %>% 
  mutate(excelenteNegocios = 1)
length(unique(bus$CTACLIENTE))

finalVIP <- VIPJoin_DailyVal %>% 
  left_join(bus, by = 'CTACLIENTE') %>% 
  mutate(excelenteNegocios = ifelse(is.na(CTACLIENTE), NA, excelenteNegocios))
# write.xlsx(finalVIP, 'D:/!bso/vipCartera/.xlsx')

####____FILLING MISSING VALUES AND DEFINING TARGET____####
bdcVIPScore <- finalVIP %>% 
  mutate(CR_Monto = ifelse(montoRatBin %in% c("4. 4x +", "3. 2x-4x"),1,0)) %>% 
  mutate(CR_LoanDays = ifelse(tldBin %in% c("4. 3240 +", "3. 2160-3240 días", "2. 1080-2160 días"),1,0)) %>% 
  mutate(CR_Mora2015 = ifelse(pctAcumBin %in% c('1. 0%', '2. 1%-3%', '3. 3%-5%'),1,0)) %>% #Nueva medida
  mutate(CR_MoraIM = ifelse(is.na(maxMoraIMclBin) | maxMoraIMclBin %in% c("1. 0 días"),1,0)) %>% 
  mutate(CR_LastHist = ifelse(is.na(worstSFhistLast) | worstSFhistLast=='1',1,0)) %>% 
  mutate(CR_Hist = ifelse(is.na(badCreditBin) | badCreditBin %in% c("1. 0","2. 1"),1,0)) %>% 
  mutate(target = ifelse(isinLast==1 & lastESTADO==1 & totalNops>1 &
                           CR_Monto==1 & CR_LoanDays==1 & CR_Mora2015==1 & 
                           CR_MoraIM==1 & CR_LastHist==1 & CR_Hist==1, 1, 0 )) %>% # Este es el ifelse que identifica al cliente VIP
  select(-ends_with('Bin'), -starts_with('histStr'), -ends_with('Credit'),-starts_with("CR_")) %>% 
  select(CTACLIENTE, GENERO, totalMonto, minMonto, maxMonto, totalNops, montoRat, totalLoanDays,
         pct_acumDM_TLD, pct_maxDM_acumDM, moraAcum, moraMax, totalBC, totalWC,
         maxMoraIM_cl, excelenteNegocios,target,EXCLUSIVO) %>% 
  replace_na(list(totalBC=0, totalWC=0, maxMoraIM_cl=0, excelenteNegocios=0, EXCLUSIVO=1)) %>% 
  glimpse()

table(bdcVIPScore$target)

glimpse(bdcVIPScore)
write.csv(bdcVIPScore, paste0('D:/!bso/vipCartera/dataScore_',shortmonth,'.csv'), row.names = F)
#CON ESTE RESULTADO SE CALCULAN LOS SCORES EN PYTHON
#UNA VEZ SE EJECUTE EL RESULTADO EN PYTHON SE RETOMA A PARTIR DE AQUÍ

dfScore <- fread(paste0('D:/!bso/vipCartera/scores/vipList_wScores_',shortmonth,'.csv'), sep = ',', fill = T) %>% 
  select(CTACLIENTE, target, prob_GC, goodClient)

vipListExp <- dfScore %>% 
  left_join(finalVIP, by = 'CTACLIENTE') %>% 
  replace_na(list(totalBC=0, totalWC=0, maxMoraIM_cl=0, excelenteNegocios=0, EXCLUSIVO=1)) %>% 
  #left_join(bdcAgoExp, by = 'CI') %>% 
  glimpse()
saveRDS(vipListExp, paste0("D:/!bso/vipCartera/vipListwScores_",shortmonth,".rds"))
vipListExp <- readRDS(paste0("D:/!bso/vipCartera/vipListwScores_",shortmonth,".rds"))
####____LISTA DE PREAPROBADOS_____####
####___JOIN CON BASE DE CARTERA RECIENTE____####  
bdcLast <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',shortmonth,'.rds')) %>% 
  dplyr::filter(!MODULO %in% c(131,29)) %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  mutate(montous = ifelse(MONEDA==0,as.numeric(MONTO)/6.86,as.numeric(MONTO))) %>% 
  mutate(ESTADO = case_when(ESTADO=="VIGENTE"~"1. VIGENTE",
                            ESTADO=="OP VENCIDA"~"2. VENCIDA",
                            ESTADO=="SUSPENSO"~"3. SUSPENSO",
                            ESTADO=="JUDICIAL"~"4. JUDICIAL",
                            ESTADO=="CASTIGADA"~"5. CASTIGADA",)) %>% 
  select(OPERACION, CI, CTACLIENTE, NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT, GENERO, CALIFICACION, ESTADO,
         NOMBRE_ASESOR, NOMBRE_AGENCIA, SECTOR_CARTERA, FFINALIZA, CPOP, saldous, previus, 
         montous, intus, categ, tipoCred, Sucursal, caedec3dD, rango, rangos, rangom)

vipDescFull <- bdcLast %>% 
  left_join(select(vipListExp,-GENERO), by = 'CTACLIENTE') %>%
  group_by(CTACLIENTE) %>% 
  mutate(ncli = ifelse(row_number()==1,1,0)) %>% 
  mutate(totalMonto = sum(montous)) %>% 
  mutate(totalSaldo = sum(saldous)) %>% 
  ungroup() %>% 
  group_by(CTACLIENTE) %>% 
  mutate(pctSaldo = sum(saldous)/sum(montous)) %>% 
  mutate(vipViable = ifelse(target == 1 & pctSaldo < 0.8, 1, 0)) %>% 
  mutate(oportunity = ifelse(target == 1, totalMonto - totalSaldo, 0)) %>% 
  mutate(oportunityViable = ifelse(vipViable == 1, oportunity, 0)) %>% 
  ungroup() %>% 
  #dplyr::filter(target == 1 & vipViable == 1) %>% 
  mutate(ctileProb = ntile(prob_GC, n = 100)) %>% 
  mutate(dtileProb = ntile(prob_GC, n = 10)) %>%
  mutate(Lista_GNN = ifelse(excelenteNegocios == 1, 'En Lista', 'Fuera de Lista'),
         VIP_RC = ifelse(target==1, 'VIP-RC', 'Regular'),
         VIP_Viable = ifelse(vipViable == 1, 'Viable', 'No viable'),
         Grupo_Exclusivo = ifelse(EXCLUSIVO == 0, 'Exclusivo', 'No Exclusivo')) %>%
  select(OPERACION, CI, CTACLIENTE, GENERO, NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT, NOMBRE_ASESOR, 
         NOMBRE_AGENCIA, Lista_GNN, montous, caedec3dD, CPOP, saldous, tipoCred, dtileProb, 
         SECTOR_CARTERA, oportunityViable, Sucursal, categ, rangom, intus, prob_GC, ctileProb,
         pctSaldo, oportunity, Grupo_Exclusivo, VIP_RC, VIP_Viable) %>% 
  mutate(oportunityViable = montous - saldous) %>% 
  glimpse() %>% 
  dplyr::filter(VIP_RC == 'VIP-RC') %>% # filtro de VIPs
  glimpse() 

vipFiltrada <- vipDescFull %>% 
  dplyr::filter(oportunityViable > 1000) %>% # filtro de préstamo mínimo del BSO
  glimpse() %>% 
  dplyr::rename(Monto_USD = montous,
                Saldo_USD = saldous,
                CAEDEC_DC = caedec3dD,
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
nrow(vipFiltrada)
n_distinct(vipFiltrada$CTACLIENTE)
# Operaciones viables (>1,000$us)
nrow(vipFiltrada[vipFiltrada$Grupo_Amortización != '1. 20%-', ])

# This is the final list/dataframe
# write.xlsx(vipFiltrada, 'D:/!bso/vipRC/vipDescriptives_CompleteList__allVars_v2.xlsx')
saveRDS(vipFiltrada, paste0('D:/!bso/vipCartera/vipDescriptives_CompleteList_',shortmonth,'_allVars.rds'))

####____COMPRA DE CARTERA____####
#Revisar criterios con JM
vipFiltrada <- readRDS(paste0('D:/!bso/vipCartera/vipDescriptives_CompleteList_',shortmonth,'_allVars.rds'))
# vipDescFull_ccMar <- read.xlsx('D:/!bso/vipRC/vipDescriptives_CompleteList_Feb2023_allVars.xlsx')

infoClientes <- infoCheck %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>%  #Para conservar solo deudores
  mutate(esBSO = ifelse(SIGLA=='BSO',1,0)) %>%
  mutate(noesBSO = ifelse(SIGLA!='BSO',1,0)) %>%
  mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
  group_by(CI, esBSO) %>%
  mutate(maxSaldo = ifelse(saldo==max(saldo),1,0)) %>% 
  mutate(peorCalif = ifelse(CALIFICACION==max(CALIFICACION),1,0)) %>% 
  ungroup() %>% 
  group_by(CI) %>%
  dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  mutate(Saldo_USD_SF = sum(saldo*noesBSO,na.rm=T)) %>% 
  mutate(Saldo_Vig_USD_SF = sum(saldoVig*noesBSO,na.rm=T)) %>% 
  mutate(MontoOriginal_SF = sum(MontoOriginal*noesBSO,na.rm=T)) %>%
  mutate(Peor_Calificacion_SF = ifelse(peorCalif==1 & esBSO==0, CALIFICACION,'_')) %>% #Obtenemos la peor calificación del cliente 
  mutate(N_Cred_SF = sum(noesBSO)) %>% 
  mutate(Sigla_SF = ifelse(maxSaldo==1 & esBSO==0, SIGLA,'_')) %>% 
  mutate(Entidad_SF = ifelse(maxSaldo==1 & esBSO==0, ENTIDAD,'_')) %>% 
  mutate(Calificacion_SF = ifelse(maxSaldo==1 & esBSO==0, CALIFICACION, '_')) %>% 
  mutate(Tipo_Obligado_SF = ifelse(maxSaldo==1 & esBSO==0, TIPO_OBLIGADO, '_')) %>%
  mutate(across(Peor_Calificacion_SF:Tipo_Obligado_SF, ~ max(.x,na.rm=T))) %>% 
  dplyr::filter(SIGLA=="BSO") %>% 
  mutate(across(Saldo_USD_SF:MontoOriginal_SF,~.x/n())) %>% 
  ungroup() %>% 
  select(OPERACION,CTACLIENTE,Sigla_SF,Entidad_SF,Peor_Calificacion_SF,Tipo_Obligado_SF,
         N_Cred_SF,Saldo_USD_SF, Saldo_Vig_USD_SF,MontoOriginal_SF)

compraCartera <- vipFiltrada %>% 
  left_join(infoClientes, by = c('CTACLIENTE','OPERACION')) %>% 
  dplyr::filter(!is.na(Saldo_Vig_USD_SF)) %>% 
  dplyr::filter(Saldo_Vig_USD_SF > 0) %>% 
  dplyr::filter(VIP_RC == 'VIP-RC') %>% 
  # group_by(CTACLIENTE) %>% 
  # dplyr::filter(row_number()==1) %>% 
  # ungroup() %>% 
  dplyr::filter(Peor_Calificacion_SF == 'A')

sum(compraCartera$Saldo_USD_SF)
table(compraCartera$Lista_GNN)
table(compraCartera$Peor_Calificacion_SF)
table(compraCartera$GENERO)
####____EXPORTING LEADS____####
vipFiltrada <- vipFiltrada %>% 
  select(OPERACION, CI, CTACLIENTE, GENERO, NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT, 
         NOMBRE_ASESOR, NOMBRE_AGENCIA, Lista_GNN, Monto_USD, CAEDEC_DC, CPOP, Saldo_USD,
         Tipo_Crédito, Decil_Prob, SECTOR_CARTERA, Oportunidad_Viable, Sucursal, Categoría,
         Rango_Monto, Score, Decil_Score)
compraCartera <- compraCartera %>% 
  select(OPERACION, CI, CTACLIENTE, GENERO, NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT,	
         NOMBRE_ASESOR, NOMBRE_AGENCIA,	Lista_GNN, Monto_USD, CAEDEC_DC, CPOP,
         Probabilidad_VIP, Percentil_Prob, Grupo_Exclusivo, Saldo_USD, Tipo_Crédito,
         Decil_Prob,	SECTOR_CARTERA, Oportunidad_Viable, Sucursal, Categoría, Rango_Monto,
         Peor_Calificacion_SF, Saldo_USD_SF, Saldo_Vig_USD_SF, MontoOriginal_SF, N_Cred_SF,
         Entidad_SF, Sigla_SF, Tipo_Obligado_SF)
ListasVIP <- list(PreAprobados = vipFiltrada, CompraCartera=compraCartera)

write.xlsx(ListasVIP,paste0("D:/!bso/vipCartera/Leads_PreAprobados_CompraCartera_",shortmonth,"_v2.xlsx"))

####____CHECKS ANTERIORES____####
check_feb23 <- compraCartera %>% 
  ungroup() %>% 
  group_by(CI) %>% 
  arrange(desc(CI)) %>% 
  mutate(Saldo_USD_SF = ifelse(row_number()>1, 0, Saldo_USD_SF)) %>% 
  ungroup() %>% 
  select(Saldo_USD_SF, Oportunidad_Viable, VIP_Viable, GENERO) %>% 
  mutate(Op_Viable = ifelse(Oportunidad_Viable>0,1,0),
         Op_Compra = ifelse(Saldo_USD_SF>0,1,0)) %>% 
  group_by(VIP_Viable, GENERO) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF, na.rm = T),
            Oportunidad_Viable = sum(Oportunidad_Viable, na.rm = T),
            N_Ops_Preaprobadas = sum(Op_Viable, na.rm = T),
            N_Ops_Compra = sum(Op_Compra, na.rm = T)) %>% 
  adorn_totals('row') %>% 
  glimpse() 


cc_mar23_all <- vipDescFull_cc_mar23 %>% 
  left_join(infoPerf_Compra_mar23, by = 'CTACLIENTE') %>% 
  dplyr::filter(!is.na( `SBEF VIGENTE`)) %>% 
  dplyr::filter(`SBEF VIGENTE` > 0) %>% 
  dplyr::filter(VIP_RC == 'VIP-RC') %>% 
  group_by(CTACLIENTE) %>% 
  # dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  dplyr::rename(Saldo_USD_SF = `SBEF VIGENTE`,
                Calificacion_SF = `SBEF CALIFICACION`,
                Entidad = `ENTIDAD SBEF`,
                Tipo_Credito_SF = `TIPO CREDITO SBEF`,
                Sigla_Entidad_SF = `SIGLA SBEF`) %>% 
  mutate(Saldo_USD_SF = Saldo_USD_SF/6.86) %>% 
  dplyr::filter(Calificacion_SF == 'A')
sum(cc_mar23_all$Saldo_USD_SF)

# base sexo
bdc_I_Wanna_Have_Sex_mar23 <- readRDS('D:/!bso/girCartera/rds_v3/ec_mar2023.rds') %>% 
  select(CTACLIENTE, GENERO) %>% 
  dplyr::rename(Genero = GENERO) %>% 
  distinct_all()

ccExp_mar23 <- cc_mar23 %>% 
  select(Calificacion_SF, Saldo_USD_SF, MontoOriginal, Entidad,
         Sigla_Entidad_SF, `TIPO OBLIGADO SBEF`, CTACLIENTE) %>% 
  dplyr::rename(MontoOriginal_SF = MontoOriginal,
                Tipo_Obligado_SF = `TIPO OBLIGADO SBEF`) %>% 
  glimpse()

# Final join

vipLead_final_mar23 <- vipDescFull_cc_mar23 %>% 
  left_join(ccExp_mar23, by = 'CTACLIENTE') %>%
  left_join(bdc_I_Wanna_Have_Sex_feb23, by = 'CTACLIENTE') %>%
  dplyr::rename(Historial_SF = histStr2,
                Entidad_SF = Entidad) %>% 
  glimpse()

write.xlsx(vipLead_final, 'D:/!bso/compraCartera/AllLeads_final_feb2023_ae_v2.xlsx')

leadsJoin_mar23 <- vipLead_final_mar23 %>% 
  select(CTACLIENTE, OPERACION, ends_with('_SF'))
write_rds(leadsJoin, 'C:/!bso/vipCartera/compra_feb2023_ae.rds')

length(unique(bdcVIPScore$CTACLIENTE))

bdcVIP_ene <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CI, DIASMORA, monDate, montous, FDESEMBOLSO, GENERO) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>% 
  dplyr::filter(monDate <= 'mar. 2020'| monDate >= 'ene. 2021') %>% 
  glimpse() %>% 
  ungroup() %>%  
  select(OPERACION, CTACLIENTE, CI, DIASMORA, montous, fdes, monDate, GENERO) %>%
  mutate(lastMonth = as.Date(monDate, frac = 1)) %>%  
  mutate(loanDays = as.integer(lastMonth - fdes)) %>% # check measure
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
  select(-montous, -lastMonth, -DIASMORA, -fdes, -monDate, -OPERACION, -nops) %>% 
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
####____READING SCORES WITH PYTHON____####
vipScores <- fread("D:/!bso/vipCartera/vipList_wScores_dic2022.csv")
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Dic2022.rds") %>% 
  dplyr::filter(MODULO!=131) %>% 
  dplyr::filter(ESTADO!="CASTIGADA") %>% 
  group_by(CI) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  left_join(vipScores,by="CI")
