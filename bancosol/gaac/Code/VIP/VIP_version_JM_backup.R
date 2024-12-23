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
dfTotal <- readRDS('D:/!bso/features/Clientes_Ene15Abr23_v6.rds') %>% 
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
lastmonth <- "Abr. 2023"
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

saveRDS(dfVIP,paste0("D:/!bso/vipCartera/dfVIP_",shortmonth,".rds"))
dfVIP <- readRDS(paste0("D:/!bso/vipCartera/dfVIP_",shortmonth,".rds"))
####____AGREGANDO DATOS DE INFOCRED____####
lastcred <- "Mar. 2023" #Se selecciona un mes anterior al cierre de cartera
shortcred <- str_replace(lastcred,". ","")
#La última información de infocred tiene un mes de rezago

infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',shortcred,'.rds'))
infoCheck2 <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/califClientes/process/comp_',shortcred,'.rds'))
#Se obtienen medidas de rendimiento del cliente con bases en la historia de 
#sus créditos fuera del banco
infoClientes <- infoCheck2 %>% 
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
  select(OPERACION, CTACLIENTE, totalBC, totalWC, badCreditBin, Sigla_SF, Entidad_SF,
         Peor_Calificacion_SF, Tipo_Obligado_SF, N_Cred_SF, Saldo_USD_SF, Saldo_Vig_USD_SF,
         MontoOriginal_SF) %>% 
  mutate(EXCLUSIVO = 0)

infoClean  <- infoClientes %>% 
  select(CTACLIENTE, totalBC, totalWC, badCreditBin, EXCLUSIVO) %>% 
  group_by(CTACLIENTE) %>% 
  summarise_all(max)
####____JOIN INFOCRED WITH BDCVIP____####
VIPJoin <- dfVIP %>% 
  left_join(infoClean,by="CTACLIENTE") %>% 
  select(-CI)
####____DAILY DELINQUENCY DATA____####
#CI, OPERACION, CTACLIENTE,maxMoraIM_cl,maxMoraIM_op
daily <- readRDS(paste0("D:/!bso/vipCartera/dailyMM/dailyMM_",shortmonth,".rds"))

VIPJoin_DailyVal <- VIPJoin %>% 
  left_join(daily, by = 'CTACLIENTE') %>% 
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días')) %>% 
  glimpse()

####____LISTA DE CLIENTES EXCELENTES DE NEGOCIOS____####
# Lista excelentes para Negocios
bus <- read.delim('D:/!bso/vipCartera/excelentes/Clientes_excelentes_202304.csv',
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
  select(CTACLIENTE, GENERO, totalMonto, minMonto, maxMonto, totalNops, montoRat, totalLoanDays,
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

####____LEADS PREAPROBADOS____####
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
         FDESEMBOLSO, CAEDEC_DC, CPOP, SECTOR_CARTERA, `Categoría` = categ, 
         `Tipo_Crédito` = tipoCred, Sucursal, Rango_Monto = rangom, MONTO_USD = montous,
         SALDO_USD = saldous,ctaCont)

vipDescFull <- bdcLast %>% 
  left_join(select(vipListExp,-GENERO), by = 'CTACLIENTE') %>%
  mutate(pctSaldo = SALDO_USD/MONTO_USD) %>% 
  mutate(Amortización = 1 - pctSaldo) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(vipViable = ifelse(target == 1 & pctSaldo < 0.8, 1, 0)) %>% #Criterio de amortización de al menos el 20%
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
         Cliente_Viable = ifelse(Cliente_Viable == 1, "Cliente Viable", "Cliente No Viable"),
         Grupo_Exclusivo = ifelse(EXCLUSIVO == 0, 'No Exclusivo', 'Exclusivo')) %>%
  # dplyr::filter(VIP_RC == 'VIP-RC') %>% # filtro de VIPs
  glimpse() %>% 
  select(OPERACION, CI, CTACLIENTE, GENERO, NOMBRE_CLIENTE, NOMBRE_ASESOR, Sucursal, ctaCont,
         NOMBRE_AGENCIA, FDESEMBOLSO, CAEDEC_DC, CPOP, SECTOR_CARTERA, `Categoría`, `Tipo_Crédito`,
         Rango_Monto, MONTO_USD, SALDO_USD, Lista_GNN, `Amortización`, Probabilidad_VIP = prob_GC, 
         Decil_Prob = dtileProb, Percentil_Prob = ctileProb, Grupo_Exclusivo, Cliente_Viable, 
         VIP_Viable, Oportunidad = oportunity, Oportunidad_Viable = oportunityViable, VIP_RC, 
         starts_with("CR_")) %>%
  mutate(Score = Probabilidad_VIP * Oportunidad_Viable) %>% 
  glimpse() 

# vipFiltrada <- vipDescFull %>% 
#   mutate(Score = Probabilidad_VIP * Oportunidad_Viable,
#          Decil_Score = ntile(Score, n = 10),
#          Amortización = 1-pctSaldo,
#          Grupo_Amortización = case_when(Amortización <= 0.2 ~ '1. 20%-',
#                                         Amortización > 0.2 & Amortización <= 0.4 ~ '2. 20%-40%',
#                                         Amortización > 0.4 & Amortización <= 0.6 ~ '3. 40%-60%',
#                                         Amortización > 0.6 & Amortización <= 0.8 ~ '4. 60%-80%',
#                                         Amortización > 0.8 ~ '5. 80%+',))
# Operaciones viables (>1,000$us)
nrow(vipDescFull)
n_distinct(vipDescFull$CTACLIENTE)
table(vipDescFull$VIP_Viable)
table(vipDescFull$Lista_GNN,vipDescFull$VIP_Viable)

# This is the final list/dataframe
# write.xlsx(vipFiltrada, 'D:/!bso/vipRC/vipDescriptives_CompleteList__allVars_v2.xlsx')
saveRDS(vipFiltrada, paste0('D:/!bso/vipCartera/vipDescriptives_CompleteList_',shortmonth,'_allVars.rds'))

####____COMPRA DE CARTERA____####
#Revisar criterios con JM
vipFiltrada <- readRDS(paste0('D:/!bso/vipCartera/vipDescriptives_CompleteList_',shortmonth,'_allVars.rds'))
# vipDescFull_ccMar <- read.xlsx('D:/!bso/vipRC/vipDescriptives_CompleteList_Feb2023_allVars.xlsx')

compraCartera <- vipDescFull %>% 
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

sum(compraCartera$Saldo_Vig_USD_SF,na.rm = T)
table(compraCartera$Lista_GNN)
table(compraCartera$Peor_Calificacion_SF)
table(compraCartera$GENERO)

x <- infoClientes %>% anti_join(vipDescFull,by=c("CTACLIENTE","OPERACION")) #Para ver qué clientes dejaron de ser compartidos
#Esto es porque cancelaron sus créditos en BSO o sus créditos afuera, o ambos.
sum(infoClientes$Saldo_Vig_USD_SF)
sum(compraCartera$Saldo_Vig_USD_SF,na.rm = T)+sum(x$Saldo_Vig_USD_SF) #Check de saldo vigente compartido 

write.xlsx(compraCartera,paste0("D:/!bso/vipCartera/Leads_PreAprobados_CompraCartera_",shortmonth,"_v4.xlsx"))

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