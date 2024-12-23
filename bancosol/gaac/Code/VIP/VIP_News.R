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
library(arrow)
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
####____MEASURES OF PERFORMANCE BY CLIENT____####
#AT THIS STAGE WE DO NOT NEED DATA FROM THE CLIENTS OTHER THAN THEIR CTACLIENTE
#AND OBVIOUSLY OTHER VARIABLES TO GET THE RELEVANT MEASURES FOR THE MODEL
dfOps <- readRDS('D:/!bso/features/Historial_Operaciones.rds') %>% 
  glimpse()

lastmonth <- "Sep. 2023"
shortmonth <- str_replace(lastmonth,". ","")
n_distinct(dfOps$CTACLIENTE[dfOps$fdes>=as.Date("2015-01-01") & dfOps$monDate==lastmonth])

dfLastCierre <- dfOps %>% 
  group_by(CTACLIENTE, OPERACION) %>%  
  mutate(fdes = min(fdes)) %>% 
  ungroup() %>% 
  group_by(CTACLIENTE) %>% 
  summarise(myLastCierre = max(monDate),
            myLastDesembolso = max(fdes)) %>%
  ungroup()

dfMeasures <- dfOps %>% 
  select(CTACLIENTE, OPERACION, monDate, fdes, DIASMORA, MONTOUS) %>% 
  dplyr::filter(monDate <= 'mar. 2020' | monDate >= 'ene. 2021') %>% 
  dplyr::filter(monDate <= lastmonth) %>% 
  group_by(CTACLIENTE, OPERACION) %>% #Se agrupa por operación de cada cliente
  arrange(desc(monDate)) %>% #Se ordena por fecha de más reciente a más antigua
  mutate(fdes = min(fdes)) %>% #Se cambia la fecha de desembolso a la más antigua
  # dplyr::filter(fdes>=as.Date("2015-01-01")) %>% #Filtro de desembolsos a partir de 2015 YA NO APLICA
  mutate(moraMax = max(DIASMORA, na.rm = T)) %>% #Máxima mora al cierre
  mutate(MONTOUS = sum(unique(MONTOUS), na.rm = T)) %>% #Suma de montos desembolsados
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
  ungroup() %>% #By default cut makes left_open right-closed intervals
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

saveRDS(dfMeasures,paste0("D:/!bso/vipCartera/dfVIP_",shortmonth,".rds"))
dfMeasures <- readRDS(paste0("D:/!bso/vipCartera/dfVIP_",shortmonth,".rds"))
#HASTA ESTA ETAPA SE OBTIENE MAXIMA MORA AL CIERRE, ANTIGÜEDAD Y DUPLICACION DEL MONTO

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
VIPJoin <- dfMeasures %>% 
  left_join(HistSummary,by = "CTACLIENTE")
remove(HistCalif)
gc()
####____TUVO CONDONACION____####
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Sep2023.rds') %>% 
  mutate(monDate = as.yearmon(Fecha)) %>%
  select(monDate, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int) %>%
  # group_by(Cuenta, Operacion, monDate) %>% 
  # summarise_all(max, na.rm=T) %>% 
  group_by(Cuenta) %>% 
  summarise(ConCapInt_USD = sum(CondCapInt_USD, na.rm = T)) %>% 
  mutate(tuvoCond = 1)

VIPJoin <- VIPJoin %>% 
  left_join(condFull, by=c('CTACLIENTE'='Cuenta'))
####____MORA_IM_2022____####
daily <- readRDS(paste0("D:/!bso/vipCartera/dailyMM/dailyMM_",shortmonth,".rds"))

VIPJoin <- VIPJoin %>% 
  left_join(daily, by = 'CTACLIENTE') %>% 
  mutate(maxMoraIMclBin = case_when(maxMoraIM_cl == 0 ~ '1. 0 días',
                                    maxMoraIM_cl <= 5 ~ '2. 1-5 días',
                                    maxMoraIM_cl <= 10 ~ '3. 5-10 días',
                                    maxMoraIM_cl <= 15 ~ '4. 10-15 días',
                                    maxMoraIM_cl <= 20 ~ '5. 15-20 días',
                                    maxMoraIM_cl > 20 ~ '6. 20+ días',)) %>% 
  glimpse()
#CON ESTA VARIABLE SE CAPTURA LA MORA MÁS RECIENTE DE MANERA MÁS FIDEDIGNA QUE LA APROXIMACIÓN
####____MORA_IM_2018____####
moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Sep2023.rds")) %>% 
  # dplyr::filter(monDate!="jun. 2023") %>% #Filtro para reproducir meses anteriores
  dplyr::filter(!is.na(maximaDPD)) %>% 
  group_by(CTACLIENTE) %>% 
  # mutate(mesDiasMora = if_else(maximaDPD==0, 2014, year(monDate))) %>%
  mutate(tuvoMora = ifelse(maximaDPD>0, 1, 0)) %>% 
  summarise(meanMoraIM_cl = mean(maximaDPD),
            tuvoMora_pct = sum(tuvoMora)/n()) %>% #lastMoraIM_cl = max(mesDiasMora), maxMoraIM_cl = max(maximaDPD)
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
#CON ESTAS VARIABLE SE REGULA LA MORA DE CLIENTES DE FORMA DESAGREGADA
VIPJoin <- VIPJoin %>% 
  left_join(moraIntraMes, by = 'CTACLIENTE') 
####____LISTA DE NEGOCIOS____####
bus <- read.delim('D:/!bso/vipCartera/excelentes/Clientes_excelentes_202309.csv',
                  sep = '|') %>%
  dplyr::rename(CTACLIENTE = Cuenta) %>% 
  select(CTACLIENTE) %>% 
  distinct() %>% 
  mutate(excelenteNegocios = 1)
length(unique(bus$CTACLIENTE))

VIPJoin <- VIPJoin %>% 
  left_join(bus, by = 'CTACLIENTE') 

####____TARGET DEFINITION AND EXPORTING TO PYTHON____####
finalVIP <- VIPJoin %>% 
  replace_na(list(totalBC=0, totalWC=0, maxMoraIM_cl=0, excelenteNegocios=0, tuvoCond=0)) %>% 
  #isinLast==1 es cliente activo con historia desde 2015 tomando su fdes más antigua. YA NO APLICA
  mutate(CR_1_Mora15 = ifelse(moraMax==0, 1, 0)) %>% 
  mutate(CR_2_Hist18 = ifelse(is.na(badCreditBin) | badCreditBin %in% c("1. 0"), 1, 0)) %>% 
  mutate(CR_3_Antig = ifelse(tldBin %in% c("2. 1080-2160 días", "3. 2160-3240 días", "4. 3240 +"), 1, 0)) %>% 
  mutate(CR_4_DobMonto = ifelse(montoRatBin %in% c("3. 2x-4x","4. 4x +") & fdesMax_Min==1, 1, 0)) %>% 
  mutate(CR_5_TuvoCond = ifelse(tuvoCond==1, 0, 1)) %>% 
  mutate(CR_6_MoraIM22 = ifelse(is.na(maxMoraIMclBin) | maxMoraIMclBin %in% c("1. 0 días"), 1, 0)) %>% 
  mutate(CR_7_IntMora = ifelse(is.na(meanMoraIMclBin) | meanMoraIMclBin == '1. 0 días', 1, 0)) %>% 
  mutate(CR_7_IntMora = ifelse(!is.na(meanMoraIMclBin) & meanMoraIMclBin =='2. <= 1 día' & tuvoMora_pct <=0.15, 1, CR_7_IntMora)) %>% 
  mutate(target = ifelse(CR_1_Mora15==1 & CR_2_Hist18==1 & CR_3_Antig==1 & CR_4_DobMonto==1 
                         & CR_5_TuvoCond==1 & CR_6_MoraIM22==1 & CR_7_IntMora==1, 1, 0)) %>%  # Este es el ifelse que identifica al cliente VIP
  #Criterios antiguos que se deberían cumplir por el resto isinLast==1 & totalNops > 1
  # select(-ends_with('Bin'), -isinLast,-starts_with('Peor'), -ends_with('Credit'),
  #        -ConCapInt_USD, -meanMoraIM_cl, -tuvoMora_pct, -starts_with('CR')) %>% 
  glimpse()

sapply(finalVIP, function(x){length(which(is.na(x)))})

bdcVIPScore <- finalVIP %>% 
  select(CTACLIENTE, totalMonto, minMonto, maxMonto, totalNops, montoRat, totalLoanDays,
         pct_acumDM_TLD, pct_maxDM_acumDM, moraAcum, moraMax, totalBC, totalWC,
         maxMoraIM_cl, excelenteNegocios, target) %>% 
  glimpse()

table(bdcVIPScore$target)

write.csv(bdcVIPScore, paste0('D:/!bso/vipCartera/dataScore_',shortmonth,'.csv'), row.names = F)

VIPs <- fread(paste0('D:/!bso/vipCartera/dataScore_',shortmonth,'.csv'))

#CON ESTE RESULTADO SE CALCULAN LOS SCORES EN PYTHON
#UNA VEZ SE EJECUTE EL RESULTADO EN PYTHON SE RETOMA A PARTIR DE AQUÍ

dfScore <- fread(paste0('D:/!bso/vipCartera/scores/vipList_wScores_',shortmonth,'.csv'), sep = ',', fill = T) %>% 
  select(CTACLIENTE, prob_GC, goodClient)

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
  mutate(Seg_Merc_Asalariado = ifelse(DESC_SEGMERC=="Asalariado",1,0)) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(Seg_Merc_Asalariado = ifelse(max(Seg_Merc_Asalariado)>0,'Es asalariado','No es asalariado')) %>% 
  ungroup() %>% 
  select(OPERACION, CI, CTACLIENTE, NOMBRE_CLIENTE, GENERO, NOMBRE_ASESOR, NOMBRE_AGENCIA, 
         FDESEMBOLSO, CAEDEC_DC, CPOP, SECTOR_CARTERA, DESC_SEGMERC,`Categoría` = categ, 
         `Tipo_Crédito` = tipoCred, Sucursal, Rango_Monto = rangom, MONTO_USD = montous,
         intus, SALDO_USD = saldous,ctaCont, Seg_Merc_Asalariado)

vipDescFull <- bdcLast %>% 
  left_join(finalVIP, by = 'CTACLIENTE') %>%
  left_join(select(dfScore, CTACLIENTE, prob_GC), by = 'CTACLIENTE') %>%
  mutate(pctSaldo = SALDO_USD/MONTO_USD) %>% 
  mutate(Amortización = 1 - pctSaldo) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(vipViable = ifelse(target == 1 & pctSaldo <= 0.8, 1, 0)) %>% #Criterio de amortización de al menos el 20%
  mutate(oportunity_raw = MONTO_USD-SALDO_USD) %>% 
  mutate(oportunity = ifelse(target == 1, MONTO_USD - SALDO_USD, 0)) %>% 
  mutate(oportunityViable = ifelse(vipViable == 1, oportunity, 0)) %>% 
  mutate(Cliente_Viable = max(vipViable)) %>% 
  ungroup() %>%
  mutate(ctileProb = ntile(prob_GC, n = 100)) %>% 
  mutate(dtileProb = ntile(prob_GC, n = 10)) %>%
  mutate(Lista_GNN = ifelse(excelenteNegocios == 1, 'En Lista', 'Fuera de Lista'),
         VIP_AR = ifelse(target==1, 'VIP-AR', 'Regular'),
         VIP_Viable = ifelse(vipViable == 1, 'Viable', 'No viable'),
         Cliente_Viable = ifelse(Cliente_Viable == 1, "Cliente Viable", "Cliente No Viable")) %>%
  # dplyr::filter(VIP_RC == 'VIP-RC') %>% # filtro de VIPs
  glimpse() %>% 
  select(OPERACION, CI, CTACLIENTE, GENERO, NOMBRE_CLIENTE, NOMBRE_ASESOR, Sucursal, ctaCont,
         NOMBRE_AGENCIA, FDESEMBOLSO, CAEDEC_DC, CPOP, SECTOR_CARTERA, `Categoría`, `Tipo_Crédito`,
         Rango_Monto, MONTO_USD, SALDO_USD, Lista_GNN, `Amortización`, Probabilidad_VIP = prob_GC, 
         Decil_Prob = dtileProb, Percentil_Prob = ctileProb, Cliente_Viable, 
         VIP_Viable, Oportunidad_cruda = oportunity_raw,Oportunidad = oportunity, Oportunidad_Viable = oportunityViable, VIP_AR, moraMax,
         maxMoraIM_cl, meanMoraIM_cl, maxMoraIMclBin, meanMoraIMclBin,totalBC, totalWC,  minMonto, maxMonto, montoRat, 
         totalLoanDays, tuvoMora_pct, tuvoMoraIMclBin, Seg_Merc_Asalariado,
         starts_with("CR_")) %>%
  mutate(Score = Probabilidad_VIP * Oportunidad_Viable) %>% 
  glimpse() 

nrow(vipDescFull)==nrow(bdcLast)
n_distinct(vipDescFull$CTACLIENTE[vipDescFull$VIP_AR=='VIP-AR'])
table(vipDescFull$VIP_AR)
table(vipDescFull$VIP_Viable)
sum(vipDescFull$Oportunidad_Viable)
table(vipDescFull$Lista_GNN,vipDescFull$VIP_Viable)

write_xlsx(vipDescFull, "D:/!bso/vipCartera/Criterios_Leads_Sep2023.xlsx")
####____AJUSTE DE COMPRA CON TODOS LOS CRÉDITOS EN SF____####
lastcred <- "Ago. 2023" #Se selecciona un mes anterior al cierre de cartera
shortcred <- str_replace(lastcred,". ","")#La última información de infocred tiene un mes de rezago

infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',shortcred,'.rds'))
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
  select(CTACLIENTE, NOMBRE, TIPO_OBLIGADO, SIGLA, ENTIDAD, 
         TIPO_CREDITO, CALIFICACION, ESTADO, DiasMora, FECHAINICIO, FECHAVTO, MontoOriginal, 
         Saldo_Vig_USD_SF = saldoVig, Periodo_Pago) #DEPARTAMENTO, HISTORICO = histLast16

compraCartera <- vipDescFull %>% 
  select(CTACLIENTE, CI, GENERO, Lista_GNN, Probabilidad_VIP, Cliente_Viable, Seg_Merc_Asalariado) %>% 
  distinct_all() %>% 
  inner_join(infoClientes,by="CTACLIENTE") %>% 
  dplyr::filter(DiasMora==0 & CALIFICACION=="A" & ESTADO=="1. VIGENTE") #Filtro que antes era manual y en Sep23 se incorpora al flujo

LeadsYCompra <- list(Leads=vipDescFull, Compra=compraCartera)
write_xlsx(LeadsYCompra, "D:/!bso/vipCartera/Criterios_Leads_Sep2023.xlsx")
####____LISTAS DE LEADS Y COMPRA____####
lastcred <- "Ago. 2023" #Se selecciona un mes anterior al cierre de cartera
shortcred <- str_replace(lastcred,". ","")#La última información de infocred tiene un mes de rezago

infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',shortcred,'.rds'))
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
  select(CTACLIENTE, NOMBRE, TIPO_OBLIGADO, SIGLA, ENTIDAD, 
         TIPO_CREDITO, CALIFICACION, ESTADO, DiasMora, FECHAINICIO, FECHAVTO, MontoOriginal, 
         Saldo_Vig_USD_SF = saldoVig, Periodo_Pago) #DEPARTAMENTO, HISTORICO = histLast16

vipFiltrada <- vipDescFull %>% 
  dplyr::filter(VIP_AR=="VIP-AR") %>% 
  mutate(Grupo_Exclusivo = ifelse(CTACLIENTE %in% infoClientes$CTACLIENTE, 'Exclusivo','No Exclusivo')) %>% 
  select(OPERACION, CI, CTACLIENTE, GENERO, NOMBRE_CLIENTE, NOMBRE_ASESOR, Sucursal,
         ctaCont, NOMBRE_AGENCIA, FDESEMBOLSO, CAEDEC_DC, CPOP, SECTOR_CARTERA, Categoría,
         Tipo_Crédito, Rango_Monto, MONTO_USD, SALDO_USD, Lista_GNN, Amortización, 
         Probabilidad_VIP, Decil_Prob, Grupo_Exclusivo, VIP_Viable, Oportunidad, 
         Oportunidad_Viable, Score, Seg_Merc_Asalariado)

compraCartera <- vipDescFull %>% 
  dplyr::filter(VIP_AR=="VIP-AR") %>% 
  select(CTACLIENTE, CI, GENERO, Lista_GNN, Probabilidad_VIP, Cliente_Viable, Seg_Merc_Asalariado) %>% 
  distinct_all() %>% 
  inner_join(infoClientes,by="CTACLIENTE") %>% 
  dplyr::filter(DiasMora==0 & CALIFICACION=="A" & ESTADO=="1. VIGENTE") #Filtro que antes era manual y en Sep23 se incorpora al flujo

ListasVIP <- list(PreAprobados = vipFiltrada, CompraCartera=compraCartera)

writexl::write_xlsx(ListasVIP,paste0("D:/!bso/vipCartera/Entrega_Leads_PreAprobados_CompraCartera_",shortmonth,".xlsx"))

Leads <- read_xlsx(paste0("D:/!bso/vipCartera/Entrega_Leads_PreAprobados_CompraCartera_",shortmonth,".xlsx"), sheet = "PreAprobados")

LeadsFueraDeLista <- VIPs %>% 
  mutate(monDate = as.yearmon("Sep. 2023")) %>% 
  anti_join(Leads, by=c("CTACLIENTE")) %>% 
  left_join(dfLastCierre, by=c("CTACLIENTE")) %>% 
  mutate(FechaCese = as.Date(myLastCierre, frac=1)) %>% 
  rename(FechaUltDesembolso = myLastDesembolso) %>% 
  mutate(DifMeses = (monDate - myLastCierre)*12) %>% 
  dplyr::filter(target==1)

####____ADDING CLIENT INFORMATION____####
Clientes <- readRDS("D:/!bso/features/Clientes_Ene15Sep23.rds") %>% 
  group_by(CTACLIENTE) %>% 
  arrange(desc(monDate)) %>% 
  mutate(Sucursal = cut(AGENCIA, breaks = c(100,200,250,300,400,500,600,700,800,900,1000),
                        labels= c('Chuquisaca','La Paz','El Alto','Cochabamba','Oruro',
                                  'Potosí','Tarija','Santa Cruz','Beni','Pando'), right=FALSE)) %>% 
  select(CTACLIENTE, OPERACION, CI, NOMBRE, GENERO, MONTOUS, monDate, AGENCIA, Sucursal) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

LeadsInfo <- LeadsFueraDeLista %>% 
  left_join(Clientes, by="CTACLIENTE") %>% 
  mutate(FechaCorte=as.Date(monDate.x, frac=1)) %>% 
  relocate(FechaCorte, .before = CTACLIENTE) %>% 
  select(-starts_with("monDate"), -myLastCierre) %>% 
  left_join(dfScore, by="CTACLIENTE")

write_xlsx(LeadsInfo, "D:/!bso/vipCartera/ClientesVIPFueraDeLista_Sep2023_v3.xlsx")

####____UNION CON CUENTAS DE AHORRO____####
LeadsViables <- LeadsInfo %>% 
  dplyr::filter(FechaUltDesembolso >= as.Date("2018-01-01"))

arch1 <- 'BaseCaptaciones_CA_Mensual_Sep2023.txt'
cah_mensualSep <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/01_Base_Capataciones_CAH_Mensual/',
                            arch1),encoding="UTF-8",sep="|") %>% 
  select(NRO_CUENTA,CTA_CLIENTE, FECHA_SALDO,NOMBRE,GENERO,MONEDA, CTA_CONTABLE_SALDO, TASA_REFERENCIAL,
         TIPO_PERSONA,ESTADO,INSTITUCIONAL,TIENE_SOLNET,TIENE_CRED, TIENE_DPF,
         COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
         MARCA_INSTITUCIONAL_FINANZAS, SALDO_SUS) %>% 
  mutate(SALDO_SUS=as.numeric(SALDO_SUS)) %>% 
  # dplyr::filter(SALDO_SUS>0) %>% 
  mutate(TASA_REFERENCIAL=as.numeric(TASA_REFERENCIAL)/100) %>% 
  group_by(CTA_CLIENTE) %>% 
  summarise(NROCTAS_Sep23 = n_distinct(NRO_CUENTA),
            SALDOUS_Sep23 = sum(SALDO_SUS)) %>% 
  ungroup()

arch2 <- 'BaseCaptaciones_CA_Mensual_Oct2023.txt'
cah_mensualOct <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/01_Base_Capataciones_CAH_Mensual/',
                               arch2),encoding="UTF-8",sep="|") %>% 
  select(NRO_CUENTA,CTA_CLIENTE, FECHA_SALDO,NOMBRE,GENERO,MONEDA, CTA_CONTABLE_SALDO, TASA_REFERENCIAL,
         TIPO_PERSONA,ESTADO,INSTITUCIONAL,TIENE_SOLNET,TIENE_CRED, TIENE_DPF,
         COD_AGENCIA_ASOC,NOM_AGENCIA_ASOC,NOM_REGIONAL_ASOC,
         MARCA_INSTITUCIONAL_FINANZAS, SALDO_SUS) %>% 
  mutate(SALDO_SUS=as.numeric(SALDO_SUS)) %>% 
  # dplyr::filter(SALDO_SUS>0) %>% 
  mutate(TASA_REFERENCIAL=as.numeric(TASA_REFERENCIAL)/100) %>% 
  group_by(CTA_CLIENTE) %>% 
  summarise(NROCTAS_Oct23 = n_distinct(NRO_CUENTA),
            SALDOUS_Oct23 = sum(SALDO_SUS)) %>% 
  ungroup()

LeadsViablesJoin <- LeadsViables %>% 
  left_join(cah_mensualSep, by=c("CTACLIENTE"="CTA_CLIENTE")) %>% 
  left_join(cah_mensualOct, by=c("CTACLIENTE"="CTA_CLIENTE")) %>% 
  mutate(TieneCTA_Sep23 = ifelse(!is.na(SALDOUS_Sep23),1,0)) %>% 
  mutate(TieneCTA_Oct23 = ifelse(!is.na(SALDOUS_Oct23),1,0)) %>% 
  mutate(DIF_SALDO = ifelse(!is.na(SALDOUS_Oct23), SALDOUS_Oct23-SALDOUS_Sep23,0)) %>% 
  mutate(VAR_SALDO = ifelse(!is.na(SALDOUS_Sep23) & SALDOUS_Sep23!=0, SALDOUS_Oct23/SALDOUS_Sep23-1,0))
  

write_xlsx(LeadsViablesJoin, "D:/!bso/requests/ClientesVIPFueraDeLista_Sep2023_v4.xlsx")

table(LeadsViablesJoin$TieneCTA)

####____SEGUIMIENTO A CARTERA DE CAMPAÑA____####
longmonth <- "Oct. 2023"
shortmonth <- str_replace(longmonth,". ","")

leads <- c("May2023_v2", "Jun2023_v3", "Jul2023", "Ago2023", "Sep2023")
mydaily <- c("May2023", "Jun2023", "Jul2023", "Ago2023", "Sep2023")
lista_gnn <- c("202305", "202306", "202307", "202308", "202309")
mylead <- c("May. 2023", "Jun. 2023", "Jul. 2023", "Ago. 2023", "Sep. 2023","Oct. 2023")

Clientes <- readRDS("D:/!bso/features/Clientes_Ene15Oct23.rds")
ClientesVIP <- Clientes %>% 
  mutate(CampaniaVIP_rezago=0,
         CampaniaVIP_anclado=0,
         CampaniaGNN_rezago=0,
         CampaniaGNN_anclado=0) %>%
  mutate(FueraLista = 0)

i <- 1
for (i in 1:length(leads)) {
  print(mylead[i])
  listaVIP <- read_xlsx(paste0("D:/!bso/vipCartera/Entrega_Leads_PreAprobados_CompraCartera_",leads[i],".xlsx"),
                        sheet = "PreAprobados") %>% 
    mutate(VIP_AR = 1) %>% 
    select(CTACLIENTE, VIP_AR) %>% 
    distinct_all()
  daily <- readRDS(paste0("D:/!bso/vipCartera/dailyMM/dailyMM_",mydaily[i],".rds")) 
  listaVIP <- listaVIP %>% 
    left_join(daily, by="CTACLIENTE") %>% 
    mutate(Filtrado = ifelse(maxMoraIM_cl!=0, 1, 0)) %>% 
    select(-maxMoraIM_cl)
  
  gnn <- read.delim(paste0('D:/!bso/vipCartera/excelentes/Clientes_excelentes_',lista_gnn[i],'.csv'),
                    sep = '|') %>%
    mutate(GNN=1) %>% 
    select(CTACLIENTE=Cuenta, GNN) %>% 
    distinct_all()
  
  ClientesVIP <- ClientesVIP %>% 
    left_join(listaVIP, by = "CTACLIENTE") %>% 
    left_join(gnn, by = "CTACLIENTE") %>% 
    replace_na(list(VIP_AR=0, GNN=0)) %>% 
    mutate(FueraLista = case_when(FueraLista==1 ~ FueraLista,
                                  !is.na(Filtrado) & Filtrado==1~ 1,
                                  TRUE~FueraLista)) %>% 
    mutate(CampaniaVIP_rezago = case_when(CampaniaVIP_rezago==1 ~ 1,
                                   VIP_AR==1 & as.yearmon(fdes)==mylead[i+1] ~ 1,
                                   VIP_AR==1 & as.yearmon(fdes)!=mylead[i+1] ~ 0,
                                   TRUE~CampaniaVIP_rezago)) %>% 
    mutate(CampaniaVIP_anclado = case_when(CampaniaVIP_anclado==1 ~ 1,
                                          VIP_AR==1 & as.yearmon(fdes)>=mylead[i+1] ~ 1,
                                          VIP_AR==1 & as.yearmon(fdes)<mylead[i+1] ~ 0,
                                          TRUE~CampaniaVIP_anclado)) %>% 
    mutate(CampaniaGNN_rezago = case_when(CampaniaGNN_rezago==1 ~ 1,
                                          GNN==1 & as.yearmon(fdes)==mylead[i+1] ~ 1,
                                          GNN==1 & as.yearmon(fdes)!=mylead[i+1] ~ 0,
                                          TRUE~CampaniaGNN_rezago)) %>% 
    mutate(CampaniaGNN_anclado = case_when(CampaniaGNN_anclado==1 ~ 1,
                                           GNN==1 & as.yearmon(fdes)>=mylead[i+1] ~ 1,
                                           GNN==1 & as.yearmon(fdes)<mylead[i+1] ~ 0,
                                           TRUE~CampaniaGNN_anclado)) %>% 
    select(-VIP_AR, -GNN, -Filtrado)
}

ClientesVIP %>% 
  dplyr::filter(fdes>=as.Date("2023-01-01")) %>% 
  group_by(as.yearmon(fdes), CampaniaVIP_rezago) %>% 
  summarise(N=n(), MONTOUS=sum(MONTOUS)) %>% 
  ungroup() %>% pivot_wider(names_from = CampaniaVIP_rezago, values_from = c(N, MONTOUS))

ClientesVIP %>% 
  dplyr::filter(CampaniaVIP_anclado==1) %>% 
  group_by(ctaCont, fueRefin) %>% 
  summarise(N=n())

ClientesVIP %>% 
  dplyr::filter(CampaniaVIP_anclado==1) %>% 
  dplyr::filter(fueRefin==1) %>% 
  mutate(Mark = ifelse(FechaRefin==fdes, 1, 0)) %>% 
  group_by(ctaCont, fueRefin, Mark) %>% 
  summarise(N=n())

x <- ClientesVIP %>% 
  dplyr::filter(CampaniaVIP_anclado==1 & fueRefin==1 & ctaCont=='135')

####____METRICAS DE ALERTA____####
codMod <- read_xlsx("D:/!bso/bases/excel/CodModulo.xlsx")
bdcMetrics <- read_parquet(paste0("D:/!bso/features/Metricas_",shortmonth,".parquet")) 

bdcNow <- readRDS("D:/!bso/girCartera/rds/ec_Oct2023.rds")
bdcExp <- bdcNow %>% 
  left_join(codMod, by="MODULO") %>% 
  dplyr::filter(ctaCont %in% c("131",'133','134','135','136','137')) %>% 
  mutate(OpsBruta = 1,
         OpsMora = ifelse(DIASMORA>0, 1, 0)) %>% 
  mutate(MontoDes = ifelse(MONEDA==0, MONTO/6.86, MONTO)) %>% 
  select(CTACLIENTE, OPERACION, Sucursal, NOMBRE_AGENCIA, NOMBRE_ASESOR, Sector_Actividad,
         Sector_Destino, NOMBRE_MODULO, tipoCred, MontoDes, SaldoBruto = saldous, 
         SaldoMora = par0, OpsBruta, OpsMora, FVEN_PROXPAGO) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(Nops = n(),
         MontoTotal = sum(MontoDes),
         SaldoTotal = sum(SaldoBruto)) %>% 
  ungroup() %>% 
  mutate(RangoMonto = case_when(MontoTotal<5000 ~ '1. <5000',
                                MontoTotal<10000 ~ '2. <10000',
                                MontoTotal<15000 ~ '3. <15000',
                                MontoTotal<20000 ~ '4. <20000',
                                MontoTotal>=20000 ~ '5. >=2000',)) %>% 
  mutate(RangoSaldo = case_when(SaldoTotal<5000 ~ '1. <5000',
                                SaldoTotal<10000 ~ '2. <10000',
                                SaldoTotal<15000 ~ '3. <15000',
                                SaldoTotal<20000 ~ '4. <20000',
                                SaldoTotal>=20000 ~ '5. >=20000',)) %>% 
  left_join(select(ClientesVIP, CTACLIENTE, OPERACION, starts_with("Campania"), fueRefin, fueReprog, FueraLista), 
            by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(bdcMetrics, by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(Compartido = ifelse(is.na(califPeorSF), 0, 1)) %>% 
  mutate(ConMoraSF = ifelse(!is.na(MaxDiasMoraSF) & MaxDiasMoraSF>0, 1, 0)) %>% 
  mutate(SaldoConMIM1D = ifelse(MoraIntraMes>1, SaldoBruto, 0)) %>% 
  mutate(SaldoConMIM5D = ifelse(MoraIntraMes>5, SaldoBruto, 0)) %>% 
  mutate(SaldoConMIM10D = ifelse(MoraIntraMes>10, SaldoBruto, 0)) %>% 
  mutate(OpsConMIM1D = ifelse(MoraIntraMes>1, 1, 0)) %>% 
  mutate(OpsConMIM5D = ifelse(MoraIntraMes>5, 1, 0)) %>% 
  mutate(OpsConMIM10D = ifelse(MoraIntraMes>10, 1, 0))

write_xlsx(bdcExp, paste0("D:/!bso/vipCartera/VIPSeguimiento_",shortmonth,"_v3.xlsx"))

####____COMPARING DAILYMM TO BDCMONTHLY____####
my <- "202310"
dAux <- readRDS(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/vipCartera/rds/cons_",
                       my,".rds")) 
sort(unique(dAux$dayDate))
dailyMIM <- dAux %>% 
  select(CTACLIENTE, OPERACION, DIASMORA) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  summarise(maxMoraIM_cl = max(DIASMORA, na.rm = T)) %>% 
  ungroup()

daily <- readRDS(paste0("D:/!bso/vipCartera/dailyMM/dailyMM_",shortmonth,".rds")) 

OctJoin <- bdcMetrics %>% 
  left_join(select(bdcNow, CTACLIENTE, OPERACION, monDate, FVEN_ULTPAGO, FVEN_PROXPAGO, FULT_PAGO),
            by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(dailyMIM, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(select(daily, CTACLIENTE, MIM_cl = maxMoraIM_cl), by=c("CTACLIENTE")) %>% 
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
  mutate(Igual = maximaDPD==maxMoraIM_cl) %>% 
  group_by(CTACLIENTE) %>% 
  summarise(maxMoraIM_cl = max(maxMoraIM_cl),
            maximaDPD = max(maximaDPD),
            MIM_cl = max(MIM_cl),
            MoraIntraMes = max(MoraIntraMes)) %>% 
  ungroup() %>% 
  mutate(CoincideD = case_when(maxMoraIM_cl==0 & MIM_cl==0 ~ 1,
                               maxMoraIM_cl>0 & MIM_cl>0 ~1,
                               TRUE~0)) %>% 
  mutate(CoincideM = case_when(maximaDPD==0 & MoraIntraMes==0 ~ 1,
                               maximaDPD>0 & MoraIntraMes>0 ~1,
                               TRUE~0)) %>% 
  mutate(NoBDCSiDaily  = ifelse(maxMoraIM_cl>0 & maximaDPD==0, 1, 0)) %>% 
  mutate(NoDailySiBDC  = ifelse(maxMoraIM_cl==0 & maximaDPD>0, 1, 0)) 

table(OctJoin$NoBDCSiDaily, OctJoin$NoDailySiBDC)