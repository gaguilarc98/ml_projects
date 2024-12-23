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
library(arrow)
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
####____MEASURES OF PERFORMANCE BY CLIENT____####
#AT THIS STAGE WE DO NOT NEED DATA FROM THE CLIENTS OTHER THAN THEIR CTACLIENTE
#AND OBVIOUSLY OTHER VARIABLES TO GET THE RELEVANT MEASURES FOR THE MODEL
dfOps <- readRDS('D:/!bso/features/Historial_Operaciones.rds') %>% 
  glimpse()

lastmonth <- "Oct. 2023"
shortmonth <- str_replace(lastmonth,". ","")
n_distinct(dfOps$CTACLIENTE[dfOps$fdes>=as.Date("2015-01-01") & dfOps$monDate==lastmonth])

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
HistCalif <- readRDS("D:/!bso/vipCartera/historic/HistCalif_Sep2023.rds")
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
# condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Oct2023.rds') %>% 
condFull <- read_parquet('D:/!bso/condonaciones/CondFull.parquet') %>%   
  mutate(monDate = as.yearmon(Fecha)) %>%
  dplyr::filter(monDate>=as.yearmon("Ene. 2019")) %>% 
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
moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Oct2023.rds")) %>% 
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
bus <- read.delim('D:/!bso/vipCartera/excelentes/Clientes_excelentes_202310.csv',
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
sum(vipDescFull$Oportunidad_Viable,na.rm = T)
table(vipDescFull$Lista_GNN,vipDescFull$VIP_Viable)

write_xlsx(vipDescFull, "D:/!bso/vipCartera/Criterios_Leads_Oct2023.xlsx")
####____AJUSTE DE COMPRA CON TODOS LOS CRÉDITOS EN SF____####
lastcred <- "Sep. 2023" #Se selecciona un mes anterior al cierre de cartera
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
  select(CTACLIENTE, CI, GENERO, Lista_GNN, Probabilidad_VIP, Prom_DiasMora_Intrames = meanMoraIMclBin,
         `MesesConMoraIntraMes (%)`= tuvoMoraIMclBin,Cliente_Viable, Seg_Merc_Asalariado) %>% 
  distinct_all() %>% 
  inner_join(infoClientes,by="CTACLIENTE") %>% 
  dplyr::filter(DiasMora==0 & CALIFICACION=="A" & ESTADO=="1. VIGENTE") #Filtro que antes era manual y en Sep23 se incorpora al flujo

LeadsYCompra <- list(Leads=vipDescFull, Compra=compraCartera)
write_xlsx(LeadsYCompra, "D:/!bso/vipCartera/Criterios_Leads_Sep2023.xlsx")
####____LISTAS DE LEADS Y COMPRA____####
lastcred <- "Sep. 2023" #Se selecciona un mes anterior al cierre de cartera
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
  group_by(CTACLIENTE) %>% 
  mutate(Sucursal = Sucursal[row_number()==1]) %>% 
  mutate(NOMBRE_AGENCIA = NOMBRE_AGENCIA[row_number()==1]) %>% 
  mutate(NOMBRE_ASESOR = NOMBRE_ASESOR[row_number()==1]) %>% 
  ungroup() %>% 
  select(CTACLIENTE, CI, GENERO, Sucursal, NOMBRE_AGENCIA, NOMBRE_ASESOR, Lista_GNN, 
         Prom_DiasMora_Intrames = meanMoraIMclBin, `MesesConMoraIntraMes (%)`= tuvoMoraIMclBin,
         Probabilidad_VIP, Seg_Merc_Asalariado) %>% 
  distinct_all() %>% 
  inner_join(infoClientes,by="CTACLIENTE") %>% 
  dplyr::filter(DiasMora==0 & CALIFICACION=="A" & ESTADO=="1. VIGENTE") #Filtro que antes era manual y en Sep23 se incorpora al flujo

####____LISTA DE VIPS NO ACTIVOS____####
codAge <- read_excel("D:/!bso/bases/excel/codAgeSucReg.xlsx", sheet = "Full")
dfLastCierre <- readRDS("D:/!bso/features/Clientes_Ene15Oct23.rds") %>% 
  group_by(CTACLIENTE) %>%
  arrange(desc(fdes)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>%
  left_join(codAge, by="AGENCIA") %>% 
  mutate(FechaCese = as.Date(monDate,frac=1)) %>% 
  select(CTACLIENTE, OPERACION, CI, NOMBRE, GENERO, FechaUltDesembolso = fdes, 
         MontoUltDesembolso = MONTOUS, FechaCese, Sucursal, NOMBRE_AGENCIA)

vipOut <- finalVIP %>% 
  mutate(monDate = as.yearmon("Oct. 2023")) %>% 
  anti_join(vipFiltrada, by=c("CTACLIENTE")) %>% 
  left_join(select(dfScore, CTACLIENTE, prob_GC), by = 'CTACLIENTE') %>%
  left_join(dfLastCierre, by=c("CTACLIENTE")) %>% 
  mutate(myLastCierre= as.yearmon(FechaCese, frac=1)) %>% 
  mutate(DifMesesCese = (monDate - myLastCierre)*12) %>% 
  mutate(VIP_AR = ifelse(target==1, 'VIP-AR', 'Regular')) %>% 
  dplyr::filter(target==1) %>% 
  select(CTACLIENTE, UltimaOperacion = OPERACION, CI, NOMBRE, GENERO, FechaUltDesembolso, 
         MontoUltDesembolso, FechaCese, DifMesesCese, Sucursal, NOMBRE_AGENCIA, 
         VIP_AR, Probabilidad_VIP = prob_GC, MontoMinimo = minMonto, MontoMaximo = maxMonto, 
         MontoTotal = totalMonto, CantOperaciones = totalNops, 
         PromedioMoraIM = meanMoraIM_cl, IntensidadMesesMoraIM = tuvoMora_pct)

vipOutExp <- vipOut %>% 
  dplyr::filter(FechaUltDesembolso >= as.Date("2018-01-01"))

####____SAVE LIST WITH THREE SETS____####
ListasVIP <- list(PreAprobados = vipFiltrada, CompraCartera=compraCartera, SalidaClientesVIP=vipOutExp)

writexl::write_xlsx(ListasVIP,paste0("D:/!bso/vipCartera/Entrega_Leads_PreAprobados_CompraCartera_",shortmonth,"_v2.xlsx"))

length(compraCartera$Saldo_Vig_USD_SF[compraCartera$CALIFICACION=='A'])
sum(compraCartera$Saldo_Vig_USD_SF[compraCartera$CALIFICACION=='A'])
mean(compraCartera$Saldo_Vig_USD_SF[compraCartera$CALIFICACION=='A'])
