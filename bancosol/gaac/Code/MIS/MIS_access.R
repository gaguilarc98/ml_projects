####____CARGA DE LIBRERIAS Y FUNCIONES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(lubridate)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(quantmod)
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(scales)
library(ggplot2)
library(gt)
library(knitr)
library(openxlsx)
library(kableExtra)
library(RODBC)
library(DBI)
cases <- function(quant,levs,values,default=NA){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- rep(default,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3","slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____CONNECTING TO DATABSE____####
# MIS2 <- dbConnect("MISFeb23")
MIS <- odbcConnect("MIS_Feb23")
sqlTables(MIS)
names <- sqlTables(MIS,tableType = "TABLE")$TABLE_NAME
for (i in names) {
  print(i)
  print(sqlColumns(MIS,i)$COLUMN_NAME)
}
sqlTables(MIS,tableType = "VIEW")
sqlColumns(MIS,"ACT_CLIE_CRED_DDBB")

ACT_CLIE <- sqlFetch(MIS,"ACT_CLIE_CRED_DDBB")
glimpse(ACT_CLIE)
MIS_MERCADO <- sqlColumns(MIS,"MIS_MERCADO")

RAT00 <- sqlFetch(MIS,"Rat_00")
RATNOM <- sqlFetch(MIS,"Rat_00_Nombres")
Mercado <- sqlFetch(MIS,"EEFF_Saldos00Mercado")
Agru <- sqlFetch(MIS,"EEFF_Saldos00Corp_agru")

Company <- sqlFetch(MIS,"P01B_Company") %>% 
  rename(Company2=ID_Company)
Rubro <- sqlFetch(MIS,"P05_Rubro")
Rubro2 <- sqlFetch(MIS,"P05B_Rubro")

Companies <- sqldf::sqldf(
"SELECT Fecha, Company2, N01, N02, N03, N05, N06, N07, N08, BOB, Desc
FROM Mercado")

CompQuery <- Companies %>% 
  dplyr::filter(Fecha>as.Date("2019-12-31")) %>% 
  group_by(Company2,Fecha) %>% 
  mutate(SUS=BOB/6.86) %>% 
  mutate(Patrimonio=sum(SUS[N01==3 & N02==0 & N03==0])) %>% 
  mutate(Activo_Total=sum(SUS[N01==1 & N02==0 & N03==0])) %>% 
  mutate(Diversas=sum(SUS[N01==1 & N02==4 & N03==3])) %>% 
  mutate(Cart_Bruta=sum(SUS[N01==1 & N02==3 & N03 %in% c(1,3,4,5,6,7)])) %>% 
  mutate(Cart_Total=sum(Cart_Bruta+SUS[N01==6 & N02==0 & N03==0])) %>% 
  mutate(Cart_Vig=sum(SUS[N01==1 & N02==3 & N03 %in% c(1,5)])) %>% 
  mutate(Cart_Mora=sum(SUS[N01==1 & N02==3 & N03 %in% c(3,4,6,7)])) %>% 
  mutate(Cart_Dif=sum(SUS[N01==1 & N02==3 & (N03 %in% c(1,5,6,7) & N05==50) & (N03 %in% c(3,4) & N05==52)])) %>% 
  mutate(Prod_Dev=sum(SUS[N01==1 & N02==3 & N03==8])) %>% 
  mutate(PDev_Dif=sum(SUS[N01==1 & N02==3 & N03==8 & N05 %in% c(50:55)])) %>% 
  mutate(Int_Dif=sum(SUS[N01==1 & N02==3 & N03==8 & N05 %in% c(50,55)])) %>% 
  mutate(Prev_Incob_Cart=-sum(SUS[N01==1 & N02==3 & N03==9])) %>% 
  mutate(Prev_Dif=-sum(SUS[N01==1 & N02==3 & N03==9 & N05 %in% 50:55])) %>% 
  mutate(Prev_Esp=-sum(SUS[N01==1 & N02==3 & N03==9 & N05  %in% c(1,3:7,50:55)])) %>% 
  mutate(Prev_Gen_Otros=-sum(SUS[N01==1 & N02==3 & N03==9 & N05==9])) %>% 
  mutate(Prev_250=sum(SUS[N01==2 & N02==5 & N03 %in% c(1,3,5)])) %>% 
  mutate(Prev_Cic=sum(SUS[N01==2 & N02==5 & (N03 %in% c(1,5) | (N03==3 & N05==3))])) %>% 
  mutate(Perfil_Prev=sum(SUS[N01==2 & N02==5 & N03 %in% c(1,3,5)])) %>% 
  mutate(Cart_Reprog=sum(SUS[N01==1 & N02==3 & N03 %in% c(5,6,7)])) %>%
  mutate(Cart_RepVig=sum(SUS[N01==1 & N02==3 & N03 %in% c(5)])) %>%
  mutate(Cart_Rep_Mora=sum(SUS[N01==1 & N02==3 & N03 %in% c(6,7)])) %>%
  mutate(Cart_Dif_Mora=sum(SUS[N01==1 & N02==3 & N03 %in% c(3,4,6,7) & N05 %in% 50:58])) %>%
  mutate(Cart_Cast=sum(SUS[N01==8 & N02==6 & N03==5 & N05==1])) %>% 
  select(-Desc,-N01,-N02,-N03,-N05,-N06,-N07,-N08,-BOB,-SUS) %>% 
  summarise_all(max) %>% 
  ungroup() %>% 
  mutate(mora = Cart_Mora/Cart_Bruta,
         CR_CB = Cart_Reprog/Cart_Bruta,
         CRV_CB = Cart_RepVig/Cart_Bruta,
         CRM_CR = (Cart_Reprog-Cart_RepVig)/Cart_Reprog,
         IMCAST = (Cart_Mora+Cart_Cast)/(Cart_Bruta+Cart_Cast),
         CR_CT = (Cart_Mora+Cart_Cast)/Cart_Bruta,
         CD_CB = Cart_Dif/Cart_Bruta,
         PEDPG_CDID = (Prev_Dif+Prev_Gen_Otros)/(Cart_Dif+Int_Dif),
         PROD_DEV_CB = Prod_Dev/Cart_Bruta,
         PDEV_DIF_PDEV = PDev_Dif/Prod_Dev,
         PT_CT = (Prev_Incob_Cart+Prev_250)/Cart_Total,
         PT_CM = (Prev_Incob_Cart+Prev_250)/Cart_Mora,
         Cvig_CB = Cart_Vig/Cart_Bruta,
         Cvig_AT = Cart_Vig/Activo_Total,
         PIncob_CM = Prev_Incob_Cart/Cart_Mora,
         PInEsp_CM = Prev_Esp/Cart_Mora,
         PIncob_CB = Prev_Incob_Cart/Cart_Bruta,
         PDif_PInEsp=Prev_Dif/Prev_Esp,
         PDif_CD = Prev_Dif/Cart_Dif,
         CRM_PAT = Cart_Rep_Mora/Patrimonio,
         PT_CB = (Prev_Incob_Cart+Prev_250)/Cart_Bruta,
         PEspCic_CM = (Prev_Esp+Prev_Cic)/Cart_Mora,
         Perf_CM = Perfil_Prev/Cart_Mora,
         PInEsp_CM = Prev_Esp/Cart_Mora,
         Div_AT = Diversas/Activo_Total,
         CDM_CD = Cart_Dif_Mora/Cart_Dif)

Prueba <- CompQuery %>% 
  select(Company2,Fecha,mora) %>% 
  pivot_wider(names_from = Company2,values_from = PEDPG_CDID)
####____DICCIONARIO____####
#201010100 Mora: Cartera Vencida y Ejecución/ Cartera Vencida, Ejecución y Vigente
#201010200 CR_CB: Cartera Reprogramada/ Cartera Bruta
#201010300 CRV_CB: Cartera Reprogramada Vigente/Cartera Bruta
#201010400 CRM_CR: Cartera Reprogramada en mora/Cartera reprogramada
#201010500 IMCAST: Índice de Mora más castigo, Cartera Mora+Castigada/ Cartera Bruta + Cartera Castigada
#201010600 CR_CT: Cartera Mora+Cartera Castigada/Cartera Bruta
#201020100 CD_CB: Cartera Diferida/Cartera Bruta
#201020200 PEDPG_CDID: Previsión específica Diferida + Previsión Genérica Otros/Cartera Diferida + Inter+es diferido
#201020301 PROD_DEV_CB: Productos devengados/Cartera Bruta
#201020302 PDEV_DIF_PDEV: Previsión específica/Cartera Mora
#201020500 PT_CT: Previsión total/Cartera Total
#201020600 PT_CM: Previsión total de cartera/Cartera en Mora
#201020800 CVig_CB: Cartera Vigente/Cartera Bruta
#201020900 Cvig_AT: Cartera Vigente/Activo_Total,
#201021000 PIncob_CM: Previsión por incobrabilidad de cartera/Cartera Mora
#201021100 PInEsp_CM: Previsión específica por incobrabilidad/Cartera Bruta
#201021200 PIncob_CB: Previsión por incobrabilidad/Cartera Bruta
#201021300 PT_CM: Es lo mismo que 201020600
#201021400 PDif_PInEsp: Previsión específica Diferida/Previsión específica por incobrabilidad
#201021500 PDif_CD: Previsión específica diferida/Capital Diferido
#201021600 CRM_PAT: Cartera Reprog (vencida + ejecución) /Patrimonio
#201021700 PT_CB: Previsión Total/Cartera Bruta
#201021800 PEspCic_CM: Previsión específica + cíclica/Cartera Mora
#201021900 Perf_CM: Perfil de previsión total/Cartera Mora
#201022000 PInEsp_CM: Previsión específica por incobrabilidad/Cartera en Mora
#301010100 Div_AT: Diversas cuentas por cobrar/Activo total
#JM CDM_CD 

####____A SOLICITUD DE JM____####
#Mora de cartera diferida
IMDIF <- CompQuery %>% 
  select(Company2,Fecha,CDM_CD) %>% 
  pivot_wider(names_from = Company2,values_from = CDM_CD)
####____COD_MERCADO____####
MIS <- odbcConnect("MISFeb23")
sqlTables(MIS,tableType = "TABLE")$TABLE_NAME
sqlTables(MIS,tableType = "VIEW")$TABLE_NAME
sqlColumns(MIS,"ACT_CLIE_CRED_DDBB")

ACT_CLIE <- sqlFetch(MIS,"ACT_CLIE_CRED_DDBB")
glimpse(ACT_CLIE)
MIS_MERCADO <- sqlColumns(MIS,"MIS_MERCADO")

Mercado <- sqlFetch(MIS,"EEFF_Saldos00Mercado",
                    as.is=c(F,rep(T,11),F))
Rubro <- sqlFetch(MIS,"P05_Rubro") %>% 
  select(-ID_Company) 
Rubro2 <- sqlFetch(MIS,"P05B_Rubro")
Company <- sqlFetch(MIS,"P01B_Company") %>% 
  rename(Company2=ID_Company) %>% 
  select(-FechaFin,-Company)
TC <- sqlFetch(MIS,"P03_TC",
               as.is=c(F,T,T,F))
Mon <- sqlFetch(MIS,"P02_EEFF_Nivel06",
                as.is=(rep(T,3)))
Nivel1 <- sqlFetch(MIS,"EEFF_Nivel01")
Nivel2 <- sqlFetch(MIS,"EEFF_Nivel02")
Nivel3 <- sqlFetch(MIS,"EEFF_Nivel03",
                   as.is=c(rep(T,4),T,F))
Nivel5 <- sqlFetch(MIS,"EEFF_Nivel05",
                   as.is=c(rep(T,6),F))
Nivel7 <- sqlFetch(MIS,"EEFF_Nivel07",
                   as.is=c(rep(T,8),F))
Nivel8 <- sqlFetch(MIS,"EEFF_Nivel08",
                   as.is=c(rep(T,9),F))

MercadoQuery <- Mercado %>%
  left_join(Nivel1, by=c("PlanCuenta","N01")) %>% 
  left_join(Nivel2, by=c("PlanCuenta","N01","N02")) %>% 
  left_join(Nivel3, by=c("PlanCuenta","N01","N02","N03")) %>% 
  left_join(Nivel5, by=c("PlanCuenta","N01","N02","N03","N05")) %>% 
  left_join(Nivel7, by=c("PlanCuenta","Company","N01","N02","N03","N05","N07")) %>% 
  left_join(Nivel8, by=c("PlanCuenta","Company","N01","N02","N03","N05","N07","N08")) %>% 
  left_join(Mon,by="N06") %>% 
  left_join(TC,by=c("Fecha","N06")) %>%
  left_join(Company,by=c("PlanCuenta","Company2")) %>% 
  left_join(Rubro,by=c("PlanCuenta","ID_Rubro")) %>% 
  left_join(Rubro2,by="ID_Bench") %>% 
  select(-contains("G00")) %>% 
  mutate(MIC=ifelse(Company2 %in% c("BLA","BFO","PLA","PEF","PCO","BPR","BIE"),1,0)) %>% 
  mutate(LEA=ifelse(Company2 %in% c("LBN","LFO","LBI"),1,0)) %>% 
  mutate(IFD=ifelse(Company2 %in% c("IFU","ICI","ICR","IFO","IID","IIM","IPM","ISA","IDI"),1,0)) %>% 
  mutate(COM=ifelse(Company2 %in% c("BCR","BEC","BGA","BIS","BME","BNB","BUN"),1,0)) %>% 
  mutate(EIF=ifelse(Company2 %in% c("BCR","BEC","BGA","BIS","BME","BNB","BUN","BLA","BFO","PLA","PEF","PCO","BPR","BIE","BSO","BFS"),1,0)) %>% 
  mutate(EFV=ifelse(Company2 %in% c("VPR","VL1","VPG"),1,0))

MercadoGlobal <- MercadoQuery %>% 
  dplyr::filter(Fecha>as.Date("2019-12-31")) %>%
  # dplyr::filter(PlanCuenta=="FIN") %>% 
  group_by(Company2,Fecha) %>% 
  mutate(SUS=BOB/6.86) %>% 
  mutate(Patrimonio=sum(SUS[N01=='3'])) %>% 
  mutate(Activo_Total=sum(SUS[N01=='1'])) %>% 
  mutate(Diversas=sum(SUS[N01=='1' & N02=='4' & N03=='3'])) %>% 
  mutate(Cart_Bruta=sum(SUS[N01=='1' & N02=='3' & N03 %in% c('1','3','4','5','6','7')])) %>% 
  mutate(Cart_Total=sum(SUS[N01=='6' | (N01=='1' & N02=='3' & N03 %in% c('1','3','4','5','6','7'))])) %>% 
  mutate(Cart_Vig=sum(SUS[N01=='1' & N02=='3' & N03 %in% c('1','5')])) %>% 
  mutate(Cart_Mora=sum(SUS[N01=='1' & N02=='3' & N03 %in% c('3','4','6','7')])) %>% 
  mutate(Cart_Dif=sum(SUS[N01==1 & N02==3 & ((N03 %in% c(1,5,6,7) & N05==50) | (N03 %in% c(3,4) & N05==52))])) %>% 
  mutate(Prod_Dev=sum(SUS[N01=='1' & N02=='3' & N03=='8'])) %>% 
  mutate(PDev_Dif=sum(SUS[N01=='1' & N02=='3' & N03=='8' & N05 %in% as.character(50:55)])) %>% 
  mutate(Int_Dif=sum(SUS[N01=='1' & N02=='3' & N03=='8' & N05 %in% as.character(50:55)])) %>% 
  mutate(Prev_Incob_Cart=-sum(SUS[N01=='1' & N02=='3' & N03=='9'])) %>% 
  mutate(Prev_Dif=-sum(SUS[N01=='1' & N02=='3' & N03=='9' & N05 %in% as.character(50:55)])) %>% 
  mutate(Prev_Esp=-sum(SUS[N01=='1' & N02=='3' & N03=='9' & N05  %in% c('01','03','04','05','06','07',50:55)])) %>% 
  mutate(Prev_Gen_Otros=-sum(SUS[N01=='1' & N02=='3' & N03=='9' & N05=='09'])) %>% 
  mutate(Prev_250=sum(SUS[N01=='2' & N02=='5' & N03 %in% c('1','3','5')])) %>% 
  mutate(Prev_Cic=sum(SUS[N01=='2' & N02=='5' & (N03 %in% c('1','5') | (N03=='3' & N05=='03'))])) %>% 
  mutate(Perfil_Prev=sum(SUS[N01=='2' & N02=='5' & N03 %in% c('1','3','5')])) %>% 
  mutate(Cart_Reprog=sum(SUS[N01=='1' & N02=='3' & N03 %in% c('5','6','7')])) %>%
  mutate(Cart_RepVig=sum(SUS[N01=='1' & N02=='3' & N03=='5'])) %>%
  mutate(Cart_Rep_Mora=sum(SUS[N01=='1' & N02=='3' & N03 %in% c('6','7')])) %>%
  mutate(Cart_Dif_Mora=sum(SUS[N01=='1' & N02=='3' & ((N03 %in% c(6,7) & N05==50) | (N03 %in% c(3,4) & N05==52))])) %>%
  mutate(Cart_Cast=sum(SUS[N01=='8' & N02=='6' & N03=='5' & N05=='01'])) %>% 
  select(-PlanCuenta,-Company,-contains("N0"),-contains("Desc"),
         -ICS,-Base,-TC,-ID_Rubro,-ID_Grupo,-ID_Bench,-Rubro,-Bench_Desc) %>% 
  summarise_all(max) %>% 
  ungroup() %>% 
  mutate(mora = Cart_Mora/Cart_Bruta,
         CR_CB = Cart_Reprog/Cart_Bruta,
         CRV_CB = Cart_RepVig/Cart_Bruta,
         CRM_CR = (Cart_Reprog-Cart_RepVig)/Cart_Reprog,
         IMCAST = (Cart_Mora+Cart_Cast)/(Cart_Bruta+Cart_Cast),
         CR_CT = (Cart_Mora+Cart_Cast)/Cart_Bruta,
         CD_CB = Cart_Dif/Cart_Bruta,
         PEDPG_CDID = (Prev_Dif+Prev_Gen_Otros)/(Cart_Dif+Int_Dif),
         PROD_DEV_CB = Prod_Dev/Cart_Bruta,
         PDEV_DIF_PDEV = PDev_Dif/Prod_Dev,
         PT_CT = (Prev_Incob_Cart+Prev_250)/Cart_Total,
         PT_CM = (Prev_Incob_Cart+Prev_250)/Cart_Mora,
         Cvig_CB = Cart_Vig/Cart_Bruta,
         Cvig_AT = Cart_Vig/Activo_Total,
         PIncob_CM = Prev_Incob_Cart/Cart_Mora,
         PInEsp_CM = Prev_Esp/Cart_Mora,
         PIncob_CB = Prev_Incob_Cart/Cart_Bruta,
         PDif_PInEsp=Prev_Dif/Prev_Esp,
         PDif_CD = Prev_Dif/Cart_Dif,
         CRM_PAT = Cart_Rep_Mora/Patrimonio,
         PT_CB = (Prev_Incob_Cart+Prev_250)/Cart_Bruta,
         PEspCic_CM = (Prev_Esp+Prev_Cic)/Cart_Mora,
         Perf_CM = Perfil_Prev/Cart_Mora,
         PInEsp_CM = Prev_Esp/Cart_Mora,
         Div_AT = Diversas/Activo_Total,
         CDM_CD = Cart_Dif_Mora/Cart_Dif)


agrupar <- function(x,VAR,value) {
  y <- x %>% 
    dplyr::filter({{VAR}}==1) %>% 
    select(Fecha,PlanCuenta,Company,Company2,N01,N02,N03,N05,N06,N07,N08,{{VAR}},BOB) %>% 
    mutate(Company2=value) %>% 
    select(-{{VAR}}) %>% 
    group_by(Fecha,PlanCuenta,Company,Company2,N01,N02,N03,N05,N06,N07,N08) %>% 
    summarise_all(sum)
  return(y)
}

MIC <- agrupar(MercadoQuery,MIC,"MIC")
LEA <- agrupar(MercadoQuery,LEA,"LEA")
IFD <- agrupar(MercadoQuery,IFD,"IFD")
COM <- agrupar(MercadoQuery,COM,"COM")
EIF <- agrupar(MercadoQuery,EIF,"EIF")
EFV <- agrupar(MercadoQuery,EFV,"EFV")
MercadoRubro <- bind_rows(MIC,LEA,IFD,COM,EIF,EFV)

MercadoRubro <- MercadoRubro %>% 
  dplyr::filter(Fecha>as.Date("2019-12-31")) %>%
  # dplyr::filter(PlanCuenta=="FIN") %>% 
  group_by(Company2,Fecha) %>% 
  mutate(SUS=BOB/6.86) %>% 
  mutate(Patrimonio=sum(SUS[N01=='3'])) %>% 
  mutate(Activo_Total=sum(SUS[N01=='1'])) %>% 
  mutate(Diversas=sum(SUS[N01=='1' & N02=='4' & N03=='3'])) %>% 
  mutate(Cart_Bruta=sum(SUS[N01=='1' & N02=='3' & N03 %in% c('1','3','4','5','6','7')])) %>% 
  mutate(Cart_Total=sum(SUS[N01=='6' | (N01=='1' & N02=='3' & N03 %in% c('1','3','4','5','6','7'))])) %>% 
  mutate(Cart_Vig=sum(SUS[N01=='1' & N02=='3' & N03 %in% c('1','5')])) %>% 
  mutate(Cart_Mora=sum(SUS[N01=='1' & N02=='3' & N03 %in% c('3','4','6','7')])) %>% 
  mutate(Cart_Dif=sum(SUS[N01==1 & N02==3 & (N03 %in% c(1,5,6,7) & N05==50 | N03 %in% c(3,4) & N05==52)])) %>% 
  mutate(Prod_Dev=sum(SUS[N01=='1' & N02=='3' & N03=='8'])) %>% 
  mutate(PDev_Dif=sum(SUS[N01=='1' & N02=='3' & N03=='8' & N05 %in% as.character(50:55)])) %>% 
  mutate(Int_Dif=sum(SUS[N01=='1' & N02=='3' & N03=='8' & N05 %in% as.character(50:55)])) %>% 
  mutate(Prev_Incob_Cart=-sum(SUS[N01=='1' & N02=='3' & N03=='9'])) %>% 
  mutate(Prev_Dif=-sum(SUS[N01=='1' & N02=='3' & N03=='9' & N05 %in% as.character(50:55)])) %>% 
  mutate(Prev_Esp=-sum(SUS[N01=='1' & N02=='3' & N03=='9' & N05  %in% c('01','03','04','05','06','07',50:55)])) %>% 
  mutate(Prev_Gen_Otros=-sum(SUS[N01=='1' & N02=='3' & N03=='9' & N05=='09'])) %>% 
  mutate(Prev_250=sum(SUS[N01=='2' & N02=='5' & N03 %in% c('1','3','5')])) %>% 
  mutate(Prev_Cic=sum(SUS[N01=='2' & N02=='5' & (N03 %in% c('1','5') | (N03=='3' & N05=='03'))])) %>% 
  mutate(Perfil_Prev=sum(SUS[N01=='2' & N02=='5' & N03 %in% c('1','3','5')])) %>% 
  mutate(Cart_Reprog=sum(SUS[N01=='1' & N02=='3' & N03 %in% c('5','6','7')])) %>%
  mutate(Cart_RepVig=sum(SUS[N01=='1' & N02=='3' & N03=='5'])) %>%
  mutate(Cart_Rep_Mora=sum(SUS[N01=='1' & N02=='3' & N03 %in% c('6','7')])) %>%
  mutate(Cart_Dif_Mora=sum(SUS[N01=='1' & N02=='3' & ((N03 %in% c(6,7) & N05==50) | (N03 %in% c(3,4) & N05==52))])) %>%
  mutate(Cart_Cast=sum(SUS[N01=='8' & N02=='6' & N03=='5' & N05=='01'])) %>% 
  select(-PlanCuenta,-Company2,-contains("N0"),-contains("Desc"),
         -Company) %>% 
  summarise_all(max) %>% 
  ungroup() %>% 
  mutate(mora = Cart_Mora/Cart_Bruta,
         CR_CB = Cart_Reprog/Cart_Bruta,
         CRV_CB = Cart_RepVig/Cart_Bruta,
         CRM_CR = (Cart_Reprog-Cart_RepVig)/Cart_Reprog,
         IMCAST = (Cart_Mora+Cart_Cast)/(Cart_Bruta+Cart_Cast),
         CR_CT = (Cart_Mora+Cart_Cast)/Cart_Bruta,
         CD_CB = Cart_Dif/Cart_Bruta,
         PEDPG_CDID = (Prev_Dif+Prev_Gen_Otros)/(Cart_Dif+Int_Dif),
         PROD_DEV_CB = Prod_Dev/Cart_Bruta,
         PDEV_DIF_PDEV = PDev_Dif/Prod_Dev,
         PT_CT = (Prev_Incob_Cart+Prev_250)/Cart_Total,
         PT_CM = (Prev_Incob_Cart+Prev_250)/Cart_Mora,
         Cvig_CB = Cart_Vig/Cart_Bruta,
         Cvig_AT = Cart_Vig/Activo_Total,
         PIncob_CM = Prev_Incob_Cart/Cart_Mora,
         PInEsp_CM = Prev_Esp/Cart_Mora,
         PIncob_CB = Prev_Incob_Cart/Cart_Bruta,
         PDif_PInEsp=Prev_Dif/Prev_Esp,
         PDif_CD = Prev_Dif/Cart_Dif,
         CRM_PAT = Cart_Rep_Mora/Patrimonio,
         PT_CB = (Prev_Incob_Cart+Prev_250)/Cart_Bruta,
         PEspCic_CM = (Prev_Esp+Prev_Cic)/Cart_Mora,
         Perf_CM = Perfil_Prev/Cart_Mora,
         PInEsp_CM = Prev_Esp/Cart_Mora,
         Div_AT = Diversas/Activo_Total,
         CDM_CD = Cart_Dif_Mora/Cart_Dif)

GG <- MercadoGlobal %>% 
  select(-MIC,-LEA,-IFD,-COM,-EIF,-EFV) 
tab <- bind_rows(GG,MercadoRubro)
tablas <- list(MIS_MERCADO=tab)
write.xlsx(tablas,'D:/!bso/MIS/tablasMISFeb23_desde2015.xlsx')

check <- tab %>% 
  select(Company2,Fecha,Cart_Bruta) %>% 
  pivot_wider(names_from = Company2,values_from = Cart_Bruta)
####____TODAVIA NO SIRVE____####
Comp <- Companies %>% 
  left_join(RAT00,by = c("N01","N02","N03","N05","N07")) %>% 
  rename(Ratio00=Rat_00) %>% 
  left_join(RATNOM,by = c("Ratio00")) %>% 
  mutate(SUS=BOB/6.86) %>% 
  dplyr::filter(!is.na(Frac)) %>% 
  group_by(Company2,Fecha,Ratio00,Frac) %>% 
  select(where(is.numeric)) %>% 
  summarise_all(sum) %>%
  ungroup() %>% 
  group_by(Company2,Fecha,Ratio00) %>%
  summarise(Num=sum(SUS[Frac==1]),
            Den=sum(SUS[Frac==2])) %>% 
  mutate(Indicador=ifelse(Den!=0,Num/Den,Den)) %>% 
  ungroup() %>% 
  arrange(Company2,Fecha,Ratio00)

  
#Check: PEDPG_CDID: Prev Esp + Prev Gener 139.09/Cart Dif + Int Dif
