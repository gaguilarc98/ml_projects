####____CARGA DE PAQUETES____####
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
library(expm)
library(quantmod)
library(stringr)
library(forcats)
library(janitor)
library(openxlsx)
require(XLConnect)

Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____DEFINICIONES____####
#Los créditos de vivienda se clasifican según los días de mora (DM)
#A si DM<=30, B si 31<=DM>=90, C si 91<=DM<=180, D si 181<=DM<=270, E si 271<=DM<=360, F si DM>360
#Los microcréditos se califican como sigue:
#A si DM<=5, B si 6<=DM<=30, C si 31<=DM<=55, D si 56<=DM<=75, E si 76<=DM<=90, F si DM>90
#Los microcréditos del sector agropecuario se califican como sigue:
#A si DM<=20, B si 21<=DM<=30, C si 31<=DM<=55, D si 56<=DM<=75, E si 76<=DM<=90, F si DM>90
####____KEEPING WORST CALIF SF FOR ITERATIONS____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
myrds <- myrds[-c(1:which(myrds=="Nov2021"),which(myrds=="Jun2023"):length(myrds))]
dfList <- list()
lag <- 1
i <- 1
for(i in 1:(length(myrds)-lag)) {
  tryCatch({
    print(myrds[i])
    k <- i + lag
    print(myrds[i+lag])
    if(i==1){
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i], '.rds')) %>%  #se abre mes anterior
        dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>% 
        mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
        mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S", CALIFICACION)) %>% 
        select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus)
    }else{
      df1 <- df2
    }
    infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',myrds[k],'.rds'))
    infoClean <- infoCheck %>% 
      dplyr::filter(REGULADO=="SBEF") %>% 
      dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
      mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
      mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
      group_by(CI,esBSO) %>% 
      mutate(peorCalif = ifelse(CALIFICACION==max(CALIFICACION),1,0)) %>% 
      ungroup() %>% 
      group_by(CI) %>% 
      dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
      mutate(califPeorSF = ifelse(peorCalif==1 & esBSO==0,CALIFICACION,'_')) %>% 
      mutate(califPeorBSO = ifelse(peorCalif==1 & esBSO==1,CALIFICACION,'_')) %>% 
      mutate(across(califPeorSF:califPeorBSO, ~ max(.x,na.rm=T))) %>% #Para repetir los valores en cada fila
      ungroup() %>% 
      dplyr::filter(califPeorSF!="_" & SIGLA=="BSO") %>% 
      select(CTACLIENTE, OPERACION, califPeorSF)
    
    df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[k], '.rds')) %>%  #se abre mes posterior 
      dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>%
      mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
      mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S",CALIFICACION)) %>% 
      select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus) %>% 
      left_join(infoClean, by=c("CTACLIENTE","OPERACION"), suffix = c("_BSO","_SF")) %>% 
      mutate(CALIFICACION = ifelse(!is.na(califPeorSF) & califPeorSF>CALIFICACION, 
                                       califPeorSF, CALIFICACION)) %>% 
      select(-califPeorSF)
    
    dfCancel <- df1 %>% 
      anti_join(df2,by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(CALIFICACION = "Z") %>% 
      mutate(saldous = 0) %>% 
      mutate(previus = 0) %>% 
      mutate(monDate = monDate+1/12) %>% 
      bind_rows(df2)
    
    dfTrans <- df1 %>% 
      left_join(dfCancel, by=c("CTACLIENTE","OPERACION"), suffix = c("_ini","_fin")) %>% 
      mutate(trans = paste(CALIFICACION_ini, CALIFICACION_fin,sep="-")) %>% 
      mutate(trans_wSF = paste(CALIFICACION_ini, CALIFICACION_fin,sep="-")) %>% 
      mutate(difPrev = previus_fin - previus_ini) %>% 
      mutate(difSaldo = saldous_fin - saldous_ini) %>% 
      mutate(one = 1) %>% 
      select(monDate = monDate_ini, trans, cm1 = CALIFICACION_ini, cmt = CALIFICACION_fin, 
             saldous_ini, saldous = saldous_fin, one, previus_ini, previus_fin, difPrev, 
             difSaldo) %>% 
      group_by(monDate, trans, cm1, cmt) %>% 
      summarise_all(sum,na.rm=T) %>% 
      ungroup() %>% 
      group_by(monDate, cm1) %>% 
      mutate(prob = one/sum(one)) %>% 
      mutate(probS = saldous/sum(saldous))
    
    dfList[[i]] <- dfTrans
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

dfFull <- rbindlist(dfList)
dfFull <- dfFull %>% 
  dplyr::filter(monDate<"Jul. 2023")

saveRDS(dfFull, "D:/!bso/previsiones/transSF_Ene2022May2023.rds")

####____FACTORES DE STRESS EN SF____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
dfList <- list()
lag <- 4
i <- 1
for(i in 1:(length(myrds)-lag)) {
  tryCatch({
    print(myrds[i])
    k <- i + lag
    print(myrds[i+lag])
    
    df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i], '.rds')) %>%  #se abre mes anterior
      dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>% 
      mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
      mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S", CALIFICACION)) %>% 
      select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus)
    
    infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',myrds[k],'.rds'))
    infoClean <- infoCheck %>% 
      dplyr::filter(REGULADO=="SBEF") %>% 
      dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
      mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
      mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
      group_by(CI,esBSO) %>% 
      mutate(peorCalif = ifelse(CALIFICACION==max(CALIFICACION),1,0)) %>% 
      ungroup() %>% 
      group_by(CI) %>% 
      dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
      mutate(califPeorSF = ifelse(peorCalif==1 & esBSO==0,CALIFICACION,'_')) %>% 
      mutate(califPeorBSO = ifelse(peorCalif==1 & esBSO==1,CALIFICACION,'_')) %>% 
      mutate(across(califPeorSF:califPeorBSO, ~ max(.x,na.rm=T))) %>% #Para repetir los valores en cada fila
      ungroup() %>% 
      dplyr::filter(califPeorSF!="_" & SIGLA=="BSO") %>% 
      select(CTACLIENTE, OPERACION, califPeorSF)
    
    df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[k], '.rds')) %>%  #se abre mes posterior 
      dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>%
      mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
      mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S",CALIFICACION)) %>% 
      select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus) %>% 
      left_join(infoClean, by=c("CTACLIENTE","OPERACION"), suffix = c("_BSO","_SF")) %>% 
      mutate(CALIFICACION_wSF = ifelse(!is.na(califPeorSF) & califPeorSF>CALIFICACION, 
                                   califPeorSF, CALIFICACION)) %>% 
      select(-califPeorSF)
    
    dfCancel <- df1 %>% 
      anti_join(df2,by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(CALIFICACION = "Z") %>% 
      mutate(saldous = 0) %>% 
      mutate(previus = 0) %>% 
      mutate(monDate = monDate+1/12) %>% 
      bind_rows(df2)
    
    dfTrans <- df1 %>% 
      left_join(dfCancel, by=c("CTACLIENTE","OPERACION"), suffix = c("_ini","_fin")) %>% 
      mutate(trans = paste(CALIFICACION_ini, CALIFICACION_fin,sep="-")) %>% 
      mutate(trans_wSF = paste(CALIFICACION_ini, CALIFICACION_fin,sep="-")) %>% 
      mutate(difPrev = previus_fin - previus_ini) %>% 
      mutate(difSaldo = saldous_fin - saldous_ini) %>% 
      mutate(one = 1) %>% 
      select(monDate = monDate_ini, trans, cm1 = CALIFICACION_ini, cmt = CALIFICACION_fin, 
             saldous_ini, saldous = saldous_fin, one, previus_ini, previus_fin, difPrev, 
             difSaldo) %>% 
      group_by(monDate, trans, cm1, cmt) %>% 
      summarise_all(sum,na.rm=T) %>% 
      ungroup() %>% 
      group_by(monDate, cm1) %>% 
      mutate(prob = one/sum(one)) %>% 
      mutate(probS = saldous/sum(saldous))
    
    dfList[[i]] <- dfTrans
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

dfFull <- rbindlist(dfList)  
dfFull <- dfFull %>% 
  dplyr::filter(monDate<"Jul. 2023")

saveRDS(dfFull, "D:/!bso/previsiones/transSF_Ene2018Jun2023.rds")

####____COMPOSICION PORCENTUAL DE SAPOS DESDE 2019____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2018,2019,2020,2021,2022,2023)

ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Ago23.rds")
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Ago2023.rds')
cond_clean <- condFull %>% 
  select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int, CondCap_USD = Cond_Cap) %>%
  mutate(myCond = as.yearmon(Fecha)) %>% 
  group_by(myCond, Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(Fecha),
            across(starts_with("Cond"), ~sum(.x))) %>% 
  ungroup()

mylong <- as.vector(sapply(year,function(x){paste0(mes,". ",x)}))
mylong <- mylong[-c(1:which(mylong=="Dic. 2019"))]
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
myrds <- myrds[-c(1:which(myrds=="Dic2019"))]
SaposList <- list()
i <- 1
for(i in 1:(length(myrds))) {
  tryCatch({
    print(myrds[i])
    
    pt_grouped <- ptFull %>% 
      dplyr::filter(myPago>(as.yearmon(mylong[i])-1) & myPago<=mylong[i]) %>% 
      group_by(Operacion) %>% 
      mutate(Ult12MesesPagoTardio = n()) %>% 
      ungroup() %>% 
      mutate(Cierre = as.yearmon(mylong[i])) %>% 
      group_by(Cierre, Cuenta, Operacion) %>% 
      summarise(CantPT12Meses = max(Ult12MesesPagoTardio)) %>% 
      ungroup()
    cond_grouped <- cond_clean %>% 
      dplyr::filter(myCond>(as.yearmon(mylong[i])-1) & myCond<=mylong[i]) %>% 
      mutate(Cierre = as.yearmon(mylong[i])) %>% 
      group_by(Cierre, Cuenta, Operacion) %>% 
      summarise(CantCond12Meses = n()) %>% 
      ungroup()
    
    bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i], '.rds')) %>% 
      mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
      mutate(CALIFICACION = ifelse(ctaCont == "865","S", CALIFICACION)) %>% 
      select(monDate, CTACLIENTE, OPERACION, tipoCred, CALIFICACION, ctaCont, 
             saldous, previus) %>% 
      mutate(one=1) %>% 
      left_join(pt_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
      left_join(cond_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
      mutate(NXN = case_when(CantPT12Meses>=12 & CantCond12Meses>=12 ~ '12x12',
                             CantPT12Meses>=11 & CantCond12Meses>=11 ~ '11x11',
                             CantPT12Meses>=10 & CantCond12Meses>=10 ~ '10x10',
                             CantPT12Meses>=9 & CantCond12Meses>=9 ~ '9x9',
                             CantPT12Meses>=8 & CantCond12Meses>=8 ~ '8x8',
                             CantPT12Meses>=7 & CantCond12Meses>=7 ~ '7x7',
                             CantPT12Meses>=6 & CantCond12Meses>=6 ~ '6x6',
                             CantPT12Meses>=5 & CantCond12Meses>=5 ~ '5x5',
                             CantPT12Meses>=4 & CantCond12Meses>=4 ~ '4x4',
                             CantPT12Meses>=3 & CantCond12Meses>=3 ~ '3x3',
                             CantPT12Meses>=2 & CantCond12Meses>=2 ~ '2x2',
                             CantPT12Meses>=1 & CantCond12Meses>=1 ~ '1x1',)) %>% 
      select(-CTACLIENTE, -OPERACION, -starts_with("Cierre"), -starts_with("Cant")) %>% 
      group_by(monDate, CALIFICACION, tipoCred, ctaCont, NXN) %>% 
      summarise_all(sum, na.rm=T) %>% 
      ungroup() 
      
    SaposList[[i]] <- bdc
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

SaposFull <- rbindlist(SaposList)  

saveRDS(SaposFull, "D:/!bso/previsiones/CompSapos_Ene2020Ago2023.rds")
####____COMPOSICION POR CALIFICACION____####
Cuadre <- SaposFull %>% 
  dplyr::filter(!ctaCont %in% c('623','865')) %>% 
  dplyr::filter(!NXN %in% c('1x1','2x2','3x3'), !is.na(NXN)) %>% 
  group_by(monDate) %>% 
  summarise(S=sum(saldous),
            N=sum(one)) %>% 
  ungroup()
  
comp <- SaposFull %>% 
  ungroup() %>% 
  select(CALIFICACION, NXN, saldous, previus, one) %>% 
  group_by(CALIFICACION, NXN) %>% 
  summarise(one=sum(one), sum(saldous)) %>% 
  ungroup() %>% 
  group_by(CALIFICACION) %>% 
  mutate(Prob = one/sum(one)*100) %>% 
  ungroup() %>% 
  select(CALIFICACION, NXN, Prob) %>% 
  pivot_wider(names_from = NXN, values_from = Prob, values_fill = 0) %>% 
  adorn_totals("col")
####____CREATING PROJECTED MATRICES____####
tm <- read.csv('D:/!bso/transMat/Oreports/tmAll_Ago23.csv') %>% 
  mutate(monDate=as.yearmon(monDate)+1/12) %>% 
  # rename(prob=probN) %>% #For older versions e.g. Dic22
  dplyr::filter(monDate >= 'Ene. 2022' & monDate <= 'May. 2023') %>% 
  ungroup()

trans <- c("AA","AB","BA","BB") #Run this without filters for monDate
for (t  in trans) {
  gph <- tm %>% 
    dplyr::filter(trans==t) %>% 
    ggplot(aes(x=monDate,y=prob))+
    geom_line(color="red",size=1.5)+
    labs(x="Fecha",y="Probabilidad (%)",title=paste0("Transición ",t))+
    theme_light()
  ggsave(paste0('D:/!bso/transMat/prevMar23/EvolTrans_',t,'.png'),gph,
         width = 9,height = 6,units = "in")
  print(gph)
}
workdir <- "D:/!bso/previsiones/prev_Ago2023/"
dir.create(workdir)
####____AVERAGE MATRIX____####
gph <- tm %>% 
  select(monDate, cm1, cmt, prob) %>% 
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  arrange(monDate, cm1) %>% 
  select(-monDate) %>% 
  group_by(cm1) %>%
  summarise_all(mean) %>% #Medida resumen para matriz de transición acumulada
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  relocate(A,B,C,D,E,`F`,S,Z) %>% 
  add_row(A = 0, B=0, C=0, D=0, E=0, F=0, S=0, Z=1, cm1="Z") %>% 
  column_to_rownames("cm1")
rowSums(gph)

M <- data.matrix(gph)
cm1v <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')
M
TimeSteps <- c(4,16)
for (t in TimeSteps) {
  z <- as_tibble(M %^% t) %>% 
    bind_cols(cm1=cm1v) %>% 
    pivot_longer(!cm1) %>% 
    dplyr::rename(prob = value,
                  cmt = name) %>% 
    group_by(cm1) %>% 
    mutate(cumProb = cumsum(prob)) %>% 
    mutate(prob = prob*100,
           cumProb = round(cumProb*100,6)) %>% 
    ungroup() %>% 
    select(-prob) %>% 
    pivot_wider(names_from = cmt, values_from = cumProb, values_fill = 0) %>%
    column_to_rownames("cm1") %>%
    relocate(A,B,C,D,E,`F`,S,Z)
  write.csv(z, paste0(workdir,"tmCum_avg",t,".csv"), row.names = F, quote = F)
  M_t <- as_tibble(M %^% t)
  M_t
  Matrices <- list(M=M, M_t=M_t)
  write.xlsx(Matrices,file = paste0(workdir,"M_Mt_avg",t,".xlsx"))
}
####____AVERAGE MATRIX CON IM EN SF____####
gph <- tm %>% 
  select(monDate, cm1, cmt, prob) %>% 
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  arrange(monDate, cm1) %>% 
  select(-monDate) %>% 
  group_by(cm1) %>%
  summarise_all(mean) %>% #Medida resumen para matriz de transición acumulada
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  relocate(A,B,C,D,E,`F`,S,Z) %>% 
  add_row(A = 0, B=0, C=0, D=0, E=0, F=0, S=0, Z=1, cm1="Z") %>% 
  column_to_rownames("cm1")
rowSums(gph)

# stressSF <- function(add){
#   stf <- matrix(data=0, nrow=8, ncol=8)
#   for (i in 1:1) {
#     if (add<0) {
#       stf[i,i] <- add
#       stf[i,8] <- -add
#     }else{
#       stf[i,i+1] <- add
#       stf[i,i] <- -add
#     }
#   }
#   stf <- data.matrix(stf)
#   colnames(stf) <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')
#   rownames(stf) <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')
#   return(stf)
# }

M <- data.matrix(gph)
cm1v <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')
M

stressIM <- c(0.03722, 0.12155, 0.13770, 
              0.24340, 0.53407, 0.63916)
stressN <- c("MeanIM4","Sup90IM4","Sup95IM4",
             "MeanIM16","Sup90IM16","Sup95IM16")

for (i in 1:3) {
  ####HIDE####
  # z <- M %>% 
  #   bind_cols(cm1=cm1v) %>% 
  #   pivot_longer(!cm1) %>% 
  #   dplyr::rename(prob = value,
  #                 cmt = name) %>% 
  #   mutate(prob = case_when(cm1=="A" & add >0 & cmt %in% c("B","C","D","E","F")~prob*(1+stressIM[i]),
  #                           cm1=="A" & add <0 & cmt %in% c("S","Z")~prob*(1-stressIM[i]),
  #                           TRUE~prob)) %>% 
  #   group_by(cm1) %>% 
  #   mutate(prob = prob/sum(prob)) %>% 
  #   ungroup() %>%
  #   pivot_wider(names_from = cmt,values_from = prob,values_fill = 0) %>% 
  #   column_to_rownames("cm1")
  # 
  # z <- as.tibble(data.matrix(z) %^% 16)
  ####CONTINUE####
  z <- as_tibble(M %^% 4) %>% 
    bind_cols(cm1=cm1v) %>% 
    pivot_longer(!cm1) %>% 
    dplyr::rename(prob = value,
                  cmt = name) %>% 
    mutate(prob = case_when(cm1=="A" & stressIM[i] >0 & cmt %in% c("B","C","D","E","F")~prob*(1+stressIM[i]),
                            cm1=="A" & stressIM[i] <0 & cmt %in% c("S","Z")~prob*(1-stressIM[i]),
                            TRUE~prob)) %>% 
    group_by(cm1) %>% 
    mutate(prob = prob/sum(prob)) %>% 
    ungroup()
  M_t <- z %>% 
    pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>% 
    column_to_rownames("cm1") %>%
    as.tibble(.)
  z <- z %>% 
    group_by(cm1) %>% 
    mutate(cumProb = cumsum(prob)) %>% 
    mutate(prob = prob*100,
           cumProb = round(cumProb*100,6)) %>% 
    ungroup() %>% 
    select(-prob) %>% 
    pivot_wider(names_from = cmt, values_from = cumProb, values_fill = 0) %>%
    column_to_rownames("cm1") %>%
    relocate(A,B,C,D,E,`F`,S,Z)
  write.csv(z, paste0(workdir,"tmCum_",stressN[i],".csv"), row.names = F, quote = F)

  Matrices <- list(M=M, M_t=M_t)
  write.xlsx(Matrices,file = paste0(workdir,"M_Mt_",stressN[i],".xlsx"))
}
for (i in 4:6) {
  z <- as_tibble(M %^% 12) %>% 
    bind_cols(cm1=cm1v) %>% 
    pivot_longer(!cm1) %>% 
    dplyr::rename(prob = value,
                  cmt = name) %>% 
    mutate(prob = case_when(cm1=="A" & stressIM[i] >0 & cmt %in% c("B","C","D","E","F")~prob*(1+stressIM[i]),
                            cm1=="A" & stressIM[i] <0 & cmt %in% c("S","Z")~prob*(1-stressIM[i]),
                            TRUE~prob)) %>% 
    group_by(cm1) %>% 
    mutate(prob = prob/sum(prob)) %>% 
    ungroup()
  M_t <- z %>% 
    pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>% 
    column_to_rownames("cm1") %>%
    as.tibble(.)
  z <- z %>% 
    group_by(cm1) %>% 
    mutate(cumProb = cumsum(prob)) %>% 
    mutate(prob = prob*100,
           cumProb = round(cumProb*100,6)) %>% 
    ungroup() %>% 
    select(-prob) %>% 
    pivot_wider(names_from = cmt, values_from = cumProb, values_fill = 0) %>%
    column_to_rownames("cm1") %>%
    relocate(A,B,C,D,E,`F`,S,Z)
  write.csv(z, paste0(workdir,"tmCum_",stressN[i],".csv"), row.names = F, quote = F)
  
  Matrices <- list(M=M, M_t=M_t)
  write.xlsx(Matrices,file = paste0(workdir,"M_Mt_",stressN[i],".xlsx"))
}
####____AJUSTE DE TRANSICION POR SAPOS____####
sapos <- readRDS("D:/!bso/previsiones/CompSapos_Ene2020Ago2023.rds") %>% 
  dplyr::filter(monDate>="Ene. 2022") %>% 
  replace_na(list(NXN="0x0")) %>% 
  dplyr::filter(CALIFICACION!="") %>% 
  ungroup()

stress_factors <- sapos %>% 
  select(monDate, CALIFICACION, NXN, saldous, previus, one) %>% 
  group_by(monDate, CALIFICACION, NXN) %>% 
  summarise(one=sum(one)) %>% 
  ungroup() %>% 
  group_by(monDate, CALIFICACION) %>% #Composicion por cada mes
  mutate(Prob = one/sum(one)*100) %>% 
  ungroup() %>% 
  select(monDate,Calif_ini = CALIFICACION, NXN, Prob) %>% #Composición promedio
  pivot_wider(names_from = NXN, names_prefix = "A",values_from = Prob, values_fill = 0) %>% 
  select(-monDate) %>% 
  group_by(Calif_ini) %>% 
  summarise_all(mean, na.rm=T) %>% 
  ungroup() %>% 
  pivot_longer(cols = starts_with("A"),names_to = "NXN",values_to = "Prob") %>% 
  mutate(Calif_fin = case_when(NXN %in% c('A0x0','A1x1','A2x2','A3x3')~'A',
                               NXN %in% c('A4x4','A5x5')~'B',
                               NXN %in% c('A6x6','A7x7')~'C',
                               NXN %in% c('A8x8','A9x9')~'D',
                               NXN %in% c('A10x10')~'E',
                               NXN %in% c('A11x11','A12x12')~'F',)) %>% 
  mutate(Calif_fin = ifelse(Calif_ini>Calif_fin, Calif_ini, Calif_fin)) %>% 
  mutate(Prob_fin = ifelse(Calif_fin>Calif_ini, Prob, 0)) %>% 
  select(Prob_fin, Calif_ini, Calif_fin) %>% 
  group_by(Calif_ini, Calif_fin) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Calif_fin, values_from = Prob_fin, values_fill = 0)

comp <- stress_factors %>% 
  mutate(A = ifelse(Calif_ini=='A',-B-C-D-E-`F`-S, A)) %>% 
  mutate(B = ifelse(Calif_ini=='B',-A-C-D-E-`F`-S, B)) %>% 
  mutate(C = ifelse(Calif_ini=='C',-A-B-D-E-`F`-S, C)) %>% 
  mutate(D = ifelse(Calif_ini=='D',-A-B-C-E-`F`-S, D)) %>% 
  mutate(E = ifelse(Calif_ini=='E',-A-B-C-D-`F`-S, E)) %>% 
  mutate(`F` = ifelse(Calif_ini=='F',-A-B-C-D-E-S, `F`)) %>% 
  mutate(S = ifelse(Calif_ini=='S',-A-B-C-D-E-`F`, S)) %>% 
  add_row(Calif_ini="Z",A=0,B=0,C=0,D=0,E=0,`F`=0,S=0) %>% 
  add_column(Z=rep(0,8))

MS <- data.matrix(gph)+ data.matrix(select(comp,-Calif_ini))/100
cm1v <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')
MS
TimeSteps <- c(4,16)
for (t in TimeSteps) {
  z <- as_tibble(MS %^% t) %>% 
    bind_cols(cm1=cm1v) %>% 
    pivot_longer(!cm1) %>% 
    dplyr::rename(prob = value,
                  cmt = name) %>% 
    group_by(cm1) %>% 
    mutate(cumProb = cumsum(prob)) %>% 
    mutate(prob = prob*100,
           cumProb = round(cumProb*100,6)) %>% 
    ungroup() %>% 
    select(-prob) %>% 
    pivot_wider(names_from = cmt, values_from = cumProb, values_fill = 0) %>%
    column_to_rownames("cm1") %>%
    relocate(A,B,C,D,E,`F`,S,Z)
  write.csv(z, paste0(workdir,"tmCum_BC_avg",t,".csv"),row.names = F, quote = F)
  
  M_t <- as_tibble(MS %^% t)
  M_t
  Matrices <- list(M=M, M_t=M_t, Comp=comp)
  write.xlsx(Matrices,file = paste0(workdir,"M_Mt_BC_avg",t,".xlsx"))
}

####____AVERAGE MATRIX CON CONTAGIO BC A UN MES____####
tm <- readRDS('D:/!bso/previsiones/transBC3_Ene2022May2023.rds') %>% 
  mutate(monDate = monDate+1/12) %>% 
  dplyr::filter(monDate >= 'Ene. 2022' & monDate <= 'May. 2023') %>% 
  ungroup()
gph <- tm %>% 
  select(monDate, cm1, cmt, prob) %>% 
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  arrange(monDate, cm1) %>% 
  select(-monDate) %>% 
  group_by(cm1) %>%
  summarise_all(mean) %>% #Medida resumen para matriz de transición acumulada
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  relocate(A,B,C,D,E,`F`,S,Z) %>% 
  add_row(A = 0, B=0, C=0, D=0, E=0, F=0, S=0, Z=1, cm1="Z") %>% 
  column_to_rownames("cm1")
rowSums(gph)

M <- data.matrix(gph)
cm1v <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')
M
TimeSteps <- c(4,16)
for (t in TimeSteps) {
  z <- as_tibble(M %^% t) %>% 
    bind_cols(cm1=cm1v) %>% 
    pivot_longer(!cm1) %>% 
    dplyr::rename(prob = value,
                  cmt = name) %>% 
    group_by(cm1) %>% 
    mutate(cumProb = cumsum(prob)) %>% 
    mutate(prob = prob*100,
           cumProb = round(cumProb*100,6)) %>% 
    ungroup() %>% 
    select(-prob) %>% 
    pivot_wider(names_from = cmt, values_from = cumProb, values_fill = 0) %>%
    column_to_rownames("cm1") %>%
    relocate(A,B,C,D,E,`F`,S,Z)
  write.csv(z, paste0(workdir,"tmCum_BC3_avg",t,".csv"),row.names = F, quote = F)
  
  M_t <- as_tibble(M %^% t)
  M_t
  Matrices <- list(M=M, M_t=M_t)
  write.xlsx(Matrices,file = paste0(workdir,"M_Mt_BC3_avg",t,".xlsx"))
}
####____AVERAGE MATRIX CON CONTAGIO SF A UN MES____####
tm <- readRDS('D:/!bso/previsiones/transSF_Ene2022May2023.rds') %>% 
  mutate(monDate = monDate+1/12) %>% 
  dplyr::filter(monDate >= 'Ene. 2022' & monDate <= 'May. 2023') %>% 
  ungroup()
gph <- tm %>% 
  select(monDate, cm1, cmt, prob) %>% 
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  arrange(monDate, cm1) %>% 
  select(-monDate) %>% 
  group_by(cm1) %>%
  summarise_all(mean) %>% #Medida resumen para matriz de transición acumulada
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  relocate(A,B,C,D,E,`F`,S,Z) %>% 
  add_row(A = 0, B=0, C=0, D=0, E=0, F=0, S=0, Z=1, cm1="Z") %>% 
  column_to_rownames("cm1")
rowSums(gph)

M <- data.matrix(gph)
cm1v <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')
M
TimeSteps <- c(4,16)
for (t in TimeSteps) {
  z <- as_tibble(M %^% t) %>% 
    bind_cols(cm1=cm1v) %>% 
    pivot_longer(!cm1) %>% 
    dplyr::rename(prob = value,
                  cmt = name) %>% 
    group_by(cm1) %>% 
    mutate(cumProb = cumsum(prob)) %>% 
    mutate(prob = prob*100,
           cumProb = round(cumProb*100,6)) %>% 
    ungroup() %>% 
    select(-prob) %>% 
    pivot_wider(names_from = cmt, values_from = cumProb, values_fill = 0) %>%
    column_to_rownames("cm1") %>%
    relocate(A,B,C,D,E,`F`,S,Z)
  write.csv(z, paste0(workdir,"tmCum_SF2_avg",t,".csv"),row.names = F, quote = F)
  
  M_t <- as_tibble(M %^% t)
  M_t
  Matrices <- list(M=M, M_t=M_t)
  write.xlsx(Matrices,file = paste0(workdir,"M_Mt_SF_avg",t,".xlsx"))
}

####____TRANSICIONES CON SAPOS MANTENIENDO PEOR CALIF____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2021,2022,2023)

ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Jul23.rds")
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Jul2023.rds')
cond_clean <- condFull %>% 
  select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int, CondCap_USD = Cond_Cap) %>%
  mutate(myCond = as.yearmon(Fecha)) %>% 
  group_by(myCond, Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(Fecha),
            across(starts_with("Cond"), ~sum(.x))) %>% 
  ungroup()

mylong <- as.vector(sapply(year,function(x){paste0(mes,". ",x)}))
mylong <- mylong[-c(1:which(mylong=="Nov. 2021"))]
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
myrds <- myrds[-c(1:which(myrds=="Nov2021"))]
dfList <- list()
lag <- 1
i <- 1
for(i in 1:(length(myrds)-lag)) {
  tryCatch({
    print(myrds[i])
    k <- i + lag
    print(myrds[i+lag])
    
    if(i==1){
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i], '.rds')) %>%  #se abre mes anterior
        dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>% 
        mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
        mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S", CALIFICACION)) %>% 
        select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus)
    }else{
      df1 <- df2
    }
    
    pt_grouped <- ptFull %>% 
      dplyr::filter(myPago>(as.yearmon(mylong[i])-1) & myPago<=mylong[i]) %>% 
      group_by(Operacion) %>% 
      mutate(Ult12MesesPagoTardio = n()) %>% 
      ungroup() %>% 
      mutate(Cierre = as.yearmon(mylong[i])) %>% 
      group_by(Cierre, Cuenta, Operacion) %>% 
      summarise(CantPT12Meses = max(Ult12MesesPagoTardio)) %>% 
      ungroup()
    cond_grouped <- cond_clean %>% 
      dplyr::filter(myCond>(as.yearmon(mylong[i])-1) & myCond<=mylong[i]) %>% 
      mutate(Cierre = as.yearmon(mylong[i])) %>% 
      group_by(Cierre, Cuenta, Operacion) %>% 
      summarise(CantCond12Meses = n()) %>% 
      ungroup()
    
    df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[k], '.rds')) %>%  #se abre mes posterior 
      dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>%
      mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
      mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S",CALIFICACION)) %>% 
      select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus) %>% 
      left_join(pt_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
      left_join(cond_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
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
      mutate(CsAj = CALIFICACION) %>% 
      mutate(CalifSapo = case_when(NXN %in% c('06x06','07x07','08x08','09x09','10x10','11x11','12x12') ~ 'F',
                                   NXN %in% c('05x05') ~ 'E',
                                   NXN %in% c('04x04') ~ 'D',
                                   NXN %in% c('03x03') ~ 'C',
                                   NXN %in% c('01x01','02x02') ~ 'B',
                                   TRUE~'_')) %>% 
      mutate(CALIFICACION = ifelse(!is.na(CalifSapo) & CalifSapo>CALIFICACION, 
                                   CalifSapo, CALIFICACION)) %>% 
      select(-starts_with("Cant"),-NXN,-starts_with("Cierre"), -CalifSapo)
    
    dfCancel <- df1 %>% 
      anti_join(df2,by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(CALIFICACION = "Z") %>% 
      mutate(saldous = 0) %>% 
      mutate(previus = 0) %>% 
      mutate(monDate = monDate+1/12) %>% 
      bind_rows(df2)
    
    dfTrans <- df1 %>% 
      left_join(dfCancel, by=c("CTACLIENTE","OPERACION"), suffix = c("_ini","_fin")) %>% 
      mutate(trans = paste(CALIFICACION_ini, CALIFICACION_fin,sep="-")) %>% 
      mutate(difPrev = previus_fin - previus_ini) %>% 
      mutate(difSaldo = saldous_fin - saldous_ini) %>% 
      mutate(one = 1) %>% 
      select(monDate = monDate_ini, trans, cm1 = CALIFICACION_ini, cmt = CALIFICACION_fin, 
             saldous_ini, saldous = saldous_fin, one, previus_ini, previus_fin, difPrev, 
             difSaldo) %>% 
      group_by(monDate, trans, cm1, cmt) %>% 
      summarise_all(sum,na.rm=T) %>% 
      ungroup() %>% 
      group_by(monDate, cm1) %>% 
      mutate(prob = one/sum(one)) %>% 
      mutate(probS = saldous/sum(saldous)) %>% 
      ungroup()
    
    dfList[[i]] <- dfTrans
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

dfFull <- rbindlist(dfList)  

dfFull <- dfFull %>% 
  dplyr::filter(monDate<="Jul. 2023")

saveRDS(dfFull, "D:/!bso/previsiones/transBC_Ene2022May2023.rds")


####____TRANSICIONES CON SAPOS SIN MANTENER PEOR CALIF____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2021,2022,2023)

ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Jul23.rds")
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Jul2023.rds')
cond_clean <- condFull %>% 
  select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int, CondCap_USD = Cond_Cap) %>%
  mutate(myCond = as.yearmon(Fecha)) %>% 
  group_by(myCond, Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(Fecha),
            across(starts_with("Cond"), ~sum(.x))) %>% 
  ungroup()

mylong <- as.vector(sapply(year,function(x){paste0(mes,". ",x)}))
mylong <- mylong[-c(1:which(mylong=="Nov. 2021"))]
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
myrds <- myrds[-c(1:which(myrds=="Nov2021"))]
dfList <- list()
lag <- 1
i <- 1
for(i in 1:(length(myrds)-lag)) {
  tryCatch({
    print(myrds[i])
    k <- i + lag
    print(myrds[i+lag])
    
    df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i], '.rds')) %>%  #se abre mes anterior
      dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>% 
      mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
      mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S", CALIFICACION)) %>% 
      select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus)
    
    pt_grouped <- ptFull %>% 
      dplyr::filter(myPago>(as.yearmon(mylong[i])-1) & myPago<=mylong[i]) %>% 
      group_by(Operacion) %>% 
      mutate(Ult12MesesPagoTardio = n()) %>% 
      ungroup() %>% 
      mutate(Cierre = as.yearmon(mylong[i])) %>% 
      group_by(Cierre, Cuenta, Operacion) %>% 
      summarise(CantPT12Meses = max(Ult12MesesPagoTardio)) %>% 
      ungroup()
    cond_grouped <- cond_clean %>% 
      dplyr::filter(myCond>(as.yearmon(mylong[i])-1) & myCond<=mylong[i]) %>% 
      mutate(Cierre = as.yearmon(mylong[i])) %>% 
      group_by(Cierre, Cuenta, Operacion) %>% 
      summarise(CantCond12Meses = n()) %>% 
      ungroup()
    
    df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[k], '.rds')) %>%  #se abre mes posterior 
      dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>%
      mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
      mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S",CALIFICACION)) %>% 
      select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus) %>% 
      left_join(pt_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
      left_join(cond_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
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
      # mutate(CsAj = CALIFICACION) %>% 
      mutate(CalifSapo = case_when(NXN %in% c('09x09','10x10','11x11','12x12') ~ 'F',
                                   NXN %in% c('08x08') ~ 'E',
                                   NXN %in% c('07x07') ~ 'D',
                                   NXN %in% c('06x06') ~ 'C',
                                   NXN %in% c('05x05') ~ 'B',
                                   NXN %in% c('01x01','02x02','03x03','04x04') ~ 'A', TRUE~'_')) %>% 
      mutate(CALIFICACION = ifelse(!is.na(CalifSapo) & CalifSapo>CALIFICACION, 
                                   CalifSapo, CALIFICACION)) %>% 
      select(-starts_with("Cant"),-NXN,-starts_with("Cierre"), -CalifSapo)
    
    dfCancel <- df1 %>% 
      anti_join(df2,by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(CALIFICACION = "Z") %>% 
      mutate(saldous = 0) %>% 
      mutate(previus = 0) %>% 
      mutate(monDate = monDate+1/12) %>% 
      bind_rows(df2)
    
    dfTrans <- df1 %>% 
      left_join(dfCancel, by=c("CTACLIENTE","OPERACION"), suffix = c("_ini","_fin")) %>% 
      mutate(trans = paste(CALIFICACION_ini, CALIFICACION_fin,sep="-")) %>% 
      mutate(difPrev = previus_fin - previus_ini) %>% 
      mutate(difSaldo = saldous_fin - saldous_ini) %>% 
      mutate(one = 1) %>% 
      select(monDate = monDate_ini, trans, cm1 = CALIFICACION_ini, cmt = CALIFICACION_fin, 
             saldous_ini, saldous = saldous_fin, one, previus_ini, previus_fin, difPrev, 
             difSaldo) %>% 
      group_by(monDate, trans, cm1, cmt) %>% 
      summarise_all(sum,na.rm=T) %>% 
      ungroup() %>% 
      group_by(monDate, cm1) %>% 
      mutate(prob = one/sum(one)) %>% 
      mutate(probS = saldous/sum(saldous)) %>% 
      ungroup()
    
    dfList[[i]] <- dfTrans
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

dfFull <- rbindlist(dfList)  

dfFull <- dfFull %>% 
  dplyr::filter(monDate<="Jul. 2023")

saveRDS(dfFull, "D:/!bso/previsiones/transBC3_Ene2022May2023.rds")
####____SHIT DE JM____####
cic <- readRDS("D:/!bso/CIC/rds/cic_Dic2022.rds")
cicIndicators <- cic %>% 
  mutate(par0=ifelse(DiasMora>0,SaldoBruto,0)) %>% 
  mutate(PrevEspMora = ifelse(CuentaContable.y %in% c('133','134','136','137'),PrevEspecifica,0)) %>% 
  mutate(PrevEspVig = ifelse(CuentaContable.y %in% c('131','135'),PrevEspecifica,0)) %>% 
  select(Genero, SaldoBruto, par0, SaldoCastigado,PrevEspecifica,PrevEspMora,PrevEspVig,PrevCiclica) %>% 
  group_by(Genero) %>% 
  summarise_all(sum, na.rm=T) %>% 
  adorn_totals("row")

cic_old <- readRDS("D:/!bso/CIC/rds/cic_Dic2021.rds")
cicIndicators_old <- cic_old %>% 
  select(Genero, SaldoCastigado) %>% 
  group_by(Genero) %>% 
  summarise_all(sum, na.rm=T) %>% 
  adorn_totals("row") %>% 
  left_join(cicIndicators, by="Genero", suffix=c("21","22")) %>% 
  mutate(SaldoCastNeto=SaldoCastigado22-SaldoCastigado21) %>% 
  mutate(PEC_CBC = (PrevEspecifica+PrevCiclica)/(SaldoBruto+SaldoCastNeto)) %>% 
  mutate(SMPECC_CBC = (par0-PrevEspecifica-PrevCiclica+SaldoCastNeto)/(SaldoBruto+SaldoCastNeto))
write_xlsx(cicIndicators_old, "D:/!bso/requests/Ratios_Genero_Dic2022.xlsx")  
