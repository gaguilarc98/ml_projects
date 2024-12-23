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
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(janitor)
library(openxlsx)
require(XLConnect)

Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
cases <- function(quant,levs,values){
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- vector(mode = 'character',length = length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
####____DEFINICIONES____####
#Los créditos de vivienda se clasifican según los días de mora (DM)
#A si DM<=30, B si 31<=DM>=90, C si 91<=DM<=180, D si 181<=DM<=270, E si 271<=DM<=360, F si DM>360
#Los microcréditos se califican como sigue:
#A si DM<=5, B si 6<=DM<=30, C si 31<=DM<=55, D si 56<=DM<=75, E si 76<=DM<=90, F si DM>90
#Los microcréditos del sector agropecuario se califican como sigue:
#A si DM<=20, B si 21<=DM<=30, C si 31<=DM<=55, D si 56<=DM<=75, E si 76<=DM<=90, F si DM>90
####____CREATING BDCFULL____####
bdcList <- list()
file_list <- list.files(path='D:/!bso/girCartera/rdsGAR')
for (i in 1:length(file_list)) {
  
  print(file_list[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/',
                        file_list[i])) %>% 
    #select(-MODULO) %>% 
    #dplyr::filter(substr(AGENCIA, 1, 1) == '6') %>% 
    # mutate(RUBRO = as.character(RUBRO)) %>% 
    # mutate(CALIFICACION = as.character(CALIFICACION)) %>% 
    # mutate(SALDO = as.double(SALDO)) %>% 
    # mutate(CAEDEC_DEST = as.character(CAEDEC_DEST)) %>% 
    # mutate(fbase = substr(file_list[i], 4, 10)) %>% 
    select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, 
           CALIFICACION, fbase, montous, saldous, previus, saldoCast, tipoCred, SECTOR_CARTERA) %>% 
    mutate(CALIFICACION = ifelse(ESTADO == 'CASTIGADA', 'S', CALIFICACION))
  bdcList[[i]] <- bdc
}
gc()
bdcFull <- bind_rows(bdcList) %>% 
  mutate(mon = substr(fbase,1,3)) %>% 
  mutate(year = substr(fbase,4,7)) %>% 
  mutate(mes = cases(mon, c('Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dic'),
                     c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'))) %>% 
  mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>% 
  mutate(monDate = as.yearmon(dayDate)) %>% 
  select(-dayDate, -mon, -year, -mes) %>% 
  arrange(CI, CTACLIENTE, OPERACION, monDate) %>% 
  glimpse()
gc()
bdcList <- NULL
write_rds(bdcFull, 'D:/!bso/transMat/bdcFull.rds')
####____READING BDCFULL____####
bdcFull <- readRDS('D:/!bso/transMat/bdcFull.rds')

####____Lag 12 transition matrix____####
bdcTrans <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CALIFICACION, monDate) %>% 
  mutate(monDate = zoo::as.yearmon(monDate)) %>% 
  dplyr::filter(monDate >= 'mar. 2015') %>% 
  group_by(OPERACION, CTACLIENTE) %>%
  arrange(OPERACION, CTACLIENTE, monDate) %>%
  dplyr::rename(cmt = CALIFICACION) %>% 
  mutate(cm1 = dplyr::lag(cmt, 12)) %>% 
  dplyr::filter(!is.na(cm1)) %>% 
  ungroup() %>% 
  mutate(trans = ifelse(cm1 %in% c('A','B','C','D','E','F') &
                          cmt %in% c('A','B','C','D','E','F','S'),paste0(cm1,cmt),NA)) %>% 
  mutate(trans = ifelse(cm1=='S'&cmt=='S',paste0(cm1,cmt),trans)) %>% 
  ungroup() %>% 
  group_by(cm1, monDate) %>% 
  mutate(rowTot = n()) %>% 
  ungroup() %>% 
  group_by(OPERACION) %>% 
  arrange(OPERACION, monDate) %>% 
  mutate(det = ifelse(cm1 < cmt,1,0)) %>% 
  glimpse()

write.csv(bdcTrans, 'D:/!bso/transMat/bdcTrans_Lag12.csv')

bdcTrans <- fread('D:/!bso/transMat/bdcTrans_Lag12.csv')
bdcCancel <- readRDS('D:/!bso/transMat/matCancel2.rds') 
tm2 <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, trans) %>%
  summarise_all(sum) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(bdcCancel) %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% 
  group_by(monDate, cm1) %>% 
  mutate(prob = round(one/sum(one)*100,2)) %>% 
  arrange(cm1, monDate) %>% 
  mutate(Deterioro = case_when(cm1 == 'A' ~ sum(prob[!(cmt %in% c('A','Z'))]), 
                               cm1 == 'B' ~ sum(prob[!(cmt %in% c('A','B','Z'))]),
                               cm1 == 'C' ~ sum(prob[!(cmt %in% c('A','B','C','Z'))]),
                               cm1 == 'D' ~ sum(prob[!(cmt %in% c('A','B','C','D','Z'))]),
                               cm1 == 'E' ~ sum(prob[!(cmt %in% c('A','B','C','D','E','Z'))]),
                               cm1 == 'F' ~ sum(prob[!(cmt %in% c('A','B','C','D','E','F','Z'))]),
                               TRUE ~ 0)) %>% 
  mutate(Recuperacion = case_when(cm1 == 'A' ~ 0,
                                  cm1 == 'Z' ~ 0,
                                  cm1 == 'B' ~ sum(prob[cmt %in% c('A','Z')]),
                                  cm1 == 'C' ~ sum(prob[cmt %in% c('A','B','Z')]),
                                  cm1 == 'D' ~ sum(prob[cmt %in% c('A','B','C','Z')]),
                                  cm1 == 'E' ~ sum(prob[cmt %in% c('A','B','C','D','Z')]),
                                  cm1 == 'F' ~ sum(prob[cmt %in% c('A','B','C','D','E','Z')]),
                                  cm1 == 'S' ~ sum(prob[cmt == 'Z']),
                                  TRUE ~ 0)) %>% 
  mutate(Permanencia = case_when(cm1 == 'A' ~ sum(prob[cmt == 'A']), 
                                 cm1 == 'B' ~ sum(prob[cmt == 'B']),
                                 cm1 == 'C' ~ sum(prob[cmt == 'C']),
                                 cm1 == 'D' ~ sum(prob[cmt == 'D']),
                                 cm1 == 'E' ~ sum(prob[cmt == 'E']),
                                 cm1 == 'F' ~ sum(prob[cmt == 'F']),
                                 cm1 == 'S' ~ sum(prob[cmt == 'S']),
                                 cm1 == 'Z' ~ sum(prob[cmt == 'Z']),
                                 TRUE ~ 0)) %>% 
  glimpse()

####____Lag 1 ^12 TRANSITION MATRICES____####
bdcTrans <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CALIFICACION, monDate,DIASMORA) %>% 
  mutate(monDate = zoo::as.yearmon(monDate)) %>% 
  dplyr::filter(monDate >= 'mar. 2015') %>% 
  group_by(OPERACION, CTACLIENTE) %>%
  arrange(OPERACION, CTACLIENTE, monDate) %>%
  dplyr::rename(cmt = CALIFICACION) %>% 
  mutate(cm1 = dplyr::lag(cmt, 1)) %>% 
  mutate(det = ifelse(cm1 < cmt,1,0)) %>% 
  mutate(DiasMoraMes= DIASMORA - dplyr::lag(DIASMORA,1)) %>% 
  ungroup() %>% 
  mutate(trans = ifelse(cm1 %in% c('A','B','C','D','E','F') &
                          cmt %in% c('A','B','C','D','E','F','S'),paste0(cm1,cmt),NA)) %>% 
  mutate(trans = ifelse(cm1=='S'&cmt=='S',paste0(cm1,cmt),trans)) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  dplyr::filter(!is.na(DiasMoraMes) & !(DiasMoraMes>31)) %>% 
  ungroup() %>% 
  group_by(cm1, monDate) %>% 
  mutate(rowTot = n()) %>% 
  ungroup()

table(bdcTrans$trans)

write_rds(bdcTrans,"D:/!bso/transMat/bdcTrans_cor.rds")
####____READING BDCTRANS_COR____####
bdcTrans <- readRDS('D:/!bso/transMat/bdcTrans_cor.rds')
bdcCancel <- readRDS('D:/!bso/transMat/matCancel2.rds')

table(bdcTrans$trans)
####____CAPTURE EXAMPLES____####
bdcTrans <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CALIFICACION, monDate, DIASMORA) %>% 
  mutate(monDate = zoo::as.yearmon(monDate)) %>% 
  #select(-dayDate) %>%
  dplyr::filter(monDate >= 'mar. 2015') %>% 
  group_by(OPERACION,CTACLIENTE) %>%
  arrange(OPERACION, CTACLIENTE, monDate) %>%
  mutate(DiasMoraMes= DIASMORA - dplyr::lag(DIASMORA,1)) 

pos <- which(bdcTrans$DiasMoraMes>31)
bdcExample <- bdcTrans[sort(c(pos-1,pos)),]

tm2 <- bdcTrans %>% 
  dplyr::filter(cm1=="F" & cmt=="E")

ex <- bdcFull %>% 
  dplyr::filter(OPERACION==1175414)
####____CONTINUE____####
tm <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, trans) %>%
  summarise_all(sum) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(bdcCancel) %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% 
  group_by(monDate, cm1) %>% 
  mutate(rowTot = sum(one)) %>% 
  mutate(prob = round(one/rowTot*100,2)) %>% 
  arrange(cm1, monDate) %>% 
  mutate(Deterioro = case_when(cm1 == 'A' ~ sum(prob[cmt != 'A' & cmt != 'Z']), 
                               cm1 == 'B' ~ sum(prob[cmt != 'A' & cmt != 'B' & cmt != 'Z']),
                               cm1 == 'C' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C' & cmt != 'Z']),
                               cm1 == 'D' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D' & cmt != 'Z']),
                               cm1 == 'E' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E' & cmt != 'Z']),
                               cm1 == 'F' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E'& cmt != 'F' & cmt != 'Z']),
                               TRUE ~ 0)) %>% 
  mutate(Recuperacion = case_when(cm1 == 'A' ~ 0,
                                  cm1 == 'Z' ~ 0,
                                  cm1 == 'B' ~ sum(prob[cmt == 'A' | cmt == 'Z']),
                                  cm1 == 'C' ~ sum(prob[cmt == 'A' | cmt == 'B' | cmt == 'Z']),
                                  cm1 == 'D' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C' | cmt == 'Z']),
                                  cm1 == 'E' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D' | cmt == 'Z']),
                                  cm1 == 'F' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D'| cmt == 'E' | cmt == 'Z']),
                                  cm1 == 'S' ~ sum(prob[cmt == 'Z']),
                                  TRUE ~ 0)) %>% 
  mutate(Permanencia = case_when(cm1 == 'A' ~ sum(prob[cmt == 'A']), 
                                 cm1 == 'B' ~ sum(prob[cmt == 'B']),
                                 cm1 == 'C' ~ sum(prob[cmt == 'C']),
                                 cm1 == 'D' ~ sum(prob[cmt == 'D']),
                                 cm1 == 'E' ~ sum(prob[cmt == 'E']),
                                 cm1 == 'F' ~ sum(prob[cmt == 'F']),
                                 cm1 == 'S' ~ sum(prob[cmt == 'S']),
                                 cm1 == 'Z' ~ sum(prob[cmt == 'Z']),
                                 TRUE ~ 0)) %>% 
  glimpse()

write_rds(tm,"D:/!bso/transMat/tm_bdcTrans_corrds_matCancel2.rds")
tm <- readRDS('D:/!bso/transMat/tm_bdcTrans_corrds_matCancel2.rds') %>% 
  dplyr::filter(monDate < 'mar. 2020' | monDate > 'dic. 2021')
####____CREATING CUMULATED MATRICES FOR SIMULATION____####
mes <- "Mar23"
dir.create(paste0("D:/!bso/transMat/prev",mes))
tm <- read.csv(paste0('D:/!bso/transMat/Oreports/tmAll_',mes,'.csv')) %>% 
  mutate(monDate=as.yearmon(monDate)) %>% 
  # rename(prob=probN) %>% #For older versions e.g. Dic22
  dplyr::filter(monDate < 'mar. 2020' | monDate > 'dic. 2021') %>% 
  glimpse()

trans <- c("AA","AB","BA","BB") #Run this without filters for monDate
for (t  in trans) {
  gph <- tm %>% 
    dplyr::filter(trans==t) %>% 
    ggplot(aes(x=monDate,y=prob))+
    geom_line(color="red",size=1.5)+
    labs(x="Fecha",y="Probabilidad (%)",title=paste0("Transición ",t))+
    theme_light()
  ggsave(paste0('D:/!bso/transMat/prev',mes,'/EvolTrans_',t,'.png'),gph,
         width = 9,height = 6,units = "in")
  print(gph)
}
  
####___MATRIZ SENSIBILIZADA CON CUANTILES____####
####____SELECTING QUANTILES____####
#Plow for AA transitions or whenever cmt<cm1 or whenever cmt==Z
#Phigh for whenever cmt>cm1
#P50 for whenever cm1==cmt (except AA)
#From March23 and after AA gets P50
#From March23 and after cmtZ gets P50
plow <- 5
phigh <- 100-plow
fixed_trans <- c("AA","AB","BA","BB")
gph <- tm %>% 
  arrange(trans, monDate) %>%
  ungroup() %>% 
  select(trans, prob, cm1, cmt) %>%
  mutate(prob=prob/100) %>% 
  group_by(trans, cm1, cmt) %>%
  summarise(perl=quantile(prob,probs=plow/100,na.rm=T),
            per50=quantile(prob,probs=0.50,na.rm=T),
            perh=quantile(prob,probs=phigh/100,na.rm=T)) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>%
  mutate(prob=case_when(cmt<cm1~ perl,
                        trans %in% c("AA","BB","CC","DD","EE","SS","FF") | cmt=="Z" ~ per50,
                        cmt>cm1 & cmt!="Z" ~ perh,)) %>% 
  select(-perl,-per50,-perh) %>% 
  group_by(cm1) %>% 
  mutate(prest = ifelse(length(prob[which(cmt=="A")])!=0,1-prob[which(cmt=="A")],1)) %>% 
  mutate(prob = ifelse(cmt=="A",prob,prob*prest/sum(prob[which(cmt!="A")]))) %>% 
  mutate(ptot = sum(prob)) %>% 
  glimpse()

tmExp <- gph %>% 
  ungroup() %>% 
  arrange(cm1) %>% 
  select(cm1, cmt, prob) %>%
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  select(-cm1) %>%
  relocate(A,B,C,D,E,`F`,S,Z) %>% 
  add_row(A = 0, B=0, C=0, D=0, E=0, F=0, S=0, Z=1) %>% 
  glimpse()

M <- data.matrix(tmExp)
rownames(M) <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')

cm1v <- c('A', 'B', 'C', 'D', 'E', 'F','S','Z')
z <- as_tibble(M %^% 12) %>% 
  bind_cols(cm1=cm1v) %>% 
  pivot_longer(!cm1) %>% 
  dplyr::rename(prob = value,
                cmt = name) %>% 
  group_by(cm1) %>% 
  mutate(cumProb = cumsum(prob)) %>% 
  mutate(prob = prob*100,
         cumProb = cumProb*100) %>% 
  ungroup() %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(trans, cumProb) %>% 
  glimpse()
zavg <- as.data.frame(t(z)) %>% 
  row_to_names(row_number = 1) %>% 
  glimpse()
write.csv(zavg, paste0('D:/!bso/transMat/prev',mes,'/tm23_P',plow,'P',phigh,'.csv'), row.names = F)

M12 <- as_tibble(M %^% 12)
M12
Matrices <- list(M=M,M12=M12)
write.xlsx(Matrices,file = paste0('D:/!bso/transMat/prev',mes,'/MyM12_P',plow,'P',phigh,'.xlsx'))
####____AVERAGE MATRIX____####
gph <- tm %>% 
  arrange(trans, monDate) %>%
  ungroup() %>% 
  select(trans, prob, cm1, cmt) %>% 
  group_by(trans, cm1, cmt) %>%
  summarise_all(mean) %>% #Medida resumen para matriz de transición acumulada
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1) %>% 
  mutate(prob=prob/sum(prob)) %>% #Renormalización
  mutate(ptot = sum(prob)) %>% 
  glimpse()

tmExp <- gph %>% 
  ungroup() %>% 
  arrange(cm1) %>% 
  select(cm1, cmt, prob) %>%
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  select(-cm1) %>%
  relocate(A,B,C,D,E,`F`,S,Z) %>% 
  add_row(A = 0, B=0, C=0, D=0, E=0, F=0, S=0, Z=1) %>% 
  glimpse()

M <-data.matrix(tmExp)
cm1v <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')
M
z <- as_tibble(M %^% 12) %>% 
  bind_cols(cm1=cm1v) %>% 
  pivot_longer(!cm1) %>% 
  dplyr::rename(prob = value,
                cmt = name) %>% 
  group_by(cm1) %>% 
  mutate(cumProb = cumsum(prob)) %>% 
  mutate(prob = prob*100,
         cumProb = cumProb*100) %>% 
  ungroup() %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(trans, cumProb) %>% 
  glimpse()
zavg <- as.data.frame(t(z)) %>% 
  row_to_names(row_number = 1) %>% 
  glimpse()
write.csv(zavg, paste0('D:/!bso/transMat/prev',mes,'/tm23_avg12.csv'), row.names = F)

M12 <- as_tibble(M %^% 12)
M12
Matrices <- list(M=M,M12=M12)
write.xlsx(Matrices,file = paste0('D:/!bso/transMat/prev',mes,'/MyM12_avg12.xlsx'))
####____MATRIZ SENSIBILIZADA AB____####
gph <- tm %>% 
  arrange(trans, monDate) %>%
  ungroup() %>% 
  select(trans, prob, cm1, cmt) %>% 
  group_by(trans, cm1, cmt) %>%
  summarise_all(mean) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1) %>% 
  mutate(prob=prob/sum(prob)) %>% 
  mutate(ptot = sum(prob)) %>% 
  glimpse()

tmSens <- gph %>% 
  ungroup() %>%
  arrange(cm1) %>%
  select(cm1, cmt, prob) %>%
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  select(-cm1) %>%
  relocate(A,B,C,D,E,`F`,S,Z) %>% 
  add_row(A = 0, B=0, C=0, D=0, E=0, F=0, S=0, Z=1) %>% 
  glimpse()
tmSens$A[1] <- tmSens$A[1] - 0.0005
tmSens$B[1] <- tmSens$B[1] + 0.0005
MSens <- data.matrix(tmSens)
cm1v <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')
MSens

zSens <- as_tibble(MSens %^% 12) %>% 
  bind_cols(cm1=cm1v) %>% 
  pivot_longer(!cm1) %>% 
  dplyr::rename(prob = value,
                cmt = name) %>% 
  group_by(cm1) %>% 
  mutate(cumProb = cumsum(prob)) %>% 
  mutate(prob = prob*100,
         cumProb = cumProb*100) %>% 
  ungroup() %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(trans, cumProb) %>% 
  glimpse()

zSens <- as.data.frame(t(zSens)) %>% 
  row_to_names(row_number = 1) %>% 
  glimpse()
write.csv(zSens, paste0('D:/!bso/transMat/prev',mes,'/tm23_avg12_sens_ab.csv'), row.names = F)

M12 <- as_tibble(MSens %^% 12)
M12
Matrices <- list(M=MSens,M12=M12)
write.xlsx(Matrices,file = paste0('D:/!bso/transMat/prev',mes,'/MyM12_avg_12_sensab.xlsx'))
####____MATRIZ SENS BA____####
gph <- tm %>% 
  arrange(trans, monDate) %>%
  ungroup() %>% 
  select(trans, prob, cm1, cmt) %>% 
  group_by(trans, cm1, cmt) %>%
  summarise_all(mean) %>% 
  dplyr::filter(!is.na(cm1)) %>%
  ungroup() %>% 
  group_by(cm1) %>% 
  mutate(prob=prob/sum(prob)*100) %>% 
  mutate(ptot = sum(prob)) %>% 
  glimpse()

tmSens <- gph %>% 
  ungroup() %>% 
  arrange(cm1) %>% 
  select(cm1, cmt, prob) %>%
  mutate(prob = prob/100) %>%
  pivot_wider(names_from = cmt, values_from = prob, values_fill = 0) %>%
  select(-cm1) %>%
  relocate(A,B,C,D,E,`F`,S,Z) %>% 
  add_row(A = 0, B=0, C=0, D=0, E=0, F=0, S=0, Z=1) %>% 
  glimpse()
tmSens$A[2] <- tmSens$A[2] + 0.05
tmSens$B[2] <- tmSens$B[2] - 0.05
MSens <- data.matrix(tmSens)
cm1v <- c('A', 'B', 'C', 'D', 'E', 'F', 'S', 'Z')
MSens

zSens <- as_tibble(MSens %^% 12) %>% 
  bind_cols(cm1=cm1v) %>% 
  pivot_longer(!cm1) %>% 
  dplyr::rename(prob = value,
                cmt = name) %>% 
  group_by(cm1) %>% 
  mutate(cumProb = cumsum(prob)) %>% 
  mutate(prob = prob*100,
         cumProb = cumProb*100) %>% 
  ungroup() %>% 
  mutate(trans = paste0(cm1, cmt)) %>% 
  select(trans, cumProb) %>% 
  glimpse()
zSens <- as.data.frame(t(zSens)) %>% 
  row_to_names(row_number = 1) %>% 
  glimpse()
write.csv(zSens, paste0('D:/!bso/transMat/prev',mes,'/tm22_avg12_sens_ba.csv'), row.names = F)
M12 <- as_tibble(MSens %^% 12)
M12

Matrices <- list(M=MSens,M12=M12)
write.xlsx(Matrices,file = paste0('D:/!bso/transMat/prev',mes,'/MyM12_avg_12_sensba.xlsx'))

####____bdcTrans Corregido____####
bdcFull <- readRDS('D:/!bso/transMat/bdcFull.rds')

bdcTrans <- bdcFull %>% 
  select(OPERACION, CTACLIENTE, CALIFICACION, monDate,DIASMORA,tipoCred, SECTOR_CARTERA,ESTADO) %>% 
  mutate(CALIF2=case_when(ESTADO=="CASTIGADA"~'S',
                          tipoCred=="Vivienda" & DIASMORA<=30 ~ "A",
                          tipoCred=="Vivienda" & DIASMORA>30 & DIASMORA<=90 ~ "B",
                          tipoCred=="Vivienda" & DIASMORA>90 & DIASMORA<=180 ~ "C",
                          tipoCred=="Vivienda" & DIASMORA>180 & DIASMORA<=270 ~ "D",
                          tipoCred=="Vivienda" & DIASMORA>270 & DIASMORA<=360 ~ "E",
                          tipoCred=="Vivienda" & DIASMORA>360 ~ "F",
                          tipoCred=="Consumo" & DIASMORA<=5~"A",
                          tipoCred=="Consumo" & DIASMORA>5 & DIASMORA<=30~"B",
                          tipoCred=="Consumo" & DIASMORA>30 & DIASMORA<=55~"C",
                          tipoCred=="Consumo" & DIASMORA>55 & DIASMORA<=75~"D",
                          tipoCred=="Consumo" & DIASMORA>75 & DIASMORA<=90~"E",
                          tipoCred=="Consumo" & DIASMORA>90~"F",
                          tipoCred=="Micro" & !(SECTOR_CARTERA %in% c("1. PRODUCCION AGROPECUARIA CONTROLADA",
                                                                      "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
                                                                      "7.Prod.Agropec.No Controlada",
                                                                      "1.Prod. Agropec. Controlada" )) & DIASMORA<=5~"A",
                          tipoCred=="Micro" & !(SECTOR_CARTERA %in% c("1. PRODUCCION AGROPECUARIA CONTROLADA",
                                                                      "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
                                                                      "7.Prod.Agropec.No Controlada",
                                                                      "1.Prod. Agropec. Controlada" )) & DIASMORA>5 & DIASMORA<=30~"B",
                          tipoCred=="Micro" & !(SECTOR_CARTERA %in% c("1. PRODUCCION AGROPECUARIA CONTROLADA",
                                                                      "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
                                                                      "7.Prod.Agropec.No Controlada",
                                                                      "1.Prod. Agropec. Controlada" )) & DIASMORA>30 & DIASMORA<=55~"C",
                          tipoCred=="Micro" & !(SECTOR_CARTERA %in% c("1. PRODUCCION AGROPECUARIA CONTROLADA",
                                                                      "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
                                                                      "7.Prod.Agropec.No Controlada",
                                                                      "1.Prod. Agropec. Controlada" )) & DIASMORA>55 & DIASMORA<=75~"D",
                          tipoCred=="Micro" & !(SECTOR_CARTERA %in% c("1. PRODUCCION AGROPECUARIA CONTROLADA",
                                                                      "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
                                                                      "7.Prod.Agropec.No Controlada",
                                                                      "1.Prod. Agropec. Controlada" )) & DIASMORA>75 & DIASMORA<=90~"E",
                          tipoCred=="Micro" & !(SECTOR_CARTERA %in% c("1. PRODUCCION AGROPECUARIA CONTROLADA",
                                                                      "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
                                                                      "7.Prod.Agropec.No Controlada",
                                                                      "1.Prod. Agropec. Controlada" )) & DIASMORA>90~"F",
                          tipoCred=="Micro" & SECTOR_CARTERA %in% c("1. PRODUCCION AGROPECUARIA CONTROLADA",
                                                                    "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
                                                                    "7.Prod.Agropec.No Controlada",
                                                                    "1.Prod. Agropec. Controlada" ) & DIASMORA<=20~"A",
                          tipoCred=="Micro" & SECTOR_CARTERA %in% c("1. PRODUCCION AGROPECUARIA CONTROLADA",
                                                                    "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
                                                                    "7.Prod.Agropec.No Controlada",
                                                                    "1.Prod. Agropec. Controlada" ) & DIASMORA>20 & DIASMORA<=30~"B",
                          tipoCred=="Micro" & SECTOR_CARTERA %in% c("1. PRODUCCION AGROPECUARIA CONTROLADA",
                                                                    "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
                                                                    "7.Prod.Agropec.No Controlada",
                                                                    "1.Prod. Agropec. Controlada" ) & DIASMORA>30 & DIASMORA<=55~"C",
                          tipoCred=="Micro" & SECTOR_CARTERA %in% c("1. PRODUCCION AGROPECUARIA CONTROLADA",
                                                                    "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
                                                                    "7.Prod.Agropec.No Controlada",
                                                                    "1.Prod. Agropec. Controlada" ) & DIASMORA>55 & DIASMORA<=75~"D",
                          tipoCred=="Micro" & SECTOR_CARTERA %in% c("1. PRODUCCION AGROPECUARIA CONTROLADA",
                                                                    "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
                                                                    "7.Prod.Agropec.No Controlada",
                                                                    "1.Prod. Agropec. Controlada" ) & DIASMORA>75 & DIASMORA<=90~"E",
                          tipoCred=="Micro" & SECTOR_CARTERA %in% c("1. PRODUCCION AGROPECUARIA CONTROLADA",
                                                                    "4. PRODUCCION AGROPECUARIA NO CONTROLADA",
                                                                    "7.Prod.Agropec.No Controlada",
                                                                    "1.Prod. Agropec. Controlada" ) & DIASMORA>90~"F",
                          TRUE~CALIFICACION)) %>% 
  mutate(monDate = zoo::as.yearmon(monDate)) %>% 
  select(-CALIFICACION,-ESTADO,-tipoCred,-SECTOR_CARTERA) %>%
  dplyr::filter(monDate >= 'mar. 2015') %>% 
  group_by(OPERACION, CTACLIENTE) %>%
  arrange(OPERACION, CTACLIENTE, monDate) %>%
  dplyr::rename(cmt = CALIF2) %>% 
  mutate(cm1 = dplyr::lag(cmt, 1)) %>% 
  mutate(det = ifelse(cm1 < cmt,1,0)) %>% 
  mutate(DiasMoraMes= DIASMORA - dplyr::lag(DIASMORA,1)) %>% 
  mutate(my= DIASMORA - dplyr::lag(DIASMORA,1)) %>% 
  ungroup() %>% 
  mutate(trans = ifelse(cm1 %in% c('A','B','C','D','E','F') &
                          cmt %in% c('A','B','C','D','E','F','S'),paste0(cm1,cmt),NA)) %>% 
  mutate(trans = ifelse(cm1=='S'&cmt=='S','SS',trans)) %>% 
  dplyr::filter(!is.na(cm1))
  
xx <- bdcTrans %>% 
  dplyr::filter(!is.na(DiasMoraMes) & !(DiasMoraMes>31)) %>% 
  ungroup() %>% 
  group_by(cm1, monDate) %>% 
  mutate(rowTot = n()) %>% 
  ungroup()

table(bdcTrans$trans)

write_rds(xx,"D:/!bso/transMat/bdcTrans_cor3.rds")
#En la versión previa (bdcTrans_cor2), cambiaba involuntariamente los castigos (S) a F 
#esto cambiaba los estados de 4,7% de las calificaciones (la mayoría de S a F)
#en esta versión corregida (bdcTrans_cor3), 0,115% de las califiaciones son cambiadas.
####____READING bdcTrans____####
bdcTrans <- readRDS('D:/!bso/transMat/bdcTrans_cor3.rds')
bdcCancel <- readRDS('D:/!bso/transMat/matCancel2.rds')

table(bdcTrans$trans)
####____CREATING TM____####
tm <- bdcTrans %>% 
  ungroup() %>% 
  select(trans, monDate) %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  mutate(one = 1) %>% 
  group_by(monDate, trans) %>%
  summarise_all(sum) %>% 
  mutate(cm1 = substr(trans,1,1)) %>% 
  mutate(cmt = substr(trans,2,2)) %>% 
  ungroup() %>%
  bind_rows(bdcCancel) %>% 
  arrange(trans, monDate) %>% 
  dplyr::filter(monDate > 'feb. 2015') %>% 
  group_by(monDate, cm1) %>% 
  mutate(rowTot = sum(one)) %>% 
  mutate(prob = round(one/rowTot*100,2)) %>% 
  arrange(cm1, monDate) %>% 
  mutate(Deterioro = case_when(cm1 == 'A' ~ sum(prob[cmt != 'A' & cmt != 'Z']), 
                               cm1 == 'B' ~ sum(prob[cmt != 'A' & cmt != 'B' & cmt != 'Z']),
                               cm1 == 'C' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C' & cmt != 'Z']),
                               cm1 == 'D' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D' & cmt != 'Z']),
                               cm1 == 'E' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E' & cmt != 'Z']),
                               cm1 == 'F' ~ sum(prob[cmt != 'A' & cmt != 'B'& cmt != 'C'& cmt != 'D'& cmt != 'E'& cmt != 'F' & cmt != 'Z']),
                               TRUE ~ 0)) %>% 
  mutate(Recuperacion = case_when(cm1 == 'A' ~ 0,
                                  cm1 == 'Z' ~ 0,
                                  cm1 == 'B' ~ sum(prob[cmt == 'A' | cmt == 'Z']),
                                  cm1 == 'C' ~ sum(prob[cmt == 'A' | cmt == 'B' | cmt == 'Z']),
                                  cm1 == 'D' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C' | cmt == 'Z']),
                                  cm1 == 'E' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D' | cmt == 'Z']),
                                  cm1 == 'F' ~ sum(prob[cmt == 'A' | cmt == 'B'| cmt == 'C'| cmt == 'D'| cmt == 'E' | cmt == 'Z']),
                                  cm1 == 'S' ~ sum(prob[cmt == 'Z']),
                                  TRUE ~ 0)) %>% 
  mutate(Permanencia = case_when(cm1 == 'A' ~ sum(prob[cmt == 'A']), 
                                 cm1 == 'B' ~ sum(prob[cmt == 'B']),
                                 cm1 == 'C' ~ sum(prob[cmt == 'C']),
                                 cm1 == 'D' ~ sum(prob[cmt == 'D']),
                                 cm1 == 'E' ~ sum(prob[cmt == 'E']),
                                 cm1 == 'F' ~ sum(prob[cmt == 'F']),
                                 cm1 == 'S' ~ sum(prob[cmt == 'S']),
                                 cm1 == 'Z' ~ sum(prob[cmt == 'Z']),
                                 TRUE ~ 0)) %>% 
  glimpse()

write_rds(tm,"D:/!bso/transMat/tm_bdcTrans_cor3rds_matCancel2.rds")
tm <- readRDS('D:/!bso/transMat/tm_bdcTrans_cor3rds_matCancel2.rds')

####____PRUEBAS____####
bdcTrans <- bdcTrans %>% 
  mutate(my=as.Date(monDate)) %>% 
  group_by(OPERACION,CTACLIENTE) %>% 
  arrange(OPERACION,CTACLIENTE,monDate) %>% 
  mutate(Difmes=my-dplyr::lag(my,1)) %>% 
  dplyr::filter(Difmes>31) %>% 
  ungroup()

xx <- bdcTrans %>% 
  dplyr::filter(CALIFICACION!=CALIF2) %>% 
  arrange(OPERACION,CTACLIENTE,monDate) %>% 
  dplyr::filter(CALIF2=="B")
