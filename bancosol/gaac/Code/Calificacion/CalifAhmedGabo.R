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
library(fastDummies)
library(openxlsx)
library(sqldf)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
################################################################################
bdcDic <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Dic2022.rds')
bdcBSO_full <- fread('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraDic2022.txt',
                     encoding = 'Latin-1', fill = T) %>% 
  mutate(fbase = 'Dic2022') %>%
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
  mutate(Fecha = as.yearmon(dayDate)) %>%
  select(-fbase) %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(ctaCont = substr(RUBRO,1,3)) %>% 
  mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                               ctaCont == '134'~saldous,
                               ctaCont == '136'~saldous,
                               ctaCont == '137'~saldous,
                               TRUE ~ 0)) %>% 
  # mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
  select(CTACLIENTE,OPERACION,CI, previus, saldous, saldoMora, DIASMORA, GENERO,ESTADO,MODULO) %>%
  mutate(par0 = ifelse(DIASMORA > 0, saldous, 0)) %>%
  glimpse()

bdcBSO_full <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Dic2022.rds') %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  dplyr::filter(MODULO != 131) %>%
  mutate(fbase = 'Dic2022') %>%
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
  mutate(Fecha = as.yearmon(dayDate)) %>%
  select(-fbase) %>%
  select(CI, previus, saldous, saldoMora, DIASMORA, GENERO) %>%
  mutate(par0 = ifelse(DIASMORA > 0, saldous, 0)) %>%
  glimpse()
###INFOFULL
infoRaw <- fread(paste0('D:/!bso/califClientes/BSO202212_utf8.txt'), encoding = 'UTF-8', fill = T)
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
  rename(saldoMoraInfo=saldoMora) %>% 
  mutate(fbase= '202212') %>%
  mutate(Mes=as.yearmon(as.Date(paste0(fbase, "01"), "%Y%m%d")))
####____####

infoCleanA <- infoCheck %>% 
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% 
  mutate(esBSO=ifelse(`SIGLA SBEF`=='BSO',1,0)) %>%
  mutate(noesBSO=ifelse(`SIGLA SBEF`!='BSO',1,0)) %>%
  group_by(CI) %>%
  dplyr::filter(max(row_number())>1) %>% 
  # rename(Calif = `SBEF CALIFICACION`) %>% 
  # mutate(Calif = ifelse(is.na(Calif),'_',Calif)) %>% 
  # mutate(PeorCalifBSOG = max(Calif[esBSO==1])) %>%
  # mutate(PeorCalifSFG = max(`SIGLA SBEF`[`SIGLA SBEF`!='BSO'],na.rm = T)) %>%
  mutate(califBSO = ifelse(`SIGLA SBEF` == 'BSO', `SBEF CALIFICACION`, '_'),
         califBSO_2 = max(califBSO, na.rm = T)) %>% # Max. calificación en BSO
  # dplyr::filter(califBSO_2 != '_') %>%
  # select(-califBSO) %>%
  mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
         califSF_2 = max(califSF, na.rm = T)) %>%  # Max calificación en SF
  mutate(saldoNoBSO = sum(saldo*noesBSO,na.rm=T)) %>% 
  mutate(saldoMoraNoBSO = sum(saldoMoraInfo*noesBSO,na.rm=T)) %>% 
  mutate(saldoMMCNoBSO = sum(saldoMMC*noesBSO,na.rm=T)) %>% 
  dplyr::filter(califBSO_2==califBSO & `SIGLA SBEF`=='BSO') %>% 
  select(-califBSO,-califSF,-`SBEF CALIFICACION`) %>% 
  ungroup() %>% 
  dplyr::filter(califSF_2!="_")
  # group_by(OPERACION) %>% 
  # mutate(Opcount=max(row_number())) %>% 
  # ungroup()
  # # dplyr::filter(califSF_2 != '_') %>% 
  # # mutate(PeorOpBSO=ifelse(`SBEF CALIFICACION`==califBSO_2,OPERACION,-1),
  # #        PeorOpBSO_2=max(PeorOpBSO,na.rm=T)) %>% 
  # # select(-califSF) %>%
  mutate(CastigadoBSO = ifelse(`SBEF CASTIGADO`>0 & `SIGLA SBEF`=='BSO',1,0)) %>% 
  select(CI, califBSO_2,califSF_2,saldo,saldoMora,saldoMMC,``) %>% 
  group_by(CI) %>% 
  
  group_by(CI, califBSO_2, califSF_2) %>% #Este es el group_by importante
  # summarise_all(max) %>%
  # ungroup() %>% 
  # mutate(CI_2 = gsub("[^0-9.-]", "", CI)) %>%
  # mutate(CI_2 = gsub("[^[:alnum:] ]", "", CI_2)) %>%
  # mutate(CI_2 = as.numeric(CI_2)) %>% 
  mutate(CI_2 = str_extract(CI,pattern = "[0-9]+")) %>% 
  mutate(CI_2 = as.numeric(CI_2)) %>% 
  glimpse()

table(infoCleanA$`SIGLA SBEF`)  
length(which(is.na(infoCleanA$OPERACION)))
n_distinct(infoCleanA$OPERACION)

infoCleanG <- infoCheck %>% 
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% 
  mutate(esBSO=ifelse(`SIGLA SBEF`=='BSO',1,0)) %>%
  mutate(noBSO=ifelse(`SIGLA SBEF`!='BSO',1,0)) %>%
  group_by(CI) %>%
  mutate(tieneBSO = sum(esBSO),
         noBSO = sum(noBSO)) %>%
  mutate(califBSO = ifelse(`SIGLA SBEF` == 'BSO', `SBEF CALIFICACION`, '_'),
         califBSO_2 = max(califBSO, na.rm = T)) %>% # Max. calificación en BSO
  # select(-califBSO) %>%
  mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
         califSF_2 = max(califSF, na.rm = T)) %>%  # Max calificación en SF
  ungroup() %>% 
  dplyr::filter(tieneBSO>=1) %>%
  dplyr::filter(noBSO>=1) %>% 
  arrange(CI,tieneBSO) %>% 
  dplyr::filter(`SIGLA SBEF`=='BSO') %>% 
  group_by(CI) %>% 
  summarise(saldo=sum(saldo),saldoVig=sum(saldoVig),Mora=sum(saldoMora),
            MontoOriginal=sum(MontoOriginal),saldoVig=sum(saldoVig),
            califBSO=max(califBSO),califSF=max(califSF))
  
# infoCleanG$CI[!(infoCleanG$CI %in% infoCleanA$CI)]

bsoFull <- bdcBSO_full %>% 
  # mutate(CI_2 = gsub("[^0-9.-]", "", CI)) %>%
  # mutate(CI_2 = gsub("[^[:alnum:] ]", "", CI_2)) %>%
  # mutate(CI_2 = as.numeric(CI_2)) %>%
  mutate(CI_2 = str_extract(CI,pattern = "[0-9]+")) %>% 
  mutate(CI_2 = as.numeric(CI_2)) %>% 
  # select(CI,CI_2,GENERO) %>% 
  distinct_all() %>% 
  glimpse()
  
bsoFull2 <-bsoFull %>% 
  select(-CI) %>% rename(GENERO2=GENERO)
bsoFull<- bsoFull %>% select(-CI_2)
####
infoJoin <- infoCleanA %>% 
  select(-CI) %>% 
  left_join(bsoFull,by=c("CTACLIENTE","OPERACION")) 

n_distinct(infoJoin$CI) #Prueba de CIS únicos
  
table(infoJoin$ESTADO,useNA = "ifany")#Contabilización de operaciones por estado
infoJoin %>% #Saldo y Operaciones por estado
  select(saldous,ESTADO) %>% 
  group_by(ESTADO) %>% 
  summarise_all(.funs=list(saldo=~sum(.x),
                       count=~n()))
sum(infoJoin$saldoCastInfo,na.rm = T)
table(infoJoin$GENERO,useNA = "ifany")
table(infoJoin$GENERO,infoJoin$ESTADO,useNA = "ifany")

write.xlsx(infoJoin,'D:/!bso/califClientes/infoJoin.xlsx')
# infoJ <- sqldf("SELECT *
#       FROM bsoFull AS b, infoCleanA AS i
#       WHERE (b.CTACLIENTE=i.CTACLIENTE AND b.OPERACION=i.OPERACION) OR 
#       (b.CTACLIENTE=i.CTACLIENTE AND b.CI_2=i.CI_2) ")


infoJoin <- infoCleanA %>% 
  left_join(bsoFull,by="CI") %>% 
  mutate(JoinCI=1) %>% 
  left_join(bsoFull2,by="CI_2") %>% 
  group_by(CI) %>% 
  mutate(JoinCI2=sum(JoinCI)) %>% 
  mutate(GENERO=ifelse(is.na(GENERO) & !is.na(GENERO2),GENERO2,GENERO)) %>% 
  select(-GENERO2) %>% 
  distinct_all() %>% 
  dplyr::filter(`SBEF CASTIGADO`<=0)

ops <- which(is.na(infoJoin$GENERO))
table(infoJoin$GENERO,useNA = "ifany")

infoJoin2 <- infoJoin[ops,] %>% 
  select(-GENERO) %>% 
  left_join(bsoFull,by="CI_2")
infoFinal <- infoJoin[-ops,] %>% 
  bind_rows(infoJoin2)
################################################################################
#TABLAS POR GENERO
bdcBSO_tab <- bsoFull %>% 
  dplyr::filter(MODULO!=131) %>%
  dplyr::filter(ESTADO!="CASTIGADA") %>%
  group_by(GENERO) %>% 
  summarise(SaldoBSO=sum(saldous),ClientesBSO=n_distinct(CI),SaldoMoraBSO=sum(saldoMora),
            par0BSO=sum(par0)) %>% 
  ungroup()

infoPerfFiltro <- infoJoin %>% 
  dplyr::filter(MODULO!=131) %>%
  dplyr::filter(ESTADO!="CASTIGADA") %>%
  group_by(CI,GENERO) %>% 
  summarise(saldo=sum(saldous),saldoMora=sum(saldoMora),
            par0=sum(par0),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2),
            saldoNoBSO=max(saldoNoBSO),saldoMoraNoBSO=max(saldoMoraNoBSO),
            saldoMMCNoBSO=max(saldoMMCNoBSO)) %>% 
  ungroup() %>% 
  mutate(Cliente = 1.0) %>% 
  mutate(PeorCalif=ifelse(califBSO_2<califSF_2,1,0)) %>% 
  mutate(MejorCalif=ifelse(califBSO_2>califSF_2,1,0)) %>% 
  mutate(igualCalif=ifelse(califBSO_2==califSF_2,1,0)) %>% 
  left_join(bdcBSO_tab,by="GENERO") %>% 
  mutate(Clientes_comp=Cliente/ClientesBSO,
         Saldo_comp=saldo/SaldoBSO,
         Saldo_peor=PeorCalif*saldo/SaldoBSO,
         Saldo_peor=PeorCalif*saldo)
  

infoPerf <- infoJoin %>% 
  group_by(CI,GENERO) %>% 
  summarise(saldo=sum(saldous),saldoMora=sum(saldoMora),
            par0=sum(par0),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2),
            saldoNoBSO=max(saldoNoBSO),saldoMoraNoBSO=max(saldoMoraNoBSO),
            saldoMMCNoBSO=max(saldoMMCNoBSO)) %>% 
  ungroup() %>% 
  mutate(Cliente = 1.0) %>% 
  mutate(PeorCalif=ifelse(califBSO_2<califSF_2,1,0)) %>% 
  mutate(MejorCalif=ifelse(califBSO_2>califSF_2,1,0)) %>% 
  mutate(igualCalif=ifelse(califBSO_2==califSF_2,1,0)) %>% 
  left_join(bdcBSO_tab,by="GENERO") %>% 
  mutate(Clientes_comp=Cliente/ClientesBSO,
         Saldo_comp=saldo/SaldoBSO,
         Saldo_peor=PeorCalif*saldo/SaldoBSO,
         Saldo_peor=PeorCalif*saldo)

infos <- list(infoPerfFiltro=infoPerfFiltro,infoPerf=infoPerf)

write.xlsx(infos,'D:/!bso/califClientes/infoPerf3.xlsx')

infoPerf <- infoJoin %>% 
  group_by(CI,GENERO) %>% 
  summarise(saldo=sum(saldo),saldoMora=sum(saldoMora),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2)) %>% 
  ungroup() %>% 
  select(califBSO_2, califSF_2,GENERO) %>% 
  group_by(califBSO_2, califSF_2,GENERO) %>% # Agrupamos por calificaciones para hacer el recuento 
  summarise(n=n()) %>% 
  pivot_wider(names_from = califSF_2, values_from = n) %>% # Ponemos calif en el SF en las columnas
  mutate(across(A:`F`,~as.double(.x))) %>% 
  rowwise() %>% 
  mutate(igualCalif=case_when(califBSO_2 == 'A'~ A,
                              califBSO_2 == 'B'~ B,
                              califBSO_2 == 'C'~ C,
                              califBSO_2 == 'D'~ D,
                              califBSO_2 == 'E'~ E,
                              califBSO_2 == 'F'~ `F`)) %>% 
  mutate(peorCalif=case_when(califBSO_2 == 'A'~ sum(B,C,D,E,`F`,na.rm = T),
                             califBSO_2 == 'B'~ sum(C,D,E,`F`,na.rm = T),
                             califBSO_2 == 'C'~ sum(D,E,`F`,na.rm = T),
                             califBSO_2 == 'D'~ sum(E,`F`,na.rm = T),
                             califBSO_2 == 'E'~ (`F`),)) %>% 
  mutate(mejorCalif=case_when(califBSO_2 == 'B'~ (A),
                              califBSO_2 == 'C'~ sum(A,B,na.rm = T),
                              califBSO_2 == 'D'~ sum(A,B,C,na.rm = T),
                              califBSO_2 == 'E'~ sum(A,B,C,D,na.rm = T),
                              califBSO_2 == 'F'~ sum(A,B,C,D,E,na.rm = T))) %>% 
  mutate(TotalFila=sum(A,B,C,D,E,`F`,na.rm = T)) %>%
  mutate(TotalCalif= sum(c(igualCalif, peorCalif, mejorCalif), na.rm = T)) %>% 
  mutate(across(A:TotalCalif, ~replace_na(. , 0))) %>% 
  adorn_totals('row', 'col')
