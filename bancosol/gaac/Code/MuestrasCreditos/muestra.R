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
  #new <- vector(mode = 'character',length = length(quant))
  new <- rep(NA,length(quant))
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
####____READING CIERRES____####
lastday <- data.frame(dia=seq.Date(as.Date("2021-12-01"),as.Date("2022-11-30"),by="1 day")) %>%
  mutate(fdes=as.yearmon(dia)) %>%
  group_by(fdes) %>%
  summarise(Cierre=max(dia)) %>% 
  ungroup()

#La muestra es con la última fecha de corte y filtrando las fechas de desembolso
flist <- c('ec_Nov2022.rds')

bdcNC <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/', flist))
bdcFull <- bdcNC %>% 
  mutate(fdes = as.Date(FDESEMBOLSO,"%d/%m/%y")) %>% 
  dplyr::filter(year(fdes)==2022 | (month(fdes)==12 & year(fdes)==2021)) %>% 
  dplyr::filter(MODULO != 131) %>% 
  dplyr::filter(ESTADO!="CASTIGADA", TIPO_CREDITO %in% c("H0","H1","H2","H3","H4"))

for(i in 1:length(flist)) {
  print(flist[i])
  bdcNC <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/', flist[i]))
  bdcFilter <- bdcNC %>% 
    mutate(fdes = as.yearmon(fdes)) %>% 
    left_join(lastday,by="fdes") %>% 
    dplyr::filter(MODULO != 131) %>% 
    dplyr::filter(fdes==
                    as.yearmon(paste0(substr(flist[i],4,6),". ",substr(flist[i],7,10))) ) %>% 
    dplyr::filter(ESTADO!="CASTIGADA", TIPO_CREDITO %in% c("H0","H1","H2","H3","H4"))
  bdcList[[i]] <- bdcFilter
}

bdcList[[12]]$TIPO_OPER <- as.integer(bdcList[[12]]$TIPO_OPER)
bdcFull <- NULL
for (i in 1:length(bdcList)) {
  bdcFull <- bdcFull %>% 
    bind_rows(bdcList[[i]])
}

bdcFull <- bdcFull %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT,
         CI,ASESOR,NOMBRE_ASESOR,MONEDA,SALDO,FDESEMBOLSO,FFINALIZA,ESTADO,CIU,
         PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE)
write.csv(bdcFull,"D:/!bso/girCartera/samples/bdcDic21Nov22.csv",row.names = F)

####____READING POPULATION DATABASES____####
bdcFull <- fread("D:/!bso/girCartera/samples/bdcDic21Nov22.csv",
                 encoding = "UTF-8",fill=T,sep=",") %>% 
  mutate(REGIONAL=case_when(str_detect(sucursal,"La Paz|Oruro")~"Regional Occidente",
                            str_detect(sucursal,"Santa Cruz|Beni")~"Regional Oriente",
                            str_detect(sucursal,"Cochabamba|Tarija")~"Regional Centro",
                            str_detect(sucursal,"El Alto|Pando")~"Regional El Alto",
                            str_detect(sucursal,"Potosí|Chuquisaca")~"Regional Sur"))
bdcFull <- bdcFull %>% 
  mutate(REGIONAL=case_when(str_detect(sucursal,"La Paz|Oruro")~"Regional Occidente",
                            str_detect(sucursal,"Santa Cruz|Beni")~"Regional Oriente",
                            str_detect(sucursal,"Cochabamba|Tarija")~"Regional Centro",
                            str_detect(sucursal,"El Alto|Pando")~"Regional El Alto",
                            str_detect(sucursal,"Potosí|Chuquisaca")~"Regional Sur"))
####____TAMAÑO DE MUESTRA____####
####____MUESTRA ESTRATIFICADA POR SUCURSAL____####
Tot <- bdcFull %>% 
  group_by(TIPO_CREDITO) %>% 
  summarise(SALDO=sum(SALDO)/6.86,nOps=n()) %>% 
  ungroup()

Tot <- bdcFull %>% 
  group_by(REGIONAL) %>% 
  summarise(SALDO=sum(SALDO)/6.86,nOps=n()) %>% 
  ungroup()

tam <- bdcFull %>% 
  mutate(Nops = sum(opTot)) %>%
  group_by(sucursal) %>% 
  summarise(Nh = sum(opTot),Wh= sum(opTot)/max(Nops),
            Ph = 1-0.136)%>%
  ungroup() %>% 
  mutate(pph = Wh*Ph) %>% 
  mutate(Sh = sqrt(Nh*Ph*(1-Ph)/(Nh-1))) %>% 
  summarise(Nops=sum(Nh), sumSh= sum(Wh*Sh), sumSh2=sum(Wh*Sh^2),
            z = qnorm(1-0.05/2), e = 0.05*sum(pph)) %>% 
  mutate(n=sumSh^2/(e^2/z^2+sumSh2/Nops))
#SEED 1234 PARA PRINCIPAL y 12345 PARA REEMPLAZO
set.seed(1234)
muestra <- bdcFull %>%
  mutate(Nops = sum(opTot)) %>%
  group_by(sucursal) %>%
  mutate(Wh=sum(opTot)/max(Nops)) %>% 
  sample_n(round(max(Wh)*tam$n,0)) %>%
  mutate(x=1) %>% 
  ungroup() %>% 
  select(-Wh,-x)


agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")
muestra <- muestra %>% 
  left_join(agen,by="AGENCIA") %>% 
  rename(SALDO_USD=saldous, SUCURSAL=sucursal) %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT,
         CI,ASESOR,NOMBRE_AGENCIA,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,ESTADO,CIU,
         PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,REGIONAL,SUCURSAL)
write_excel_csv(muestra,file="D:/!bso/girCartera/samples/Muestra_Principal_1122_RME_VIVIENDA.csv",delim = ",")
write_excel_csv(muestra,file="D:/!bso/girCartera/samples/Muestra_Reemplazo_1122_RME_VIVIENDA.csv")
muestra <- fread("D:/!bso/girCartera/samples/Muestra_Principal_1122_RME_VIVIENDA.csv",
                 encoding = "UTF-8",fill=TRUE)
muestra <- fread("D:/!bso/girCartera/samples/Muestra_Reemplazo_1122_RME_VIVIENDA.csv",
                 encoding = "UTF-8",fill=TRUE)

M <- muestra %>% 
  group_by(REGIONAL) %>% 
  summarise(SALDO=sum(SALDO)/6.86,nOps=n()) %>% 
  ungroup() 
write_xlsx(bind_cols(Tot,M),"D:/!bso/girCartera/samples/Replica2.xlsx")
t <- muestra %>% 
  select(sucursal,opTot,x) %>% 
  mutate(Nops = sum(opTot)) %>%
  group_by(sucursal) %>% 
  summarise(Wh=sum(opTot)/max(Nops),x=sum(x))

sum(muestra$x)

####____PARA DAN____####
mon <- c(182014520/15513,580894310/97708,26858436/1244,46132168/5790,
         666620/29,28827144/8176,72395883/17454,7812769/597)

m <- mean(mon)
s <- var(mon)
N <- 146511
#PARA VIVIENDA
m <- mean(bdcFull$saldous)
s <- var(bdcFull$saldous)
#m <- 0.88
#s <- m*(1-m)
N <- nrow(bdcFull)

c_lev <- qnorm(1-seq(0.2,0.01,-0.01)/2)
bound <- seq(0.1,0.01,-0.01)*m
tam <- NULL
for (b in bound) {
  n <- ceiling(N*s/(N*b^2/c_lev^2+s))
  tam <- rbind(tam,n)
}
colnames(tam) <- 1-seq(0.2,0.01,-0.01)
rownames(tam) <- seq(0.1,0.01,-0.01)
tam <- as.data.frame(tam)
tam$error <- seq(0.01,0.1,0.01)
tam <- tam %>% relocate(error)
write_xlsx(tam,"D:/!bso/girCartera/samples/size.xlsx")

####____MUESTRAS____####
#PARA TODAS LAS MUESTRAS POSIBLES
for(i in 1:nrow(tam)){
  for (j in 1:ncol(tam)) {
    set.seed(1234)
    pos <- sample(nrow(bdcFull),size = tam[i,j])
    muestra <- bdcFull[pos,]
    write_xlsx(muestra,paste0("D:/!bso/girCartera/samples/sampleDic21Nov22_size",tam[i,j],".xlsx"))
  }
}
#PARA UNA MUESTRA ESPECÍFICA

set.seed(1234567)
pos <- sample(nrow(bdcFull),size = tam[1,17])
muestra <- bdcFull[pos,]
write_xlsx(muestra,paste0("D:/!bso/girCartera/samples/sampleDic21Nov22_size",tam[1,12],".xlsx"))

Tot <- bdcFull %>% 
  group_by(TIPO_CREDITO) %>% 
  summarise(saldous=sum(saldous),nOps=n()) %>% 
  ungroup()

M <- muestra %>% 
  group_by(TIPO_CREDITO) %>% 
  summarise(saldous=sum(saldous),nOps=n()) %>% 
  ungroup()
#####____REPLICA DE MUESTRA FRANZ____####
bdcFull <- fread("D:/!bso/girCartera/samples/Base_Poblacion_Consolidado_1121_RME_VIVIENDA.csv",
                 encoding = "Latin-1",fill=T,sep=",") %>% 
  mutate(REGIONAL=case_when(str_detect(Nombre_Regional,"La Paz|Oruro")~"Regional Occidente",
                            str_detect(Nombre_Regional,"Santa Cruz|Beni")~"Regional Oriente",
                            str_detect(Nombre_Regional,"Cochabamba|Tarija")~"Regional Centro",
                            str_detect(Nombre_Regional,"El Alto|Pando")~"Regional El Alto",
                            str_detect(Nombre_Regional,"Potosí|Chuquisaca")~"Regional Sur"))
Tot <- bdcFull %>% 
  group_by(TIPO_CREDITO) %>% 
  summarise(SALDO=sum(SALDO)/6.86,nOps=n()) %>% 
  ungroup()

Tot <- bdcFull %>% 
  group_by(REGIONAL) %>% 
  summarise(SALDO=sum(SALDO)/6.86,nOps=n()) %>% 
  ungroup() 

tam <- bdcFull %>% 
  mutate(Nops = sum(`n()`)) %>%
  group_by(Nombre_Regional) %>% 
  summarise(Nh = sum(`n()`),Wh= sum(`n()`)/max(Nops),
            Ph = 1-0.146)%>%
  ungroup() %>% 
  mutate(pph = Wh*Ph) %>% 
  mutate(Sh = sqrt(Nh*Ph*(1-Ph)/(Nh-1))) %>% 
  summarise(Nops=sum(Nh), sumSh= sum(Wh*Sh), sumSh2=sum(Wh*Sh^2),
            z = qnorm(1-0.05/2), e = 0.05*sum(pph)) %>% 
  mutate(n=sumSh^2/(e^2/z^2+sumSh2/Nops))

muestra <- bdcFull %>%
  mutate(Nops = sum(`n()`)) %>%
  group_by(REGIONAL) %>%
  mutate(Wh=sum(`n()`)/max(Nops)) %>% 
  sample_n(round(max(Wh)*tam$n,0)) %>% 
  mutate(x=1) %>% 
  ungroup()

M <- muestra %>% 
  group_by(REGIONAL) %>% 
  summarise(SALDO=sum(SALDO)/6.86,nOps=n()) %>% 
  ungroup() 

sum(muestra$x)

write_xlsx(bind_cols(Tot,M),"D:/!bso/girCartera/samples/Replica.xlsx")

####____MUESTRA DESEMBOLSOS PARA ALVARO RIOS____####
#Solicitada verbalmente en fecha 16 de mayo de 2023
bdcDesem <- readRDS('D:/!bso/girCartera/rds/ec_Abr2023.rds') %>% 
  dplyr::filter(MODULO != 131) %>% #Aqui se eliminan lineas de credito
  dplyr::filter(ctaCont!=623) %>% #Aquí se eliminan las operaciones cuya ctaCont es Boleta de garantía
  dplyr::filter(as.yearmon(fdes)>="Ene. 2023") %>% 
  select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_AGENCIA,NOMBRE_TIT, PATERNO_TIT,
         MATERNO_TIT,CI,ASESOR,NOMBRE_ASESOR,MONEDA,MONTO,SALDO,FDESEMBOLSO,FFINALIZA,
         ESTADO,CIU,PLAZODIAS,TIPO_CREDITO,DESC_OBJCRED,CAEDEC_DEST,TIPO_CLIENTE,
         REGIONAL=Regional,SUCURSAL=Sucursal,LINEA)

dir.create(path = paste0("D:/!bso/girCartera/desembolsados"))

suc <- unique(bdcDesem$SUCURSAL)
desemList <- list()
for (i in 1:length(suc)) {
  m <- bdcDesem %>% 
    dplyr::filter(SUCURSAL==suc[i])
  desemList[[i]] <- m
}
names(desemList) <- suc
write.xlsx(desemList,"D:/!bso/girCartera/desembolsados/Desembolsos_Sucursal_Abr2023.xlsx") 

reg <- unique(bdcDesem$REGIONAL)
desemList <- list()
for (i in 1:length(reg)) {
  m <- bdcDesem %>% 
    dplyr::filter(REGIONAL==reg[i])
  desemList[[i]] <- m
}
names(desemList) <- reg
write.xlsx(desemList,"D:/!bso/girCartera/desembolsados/Desembolsos_Regional_Abr2023.xlsx") 
