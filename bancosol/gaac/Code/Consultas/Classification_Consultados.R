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
require(XLConnect)
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
makedummies <- function(x,var,VAR){
  values <- unique(x[[var]])
  x <- x %>% 
    mutate(NEW=0)
  for (i in 1:length(values)) {
    x <- x %>% 
      mutate(NEW=ifelse({{VAR}}==values[i],i,NEW))
  }
  x <- x %>% 
    select(-{{VAR}})
  return(x)
}
####____LECTURA DE CONSOLIDADO DE CONSULTAS####
bdcCon1xmes <- readRDS('D:/!bso/Consultas/Consultas1XMesNov20Ene23.rds') %>% 
  glimpse()

check1xmes <- bdcCon1xmes %>% 
  group_by(myCon) %>% 
  summarise(saldoCon=sum(saldous),nOpsCon=n()) %>% 
  ungroup() %>%
  mutate(myCancel=myCon) %>% 
  select(-myCon) %>% 
  glimpse()

bdcConsultada <- bdcCon1xmes %>% 
  rename(saldoConsulta=saldous) %>% 
  select(CI,OPERACION,CTACLIENTE,myCon,MONTOUS,MONEDA,CALIFICACION,tipoCred,
         ENTIDAD,grupo_Entidad,sucursal,rangos,DIASMORA,MONTOUS,saldoConsulta)
####____LECTURA DE CANCELADOS CONSULTADOS____####
bdcCanLastCon <- readRDS('D:/!bso/Consultas/CanLastConEne21Ene23.rds') %>% 
  glimpse()

bdcCanCon <- bdcCanLastCon %>% 
  select(CI,OPERACION,CTACLIENTE,myCon,tipoCancel,Consultado,saldoCancel) %>% 
  rename(CancelAmort = Consultado)

Cruce <- bdcConsultada %>% 
  left_join(bdcCanCon,by=c("CI","OPERACION","CTACLIENTE","myCon")) %>% 
  mutate(Ratio=ifelse(MONTOUS!=0,saldoConsulta/MONTOUS,0)) %>% 
  replace_na(list(CancelAmort=0,tipoCancel="Sin cancel/amort"))
####____LAST CANCEL AMORT____####
Cruce2 <- Cruce %>% 
  dummy_cols(select_columns = c("sucursal","tipoCred","grupo_Entidad","rangos"),remove_selected_columns = TRUE) %>% 
  select(-tipoCancel,-DIASMORA,-ENTIDAD,-CALIFICACION,-MONEDA,-MONTOUS,-myCon,-CTACLIENTE,-OPERACION,-CI)

Cruce2 %>% 
  group_by(CancelAmort) %>% 
  summarise_all(sum,na.rm=T)
####____CRUCE CON PAGOS TARDIOS____####
lastCierre <- fread("D:/!bso/mph/Oreports/lastCierrreUR_Dic2022.csv",
                    encoding = "UTF-8",sep=",",fill=T) %>% 
  glimpse()

table(lastCierre$Instancias_AR,useNA = "ifany")
####___CRUCE CON VIPS____####
# cc <- readRDS('D:/!bso/bases/rds/tablacc.rds')
vipDesc <- read.xlsx('D:/!bso/bases/excel/vipDescriptives_CompleteList_ene2023.xlsx') 
VIPC <- vipDesc %>% 
  select(OPERACION,CI,VIP_RC,Lista_GNN,VIP_Viable,) %>% 
  mutate(VIP_RC=1)
CruceVIP <- Cruce %>% 
  left_join(VIPC,by=c("OPERACION","CI")) %>% 
  replace_na(list(VIP_RC=0,Lista_GNN="No Aplica",VIP_Viable="No Aplica"))


table(CruceVIP$CancelAmort,CruceVIP$VIP_Viable)
table(CruceVIP$CancelAmort,CruceVIP$VIP_RC)
table(CruceVIP$CancelAmort,CruceVIP$rangos)

####___NAIVE BAYES____####
agrupar <- function(x,VAR){
  y <- x %>% 
    group_by(myCon) %>% 
    mutate(pct=1/n()*100) %>% 
    group_by(myCon,{{VAR}}) %>% 
    summarise(pctn=sum(pct),nOps=n()) %>% 
    ungroup() %>% 
    mutate(ORDEN = ifelse({{VAR}} %in% {{VAR}}[pctn>=7 & myCon=='Ene. 2023'],{{VAR}},'Otros')) %>% 
    mutate(ORDEN = fct_reorder(factor(ORDEN),pctn)) %>% 
    group_by(myCon,ORDEN) %>% 
    summarise(pctn=sum(pctn),nOps=sum(nOps))
  z <- y %>%
    group_by(myCon) %>% 
    summarise(pctn=sum(pctn),nOps=sum(nOps))
  result <- list(y=y,z=z)
  return(result)
}

shortCateg <- function(x,VAR,q){
  x %>% 
    group_by(myCon) %>% 
    mutate(pct=1/n()*100) %>% 
    group_by(myCon,{{VAR}}) %>% 
    mutate(pctn=sum(pct)) %>% 
    ungroup() %>% 
    mutate(ORDEN = ifelse({{VAR}} %in% {{VAR}}[pctn>=7 & myCon=='Ene. 2023'],{{VAR}},'Otros')) %>% 
    select(-pct,-pctn,-{{VAR}}) %>% 
    rename({{VAR}}:=ORDEN)
}
Cruce1 <- CruceVIP %>% 
  select(-OPERACION) %>% 
  shortCateg(ENTIDAD,q="ENTIDAD")
Cruce1 <- CruceVIP %>% 
  select(CancelAmort,saldoConsulta,MONTOUS,Lista_GNN,sucursal,ENTIDAD,VIP_RC,VIP_Viable,
         tipoCred,rangos)
model <- naiveBayes(CancelAmort~.,data = Cruce1)
table(predict(model,Cruce1),Cruce1$CancelAmort)
