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
####____READING CONSOLIDADO DE CONSULTAS____####
conL <- readRDS('D:/!bso/Consultas/ConsultasFullEne17Ene23.rds') %>% 
  dplyr::filter(myCon>="feb. 2022")
vip <- read_excel('D:/!bso/Consultas/VIP/AllLeads_final_ene2023.xlsx')
agen <- read.csv('D:/!bso/bases/csv/codigos_agencia.csv')

bdcVIP <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Ene2023.rds') %>% 
  select(OPERACION,CI,cosechaY,cosechaM) %>% 
  left_join(vip,by=c("OPERACION","CI")) %>% 
  left_join(agen,by="AGENCIA") %>% 
  select(-AGENCIA) %>% 
  dplyr::filter(!is.na(VIP_RC))

####____JOINING VIP CON CONSULTADOS____####
vipCon <- bdcVIP %>%
  left_join(conL,by="CI") %>% 
  dplyr::filter(!is.na(Consultado))

#Recuento de VIP's en lista consultados en los últimos 12 meses
vipCon %>% 
  group_by(myCon) %>% 
  tally()

#Conteo de consultas por CI al mes y en los útlimos 12 meses
vipCon <- vipCon %>%
  mutate(ENTIDAD = gsub(' SA', ' S.A.', ENTIDAD)) %>% 
  group_by(CI,myCon,ENTIDAD) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% #Nos quedamos con una consulta por Entidad al mes
  ungroup() %>% 
  group_by(CI,myCon) %>% 
  mutate(Consulta_mes=n()) %>% #Contamos el nro de consultas recibidas cada mes por entidades distintas
  ungroup() %>% 
  group_by(CI) %>% 
  mutate(Consultas12mes=n()) %>% #Contamos el nro de consultas recibidas en los 12 meses anteriores
  ungroup()

write_rds(vipCon,'D:/!bso/Consultas/VIP/vipsConsultadosEne23.rds')
tablas <- list(vipsCon = vipCon, vips = bdcVIP)
write.xlsx(tablas,'D:/!bso/Consultas/VIP/tablasVIPCONEne23.xlsx')
####____LECTURA DE VIPs CONSULTADOS____####
vipCon <- readRDS('D:/!bso/Consultas/VIP/vipsConsultadosEne23.rds')

#Recuento de VIP consultados (1 vez por entidad al mes)
vipCon %>% 
  group_by(myCon) %>% 
  tally()
####____EXPLORACION DE INTERACCIONES____####
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
tc <- vipCon %>% 
  group_by(myCon) %>% 
  mutate(pct=Consultado/sum(Consultado)*100) %>% 
  group_by(myCon,ENTIDAD) %>%
  summarise(pctn=sum(pct),nOps=sum(Consultado)) %>% 
  ungroup() %>% 
  mutate(ORDEN=ifelse(ENTIDAD %in% ENTIDAD[pctn>=6 & myCon=='Ene. 2023'],ENTIDAD,'Otros')) %>% 
  mutate(ORDEN=fct_reorder(factor(ORDEN),pctn)) %>% 
  group_by(myCon,ORDEN) %>% 
  summarise(pctn=sum(pctn),nOps=sum(nOps))

#GRAFICO POR SUCURSAL
tc <- agrupar(vipCon,Sucursal)
ggplot(tc$y,aes(x=myCon,y=nOps,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(nOps,2)),size=3,color="white",
            position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=tc$z$myCon,y=tc$z$nOps*1.05,
           label=round(tc$z$nOps,2),size=3.5,color=paleta(12)[7])+
  labs(x="Mes de consulta",y="Consultas a VIPs de Enero 2023",
       fill="Sucursal")+
  scale_fill_manual(values=paleta(12))+
  scale_x_continuous(breaks = as.numeric(tc$y$myCon),labels = format(tc$y$myCon,"%m/%y"))+
  theme_minimal()+
  theme(legend.position = "bottom")
#GRAFICO POR RANGO_MONTO
tc <- agrupar(vipCon,Rango_Monto)
ggplot(tc$y,aes(x=myCon,y=nOps,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(nOps,2)),size=3,color="white",
            position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=tc$z$myCon,y=tc$z$nOps*1.05,
           label=round(tc$z$nOps,2),size=3.5,color=paleta(12)[7])+
  labs(x="Mes de consulta",y="Consultas a VIPs de Enero 2023",
       fill="Rango Monto")+
  scale_fill_manual(values=paleta(12))+
  scale_x_continuous(breaks = as.numeric(tc$y$myCon),labels = format(tc$y$myCon,"%m/%y"))+
  theme_minimal()+
  theme(legend.position = "bottom")

#GRAFICO POR ENTIDAD
tc <- agrupar(vipCon,ENTIDAD)
ggplot(tc$y,aes(x=myCon,y=nOps,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(nOps,2)),size=3,color="white",
            position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=tc$z$myCon,y=tc$z$nOps*1.05,
           label=round(tc$z$nOps,2),size=3.5,color=paleta(12)[7])+
  labs(x="Mes de consulta",y="Consultas a VIPs de Enero 2023",
       fill="Entidad")+
  scale_fill_manual(values=paleta(12))+
  scale_x_continuous(breaks = as.numeric(tc$y$myCon),labels = format(tc$y$myCon,"%m/%y"))+
  theme_minimal()+
  theme(legend.position = "bottom")

#GRAFICO POR COSECHAY
tc <- agrupar(vipCon,cosechaY)
ggplot(tc$y,aes(x=myCon,y=nOps,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(nOps,2)),size=3,color="white",
            position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=tc$z$myCon,y=tc$z$nOps*1.05,
           label=round(tc$z$nOps,2),size=3.5,color=paleta(12)[7])+
  labs(x="Mes de consulta",y="Consultas a VIPs de Enero 2023",
       fill="Año de desembolso")+
  scale_fill_manual(values=paleta(12))+
  scale_x_continuous(breaks = as.numeric(tc$y$myCon),labels = format(tc$y$myCon,"%m/%y"))+
  theme_minimal()+
  theme(legend.position = "bottom")

x <- sum(tc$nOps[(tc$Rango_Monto %in% c('6. 15k-20k','5. 10k-15k')) & tc$myCon=='ene. 2023'])
n <- sum(tc$nOps[tc$myCon=='ene. 2023'])
binom.test(x,n,0.54,alternative = "greater")
prop.test(x = c(940,19915),n = c(1487,36324),alternative = "less")

t2 <- bdcVIP %>% 
  mutate(pct=1/n()*100) %>% 
  group_by(Rango_Monto) %>% 
  summarise(pctn=sum(pct),nOps=n()) %>% 
  ungroup() %>% 
  mutate(ORDEN=ifelse(Rango_Monto %in% Rango_Monto[pctn>=6],Rango_Monto,'Otros')) %>% 
  mutate(ORDEN=fct_reorder(factor(ORDEN),pctn))
