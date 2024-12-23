#El objetivo de este script es, con las bases de cajas de ahorro, agregar categorías para personas naturales para desde 2022 y agruparlas con las personas jurídicas
library(data.table)
library(dplyr)
library(readr)
library(openxlsx)
library(ggplot2)
library(stringr)
setwd("\\\\VFSNALSRV\\RiesgoCrediticioOFN\\Bases_Riesgos\\03_Base_Capataciones_CAH_Diario")
a1<-dir()





cah1<-fread(a1[88], sep="|")
#aux<-substr(a1[88],20,27)

#cah$saldo_20220101<-cah$SALDO_SUS
#cah$tasa_20220101<-cah$TASA_REFERENCIAL
cah1$saldo<-as.character(cah1$SALDO_SUS)
cah1$saldo<-str_replace_all(cah1$saldo,",",".")
cah1$saldo<-as.numeric(cah1$saldo)
cah1$SALDO_SUS<-cah1$saldo
cah1$tasa<-as.character(cah1$TASA_REFERENCIAL)
cah1$tasa<-str_replace_all(cah1$tasa,",",".")
cah1$tasa<-as.numeric(cah1$tasa)
cah1$CTA_CLIENTE<-as.character(cah1$CTA_CLIENTE)
cah1$CTA_CONTABLE_SALDO<-as.character(cah1$CTA_CONTABLE_SALDO)
cah1$nombre2<-str_replace_all(cah1$NOMBRE,",",".")
cah1$tipoop<-str_replace_all(cah1$DESC_TIPO_OPERACION,",",".")
cah1$NOMBRE<-cah1$nombre2
cah1$DESC_TIPO_OPERACION<-cah1$tipoop
h1<-cah1 %>%
  select(NRO_CUENTA,CTA_CLIENTE,NOMBRE,TIPO_PERSONA,NOM_REGIONAL_ASOC,CTA_CONTABLE_SALDO,DESC_TIPO_OPERACION,MARCA_INSTITUCIONAL_FINANZAS,saldo,tasa)
cah1$ctacont<-as.character(cah1$CTA_CONTABLE_SALDO)
cah1$aux1<-substr(cah1$ctacont,1,3)
t1<-cah1 %>%
  filter(TIPO_PERSONA=="Persona Natural") %>%
  mutate(tipo_a="Persona Natural") %>%
  group_by(DESC_TIPO_OPERACION) %>%
  mutate(tipo_b=DESC_TIPO_OPERACION) %>%
  group_by(FECHA_SALDO,tipo_a,tipo_b,MONEDA) %>%
  summarise(suma=sum(SALDO_SUS))
t2<-cah1 %>%
  filter(TIPO_PERSONA=="Persona Jurídica" & MARCA_INSTITUCIONAL_FINANZAS=="S") %>%
  mutate(tipo_a="Persona Jurídica") %>%
  mutate(tipo_b="Institucional") %>%
  group_by(FECHA_SALDO,tipo_a,tipo_b,MONEDA) %>%
  summarise(suma=sum(SALDO_SUS))
t3<-cah1 %>%
  filter(TIPO_PERSONA=="Persona Jurídica" & MARCA_INSTITUCIONAL_FINANZAS!="S" & aux1=="235") %>%
  mutate(tipo_a="Persona Jurídica") %>%
  mutate(tipo_b="EIF") %>%
  group_by(FECHA_SALDO,tipo_a,tipo_b,MONEDA) %>%
  summarise(suma=sum(SALDO_SUS))
t4<-cah1 %>%
  filter(TIPO_PERSONA=="Persona Jurídica" & MARCA_INSTITUCIONAL_FINANZAS!="S" & aux1!="235") %>%
  mutate(tipo_a="Persona Jurídica") %>%
  mutate(tipo_b="Resto") %>%
  group_by(FECHA_SALDO,tipo_a,tipo_b,MONEDA) %>%
  summarise(suma=sum(SALDO_SUS))
t5<-bind_rows(t1,t2,t3,t4)
#89:573






###correr desde acá
h1<-readRDS("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/CaptacionesJoin/Cah_diario2.rds")
t5<-readRDS("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/CaptacionesJoin/cah_desc2.rds")
for (i in 175:573) {
  print(i)
cah1<-fread(a1[i], sep="|")
aux<-substr(a1[i],20,27)
cah1$saldo<-as.character(cah1$SALDO_SUS)
cah1$saldo<-str_replace_all(cah1$saldo,",",".")
cah1$saldo<-as.numeric(cah1$saldo)
cah1$SALDO_SUS<-cah1$saldo
cah1$tasa<-as.character(cah1$TASA_REFERENCIAL)
cah1$tasa<-str_replace_all(cah1$tasa,",",".")
cah1$tasa<-as.numeric(cah1$tasa)
cah1$CTA_CLIENTE<-as.character(cah1$CTA_CLIENTE)
cah1$CTA_CONTABLE_SALDO<-as.character(cah1$CTA_CONTABLE_SALDO)
cah1$nombre2<-str_replace_all(cah1$NOMBRE,",",".")
cah1$tipoop<-str_replace_all(cah1$DESC_TIPO_OPERACION,",",".")
cah1$NOMBRE<-cah1$nombre2
cah1$DESC_TIPO_OPERACION<-cah1$tipoop
h2<-cah1 %>%
  select(NRO_CUENTA,CTA_CLIENTE,NOMBRE,TIPO_PERSONA,NOM_REGIONAL_ASOC,CTA_CONTABLE_SALDO,DESC_TIPO_OPERACION,MARCA_INSTITUCIONAL_FINANZAS,saldo,tasa)
h1<-h1 %>%
  full_join(h2,by=c("NRO_CUENTA","CTA_CLIENTE","NOMBRE","TIPO_PERSONA","NOM_REGIONAL_ASOC","CTA_CONTABLE_SALDO","DESC_TIPO_OPERACION","MARCA_INSTITUCIONAL_FINANZAS"),suffix=c("",aux))
saveRDS(h1,"//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/CaptacionesJoin/Cah_diario2.rds")
cah1$ctacont<-as.character(cah1$CTA_CONTABLE_SALDO)
cah1$aux1<-substr(cah1$ctacont,1,3)
t1<-cah1 %>%
  filter(TIPO_PERSONA=="Persona Natural") %>%
  mutate(tipo_a="Persona Natural") %>%
  group_by(DESC_TIPO_OPERACION) %>%
  mutate(tipo_b=DESC_TIPO_OPERACION) %>%
  group_by(FECHA_SALDO,tipo_a,tipo_b,MONEDA) %>%
  summarise(suma=sum(SALDO_SUS))
t2<-cah1 %>%
  filter(TIPO_PERSONA=="Persona Jurídica" & MARCA_INSTITUCIONAL_FINANZAS=="S") %>%
  mutate(tipo_a="Persona Jurídica") %>%
  mutate(tipo_b="Institucional") %>%
  group_by(FECHA_SALDO,tipo_a,tipo_b,MONEDA) %>%
  summarise(suma=sum(SALDO_SUS))
t3<-cah1 %>%
  filter(TIPO_PERSONA=="Persona Jurídica" & MARCA_INSTITUCIONAL_FINANZAS!="S" & aux1=="235") %>%
  mutate(tipo_a="Persona Jurídica") %>%
  mutate(tipo_b="EIF") %>%
  group_by(FECHA_SALDO,tipo_a,tipo_b,MONEDA) %>%
  summarise(suma=sum(SALDO_SUS))
t4<-cah1 %>%
  filter(TIPO_PERSONA=="Persona Jurídica" & MARCA_INSTITUCIONAL_FINANZAS!="S" & aux1!="235") %>%
  mutate(tipo_a="Persona Jurídica") %>%
  mutate(tipo_b="Resto") %>%
  group_by(FECHA_SALDO,tipo_a,tipo_b,MONEDA) %>%
  summarise(suma=sum(SALDO_SUS))
t6<-bind_rows(t1,t2,t3,t4)
t5<-bind_rows(t5,t6)
saveRDS(t5,"//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/CaptacionesJoin/cah_desc2.rds")
}

#saveRDS(h1,"//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/CaptacionesJoin/Cah_diario2.rds")
#h1<-readRDS("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/CaptacionesJoin/Cah_diario2.rds")
# fwrite(h1,"//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/CaptacionesJoin/Cah_diario.csv",sep=",")
#saveRDS(t5,"//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/CaptacionesJoin/cah_desc2.rds")
#t5<-readRDS("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/CaptacionesJoin/cah_desc2.rds")
# fwrite(t5,"//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/CaptacionesJoin/Cah_diario.csv",sep=",")
