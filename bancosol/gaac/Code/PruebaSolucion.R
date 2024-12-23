####SOLUCION PRUEBA TECNICA ANALISTA MODELOS CUANTITATIVOS DE RIESGOS
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
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####___LECTURA DE BASES DE DATOS____####
banco <- fread("D:/OneDrive - Banco Solidario SA/Postulantes/ENRIQUE LAUREL/BDC_MiBanco_Ene22_May22.csv",
                      encoding = "UTF-8",fill=T,sep=",")
sistema_ini <- fread("D:/OneDrive - Banco Solidario SA/Postulantes/ENRIQUE LAUREL/Info_SF_Ene22_May22.csv",
                      encoding = "UTF-8",fill=T,sep=",") %>% 
  select(-CALIFICACION)
####____INSTRUCCIONES____####
#1
banco <- banco %>% 
  dplyr::filter(CALIFICACION != "") %>% 
  dplyr::filter(FECHA %in% c(202201,202202,202203,202204,202205)) %>% 
  dplyr::filter(ESTADO != "")
#2 y 3
sistema <- sistema_ini %>% 
  dplyr::filter(TIPO_DEUDOR=="Deudor") %>%
  dplyr::filter(PEOR_CALIF != "_")
#4
join <- banco %>% 
  inner_join(sistema, by=c("ID","OPERACION","FECHA"), suffix=c("_MB","_SF"))
#5
join %>% 
  group_by(FECHA) %>% 
  summarise(across(c(SALDO_MB,SALDO_SF),~sum(.x)))
#6 
Cliente <- join %>% 
  group_by(FECHA,ID) %>% 
  summarise(across(c(SALDO_MB,SALDO_NO_BAN),~sum(.x)),
            across(c(CALIFICACION,PEOR_CALIF),~max(.x))) %>% 
  ungroup() %>% 
  mutate(ym = as.Date(paste0(FECHA,'01'),format="%Y%m%d")) %>%
  mutate(monDate=as.yearmon(ym)) %>% 
  mutate(across(c(SALDO_MB,SALDO_NO_BAN),~.x/1e6)) 

#7
CalifEvol <- Cliente %>% 
  group_by(monDate) %>% 
  summarise(SALDO_MB = sum(SALDO_MB),
            SALDO_NO_BAN = sum(SALDO_NO_BAN),
            NOps = n_distinct(ID)) %>% 
  ungroup() %>% 
  mutate(labSaldo = paste(round(SALDO_MB,1)," MM USD")) %>% 
  mutate(labSaldoSF = paste(round(SALDO_NO_BAN,1)," MM USD")) %>% 
  mutate(labOps = comma(round(NOps)))

scalefac <- max(CalifEvol$SALDO_MB)/max(CalifEvol$NOps)
ggplot(CalifEvol,aes(x=monDate,y=SALDO_MB))+
  geom_bar(stat = "identity",fill="midnightblue")+
  geom_text(aes(label=labSaldo),position = position_stack(vjust=0.5),color="white",angle=90)+
  geom_line(aes(y=NOps*scalefac),size=1.5,color="orange")+
  geom_label(aes(label=labOps,y=NOps*scalefac),fill="orange")+
  # geom_label_repel(aes(label=lab),position = position_stack(vjust = 0.5))+
  labs(x="Mes de cierre",y="Saldo en SF (en MM USD)")+
  scale_y_continuous(breaks=seq(0,250,50),labels = scales::comma,sec.axis = 
                       sec_axis(~./scalefac,name="Número de clientes"))+
  # scale_x_continuous(breaks = as.numeric(basePlot$monDate),labels = format(basePlot$monDate,"%m/%y"))+
  theme_light()+
  theme(text = element_text(face = "bold"),
        axis.text.y.left = element_text(color="midnightblue"),
        axis.text.y.right = element_text(color="orange"),
        axis.title.y.left = element_text(color="midnightblue"),
        axis.title.y.right = element_text(color="orange"))
#8
scalefac <- max(CalifEvol$SALDO_NO_BAN)/max(CalifEvol$NOps)
ggplot(CalifEvol,aes(x=monDate,y=SALDO_NO_BAN))+
  geom_bar(stat = "identity",fill="midnightblue")+
  geom_text(aes(label=labSaldoSF),position = position_stack(vjust=0.5),color="white",angle=90)+
  geom_line(aes(y=NOps*scalefac),size=1.5,color="orange")+
  geom_label(aes(label=labOps,y=NOps*scalefac),fill="orange")+
  # geom_label_repel(aes(label=lab),position = position_stack(vjust = 0.5))+
  labs(x="Mes de cierre",y="Saldo en SF (en MM USD)")+
  scale_y_continuous(breaks=seq(0,700,100),labels = scales::comma,sec.axis = 
                       sec_axis(~./scalefac,name="Número de clientes"))+
  # scale_x_continuous(breaks = as.numeric(basePlot$monDate),labels = format(basePlot$monDate,"%m/%y"))+
  theme_light()+
  theme(text = element_text(face = "bold"),
        axis.text.y.left = element_text(color="midnightblue"),
        axis.text.y.right = element_text(color="orange"),
        axis.title.y.left = element_text(color="midnightblue"),
        axis.title.y.right = element_text(color="orange"))

#10
Cliente %>% 
  dplyr::filter(monDate=="May. 2022") %>% 
  group_by(CALIFICACION,PEOR_CALIF) %>% 
  summarise(NOps=n_distinct(ID)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "PEOR_CALIF",values_from = NOps,values_fill = 0)
