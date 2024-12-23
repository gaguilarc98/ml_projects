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
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____LECTURA DE BASE____####
bdcCon <- fread("D:/!bso/Consultas/bdcConsultasJul22Dic22.csv",
                encoding = "UTF-8",fill = T,sep=",")

####____GRAFICAS DESCRIPTIVAS____####

bdcCon01 <- bdcCon %>% 
  group_by(sucursal) %>% 
  dplyr::filter(Consultado==1) %>% 
  summarise(n=n(), NAsesor=length(unique(NOMBRE_ASESOR)),
            NAsesorProm = n()/length(unique(NOMBRE_ASESOR))) %>% 
  ungroup()

scalefac <- max(bdcCon01$n)/max(bdcCon01$NAsesorProm)
ggplot(bdcCon01,aes(x=sucursal,y=n)) +
  geom_col(fill="red") +
  geom_text(aes(label=comma(n)),color="black",position = position_stack(vjust = .75))+
  geom_point(aes(y=NAsesorProm*scalefac),col="blue")+
  labs(x="Sucursal",y="Número de operaciones consultadas",
       title = "Operaciones consultadas y número de operaciones consultadas por asesor")+
  scale_y_continuous(labels = scales::comma,sec.axis = sec_axis(~./scalefac,name="Op por asesor",
                                                                breaks = seq(0,230,26)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45,vjust=0.75,hjust=0.5),
        plot.title = element_text(size=11,face = "bold"))

bdcCon02 <- bdcCon %>% 
  dplyr::filter(Consultado==1) %>% 
  mutate(mesano=as.yearmon(as.Date(paste("01",MesCierre,yearCierre,sep = "-"),"%d-%m-%y"))) %>% 
  group_by(mesano, sucursal) %>% 
  summarise(n=n(), NAsesor=length(unique(NOMBRE_ASESOR))) %>% 
  ungroup() %>% 
  group_by(mesano) %>% 
  mutate(pct=n/sum(n)*100) %>% 
  arrange(desc(pct)) %>% 
  mutate(pos=row_number()) %>% 
  mutate(sucursal=ifelse(sucursal %in% sucursal[pos>4],"Otros",sucursal)) %>% 
  ungroup() %>% 
  group_by(mesano,sucursal) %>% 
  summarise_all(sum) %>% 
  mutate(NAsesorProm = n/NAsesor) %>% 
  ungroup()

bdcCon02 %>% 
  mutate(sucursal=fct_reorder(sucursal,pct)) %>% 
  ggplot(aes(x=as.factor(mesano),y=pct,fill=sucursal))+
  geom_bar(stat = "identity") + 
  geom_text(aes(label=round(pct,1)),color="white", size=2.5,
            position = position_stack(vjust=0.5))+
  labs(x = "Mes", y="Participación en porcentaje (%)",
       title = "Participación en Consultas por Sucursal",fill="Sucursal")+
  scale_fill_manual(values=paleta(5)[5:1]) +
  scale_x_discrete(labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"))+
  theme_minimal()+
  theme(plot.title = element_text(size=11,face="bold"),
        legend.position = "bottom")

bdcCon03 <- bdcCon %>% 
  dplyr::filter(Consultado==1) %>% 
  mutate(mesano=as.yearmon(as.Date(paste("01",MesCierre,yearCierre,sep = "-"),"%d-%m-%y"))) %>% 
  group_by(mesano,tipoCred) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(mesano) %>% 
  mutate(pct=n/sum(n)*100)

bdcCon03 %>% 
  mutate(tipoCred=fct_reorder(tipoCred,pct)) %>% 
  ggplot(aes(x=as.factor(mesano),y=pct,fill=tipoCred)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label=round(pct,1)),color="white", size=2.5,
            position = position_stack(vjust=0.5))+
  labs(x = "Mes", y="Participación en porcentaje (%)",
       title = "Participación en Consultas por Tipo de Crédito",fill="Tipo Entidad")+
  scale_fill_manual(values=paleta(4)[4:1]) +
  scale_x_discrete(labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"))+
  theme_minimal()+
  theme(plot.title = element_text(size=11,face="bold"),
        legend.position = "bottom")

bdcCon06 <- bdcCon %>% 
  dplyr::filter(Consultado==1) %>%
  dplyr::filter(CALIFICACION!="A") %>% 
  mutate(mesano=as.yearmon(as.Date(paste("01",MesCierre,yearCierre,sep = "-"),"%d-%m-%y"))) %>% 
  group_by(mesano,CALIFICACION) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(mesano) %>% 
  mutate(pct=n/sum(n)*100)

bdcCon04 <- bdcCon %>% 
  dplyr::filter(Consultado==1) %>% 
  dplyr::filter(DIASMORA>0) %>% 
  mutate(mesano=as.yearmon(as.Date(paste("01",MesCierre,yearCierre,sep = "-"),"%d-%m-%y"))) %>% 
  group_by(mesano) %>% 
  summarise(n=n()) %>% 
  ungroup() 

bdcCon04 %>% 
  ggplot(aes(x=mesano,y=n)) + 
  geom_line(color=paleta(12)[1],size=1.5)+
  scale_x_continuous(breaks = as.numeric(bdcCon04$mesano),labels=format(bdcCon04$mesano,"%b"))+
  labs(x="Mes",y="Número de consultas con días de mora",
       title="Operaciones consultadas con días de mora al cierre mensual previo")+
  theme_minimal()+
  theme(plot.title = element_text(size=11,face="bold"))

bdcCon05 <- bdcCon %>% 
  dplyr::filter(Consultado==1) %>% 
  mutate(mesano=as.yearmon(as.Date(paste("01",MesCierre,yearCierre,sep = "-"),"%d-%m-%y"))) %>% 
  group_by(mesano,grupo_Entidad) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(mesano) %>% 
  mutate(pct=n/sum(n)*100)

bdcCon05 %>% 
  mutate(grupo_Entidad=fct_reorder(grupo_Entidad,pct)) %>% 
  ggplot(aes(x=as.factor(mesano),y=pct,fill=grupo_Entidad)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label=round(pct,1)),color="white", size=2.5,
            position = position_stack(vjust=0.5))+
  labs(x = "Mes", y="Participación en porcentaje (%)",
       title = "Participación en Consultas por Tipo de Entidad",fill="Tipo Entidad")+
  scale_fill_manual(values=paleta(5)[5:1]) +
  scale_x_discrete(labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"))+
  theme_minimal()+
  theme(plot.title = element_text(size=11,face="bold"),
        legend.position = "bottom")

bdcCon06 <- bdcCon %>% 
  dplyr::filter(Consultado==1) %>%
  dplyr::filter(CALIFICACION!="A") %>% 
  mutate(mesano=as.yearmon(as.Date(paste("01",MesCierre,yearCierre,sep = "-"),"%d-%m-%y"))) %>% 
  group_by(mesano,CALIFICACION) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(mesano) %>% 
  mutate(pct=n/sum(n)*100)

bdcCon06 %>% 
  ggplot(aes(x=as.factor(mesano),y=pct,fill=CALIFICACION)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label=round(pct,1)),color="white", size=2.5,
            position = position_stack(vjust=0.5))+
  labs(x = "Mes", y="Participación en porcentaje (%)",
       title = "Participación en Consultas por Calificación (omitiendo A)",fill="Calificación")+
  scale_fill_manual(values=paleta(5)[5:1]) +
  scale_x_discrete(labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"))+
  theme_minimal()+
  theme(plot.title = element_text(size=11,face="bold"),
        legend.position = "bottom")

#El análisis con DIASMORA no es interesante, pues el número de
#Operaciones consultadas con dias de mora es insignificante.