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
library(quantmod)
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(scales)
library(janitor)
library(ggplot2)

Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3","slateblue3",
                             "red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)

####____LECTURA DE BASE DE PAGOS____####
#Aquí leemos y resumimos los pagos por cliente
PagosNov <- fread('D:/!bso/bases/csv/Bases Transacciones 202211.csv',
                  encoding = "Latin-1",fill = T,sep=',') %>% 
  dplyr::filter(month(FechaPago)==11)  

PagosNov$codAsesorAsignacion <- as.character(PagosNov$codAsesorAsignacion)
PagosNov$codRegional <- as.character(PagosNov$codRegional)
PagosNov$codAgencia <- as.character(PagosNov$codAgencia)

Pagos <- fread('D:/!bso/bases/csv/PagosCarteraDesdeEne2022_Oct2022.csv',
                encoding = "Latin-1",fill = T,sep=',') %>% 
  dplyr::filter(month(FechaPago)!=11 &year(FechaPago)>=2022)

Pagos <- Pagos %>% bind_rows(PagosNov)

Pagos <- fread('D:/!bso/bases/csv/PagosCarteraEne22Dic22.csv',
               encoding = "Latin-1",fill = T,sep=',')

P2full <- Pagos %>% 
  #dplyr::filter(FechaPago>=as.Date('2022-01-30') & FechaPago<=as.Date('2022-01-31')) %>%
  mutate(mesPago = month(FechaPago)) %>% 
  mutate(dayPago = day(FechaPago)) %>% 
  mutate(yearPago = year(FechaPago)) %>% 
  select(Operacion, CapitalPagado, FechaPago, HoraPago, mesPago, dayPago, 
         yearPago, FechaPrevistaPago) %>% 
  group_by(Operacion, mesPago, yearPago) %>% 
  mutate(Hora_UltDia = max(HoraPago[which(dayPago==max(dayPago))])) %>% 
  summarise(HoraPago = max(HoraPago), Hora_UltDia = max(Hora_UltDia),
            dayPago = max(dayPago),FechaPrevistaPago = max(FechaPrevistaPago)) %>% 
  ungroup() %>% 
  dplyr::filter(yearPago == 2022)

write.csv(P2full, "D:/!bso/mph/202212_P2Full_Evol.csv",row.names = F)
P2full <- fread("D:/!bso/mph/202212_P2Full_Evol.csv",
                encoding = "Latin-1",fill = T,sep=',')
####____PAGOS TARDÍOS____####
#Aquí obtenemos a las operaciones con pagos extemporáneos por cada mes.
#Por pagos extemporáneos entendemos pago en último día después de mediodía.
P2uh <- P2full %>%
  mutate(hourUltDia = as.numeric(str_sub(Hora_UltDia, 1, 2))) %>% 
  group_by(mesPago) %>% 
  mutate(maxDia = max(dayPago)) %>% 
  mutate(maxDia = ifelse(mesPago==2,25,maxDia)) %>% 
  mutate(maxDia = ifelse(mesPago==12,29,maxDia)) %>% 
  ungroup() %>% 
  mutate(FechaPago = as.Date(paste(yearPago, mesPago, dayPago), format = '%Y%m%d')) %>% 
  mutate(Fecha = as.yearmon(FechaPago)) %>%
  mutate(appsU = case_when(mesPago==12 & dayPago>=maxDia & FechaPago > FechaPrevistaPago &
                             hourUltDia >=12~1,
                           mesPago!=12 & dayPago==maxDia & FechaPago > FechaPrevistaPago &
                             hourUltDia>=12~1,TRUE~0)) %>% 
  # mutate(appsU = ifelse(dayPago == maxDia & FechaPago > FechaPrevistaPago & 
  #                         hourUltDia >= 12, 1, 0)) %>% 
  dplyr::filter(appsU==1)

####____JOIN girCartera CIERRE MES CON OPERACIONES TARDÍAS____####
agen <- read.csv("D:/!bso/bases/csv/codigos_agencia.csv")

lab_mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
years <- c("2022")
n <- length(years)

bdcList <- list()
for (k in 1:length(years)) {
  for (i in 1:length(lab_mes)) {
    tryCatch({
      print(paste0(lab_mes[i],years[k]))
      Cierre <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',lab_mes[i],years[k],'.rds')) %>% 
        left_join(agen,by="AGENCIA") %>% 
        dplyr::filter(MODULO != 131) %>%
        dplyr::filter(ESTADO != 'CASTIGADA') %>%
        mutate(fdes = dmy(FDESEMBOLSO)) %>%
        rename(Operacion=OPERACION) %>% 
        mutate(Reprogramado=ifelse(saldoReprog>0,1,0)) %>% 
        mutate(Refinanciado=ifelse(saldoRef>0,1,0)) %>% 
        mutate(Diferido=ifelse(saldoDif>0,1,0)) %>% 
        mutate(Periodo_Gracia=ifelse(GRACIA_MESES>0,1,0)) %>% 
        mutate(Rango_Desembolso = ifelse(montous > 20000, '20K+', 'under20K')) %>% 
        mutate(Fecha=as.yearmon(paste0(lab_mes[i],". ",years[k]))) %>% 
        group_by(Fecha) %>% 
        mutate(saldoTot=sum(saldous),
               par0Tot=sum(par0),
               par30Tot=sum(par30)) %>% 
        ungroup() %>% 
        select(Fecha,Operacion,sucursal,NOMBRE_AGENCIA,NOMBRE_ASESOR,tipoCred, FDESEMBOLSO,
               labGrupoC,labGrupoD,ESTADO,DIASMORA, Rango_Desembolso,rangom,previus,
               opTot,saldous,saldoMora,par0,par30,Reprogramado, Refinanciado, 
               Diferido, Periodo_Gracia,ctaCont, saldoTot,par0Tot,par30Tot) %>% 
        left_join(P2uh, by=c("Operacion","Fecha")) %>% 
        mutate(appsU=ifelse(is.na(appsU),0,appsU)) %>% 
        dplyr::filter(appsU==1) %>% 
        mutate(Tipo_Cartera=case_when(ctaCont=="131"~"Vigente",
                                      ctaCont=="133"~"Vencida",
                                      ctaCont=="134"~"Ejecución",
                                      ctaCont=="135"~"Repro Vigente",
                                      ctaCont=="136"~"Repro Vencida",
                                      ctaCont=="137"~"Repro Ejecución"))
      
      bdcList[[(k-1)*n+i]] <- Cierre
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

bdcFinal <- bind_rows(bdcList)

write.csv(bdcFinal,"D:/!bso/mph/Oreports/bdcEvolEne22Dic22_293031.csv",row.names = F)
####____SALDO TARDIO EVOLUTIVO____####
bdcFinal <- read.csv("D:/!bso/mph/Oreports/bdcEvolEne22Dic22.csv") %>% 
  mutate(Fecha=as.yearmon(Fecha))
bdcProc <- bdcFinal %>% 
  select(Fecha,saldous,opTot) %>% 
  group_by(Fecha) %>% 
  summarise_all(sum) %>% 
  ungroup()

scale_fac <- max(bdcProc$saldous)/max(bdcProc$opTot)
ggplot(bdcProc,aes(x=Fecha,y=saldous))+
  geom_line(color=paleta(12)[2],size=1.5,group=1)+
  geom_line(aes(y=opTot*scale_fac),color=paleta(12)[8],size=1.5)+
  labs(x="Mes",y="Saldo tardío en USD")+
  scale_y_continuous(breaks=seq(0,30e7,5e6),labels = scales::comma,
                     sec.axis = sec_axis(~./scale_fac,name="Nro Operaciones",
                                         breaks = seq(0,7000,950)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 0.5),
        axis.text.y.left = element_text(color=paleta(8)[2]),
        axis.text.y.right = element_text(color=paleta(8)[6]))
####____SALDO MORA POTENCIAL EVOLUTIVO____####
bdcProc <- bdcFinal %>% 
  select(Fecha,saldous,par0,opTot) %>% 
  group_by(Fecha) %>% 
  summarise_all(sum) %>% 
  ungroup()

scale_fac <- max(bdcProc$saldous)/max(bdcProc$opTot)
ggplot(bdcProc,aes(x=Fecha,y=saldous))+
  geom_line(color=paleta(12)[2],size=1.5,group=1)+
  geom_line(aes(y=opTot*scale_fac),color=paleta(12)[8],size=1.5)+
  labs(x="Mes",y="Saldo tardío en USD")+
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~./scale_fac,name="Nro Operaciones"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 0.5),
        axis.text.y.left = element_text(color=paleta(8)[2]),
        axis.text.y.right = element_text(color=paleta(8)[6]))

bdcProc <- bdcFinal %>% 
  select(Fecha,sucursal,saldous,opTot) %>% 
  group_by(Fecha,sucursal) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  arrange(desc(saldous)) %>% 
  mutate(pos = row_number()) %>% 
  ungroup() %>% 
  mutate(sucursal=ifelse(sucursal %in% sucursal[pos>4 & Fecha=="nov. 2022"],"Otros",sucursal)) %>% 
  group_by(Fecha,sucursal) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(Fecha, sucursal) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>% 
  ungroup()
    
  
ggplot(bdcProc,aes(x=Fecha,y=pct,fill=sucursal))+
  geom_col()+
  geom_text(aes(label=round(pct,1),group=sucursal),color="white",
            size=2.5,position = position_stack(vjust = .5))+
  labs(x="Mes",y="Participación en saldo tardío (%)",
       fill="Regional")+
  scale_fill_manual(values = paleta(5))+
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust =0.5, hjust = 0.5),
        axis.text.y.left = element_text(color=paleta(8)[2]))
  
bdcProc <- bdcFinal %>% 
  dplyr::filter(sucursal=="La Paz" & !str_detect(NOMBRE_AGENCIA,"Normalizadora")) %>% 
  select(Fecha,NOMBRE_AGENCIA,saldous,opTot) %>% 
  group_by(Fecha,NOMBRE_AGENCIA) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  arrange(desc(saldous)) %>% 
  mutate(pos = row_number()) %>% 
  ungroup() %>%
  mutate(NOMBRE_AGENCIA = 
           ifelse(NOMBRE_AGENCIA %in% NOMBRE_AGENCIA[pos>9 & Fecha=="nov. 2022"],"Otros",NOMBRE_AGENCIA)) %>% 
  group_by(Fecha,NOMBRE_AGENCIA) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>% 
  ungroup()

ord <- bdcProc %>%
  dplyr::filter(Fecha=="Nov. 2022") %>% 
  arrange(saldous) %>% 
  select(NOMBRE_AGENCIA)

ggplot(bdcProc[bdcProc$Fecha!="Feb. 2022",],aes(x=as.factor(Fecha),y=pct,fill=factor(NOMBRE_AGENCIA,levels = ord$NOMBRE_AGENCIA),
                   group=factor(NOMBRE_AGENCIA,levels = ord$NOMBRE_AGENCIA)))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pct,1)),color="white",
            size=2.5,position = position_stack(vjust = .5))+
  labs(x="Mes",y="Saldo en Mora en MM USD",fill="Agencia")+
  scale_fill_manual(values = paleta(10))+
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust =0.5, hjust = 0.5),
        axis.text.y.left = element_text(color=paleta(8)[2]))


bdcProc <- bdcEvol %>% 
  mutate(fdesbin = cut(as.Date(FDESEMBOLSO,"%d/%m/%y"),
                       c(as.Date("2010-01-01"),seq.Date(as.Date("2020-01-01"),as.Date("2022-01-31"),by="6 months"),as.Date("2022-12-30")),
                       c("2010-2020","I/2020","II/2020","I/2021","II/2021","2022"))) %>% 
  select(Fecha,fdesbin,opTot) %>% 
  group_by(Fecha,fdesbin) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  mutate(pct=opTot/sum(opTot)*100) %>% 
  ungroup()
  
ord <- bdcProc %>%
  dplyr::filter(Fecha=="Nov. 2022") %>% 
  arrange(opTot) %>% 
  select(fdesbin)

ggplot(bdcProc[bdcProc$Fecha!="Feb. 2022",],aes(x=as.factor(Fecha),y=pct,fill=factor(fdesbin,levels = ord$fdesbin),
                                                group=factor(fdesbin,levels = ord$fdesbin)))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pct,1)),color="white",
            size=2.5,position = position_stack(vjust = .5))+
  labs(x="Mes",y="Saldo en Mora en MM USD",fill="Agencia")+
  scale_fill_manual(values = paleta(10))+
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust =0.5, hjust = 0.5),
        axis.text.y.left = element_text(color=paleta(8)[2]))

####___OTROS___####
ggplot(bdcFinal,aes(x=fdesbin))+
  geom_bar(stat = "count")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust =0.5, hjust = 0.5))
