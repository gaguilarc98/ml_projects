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
library(ca)
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
####____READING CONSULTAS____####
####____SINGLE FILE____####
arch <- list.files("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/01_Consulta Ajena",
           pattern = "^VAR7.*.1222.xls$")
arch <- sort(arch)
conList <- list()
for (i in 1:length(arch)) {
  print(arch[i])
  con <- read_excel(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/01_Consulta Ajena/",
                           arch[i]), col_names = c("TIPO","NDOC","EXT","CLIENTE","FNAC","ENTIDAD","FCON"))
  conList[[i]] <- con
}

conL <- bind_rows(conList) %>% 
  dplyr::filter(TIPO=="CI") %>% 
  rename(CI=NDOC) %>% 
  mutate(grupo_Entidad= case_when(str_detect(ENTIDAD,'BANCO')~'Banco',
                                  str_detect(ENTIDAD,'COOPERATIVA')~'Cooperativa',
                                  str_detect(ENTIDAD,'IFD')~'ifd',
                                  str_detect(ENTIDAD,'FONDO')~'Fondo',
                                  TRUE~'Otros')) %>% 
  mutate(FechaCon = as.Date(FCON)) %>%
  mutate(DiaCon = day(FCON)) %>% 
  mutate(MesCon = month(FCON)) %>% 
  mutate(YearCon = year(FCON)) %>% 
  mutate(HoraCon = hour(FCON)) %>% 
  mutate(Consultado = 1)

Control <- conL %>% 
  group_by(YearCon,MesCon) %>% 
  summarise(nArch=n())

####____PARA VARIOS MESES____####
mes <- c("Dic","Nov","Oct","Sep","Ago","Jul","Jun","May","Abr","Mar","Feb","Ene")
nmes <- c("12","11","10","09","08","07","06","05","04","03","02","01")
year <- c(22,21)
#year <- c(22,21,20,19,18,17)
mycon <- as.vector(sapply(year,function(x){paste0(nmes,x)}))
mycon <- mycon[-length(mycon)]
mycierre <- as.vector(sapply(year, function(x){paste0(mes,"20",x)}))
mycierre <- mycierre[-1]
#Los años 2015 y 2016 tienen 26 y 53 fecha de consulta respectivamente.
#Como representan menos del 15% de los días del año, no se consideran para el análisis
#Del resto de los años, el 2020 tiene 105 fechas de consulta que representa 29% 
#de los días del años. Si consideramos un mínimo de 25%, ingresa en el análisis.

bdcList <- list()
for (my in 1:length(mycon)) {
  # arch <- list.files("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/01_Consulta Ajena",
  #                    pattern = paste0("^VAR7.*.",mycon[my],".xls$"))
  arch <- list.files("D:/!bso/Consultas/conxls/",
                     pattern = paste0("^VAR7.*.",mycon[my],".xls$"))
  arch <- sort(arch)
  conList <- list()
  for (i in 1:length(arch)) {
    tryCatch({
      print(arch[i])
      # con <- read_excel(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/01_Consulta Ajena/",
      #                          arch[i]), sheet = "inf",col_names = c("TIPO","NDOC","EXT","CLIENTE","FNAC","ENTIDAD","FCON"),
      #                   col_types = c("text","text","text","text","text","text","date")) %>%
      con <- read_excel(paste0("D:/!bso/Consultas/conxls/",arch[i]),
                        sheet="inf",col_names=c("TIPO","NDOC","EXT","CLIENTE","FNAC","ENTIDAD","FCON"),
                        col_types = c("text","text","text","text","text","text","date")) %>% 
        select(-EXT,-FNAC) %>% 
        mutate(FCON=as.Date(FCON))
      #print(length(which(is.na(con$FCON)))) #PARA VERIFICAR SI LAS FECHAS DE CONSULTA SON LEIDAS CORRECTAMENTE
      conList[[i]] <- con
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  conL <- bind_rows(conList) %>% 
    dplyr::filter(TIPO=="CI") %>%
    select(-TIPO) %>% 
    rename(CI=NDOC) %>% 
    mutate(grupo_Entidad= case_when(str_detect(ENTIDAD,'BANCO')~'Banco',
                                    str_detect(ENTIDAD,'COOPERATIVA')~'Cooperativa',
                                    str_detect(ENTIDAD,'IFD')~'IFD',
                                    str_detect(ENTIDAD,'FONDO')~'Fondo',
                                    TRUE~'Otros')) %>% 
    mutate(FechaCon = as.Date(FCON)) %>%
    mutate(DiaCon = day(FCON)) %>% 
    mutate(MesCon = month(FCON)) %>% 
    mutate(YearCon = year(FCON)) %>% 
    mutate(Consultado = 1)
  print(mycierre[my])
  tryCatch({
    bdc <- readRDS(paste0("D:/!bso/girCartera/rdsGAR/ec_",mycierre[my],".rds"))%>% 
      mutate(NOMBRE_CLIENTE=paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT,sep=" ")) %>% 
      mutate(saldoTot=sum(saldous),opsTot=sum(opTot),montoTot=sum(montous)) %>% 
      select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_CLIENTE,CI,ASESOR,NOMBRE_ASESOR,
             MONEDA,FDESEMBOLSO,FFINALIZA,ESTADO,CIU,PLAZODIAS,DESC_OBJCRED,TIPO_CLIENTE,
             CALIFICACION, MONTO, DIASMORA,saldous,montous,labGrupoC,labGrupoD,
             par0,par30,rango,rangom,rangos,tipoCred,sucursal,saldoTot,opsTot,montoTot) %>% 
      left_join(conL,by="CI") %>% 
      mutate(Consultado = ifelse(is.na(Consultado),0,Consultado)) %>% 
      dplyr::filter(Consultado==1) %>% 
      mutate(mesCierre = substr(mycierre[my],1,3)) %>% 
      mutate(yearCierre = as.numeric(substr(mycierre[my],4,7)))
    bdcList[[my]] <- bdc
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

bdcCon <- bind_rows(bdcList)
write_xlsx(bdcCon,"D:/!bso/Consultas/bdcConsultasEne22Dic22.xlsx")
write_excel_csv(bdcCon,"D:/!bso/Consultas/bdcConsultasEne17Dic22_v2.csv")

####____READING BDC CON CONSULTAS____####
bdcCon <- fread("D:/!bso/Consultas/bdcConsultasEne17Dic22_v2.csv",
                encoding = "UTF-8",fill = T,sep=",")
# bdcCon <- fread("D:/!bso/Consultas/bdcConsultasEne22Dic22.csv",
#                 encoding = "UTF-8",fill = T,sep=",")

bdcCon <- bdcCon %>% 
  dplyr::filter(Consultado==1) %>% 
  group_by(OPERACION,FechaCon,ENTIDAD) %>% 
  mutate(pos=row_number()) %>% 
  ungroup() %>% 
  dplyr::filter(pos==1)

write_excel_csv(bdcCon, "D:/!bso/Consultas/Consultas_1XEntidad_Ene17Dic22.csv")

####___CONSULTAS DEPURADO____####
#La depuración consiste en seleccionar una sola consulta por entidad para cada fecha
bdcCon <- fread("D:/!bso/Consultas/Consultas_1XEntidad_Ene17Dic22.csv",
                encoding = "UTF-8",fill = T,sep=",")

#Conservar una sola operación por año y mes
bdcConOrd <- bdcCon %>% 
  arrange(OPERACION,YearCon,MesCon) %>% 
  dplyr::filter(FechaCon>=as.Date("2021-03-01")) %>% 
  group_by(OPERACION,YearCon,MesCon) %>% 
  mutate(pos=row_number()) %>% 
  ungroup() %>% 
  dplyr::filter(pos==1)

#Acumular el saldo de las operaciones consultadas por cada fecha en un mes
bdcConCum <- bdcConOrd %>% 
  group_by(YearCon,MesCon,FechaCon) %>% 
  summarise(saldous=sum(saldous),opTot=n(),saldoTot=max(saldoTot)) %>%
  ungroup() %>% 
  group_by(YearCon,MesCon) %>% 
  mutate(saldoCum=cumsum(saldous),opTot=cumsum(opTot),
         saldoConRel=cumsum(saldous)/saldoTot)

scalefac <- max(bdcConCum$saldoCum)/max(bdcConCum$opTot)
ggplot(bdcConCum,aes(x=FechaCon,y=saldoCum)) +
  geom_line(size=1.25,color=paleta(12)[2])+
  geom_line(aes(y=opTot*scalefac),size=1.25,color=paleta(12)[8])+
 scale_x_continuous(breaks = as.numeric(seq.Date(as.Date("2017-02-01"),as.Date("2022-12-01"),by="6 month")),
                    labels = format(seq.Date(as.Date("2017-02-01"),as.Date("2022-12-01"),by="6 month"),"%d %b %y"))+
labs(x="Fecha de Consulta",y="Saldo consultado acumulado (USD)",
     title = "Saldo de operaciones consultadas")+
  scale_y_continuous(labels = scales::comma, sec.axis = 
                       sec_axis(~./scalefac,name = "Operaciones consultadas acumulado",
                                breaks = seq(0,20000,3500)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45,vjust=0.75,hjust=0.5),
        plot.title = element_text(size=11,face = "bold"))

#Plot de cuanto representa el saldo consultado respecto del saldo total (cartera bruta)
ggplot(bdcConCum,aes(x=FechaCon,y=saldoConRel)) +
  geom_line(size=1.5,color=paleta(12)[2])+
  scale_x_continuous(breaks = as.numeric(seq.Date(as.Date("2017-02-01"),as.Date("2022-12-01"),by="6 month")),
                     labels = format(seq.Date(as.Date("2017-02-01"),as.Date("2022-12-01"),by="6 month"),"%d %b %y"))+
  labs(x="Fecha de Consulta",y="Saldo relativo consultado (USD)",
       title = "Saldo de operaciones consultadas respecto a cartera bruta")+
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45,vjust=0.75,hjust=0.5),
        plot.title = element_text(size=11,face = "bold"))

#Número de consultas de una operación en un año
bdcConOrd <- bdcCon %>% 
  arrange(OPERACION,YearCon) %>% 
  group_by(OPERACION,YearCon) %>% 
  summarise(Nveces=n()) %>% 
  ungroup()

#Diagrama de barras de número de consultas al año
ggplot(bdcConOrd,aes(x=factor(Nveces))) +
  geom_bar(fill=paleta(12)[1])+
  labs(x="Número de consultas al año",y="Cantidad de operaciones",
       title="Distribución de número de consultas anuales (2017-2022)")+
  scale_x_discrete(breaks=seq(1,37,4),labels=seq(1,37,4))+
  scale_y_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"))
  
####____MAT CANCEL____####

dlist <- c('Ene2017', 'Feb2017', 'Mar2017', 'Abr2017', 'May2017', 'Jun2017', 'Jul2017', 'Ago2017', 'Sep2017', 'Oct2017', 'Nov2017', 'Dic2017',
           'Ene2018', 'Feb2018', 'Mar2018', 'Abr2018', 'May2018', 'Jun2018', 'Jul2018', 'Ago2018', 'Sep2018', 'Oct2018', 'Nov2018', 'Dic2018',
           'Ene2019', 'Feb2019', 'Mar2019', 'Abr2019', 'May2019', 'Jun2019', 'Jul2019', 'Ago2019', 'Sep2019', 'Oct2019', 'Nov2019', 'Dic2019',
           'Ene2020', 'Feb2020', 'Mar2020', 'Abr2020', 'May2020', 'Jun2020', 'Jul2020', 'Ago2020', 'Sep2020', 'Oct2020', 'Nov2020', 'Dic2020',
           'Ene2021', 'Feb2021', 'Mar2021', 'Abr2021', 'May2021', 'Jun2021', 'Jul2021', 'Ago2021', 'Sep2021', 'Oct2021', 'Nov2021', 'Dic2021',
           'Ene2022', 'Feb2022', 'Mar2022', 'Abr2022', 'May2022', 'Jun2022', 'Jul2022', 'Ago2022', 'Sep2022', 'Oct2022', 'Nov2022', 'Dic2022')
clist <- list()

for(i in 1:(length(dlist)-1)) {
  tryCatch({
    print(i)
    k = i + 1
    print(k)
    print(dlist[k])
    
    df1 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_', dlist[i], '.rds'))
    df2 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_', dlist[k], '.rds'))
    
    df3 <- df1[!(df1$OPERACION %in% df2$OPERACION),] %>% 
      select(OPERACION, CI, CTACLIENTE, saldous, CALIFICACION, ESTADO,
             tipoCred, montous, sucursal) %>% 
      dplyr::filter(CALIFICACION %in% c("A","B","C","D","E","F","S")) %>% 
      mutate(rangom = ifelse(montous > 20000, '20k+', 'under20k')) %>% 
      mutate(yearCancel=as.numeric(substr(dlist[k],4,7))) %>% 
      mutate(mCancel = dlist[k]) %>% #paste0(substr(dlist[k],1,3),". ",substr(dlist[k],4,7)) 
      mutate(mSearch = dlist[i])
    clist[[i]] <- df3
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

dfCancel <- rbindlist(clist) %>% 
  dplyr::filter(!is.na(mCancel)) %>% 
  mutate(FCANCEL=as.yearmon(sub("(.{3})(.{4})","\\1. \\2",mCancel))) %>% 
  select(OPERACION, CTACLIENTE,rangom,FCANCEL,yearCancel,mCancel,saldous)

write.csv(dfCancel,"D:/!bso/Consultas/dfCancelEne17Dic22.csv",row.names = F)

####____READING CREDITOS CANCELADOS____####
dfCancel <- read.csv("D:/!bso/Consultas/dfCancelEne17Dic22.csv") %>% 
  mutate(FCANCEL=as.yearmon(FCANCEL))

bdcConbackup <- bdcCon

#Nuevamente selecciono una sola vez por mes a cada operación consultada (la de fecha más reciente)
bdcCon <- bdcCon %>% 
  #mutate(FCON=as.yearmon(as.Date(paste(YearCon,MesCon,"01",sep = "-")))) %>%
  #dplyr::filter(FCON>=as.Date("2021-02-01")) %>% 
  mutate(FCON=as.yearmon(FechaCon)) %>% 
  dplyr::filter(FCON>"feb. 2021") %>% 
  mutate(mesCierre=cases(mesCierre,c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
                         1:12)) %>% 
  mutate(FCIERRE=as.yearmon(as.Date(paste(yearCierre,mesCierre,"01",sep = "-")))) %>%
  arrange(OPERACION,FCON) %>% 
  group_by(OPERACION,FCON) %>% 
  arrange(desc(FechaCon)) %>% 
  mutate(pos=row_number()) %>% 
  ungroup() %>% 
  dplyr::filter(pos==1) %>% 
  group_by(FCON) %>% 
  mutate(saldoCum=sum(saldous)) %>% 
  mutate(opsCum=n()) %>% 
  ungroup()

#Unión de base de cartera consultada con operaciones canceladas
#Quedarse solo con operaciones cuya fecha de cancelación sea posterior
bdcConCan <- bdcCon %>% 
  left_join(dfCancel,by=c("OPERACION","CTACLIENTE")) %>% 
  dplyr::filter(FCON<FCANCEL) %>% 
  dplyr::filter(FCON==FCANCEL-1/12) %>% 
  dplyr::filter(saldous.y/(MONTO/6.86)>0.5)

ConCancel<- bdcConCan %>% 
  group_by(OPERACION) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  group_by(FCANCEL) %>% 
  summarise(saldous=sum(saldous.y),nOps=n(),ration=n()/max(opsCum)*100,
            ratio=sum(saldous.y)/max(saldoCum)*100,saldoCum=max(saldoCum),nCum=max(opsCum)) %>% 
  ungroup()

scalefac <- max(ConCancel$saldoCum/1e6)/max(ConCancel$ratio)
ggplot(ConCancel,aes(x=FCANCEL,y=saldoCum/1e6))+
  geom_bar(stat="identity",size=1.5,fill=paleta(12)[2])+
  geom_line(aes(y=ratio*scalefac),size=1.5,color=paleta(12)[8])+
  geom_text(aes(y=ratio*scalefac-5,label=round(ratio,1)),size=3.5,
            color=paleta(12)[8])+
  geom_text(aes(label=round(saldoCum/1e6,1)),size=3.5,color="white",
            position = position_stack(vjust = 0.75))+
  labs(x="Mes",y="Saldo consultado (MM USD)",
       title = "Evolución de saldo consultado y ratio de conversión")+
  scale_y_continuous(breaks=seq(0,125,25),labels = scales::comma,sec.axis = 
                       sec_axis(~./scalefac,name="Ratio de conversión (%)"))+
  #scale_x_continuous(breaks = 3:12)+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[8]))

scalefac <- max(ConCancel$nCum/1e3)/max(ConCancel$ration)
ggplot(ConCancel,aes(x=FCANCEL,y=nCum/1e3))+
  geom_bar(stat="identity",size=1.5,fill=paleta(12)[2])+
  geom_line(aes(y=ration*scalefac),size=1.5,color=paleta(12)[8])+
  geom_text(aes(y=ration*scalefac-1,label=round(ration,1)),size=3.5,
            color=paleta(12)[8])+
  geom_text(aes(label=round(nCum/1e3,2)),size=3.5,color="white",
            position = position_stack(vjust = 0.5))+
  labs(x="Mes",y="Operaciones consultadas (en miles)",
       title = "Evolución de operaciones consultadas y ratio de conversión")+
  scale_y_continuous(breaks=seq(0,20,2),labels = scales::comma,sec.axis = 
                       sec_axis(~./scalefac,name="Ratio de conversión (%)"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[8]))
  
ConCancel <- bdcConCan%>% 
  group_by(OPERACION) %>% 
  arrange(FechaCon) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(grupo_Entidad=ifelse(grupo_Entidad=="Fondo","Otros",grupo_Entidad)) %>% 
  group_by(FCANCEL,grupo_Entidad) %>% 
  summarise(saldous=sum(saldous.x),nOps=n()) %>% 
  ungroup() %>% 
  group_by(FCANCEL) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>% 
  mutate(pctn=nOps/sum(nOps)*100) %>% 
  ungroup() %>% 
  mutate(grupo_Entidad=fct_reorder(grupo_Entidad,saldous))


ggplot(ConCancel,aes(x=FCANCEL,y=saldous,fill=grupo_Entidad))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pct,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Saldo cancelado consultado (USD)",
       title = "Participación de Tipo Entidad en saldo cancelado consultado (1 mes antes)",
       fill="Tipo de Entidad")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = paleta(5)[4:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom")

ggplot(ConCancel,aes(x=FCANCEL,y=nOps,fill=grupo_Entidad))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pctn,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Operaciones canceladas previamente consultadas",
       title = "Participación de Tipo Entidad en operaciones canceladas consultadas (1 mes antes)",
       fill="Tipo de Entidad")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = paleta(5)[4:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom")

conCancel <- bdcConCan %>% 
  group_by(OPERACION) %>% 
  arrange(FCON) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  dplyr::filter(grupo_Entidad=="Banco") %>%
  mutate(ENTIDAD=ifelse(ENTIDAD=="BANCO FORTALEZA SA","BANCO FORTALEZA S.A.",ENTIDAD)) %>% 
  mutate(ENTIDAD=ifelse(ENTIDAD=="BANCO FIE SA","BANCO FIE S.A.",ENTIDAD)) %>% 
  group_by(FCANCEL,ENTIDAD) %>% 
  summarise(saldous=sum(saldous.x),nOps=n()) %>% 
  ungroup() %>% 
  group_by(FCANCEL) %>% 
  arrange(desc(saldous)) %>% 
  mutate(pos=row_number()) %>% 
  ungroup() %>% 
  mutate(ENTIDAD=ifelse(ENTIDAD %in% ENTIDAD[pos<=7 & FCANCEL=="Nov. 2022"],ENTIDAD,"Otros")) %>% 
  group_by(FCANCEL,ENTIDAD) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(FCANCEL) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>%
  mutate(pctn=nOps/sum(nOps)*100) %>%
  ungroup() %>%
  mutate(ENTIDAD=fct_reorder(ENTIDAD,pct))
  
ggplot(conCancel,aes(x=FCANCEL,y=pct,fill=ENTIDAD))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pct,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Participación en saldo cancelado consultado (%)",
       title = "Participación de Banco en saldo cancelado consultado (1 mes antes)",
       fill="Banco")+
  scale_fill_manual(values = paleta(8)[8:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom")

ggplot(conCancel,aes(x=FCANCEL,y=pctn,fill=ENTIDAD))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pctn,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Participación en ops. canceladas consultadas (%)",
       title = "Participación de Banco en operaciones canceladas consultadas (1 mes antes)",
       fill="Banco")+
  scale_fill_manual(values = paleta(8)[8:1])+
  #scale_x_discrete(labels=c("Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom")

bdcConCan %>% 
  dplyr::filter(FCON==FCANCEL-2/12) %>% 
  dplyr::filter(saldous.y/(MONTO/6.86)>0.5) %>% 
  group_by(OPERACION) %>% 
  arrange(FCON) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  dplyr::filter(grupo_Entidad=="Banco") %>%
  mutate(ENTIDAD=ifelse(ENTIDAD=="BANCO FORTALEZA SA","BANCO FORTALEZA S.A.",ENTIDAD)) %>% 
  mutate(ENTIDAD=ifelse(ENTIDAD=="BANCO FIE SA","BANCO FIE S.A.",ENTIDAD)) %>% 
  group_by(FCANCEL,ENTIDAD) %>% 
  summarise(saldous=sum(saldous.x),nOps=n()) %>% 
  ungroup() %>% 
  group_by(FCANCEL) %>% 
  arrange(desc(saldous)) %>% 
  mutate(pos=row_number()) %>% 
  ungroup() %>%
  arrange(FCANCEL) %>% 
  mutate(ENTIDAD=ifelse(ENTIDAD %in% ENTIDAD[pos<=7 & FCANCEL=="nov. 2022"],ENTIDAD,"Otros")) %>% 
  group_by(FCANCEL,ENTIDAD) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(FCANCEL) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>% 
  ungroup() %>%
  mutate(ENTIDAD=fct_reorder(ENTIDAD,pct)) %>% 
  ggplot(aes(x=FCANCEL,y=pct,fill=ENTIDAD))+
  geom_bar(stat="identity")+
  # geom_text(aes(label=round(pct,2)),color="white",size=2.5,
  #           position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Participación (%)",
       title = "Participación de Banco en saldo cancelado (consultado 2 meses antes)",
       fill="Banco")+
  scale_fill_manual(values = paleta(8))+
  # scale_x_discrete(labels=c("Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"))

####____ANÁLISIS POR RATIO SALDO/MONTO____####
bdcConCan <- bdcCon %>% 
  left_join(dfCancel,by=c("OPERACION","CTACLIENTE")) %>% 
  dplyr::filter(FCON<FCANCEL) %>% 
  dplyr::filter(FCON==FCANCEL-1/12) %>% 
  mutate(Saldo_Monto=cut(saldous.x/(MONTO/6.86),seq(0,1,0.25),include.lowest=T))
  # dplyr::filter(saldous.y/(MONTO/6.86)>0.5)

ConCancel<- bdcConCan %>% 
  group_by(OPERACION) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  group_by(FCANCEL, Saldo_Monto) %>% 
  summarise(saldous=sum(saldous.x),nOps=n()) %>% 
  ungroup() %>% 
  group_by(FCANCEL) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>% 
  mutate(pctn=nOps/sum(nOps)*100) %>% 
  ungroup() %>% 
  mutate(Saldo_Monto=fct_reorder(Saldo_Monto,saldous))

ggplot(ConCancel,aes(x=FCANCEL,y=saldous,fill=Saldo_Monto))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pct,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Saldo cancelado consultado (USD)",
       title = "Participación de Tipo Entidad en saldo cancelado consultado (1 mes antes)",
       fill="Tipo de Entidad")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = paleta(6)[5:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom")

ggplot(ConCancel,aes(x=FCANCEL,y=nOps,fill=grupo_Entidad))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pctn,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Operaciones canceladas previamente consultadas",
       title = "Participación de Tipo Entidad en operaciones canceladas consultadas (1 mes antes)",
       fill="Tipo de Entidad")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = paleta(5)[4:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom")