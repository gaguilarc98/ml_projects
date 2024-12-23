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
####____CONSOLIDADO DE CONSULTAS____####
mes <- c("Dic","Nov","Oct","Sep","Ago","Jul","Jun","May","Abr","Mar","Feb","Ene")
nmes <- c("12","11","10","09","08","07","06","05","04","03","02","01")
year <- c(22,21)
#year <- c(22,21,20,19,18,17)
mycon <- as.vector(sapply(year,function(x){paste0(nmes,x)}))
mycon <- mycon[-length(mycon)]
mycierre <- as.vector(sapply(year, function(x){paste0(mes,"20",x)}))
mycierre <- mycierre[-1]
conList <- list()
k <- 1
for (my in 1:length(mycon)) {
  arch <- list.files("D:/!bso/Consultas/conxls/",
                     pattern = paste0("^VAR7.*.",mycon[my],".xls$"))
  arch <- sort(arch)
  for (i in 1:length(arch)) {
    tryCatch({
      print(arch[i])
      con <- read_excel(paste0("D:/!bso/Consultas/conxls/",arch[i]),
                        sheet="inf",col_names=c("TIPO","NDOC","EXT","CLIENTE","FNAC","ENTIDAD","FCON"),
                        col_types = c("text","text","text","text","text","text","date")) %>% 
        select(-EXT,-FNAC) %>% 
        mutate(FCON=as.Date(FCON)) %>% 
        group_by(NDOC,FCON,ENTIDAD) %>%
        arrange(desc(FCON)) %>% 
        dplyr::filter(row_number()==1) %>% 
        ungroup()
      k <- k+1
      print(k)
      conList[[k]] <- con
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
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

write_rds(conL,"D:/!bso/Consultas/ConsultasEne21Dic22.rds")

conL <- readRDS("D:/!bso/Consultas/ConsultasEne21Dic22.rds")
Check0 <- conL %>% 
  group_by(YearCon,MesCon) %>% 
  summarise(n()) %>% 
  ungroup()

conL2 <- conL %>% 
  dplyr::filter(year(FCON)==2022 & month(FCON)==12)
bdcList <- list()
for (my in 1:length(mycon)) {
  arch <- list.files("D:/!bso/Consultas/conxls/",
                     pattern = paste0("^VAR7.*.",mycon[my],".xls$"))
  arch <- sort(arch)
  conList <- list()
  #Lectura de consultas para todo un mes
  for (i in 1:length(arch)) {
    tryCatch({
      print(arch[i])
      con <- read_excel(paste0("D:/!bso/Consultas/conxls/",arch[i]),
                        sheet="inf",col_names=c("TIPO","NDOC","EXT","CLIENTE","FNAC","ENTIDAD","FCON"),
                        col_types = c("text","text","text","text","text","text","date")) %>% 
        select(-EXT,-FNAC) %>% 
        mutate(FCON=as.Date(FCON)) %>% 
        group_by(NDOC,FCON,ENTIDAD) %>% 
        arrange(desc(FCON)) %>% 
        dplyr::filter(row_number()==1) %>% 
        ungroup()
      #print(length(which(is.na(con$FCON)))) #PARA VERIFICAR SI LAS FECHAS DE CONSULTA SON LEIDAS CORRECTAMENTE
      conList[[i]] <- con
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  #consolidado de consultas del mes
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
  #unir con el cierre del mes anterior, solo me quedo con las consultadas
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
write_excel_csv(bdcCon,"D:/!bso/Consultas/bdcConsultasEne17Dic22_v2.csv")

####____READING BDC CON CONSULTAS____####
bdcCon <- fread("D:/!bso/Consultas/bdcConsultasEne17Dic22_v2.csv",
                encoding = "UTF-8",fill = T,sep=",")
# bdcCon <- fread("D:/!bso/Consultas/bdcConsultasEne22Dic22.csv",
#                 encoding = "UTF-8",fill = T,sep=",")
#Chequeo por mes de consultas
bdCCheck1 <- bdcCon %>% 
  group_by(YearCon,MesCon) %>% 
  summarise(saldo=sum(saldous),nOps=n()) %>% 
  ungroup()


bdcCon <- bdcCon %>% 
  dplyr::filter(Consultado==1) %>% 
  group_by(OPERACION,FechaCon,ENTIDAD) %>% 
  mutate(pos=row_number()) %>% 
  ungroup() %>% 
  dplyr::filter(pos==1)

#Chequeo por mes de consultas de consultas únicas por fecha y entidad
bdcCheck2 <- bdcCon %>% 
  group_by(YearCon,MesCon) %>% 
  summarise(saldo=sum(saldous),nOps=n()) %>% 
  ungroup()

write_excel_csv(bdcCon, "D:/!bso/Consultas/Consultas_1XEntidad_Ene17Dic22.csv")

####___CONSULTAS DEPURADO____####
#La depuración consiste en seleccionar una sola consulta por entidad para cada fecha
bdcCon <- fread("D:/!bso/Consultas/Consultas_1XEntidad_Ene17Dic22.csv",
                encoding = "UTF-8",fill = T,sep=",")

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
dfCancel <- readRDS("D:/!bso/Consultas/dfCancelEne17Dic22.rds") %>% 
  mutate(FCANCEL=as.yearmon(mCancel))

CheckCancel <- dfCancel %>% 
  group_by(FCANCEL) %>% 
  summarise(saldo=sum(saldous),nOps=n()) %>% 
  ungroup()
####____GRAFICOS DESCRIPTIVOS____####
bdcConbackup <- bdcCon

#Selecciono una sola vez por mes a cada operación consultada (la de fecha más reciente)
#Filtro desde marzo 2021
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

#Chequeo por año y mes cuanto de saldo y operaciones acumulado es consultado
#Sin embargo, como la unión de consultas se realizó con el mes anterior a las
#consultas se reporta el saldo del mes anterior a las consultas.
bdcCheck3 <- bdcCon %>% 
  group_by(YearCon,MesCon) %>% 
  summarise(saldo=sum(saldous),nOps=n()) %>% 
  ungroup()
#Unión de base de cartera consultada con operaciones canceladas
#Quedarse solo con operaciones cuya fecha de cancelación sea posterior
#Para fines explicativos me quedo con el saldo del mes previo a la consulta
#El saldo acumulado del mes se encuentra en saldoCum y las operaciones del mes en opsCum
CheckConCan <- bdcCon %>% 
  left_join(dfCancel,by=c("OPERACION","CTACLIENTE")) %>% 
  dplyr::filter(FCON<FCANCEL) %>% 
  group_by(YearCon,MesCon) %>% 
  summarise(saldo=sum(saldous.x),nOps=n()) %>% 
  ungroup()

CheckConCan1m <- bdcCon %>% 
  left_join(dfCancel,by=c("OPERACION","CTACLIENTE")) %>% 
  dplyr::filter(FCON<FCANCEL) %>% 
  dplyr::filter(FCON==FCANCEL-1/12) %>% 
  group_by(YearCon,MesCon) %>% 
  summarise(saldo=sum(saldous.x),nOps=n()) %>% 
  ungroup()

bdcConCan <- bdcCon %>% 
  left_join(dfCancel,by=c("OPERACION","CTACLIENTE")) %>% 
  dplyr::filter(FCON<FCANCEL) %>% 
  dplyr::filter(FCON==FCANCEL-1/12) %>% 
  dplyr::filter(saldous.y/(MONTO/6.86)>0.5)
#El filtro se aplica sobre el saldo en el mes antes de la cancelación (saldo.y)
#No sobre el saldo de la unión de consultas con cierre (saldo.x que es 2 meses antes)
bdcCheck4 <- bdcConCan %>% 
  group_by(YearCon,MesCon) %>% 
  summarise(saldo=sum(saldous.x),nOps=n()) %>% 
  ungroup()

bdcCheck4$nOps/CheckConCan1m$nOps*100
#Por consistencia se continúa mostrando el saldo 2 meses antes, puesto que el ratio de
#conversión se tomará sobre ese saldo.

ConCancel<- bdcConCan %>% 
  group_by(OPERACION) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  group_by(FCON) %>% 
  summarise(saldous=sum(saldous.x),nOps=n(),ration=n()/max(opsCum)*100,
            ratio=sum(saldous.x)/max(saldoCum)*100,saldoCum=max(saldoCum),nCum=max(opsCum)) %>% 
  ungroup()
  
scalefac <- max(ConCancel$saldoCum/1e6)/max(ConCancel$ratio)
ggplot(ConCancel,aes(x=FCON,y=saldoCum/1e6))+
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
  scale_x_continuous(breaks=as.numeric(ConCancel$FCON),labels = format(ConCancel$FCON,"%m/%y"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[8]),
        axis.text.x.bottom = element_text(angle = 90))

scalefac <- max(ConCancel$nCum/1e3)/max(ConCancel$ration)
ggplot(ConCancel,aes(x=FCON,y=nCum/1e3))+
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
  scale_x_continuous(breaks=as.numeric(ConCancel$FCON),labels = format(ConCancel$FCON,"%m/%y"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[8]),
        axis.text.x.bottom = element_text(angle = 90))

#Agrupo tipos de entidades minoritarias y calculo porcentajes de participación
ConCancel <- bdcConCan%>% 
  group_by(OPERACION) %>% 
  arrange(FechaCon) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(grupo_Entidad=ifelse(grupo_Entidad=="Fondo","Otros",grupo_Entidad)) %>% 
  group_by(FCON,grupo_Entidad) %>% 
  summarise(saldous=sum(saldous.x),nOps=n()) %>% 
  ungroup() %>% 
  group_by(FCON) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>% 
  mutate(pctn=nOps/sum(nOps)*100) %>% 
  ungroup() %>% 
  mutate(grupo_Entidad=fct_reorder(grupo_Entidad,saldous))


labels <- ConCancel %>% 
  group_by(FCON) %>% 
  summarise(saldo=sum(saldous)/1e6,nOps=sum(nOps))

#Gráfico de participación de tipo de entidad por saldo
ggplot(ConCancel,aes(x=FCON,y=saldous/1e6,fill=grupo_Entidad))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pct,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Saldo cancelado consultado (MM USD)",
       title = "Participación de Tipo Entidad en saldo cancelado consultado (1 mes antes)",
       fill="Tipo de Entidad")+
  annotate(geom="text",x=labels$FCON,y=labels$saldo+0.5,
           label=round(labels$saldo,2),size=3.5)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks=as.numeric(ConCancel$FCON),labels = format(ConCancel$FCON,"%m/%y"))+
  scale_fill_manual(values = paleta(5)[4:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom",
        axis.text.x.bottom = element_text(angle=90))

#Gráfico de participación de tipo de entidad por operaciones
ggplot(ConCancel,aes(x=FCON,y=nOps,fill=grupo_Entidad))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pctn,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Operaciones canceladas consultadas",
       title = "Participación de Tipo Entidad en operaciones canceladas consultadas (1 mes antes)",
       fill="Tipo de Entidad")+
  annotate(geom="text",x=labels$FCON,y=labels$nOps+50,
           label=round(labels$nOps,2),size=3.5)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks=as.numeric(ConCancel$FCON),labels = format(ConCancel$FCON,"%m/%y"))+
  scale_fill_manual(values = paleta(5)[4:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom", 
        axis.text.x.bottom = element_text(angle=90))

#Agrupo bancos minoritarios y los ordeno por contribución en porcentaje
ConCancel <- bdcConCan %>% 
  group_by(OPERACION) %>% 
  arrange(FCON) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  dplyr::filter(grupo_Entidad=="Banco") %>%
  mutate(ENTIDAD=ifelse(ENTIDAD=="BANCO FORTALEZA SA","BANCO FORTALEZA S.A.",ENTIDAD)) %>% 
  mutate(ENTIDAD=ifelse(ENTIDAD=="BANCO FIE SA","BANCO FIE S.A.",ENTIDAD)) %>% 
  group_by(FCON,ENTIDAD) %>% 
  summarise(saldous=sum(saldous.x),nOps=n()) %>% 
  ungroup() %>% 
  group_by(FCON) %>% 
  arrange(desc(saldous)) %>% 
  mutate(pos=row_number()) %>% 
  ungroup() %>% 
  mutate(ENTIDAD=ifelse(ENTIDAD %in% ENTIDAD[pos<=7 & FCON=="Nov. 2022"],ENTIDAD,"Otros")) %>% 
  group_by(FCON,ENTIDAD) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(FCON) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>%
  mutate(pctn=nOps/sum(nOps)*100) %>%
  ungroup() %>%
  mutate(ENTIDAD=fct_reorder(ENTIDAD,pct))

#Gráfico de participación de banco en saldo
ggplot(ConCancel,aes(x=FCON,y=pct,fill=ENTIDAD))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pct,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Participación en saldo cancelado consultado (%)",
       title = "Participación de Banco en saldo cancelado consultado (1 mes antes)",
       fill="Banco")+
  scale_fill_manual(values = paleta(8)[8:1])+
  scale_x_continuous(breaks=as.numeric(ConCancel$FCON),labels = format(ConCancel$FCON,"%m/%y"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom",
        axis.text.x.bottom = element_text(angle=90))

#Gráfico de participación de banco en operaciones
ggplot(ConCancel,aes(x=FCON,y=pctn,fill=ENTIDAD))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pctn,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Participación en ops. canceladas consultadas (%)",
       title = "Participación de Banco en operaciones canceladas consultadas (1 mes antes)",
       fill="Banco")+
  scale_fill_manual(values = paleta(8)[8:1])+
  scale_x_continuous(breaks=as.numeric(ConCancel$FCON),labels = format(ConCancel$FCON,"%m/%y"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom",
        axis.text.x.bottom = element_text(angle=90))

#Gráfico de participación de rango de saldo
ConCancel <- bdcConCan %>% 
  group_by(OPERACION) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(rangos=case_when(rangos %in% c("1. <500USD","2. 500-1k")~"< 1k",
                          rangos == "2. 500-1k"~"500-1k",
                          rangos == "3. 1k-5k"~"1k-5k",
                          rangos == "4. 5k-10k"~"5k-10k",
                          rangos == "5. 10k-15k"~"10k-15k",
                          rangos %in% c("6. 15k-20k","7. >20k")~"> 15k",
                          TRUE~rangos)) %>% 
  group_by(FCON,rangos) %>% 
  summarise(saldous=sum(saldous.x),nOps=n()) %>% 
  ungroup() %>% 
  group_by(FCON) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>% 
  mutate(pctn=nOps/sum(nOps)*100) %>% 
  ungroup() %>% 
  mutate(rangos=fct_reorder(rangos,pctn))

#Gráfico de porcentaje de saldo por operaciones
ggplot(ConCancel,aes(x=FCON,y=pctn,fill=rangos)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pctn,1)),size=3,color="white",
            position=position_stack(vjust=0.5))+
  labs(x="Mes",y="Participación (%)", fill="Rango de saldo (USD)",
       title="Participación de Rango de saldo en Saldo consultado cancelado (1 mes antes)")+
  scale_x_continuous(breaks=as.numeric(ConCancel$FCON),labels = format(ConCancel$FCON,"%m/%y"))+
  scale_fill_manual(values = paleta(5)[5:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom",
        axis.text.x.bottom = element_text(angle=90))

#Gráfico participación de saldo/monto
ConCancel <- bdcCon %>% 
  left_join(dfCancel,by=c("OPERACION","CTACLIENTE")) %>% 
  dplyr::filter(FCON<FCANCEL) %>% 
  dplyr::filter(FCON==FCANCEL-1/12) %>% 
  mutate(Saldo_Monto=cut(saldous.y/(MONTO/6.86),seq(0,1,0.125),include.lowest=T)) %>% 
  dplyr::filter(!is.na(Saldo_Monto)) %>% 
  group_by(OPERACION) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  group_by(FCON,Saldo_Monto) %>% 
  summarise(saldous=sum(saldous.x),nOps=n()) %>% 
  ungroup() %>% 
  group_by(FCON) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>% 
  mutate(pctn=nOps/sum(nOps)*100) %>% 
  ungroup() %>% 
  mutate(Saldo_Monto=fct_reorder(Saldo_Monto,pctn))

ggplot(ConCancel,aes(x=FCON,y=pctn,fill=Saldo_Monto)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pctn,1)),size=3,color="white",
            position=position_stack(vjust=0.5))+
  labs(x="Mes",y="Participación (%)", fill="Ratio saldo/monto",
       title="Participación de Ratio saldo/monto en Saldo consultado cancelado (1 mes antes)")+
  scale_x_continuous(breaks=as.numeric(ConCancel$FCON),labels = format(ConCancel$FCON,"%m/%y"))+
  scale_fill_manual(values = paleta(8)[8:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom",
        axis.text.x.bottom = element_text(angle=90))
