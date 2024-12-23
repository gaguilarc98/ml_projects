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
nmes <- c("12","11","10","09","08","07","06","05","04","03","02","01")
year <- c(22,21,20,19,18,17)
#year <- c(22,21,20,19,18,17)
mycon <- as.vector(sapply(year,function(x){paste0(nmes,x)}))
mycon <- mycon[-1]
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
        mutate(FCON=as.Date(FCON))
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
  group_by(CI,FCON,ENTIDAD) %>%
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(grupo_Entidad= case_when(str_detect(ENTIDAD,'BANCO')~'Banco',
                                  str_detect(ENTIDAD,'COOPERATIVA')~'Cooperativa',
                                  str_detect(ENTIDAD,'IFD')~'IFD',
                                  str_detect(ENTIDAD,'FONDO')~'Fondo',
                                  TRUE~'Otros')) %>% 
  mutate(DiaCon = day(FCON)) %>% 
  mutate(MesCon = month(FCON)) %>% 
  mutate(YearCon = year(FCON)) %>% 
  mutate(myCon= as.yearmon(FCON)) %>% 
  mutate(Consultado = 1) 

conL <- conL %>% 
  dplyr::filter(myCon>'dic. 2016')
write_rds(conL,'D:/!bso/Consultas/ConsultasFullEne17Dic22.rds')
conL <- readRDS('D:/!bso/Consultas/ConsultasFullEne17Dic22.rds')
####____ADD A MONTH TO CONSULTAS___####
#Para agregar un mes al consolidado:
#1. Se hace correr el anterior chunk para un mes.
#2. El resultado se almacena en conMes.
#3. Se crean las variables grupo Entidad, DiaCon, MesCon, YearCon y myCon y se transforma FCON a fecha.
#4. Se abre el consolidado anterior en el objeto conL.
#5. Se hace un bindrows del consolidado anterior con conMes.
#6. Se vuelven a hacer los filtros de una consulta por entidad para un CI en una fecha.
mycon <- "0123" #MM/YY
conList <- list()
k <- 1
arch <- list.files("D:/!bso/Consultas/conxls/",
                   pattern = paste0("^VAR7.*.",mycon,".xls$"))
arch <- sort(arch)
for (i in 1:length(arch)) {
  tryCatch({
    print(arch[i])
    con <- read_excel(paste0("D:/!bso/Consultas/conxls/",arch[i]),
                      sheet="inf",col_names=c("TIPO","NDOC","EXT","CLIENTE","FNAC","ENTIDAD","FCON"),
                      col_types = c("text","text","text","text","text","text","date")) %>% 
      select(-EXT,-FNAC) %>% 
      mutate(FCON=as.Date(FCON))
    k <- k+1
    print(k)
    conList[[k]] <- con
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
conMes <- bind_rows(conList) %>% 
  dplyr::filter(TIPO=="CI") %>%
  select(-TIPO) %>% 
  rename(CI=NDOC) %>% 
  group_by(CI,FCON,ENTIDAD) %>%
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(grupo_Entidad= case_when(str_detect(ENTIDAD,'BANCO')~'Banco',
                                  str_detect(ENTIDAD,'COOPERATIVA')~'Cooperativa',
                                  str_detect(ENTIDAD,'IFD')~'IFD',
                                  str_detect(ENTIDAD,'FONDO')~'Fondo',
                                  TRUE~'Otros')) %>% 
  mutate(DiaCon = day(FCON)) %>% 
  mutate(MesCon = month(FCON)) %>% 
  mutate(YearCon = year(FCON)) %>% 
  mutate(myCon= as.yearmon(FCON)) %>% 
  mutate(Consultado = 1)

conLast <- conL %>% 
  #dplyr::filter(myCon>'ene. 2021') #Filtro de temporalidad opcional
  bind_rows(conMes) %>% 
  group_by(CI,FCON,ENTIDAD) %>%
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

write_rds(conLast,'D:/!bso/Consultas/ConsultasFullEne17Ene23.rds')

####____CRUCE CON BASES DE CARTERA AL CIERRE____####
remove(list=ls())
conFull <- readRDS('D:/!bso/Consultas/ConsultasFullEne17Ene23.rds')

check0 <- conFull %>% 
  dplyr::filter(myCon>="nov. 2020") %>% 
  group_by(myCon,YearCon,MesCon) %>% 
  summarise(nOps=n())
print(check0)

check01 <- conFull %>% 
  dplyr::filter(myCon>="nov. 2020") %>% 
  group_by(CI,myCon) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  group_by(myCon) %>% 
  summarise(nOps=n())
print(check01)

mes <- c("Dic","Nov","Oct","Sep","Ago","Jul","Jun","May","Abr","Mar","Feb","Ene")
year <- c(23,22,21,20)
myclose <- as.vector(sapply(year, function(x){paste0(mes,"20",x)}))
myclose <- myclose[-c(which(myclose=='Sep2020'):length(myclose))]
myclose <- myclose[-c(1:which(myclose=='Feb2023'))]
bdcConList <- list()
for (i in 1:length(myclose)) {
  mCierre <- substr(myclose[i],1,3)
  yCierre <- as.numeric(substr(myclose[i],4,7))
  myCierre <- as.yearmon(paste0(mCierre,'. ',yCierre))
  print(myCierre)
  conMes <- conFull %>% 
    dplyr::filter(myCon==as.yearmon(myCierre)+1/12)
  bdc <- readRDS(paste0("D:/!bso/girCartera/rdsGAR/ec_",myclose[i],".rds")) %>% 
    mutate(NOMBRE_CLIENTE=paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT,sep=" ")) %>% 
    mutate(saldoTot=sum(saldous),opsTot=sum(opTot),montoTot=sum(montous)) %>% 
    mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    select(CTACLIENTE,OPERACION,MODULO,AGENCIA,NOMBRE_CLIENTE,CI,ASESOR,NOMBRE_ASESOR,
           MONEDA,FDESEMBOLSO,FFINALIZA,ESTADO,CIU,PLAZODIAS,DESC_OBJCRED,TIPO_CLIENTE,
           CALIFICACION, MONTOUS, DIASMORA,saldous,montous,labGrupoC,labGrupoD,
           par0,par30,rango,rangos,tipoCred,sucursal,saldoTot,opsTot,montoTot) %>% 
    left_join(conMes,by="CI") %>% 
    mutate(Consultado = ifelse(is.na(Consultado),0,Consultado)) %>% 
    dplyr::filter(Consultado==1)
  bdcConList[[i]] <- bdc
}

bdcCon <- bind_rows(bdcConList)
write_rds(bdcCon,"D:/!bso/Consultas/bdcConsultasNov20Ene23.rds")

bdcCon <- readRDS("D:/!bso/Consultas/bdcConsultasNov20Ene23.rds")

check1 <- bdcCon %>% 
  group_by(OPERACION,FCON,ENTIDAD) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  group_by(YearCon,MesCon) %>% 
  summarise(saldo=sum(saldous),nOps=n())
print(check1)

bdcCon1xmes <- bdcCon %>% 
  #dplyr::filter(myCon>="feb. 2021") %>% 
  arrange(OPERACION,myCon) %>% 
  group_by(OPERACION,myCon) %>% 
  arrange(desc(FCON)) %>% 
  mutate(pos=row_number()) %>% 
  ungroup() %>% 
  dplyr::filter(pos==1) %>% 
  group_by(myCon) %>% 
  mutate(saldoCum=sum(saldous)) %>% 
  mutate(opsCum=n()) %>% 
  ungroup()

write_rds(bdcCon1xmes,'D:/!bso/Consultas/Consultas1XMesNov20Ene23.rds')
####____LECTURA DE BDCXCONSULTAS DEPURADO____####
bdcCon1xmes <- readRDS('D:/!bso/Consultas/Consultas1XMesNov20Ene23.rds')

check1xmes <- bdcCon1xmes %>% 
  group_by(myCon) %>% 
  summarise(saldoCon=sum(saldous),nOpsCon=n()) %>% 
  ungroup() %>%
  mutate(myCancel=myCon) %>% 
  select(-myCon) %>% 
  glimpse()
####____READING DFCANCEL AND AMORT____####
dfCancel <- readRDS("D:/!bso/Consultas/dfCancelEne15Feb23.rds") %>% 
  mutate(myCancel=as.yearmon(mCancel)) %>% select(-mCancel) %>% 
  mutate(tipoCancel=case_when(saldous/montous<=0.5~'Cancelado <= 0.5',
                              saldous/montous>0.5~'Cancelado > 0.5')) %>% 
  rename(saldoCancel=saldous)
  
checkCancel <- dfCancel %>% 
  group_by(myCancel) %>% 
  summarise(saldo=sum(saldoCancel),nOps=n()) %>% 
  ungroup()
print(checkCancel)
#Check unique cancel
checkCancel2 <- dfCancel %>% 
  group_by(OPERACION) %>% 
  mutate(n=n()) %>% 
  arrange(desc(n),OPERACION) %>% 
  dplyr::filter(n>1)
##
amorFull <- readRDS("D:/!bso/Consultas/amortFullEne17Ene23.rds") %>% 
  mutate(tipoCancel='Amortizado') %>% 
  mutate(saldoCancel = susPrevAmor-susAmor) %>% 
  select(-myPrevAmor,-susPrevAmor,-susAmor) %>% 
  mutate(yearCancel=year(myAmor),
         mesCancel=month(myAmor)) %>% 
  rename(myCancel=myAmor) %>% 
  glimpse()

checkAmor <- amorFull %>% 
  group_by(myCancel) %>% 
  summarise(saldo=sum(saldoCancel),nOps=n()) %>%
  ungroup()

Cancel <- dfCancel %>% 
  bind_rows(amorFull) %>% 
  group_by(OPERACION) %>% 
  arrange(desc(myCancel)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  dplyr::filter(myCancel>'dic. 2016')

write_rds(Cancel,'D:/!bso/Consultas/CancelAmortEne17Ene23.rds')
Cancel <- readRDS('D:/!bso/Consultas/CancelAmortEne17Ene23.rds')

checkCan <- Cancel %>% 
  dplyr::filter(myCancel>'feb. 2021') %>% 
  group_by(myCancel,tipoCancel) %>% 
  summarise(Saldo_USD=sum(saldoCancel),Operaciones=n()) %>% 
  ungroup() %>% 
  arrange(myCancel) %>% 
  pivot_wider(names_from = tipoCancel,values_from = c(Saldo_USD,Operaciones))

Cancelados <- Cancel %>% 
  dplyr::filter(myCancel>'feb. 2021') %>% 
  group_by(myCancel,tipoCancel) %>% 
  summarise(SaldoCancelTot=sum(saldoCancel),OpsCancelTot=n())
####____JOINING CANCELADOS CON CONSULTADOS____####
bdcConCan <- bdcCon1xmes %>% 
  left_join(Cancel,by=c("OPERACION","CTACLIENTE")) %>% 
  dplyr::filter(myCon<=myCancel & myCon>=myCancel-2/12) %>%
  mutate(difMes= case_when(myCon==myCancel~0,
                           myCon==myCancel-1/12~1,
                           myCon==myCancel-2/12~2,))

check3 <- bdcConCan %>% 
  group_by(myCancel,difMes,tipoCancel) %>% 
  summarise(nOps=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = difMes,values_from = nOps)

#Sin embargo, como estamos considerando consultas hasta 2 meses antes de la cancelación
#Debemos quedarnos con el mayor mes consultado para cada operación

bdcCanLastCon <- bdcCon1xmes %>% 
  mutate(ENTIDAD = gsub(' SA', ' S.A.', ENTIDAD)) %>% 
  left_join(Cancel,by=c("OPERACION","CTACLIENTE")) %>% 
  dplyr::filter(myCon<=myCancel & myCon>=myCancel-2/12) %>%
  group_by(OPERACION) %>% 
  arrange(desc(myCon)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(difMes= case_when(myCon==myCancel~0,
                           myCon==myCancel-1/12~1,
                           myCon==myCancel-2/12~2,)) %>% 
  select(-pos,-montous.x) %>% 
  left_join(check1xmes,by="myCancel") %>%
  left_join(Cancelados,by="myCancel") %>% 
  dplyr::filter(myCancel>'feb. 2021') %>% 
  glimpse()
  
bdcCanLastCon %>% 
  dplyr::filter(myCancel>='dic. 2021') %>% 
  group_by(myCancel,difMes) %>% 
  tally() %>% 
  ungroup() %>% 
  pivot_wider(names_from = difMes,values_from = n) %>% 
  adorn_totals(c('row','col'))
write_rds(bdcCanLastCon,'D:/!bso/Consultas/CanLastConEne21Ene23.rds')
bdcCanLastCon <- readRDS('D:/!bso/Consultas/CanLastConEne21Ene23.rds')

lastday <- data.frame(dia=seq.Date(as.Date("2016-01-01"),as.Date("2023-12-31"),by="1 day")) %>% 
  mutate(my=as.yearmon(dia)) %>% 
  group_by(my) %>% 
  summarise(maxdia=max(dia))

bdcCanLastCon2 <- bdcCanLastCon %>% 
  left_join(lastday %>% rename(myCancel=my,fCancel=maxdia),by="myCancel") %>% 
  left_join(lastday %>% rename(myCon=my,fCon=maxdia),by="myCon")
  
write.xlsx(bdcCanLastCon2,'D:/!bso/Consultas/CanLastConMar21Ene23.xlsx')
####____READING CONSULTADOS CANCELADOS LAST # MONTHS____####
bdcCanLastCon <- readRDS('D:/!bso/Consultas/CanLastConEne21Ene23.rds')

check4 <- bdcCanLastCon %>% 
  group_by(myCancel,difMes,tipoCancel) %>% 
  summarise(nOps=n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = difMes,values_from = nOps)

####____FIGURAS____####
#GRAFICO DE EVOLUCION DE CANCELADO Y RATIO CONCAN/CANCELADO
Cancelados <- Cancel %>% 
  dplyr::filter(myCancel>'feb. 2021') %>% 
  group_by(myCancel,tipoCancel) %>% 
  summarise(SaldoCancelTot=sum(saldoCancel),OpsCancelTot=n()) %>% 
  ungroup() %>% 
  mutate(SaldoCancelTot=SaldoCancelTot/1e6,OpsCancelTot=OpsCancelTot/1e3)

RatioC <- bdcCanLastCon %>% 
  dplyr::filter(difMes==0) %>% 
  group_by(myCancel) %>% 
  summarise(saldous=sum(saldoCancel),nOps=n(),ratio=sum(saldoCancel/SaldoCancelTot)*100,
            ration=sum(Consultado/OpsCancelTot)*100,saldoCan=max(SaldoCancelTot),nCan=max(OpsCancelTot)) %>% 
  ungroup() 
scalefac <- max(RatioC$saldoCan/1e6)/max(RatioC$ratio)
ggplot(Cancelados,aes(x=myCancel,y=SaldoCancelTot,fill=tipoCancel))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(SaldoCancelTot,1)),color="white",size=3,
            position = position_stack(vjust=0.5))+
  annotate(geom = "line",x=RatioC$myCancel,y=RatioC$ratio*scalefac,
           color=paleta(12)[8],size=1.15)+
  annotate(geom = "label",x=RatioC$myCancel,y=RatioC$ratio*scalefac,
           label=round(RatioC$ratio,1),color=paleta(12)[8],size=3)+
  labs(x="Mes",y="Saldo cancelado/amortizado (MM USD)",fill="Tipo de amortización",
       title = "Evolución de saldo cancelado-amortizado")+
  scale_y_continuous(breaks=seq(0,50,10),labels = scales::comma,sec.axis = 
                       sec_axis(~./scalefac,name="Consultado-Cancelado/Cancel-Amort (%)"))+
  scale_x_continuous(breaks=as.numeric(RatioC$myCancel),labels = format(RatioC$myCancel,"%m/%y"))+
  scale_fill_manual(values = paleta(8)[3:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[8]),
        axis.text.x.bottom = element_text(angle = 90),
        legend.position = "bottom")

ggplot(RatioC,aes(x=myCancel,y=saldoCan/1e6,fill=tipoCancel))+
  geom_bar(stat="identity",size=1.5,fill=paleta(12)[2])+
  geom_line(aes(y=ratio*scalefac),size=1.5,color=paleta(12)[8])+
  geom_label(aes(y=ratio*scalefac-2,label=round(ratio,1)),size=3.5,
             color=paleta(12)[8])+
  geom_text(aes(label=round(saldoCan/1e6,1)),size=3.5,color="white",
            position = position_stack(vjust = 0.25),angle=90)+
  labs(x="Mes",y="Saldo cancelado/amortizado (MM USD)",
       title = "Evolución de saldo cancelado-amortizado")+
  scale_y_continuous(breaks=seq(0,50,10),labels = scales::comma,sec.axis = 
                       sec_axis(~./scalefac,name="Consultado-Cancelado/Cancel-Amort (%)"))+
  scale_x_continuous(breaks=as.numeric(RatioC$myCancel),labels = format(RatioC$myCancel,"%m/%y"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[8]),
        axis.text.x.bottom = element_text(angle = 90))

scalefac <- max(RatioC$nCan/1e3)/max(RatioC$ration)
ggplot(RatioC,aes(x=myCancel,y=nCan/1e3))+
  geom_bar(stat="identity",size=1.5,fill=paleta(12)[2])+
  geom_line(aes(y=ration*scalefac),size=1.5,color=paleta(12)[8])+
  geom_label(aes(y=ration*scalefac-2,label=round(ration,1)),size=3.5,
             color=paleta(12)[8])+
  geom_text(aes(label=round(nCan/1e3,1)),size=3.5,color="white",
            position = position_stack(vjust = 0.25),angle=90)+
  labs(x="Mes",y="Ops. cancelado/amortizado (en miles)",
       title = "Evolución de operaciones canceladas/amortizadas")+
  scale_y_continuous(breaks=seq(0,50,10),labels = scales::comma,sec.axis = 
                       sec_axis(~./scalefac,name="Consultado-Cancelado/Cancel-Amort (%)"))+
  scale_x_continuous(breaks=as.numeric(RatioC$myCancel),labels = format(RatioC$myCancel,"%m/%y"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[8]),
        axis.text.x.bottom = element_text(angle = 90))
#GRAFICO DE EVOLUCION DE CONSULTAS Y RATIO DE CONVERSION (MES 0)
Ratio <- bdcCanLastCon %>%
  dplyr::filter(difMes==0) %>% 
  group_by(myCancel) %>% 
  summarise(saldous=sum(saldoCancel),nOps=n(),ratio=sum(saldoCancel/saldoCon)*100,
            ration=sum(Consultado/nOpsCon)*100,saldoCon=max(saldoCon),nCon=max(nOpsCon)) %>% 
  ungroup()
scalefac <- max(Ratio$saldoCon/1e6)/max(Ratio$ratio)
ggplot(Ratio,aes(x=myCancel,y=saldoCon/1e6))+
  geom_bar(stat="identity",size=1.5,fill=paleta(12)[2])+
  geom_line(aes(y=ratio*scalefac),size=1.5,color=paleta(12)[8])+
  geom_label(aes(y=ratio*scalefac-5,label=round(ratio,1)),size=3.5,
             color=paleta(12)[8])+
  geom_text(aes(label=round(saldoCon/1e6,1)),size=3.5,color="white",
            position = position_stack(vjust = 0.25),angle=90)+
  labs(x="Mes",y="Saldo consultado (MM USD)",
       title = "Evolución de saldo consultado y ratio de conversión")+
  scale_y_continuous(breaks=seq(0,125,25),labels = scales::comma,sec.axis = 
                       sec_axis(~./scalefac,name="Ratio de conversión (%)"))+
  scale_x_continuous(breaks=as.numeric(Ratio$myCancel),labels = format(Ratio$myCancel,"%m/%y"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[8]),
        axis.text.x.bottom = element_text(angle = 90))

scalefac <- max(Ratio$nCon/1e3)/max(Ratio$ration)
ggplot(Ratio,aes(x=myCancel,y=nCon/1e3))+
  geom_bar(stat="identity",size=1.5,fill=paleta(12)[2])+
  geom_line(aes(y=ration*scalefac),size=1.5,color=paleta(12)[8])+
  geom_label(aes(y=ration*scalefac-1,label=round(ration,1)),size=3.5,
             color=paleta(12)[8])+
  geom_text(aes(label=round(nCon/1e3,2)),size=3.5,color="white",
            position = position_stack(vjust = 0.5),angle=90)+
  labs(x="Mes",y="Operaciones consultadas (en miles)",
       title = "Evolución de operaciones consultadas y ratio de conversión")+
  scale_y_continuous(breaks=seq(0,20,2),labels = scales::comma,sec.axis = 
                       sec_axis(~./scalefac,name="Ratio de conversión (%)"))+
  scale_x_continuous(breaks=as.numeric(Ratio$myCancel),labels = format(Ratio$myCancel,"%m/%y"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[8]),
        axis.text.x.bottom = element_text(angle = 90))
#GRAFICO DE EVOLUCION DE CONSULTAS Y RATIO DE CONVERSION
Ratio <- bdcCanLastCon %>% 
  group_by(myCancel) %>% 
  summarise(saldous=sum(saldoCancel),nOps=n(),ratio=sum(saldoCancel/saldoCon)*100,
            ration=sum(Consultado/nOpsCon)*100,saldoCon=max(saldoCon),nCon=max(nOpsCon)) %>% 
  ungroup()

scalefac <- max(Ratio$saldoCon/1e6)/max(Ratio$ratio)

ggplot(Ratio,aes(x=myCancel,y=saldoCon/1e6))+
  geom_bar(stat="identity",size=1.5,fill=paleta(12)[2])+
  geom_line(aes(y=ratio*scalefac),size=1.5,color=paleta(12)[8])+
  geom_label(aes(y=ratio*scalefac-5,label=round(ratio,1)),size=3.5,
            color=paleta(12)[8])+
  geom_text(aes(label=round(saldoCon/1e6,1)),size=3.5,color="white",
            position = position_stack(vjust = 0.25),angle=90)+
  labs(x="Mes",y="Saldo consultado (MM USD)",
       title = "Evolución de saldo consultado y ratio de conversión")+
  scale_y_continuous(breaks=seq(0,125,25),labels = scales::comma,sec.axis = 
                       sec_axis(~./scalefac,name="Ratio de conversión (%)"))+
  scale_x_continuous(breaks=as.numeric(Ratio$myCancel),labels = format(Ratio$myCancel,"%m/%y"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[8]),
        axis.text.x.bottom = element_text(angle = 90))

scalefac <- max(Ratio$nCon/1e3)/max(Ratio$ration)
ggplot(Ratio,aes(x=myCancel,y=nCon/1e3))+
  geom_bar(stat="identity",size=1.5,fill=paleta(12)[2])+
  geom_line(aes(y=ration*scalefac),size=1.5,color=paleta(12)[8])+
  geom_label(aes(y=ration*scalefac-1,label=round(ration,1)),size=3.5,
            color=paleta(12)[8])+
  geom_text(aes(label=round(nCon/1e3,2)),size=3.5,color="white",
            position = position_stack(vjust = 0.5),angle=90)+
  labs(x="Mes",y="Operaciones consultadas (en miles)",
       title = "Evolución de operaciones consultadas y ratio de conversión")+
  scale_y_continuous(breaks=seq(0,20,2),labels = scales::comma,sec.axis = 
                       sec_axis(~./scalefac,name="Ratio de conversión (%)"))+
  scale_x_continuous(breaks=as.numeric(Ratio$myCancel),labels = format(Ratio$myCancel,"%m/%y"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[8]),
        axis.text.x.bottom = element_text(angle = 90))

#GRAFICO EVOLUTIVO DE GRUPO ENTIDAD
Group_Ent <- bdcCanLastCon %>% 
  mutate(grupo_Entidad=ifelse(grupo_Entidad=="Fondo","Otros",grupo_Entidad)) %>% 
  group_by(myCancel,grupo_Entidad) %>% 
  summarise(saldous=sum(saldoCancel),nOps=n()) %>% 
  ungroup() %>% 
  group_by(myCancel) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>% 
  mutate(pctn=nOps/sum(nOps)*100) %>% 
  ungroup() %>% 
  mutate(grupo_Entidad=fct_reorder(grupo_Entidad,saldous))


labels <- Group_Ent %>% 
  group_by(myCancel) %>% 
  summarise(saldo=sum(saldous)/1e6,nOps=sum(nOps))

#Gráfico de participación de tipo de entidad por saldo
ggplot(Group_Ent,aes(x=myCancel,y=saldous/1e6,fill=grupo_Entidad))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pct,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Saldo cancelado consultado (MM USD)",
       title = "Participación de Tipo Entidad en saldo cancelado consultado",
       fill="Tipo de Entidad")+
  annotate(geom="label",x=labels$myCancel,y=labels$saldo+0.5,
           label=round(labels$saldo,2),color=paleta(12)[8],size=3.5)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks=as.numeric(Group_Ent$myCancel),labels = format(Group_Ent$myCancel,"%m/%y"))+
  scale_fill_manual(values = paleta(5)[4:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom",
        axis.text.x.bottom = element_text(angle=90))

ggplot(Group_Ent,aes(x=myCancel,y=nOps,fill=grupo_Entidad))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pctn,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Operaciones canceladas consultadas",
       title = "Participación de Tipo Entidad en operaciones canceladas consultadas",
       fill="Tipo de Entidad")+
  annotate(geom="label",x=labels$myCancel,y=labels$nOps+100,
           label=comma(round(labels$nOps,2)),color=paleta(12)[8],size=3.5)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks=as.numeric(Group_Ent$myCancel),labels = format(Group_Ent$myCancel,"%m/%y"))+
  scale_fill_manual(values = paleta(5)[4:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom", 
        axis.text.x.bottom = element_text(angle=90))

#GRAFICO DE PARTICIPACION DE BANCOS
Banco <- bdcCanLastCon %>% 
  dplyr::filter(grupo_Entidad=="Banco") %>%
  mutate(ENTIDAD=ifelse(ENTIDAD=="BANCO FORTALEZA SA","BANCO FORTALEZA S.A.",ENTIDAD)) %>% 
  mutate(ENTIDAD=ifelse(ENTIDAD=="BANCO FIE SA","BANCO FIE S.A.",ENTIDAD)) %>% 
  group_by(myCancel,ENTIDAD) %>% 
  summarise(saldous=sum(saldoCancel),nOps=n()) %>% 
  ungroup() %>% 
  group_by(myCancel) %>% 
  arrange(desc(saldous)) %>% 
  mutate(pos=row_number()) %>% 
  ungroup() %>% 
  mutate(ENTIDAD=ifelse(ENTIDAD %in% ENTIDAD[pos<=5 & myCancel=="Nov. 2022"],ENTIDAD,"Otros")) %>% 
  group_by(myCancel,ENTIDAD) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(myCancel) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>%
  mutate(pctn=nOps/sum(nOps)*100) %>%
  ungroup() %>%
  mutate(ENTIDAD=fct_reorder(ENTIDAD,pct))

labels <- Banco %>% 
  group_by(myCancel) %>% 
  summarise(saldo=sum(saldous)/1e6,nOps=sum(nOps))

#Gráfico de participación de banco en saldo
ggplot(Banco,aes(x=myCancel,y=pct,fill=ENTIDAD))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pct,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Participación en saldo cancelado consultado (%)",
       title = "Participación de Banco en saldo cancelado consultado",
       fill="Banco")+
  # annotate(geom='text',x=labels$myCancel,y=105,
  #          label=paste(round(labels$saldo,1),'MM USD'),color=paleta(12)[8],size=3.5,angle=45)+
  scale_fill_manual(values = paleta(8)[6:1])+
  scale_x_continuous(breaks=as.numeric(Banco$myCancel),labels = format(Banco$myCancel,"%m/%y"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom",
        axis.text.x.bottom = element_text(angle=90))

#Gráfico de participación de banco en operaciones
ggplot(Banco,aes(x=myCancel,y=pctn,fill=ENTIDAD))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pctn,1)),color="white",size=3,
            position = position_stack(vjust=0.75))+
  labs(x="Mes",y="Participación en ops. canceladas consultadas (%)",
       title = "Participación de Banco en operaciones canceladas consultadas",
       fill="Banco")+
  scale_fill_manual(values = paleta(8)[6:1])+
  scale_x_continuous(breaks=as.numeric(Banco$myCancel),labels = format(Banco$myCancel,"%m/%y"))+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom",
        axis.text.x.bottom = element_text(angle=90))

#GRAFICO POR TIPO CANCELACION
TipoCan <- bdcCanLastCon %>% 
  group_by(myCancel,tipoCancel) %>% 
  summarise(saldous=sum(saldoCancel),nOps=n()) %>% 
  ungroup() %>% 
  group_by(myCancel) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>% 
  mutate(pctn=nOps/sum(nOps)*100) %>% 
  ungroup() %>% 
  mutate(tipoCancel=fct_reorder(tipoCancel,pctn))

ggplot(TipoCan,aes(x=myCancel,y=pctn,fill=tipoCancel)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pctn,1)),size=3,color="white",
            position=position_stack(vjust=0.5))+
  labs(x="Mes",y="Participación (%)", fill="Tipo de Cancelación",
       title="Participación de tipo de cancelación o amortización")+
  scale_x_continuous(breaks=as.numeric(TipoCan$myCancel),labels = format(TipoCan$myCancel,"%m/%y"))+
  scale_fill_manual(values = paleta(4)[3:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom",
        axis.text.x.bottom = element_text(angle=90))

#GRAFICO POR DISTANCIA DE CONSULTA CON CANCELAMORT
difMes <- bdcCanLastCon %>% 
  group_by(myCancel,difMes) %>% 
  summarise(saldous=sum(saldoCancel),nOps=n()) %>% 
  ungroup() %>% 
  group_by(myCancel) %>% 
  mutate(pct=saldous/sum(saldous)*100) %>% 
  mutate(pctn=nOps/sum(nOps)*100) %>% 
  ungroup() %>% 
  mutate(difMes=fct_reorder(factor(difMes),pctn))

ggplot(difMes,aes(x=myCancel,y=pctn,fill=difMes)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pctn,1)),size=3,color="white",
            position=position_stack(vjust=0.5))+
  labs(x="Mes",y="Participación (%)", fill="Meses de consulta antes de Cancelación",
       title="Participación de mes antes de la cancelación/amortización")+
  scale_x_continuous(breaks=as.numeric(difMes$myCancel),labels = format(difMes$myCancel,"%m/%y"))+
  scale_fill_manual(values = paleta(4)[3:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom",
        axis.text.x.bottom = element_text(angle=90))
#GRAFICO POR SUCURSAL
sucursal <- bdcCanLastCon %>% 
  group_by(myCancel) %>% 
  mutate(saldopct=saldoCancel/sum(saldoCancel)*100, 
         npct=Consultado/sum(Consultado)*100) %>% 
  group_by(myCancel,sucursal) %>% 
  summarise(saldous=sum(saldoCancel),pct=sum(saldopct),
            nOps=n(),pctn=sum(npct)) %>% 
  ungroup() %>%
  mutate(sucursal=ifelse(sucursal %in% sucursal[pct<5 & myCancel=='Ene. 2023'],'Otros',sucursal)) %>% 
  group_by(myCancel,sucursal) %>% 
  summarise_all(sum) %>% 
  mutate(sucursal=fct_reorder(factor(sucursal),pctn))  

ggplot(sucursal,aes(x=myCancel,y=pct,fill=sucursal)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pct,1)),size=3,color="white",
            position=position_stack(vjust=0.5))+
  labs(x="Mes",y="Participación en saldo cancelado consultado (%)", fill="Meses antes de cancelación",
       title="Participación de sucursal en saldo cancelado consultado")+
  scale_x_continuous(breaks=as.numeric(sucursal$myCancel),labels = format(sucursal$myCancel,"%m/%y"))+
  scale_fill_manual(values = paleta(8)[5:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom",
        axis.text.x.bottom = element_text(angle=90))

ggplot(sucursal,aes(x=myCancel,y=pctn,fill=sucursal)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=round(pctn,1)),size=3,color="white",
            position=position_stack(vjust=0.5))+
  labs(x="Mes",y="Participación en ops. canceladas consultadas (%)", fill="Meses antes de cancelación",
       title="Participación de sucursal en saldo cancelado consultado")+
  scale_x_continuous(breaks=as.numeric(sucursal$myCancel),labels = format(sucursal$myCancel,"%m/%y"))+
  scale_fill_manual(values = paleta(8)[5:1])+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"),
        legend.position = "bottom",
        axis.text.x.bottom = element_text(angle=90))
