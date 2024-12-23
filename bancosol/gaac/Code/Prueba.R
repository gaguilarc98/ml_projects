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
####___CREACION DE BDC FICTICIA____####
mes <- c("202112","202201","202202","202203","202204","202205")
baseList <- list()
for (i in 1:length(mes)) {
  if(i==1){
    sz1 <- 1e5
    set.seed(i)
    key1 <- sample(1e2:5e5,size=sz1,replace = TRUE)
    key2 <- sample(1e5:2e5,size=sz1,replace = FALSE)
    saldo <- rnorm(sz1,7000,1500)
    ESTADO <- sample(c(rep("VIGENTE",30),rep("VENCIDA",4),"EJECUCIÓN",rep("CASTIGADA",5)),sz1,replace = TRUE)
    CALIFICACION <- sample(c(rep("A",30),rep("B",2),"C","D","E",rep("F",5)),sz1,replace = TRUE)
    
    BASE_CARTERA <- data.frame(ID=key1,OPERACION=key2,SALDO=saldo,ESTADO=ESTADO,
                               CALIFICACION=CALIFICACION) %>% 
      mutate(FECHA = mes[i])
    baseList[[i]] <- BASE_CARTERA
  }
  else{
    sz2 <- 5e3
    set.seed(i)
    key1 <- sample(1e2:5e5,size=sz2,replace = TRUE)
    key2 <- sample(1e5:2e5,size=sz2,replace = FALSE)
    saldo <- rnorm(sz2,7000,1500)
    ESTADO <- sample(c(rep("VIGENTE",30),rep("VENCIDA",4),"EJECUCIÓN",rep("CASTIGADA",5)),sz2,replace = TRUE)
    CALIFICACION <- sample(c(rep("A",30),rep("B",2),"C","D","E",rep("F",5)),sz2,replace = TRUE)
    base <- data.frame(ID=key1,OPERACION=key2,SALDO=saldo,ESTADO=ESTADO,
                       CALIFICACION=CALIFICACION)
    BASE_CARTERA <- BASE_CARTERA %>% 
      dplyr::filter(row_number()<=runif(1,n()*0.96,n())) %>%
      mutate(rat = runif(n(),0.90,1)) %>% 
      mutate(SALDO = SALDO*rat) %>% 
      mutate(CALIFICACION = case_when(CALIFICACION=="A" & rat>0.999~"B",
                                      CALIFICACION=="B" & rat>0.999~"C",
                                      CALIFICACION=="C" & rat>0.999~"D",
                                      CALIFICACION=="E" & rat>0.999~"F",
                                      TRUE~CALIFICACION)) %>% 
      select(-rat) %>% 
      bind_rows(base) %>% 
      mutate(FECHA = mes[i])
    baseList[[i]] <- BASE_CARTERA
  }
}

baseFull <- rbindlist(baseList)
table(baseFull$FECHA)
####____CREACION SF FICTICIA____####
set.seed(1234)
SF <- baseFull %>% 
  group_by(ID,OPERACION) %>% 
  mutate(ID_SF = sample(1e8:1e9,size = 1,replace = FALSE)) %>% 
  ungroup() %>% 
  group_by(FECHA) %>%
  sample_n(ceiling(runif(1,4.5e4,5e4))) %>% 
  mutate(TIPO_DEUDOR = sample(c(rep("Deudor",50),rep("Codeudor",4),rep("Garante",2)),size=n(),replace = TRUE)) %>% 
  ungroup() %>% 
  mutate(NRO_OPERACION = paste(ID,OPERACION,sep="-")) %>% 
  mutate(SALDO = SALDO) %>% 
  select(-ESTADO,-ID,-OPERACION) %>% 
  mutate(ENTIDAD = "MiBanco")

sfList <- list()

for (i in 1:length(mes)) {
  if(i<=1){
    set.seed(i+200)
    tam0 <- ceiling(runif(1,5e4,6e4)*exp(i/20))
    aux <- SF$ID_SF[SF$FECHA==mes[i]]
    key <- c(aux,sample(SF$ID_SF[SF$FECHA==mes[i]],size=(tam0-length(aux)),replace = TRUE))
    op <- as.character(sample(1e5:9e5,size=tam0,replace = FALSE))
    saldo <- rnorm(tam0,8000,1500)*exp(i/20)
    CALIFICACION <- sample(c(rep("A",30),rep("B",5),"C","D","E",rep("F",10)),tam0,replace = TRUE)
    ENTIDAD <- sample(c("Banco1",rep("Banco2",3),rep("Banco3",2),"Banco4"),size = tam0,replace = TRUE)
    DEUDOR <- sample(c(rep("Deudor",50),rep("Codeudor",4),rep("Garante",2)),size=tam0,replace = TRUE)
    BASE_SF <- data.frame(ID_SF=key, NRO_OPERACION=op, SALDO=saldo,
                          CALIFICACION=CALIFICACION, ENTIDAD=ENTIDAD, TIPO_DEUDOR=DEUDOR) %>% 
      mutate(FECHA = mes[i])
    sfList[[i]] <- BASE_SF
  }
  else{
    set.seed(i+200)
    tam <- ceiling(runif(1,5e4,6e4)*exp(i/20))
    aux <- SF$ID_SF[SF$FECHA==mes[i]]
    key <- c(aux,sample(SF$ID_SF[SF$FECHA==mes[i]],size=(tam-length(aux)),replace = TRUE))
    op <- as.character(sample(1e5:9e5,size=tam,replace = FALSE))
    saldo <- rnorm(tam,8000,1500)*exp(i/20)
    CALIFICACION <- sample(c(rep("A",30),rep("B",5),"C","D","E",rep("F",10)),tam,replace = TRUE)
    ENTIDAD <- sample(c("Banco1",rep("Banco2",3),rep("Banco3",2),"Banco4"),size = tam,replace = TRUE)
    DEUDOR <- sample(c(rep("Deudor",50),rep("Codeudor",4),rep("Garante",2)),size=tam,replace = TRUE)
    
    base <- data.frame(ID_SF=key,NRO_OPERACION=op,SALDO=saldo,
                       CALIFICACION=CALIFICACION, ENTIDAD=ENTIDAD, TIPO_DEUDOR=DEUDOR) %>% 
      mutate(FECHA = mes[i])
    # BASE_SF <- BASE_SF %>% 
    #   sample_n(ceiling(n()*runif(1,0.10,0.20))) %>% 
    #   mutate(rat = runif(n(),0.90,1)) %>% 
    #   mutate(SALDO = SALDO*rat) %>% 
    #   mutate(CALIFICACION = case_when(CALIFICACION=="A" & rat>0.999~"B",
    #                                   CALIFICACION=="B" & rat>0.999~"C",
    #                                   CALIFICACION=="C" & rat>0.999~"D",
    #                                   CALIFICACION=="E" & rat>0.999~"F",
    #                                   TRUE~CALIFICACION)) %>% 
    #   select(-rat) %>% 
    #   bind_rows(base) %>% 
    #   mutate(FECHA = mes[i])
    sfList[[i]] <- base
  }
}
table(BASE_SF$CALIFICACION)
  
SFExt <- rbindlist(sfList)

table(SFExt$FECHA,SFExt$CALIFICACION)
table(SF$FECHA,SF$CALIFICACION)

SFTotal <- SF %>% 
  bind_rows(SFExt) %>% 
  arrange(FECHA,SALDO) 
  

table(SFTotal$FECHA,SFTotal$CALIFICACION)
table(SFTotal$FECHA,SFTotal$ENTIDAD)

####____BASES FINALES____####
set.seed(12345)
mes <- c("202201","202202","202203","202204","202205")
junk <- data.frame(ID = sample(1:1e2,size=30),
                   OPERACION = sample(2e5:3e5,size=30),
                   SALDO = rnorm(30,500,100),
                   CALIFICACION = c("B","F","C","D","F",rep(NA,25)),
                   ESTADO = sample(c(rep("VIGENTE",3),NA),30,replace = TRUE),
                   FECHA = c(rep("202112",5),sample(c(mes,"202112","202111"),25,replace = TRUE)))
baseFinal <- baseFull %>% 
  bind_rows(junk) %>% 
  dplyr::filter(FECHA %in% mes)
SFTotal <- SFTotal %>% 
  dplyr::filter(FECHA %in% mes) %>% 
  mutate(FECHA = case_when(FECHA =="202201"~"Ene2022",
                           FECHA =="202202"~"Feb2022",
                           FECHA =="202203"~"Mar2022",
                           FECHA =="202204"~"Abr2022",
                           FECHA =="202205"~"May2022",)) %>% 
  relocate(SALDO,CALIFICACION,.after = ENTIDAD)

fwrite(baseFinal, "D:/Files/BDC_MiBanco_Ene22_May22.csv",row.names = F)
fwrite(SFTotal, "D:/Files/Info_SF_Ene22_May22.csv",row.names = F)
SFTotal <- fread("D:/Files/Info_SF_Ene22_May22.csv",encoding = "UTF-8",sep=",",fill = T) %>% 
  select(-CALIFICACION)
fwrite(SFTotal, "D:/Files/Info_SF_Ene22_May22.csv",row.names = F)
####____PREGUNTAS Y SOLUCIONES_____####
#El propósito final es obtener la peor calificación de los clientes que MiBanco
#comparte con el SF, y una proyección del saldo a 1 y 2 meses. Para ello se cuenta 
#con baseFinal que contiene los cierres mensuales desde enero hasta mayo de 2022.
#1. Como se desea comparar la calificación de los clientes con las del sistema, se debe
#remover la información incompleta en el campo calificación, y en lo posible remover 
#cualquier otra impureza (ESTADO, FECHA fuera del campo)
#(OPCIONAL) Generar un reporte de datos faltantes en cada columna de MiBanco
mes <- c("202201","202202","202203","202204","202205")
sapply(baseFinal, function(x){length(which(is.na(x)))})
BaseLimpia <- baseFinal %>% 
  dplyr::filter(!is.na(CALIFICACION), !is.na(ESTADO)) %>% 
  dplyr::filter(FECHA %in% mes) 
sapply(BaseLimpia, function(x){length(which(is.na(x)))})
#2. Comprobar si no hay datos faltantes en la base del Sistema Financiero
#(OPCIONAL) Generar un reporte de datos faltantes en cada columna del SF
sapply(SFTotal, function(x){length(which(is.na(x)))})

#3. Obtener los campos ID y OPERACION en la base SF a partir de NRO_OPERACION. Nota: para
#miBanco el NRO_OPERACION es la concatenación de ID y OPERACION separados por "-"
SFSep <- SFTotal %>% 
  separate_wider_delim(NRO_OPERACION,delim="-",names = c("ID","OPERACION"),
                       too_few = "align_start",too_many = "drop") %>% 
  mutate(ID=as.numeric(ID)) %>% 
  mutate(OPERACION=as.numeric(OPERACION)) %>% 
  dplyr::filter(TIPO_DEUDOR=="Deudor") %>% 
  mutate(FECHA = case_when(FECHA =="Ene2022"~"202201",
                           FECHA =="Feb2022"~"202202",
                           FECHA =="Mar2022"~"202203",
                           FECHA =="Abr2022"~"202204",
                           FECHA =="May2022"~"202205",))

#4. Para cada persona, identificada con su codigo en el SF, obtener su peor
#calificación en el SF (sin tomar en cuenta la 
#calificación de miBanco). Para ello agrupar por el ID_SF y buscar la peor calificación
#cuando la Entidad sea diferente a MiBanco. Por otro lado, obtener el saldo total fuera
#de MiBanco (el saldo en Saldo_SF está en Bs. así que se debe convertir a dólares)

#5. Quedarse solo con las filas que pertenecen a miBanco
SFPeor <- SFSep %>% 
  mutate(esMiBanco = ifelse(ENTIDAD=="MiBanco",1,0)) %>% 
  mutate(SALDO = SALDO) %>% 
  group_by(FECHA,ID_SF,esMiBanco) %>% 
  mutate(peorCalif = ifelse(CALIFICACION==max(CALIFICACION),1,0)) %>% 
  ungroup() %>% 
  group_by(FECHA,ID_SF) %>% 
  mutate(PEOR_ENTIDAD = ifelse(peorCalif==1 & esMiBanco==0,ENTIDAD,'_')) %>% 
  mutate(PEOR_CALIF = ifelse(peorCalif==1 & esMiBanco==0,CALIFICACION,'_')) %>% 
  mutate(across(PEOR_ENTIDAD:PEOR_CALIF,~max(.x,na.rm = T))) %>% 
  mutate(SALDO_NO_BAN = sum(SALDO*(1-esMiBanco))) %>% 
  ungroup() %>% 
  dplyr::filter(ENTIDAD=="MiBanco") %>% 
  select(-esMiBanco,-peorCalif) %>% 
  mutate(TIPO_DEUDOR = ifelse(SALDO>600 & SALDO<900,"Codeudor",TIPO_DEUDOR))

fwrite(SFPeor, "D:/Files/Info_SF_Ene22_May22.csv",row.names = F)
SFPeor <- SFPeor %>% 
  dplyr::filter(CalifPeor!="_")
#5. Unir la información del sistema con la de MiBanco utilizando el ID y la OPERACION
#como llaves únicas. Considerar que además de los campo señalados la FECHA también debe
#coincidir para evitar duplicar información
baseJoin <- SFPeor %>% 
  inner_join(BaseLimpia,by=c("FECHA","ID","OPERACION"),suffix=c("_BAN","_SF"))

#6. Comprobar la integridad de la unión asegurándose que la suma de saldos de Mi Banco de los
#que si se emparejaron sea el mismo para todos los meses
baseJoin %>% 
  dplyr::filter(esMiBanco==1) %>% 
  group_by(FECHA) %>% 
  summarise(SaldoMiBanco=sum(SALDO_BAN),SaldoSF=sum(SALDO_SF,na.rm = T),n=n(),Cl=n_distinct(ID))
#7. Consolidar la información a nivel de cliente para cada mes, es decir, agrupar por FECHA e ID y sumar su saldo en MiBanco
#Obtener su peor Calificación dentro de MiBanco y conservar su saldo en SF y su peor Calificación en SF

baseCliente <- baseJoin %>% 
  group_by(FECHA) %>% 
  summarise(CalifPeor_BAN = max(CALIFICACION_BAN,na.rm = T),
            CalifPeor_SF  = max(CalifPeor,na.rm = T),
            SaldoEnMiBanco = sum(SALDO_BAN,na.rm = T),
            SaldoEnSF = max(SaldoEnSF,na.rm = T))

#8. Crear un gráfico que muestre la evolución (para cada mes) del saldo en MiBanco que proviene 
#de clientes compartidos. En el mismo gráfico mostrar la evolución de la cantidad de clientes 
#que se comparten cada mes.
basePlot <- baseJoin %>% 
  group_by(FECHA) %>% 
  summarise(SaldoComp = sum(SaldoEnSF),
            NOps = n_distinct(ID)) %>% 
  mutate(mes = case_when(substr(FECHA,5,6)=="01"~"Ene",
                         substr(FECHA,5,6)=="02"~"Feb",
                         substr(FECHA,5,6)=="03"~"Mar",
                         substr(FECHA,5,6)=="04"~"Abr",
                         substr(FECHA,5,6)=="05"~"May",)) %>% 
  mutate(year=substr(FECHA,1,4)) %>% 
  mutate(monDate = as.yearmon(paste(mes,year,sep=". ")))
  
scalefac <- max(basePlot$SaldoComp/1e6)/max(basePlot$NOps)
ggplot(basePlot, aes(x=monDate, y=SaldoComp/1e6)) +
  geom_bar(stat = "identity",fill="darkblue")+
  geom_line(aes(x=monDate,y=NOps*scalefac),size=1.5,color="darkorange")+
  labs(x="FECHA",y="Saldo de clientes compartidos (en MM USD)")+
  scale_y_continuous(labels = comma,sec.axis=sec_axis(~./scalefac,name="Clientes"))+
  scale_x_continuous(breaks = as.numeric(basePlot$monDate),labels = format(basePlot$monDate,"%m/%y"))+
  theme_light()
#9. Crear un gráfico que muestre la evolución (para cada mes) de la composición 
#del saldo en SF de clientes compartidos según la Peor Calificación en SF
basePlot <- baseCliente %>% 
  group_by(FECHA) %>% 
  mutate(Saldo_Tot = sum(SaldoEnSF)) %>% 
  ungroup() %>% 
  group_by(FECHA,CalifPeor_SF) %>% 
  summarise(SaldoComp = sum(SaldoEnSF),
            Saldopct = SaldoComp/max(Saldo_Tot),
            NOps = n_distinct(ID)) %>% 
  mutate(mes = case_when(substr(FECHA,5,6)=="01"~"Ene",
                         substr(FECHA,5,6)=="02"~"Feb",
                         substr(FECHA,5,6)=="03"~"Mar",
                         substr(FECHA,5,6)=="04"~"Abr",
                         substr(FECHA,5,6)=="05"~"May",)) %>% 
  mutate(year=substr(FECHA,1,4)) %>% 
  mutate(monDate = as.yearmon(paste(mes,year,sep=". "))) %>% 
  mutate(lab = paste(round(Saldopct*100,1),"%"))

ggplot(basePlot, aes(x=monDate, y=Saldopct, fill=CalifPeor_SF)) +
  geom_bar(stat = "identity")+
  geom_label_repel(aes(label=lab),position = position_stack(vjust = 0.5))+
  labs(x="FECHA",y="Composición de saldo en SF (en MM USD)")+
  scale_y_continuous(labels = percent,sec.axis=sec_axis(~./scalefac,name="Clientes"))+
  scale_x_continuous(breaks = as.numeric(basePlot$monDate),labels = format(basePlot$monDate,"%m/%y"))+
  theme_light()
#8. Para el último mes crear una tabla de doble entrada donde se haga el recuento de operaciones 
#por Peor Calif en MiBanco y peor Calif en SF.
baseJoin %>% 
  dplyr::filter(FECHA=="202205") %>% 
  mutate(Ops=1) %>% 
  group_by(CalifPeor_BAN,CalifPeor_SF) %>% 
  summarise(Clientes = n_distinct(ID)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = CalifPeor_SF,values_from = Clientes) %>% 
  adorn_totals('col')
  
