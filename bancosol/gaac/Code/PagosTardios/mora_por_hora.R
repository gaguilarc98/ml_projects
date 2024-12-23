#-------------------------
####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(xlsx)
library(dplyr)
#library(foreign)
#library(reshape)
#library(reshape2)
#library(stringr)
library(lubridate)
#library(Hmisc)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(forecast)
library(quantmod)
library(astsa)
#library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
#library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
#library(timeSeries)
library(tseries)
library(xtable)
library(openxlsx)
#library(hrbrthemes)
#library(viridis)
library(scales)
library(janitor)
library(RColorBrewer)
library(paletteer)
library(plotly)

Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
paleta <- colorRampPalette(c("blue2","slateblue4","slateblue3","violetred3","red3","tan2","yellow3","yellow2"),bias=1.5)
cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
#___________________________________________________________
#####_____LECTURA de BASE DIARIA____####
diaria <- fread('D:/!bso/girCartera/BaseCartera_20220129.txt',
                encoding = 'Latin-1', fill = T)
dia <- diaria %>% 
dplyr::filter(MODULO != 131) %>%
  dplyr::filter(ESTADO != 'CASTIGADA') %>%
  mutate(activoAgo = 1) %>%
  mutate(fdes = dmy(FDESEMBOLSO)) %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>%
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  dplyr::filter(DIASMORA>0) %>% 
  select(OPERACION) %>% 
  rename(Operacion=OPERACION)
#---------------------------------------------------------
#####_____PARA ENERO_____####
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraDesdeEnero2022.csv')
P2 <- Pagos %>% 
  dplyr::filter(FechaPago>=as.Date('2022-01-30') & FechaPago<=as.Date('2022-01-31')) %>% 
  left_join(dia,by='Operacion')

diaPago <- dia %>% 
  left_join(P2,by='Operacion') %>% 
  dplyr::filter(!is.na(HoraPago)) %>% 
  mutate(hora=as.POSIXct(paste(FechaPago,HoraPago),format="%Y-%m-%d %H:%M:%S"))

bins <- seq.POSIXt(min(diaPago$hora),(max(diaPago$hora)+60*60),by="60 min") #Add to max 60 times the size of by

diaPago <- diaPago %>% 
  mutate(horabinned=cut(hora,breaks=bins)) %>% 
  select(horabinned,Operacion) %>% 
  group_by(horabinned) %>% 
  tally()

ggplot(diaPago,aes(x=horabinned,y=n))+
  geom_line(group=1,size=1.25)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x="Fecha",y="Número de Operaciones")
#___________________________________________________________
#####_____MENSUAL____####

fecha <- c('20220129','20220223','20220329','20220428','20220529',
           '20220628','20220729','20220829')
dias_base <- paste0('BaseCartera_',fecha)
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraDesdeEnero2022.csv')
i <- 1


for (i in 1:length(dias_base)) {
  dia <- fread(paste0('D:/!bso/girCartera/',dias_base[i],'.txt'),
                  encoding = 'Latin-1', fill = T)
  diad <- dia %>% 
    dplyr::filter(MODULO != 131) %>%
    dplyr::filter(ESTADO != 'CASTIGADA') %>%
    mutate(activoAgo = 1) %>%
    mutate(fdes = dmy(FDESEMBOLSO)) %>%
    mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>%
    mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
    dplyr::filter(DIASMORA>0) %>% 
    select(OPERACION,saldous) %>% 
    rename(Operacion=OPERACION)
  fechad <- as.Date(fecha[i],format="%Y%m%d")
  P <- Pagos %>%
    dplyr::filter(FechaPago>fechad & month(FechaPago)==month(fechad)) %>% 
    select(Operacion, CapitalPagado, FechaPago, HoraPago) %>%
    group_by(Operacion) %>%
    summarise(HoraPago = max(HoraPago), FechaPago = max(FechaPago),
              CapitalPagado = sum(CapitalPagado)) %>%
    inner_join(diad,by='Operacion') %>% 
    mutate(hora=as.POSIXct(paste(FechaPago,HoraPago),format="%Y-%m-%d %H:%M:%S"))
  ini <- min(P$hora)-difftime(min(P$hora),as.Date(min(P$hora)))
  bins <- seq.POSIXt(ini,(max(P$hora)+60*60),by="60 min") #Add to max 60 times the size of by
  
  if(i==1){
    diaPago <- P %>% 
      mutate(horabinned=cut(hora,breaks=bins)) %>%
      mutate(hora=hour(horabinned)) %>% 
      mutate(mes=month(FechaPago)) %>% 
      select(hora,mes,horabinned,Operacion) #%>% 
      # group_by(horabinned) %>% 
      # tally()
  }else{
    d <- P %>% 
      mutate(horabinned=cut(hora,breaks=bins)) %>%
      mutate(hora=hour(horabinned)) %>% 
      mutate(mes=month(FechaPago)) %>% 
      select(hora,mes,horabinned,Operacion) #%>% 
    diaPago <- diaPago %>% 
      bind_rows(d)
  }
}

d <- diaPago %>% 
  group_by(mes) %>% 
  group_by(horabinned) %>%
  tally() %>% 
  mutate(mes=month(horabinned)) %>% 
  mutate(hora=as.character(hour(horabinned))) %>%
  mutate(hora=ifelse(nchar(hora)==1,paste0('0',hora,':00'),paste0(hora,':00'))) %>%  
  mutate(meses = case_when(mes==1~ 'Enero',
                         mes==2~ 'Febrero',
                         mes==3~ 'Marzo',
                         mes==4~ 'Abril',
                         mes==5~ 'Mayo',
                         mes==6~ 'Junio',
                         mes==7~ 'Julio',
                         mes==8~ 'Agosto')) %>% 
mutate(meses=factor(meses,levels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto")))  
  
png(filename = paste0("D:/!bso/transMat/Mora_porHora",fecha[i],".png"))
diaplot <- ggplot(d,aes(x=horabinned,y=n))+
  geom_line(group=1,size=1.25)+
  labs(x="Hora para últimos días del mes",y="Número de Operaciones")+
  scale_x_discrete(breaks = d$horabinned[seq(1,length(d$n),3)],labels = d$hora[seq(1,length(d$n),3)])+
  facet_wrap(.~meses,nrow=2,scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 0.5))
print(diaplot)
dev.off()
#________________________________________________
####____HASTA OCTUBRE____####
fecha <- c('20220129','20220223','20220329','20220428','20220529',
           '20220628','20220729','20220829','20220928','20221029')
dias_base <- paste0('BaseCartera_',fecha)
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraHastaOct2022.csv')
i <- 1


for (i in 1:length(dias_base)) {
  dia <- fread(paste0('D:/!bso/girCartera/',dias_base[i],'.txt'),
               encoding = 'Latin-1', fill = T)
  diad <- dia %>% 
    dplyr::filter(MODULO != 131) %>%
    dplyr::filter(ESTADO != 'CASTIGADA') %>%
    mutate(activoAgo = 1) %>%
    mutate(fdes = dmy(FDESEMBOLSO)) %>%
    mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>%
    mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
    dplyr::filter(DIASMORA>0) %>% 
    select(OPERACION,saldous) %>% 
    rename(Operacion=OPERACION) %>% 
    
    
  fechad <- as.Date(fecha[i],format="%Y%m%d")
  P <- Pagos %>%
    dplyr::filter(FechaPago>fechad & month(FechaPago)==month(fechad)) %>% 
    select(Operacion, CapitalPagado, FechaPago, HoraPago) %>%
    group_by(Operacion) %>%
    summarise(HoraPago = max(HoraPago), FechaPago = max(FechaPago),
              CapitalPagado = sum(CapitalPagado)) %>%
    inner_join(diad,by='Operacion') %>% 
    mutate(hora=as.POSIXct(paste(FechaPago,HoraPago),format="%Y-%m-%d %H:%M:%S"))
  ini <- min(P$hora)-difftime(min(P$hora),as.Date(min(P$hora)))
  bins <- seq.POSIXt(ini,(max(P$hora)+60*60),by="60 min") #Add to max 60 times the size of by
  
  if(i==1){
    diaPago <- P %>% 
      mutate(horabinned=cut(hora,breaks=bins)) %>%
      mutate(hora=hour(horabinned)) %>% 
      mutate(mes=month(FechaPago)) %>% 
      select(hora,mes,horabinned,Operacion) #%>% 
    # group_by(horabinned) %>% 
    # tally()
  }else{
    d <- P %>% 
      mutate(horabinned=cut(hora,breaks=bins)) %>%
      mutate(hora=hour(horabinned)) %>% 
      mutate(mes=month(FechaPago)) %>% 
      select(hora,mes,horabinned,Operacion) #%>% 
    diaPago <- diaPago %>% 
      bind_rows(d)
  }
}

d <- diaPago %>% 
  group_by(mes) %>% 
  group_by(horabinned) %>%
  tally() %>% 
  mutate(mes=month(horabinned)) %>% 
  mutate(hora=as.character(hour(horabinned))) %>%
  mutate(hora=ifelse(nchar(hora)==1,paste0('0',hora,':00'),paste0(hora,':00'))) %>%  
  mutate(meses = case_when(mes==1~ 'Enero',
                           mes==2~ 'Febrero',
                           mes==3~ 'Marzo',
                           mes==4~ 'Abril',
                           mes==5~ 'Mayo',
                           mes==6~ 'Junio',
                           mes==7~ 'Julio',
                           mes==8~ 'Agosto',
                           mes==9~ 'Septiembre',
                           mes==10~ 'Octubre')) %>% 
  mutate(meses=factor(meses,levels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre")))  

png(filename = paste0("D:/!bso/transMat/Mora_porHora_Ene_Oct.png"))
diaplot <- ggplot(d,aes(x=horabinned,y=n))+
  geom_line(group=1,size=1.25)+
  labs(x="Hora para últimos días del mes",y="Número de Operaciones")+
  scale_x_discrete(breaks = d$horabinned[seq(1,length(d$n),3)],labels = d$hora[seq(1,length(d$n),3)])+
  facet_wrap(.~meses,nrow=2,scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 0.5))
print(diaplot)
dev.off()
ggsave(filename = "D:/!bso/transMat/Mora_porHora_Ene_Oct.png",plot=diaplot)
####____FUNCION CASE WHEN____####
cases <- function(quant,levs,values){
  #quant es la variable (columna) a la que se aplica la función
  #levs es un vector con los niveles que tiene la variable quant
  #values es un vector con los valores a asignar por cada valor en levs
  if(length(levs)!=length(values)){ 
    print("ERROR: NUMERO DE NIVELES Y VALORES NO COINCIDE")
    return()
  }
  n <- length(values)
  new <- vector(mode = 'character',length = n)
  for (i in 1:n) {
    new[which(quant==levs[i])] <- values[i]
  }
  return(new)
}
month(Pagos$FechaPago)
values <- 1:10
meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre")
m <- cases(month(Pagos$FechaPago),values,meses)

