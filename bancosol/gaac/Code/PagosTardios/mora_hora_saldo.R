#-------------------------
####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
#library(xlsx)
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
#library(forecast)
library(quantmod)
#library(astsa)
#library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
#library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
#library(timeSeries)
library(tseries)
#library(xtable)
#library(openxlsx)
#library(hrbrthemes)
#library(viridis)
#library(scales)
#library(janitor)
#library(RColorBrewer)
#library(paletteer)
#library(plotly)
library(ggplot2)
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
####____SALDO EN MORA POR HORA____####
values <- 1:12
meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
fecha <- c('20220129','20220223','20220329','20220428','20220529',
           '20220628','20220729','20220829','20220928')
dias_base <- paste0('BaseCartera_',fecha)
Pagos <- fread('D:/!bso/bases/csv/PagosCarteraDesdeEnero2022.csv')
i <- 1

par0pago <- vector(mode = "numeric",length = length(fecha))
saldo_ini <- vector(mode = "numeric",length = length(fecha))
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
  saldo_ini[i] <- sum(na.omit(diad$saldous))
  fechad <- as.Date(fecha[i],format="%Y%m%d")
  P <- Pagos %>%
    dplyr::filter(FechaPago>fechad & month(FechaPago)==month(fechad)) %>% 
    select(Operacion, CapitalPagado, FechaPago, HoraPago) %>%
    group_by(Operacion) %>%
    summarise(HoraPago = max(HoraPago), FechaPago = max(FechaPago),
              CapitalPagado = sum(CapitalPagado)) %>%
    inner_join(diad,by='Operacion') %>% 
    mutate(hora=as.POSIXct(paste(FechaPago,HoraPago),format="%Y-%m-%d %H:%M:%S"))
  par0pago[i] <- sum(na.omit(P$saldous))
  
  ini <- min(P$hora)-difftime(min(P$hora),as.Date(min(P$hora)))
  bins <- seq.POSIXt(ini,(max(P$hora)+60*60),by="60 min") #Add to max 60 times the size of by
  
  if(i==1){
    diaPago <- P %>%
      mutate(horabinned=cut(hora,breaks=bins)) %>%
      group_by(horabinned) %>% 
      summarise(NOps=n(),saldoMoraPago=sum(na.omit(saldous))) %>% 
      mutate(saldoMoraAcum = cumsum(saldoMoraPago)) %>% 
      mutate(smf=saldo_ini[i]-saldoMoraAcum) %>% 
      mutate(hora=as.character(hour(horabinned))) %>%
      mutate(hora=ifelse(nchar(hora)==1,paste0('0',hora,':00'),paste0(hora,':00'))) %>% 
      mutate(mes=month(horabinned)) %>% 
      mutate(mes = cases(mes,values,meses)) %>% 
      mutate(mes = factor(mes,levels = meses))  
  }else{
    d <- P %>%
      mutate(horabinned=cut(hora,breaks=bins)) %>%
      group_by(horabinned) %>% 
      summarise(NOps=n(),saldoMoraPago=sum(na.omit(saldous))) %>% 
      mutate(saldoMoraAcum = cumsum(saldoMoraPago)) %>% 
      mutate(smf=saldo_ini[i]-saldoMoraAcum) %>% 
      mutate(hora=as.character(hour(horabinned))) %>%
      mutate(hora=ifelse(nchar(hora)==1,paste0('0',hora,':00'),paste0(hora,':00'))) %>% 
      mutate(mes=month(horabinned)) %>% 
      mutate(mes = cases(mes,values,meses)) %>% 
      mutate(mes = factor(mes,levels = meses))
    diaPago <- diaPago %>% 
      bind_rows(d)
  }
}

diaPago <- diaPago %>% 
  mutate(fecha=as.POSIXct(horabinned)) %>% 
  mutate(smf_mill=smf/1000000)

write.csv(x=diaPago,file="D:/!bso/output/Saldo_mora_mes.csv",row.names = F)
write_xlsx(diaPago,path="D:/!bso/output/Saldo_mora_mes.xlsx")

scale_fac <- max(diaPago$smf_mill)/max(diaPago$NOps)
diaplot <- ggplot(diaPago,aes(x=fecha,y=smf_mill))+
  geom_line(colour=paleta(8)[2],group=1,size=1.25)+
  geom_line(aes(y=NOps*scale_fac),colour=paleta(8)[6],size=1.25)+
  labs(x="Hora para últimos días del mes",y="Saldo en Mora en MM USD")+
  facet_wrap(.~mes,nrow=2,scales = "free_x")+
  scale_y_continuous(sec.axis = sec_axis(~./scale_fac,name = "Nro Operaciones"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 0.5),
        axis.text.y.left = element_text(color=paleta(8)[2]),
        axis.text.y.right = element_text(color=paleta(8)[6]))
print(diaplot)
ggsave(filename = "D:/!bso/output/Saldo_Ops_mora_hora.jpg",plot=diaplot)

#_____________________________________
####____SALDO AL FINAL DEL DIA___####
d <- diaPago %>% 
  mutate(fecha=as.Date(horabinned)) %>% 
  group_by(fecha) %>% 
  summarise(saldoPagoFin=min(smf))

