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
library(factoextra)
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
####____NEW APPROACH___####
mes <- c("Dic","Nov","Oct","Sep","Ago","Jul","Jun","May","Abr","Mar","Feb","Ene")
year <- c(2023:2015)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)}))
myrds <- myrds[-c(1:which(myrds=="Abr2023"))]
castList <- list()
for (i in 1:(length(myrds)-1)) {
  tryCatch({
    print(i)
    print(myrds[i])
    k <- i+1
    if(i==1){
      df1 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',myrds[i],'.rds')) %>% 
        mutate(key = paste(CTACLIENTE,OPERACION,sep="-")) %>% 
        dplyr::filter(ESTADO=="CASTIGADA") %>% 
        select(key,CTACLIENTE,OPERACION,CI,ESTADO,saldoCast,saldous,monDate,ASESOR,
               NOMBRE_ASESOR,AGENCIA,tipoCred,fdes, MONTO, MONEDA)
      df2 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',myrds[k],'.rds')) %>% 
        mutate(key = paste(CTACLIENTE,OPERACION,sep="-")) %>% 
        dplyr::filter(ESTADO=="CASTIGADA" | key %in% df1$key) %>% 
        select(key,CTACLIENTE,OPERACION,CI,ESTADO,saldoCast,saldous,monDate,ASESOR,
               NOMBRE_ASESOR,AGENCIA,tipoCred,fdes, MONTO,MONEDA) 
      df3 <- df1 %>% 
        bind_rows(df2)
    }else{
      df1 <- df2
      df2 <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',myrds[k],'.rds')) %>% 
        mutate(key = paste(CTACLIENTE,OPERACION,sep="-")) %>% 
        dplyr::filter(ESTADO=="CASTIGADA" | key %in% df1$key) %>% 
        select(key,CTACLIENTE,OPERACION,CI,ESTADO,saldoCast,saldous,monDate,ASESOR,
               NOMBRE_ASESOR,AGENCIA,tipoCred,fdes, MONTO,MONEDA) 
      df3 <- df3 %>% 
        bind_rows(df2)
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

write_rds(df3,'D:/!bso/castigos/castigosFull.rds')#Antes sin key es castigosFullHist.rds
df3 <- readRDS('D:/!bso/castigos/castigosFull.rds')

x <- df3 %>% group_by(monDate) %>% 
  summarise(saldo=sum(saldoCast))

ase <- c("Dic2015","Dic2016","Dic2017","Dic2018","Dic2019","Dic2020","Dic2021","Dic2022","Mar2023")
bdcList <- list()
for (i in 1:length(ase)) {
  print(ase[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',ase[i],'.rds')) %>%
    group_by(year,Sucursal) %>% 
    summarise(SaldoBSO=sum(saldous),OpsBSO=n_distinct(OPERACION)) %>% 
    mutate(SaldoBSOTot=sum(SaldoBSO))
  bdcList[[i]] <- bdc
}
bdcSaldo <- bind_rows(bdcList) %>% 
  mutate(year=as.integer(year))

bdcCast <- df3 %>% 
  # dplyr::filter(monDate>"ene.2021") %>% 
  group_by(OPERACION,CTACLIENTE) %>% 
  arrange(monDate) %>% 
  mutate(pos=row_number()) %>% 
  mutate(difCast  = saldoCast - dplyr::lag(saldoCast)) %>% 
  mutate(rowCast = min(pos[which(ESTADO=='CASTIGADA')],na.rm = T)) %>% 
  mutate(rowprevCast = ifelse(rowCast>1,rowCast-1,rowCast)) %>% 
  mutate(rowCast = ifelse(pos==rowCast,1,0)) %>% 
  mutate(rowprevCast = ifelse(pos==rowprevCast,1,0)) %>% 
  mutate(rowFirst = ifelse(pos==1,1,0)) %>% 
  mutate(rowLast = ifelse(pos==max(pos),1,0)) %>% 
  ungroup() %>% 
  mutate(year=year(monDate)) %>% 
  left_join(codAge, by= c("AGENCIA")) %>%
  select(-Regional,-NOMBRE_AGENCIA) %>% 
  left_join(bdcSaldo,by=c("year","Sucursal")) %>% 
  mutate(cosechaY = year(fdes)) %>% 
  glimpse()

write_rds(bdcCast,'D:/!bso/castigos/bdcCast_v2.rds')
bdcCast <- readRDS('D:/!bso/castigos/bdcCast_v2.rds')

CheckCastMonth <- bdcCast %>% 
  group_by(monDate) %>% 
  summarise(saldoCast=sum(saldoCast,na.rm = T),saldo=sum(unique(SaldoBSO),na.rm=T))

bdcCastNew <- bdcCast %>% 
  dplyr::filter((year==2015 & rowCast == 1 & rowprevCast!=rowCast) | 
                  (year!=2015 & rowCast==1)) %>% 
  mutate(SaldoNew = saldoCast)

CheckCastNew <- bdcCastNew %>% 
  group_by(year) %>% 
  summarise(SaldoNew=sum(SaldoNew,na.rm = T))

bdcCastRec <- bdcCast %>% 
  dplyr::filter(difCast<0) %>% 
  mutate(SaldoCancel = -difCast)
bdcCastCancel <- bdcCast %>% 
  dplyr::filter(rowLast==1) %>% 
  mutate(SaldoCancel = ifelse(monDate!='mar. 2023',saldoCast,0)) %>% 
  mutate(monDate=monDate+1/12) %>% 
  mutate(year=year(monDate)) %>% 
  bind_rows(bdcCastRec)

CheckCastCancel <- bdcCastCancel %>% 
  group_by(year) %>% 
  summarise(saldoCancel=sum(SaldoCancel,na.rm = T))
####___PROCESS EXAMPLE____####
bdcExample <- bdcCast %>% 
  dplyr::filter(OPERACION==3258844) %>% 
  select(CTACLIENTE,OPERACION,monDate,ESTADO,saldous,saldoCast,ASESOR,NOMBRE_ASESOR,
         AGENCIA,rowFirst, rowCast, rowprevCast, rowLast) %>% 
  arrange(monDate) %>% 
  left_join(codAge, by ="AGENCIA") %>% 
  select(-Sucursal,-Regional) %>% 
  relocate(NOMBRE_AGENCIA,.after = AGENCIA) %>% 
  mutate(Fecha = as.Date(monDate, frac=1))
write.xlsx(bdcExample, 'D:/!bso/castigos/CastExample.xlsx')
####____HISTORIC PLOTS____####
####____**Castigado Histórico___####
gph <- bdcCast %>% 
  dplyr::filter(saldoCast>0) %>% 
  select(key,monDate,year,saldoCast, cosechaY) %>% 
  group_by(monDate,year) %>% 
  summarise(sum = sum(saldoCast, na.rm = T),
            count = n_distinct(key),
            n=n()) %>% 
  ungroup() %>% 
  dplyr::filter(month(monDate)==12 | (year==2023 & month(monDate)==3))

scaleFac <- max(gph$sum/1e3)/max(gph$count)
ggplot(gph, aes(x = year, y = sum/1000)) + 
  geom_bar(stat = 'identity',fill = paleta(12)[3])  +
  geom_line(aes(y = count*scaleFac), size = 2, color = paleta(12)[7]) + 
  geom_label(aes(label = format(count, big.mark = ','),y = count*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  geom_label(aes(label = paste0(comma(round(sum/1000)),'$'),y = sum/1000), 
             color = paleta(12)[3], fontface = 'bold',
             vjust = 2) +
  labs(x="",y="Castigos (M de USD, en barras)") +
  scale_x_continuous(breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma, 
                     sec.axis = sec_axis(~./scaleFac, name="Ops. Castigadas",
                                         label = comma)) + 
  theme_minimal()+
  theme(axis.text.y.left = element_text(color=paleta(12)[3]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[3]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoCastHist.png',width = 9,height = 6,units = "in")
####____**Castigado Nuevo____####
gph <- bdcCast %>% 
  dplyr::filter(rowCast==1 & year>2015) %>% 
  select(key, year, saldoCast, SaldoBSO,SaldoBSOTot) %>% 
  group_by(year) %>% 
  mutate(SaldoBSOTot =sum(unique(SaldoBSO),na.rm = T)) %>%
  ungroup() %>% 
  mutate(saldoCast_Rel=saldoCast/SaldoBSOTot*100) %>% 
  group_by(year) %>%
  summarise(saldoCast = sum(saldoCast,na.rm = T), 
            saldoCast_Rel = sum(saldoCast_Rel),
            count = n_distinct(key),
            CastProm = saldoCast/count)
#Por operaciones
scaleFac <- max(gph$saldoCast/1000)/max(gph$count)
ggplot(gph, aes(x = year, y = saldoCast/1000)) + 
  geom_bar(stat = 'identity', position = 'dodge', fill = paleta(12)[3])  +
  geom_line(aes(y = count*scaleFac), size = 2, color = paleta(12)[7]) + 
  geom_label(aes(label = format(count, big.mark = ','),
                 y = count*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  geom_label(aes(label = paste0(comma(round(saldoCast/1000)),'$'),
                 y = saldoCast/1000), 
             color = paleta(12)[3], fontface = 'bold')  +
  labs(x="",y="Castigos (M de USD, en barras)") +
  scale_x_continuous( breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma,
                     sec.axis = sec_axis(~./scaleFac, name="Ops. Castigadas",
                                         label = comma)) + 
  theme_minimal() +
  theme(axis.text.y.left = element_text(color=paleta(12)[3]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[3]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoCast.png',width = 9,height = 6,units = "in")
#Por promedio
scaleFac <- max(gph$saldoCast/1000)/max(gph$CastProm)
ggplot(gph, aes(x = year, y = saldoCast/1000)) + 
  geom_bar(stat = 'identity', position = 'dodge', fill = paleta(12)[3])  +
  geom_line(aes(y = CastProm*scaleFac), size = 2, color = paleta(12)[7]) + 
  geom_label(aes(label = comma(round(CastProm)),
                 y = CastProm*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  geom_label(aes(label = paste0(comma(round(saldoCast/1000)),'$'),
                 y = saldoCast/1000), position = position_stack(vjust = 0.5),
             color = paleta(12)[3], fontface = 'bold')  +
  labs(x="",y="Castigos (M de USD, en barras)") +
  scale_x_continuous( breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma,
                     sec.axis = sec_axis(~./scaleFac, name="Saldo castigado promedio",
                                         label = comma)) + 
  theme_minimal() +
  theme(axis.text.y.left = element_text(color=paleta(12)[3]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[3]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoCastProm.png',width = 9,height = 6,units = "in")
#Relativo
scaleFac <- max(gph$saldoCast/1e3)/max(gph$saldoCast_Rel)
ggplot(gph, aes(x = year, y = saldoCast/1e3)) + 
  geom_bar(stat = 'identity', fill = paleta(12)[3])  +
  geom_line(aes(y = saldoCast_Rel*scaleFac), size = 2, color = paleta(12)[7]) + 
  geom_label(aes(label = paste0(comma(round(saldoCast/1000)),'$'),
                 y = saldoCast/1000), position = position_stack(vjust = 0.5),
             color = paleta(12)[3], fontface = 'bold')  +
  geom_label(aes(label = paste0(round(saldoCast_Rel,2),'%'),
                 y = saldoCast_Rel*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  labs(x="",y="Castigos (M de USD, en barras)")+
  scale_x_continuous( breaks = seq(2015, 2022, 1)) +
  scale_y_continuous(label=comma,
                     sec.axis = sec_axis(~./scaleFac, name="Porcentaje de cartera bruta",
                                         label = comma)) + 
  theme_minimal()+
  theme(axis.text.y.left = element_text(color=paleta(12)[3]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[3]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoCast_Rel.png',width = 9,height = 6,units = "in")
####____Castigado en bruto = New-Rec____####
bdcCastNew <- bdcCast %>% 
  dplyr::filter((year==2015 & rowCast == 1 & rowprevCast!=rowCast) | 
                  (year!=2015 & rowCast==1)) %>% 
  mutate(SaldoNew = saldoCast) %>% 
  group_by(year) %>% 
  summarise(SaldoNew = sum(SaldoNew,na.rm = T))
bdcCastRaw <- bdcCastCancel %>% 
  group_by(year) %>% 
  summarise(SaldoRec=sum(SaldoCancel,na.rm = T),
            SaldoBSOTot = max(SaldoBSOTot,na.rm = T)) %>% 
  left_join(bdcCastNew,by="year") %>% 
  mutate(SaldoCastRaw = ifelse(!is.na(SaldoNew),SaldoNew - SaldoRec,-SaldoRec)) %>% 
  mutate(SaldoRaw_Rel = SaldoCastRaw/SaldoBSOTot*100)

scaleFac <- max(bdcCastRaw$SaldoCastRaw/1e3)/max(bdcCastRaw$SaldoRaw_Rel)
ggplot(bdcCastRaw, aes(x = year, y = SaldoCastRaw/1e3)) + 
  geom_bar(stat = 'identity', fill = paleta(12)[3])  +
  geom_line(aes(y = SaldoRaw_Rel*scaleFac), size = 2, color = paleta(12)[7]) + 
  geom_label(aes(label = paste0(comma(round(SaldoCastRaw/1000)),'$'),
                 y = SaldoCastRaw/1000), position = position_stack(vjust = 0.25),
             color = paleta(12)[3], fontface = 'bold')  +
  geom_label(aes(label = paste0(round(SaldoRaw_Rel,2),'%'),
                 y = SaldoRaw_Rel*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  labs(x="",y="Castigos (M de USD, en barras)")+
  scale_x_continuous( breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma,
                     sec.axis = sec_axis(~./scaleFac, name="Porcentaje de cartera bruta",
                                         label = comma)) + 
  theme_minimal()+
  theme(axis.text.y.left = element_text(color=paleta(12)[3]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[3]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoCastRaw_Rel.png',width = 9,height = 6,units = "in")
####____**Castigado Reducido____####
gph <- bdcCastCancel %>% 
  dplyr::filter(SaldoCancel>0) %>% 
  select(key, year, SaldoCancel, SaldoBSOTot) %>% 
  mutate(saldoCancel_Rel=SaldoCancel/SaldoBSOTot*100) %>% 
  group_by(year) %>%
  summarise(SaldoCancel = sum(SaldoCancel,na.rm = T), 
            SaldoCancel_Rel = sum(saldoCancel_Rel),
            count = n_distinct(key))

scaleFac <- max(gph$SaldoCancel/1e3)/max(gph$count)
ggplot(gph, aes(x = year, y = SaldoCancel/1e3)) + 
  geom_bar(stat = 'identity', position = 'dodge', fill = paleta(12)[3])  +
  geom_line(aes(y = count*scaleFac), size = 2, color = paleta(12)[7]) + 
  geom_label(aes(label = format(count, big.mark = ','),
                 y = count*scaleFac), color = paleta(12)[7], fontface = 'bold') +
  geom_label(aes(label = paste0(comma(round(SaldoCancel/1e3)),'$')), 
             color = paleta(12)[3], fontface = 'bold',position = position_stack(vjust = 0.75))  +
  labs(x="",y="Reducción de castigo (M de USD, en barras)") +
  scale_x_continuous( breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma,
                     sec.axis = sec_axis(~./scaleFac, name="Ops. Recuperadas",
                                         label = comma)) + 
  theme_minimal() +
  theme(axis.text.y.left = element_text(color=paleta(12)[3]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[3]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoCastRec.png',width = 9,height = 6,units = "in")
#Relativo
scaleFac <- max(gph$SaldoCancel/1e3)/max(gph$SaldoCancel_Rel)
ggplot(gph, aes(x = year, y = SaldoCancel/1e3)) + 
  geom_bar(stat = 'identity', fill = paleta(12)[3])  +
  geom_line(aes(y = SaldoCancel_Rel*scaleFac), size = 2, color = paleta(12)[7]) + 
  geom_label(aes(label = paste0(comma(round(SaldoCancel/1000)),'$')), 
             position = position_stack(vjust = 0.5),color = paleta(12)[3], fontface = 'bold')  +
  geom_label(aes(label = paste0(round(SaldoCancel_Rel,2),'%'),
                 y = SaldoCancel_Rel*scaleFac), color = paleta(12)[7], fontface = 'bold') +
  labs(x="",y="Reducción de castigo (M de USD, en barras)")+
  scale_x_continuous( breaks = seq(2015, 2022, 1)) +
  scale_y_continuous(label=comma,
                     sec.axis = sec_axis(~./scaleFac, name="Porcentaje de cartera bruta",
                                         label = comma)) + 
  theme_minimal()+
  theme(axis.text.y.left = element_text(color=paleta(12)[3]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[3]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoCastRec_Rel.png',width = 9,height = 6,units = "in")
####____COMPOSICION REDUCIDOS____####
codAge <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')

gph <- bdcCastCancel %>% 
  group_by(OPERACION,CTACLIENTE) %>%
  mutate(tipoCred=ifelse(length(which(!is.na(tipoCred)))==0,tipoCred,max(tipoCred,na.rm = T))) %>% 
  ungroup() %>% 
  mutate(year = year(monDate)) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(rangom = cut(montous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                      labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>% 
  mutate(rangom = as.character(rangom)) %>% 
  mutate(rangos = cut(SaldoCancel,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                      labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
  mutate(rangos = as.character(rangos)) %>% 
  mutate(cosechaS = case_when(cosechaY<=2000 ~"< 2001",
                              cosechaY>2000 & cosechaY<=2010 ~"2001-2010",
                              cosechaY>2010 & cosechaY<=2015 ~"2011-2015",
                              cosechaY>2015 ~ ">2015",)) %>% 
  mutate(ratioCastMonto = ifelse(montous>0,SaldoCancel/montous,0)) %>% 
  mutate(binCastMonto = case_when(ratioCastMonto<0.20 ~ "Ratio < 0.20",
                                  ratioCastMonto>=0.20 & ratioCastMonto<0.40 ~ "0.20 <= Ratio < 0.40",
                                  ratioCastMonto>=0.40 & ratioCastMonto<0.60 ~ "0.40 <= Ratio < 0.60",
                                  ratioCastMonto>=0.60 ~ "Ratio >= 0.60")) %>% 
  mutate(saldoCast=saldoCast/1000) %>% 
  left_join(codAge,by="AGENCIA") %>% 
  dplyr::filter(year>2015)
#Por periodo de desembolso
plt <- agrupar(gph, vstep=year, vgrupo=cosechaS, vagre=SaldoCancel,pct=5,last = 3)
plt$y <- plt$y %>% 
  mutate(lab = paste0(round(rat),'% (',comma(round(tot/1e3)),' M USD)'))
ggplot(plt$y,aes(x=year,y=rat,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label(aes(label=lab),size=3,color="white",show.legend = F,
             position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=plt$z$year,y=plt$z$rat*1.05,
           label=comma(round(plt$z$tot,0)),size=3.5,color=paleta(12)[7])+
  labs(x="",y="Participación (%)",
       fill="Periodo de desembolso")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2015, 2023, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/castRecDesembolso.png',width = 9,height = 6,units = "in")
#Por ratio saldo/monto
plt <- agrupar(gph, vstep=year, vgrupo=binCastMonto, vagre=SaldoCancel,pct=5,last = 2)
plt$y <- plt$y %>% 
  mutate(lab = paste0(round(rat),'% (',comma(round(tot/1e3)),' M USD)'))
ggplot(plt$y,aes(x=year,y=rat,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label(aes(label=lab),size=3,color="white",show.legend = F,
             position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=plt$z$year,y=plt$z$rat*1.05,
           label=comma(round(plt$z$tot,0)),size=3.5,color=paleta(12)[7])+
  labs(x="",y="Participación (%)",
       fill="Ratio Saldo Recuperado/Monto")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2015, 2023, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/castRecRatio.png',width = 9,height = 6,units = "in")

####____COMPOSICION CASTIGADOS NUEVOS____#####
gph <- bdcCast %>% 
  group_by(OPERACION,CTACLIENTE) %>%
  mutate(tipoCred=ifelse(length(which(!is.na(tipoCred)))==0,tipoCred,max(tipoCred,na.rm = T))) %>% 
  ungroup() %>% 
  mutate(year = year(monDate)) %>% 
  dplyr::filter(rowCast==1 & year>2015) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(rangom = cut(montous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                      labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>% 
  mutate(rangom = as.character(rangom)) %>% 
  mutate(rangos = cut(saldoCast,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                      labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
  mutate(rangos = as.character(rangos)) %>% 
  mutate(ratioCastMonto = ifelse(montous>0,saldoCast/montous,0)) %>% 
  mutate(binCastMonto = case_when(ratioCastMonto<0.20 ~ "Ratio < 0.20",
                                  ratioCastMonto>=0.20 & ratioCastMonto<0.40 ~ "0.20 <= Ratio < 0.40",
                                  ratioCastMonto>=0.40 & ratioCastMonto<0.60 ~ "0.40 <= Ratio < 0.60",
                                  ratioCastMonto>=0.60 ~ "Ratio >= 0.60")) %>% 
  mutate(saldoCast=saldoCast/1000) %>% 
  select(-Sucursal) %>% 
  left_join(codAge,by="AGENCIA") 
####____**Por desembolso____####
gph <- gph %>% 
  dplyr::filter(monDate<'ene.2023') %>% 
  mutate(difyear = year-cosechaY) %>% 
  mutate(difyear = paste0("t - ", difyear))

plt <- agrupar(gph, vstep=year, vgrupo=difyear, vagre=saldoCast,pct=6,last = 2)
plt$y <- plt$y %>% 
  mutate(lab = paste0(round(rat),'% (',comma(round(tot/1e3,1)),' M USD)'))

ggplot(plt$y,aes(x=year,y=tot,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label(aes(label=lab),size=3,color="white",show.legend = F,
             position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=plt$z$year,y=plt$z$tot*1.05,
           label=comma(round(plt$z$tot,0)),size=3.5,color=paleta(12)[7])+
  labs(x="",y="Castigos (en M de USD)",
       fill="Diferencia de año con desembolso")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2015, 2022, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/castDifDesembolso.png',width = 9,height = 6,units = "in")

####____**Por ratio saldoCast/Monto####
plt <- agrupar(gph, vstep = year, vgrupo = binCastMonto, vagre = saldoCast, pct=4)
plt$y <- plt$y %>% 
  mutate(lab = paste0(round(rat),'% (',comma(round(tot/1e3,1)),' M USD)'))
ggplot(plt$y,aes(x=year,y=tot,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label(aes(label=lab),size=3,color="white",show.legend = F,
             position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=plt$z$year,y=plt$z$tot*1.05,
           label=comma(round(plt$z$tot,0)),size=3.5,color=paleta(12)[7])+
  labs(x="",y="Castigos (en M de USD)",
       fill="Ratio Saldo Castigado/Desembolso")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2015, 2022, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/castRatio.png',width = 9,height = 6,units = "in")

####____**Por Sucursal____####
suc <- gph %>% 
  mutate(SaldoCast_Rel = saldoCast*1000/SaldoBSO*100) %>% 
  group_by(year,Sucursal) %>% 
  summarise(SaldoCast=sum(saldoCast*1000,na.rm = T),
            SaldoBSO = max(SaldoBSO,na.rm = T),
            SaldoCast_Rel = sum(SaldoCast_Rel,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ORDEN=ifelse(Sucursal %in% Sucursal[SaldoCast_Rel<0.4 &year==2022],"Otros",Sucursal)) %>% 
  group_by(year,ORDEN) %>% 
  mutate(SaldoORDEN = sum(unique(SaldoBSO),na.rm = T)) %>% 
  mutate(SaldoCast_Rel = SaldoCast/SaldoORDEN*100) %>% 
  summarise(SaldoCast=sum(SaldoCast,na.rm = T),
            SaldoBSO = max(SaldoBSO,na.rm = T),
            SaldoCast_Rel = sum(SaldoCast_Rel,na.rm = T)) %>% 
  mutate(lab = paste0(round(SaldoCast_Rel,1),'%'))

ggplot(suc,aes(x=year,y=SaldoCast_Rel,color=ORDEN))+
  geom_line(size=1.25)+
  geom_point(size=3)+
  geom_label_repel(aes(label=lab),size=3.5,show.legend = F)+
  labs(x="",y="Castigo/Cartera (%)",
       fill="Sucursal")+
  scale_color_manual(values=paleta(5))+
  scale_x_continuous(breaks = seq(2015, 2022, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/castSucursalRel.png',width = 9,height = 6, units = "in")
#Sin Pando y Beni
suc <- gph %>% 
  dplyr::filter(!Sucursal %in% c("Pando","Beni")) %>% 
  mutate(SaldoCast_Rel = saldoCast*1000/SaldoBSO*100) %>% 
  group_by(year,Sucursal) %>% 
  summarise(SaldoCast=sum(saldoCast*1000,na.rm = T),
            SaldoBSO = max(SaldoBSO,na.rm = T),
            SaldoCast_Rel = sum(SaldoCast_Rel,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ORDEN=ifelse(Sucursal %in% c("Potosí","Tarija","Oruro","Chuquisaca"),"Otros",Sucursal)) %>% 
  group_by(year,ORDEN) %>% 
  mutate(SaldoORDEN = sum(unique(SaldoBSO),na.rm = T)) %>% 
  mutate(SaldoCast_Rel = SaldoCast/SaldoORDEN*100) %>% 
  summarise(SaldoCast=sum(SaldoCast,na.rm = T),
            SaldoBSO = max(SaldoBSO,na.rm = T),
            SaldoCast_Rel = sum(SaldoCast_Rel,na.rm = T)) %>% 
  mutate(lab = paste0(round(SaldoCast_Rel,1),'%'))
ggplot(suc,aes(x=year,y=SaldoCast_Rel,color=ORDEN))+
  geom_line(size=1.25)+
  geom_point(size=3)+
  geom_label_repel(aes(label=lab),size=3,show.legend = F)+
  labs(x="",y="Castigo/Cartera (%)",
       fill="Sucursal")+
  scale_color_manual(values=paleta(8)[2:6])+
  scale_x_continuous(breaks = seq(2015, 2022, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/castSucursalRel_SinPDBN.png',width = 9,height = 6, units = "in")
####____LISTA DE CASTIGOS___####
bdcCast <- readRDS('D:/!bso/castigos/bdcCast.rds')
codAge <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')

bdcCastCancel <- bdcCast %>% 
  dplyr::filter(rowLast==1 & monDate!='feb. 2023') %>% 
  mutate(saldoCast = 0) %>% 
  mutate(monDate=monDate+1/12) %>% 
  mutate(year=year(monDate))
####____ADDING RAFAEL AGENCY____####
rafa <- read.xlsx('D:/!bso/castigos/join_castigos_Feb2023.xlsx') %>% 
  select(-Saldo.Ene23,-Saldo.Dic22,-regional,-NoOp,NOMBRE_AGENCIA=Agencia.Origen.Final) %>% 
  left_join(codAge,by="NOMBRE_AGENCIA") %>% 
  select(-Regional,-Sucursal,AGENCIA_ORI=NOMBRE_AGENCIA, COD_AGENCIA_ORI=AGENCIA) %>% 
  glimpse()

bdcCastOri <- bdcCast %>% 
  bind_rows(bdcCastCancel) %>% 
  group_by(OPERACION,CTACLIENTE) %>% 
  arrange(monDate) %>% 
  mutate(pos=row_number()) %>% 
  mutate(difCast  = saldoCast - dplyr::lag(saldoCast)) %>% 
  mutate(tipoCred=ifelse(length(which(!is.na(tipoCred)))==0,tipoCred,max(tipoCred,na.rm = T))) %>% 
  mutate(ASESOR=ASESOR[rowFirst==1]) %>% 
  mutate(AGENCIA=AGENCIA[rowFirst==1]) %>% 
  mutate(NOMBRE_ASESOR=NOMBRE_ASESOR[rowFirst==1]) %>% 
  ungroup() %>% 
  left_join(codAge,by="AGENCIA") %>% 
  left_join(rafa,by=c("CTACLIENTE","OPERACION")) %>% 
  group_by(ASESOR) %>% 
  mutate(NOMBRE_ASESOR=NOMBRE_ASESOR[row_number()==1]) %>% 
  ungroup() %>% 
  mutate(SaldoCastNew = case_when(year==2015 & rowCast==1 & rowprevCast!= rowCast~ saldoCast,
                                  year!=2015 & rowCast==1~ saldoCast,
                                  TRUE~0)) %>% 
  mutate(SaldoCastRec = ifelse(difCast<0,difCast,0)) %>% 
  mutate(CastRec = ifelse(difCast<0,1,0)) %>% 
  mutate(CastNew = case_when(monDate==2015 & rowCast==1 & rowprevCast!= rowCast~1,
                             monDate!=2015 & rowCast==1~1,
                             TRUE~0)) %>% 
  select(Sucursal, ASESOR, NOMBRE_ASESOR, AGENCIA, NOMBRE_AGENCIA, monDate, 
         year, saldoCast, difCast, SaldoCastNew, SaldoCastRec, CastRec, CastNew,
         AGENCIA_ORI,COD_AGENCIA_ORI)

bdcCastOri %>% 
  group_by(year) %>% 
  summarise(New=sum(SaldoCastNew,na.rm=T),
            Rec=sum(SaldoCastRec,na.rm = T))

bdcFeb <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Feb2023.rds') %>% 
  left_join(codAge,by="AGENCIA") %>% 
  group_by(monDate, Sucursal, AGENCIA, NOMBRE_AGENCIA) %>% 
  summarise(saldoBSO=sum(saldous)) %>% 
  ungroup()
#CON RAFA
bdcAgencia <- bdcCastOri %>% 
  select(year, monDate, Sucursal, AGENCIA, NOMBRE_AGENCIA, AGENCIA_ORI, COD_AGENCIA_ORI,
         saldoCast, SaldoCastRec, SaldoCastNew, CastRec, CastNew) %>% 
  mutate(NOMBRE_AGENCIA = ifelse(!is.na(AGENCIA_ORI),AGENCIA_ORI,NOMBRE_AGENCIA)) %>% 
  mutate(AGENCIA = ifelse(!is.na(COD_AGENCIA_ORI),COD_AGENCIA_ORI,AGENCIA)) %>% 
  select(-AGENCIA_ORI,-COD_AGENCIA_ORI) %>% 
  group_by(year, monDate, Sucursal, AGENCIA, NOMBRE_AGENCIA) %>% 
  summarise_all(sum, na.rm = T) %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
  dplyr::filter(saldoCast>0 | SaldoCastRec<0) %>% 
  ungroup() %>% 
  group_by(AGENCIA, NOMBRE_AGENCIA) %>% 
  arrange(monDate) %>% 
  mutate(cumSaldoNew=cumsum(SaldoCastNew),
         cumSaldoRec=cumsum(SaldoCastRec),
         cumOpsNew= cumsum(CastNew),
         cumOpsRec=cumsum(CastRec)) %>% 
  ungroup() %>% 
  left_join(bdcFeb, by=c("monDate","Sucursal","AGENCIA","NOMBRE_AGENCIA"))
write.xlsx(bdcAgencia,'D:/!bso/castigos/Castigos_Feb2023.xlsx')
#SIN RAFA
bdcAgencia <- bdcCastOri %>% 
  select(year, monDate, Sucursal, AGENCIA, NOMBRE_AGENCIA,
         saldoCast, SaldoCastRec, SaldoCastNew, CastRec, CastNew) %>% 
  group_by(year, monDate, Sucursal, AGENCIA, NOMBRE_AGENCIA) %>% 
  summarise_all(sum, na.rm = T) %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
  dplyr::filter(saldoCast>0 | SaldoCastRec<0) %>% 
  ungroup() %>% 
  group_by(AGENCIA, NOMBRE_AGENCIA) %>% 
  arrange(monDate) %>% 
  mutate(cumSaldoNew=cumsum(SaldoCastNew),
         cumSaldoRec=cumsum(SaldoCastRec),
         cumOpsNew= cumsum(CastNew),
         cumOpsRec=cumsum(CastRec)) %>% 
  ungroup() %>% 
  left_join(bdcFeb, by=c("monDate","Sucursal","AGENCIA","NOMBRE_AGENCIA"))

bdcFeb <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Feb2023.rds') %>% 
  left_join(codAge,by="AGENCIA") %>% 
  group_by(monDate, Sucursal, AGENCIA, NOMBRE_AGENCIA, ASESOR, NOMBRE_ASESOR) %>% 
  summarise(saldoBSO=sum(saldous)) %>% 
  ungroup()

bdcAsesor <- bdcCastOri %>% 
  select(year, monDate, Sucursal, ASESOR, NOMBRE_ASESOR,
         saldoCast, SaldoCastRec, SaldoCastNew, CastRec, CastNew) %>% 
  group_by(year, monDate, Sucursal, ASESOR, NOMBRE_ASESOR) %>% 
  summarise_all(sum, na.rm =T) %>% 
  mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
  dplyr::filter(saldoCast>0 | SaldoCastRec<0) %>% 
  ungroup() %>% 
  group_by(ASESOR, NOMBRE_ASESOR) %>% 
  arrange(monDate) %>% 
  mutate(cumSaldoNew=cumsum(SaldoCastNew),
         cumSaldoRec=cumsum(SaldoCastRec),
         cumOpsNew= cumsum(CastNew),
         cumOpsRec=cumsum(CastRec)) %>% 
  ungroup() %>% 
  left_join(bdcFeb, by=c("monDate","Sucursal","ASESOR","NOMBRE_ASESOR"))

tablas  <- list(DatosAgencia = bdcAgencia, DatosAsesor = bdcAsesor)

write.xlsx(tablas, 'D:/!bso/castigos/Castigos_Feb2023_v5.xlsx')
write_rds(bdcAgencia, 'D:/!bso/castigos/bdcAgencia.rds')
write_rds(bdcAsesor, 'D:/!bso/castigos/bdcAsesor.rds')
####____CLUSTERS____####
bdcAgencia <- readRDS('D:/!bso/castigos/bdcAgencia.rds')
gph <- bdcAgencia %>% 
  select(monDate, Sucursal, AGENCIA, NOMBRE_AGENCIA, saldoCast, saldoBSO) %>% 
  dplyr::filter(monDate=='feb.2023') %>% 
  dplyr::filter(!str_detect(NOMBRE_AGENCIA,"Normal")) %>% 
  dplyr::filter(!str_detect(NOMBRE_AGENCIA,"Móvil")) %>% 
  dplyr::filter(!is.na(saldoBSO)) %>% 
  mutate(ratio = saldoCast/saldoBSO)

k3 <- kmeans(gph[,7], 3, nstart = 20)

clusters <- as.data.frame(k3$cluster) %>% 
  dplyr::rename(Grupo = `k3$cluster`) %>% 
  bind_cols(gph) %>% 
  mutate(Grupo = case_when(Grupo==1~'2.0% <= Ratio < 9.2%',
                           Grupo==2~'Ratio < 2.0%',
                           Grupo==3~'Ratio >= 9.2%')) %>% 
  mutate(label = paste0(Sucursal, ', ', NOMBRE_AGENCIA, ': ',
                        as.character(round(ratio*100, 1)), '%')) %>% 
  arrange(desc(ratio)) %>% 
  mutate(label = ifelse(row_number() <= 12, label, NA))

ggplot(clusters, aes(x = saldoCast/1e3, y = saldoBSO/1e6, 
                     color = as.factor(Grupo))) + 
  geom_point(size = 3) + 
  geom_label_repel(aes(label = label, color = factor(Grupo)), size = 2.5, show.legend = F) +
  scale_color_manual(values = paleta(5)) +
  scale_x_continuous(label = comma,
                     name = 'Saldo Castigado (M USD)') +
  scale_y_continuous(label = comma, name = 'Saldo BSO (MM USD)') +
  theme_minimal() + 
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(title="Grupo"))
ggsave('D:/!bso/castigos/castScatter_Feb23.png', width = 9, height = 6, units = "in")

####____OTRAS VAINAS ANTIGUAS____####
gph  <- bdcCast %>% 
  mutate(year = year(monDate)) %>% 
  dplyr::filter(rowCast==1) %>% 
  group_by(year) %>% 
  summarise(saldoCast=sum(saldoCast)) %>% 
  mutate(saldoCastCum=cumsum(saldoCast))

cum <- df3 %>% 
  mutate(nopsCast = ifelse(saldoCast > 0 & !is.na(saldoCast), 1, 0)) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(rangom = cut(montous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                      labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>% 
  mutate(rangom = as.character(rangom)) %>% 
  mutate(saldoCast=saldoCast/1000) %>% 
  left_join(codAge,by="AGENCIA")

cumSuc <- cum %>% 
  group_by(monDate,rangom) %>% 
  summarise(sum = sum(saldoCast, na.rm = T),
            count = sum(nopsCast, na.rm = T)) %>% 
  mutate(year = year(monDate)) %>% 
  ungroup() %>% 
  dplyr::filter(month(monDate)==12 | (year==2023 & month(monDate)==2)) %>% 
  mutate(ORDEN = ifelse(rangom %in% rangom[sum>5000 & year==2022],rangom,'Otros')) %>% 
  mutate(ORDEN = fct_reorder(factor(ORDEN),sum)) %>% 
  group_by(year,ORDEN) %>% 
  summarise(sum=sum(sum),count=sum(count))

z <- cumSuc %>%
  group_by(year) %>% 
  summarise(sum=sum(sum),count=sum(count))

ggplot(cumSuc, aes(x = year, y = sum, fill=ORDEN)) + 
  geom_bar(stat = 'identity')  +
  labs(x="",y="Castigos (en M de USD)",
       fill="Rango de desembolso")+
  geom_text(aes(label=comma(round(sum,0))),size=3,color="white",
            position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=z$year,y=z$sum*1.05,
           label=comma(round(z$sum,0)),size=3.5,color=paleta(12)[7])+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma) + 
  theme_minimal()+
  theme(legend.position = "bottom")

ggsave('D:/!bso/castigos/ranAcum.png', width=9, height=6, units="in")

cumSuc <- cum %>% 
  group_by(monDate,Sucursal) %>% 
  summarise(sum = sum(saldoCast, na.rm = T),
            count = sum(nopsCast, na.rm = T)) %>% 
  mutate(year = year(monDate)) %>% 
  ungroup() %>% 
  dplyr::filter(month(monDate)==12 | (year==2023 & month(monDate)==2)) %>% 
  mutate(ORDEN = ifelse(Sucursal %in% Sucursal[sum>5000 & year==2022],Sucursal,'Otros')) %>% 
  mutate(ORDEN = fct_reorder(factor(ORDEN),sum)) %>% 
  group_by(year,ORDEN) %>% 
  summarise(sum=sum(sum),count=sum(count))

z <- cumSuc %>%
  group_by(year) %>% 
  summarise(sum=sum(sum),count=sum(count))

ggplot(cumSuc, aes(x = year, y = sum, fill=ORDEN)) + 
  geom_bar(stat = 'identity')  +
  labs(x="",y="Castigos (en M de USD)",
       fill="Sucursal")+
  geom_text(aes(label=comma(round(sum,0))),size=3,color="white",
            position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=z$year,y=z$sum*1.05,
           label=comma(round(z$sum,0)),size=3.5,color=paleta(12)[7])+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma) + 
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/sucAcum.png', width=9, height=6, units="in")
################################################################################

gph <- bdcCast %>% 
  dplyr::filter(rowCast==1 & rowCast!=rowprevCast) %>% 
  mutate(year = year(monDate)) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(rangom = cut(montous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                      labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
  group_by(year,rangom) %>% 
  summarise(saldoCast = sum(saldoCast), count = n()) %>% 
  ungroup()


ggplot(gph,aes(x=year,y=saldoCast,fill=rangom)) +
  geom_bar(stat = "identity")
gph <- bdcCast %>% 
  dplyr::filter(rowCast==1 & rowCast!=rowprevCast) %>% 
  mutate(year = year(monDate)) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(rangom = cut(montous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                      labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
  group_by(year,rangom) %>% 
  summarise(saldoCast = sum(saldoCast), count = n()) %>% 
  ungroup()
ggplot(gph,aes(x=year,y=saldoCast,fill=rangom)) +
  geom_bar(stat = "identity")