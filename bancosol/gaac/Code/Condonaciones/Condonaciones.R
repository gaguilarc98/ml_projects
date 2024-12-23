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
####____CIERRES MENSUALES____####
cierre <- c("Dic2015","Dic2016","Dic2017","Dic2018","Dic2019","Dic2020","Dic2021","Dic2022","Abr2023")
bdcList <- list()
for (i in 1:length(cierre)) {
  print(cierre[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',cierre[i],'.rds')) %>%
    group_by(year,Sucursal) %>% 
    summarise(SaldoBSO=sum(saldous),OpsBSO=n_distinct(OPERACION)) %>% 
    mutate(SaldoBSOTot=sum(SaldoBSO))
  bdcList[[i]] <- bdc
}
bdcSaldo <- bind_rows(bdcList) %>% 
  mutate(year=as.integer(year))
####____CONDONACIONES____####
codAge <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')
cond <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Abr2023.rds') %>% 
  glimpse()
sapply(cond, function(x){length(which(is.na(x)))})

process <- function(x, vstep){
  x %>% 
    select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
           CondInt_USD = Cond_Int,
           CondCap_USD = Cond_Cap,
           Cod_Asesor = Asesor,
           Nombre_Asesor = NombreAsesor,
           AGENCIA = Sucursal_operacion,
           {{vstep}}) %>%
    left_join(codAge,by="AGENCIA") %>% 
    select(-Fecha, -Regional) %>% 
    rename(Cod_Agencia = AGENCIA) %>% 
    group_by({{vstep}}, Sucursal, Cod_Agencia, NOMBRE_AGENCIA, 
             Cod_Asesor, Nombre_Asesor, Cuenta, Operacion) %>% 
    summarise_all(sum) %>% 
    ungroup() %>% 
    mutate(Int_Condonado = ifelse(CondInt_USD>0,1,0)) %>% 
    mutate(Cap_Condonado = ifelse(CondCap_USD>0,1,0)) %>% 
    mutate(IntCap_Condonado = ifelse(CondCapInt_USD>0,1,0)) %>% 
    mutate(Key = paste(Cuenta,Operacion,sep="-")) 
}

gph <- cond %>% 
  mutate(year = year(Fecha)) %>% 
  process(vstep = year) %>% 
  left_join(bdcSaldo,by=c("year","Sucursal")) %>% 
  mutate(CondCapInt_Rel = CondCapInt_USD/SaldoBSOTot,
         CondCap_Rel = CondCap_USD/SaldoBSOTot,
         CondInt_Rel = CondInt_USD/SaldoBSOTot) %>% 
  dplyr::filter(IntCap_Condonado== 1) %>% 
  group_by(year) %>% 
  summarise(Cond = sum(CondCapInt_USD), Ops = n_distinct(Key),
            Cond_Rel = sum(CondCapInt_Rel)*100) %>% 
  ungroup() %>% 
  mutate(Condcum = cumsum(Cond), Opscum = cumsum(Ops)) %>% 
  glimpse()
####____ABSOLUTE AND RELATIVE PLOTS___####
scaleFac <- max(gph$Cond/1000)/max(gph$Ops)
ggplot(gph, aes(x = year, y = Cond/1000)) + 
  geom_bar(stat = 'identity', position = 'dodge', fill = paleta(12)[3])  +
  geom_line(aes(y = Ops*scaleFac), size = 2, color = paleta(12)[7]) + 
  scale_x_continuous( breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma, name = "Condonaciones (M de USD, en barras)",
                     sec.axis = sec_axis(~./scaleFac, name="Ops. Condonadas",
                                         label = comma)) + 
  theme_minimal() + 
  labs(x="",title = "Condonaciones por año") +
  geom_label(aes(label = format(Ops, big.mark = ','),
                 y = Ops*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  geom_label(aes(label = paste0(as.character(format(round(Cond/1000), big.mark = ',')),'$')), 
             color = paleta(12)[3], fontface = 'bold', position = position_stack(vjust = 0.75))  +
  theme(text = element_text(face="bold"),
        axis.text.y.left = element_text(color=paleta(12)[3]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[3]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoOpsCond.png',height = 6,width = 9,units = "in")

#Condonaciones acumulado
scaleFac <- max(gph$Condcum/1000)/max(gph$Opscum)
ggplot(gph, aes(x = year, y = Condcum/1000)) + 
  geom_bar(stat = 'identity', position = 'dodge', fill = paleta(12)[3])  +
  geom_line(aes(y = Opscum*scaleFac), size = 2, color = paleta(12)[7]) + 
  scale_x_continuous( breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma, name = "Condonaciones (M de USD, en barras)",
                     sec.axis = sec_axis(~./scaleFac, name="Ops. Condonadas",
                                         label = comma)) + 
  theme_minimal() + 
  labs(x="",title="Condonaciones acumuladas desde 2019") +
  geom_label(aes(label = format(Opscum, big.mark = ','),
                 y = Opscum*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  geom_label(aes(label = paste0(as.character(format(round(Condcum/1000), big.mark = ',')),'$')), 
             color = paleta(12)[3], fontface = 'bold', position = position_stack(vjust = 0.75))  +
  theme(text = element_text(face = "bold"),
        axis.text.y.left = element_text(color=paleta(12)[3]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[3]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoOpsCondCum.png',width = 9,height = 6,units = "in")

#Condonación relativo
scaleFac <- max(gph$Cond/1000)/max(gph$Cond_Rel)
ggplot(gph, aes(x = year, y = Cond/1000))+ 
  geom_bar(stat = 'identity', position = 'dodge', fill = paleta(12)[3])+
  geom_line(aes(y = Cond_Rel*scaleFac), size = 2, color = paleta(12)[7])+ 
  geom_label(aes(label = paste0(round(Cond_Rel,2),' %'),
                 y = Cond_Rel*scaleFac), 
             color = paleta(12)[7], fontface = 'bold')+
  geom_label(aes(label = paste0(as.character(format(round(Cond/1000), big.mark = ',')),'$')), 
             color = paleta(12)[3], fontface = 'bold', position = position_stack(vjust = 0.5))+
  labs(x="",y="Condonaciones (M de USD, en barras)")+
  scale_x_continuous( breaks = seq(2015, 2023, 1))+
  scale_y_continuous(label=comma,
                     sec.axis = sec_axis(~./scaleFac, name="Porcentaje de Cartera Bruta",
                                         label = comma))+ 
  theme_minimal()+ 
  theme(axis.text.y.left = element_text(color=paleta(12)[3]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[3]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoCondRel.png',height = 6,width = 9,units = "in")
####____ADDING BDC VARIABLES BY PREVIOUS MONTH OF CONDONACION____####
gph <- cond %>% 
  mutate(monDate = as.yearmon(Fecha)) %>% 
  process(vstep = monDate)

mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2018:2023)
myrds <- as.vector(sapply(year, function(x){paste0(mes,x)}))
myrds <- myrds[-c(1:which(myrds=="Nov2018"),which(myrds=="Feb2023"):length(myrds))]
bdcList <- list()
#Para el join se busca al condonado en la base de cartera de un mes anterior 
#esto porque un cliente puede cancelar por completo luego de ser condonado
for (i in 1:length(myrds)) {
  print(myrds[i])
  prevCierre <- paste0(substr(myrds[i],1,3),'. ',substr(myrds[i],4,7))
  print(prevCierre)
  bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',myrds[i],'.rds')) %>%
    select(CTACLIENTE,OPERACION,GENERO,tipoCred, AGENCIA, saldous, MONTO, MONEDA, fdes) %>% 
    mutate(cosechaY = year(fdes)) %>% 
    mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(rangom = cut(montous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                        labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>% 
    mutate(rangom = as.character(rangom)) %>% 
    mutate(saldoBSO = sum(saldous,na.rm = T),
           nOpsBSO = n_distinct(OPERACION))
  
  cond <- gph %>% 
    dplyr::filter(monDate == as.yearmon(prevCierre)+1/12) %>% 
    rename(CTACLIENTE=Cuenta, OPERACION=Operacion) %>% 
    left_join(bdc,by=c("CTACLIENTE","OPERACION"))
  bdcList[[i]] <- cond
}
bdcCond <- bind_rows(bdcList)
#Existen 8 casos raros en los que un cliente es condonado el mismo mes de apertura

write_rds(bdcCond,'D:/!bso/castigos/CondFull_wBDCinfo.rds')
####____READING BDCCOND___####
bdcCond <- readRDS('D:/!bso/castigos/CondFull_wBDCinfo.rds')
bdcYear <- bdcCond %>% 
  mutate(year=year(monDate)) %>% 
  left_join(bdcSaldo,by="year") %>% 
  mutate(CondCapInt_Rel = CondCapInt_USD/SaldoBSO,
         CondCap_Rel = CondCap_USD/SaldoBSO,
         CondInt_Rel = CondInt_USD/SaldoBSO) %>% 
  mutate(rangos = cut(saldous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                      labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
  mutate(rangos = as.character(rangos)) %>% 
  mutate(ratioCondMonto = ifelse(montous>0,CondCapInt_USD/montous,0)) %>% 
  mutate(binCondMonto = case_when(ratioCondMonto<0.01 ~ "Ratio < 10%",
                                  ratioCondMonto>=0.01 & ratioCondMonto<0.015 ~ "10% <= Ratio < 15%",
                                  ratioCondMonto>=0.015 & ratioCondMonto<0.03 ~ "15% <= Ratio < 30%",
                                  ratioCondMonto>=0.03 ~ "Ratio >= 0.30")) %>% 
  mutate(CondCapInt_USD=CondCapInt_USD) %>% 
  glimpse() 
####____COMPOSICION DE CONDONACIONES____####
####____**Por cosecha del desembolso____####
condG <- bdcYear %>% 
  mutate(difyear = year-cosechaY) %>% 
  mutate(difyear = paste0("t - ", difyear))
condG <- agrupar(condG, vstep=year, vgrupo=difyear, vagre=CondCapInt_USD,pct=4,last = 2) 

condG$y <- condG$y %>% 
  mutate(lab = paste0(round(rat),'% (',comma(round(tot/1e3,1)),' M USD)'))  
ggplot(condG$y,aes(x=year,y=rat,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label(aes(label=lab),size=3,color="white",show.legend = F,
             position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=condG$z$year,y=condG$z$rat*1.05,
           label=comma(round(condG$z$tot,0)),size=3.5,color=paleta(12)[7])+
  labs(x="",y="Participación (%)",
       fill="Diferencia con año de desembolso")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2015, 2023, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/conDifDesembolso.png',width = 9,height = 6,units = "in")

#####____**Por ratio de cond/monto____####
condG <- agrupar(bdcYear, vstep=year, vgrupo=binCondMonto, vagre=CondCapInt_USD,pct=6,last = 1) 

condG$y <- condG$y %>% 
  mutate(lab = paste0(round(rat),'% (',comma(round(tot/1e3,1)),' M USD)'))  
ggplot(condG$y,aes(x=year,y=rat,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label(aes(label=lab),size=3,color="white",show.legend = F,
             position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=condG$z$year,y=condG$z$rat*1.05,
           label=comma(round(condG$z$tot,0)),size=3.5,color=paleta(12)[7])+
  labs(x="",y="Participación (%)",
       fill="Ratio Condonado/Desembolso")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2015, 2023, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/conRatio.png',width = 9,height = 6,units = "in")
####____**Tipo de credito____####  
condG <- agrupar(bdcYear, vstep=year, vgrupo=tipoCred, vagre=CondCapInt_USD,pct=12)
condG$y <- condG$y %>% 
  mutate(lab = paste0(round(rat,2),'% (',comma(round(tot/1e3)),' M USD)'))

ggplot(condG$y,aes(x=year,y=rat,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label(aes(label=lab),size=3.5,color="white",show.legend = F,
            position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=condG$z$year,y=condG$z$rat*1.05,
           label=comma(round(condG$z$rat,0)),size=3.5,color=paleta(12)[7])+
  labs(x="",y="Participación en condonaciones (M de USD)",
       fill="Tipo de Crédito")+
  scale_fill_manual(values=paleta(5))+
  scale_x_continuous(breaks = seq(2019, 2023, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/condTipoCred.png', width=9, height=6, units="in")
####____**Sucursal____####
condG <- agrupar(bdcYear, vstep=year, vgrupo=Sucursal, vagre=CondCapInt_USD,pct=12)
condG$y <- condG$y %>% 
  mutate(lab = paste0(round(rat,2),'% (',comma(round(tot/1e3)),' M USD)'))
ggplot(condG$y,aes(x=year,y=rat,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label(aes(label=lab),size=3,color="white",
            position=position_stack(vjust=0.5),show.legend = F)+
  annotate(geom = "label",x=condG$z$year,y=condG$z$rat*1.05,
           label=comma(round(condG$z$tot,0)),size=3.5,color=paleta(12)[7])+
  labs(x="",y="Participación (%)",
       fill="Sucursal")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2019, 2023, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/condSucursal.png', width=9, height=6, units="in")

condGcum <- condG$y %>%
  group_by(ORDEN) %>% 
  arrange(year) %>% 
  mutate(totcum=cumsum(tot)) %>% 
  mutate(lab = paste0(comma(round(totcum/1e3)),' M USD'))

ggplot(condGcum,aes(x=year,y=totcum/1e3,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label_repel(aes(label=lab),size=3,color="white",
                   position=position_stack(vjust=0.5), show.legend = F)+
  # annotate(geom = "label",x=condG$z$year,y=condG$z$rat*1.05,
  #          label=comma(round(condG$z$rat,0)),size=3.5,color=paleta(12)[7])+
  labs(x="",y="Condonaciones (M de USD)",
       fill="Sucursal")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2019, 2023, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/condSucursalCum.png', width=9, height=6, units="in")
####____**Rangom____####
condG <- agrupar(bdcYear, vstep=year, vgrupo=rangom, vagre=CondCapInt_USD,pct=12)
condG$y <- condG$y %>% 
  mutate(lab = paste0(round(rat,2),'% (',comma(round(tot/1e3)),' M USD)'))
ggplot(condG$y,aes(x=year,y=rat,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label(aes(label=lab),size=3.5,color="white",
            position=position_stack(vjust=0.5), show.legend = F)+
  # annotate(geom = "label",x=condG$z$year,y=condG$z$rat*1.05,
  #          label=comma(round(condG$z$rat,0)),size=3.5,color=paleta(12)[7])+
  labs(x="",y="Participación en condonaciones (M de USD)",
       fill="Rango de desembolso")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2019, 2023, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/condRangom.png', width=9, height=6, units="in")

condG$y <- condG$y %>%
  group_by(ORDEN) %>% 
  arrange(year) %>% 
  mutate(totcum=cumsum(tot)) %>% 
  mutate(lab = paste0(comma(round(totcum/1e3)),' M USD'))
condG$z <- condG$z %>% 
  arrange(year) %>% 
  mutate(totcum=cumsum(tot)) %>% 
  mutate(lab = paste0(comma(round(totcum/1e3)),' M USD'))

ggplot(condG$y,aes(x=year,y=totcum/1e3,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label_repel(aes(label=lab),size=3,color="white",
             position=position_stack(vjust=0.5), show.legend = F)+
  annotate(geom = "label",x=condG$z$year,y=condG$z$totcum/1e3+1e3,
           label=condG$z$lab,size=3.5,color=paleta(12)[7])+
  labs(x="",y="Condonaciones (M de USD)",
       fill="Rango de desembolso")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2019, 2023, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/condRangomCum.png', width=9, height=6, units="in")
####____**Rangos____####
condG <- agrupar(bdcYear, vstep=year, vgrupo=rangos, vagre=CondCapInt_USD,pct=12)
condG$y <- condG$y %>% 
  mutate(lab = paste0(round(rat,2),'% (',comma(round(tot/1e3)),' M USD)'))
ggplot(condG$y,aes(x=year,y=rat,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label(aes(label=lab),size=3.5,color="white",
             position=position_stack(vjust=0.5), show.legend = F)+
  # annotate(geom = "label",x=condG$z$year,y=condG$z$rat*1.05,
  #          label=comma(round(condG$z$rat,0)),size=3.5,color=paleta(12)[7])+
  labs(x="",y="Participación en condonaciones (M de USD)",
       fill="Rango de saldo")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2019, 2023, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/condRangos.png', width=9, height=6, units="in")

condG$y <- condG$y %>%
  group_by(ORDEN) %>% 
  arrange(year) %>% 
  mutate(totcum=cumsum(tot))%>% 
  mutate(lab = paste0(comma(round(totcum/1e3)),' M USD'))
condG$z <- condG$z %>% 
  arrange(year) %>% 
  mutate(totcum=cumsum(tot)) %>% 
  mutate(lab = paste0(comma(round(totcum/1e3)),' M USD'))

ggplot(condG$y,aes(x=year,y=totcum/1e3,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label_repel(aes(label=lab),size=3,color="white",
                   position=position_stack(vjust=0.5), show.legend = F)+
  annotate(geom = "label",x=condG$z$year,y=condG$z$totcum/1e3+1e3,
           label=condG$z$lab,size=3.5,color=paleta(12)[7])+
  labs(x="",y="Condonaciones (M de USD)",
       fill="Rango de saldo")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2019, 2023, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/castigos/condRangosCum.png', width=9, height=6, units="in")
####____CLUSTERS____####
bdcLast <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Ene2023.rds') %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  dplyr::filter(MODULO != 131) %>% 
  group_by(AGENCIA) %>% 
  summarise(saldoBSO = sum(saldous, na.rm = T)) %>% 
  left_join(codAge, by="AGENCIA") %>% 
  select(-Sucursal)
sum(bdcLast$saldoBSO, na.rm = T)

condLast <- bdcCond %>% 
  # dplyr::filter(monDate == 'ene. 2023') %>% 
  select(Sucursal, NOMBRE_AGENCIA, CondCapInt_USD) %>% 
  group_by(Sucursal, NOMBRE_AGENCIA) %>% 
  summarise_all(sum, na.rm = T)
sum(condLast$CondCapInt_USD, na.rm = T)

gph <- bdcLast %>% 
  left_join(condLast, by = 'NOMBRE_AGENCIA') %>% 
  group_by(Sucursal, NOMBRE_AGENCIA) %>% 
  mutate(saldoBSO = ifelse(row_number() > 1, 0, saldoBSO)) %>% 
  dplyr::filter(!str_detect(NOMBRE_AGENCIA, 'Normal')) %>% 
  dplyr::filter(!str_detect(NOMBRE_AGENCIA,"Móvil")) %>% 
  mutate(ratio = CondCapInt_USD/saldoBSO) 

k3 <- kmeans(gph[,7], 3, nstart = 20)
clusters <- as.data.frame(k3$cluster) %>% 
  dplyr::rename(Grupo = `k3$cluster`) %>% 
  bind_cols(gph) %>% 
  mutate(Grupo = case_when(Grupo==1~'0.8 % <= Ratio < 9.1%',
                           Grupo==3~'Ratio < 0.8%',
                           Grupo==2~'Ratio >= 9.1%')) %>% 
  mutate(label = paste0(Sucursal, ', ', NOMBRE_AGENCIA, ': ',
                        as.character(round(ratio*100, 1)), '%')) %>% 
  arrange(desc(ratio)) %>% 
  mutate(label = ifelse(row_number() <= 12, label, NA))

ggplot(clusters, aes(x = CondCapInt_USD, y = saldoBSO/1e6, 
                     color = as.factor(Grupo))) + 
  geom_point(size = 3) + 
  geom_label_repel(aes(label = label, color = factor(Grupo)), size = 2.5, show.legend = F) +
  scale_color_manual(values = paleta(5)) +
  scale_x_continuous(label = comma,
                     name = 'Saldo Condonado (M USD desde 2019)') +
  scale_y_continuous(label = comma, name = 'Saldo BSO (MM USD, diciembre 2022)') +
  theme_minimal() + 
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(title="Grupo"))
ggsave('D:/!bso/castigos/conScatter_Ene23.png', width = 9, height = 6, units = "in")

####____CONDONACIONES ACUMULADAS DESDE 2019____####
condExp <- cond %>% 
  mutate(year = year(Fecha)) %>% 
  mutate(monDate = as.yearmon(Fecha)) %>% 
  select(year, monDate, CondCapInt_USD = `Total Cond cap + Int En $us`,
         CondInt_USD = `Cond Intereses En $us`,
         CondCap_USD = `Cond Capital En $us`,
         AGENCIA = Sucursal_operacion) %>%
  left_join(codAge,by="AGENCIA") %>% 
  select(-Regional) %>%
  group_by(year, monDate,Sucursal, AGENCIA, NOMBRE_AGENCIA) %>% 
  summarise_all(sum) %>%
  ungroup() %>% 
  group_by(Sucursal, AGENCIA, NOMBRE_AGENCIA) %>% 
  arrange(monDate) %>% 
  mutate(cumCondCapInt = cumsum(CondCapInt_USD)) %>% 
  mutate(cumCondCap = cumsum(CondCap_USD)) %>% 
  mutate(cumCondInt = cumsum(CondInt_USD)) %>% 
  mutate(rowLast = ifelse(row_number()==max(row_number()),1,0)) %>% 
  ungroup()

condExpAddLast <- condExp %>% 
  dplyr::filter(rowLast==1 & monDate != 'ene. 2023') %>% 
  mutate(CondCapInt_USD = 0,
         CondCap_USD = 0,
         CondInt_USD = 0) %>% 
  select(-rowLast) %>% 
  mutate(monDate = as.yearmon('ene.2023'))

condExp <- condExp %>% 
  select(-rowLast) %>% 
  bind_rows(condExpAddLast)

bdcLast <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Ene2023.rds') %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  dplyr::filter(MODULO != 131) %>% 
  left_join(codAge, by="AGENCIA") %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(year, monDate, Sucursal, AGENCIA, NOMBRE_AGENCIA) %>% 
  summarise(saldoBSO = sum(saldous, na.rm = T))
sum(bdcLast$saldoBSO, na.rm = T)

condExp <- condExp %>% 
  left_join(bdcLast, by=c("year","monDate","Sucursal","AGENCIA","NOMBRE_AGENCIA"))

write.xlsx(condExp,'D:/!bso/castigos/Condonaciones_Ene2023.xlsx')
