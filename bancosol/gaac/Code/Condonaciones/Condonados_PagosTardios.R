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
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.5)
####____CONDONACIONES____####
codAge <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Jul2023.rds') %>% 
  glimpse()

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
    # group_by({{vstep}}, Sucursal, Cod_Agencia, Nombre_Agencia, NOMBRE_AGENCIA, 
    #          Cod_Asesor, Nombre_Asesor, Cuenta, Operacion) %>% 
    summarise_all(sum) %>% 
    ungroup() %>% 
    mutate(Int_Condonado = ifelse(CondInt_USD>0,1,0)) %>% 
    mutate(Cap_Condonado = ifelse(CondCap_USD>0,1,0)) %>% 
    mutate(IntCap_Condonado = ifelse(CondCapInt_USD>0,1,0)) %>% 
    mutate(Key = paste(Cuenta,Operacion,sep="-")) 
}
####____COND LAST 12 MONTH____####
lastmonth <- "May. 2023"
lastmonth12 <- "Jun. 2022"
shortmonth <- str_replace(lastmonth,". ","")
shortmonth12 <- str_replace(lastmonth12,". ","")

gph <- condFull %>% 
  mutate(monDate = as.yearmon(Fecha)) %>% 
  process(vstep = monDate)
lastCond <- gph %>% 
  dplyr::filter(monDate >= lastmonth12 & monDate<=lastmonth) %>% 
  mutate(Cond_ult_mes = ifelse(monDate==lastmonth,1,0)) %>% 
  group_by(Cuenta,Operacion) %>% 
  summarise(NCONDONACIONES = n(), COND_ULT_MES = max(Cond_ult_mes),
            TOTAL_INT_CONDONADO = sum(CondInt_USD),
            TOTAL_CAP_CONDONADO = sum(CondCap_USD),
            TOTAL_CAPINT_CONDONADO = sum(CondCapInt_USD))
write.csv(lastCond,'D:/!bso/mph/condonados/listcond.csv',row.names = F)
lastCond <- fread("D:/!bso/mph/Oreports/listcond.csv",
                  encoding = "UTF-8",sep=",",fill=T)
####____READING PAGOS TARDIOS____####
# asesores <- readRDS('D:/!bso/castigos/asesores.rds')
lastCierre <- fread(paste0("D:/!bso/mph/Oreports/lastCierrreUR_",shortmonth,".txt"),
                    encoding = "UTF-8",sep="|",fill=T) %>% 
  mutate(Ultimo_mes = if_else(is.na(Ultimo_mes),0,Ultimo_mes)) %>% 
  select(Operacion, Cuenta, CI, Sucursal, Agencia, Asesor, Saldo_USD,
         Operaciones, Instancias_UR, PaR_0, Saldo_Mora, ctaCont, Rango_Monto, Tipo_Credito,
         FDESEMBOLSO, Actividad_Cliente, Destino_Credito, Ultimo_mes)

TardioCond <- lastCierre %>% 
  # rename(Cuenta=CTACLIENTE) %>% 
  left_join(lastCond,by=c("Cuenta","Operacion")) %>% 
  replace_na(list(NCONDONACIONES=0,COND_ULT_MES=0,TOTAL_INT_CONDONADO=0,
                  TOTAL_CAP_CONDONADO=0,TOTAL_CAPINT_CONDONADO=0,Instancias_UR=0))

####____ADDING DISBURSMENT DATE FOR REFIN AND REPROG____####
Clientes_ori <- readRDS("D:/!bso/features/Clientes_AjusteRef_Ene15Jul23.rds") %>% 
  select(CTACLIENTE, OPERACION, fdes_original)

bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",shortmonth,".rds")) %>% 
  dplyr::filter(ESTADO!="CASTIGADA") %>% 
  mutate(Tipo_Cartera = case_when(ctaCont %in% c('131','133','134') & OPERACION_ORI_REF==0 ~ 'Normal',
                                  ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                  OPERACION_ORI_REF!=0 ~ 'Refinanciada',)) %>% 
  mutate(Plazo_Years = round(PLAZODIAS/365)) %>% 
  select(CTACLIENTE, OPERACION, RangoMonto = rangom, RangoSaldo = rangos,
         Sector_Actividad, Sector_Destino, Plazo_Years,
         DESC_OBJCRED, DESC_SEGMERC, TIPO_CREDITO, Tipo_Cartera) %>% 
  left_join(Clientes_ori,by=c("CTACLIENTE","OPERACION"))

####____ADDING SEQUENCE AND EXPORTING____####
dfTotal <- readRDS('D:/!bso/features/Clientes_Ene15Jul23_v2.rds') %>% 
  mutate(mydes = as.yearmon(fdes)) %>% 
  dplyr::filter(mydes <= as.yearmon(lastmonth)) %>% 
  group_by(CTACLIENTE) %>% 
  summarise(N_OPNOREFIN = length(which(fueRefin==0)), N_OPREFIN = length(which(fueRefin==1)), N_TOTAL = N_OPNOREFIN+N_OPREFIN) %>% 
  select(Cuenta = CTACLIENTE, N_OPNOREFIN, N_OPREFIN, N_TOTAL) 

TardioCondExp <- TardioCond %>% 
  left_join(dfTotal, by="Cuenta") %>% 
  left_join(bdc, by=c("Cuenta"="CTACLIENTE","Operacion"="OPERACION")) %>% 
  # mutate(ASESOR_VIGENTE = ifelse(COD_ASESOR_ORI %in% ASESOR,1,0)) %>%
  mutate(Alerta = case_when(NCONDONACIONES==0 & Instancias_UR==0 ~ '1. Verde',
                            NCONDONACIONES==0 & Instancias_UR>0 ~ '2. Amarilla',
                            NCONDONACIONES>0 & Instancias_UR==0 ~ '2. Amarilla',
                            (NCONDONACIONES>0 & NCONDONACIONES<4 & Instancias_UR>0 & Instancias_UR<4)~'2.Amarilla',
                            NCONDONACIONES>=4 & (Instancias_UR>0 & Instancias_UR<4) ~ '3. Naranja',
                            (NCONDONACIONES>0 & NCONDONACIONES<4) & Instancias_UR>=4  ~ '3. Naranja',
                            (NCONDONACIONES>=4 & Instancias_UR>=4) ~ '4. Roja'),) %>% 
  mutate(NXN = case_when(NCONDONACIONES>=12 & Instancias_UR>=12 ~ '12x12',
                            NCONDONACIONES>=11 & Instancias_UR>=11 ~ '11x11',
                            NCONDONACIONES>=10 & Instancias_UR>=10 ~ '10x10',
                            NCONDONACIONES>=9 & Instancias_UR>=9 ~ '9x9',
                            NCONDONACIONES>=8 & Instancias_UR>=8 ~ '8x8',
                            NCONDONACIONES>=7 & Instancias_UR>=7 ~ '7x7',
                            NCONDONACIONES>=6 & Instancias_UR>=6 ~ '6x6',
                            NCONDONACIONES>=5 & Instancias_UR>=5 ~ '5x5',
                            NCONDONACIONES>=4 & Instancias_UR>=4 ~ '4x4',
                            NCONDONACIONES>=3 & Instancias_UR>=3 ~ '3x3',
                            NCONDONACIONES>=2 & Instancias_UR>=2 ~ '2x2',
                            NCONDONACIONES>=1 & Instancias_UR>=1 ~ '1x1',))
  # mutate(AGENCIA_VIGENTE = ifelse(AGENCIA_ORI %in% Agencia,1,0)) %>% 
  glimpse()
TardioCondPrev <- read_xlsx('D:/!bso/mph/condonados/TardioCond_May2023_v3.xlsx', sheet = "Datos") %>% 
  mutate(TuvoAlertaRoja = ifelse(Alerta=="4. Roja",1,0)) %>% 
  select(Cuenta, Operacion, TuvoAlertaRoja) 
  
TardioCondExp <- TardioCondExp %>% 
  dplyr::filter(ctaCont!='623') %>% 
  left_join(TardioCondPrev,by=c("Cuenta","Operacion")) %>% 
  replace_na(list(TuvoAlertaRoja = 0))

write_xlsx(TardioCondExp,'D:/!bso/mph/condonados/TardioCond_May2023_v5.xlsx')
####____PLOTS____####
agrupar <- function(x, vstep, vgrupo, vagre, pct=5, tms=100, last= 1){
  ult <- x %>% distinct({{vstep}}) %>% arrange(desc({{vstep}})) %>% 
    dplyr::filter(row_number()==last) %>% pull
  y <- x %>% 
    group_by({{vstep}}) %>% 
    mutate(rat = {{vagre}}/sum({{vagre}}, na.rm = T)*tms) %>% 
    #VARIANTE1
    group_by({{vstep}},{{vgrupo}}) %>%
    summarise(tot = sum({{vagre}}, na.rm = T),rat = sum(rat, na.rm = T)) %>%
    ungroup() %>%
    mutate(ORDEN = ifelse({{vgrupo}} %in% {{vgrupo}}[rat>=pct & {{vstep}} == ult],
                          {{vgrupo}}, "Otros")) %>%
    mutate(ORDEN = fct_reorder(ORDEN,rat)) %>%
    group_by({{vstep}},ORDEN) %>% 
    summarise(tot = sum(tot,na.rm = T),rat=sum(rat,na.rm = T)) %>% 
    ungroup()
  z <- y %>%
    group_by({{vstep}}) %>% 
    summarise(tot = sum(tot,na.rm = T),rat=sum(rat,na.rm = T))
  result <- list(y=y,z=z)
  return(result)
}

Filtrada <- TardioCondExp %>% 
  dplyr::filter(Alerta=="4. Roja")

gph <- agrupar(Filtrada, vstep=Sector_Actividad, vgrupo=Alerta, vagre=Saldo_USD,pct=8,last = 2) 
gph$y <- gph$y %>% 
  mutate(lab = paste0(round(rat),'% (',comma(round(tot/1e3,1)),' M USD)'))  
ggplot(gph$y,aes(x=Sector_Actividad,y=rat,fill=ORDEN))+
  geom_bar(stat="identity")+
  coord_flip()+
  geom_label(aes(label=lab),size=3,color="white",
             position=position_stack(vjust=0.5),show.legend = F)+
  annotate(geom = "label",x=gph$z$Sector_Actividad,y=gph$z$rat*1.05,
           label=comma(round(gph$z$tot,0)),size=3.5,color=paleta(12)[7])+
  labs(x="", y="Participación (%)", fill="Sector Actividad")+
  scale_fill_manual(values = paleta(8))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")

gph <- TardioCondExp %>%
  group_by(SectorActvidad) %>% 
ggplot(TardioCondExp, aes(x=Sector_Actividad,fill=Alerta)) +
  geom_bar() +
  scale_fill_manual(values = c('red2','orange','yellow2','green')) +
  theme_light() 
  
ggplot(TardioCondExp, aes(x=Sector_Actividad,fill=Sector_Destino))
####____LISTA SERGIO____####
agencias_vigentes <- readRDS('D:/!bso/girCartera/rds/ec_Ene2023.rds') %>% 
  select(AGENCIA, NOMBRE_AGENCIA) %>% 
  distinct_all()

Listas <- TardioCond %>% 
  mutate(Alerta = case_when(NCONDONACIONES==0 & Instancias_UR==0 ~ '1. Verde',
                            (NCONDONACIONES>0) & Instancias_UR==0 ~ '2. Amarilla',
                            NCONDONACIONES==0 & Instancias_UR>0 ~ '2. Amarilla',
                            NCONDONACIONES>0 & (Instancias_UR>0 & Instancias_UR<4) ~ '3. Naranja',
                            (NCONDONACIONES>0 & NCONDONACIONES<4) & Instancias_UR>0  ~ '3. Naranja',
                            (NCONDONACIONES>=4 & Instancias_UR>=4) ~ '4. Roja'),) %>% 
  mutate(Saldo_Recurrente_1M = ifelse(Instancias_UR>0,Saldo_USD,0)) %>% 
  mutate(Ops_Condonado_12_meses = ifelse(NCONDONACIONES>0, 1, 0)) %>% 
  mutate(Saldo_Alerta_Verde = ifelse(Alerta=="1. Verde",Saldo_USD,0)) %>% 
  mutate(Saldo_Alerta_Amarilla = ifelse(Alerta=="2. Amarilla",Saldo_USD,0)) %>% 
  mutate(Saldo_Alerta_Naranja = ifelse(Alerta=="3. Naranja",Saldo_USD,0)) %>% 
  mutate(Saldo_Alerta_Roja = ifelse(Alerta=="4. Roja",Saldo_USD,0)) %>% 
  mutate(Ops_Alerta_Verde = ifelse(Alerta=="1. Verde",1,0)) %>% 
  mutate(Ops_Alerta_Amarilla = ifelse(Alerta=="2. Amarilla",1,0)) %>% 
  mutate(Ops_Alerta_Naranja = ifelse(Alerta=="3. Naranja",1,0)) %>% 
  mutate(Ops_Alerta_Roja = ifelse(Alerta=="4. Roja",1,0)) %>% 
  select(SUCURSAL,AGENCIA_ORI,starts_with("Saldo_Alerta"),starts_with("Ops_Alerta"),
         Saldo_Recurrente_1M,Ops_Condonado_12_meses,Saldo_Total_USD = Saldo_USD,
         TOTAL_CAPINT_CONDONADO,Operaciones) %>% 
  group_by(SUCURSAL,AGENCIA_ORI) %>% 
  summarise_all(sum,na.rm=T) %>% 
  ungroup() %>% 
  mutate(Vigente = ifelse(AGENCIA_ORI %in% agencias_vigentes$NOMBRE_AGENCIA,1,0)) %>% 
  mutate(Saldo_Verde_Saldo_Total = Saldo_Alerta_Verde/Saldo_Total_USD) %>% 
  mutate(Saldo_Amarilla_Saldo_Total = Saldo_Alerta_Amarilla/Saldo_Total_USD) %>% 
  mutate(Saldo_Naranja_Saldo_Total = Saldo_Alerta_Naranja/Saldo_Total_USD) %>% 
  mutate(Saldo_Roja_Saldo_Total = Saldo_Alerta_Roja/Saldo_Total_USD) %>% 
  mutate(Ops_Verde_Operaciones = Ops_Alerta_Verde/Operaciones) %>% 
  mutate(Ops_Amarilla_Operaciones = Ops_Alerta_Amarilla/Operaciones) %>% 
  mutate(Ops_Naranja_Operaciones = Ops_Alerta_Naranja/Operaciones) %>% 
  mutate(Ops_Roja_Operaciones = Ops_Alerta_Roja/Operaciones) %>% 
  mutate(Saldo_Recurrente_Cartera = Saldo_Recurrente_1M/Saldo_Total_USD) %>% 
  mutate(Ops_Condonado_Operaciones = Ops_Condonado_12_meses/Operaciones) %>% 
  rowwise() %>% 
  mutate(Promedio_Cond_y_Tardio = mean(c(Saldo_Recurrente_Cartera,Ops_Condonado_Operaciones),na.rm=T)) %>% 
  ungroup() %>% 
  group_by(SUCURSAL) %>% 
  arrange(desc(Promedio_Cond_y_Tardio)) %>% 
  mutate(Ranking_Sucursal = row_number())

write_xlsx(Listas,"D:/!bso/mph/condonados/TardiosCondonados_Feb22Ene23_v2.xlsx")

Listas <- read_xlsx("D:/!bso/mph/condonados/TardiosCondonados_Feb22Ene23_v2.xlsx") %>% 
  rowwise() %>% 
  mutate(Promedio_Cond_y_Tardio = mean(c(Saldo_Recurrente_Cartera,Ops_Condonado_Operaciones),na.rm=T)) %>% 
  ungroup() %>% 
  group_by(Sucursal) %>% 
  arrange(desc(Promedio_Cond_y_Tardio)) %>% 
  mutate(Ranking_Sucursal = row_number())

write_xlsx(Listas,"D:/!bso/mph/condonados/TardiosCondonados_Feb22Ene23_v3.xlsx")

gph <- TardioCond %>% 
  # dplyr::filter(Instancias_UR > 0 | NCONDONACIONES > 0) %>% 
  dplyr::filter(!str_detect(AGENCIA_ORI,"Normal")) %>% 
  dplyr::filter(!str_detect(AGENCIA_ORI,"Móvil")) %>% 
  mutate(CondTardio = ifelse(Instancias_UR>0 & NCONDONACIONES>0,1,0)) %>% 
  mutate(TardeOCond = sum(Instancias_UR,NCONDONACIONES,na.rm = T)) %>% 
  mutate(fueTardio = ifelse(Instancias_UR>0,1,0)) %>% 
  mutate(fueCond = ifelse(NCONDONACIONES>0,1,0)) %>% 
  group_by(AGENCIA_ORI) %>% 
  mutate(maxT = ifelse(TardeOCond==max(TardeOCond),Instancias_UR,0)) %>% 
  mutate(maxC = ifelse(TardeOCond==max(TardeOCond),NCONDONACIONES,0)) %>% 
  summarise(Saldo_Total = sum(Saldo_USD,na.rm = T),
            Monto_cond = sum(TOTAL_CAPINT_CONDONADO,na.rm = T),
            Ratio_tardio = sum(fueTardio*Saldo_USD,na.rm = T)/Saldo_Total*100,
            Ratio_cond = sum(fueCond,na.rm = T)/n()*100,
            NOps = sum(Operaciones),
            P_TARDIOS = round(max(Instancias_UR)),
            P_CONDONACIONES = round(max(NCONDONACIONES))) %>% 
  ungroup() %>% 
  mutate(Ratio_cond_bin = case_when(Ratio_cond<5~'1. < 5%',
                                    Ratio_cond<10~'2. (5-10)%',
                                    Ratio_cond<15~'3. (10-15)%',
                                    Ratio_cond>=15~'4. >= 15%',)) %>%
  mutate(Ratio_tardio_bin = case_when(Ratio_tardio<5~'1. < 5%',
                                      Ratio_tardio<10~'2. (5-10)%',
                                      Ratio_tardio<15~'3. (10-15)%',
                                      Ratio_tardio>=15~'4. >= 20%',)) %>% 
  mutate(lab = paste0(AGENCIA_ORI,' (',round(Ratio_tardio),'% , ',round(Ratio_cond),'%)')) %>% 
  mutate(lab=ifelse(Ratio_cond>10 & Ratio_tardio>10,lab,NA))

ggplot(gph,aes(x=Saldo_Total/1e6,y=Monto_cond/1e3,
               color=Ratio_cond_bin,size=as.factor(Ratio_tardio_bin))) +
  geom_point() +
  geom_label_repel(aes(label=lab),size=3.5,show.legend = F) +
  labs(x = "Saldo (MM USD)", y = "Monto condonado (M USD)",
       color = "Condonaciones/Operaciones", size = "Saldo tardío/Saldo")+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)+
  # scale_color_gradient(low="green",high = "red")+
  # scale_color_manual(values = paleta(12))+
  theme_light()

ggsave('D:/!bso/mph/condonados/scatter.png',height = 6,width = 9,units = "in")
ggplot(gph,aes(x=saldo_tardio,y=monto_cond,
               size=P85_TARDIOS,color=as.factor(P85_CONDONACIONES))) +
  geom_point() +
  geom_label_repel(aes(label=lab),size=3,show.legend = F) +
  labs(x = "Saldo tardío recurrente", y = "Monto condonado",
       color = "Perc 85 condonaciones", size = "Perc 85 pagos tardíos")+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)+
  scale_color_manual(values = paleta(12))+
  theme_light()
####____VIOLIN PLOT____####
TardioCond <- lastCierre %>% 
  rename(Cuenta=CTACLIENTE) %>% 
  inner_join(lastCond,by=c("Cuenta","Operacion")) %>% 
  replace_na(list(Instancias_UR=0,NCONDONACIONES))

ggplot(TardioCond,aes(x=as.factor(Instancias_UR),y=NCONDONACIONES))+
  geom_violin(draw_quantiles = c(0.25,0.5,0.75),fill=paleta(12)[11]) +
  labs(x="Pagos tardíos en los últimos 12 meses",
       y="Condonaciones en los últimos 12 meses")+
  scale_y_continuous(breaks = 0:12,labels = 0:12)+
  theme_light()

gph <- TardioCond %>%
  dplyr::filter(Instancias_UR>1) %>% 
  group_by(Instancias_UR) %>% 
  mutate(pct = 1/n()) %>% 
  ungroup() %>% 
  group_by(Instancias_UR,NCONDONACIONES) %>% 
  summarise(n=sum(pct),Ops=n(),Cond=sum(TOTAL_CAPINT_CONDONADO)/sum(Saldo_USD)) %>% 
  ungroup()

ggplot(TardioCond,aes(x=as.factor(Instancias_UR),y=NCONDONACIONES))+
  stat_bin_2d(aes(fill=after_stat(density))) +
  labs(x="Pagos tardíos en los últimos 12 meses",
       y="Condonaciones en los últimos 12 meses",
       fill="Porcentaje")+
  scale_y_continuous(breaks = 0:12,labels = 0:12)+
  scale_fill_gradientn(limits=c(0,1),breaks = seq(0,1,by=0.25),
                       colours = paleta(12)[2:7])+
  coord_flip()+
  theme_light()  

ggplot(gph,aes(x=as.factor(Instancias_UR),y=as.factor(NCONDONACIONES)))+
  geom_tile(aes(fill=Cond)) +
  geom_label(aes(label=Ops))+
  labs(x="Pagos tardíos en los últimos 12 meses",
       y="Condonaciones en los últimos 12 meses",
       fill="Porcentaje")
  scale_y_continuous(breaks = 0:12,labels = 0:12)+
  scale_fill_gradientn(limits=c(0,1),breaks = seq(0,1,by=0.25),
                       colours = paleta(12)[2:7])+
  coord_flip()+
  theme_light()
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
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds[i],'.rds')) %>%
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

write_rds(bdcCond,'D:/!bso/castigos/Condonaciones.rds')