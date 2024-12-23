####____CARGA DE LIBRERIAS Y FUNCIONES____####
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
####____CONSOLIDADO CONDONACIONES____####
condMes <- readxl::read_xlsx('D:/!bso/bases/excel/Condonaciones_FebreroMarzo23.xlsx') %>% 
  glimpse() %>% 
  select(1:31)
cond <- readxl::read_xlsx('D:/!bso/bases/excel/condonacionesFeb23.xlsx') %>% 
  glimpse()
tail(condMes %>% group_by(as.yearmon(Fecha)) %>% 
       summarise(n=n()))
tail(cond%>% group_by(as.yearmon(Fecha)) %>% 
       summarise(n=n()))
condFull <- cond %>% 
  bind_rows(condMes)
write_rds(condFull,'D:/!bso/condonaciones/condon/CondFull_Ene19Mar23.rds')
condFull <- readRDS('D:/!bso/condonaciones/condon/CondFull_Ene19Mar23.rds')
####____CONDONACIONES____####
codAge <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')
cond <- readxl::read_xlsx('D:/!bso/bases/excel/condonacionesFeb23.xlsx') %>% 
  glimpse()

process <- function(x, vstep){
  x %>% 
    select(Fecha, Cuenta, Operacion, CondCapInt_USD = `Total Cond cap + Int En $us`,
           CondInt_USD = `Cond Intereses En $us`,
           CondCap_USD = `Cond Capital En $us`,
           Cod_Asesor = Asesor,
           Nombre_Asesor = NombreAsesor,
           Nombre_Agencia= `NOMBRE DE AGENCIA`,
           AGENCIA = Sucursal_operacion,
           {{vstep}}) %>%
    left_join(codAge,by="AGENCIA") %>% 
    select(-Fecha, -Regional) %>% 
    rename(Cod_Agencia = AGENCIA) %>% 
    group_by({{vstep}}, Sucursal, Cod_Agencia, Nombre_Agencia, NOMBRE_AGENCIA, 
             Cod_Asesor, Nombre_Asesor, Cuenta, Operacion) %>% 
    summarise_all(sum) %>% 
    ungroup() %>% 
    mutate(Int_Condonado = ifelse(CondInt_USD>0,1,0)) %>% 
    mutate(Cap_Condonado = ifelse(CondCap_USD>0,1,0)) %>% 
    mutate(IntCap_Condonado = ifelse(CondCapInt_USD>0,1,0)) %>% 
    mutate(Key = paste(Cuenta,Operacion,sep="-")) 
}
####____COND LAST 12 MONTH____####
lastmonth <- "Mar. 2023"
lastmonth12 <- "Abr. 2022"
shortmonth <- str_replace(lastmonth,". ","")
shortmonth12 <- str_replace(lastmonth12,". ","")

gph <- condFull %>% 
  mutate(monDate = as.yearmon(Fecha)) %>% 
  process(vstep = monDate) #La agrupación de una condonación por mes se hace en esta función process
lastCond <- gph %>% 
  dplyr::filter(monDate >= lastmonth12 & monDate<=lastmonth) %>% 
  mutate(Cond_ult_mes = ifelse(monDate==lastmonth,1,0)) %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(NCONDONACIONES = n(),COND_ULT_MES = max(Cond_ult_mes),
            TOTAL_INT_CONDONADO = sum(CondInt_USD),
            TOTAL_CAP_CONDONADO = sum(CondCap_USD),
            TOTAL_CAPINT_CONDONADO = sum(CondCapInt_USD)) %>% 
  ungroup() 

max(lastCond$NCONDONACIONES)
# write.csv(lastCond,'D:/!bso/mph/condonados/listcond.csv',row.names = F)
# lastCond <- fread("D:/!bso/mph/condonados/listcond.csv",
#                   encoding = "UTF-8",sep=",",fill=T)
####____READING PAGOS TARDIOS____####
lastCierre <- fread(paste0("D:/!bso/mph/Oreports/lastCierrreUR_",shortmonth,".csv"),
                    encoding = "UTF-8",sep=",",fill=T) %>% 
  select(Operacion,CTACLIENTE,CI,NOMBRE_CLIENTE,Sucursal_Actual=Sucursal,
         Agencia_Actual = Agencia,Asesor,Saldo_USD,Operaciones,Instancias_UR)

TardioCond <- lastCierre %>% 
  rename(Cuenta=CTACLIENTE) %>% 
  left_join(lastCond,by=c("Cuenta","Operacion")) %>% 
  replace_na(list(NCONDONACIONES=0,COND_ULT_MES=0,TOTAL_INT_CONDONADO=0,
                  TOTAL_CAP_CONDONADO=0,TOTAL_CAPINT_CONDONADO=0,Instancias_UR=0))

dfTotal <- readRDS('D:/!bso/features/Clientes_Ene15Mar23.rds') %>% 
  left_join(codAge,by="AGENCIA") %>% 
  select(Cuenta = CTACLIENTE, Operacion = OPERACION, Sucursal,
         COD_AGENCIA_ORI = AGENCIA, Agencia_Origen = NOMBRE_AGENCIA, 
         ASESOR_ORI = NOMBRE_ASESOR)

TardioCond <- TardioCond %>% 
  left_join(dfTotal,by=c("Cuenta","Operacion")) %>% 
  # mutate(ASESOR_VIGENTE = ifelse(COD_ASESOR_ORI %in% ASESOR,1,0)) %>%
  mutate(AGENCIA_VIGENTE = ifelse(Agencia_Origen %in% Agencia_Actual,1,0))
write_xlsx(TardioCond,paste0('D:/!bso/mph/condonados/TardioCond_',shortmonth,'.xlsx'))
####____LISTA SERGIO____####
agencias_vigentes <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',shortmonth,'.rds')) %>% 
  select(AGENCIA,NOMBRE_AGENCIA) %>% 
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
  select(Sucursal,Agencia_Origen,starts_with("Saldo_Alerta"),starts_with("Ops_Alerta"),
         Saldo_Recurrente_1M,Ops_Condonado_12_meses,Saldo_Total_USD = Saldo_USD,
         Total_CapInt_Condonado=TOTAL_CAPINT_CONDONADO,Operaciones) %>% 
  group_by(Sucursal,Agencia_Origen) %>% 
  summarise_all(sum,na.rm=T) %>% 
  ungroup() %>% 
  mutate(Vigente = ifelse(Agencia_Origen %in% agencias_vigentes$NOMBRE_AGENCIA,1,0)) %>% 
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
  group_by(Sucursal) %>% 
  arrange(desc(Promedio_Cond_y_Tardio)) %>% 
  mutate(Ranking_Sucursal = row_number()) %>% 
  mutate(Fecha = as.Date(as.yearmon(lastmonth),frac=1)) %>% 
  relocate(Vigente, .after = Agencia_Origen) %>% 
  relocate(Fecha, .before = Sucursal)

ListasFull <- list()
ListasFull[[3]] <- Listas

ListFull <- rbindlist(ListasFull)
write_xlsx(Listas,paste0("D:/!bso/mph/condonados/TardiosCondonados_",shortmonth12,shortmonth,"_v1.xlsx"))
write_xlsx(ListFull,paste0("D:/!bso/mph/condonados/TardiosCondonados_EneFebMar2023_v1.xlsx"))

gph <- TardioCond %>% 
  # dplyr::filter(Instancias_UR > 0 | NCONDONACIONES > 0) %>% 
  dplyr::filter(!str_detect(Agencia_Origen,"Normal")) %>% 
  dplyr::filter(!str_detect(Agencia_Origen,"Móvil")) %>% 
  mutate(CondTardio = ifelse(Instancias_UR>0 & NCONDONACIONES>0,1,0)) %>% 
  mutate(TardeOCond = sum(Instancias_UR,NCONDONACIONES,na.rm = T)) %>% 
  mutate(fueTardio = ifelse(Instancias_UR>0,1,0)) %>% 
  mutate(fueCond = ifelse(NCONDONACIONES>0,1,0)) %>% 
  group_by(Agencia_Origen) %>% 
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
  mutate(lab = paste0(Agencia_Origen,' (',round(Ratio_tardio),'% , ',round(Ratio_cond),'%)')) %>% 
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

ggsave('D:/!bso/mph/condonados/scatterMar23.png',height = 6,width = 9,units = "in")
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