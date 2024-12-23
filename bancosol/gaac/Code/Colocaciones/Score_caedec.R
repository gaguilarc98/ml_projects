####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(lubridate)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(scales)
library(stringr)
library(forcats)
library(tseries)
library(scales)
library(janitor)
library(ggplot2)
library(openxlsx)
require(XLConnect)
Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____FUNCIONES____####
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
getTardios <- function(x){
  x %>%
    mutate(hourUltDia = as.numeric(str_sub(Hora_UltDia, 1, 2))) %>%
    dplyr::filter(myPago < "mar. 2020" | myPago >= "dic. 2020") %>%
    group_by(myPago, yearPago, mesPago) %>% 
    mutate(maxDia = max(dayPago)) %>% 
    mutate(maxDia = ifelse(myPago=='may. 2018',30,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='sep. 2018',29,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='dic. 2018',29,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='jun. 2019',29,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='dic. 2019',30,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='dic. 2020',30,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='ene. 2021',30,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='feb. 2021',27,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='dic. 2021',30,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='feb. 2022',25,maxDia)) %>% 
    mutate(maxDia = ifelse(myPago=='dic. 2022',30,maxDia)) %>% 
    ungroup() %>% 
    mutate(FechaPago = as.Date(paste(yearPago, mesPago, dayPago), format = '%Y%m%d')) %>% 
    mutate(appsH = ifelse(dayPago >= maxDia-2 & FechaPago > FechaPrevistaPago, 1, 0)) %>% 
    mutate(appsU = case_when(FechaPago > FechaPrevistaPago & (dayPago==maxDia & hourUltDia >= 12) | 
                               (dayPago>maxDia) ~ 1,
                             TRUE~0))
}
####____CARGA DE CAEDEC____####
caedec <- readRDS("D:/!bso/colocaciones/evolutivo_caedec.rds")
codAge <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')
####____ADDING CASTIGOS, TARDIOS AND CONDONATION BY MONTH____####
#CONDONACIONES
condFull <- readRDS('D:/!bso/condonaciones/condon/CondFull_Ene19Mar23.rds')
lastmonth <- "Mar. 2023"
lastmonth12 <- "Abr. 2022"
shortmonth <- str_replace(lastmonth,". ","")
shortmonth12 <- str_replace(lastmonth12,". ","")

gph <- condFull %>% 
  mutate(monDate = as.yearmon(Fecha)) %>% 
  process(vstep = monDate)
Cond_count <- gph %>% 
  dplyr::filter(monDate >= lastmonth12 & monDate<=lastmonth) %>% 
  mutate(Cond_ult_mes = ifelse(monDate==lastmonth,1,0)) %>% 
  group_by(Cuenta,Operacion) %>% 
  summarise(CapInt_Cond = sum(CondCapInt_USD),
            N_Cond = n()) %>% 
  ungroup() %>% 
  rename(CTACLIENTE=Cuenta, OPERACION=Operacion)
#TARDIOS
bases <- c("Ene2022_Oct2022","Nov2022","Dic2022","Ene2023","Feb2023","Mar2023")
P2uhList <- list()
for (i in 1:length(bases)) {
  P2uhFull <- readRDS(paste0("D:/!bso/mph/rds2/PagosCartera_",bases[i],".rds"))
  P2uhList[[i]] <- P2uhFull
}
P2full <- rbindlist(P2uhList)

P2uh <- P2full %>% 
  dplyr::filter(myPago >= lastmonth12 & myPago <= lastmonth) %>% 
  getTardios()

Tardios_count <- P2uh %>% 
  dplyr::filter(appsH >= 1 & appsU >= 1) %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(N_Tardios = sum(appsU)) %>% 
  ungroup() %>% 
  rename(CTACLIENTE = Cuenta, OPERACION = Operacion) %>% 
  dplyr::filter(N_Tardios>=4)

#CASTIGOS
bdcCast <- readRDS('D:/!bso/castigos/bdcCast_v2.rds')

CheckCastMonth <- bdcCast %>% 
  group_by(monDate) %>% 
  summarise(saldoCast=sum(saldoCast,na.rm = T),saldo=sum(unique(SaldoBSO),na.rm=T))

bdcCastNew <- bdcCast %>% 
  dplyr::filter((year>=2022 & rowCast==1)) %>% 
  mutate(SaldoNew = saldoCast)

CheckCastNew <- bdcCastNew %>% 
  group_by(year) %>% 
  summarise(SaldoNew=sum(SaldoNew,na.rm = T))

mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
years <- c(2022,2023)
mycast <- as.vector(sapply(years, function(x){paste0(mes,x)}))
mycast <- mycast[-c(which(mycast=="Abr2023"):length(mycast))]
bdcList <- list()
for (i in 1:length(mycast)) {
  print(mycast[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mycast[i],'.rds')) %>%
    dplyr::filter(ESTADO != 'CASTIGADA') %>% #Eliminamos las operaciones castigadas
    select(CTACLIENTE,OPERACION,CAEDEC_DEST,Sucursal,saldous,monDate)
  bdcList[[i]] <- bdc
}
bdc2022 <- bind_rows(bdcList) %>% 
  group_by(CTACLIENTE,OPERACION) %>% 
  arrange(monDate) %>% 
  dplyr::filter(row_number()==1) %>% 
  select(-monDate) %>% 
  ungroup()

CastCaedec <- bdcCastNew %>% 
  select(-Sucursal) %>% 
  left_join(bdc2022,by=c("CTACLIENTE","OPERACION")) %>% 
  rename(CAEDEC = CAEDEC_DEST) %>%
  mutate(CAEDEC=as.numeric(CAEDEC)) %>% 
  group_by(CAEDEC,Sucursal) %>% 
  summarise(Saldo_Cast_Nuevo=sum(SaldoNew),Operacion_Cast_Nueva = n_distinct(OPERACION))
CondCaedec <- Cond_count %>% 
  left_join(bdc2022,by=c("CTACLIENTE","OPERACION")) %>% 
  rename(CAEDEC = CAEDEC_DEST) %>% 
  mutate(CAEDEC=as.numeric(CAEDEC)) %>% 
  group_by(CAEDEC,Sucursal) %>% 
  summarise(Monto_Cond = sum(CapInt_Cond), Ops_Cond = n_distinct(OPERACION))

TardiosCaedec <- bind_rows(bdcList) %>% 
  dplyr::filter(monDate=="mar. 2023") %>% 
  inner_join(Tardios_count) %>% 
  rename(CAEDEC = CAEDEC_DEST) %>% 
  mutate(CAEDEC=as.numeric(CAEDEC)) %>% 
  group_by(CAEDEC,Sucursal) %>% 
  summarise(Saldo_Tardio_4M = sum(saldous),Ops_Tardio_Recurrente = n_distinct(OPERACION))
#Nota: A Marzo el informe indica 5551 ops con 4+InstanciasUR, ahora se tienen
#5559 ops (y obvio mayor SaldoRecurrente4+) esto a raíz de la actualización de
#criterios para pagos tardios
####____ADDING METRICS____####
caedecVars <- caedec %>% 
  dplyr::filter(monDate >= "abr. 2022") %>% 
  dplyr::filter(Descripción!="Otros") %>% 
  dplyr::filter(Operaciones>median(Operaciones)) %>%
  left_join(CastCaedec,by=c("CAEDEC","Sucursal")) %>% 
  left_join(CondCaedec,by=c("CAEDEC","Sucursal")) %>% 
  left_join(TardiosCaedec,by=c("CAEDEC","Sucursal")) %>% 
  group_by(CAEDEC,Sucursal) %>% 
  dplyr::filter(max(row_number())==12) %>% 
  arrange(CAEDEC,Sucursal,monDate) %>%
  mutate(dif12m_Mora_ = `Mora 1 día (%)`-dplyr::lag(`Mora 1 día (%)`,1)) %>% 
  mutate(dif12m_Mora = mean(dif12m_Mora_,na.rm = T)) %>% 
  mutate(dif12m_Mora_ = ifelse(dif12m_Mora_<0,NA,dif12m_Mora_),
         semi_sd_Mora = sd(dif12m_Mora_,na.rm = T)) %>% 
  mutate(dif12m_MoraCast_ = `Mora 1 día + Saldo Castigado/Cartera Bruta (%)`-dplyr::lag(`Mora 1 día + Saldo Castigado/Cartera Bruta (%)`,1)) %>% 
  mutate(dif12m_MoraCast = mean(dif12m_MoraCast_,na.rm = T)) %>% 
  mutate(dif12m_MoraCast_ = ifelse(dif12m_Mora_<0,NA,dif12m_Mora_),
         semi_sd_MoraCast = sd(dif12m_MoraCast_,na.rm = T)) %>% 
  mutate(dif12m_Reprog_ = `Cartera reprogramada/Cartera Bruta (%)`-dplyr::lag(`Cartera reprogramada/Cartera Bruta (%)`,1)) %>% 
  mutate(dif12m_Reprog = mean(dif12m_Reprog_,na.rm = T)) %>% 
  mutate(dif12m_Reprog_ = ifelse(dif12m_Reprog_<0,NA,dif12m_Mora_),
         semi_sd_Reprog = sd(dif12m_Reprog_,na.rm = T)) %>% 
  select(-dif12m_Mora_,-dif12m_MoraCast_,-dif12m_Reprog_) %>% 
  replace_na(list(semi_sd_Mora=0,semi_sd_MoraCast=0,semi_sd_Reprog=0,Saldo_Cast_Nuevo=0,
                  Operacion_Cast_Nueva=0,Ops_Cond=0,Monto_Cond=0,Ops_Tardio_Recurrente=0,
                  Saldo_Tardio_4M=0)) %>%
  mutate(prom_Mora = mean(`Mora 1 día (%)`)) %>% 
  mutate(prom_MoraCast = mean(`Mora 1 día + Saldo Castigado/Cartera Bruta (%)`)) %>% 
  mutate(prom_Reprog = mean(`Cartera reprogramada/Cartera Bruta (%)`)) %>% 
  mutate(cv_Mora = ifelse(prom_Mora!=0,sd(`Mora 1 día (%)`)/prom_Mora,0)) %>% 
  mutate(cv_MoraCast = ifelse(prom_MoraCast!=0,sd(`Mora 1 día + Saldo Castigado/Cartera Bruta (%)`)/prom_MoraCast,0)) %>% 
  mutate(cv_Reprog = ifelse(prom_Reprog!=0,sd(`Cartera reprogramada/Cartera Bruta (%)`)/prom_Reprog,0)) %>% 
  mutate(Ops_Mora_Rel = `Operaciones Mora 0 días`/Operaciones[monDate=="Mar. 2023"]) %>% 
  mutate(Ops_Cast_Rel = Operacion_Cast_Nueva/Operaciones[monDate=="Mar. 2023"]) %>% 
  mutate(Ops_Tardio_Rel = Ops_Tardio_Recurrente/Operaciones[monDate=="Mar. 2023"]) %>% 
  mutate(Ops_Cond_Rel = Ops_Cond/Operaciones[monDate=="Mar. 2023"]) %>% 
  mutate(Saldo_Cast_Rel = Saldo_Cast_Nuevo/`Cartera Bruta (USD)`[monDate=="Mar. 2023"]) %>% 
  mutate(Monto_Cond_Rel = Monto_Cond/`Cartera Bruta (USD)`[monDate=="Mar. 2023"]) %>% 
  mutate(Saldo_Tardio_Rel = Saldo_Tardio_4M/`Cartera Bruta (USD)`[monDate=="Mar. 2023"]) %>% 
  ungroup()

caedecScore <- caedecVars %>% 
  select(CAEDEC,Descripción,Sucursal,starts_with("dif12m"),starts_with("prom_"),
         starts_with("cv_"),starts_with("semi_"),Ops_Mora_Rel,Ops_Cast_Rel,
         Ops_Tardio_Rel,Ops_Cond_Rel,Saldo_Cast_Rel,Monto_Cond_Rel,Saldo_Tardio_Rel) %>% 
  group_by(CAEDEC,Descripción,Sucursal) %>% 
  summarise_all(max,na.rm=T) %>% 
  ungroup() %>% 
  mutate(across(dif12m_Mora:Saldo_Tardio_Rel,~.x-min(.x,na.rm = T))) %>% 
  mutate(across(dif12m_Mora:Saldo_Tardio_Rel,~cuts(.x,levs=quantile(.x,c(0,seq(0.25,1,0.025)),na.rm=T),
                                               values=c(0.25,seq(0.25,1,0.025)),lowest = -0.1)))
  # mutate(across(c(dif12m_Mora:dif12m_Reprog,cv_Mora:semi_sd_Reprog),~ (.x-min(.x))/(max(.x)-min(.x)))) %>% 
  # mutate(across(c(prom_Mora:prom_Reprog),~.x/100)) 

ListasCaedec <- list(Lista = caedecVars, Medidas = caedecScore)
write.xlsx(ListasCaedec,"D:/!bso/colocaciones/Lista_caedec_Mar23_v3.xlsx")

x <- caedecScore %>% 
  mutate(Score=rowMeans(select(.,dif12m_Mora:Saldo_Tardio_Rel)))
hist(x$Score)

caedecRanking <- caedecVars %>% 
  left_join(select(caedecScore,CAEDEC,Sucursal,Score,Score_Pond),by=c("CAEDEC","Sucursal")) %>% 
  arrange(desc(Score)) %>% 
  mutate(Scorebin=cut(Score,breaks = c(0,0.76,0.83,1),labels=c("1: Peor","2. Medio","3. Mejor"))) %>% 
  mutate(Scorebin = as.character(Scorebin))

write_rds(caedecRanking,"D:/!bso/colocaciones/caedec_score.rds")

####____MULTIVARIATE PLOTS____####
caedecPlots <- caedecRanking %>% 
  dplyr::filter(monDate=="mar. 2023") %>% 
  mutate(Ops_Mora_Rel = `Operaciones Mora 0 días`/Operaciones) %>% 
  select(`Mora 1 día (%)`:`Cartera reprogramada/Cartera Bruta (%)`,Ops_Mora_Rel,Scorebin)

plot(caedecPlots[,-ncol(caedecPlots)],col=factor(caedecPlots$Scorebin))

caedecRanking %>% 
  dplyr::filter(monDate=="mar. 2023") %>% 
  mutate(Ops_Mora_Rel = `Operaciones Mora 0 días`/Operaciones) %>% 
  ggplot(aes(x=Ops_Mora_Rel*100,y=`Cartera reprogramada/Cartera Bruta (%)`*100,color=Scorebin)) +
  geom_point(size=2) +
  scale_color_manual(values=paleta(12)[c(6,7,9)]) +
  labs(x="Operaciones en Mora (%)",y = "Cartera reprogramada/Cartera bruta",
       color="Grupo de Score") +
  theme_light()

####____WITH CLUSTERS___####
k <- kmeans(caedecScore[,-c(1:3)],centers = 3,nstart = 25)

clusters <- as.data.frame(k$cluster) %>% 
  dplyr::rename(Grupo = `k$cluster`) %>% 
  bind_cols(caedecScore)

caedecRanking2 <- caedecRanking %>% 
  dplyr::filter(monDate=="mar. 2023") %>% 
  mutate(Ops_Mora_Rel = `Operaciones Mora 0 días`/Operaciones) %>% 
  left_join(select(clusters,CAEDEC,Sucursal,Grupo),by=c("CAEDEC","Sucursal")) %>% 
  arrange(desc(Score)) %>% 
  mutate(Scorebin=cut(Score,breaks = c(0,0.76,0.83,1),labels=c("1: Peor","2. Medio","3. Mejor"))) %>% 
  mutate(Scorebin = as.character(Scorebin))

ggplot(caedecRanking2, aes(x=Ops_Mora_Rel*100,y=`Cartera reprogramada/Cartera Bruta (%)`*100,color=factor(Grupo))) + 
  geom_point(size = 3) +
  scale_color_manual(values = paleta(5)) +
  theme_minimal() + 
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(title="Grupo"))
