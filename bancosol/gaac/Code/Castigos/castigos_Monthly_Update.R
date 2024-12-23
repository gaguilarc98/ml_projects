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
library(openxlsx)
library(ggplot2)
library(ggrepel)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
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
agrupar <- function(x, vstep, vgrupo, vagre, pct=5, tms=100, last= 1){
  ult <- x %>% distinct({{vstep}}) %>% arrange(desc({{vstep}})) %>% 
    dplyr::filter(row_number()==last) %>% pull
  y <- x %>% 
    group_by({{vstep}}) %>% 
    mutate(rat = {{vagre}}/sum({{vagre}}, na.rm = T)*tms) %>% 
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
####___LECTURA DE BASES DE DATOS____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2015:2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)}))
myrds <- myrds[-c(which(myrds=="May2023"):length(myrds))]
i <- 1
for (i in 1:length(myrds)) {
  tryCatch({
    print(i)
    print(myrds[i])
    if(i==1){
      dfTotal <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds[i],'.rds')) %>% 
        dplyr::filter(ESTADO=="CASTIGADA") %>% 
        mutate(mesCast = monDate-1/12) %>% 
        select(CTACLIENTE, OPERACION, saldoCast, monDate, mesCast)
    }else{
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds[i],'.rds')) %>% 
        mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
        dplyr::filter(ESTADO=="CASTIGADA") %>% 
        select(CTACLIENTE, OPERACION, saldoCast, monDate) #Para seleccionar operaciones castigadas
      dfCancel <- dfTotal %>% 
        group_by(CTACLIENTE, OPERACION) %>% 
        dplyr::filter(min(saldoCast)!=0) %>% #Para no repetir a los que ya fueron cancelados en meses anteriores
        arrange(desc(monDate)) %>% #Ordenamos de forma descendente por mes
        dplyr::filter(row_number()==1) %>% #filtramos la primera fila. Obtenemos el último mes
        ungroup() %>% 
        anti_join(df1,by=c("CTACLIENTE","OPERACION")) %>% #Fijamos los castigados que ya no están en df1
        mutate(monDate = monDate+1/12) %>% #Cambiamos su último mes al mes siguiente
        mutate(saldoCast = 0) #Se asigna 0 al saldo
      dfNew <- df1 %>% 
        anti_join(dfTotal,by=c("CTACLIENTE","OPERACION")) %>% #Nuevos cancelados que no estaban
        mutate(mesCast = monDate) #Se coloca el mes de castigo
      
      dfTotalAux <- dfTotal %>% 
        select(CTACLIENTE, OPERACION, mesCast) %>% 
        distinct_all() #Obtener el mes de cancelación de todas las operaciones castigadas hasta ahora
      
      #Agregar la info de la última base a los castigados que aún están en cartera
      dfKeep <- df1 %>% 
        inner_join(dfTotalAux, by=c("CTACLIENTE","OPERACION")) 
      
      dfTotal <- dfTotal %>% #Partir del historial de castigados
        bind_rows(dfCancel) %>% #Unir castigados que cancelaron
        bind_rows(dfNew) %>% #Unir nuevos castigos
        bind_rows(dfKeep) #Unir última info de castigados en cartera
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

saveRDS(dfTotal, "D:/!bso/castigos/castigos_Mar2023.rds")
castigos <- readRDS("D:/!bso/castigos/castigos_Mar2023.rds")

####____UPDATE MONTHS TO CASTIGOS____####
dfTotal <- readRDS("D:/!bso/castigos/castigos_May2023_v2.rds")
tail(dfTotal %>% group_by(monDate) %>% summarise(Saldo=sum(saldoCast),nOps=n()))
myrds <- c("Jun2023")
for (i in 1:length(myrds)) {
  tryCatch({
    print(i)
    print(myrds[i])
    
    df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds[i],'.rds')) %>% 
      mutate(MONTOUS = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>% 
      dplyr::filter(ESTADO=="CASTIGADA") %>% 
      select(CTACLIENTE, OPERACION, saldoCast, monDate) 
    dfCancel <- dfTotal %>% 
      group_by(CTACLIENTE, OPERACION) %>% 
      dplyr::filter(min(saldoCast)!=0) %>% #Para no repetir a los que ya fueron cancelado en meses anteriores
      arrange(desc(monDate)) %>% #Ordenamos descendentemente por mes
      dplyr::filter(row_number()==1) %>% #filtramos la primera fila
      ungroup() %>% 
      anti_join(df1,by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(monDate = monDate+1/12) %>% #Forzamos a tener un mes adicional 
      mutate(saldoCast = 0) #Forzamos el saldo 0 cuando un castigado sale de la cartera
    dfNew <- df1 %>% 
      anti_join(dfTotal,by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(mesCast = monDate)
    
    dfTotalAux <- dfTotal %>% 
      select(CTACLIENTE, OPERACION, mesCast) %>% 
      distinct_all()
    
    dfKeep <- df1 %>% 
      inner_join(dfTotalAux, by=c("CTACLIENTE","OPERACION"))
    
    dfTotal <- dfTotal %>% 
      bind_rows(dfCancel) %>% 
      bind_rows(dfNew) %>% 
      bind_rows(dfKeep)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
tail(dfTotal %>% group_by(monDate) %>% summarise(Saldo=sum(saldoCast),nOps=n()))
#Cambiar nombre al archivo antes de guardar
saveRDS(dfTotal, "D:/!bso/castigos/castigos_Jun2023.rds")

####____CIERRES MENSUALES____####
cierre <- c("Dic2015","Dic2016","Dic2017","Dic2018","Dic2019","Dic2020","Dic2021","Dic2022","Jun2023")
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

#####____ADDING FEATURES____####
remove(list = c("dfTotal","df1","dfTotalAux","dfNew","dfKeep","dfCast","dfCancel"))
codAge <- read_excel("D:/!bso/bases/excel/codAgeSucReg.xlsx", sheet = "Old")
Features <- readRDS('D:/!bso/features/Clientes_Ene15Jun23_v2.rds') %>% 
  group_by(CTACLIENTE) %>% 
  arrange(desc(monDate)) %>% 
  mutate(GENERO = GENERO[row_number()==1]) %>% 
  ungroup() %>% 
  select(CTACLIENTE, OPERACION, GENERO, AGENCIA, ASESOR, NOMBRE_ASESOR, 
         fdes, MONTOUS, fueReprog, fueRefin)

castigos <- readRDS("D:/!bso/castigos/castigos_Jun2023.rds")

castFeatures <- castigos %>% 
  left_join(Features, by=c("CTACLIENTE","OPERACION")) %>% 
  # mutate(CAEDEC_DC = cases(grupoCaedecC,c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'),
  #                          c('5. Agropecuario','5. Agropecuario','4. Productivo','4. Productivo','5. Ind. Manufacturera',
  #                            '4. Productivo','3. Construcción','2. Comercio','1. Servicios','1. Servicios','1. Servicios','1. Servicios',
  #                            '1. Servicios','1. Servicios','1. Servicios','1. Servicios','1. Servicios','1. Servicios'))) %>% 
  left_join(select(codAge, AGENCIA, NOMBRE_AGENCIA, Sucursal), by="AGENCIA")

####____CASTIGOS____####
check <- castFeatures %>% 
  group_by(monDate) %>%
  summarise(Saldo=sum(saldoCast)) %>% 
  ungroup() 

castFeatures <- castFeatures %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  arrange(monDate) %>% 
  mutate(difCast = saldoCast-dplyr::lag(saldoCast, 1)) %>% 
  ungroup() %>% 
  mutate(year=year(monDate)) %>% 
  left_join(bdcSaldo,by=c("year","Sucursal")) %>% 
  mutate(cosechaY = year(fdes)) %>% 
  mutate(key = paste(CTACLIENTE,OPERACION,sep="-")) %>% 
  glimpse()

castNew <- castFeatures %>% 
  dplyr::filter(mesCast>"Dic. 2014") %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  arrange(monDate) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

castRec <- castFeatures %>% 
  dplyr::filter(difCast<0) %>%
  mutate(saldoRec = -difCast) 

####____LISTA DE CASTIGOS___####
####____ADDING RAFAEL AGENCY
rafa <- read_excel('D:/!bso/castigos/join_Castigos_feb2023.xlsx') %>% 
  select(-`Saldo Ene23`,-`Saldo Dic22`,-regional, -NoOp, NOMBRE_AGENCIA = `Agencia Origen Final`) %>% 
  left_join(codAge,by="NOMBRE_AGENCIA") %>% 
  select(-Regional,-Sucursal,AGENCIA_ORI=NOMBRE_AGENCIA, COD_AGENCIA_ORI=AGENCIA) %>% 
  glimpse()

bdcCastOri <- castFeatures %>% 
  left_join(rafa,by=c("CTACLIENTE","OPERACION")) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  arrange(monDate) %>% 
  mutate(SaldoCastNew = ifelse(row_number()==1,saldoCast,0)) %>% 
  mutate(SaldoCastNew = ifelse(mesCast=="Dic. 2014",0,SaldoCastNew)) %>% 
  ungroup() %>% 
  mutate(SaldoCastRec = ifelse(difCast<0,difCast,0)) %>% 
  mutate(CastRec = ifelse(difCast<0,1,0)) %>% 
  mutate(CastNew = ifelse(SaldoCastNew!=0,1,0)) %>% 
  select(Sucursal, ASESOR, NOMBRE_ASESOR, AGENCIA, NOMBRE_AGENCIA, monDate, 
         year, saldoCast, difCast, SaldoCastNew, SaldoCastRec, CastRec, CastNew,
         AGENCIA_ORI,COD_AGENCIA_ORI)

bdcCastOri %>% 
  group_by(year) %>% 
  summarise(New=sum(SaldoCastNew,na.rm=T),
            Rec=sum(SaldoCastRec,na.rm = T),Raw=New-Rec)

bdcLast <- readRDS('D:/!bso/girCartera/rds/ec_May2023.rds') %>% 
  # left_join(codAge,by="AGENCIA") %>% 
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
  left_join(bdcLast, by=c("monDate","Sucursal","AGENCIA","NOMBRE_AGENCIA"))
write.xlsx(bdcAgencia,'D:/!bso/castigos/castigos_May2023.xlsx')
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
  left_join(bdcLast, by=c("monDate","Sucursal","AGENCIA","NOMBRE_AGENCIA"))

bdcLast <- readRDS('D:/!bso/girCartera/rds/ec_May2023.rds') %>% 
  # left_join(codAge,by="AGENCIA") %>% 
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
  left_join(bdcLast, by=c("monDate","Sucursal","ASESOR","NOMBRE_ASESOR"))

tablas  <- list(DatosAgencia = bdcAgencia, DatosAsesor = bdcAsesor, )

write.xlsx(tablas, 'D:/!bso/castigos/castigos_May2023.xlsx')
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
####____HISTORIC PLOTS____####
####____**Castigado Histórico___####
gph <- castFeatures %>% 
  dplyr::filter(saldoCast>0) %>% 
  select(key, monDate, year, saldoCast, cosechaY) %>% 
  group_by(monDate,year) %>% 
  summarise(sum = sum(saldoCast, na.rm = T),
            count = n_distinct(key),
            n=n()) %>% 
  ungroup() %>% 
  dplyr::filter(month(monDate)==12 | (year==2023 & month(monDate)==6))

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
ggsave('D:/!bso/castigos/img/saldoCastHist.png',width = 9,height = 6,units = "in")

####____**Castigado Nuevo____####
gph <- castNew %>% 
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
                 y = saldoCast/1000), position = position_stack(vjust = 0.5),
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
ggsave('D:/!bso/castigos/img/saldoCast.png',width = 9,height = 6,units = "in")
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
                     sec.axis = sec_axis(~./scaleFac, name="Saldo castigado promedio (USD)",
                                         label = comma)) + 
  theme_minimal() +
  theme(axis.text.y.left = element_text(color=paleta(12)[3]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[3]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/img/saldoCastProm.png',width = 9,height = 6,units = "in")
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
  scale_x_continuous( breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma,
                     sec.axis = sec_axis(~./scaleFac, name="Porcentaje de cartera bruta",
                                         label = comma)) + 
  theme_minimal()+
  theme(axis.text.y.left = element_text(color=paleta(12)[3]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[3]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/img/saldoCast_Rel.png',width = 9,height = 6,units = "in")

####____Castigado en bruto = New-Rec____####
gph <- castRec %>% 
  group_by(year) %>% 
  summarise(SaldoRec=sum(saldoRec, na.rm = T),
            SaldoBSOTot = max(SaldoBSOTot, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(select(gph, year, SaldoNew = saldoCast),by="year") %>% 
  mutate(SaldoCastRaw = ifelse(!is.na(SaldoNew),SaldoNew - SaldoRec,-SaldoRec)) %>% 
  mutate(SaldoRaw_Rel = SaldoCastRaw/SaldoBSOTot*100)

scaleFac <- max(gph$SaldoCastRaw/1e3)/max(gph$SaldoRaw_Rel)
ggplot(gph, aes(x = year, y = SaldoCastRaw/1e3)) + 
  geom_bar(stat = 'identity', fill = paleta(12)[3])  +
  geom_line(aes(y = SaldoRaw_Rel*scaleFac), size = 2, color = paleta(12)[7]) + 
  geom_label(aes(label = paste0(comma(round(SaldoCastRaw/1000)),'$'),
                 y = SaldoCastRaw/1000), position = position_stack(vjust = 0.25),
             color = paleta(12)[3], fontface = 'bold')  +
  geom_label(aes(label = paste0(round(SaldoRaw_Rel,2),'%'),
                 y = SaldoRaw_Rel*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  labs(x="",y="Saldo castigado (M de USD, en barras)", title="Saldo castigado Nuevo - Reducido")+
  scale_x_continuous( breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma,
                     sec.axis = sec_axis(~./scaleFac, name="Porcentaje de cartera bruta",
                                         label = comma)) + 
  theme_minimal()+
  theme(text = element_text(face="bold"),
        axis.text.y.left = element_text(color=paleta(12)[3]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[3]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/img/saldoCastRaw_Rel.png',width = 9,height = 6,units = "in")

####____**Castigado Reducido____####
gph <- castRec %>% 
  select(key, year, saldoRec, SaldoBSOTot) %>% 
  mutate(saldoCancel_Rel=saldoRec/SaldoBSOTot*100) %>% 
  group_by(year) %>%
  summarise(SaldoCancel = sum(saldoRec,na.rm = T), 
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
ggsave('D:/!bso/castigos/img/saldoCastRec.png',width = 9,height = 6,units = "in")
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
ggsave('D:/!bso/castigos/img/saldoCastRec_Rel.png',width = 9,height = 6,units = "in")

####____COMPOSICION REDUCIDOS____####
codAge <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')

gph <- castRec %>% 
  rename(montous = MONTOUS) %>% 
  group_by(OPERACION,CTACLIENTE) %>%
  # mutate(tipoCred=ifelse(length(which(!is.na(tipoCred)))==0,tipoCred,max(tipoCred,na.rm = T))) %>% 
  ungroup() %>% 
  mutate(year = year(monDate)) %>% 
  mutate(rangom = cut(montous,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                      labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>% 
  mutate(rangom = as.character(rangom)) %>% 
  mutate(rangos = cut(saldoRec,breaks=c(-Inf,500,1000,5000,10000,15000,20000,Inf),
                      labels=c('1. <500USD','2. 500-1k','3. 1k-5k','4. 5k-10k','5. 10k-15k','6. 15k-20k','7. >20k'))) %>%
  mutate(rangos = as.character(rangos)) %>% 
  mutate(cosechaS = case_when(cosechaY<=2000 ~"< 2001",
                              cosechaY>2000 & cosechaY<=2010 ~"2001-2010",
                              cosechaY>2010 & cosechaY<=2015 ~"2011-2015",
                              cosechaY>2015 ~ ">2015",)) %>% 
  mutate(ratioCastMonto = ifelse(montous>0,saldoRec/montous,0)) %>% 
  mutate(binCastMonto = case_when(ratioCastMonto<0.20 ~ "Ratio < 0.20",
                                  ratioCastMonto>=0.20 & ratioCastMonto<0.40 ~ "0.20 <= Ratio < 0.40",
                                  ratioCastMonto>=0.40 & ratioCastMonto<0.60 ~ "0.40 <= Ratio < 0.60",
                                  ratioCastMonto>=0.60 ~ "Ratio >= 0.60")) %>% 
  mutate(saldoCast=saldoCast/1000) %>% 
  left_join(codAge,by="AGENCIA") %>% 
  dplyr::filter(year>2015)
#Por periodo de desembolso
plt <- agrupar(gph, vstep=year, vgrupo=cosechaS, vagre=saldoRec,pct=5,last = 3)
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
ggsave('D:/!bso/castigos/img/castRecDesembolso.png',width = 9,height = 6,units = "in")
#Por ratio saldo/monto
plt <- agrupar(gph, vstep=year, vgrupo=binCastMonto, vagre=saldoRec,pct=5,last = 2)
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
gph <- castNew %>% 
  rename(montous = MONTOUS) %>% 
  group_by(OPERACION,CTACLIENTE) %>%
  # mutate(tipoCred=ifelse(length(which(!is.na(tipoCred)))==0,tipoCred,max(tipoCred,na.rm = T))) %>% 
  ungroup() %>% 
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

plt <- agrupar(gph, vstep=year, vgrupo=difyear, vagre=saldoCast,pct=10,last = 2)
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

####____**Por ratio saldoCast/Monto____####
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
