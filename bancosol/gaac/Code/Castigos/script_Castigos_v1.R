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
library(fastDummies)
library(openxlsx)
library(sqldf)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
paleta <- colorRampPalette(c("slateblue4","purple4","slateblue3","darkorchid3","red3","tan2","yellow2","white"),bias=1.5)
#===============================================================================
# Data in
#bdcCast_full <- readRDS('D:/!bso/girCartera/bdcFullRangos_feb2023.rds')
#===============================================================================
# Data in
bdcList <- list()
file_list <- list.files(path='D:/!bso/girCartera/rdsGAR')
#i <- 1
for (i in 1:length(file_list)) {
  print(i)
  print(file_list[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds_v3/',
                        file_list[i])) %>% 
    select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, ASESOR,
           CALIFICACION, fbase, montous, saldous, previus, saldoCast,
           AGENCIA, CIU, CAEDEC_DEST, tipoCred, sucursal, NOMBRE_ASESOR,
           fdes, OBJETO_CRED, OPERACION_ORI_REF) %>% 
    mutate(CIU = as.character(CIU),
           CAEDEC_DEST = as.character(CAEDEC_DEST), 
           OPERACION_ORI_REF= as.integer(OPERACION_ORI_REF))
  bdcList[[i]] <- bdc
}
#quietly(gc())
bdcFull <- bind_rows(bdcList) %>%
  mutate(mon = substr(fbase,1,3)) %>%
  mutate(year = substr(fbase,4,7)) %>%
  mutate(mes = case_when(mon == 'Ene'~'jan',
                         mon == 'Feb'~'feb',
                         mon == 'Mar'~'mar',
                         mon == 'Abr'~'apr',
                         mon == 'May'~'may',
                         mon == 'Jun'~'jun',
                         mon == 'Jul'~'jul',
                         mon == 'Ago'~'aug',
                         mon == 'Sep'~'sep',
                         mon == 'Oct'~'oct',
                         mon == 'Nov'~'nov',
                         mon == 'Dic'~'dec',)) %>%
  mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>%
  mutate(monDate = as.yearmon(dayDate)) %>%
  select(-dayDate, -mon, -year, -mes) %>%
  arrange(CI, CTACLIENTE, OPERACION, monDate, fdes) 
#quietly(gc())
bdcList <- NULL
nrowFull <- nrow(bdcFull)
nopsFull <- length(unique(bdcFull$OPERACION))
ncliFull <- length(unique(bdcFull$CI))
write_rds(bdcFull, 'D:/!bso/girCartera/bdcFullCastigos_feb2023.rds')
# write.csv(bdcFull, 'D:/!bso/girCartera/bdcFullCastigos_feb2023.csv')
#===============================================================================
# Rafa + branch names
rafa <- read.xlsx('D:/!bso/castigos/join_castigos_Feb2023.xlsx') %>% 
  select(-Saldo.Ene23,-Saldo.Dic22) %>% 
  glimpse()

nameAg <- read.xlsx('D:/!bso/bases/excel/codigos_agencia.xlsx')
#===============================================================================
bdcFull <- readRDS('D:/!bso/girCartera/bdcFullCastigos_feb2023.rds')
bdcCast <- bdcFull %>% 
  ungroup() %>% 
  select(CTACLIENTE, OPERACION, CI, saldous, ESTADO, DIASMORA, fdes,
         AGENCIA, ASESOR, NOMBRE_ASESOR, tipoCred, monDate, CIU, CAEDEC_DEST, saldoCast) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
  glimpse()
write_rds(bdcCast, 'C:/!bso/bdcCastigos_feb2023.rds')
bdcCast <- readRDS( 'D:/!bso/castigos/bdcCastigos_feb2023.rds')

bdcCast_proc <- bdcCast %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  mutate(fueCast_ = ifelse(ESTADO == 'CASTIGADA', 1, 0),
         fueCast = sum(fueCast_)) %>% 
  dplyr::filter(fueCast > 0) %>% 
  group_by(CTACLIENTE, OPERACION, CI) %>% 
  mutate(flagCast = fueCast_ - dplyr::lag(fueCast_),
         flagKeep = dplyr::lead(flagCast),
         difCast  = saldoCast - dplyr::lag(saldoCast),
         dateCast_ = ifelse(flagCast == 1, dayDate, NA),
         dateCast = max(dateCast_, na.rm = T),
         flagLast = ifelse(row_number() == max(row_number()), 1, 0))
write_rds(bdcCast_proc, 'C:/!bso/bdcCastProc_feb2023.rds')
bdcCast_proc <- readRDS('D:/!bso/castigos/bdcCastProc_feb2023.rds') %>% 
  glimpse()

bdcCast_proc <- bdcCast_proc %>% 
  ungroup() %>% 
  mutate(mydes = as.yearmon(fdes)) %>% 
  group_by(CTACLIENTE, OPERACION, CI) %>% 
  arrange(monDate) %>% 
  mutate(flagDes = ifelse(monDate==mydes,1,0),
         flagCast = fueCast_ - dplyr::lag(fueCast_),
         flagKeep = dplyr::lead(flagCast),
         flagLast = ifelse(row_number() == max(row_number()), 1, 0)) %>% 
  ungroup()

bdcCast_PR <- bdcCast_proc %>% 
  left_join(rafa, by = c('CTACLIENTE', 'OPERACION')) %>% 
  left_join(nameAg, by = 'AGENCIA') %>% 
  mutate(year=year(monDate)) %>% 
  left_join(bdcSaldo,by="year") %>% 
  glimpse()
  
write_rds(bdcCast_PR,'D:/!bso/castigos/bdcCast_proc_feb2023.rds')

bdcCast_PR <- readRDS('D:/!bso/castigos/bdcCast_proc_feb2023.rds')
bdcCast_clean <- bdcCast_PR %>% 
  dplyr::filter(flagKeep == 1 |  flagLast == 1 | flagCast == 1 | flagDes == 1) %>% 
  mutate(SaldoCast_Rel = saldoCast/SaldoBSO*100,
         OpsCast_Rel = n_distinct(OPERACION)/OpsBSO*100)

write_rds(bdcCast_clean, 'D:/!bso/castigos/bdcCast_clean_feb2023.rds')
bdcCast_clean <- readRDS('D:/!bso/castigos/bdcCast_clean_feb2023.rds')
# castigos nuevos por fecha
gph <- bdcCast_clean %>% 
  ungroup() %>% 
  mutate(dateCast = ifelse(flagCast == 1, as.Date(dayDate), NA)) %>% 
  dplyr::filter(flagCast == 1) %>%
  select(monDate, saldoCast) %>% 
  mutate(monDate = ifelse(monDate == 'nov. 2022',
                          as.yearmon('dic. 2022'), monDate)) %>% 
  mutate(monDate=as.yearmon(monDate)) %>% 
  group_by(monDate) %>% 
  summarise_all(.funs = list(sum =~ sum(x = ., na.rm = T),
                             count =~ n()))

scaleFac <- max(gph$sum)/max(gph$count)
ggplot(gph, aes(x = monDate, y = sum)) + 
  geom_bar(stat = 'identity', fill=paleta(12)[3])  +
  geom_line(aes(y = count*scaleFac), size = 1.15, color=paleta(12)[7]) + 
  labs(x="")+
  scale_x_continuous(breaks=as.numeric(gph$monDate),labels=format(gph$monDate,"%m/%y"))+
  scale_y_continuous(label=comma, name = "Castigos USD",
    sec.axis = sec_axis(~./scaleFac, name="Ops. Castigadas")) + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


# yearly grouping para castigos nuevos
gph <- bdcCast_clean %>% 
  ungroup() %>% 
  mutate(dateCast = ifelse(flagCast == 1, as.Date(dayDate), NA)) %>% 
  dplyr::filter(flagCast == 1) %>%
  mutate(year = year(dayDate)) %>% 
  select(year, saldoCast,SaldoCast_Rel, OpsCast_Rel) %>% 
  group_by(year) %>% 
  summarise(saldoCast = sum(saldoCast), SaldoCast_Rel = sum(SaldoCast_Rel),
                OpsCast_Rel = sum(OpsCast_Rel),count = n())

scaleFac <- max(gph$saldoCast/1000)/max(gph$count)
ggplot(gph, aes(x = year, y = saldoCast/1000)) + 
  geom_bar(stat = 'identity', position = 'dodge', fill = paleta(12)[2])  +
  geom_line(aes(y = count*scaleFac), size = 2, color = paleta(12)[7]) + 
  geom_label(aes(label = format(count, big.mark = ','),
                 y = count*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  geom_label(aes(label = paste0(comma(round(saldoCast/1000)),'$'),
                 y = saldoCast/1000), 
                 color = paleta(12)[2], fontface = 'bold')  +
  labs(x="",y="Castigos (M de USD, en barras)") +
  scale_x_continuous( breaks = seq(2015, 2022, 1)) +
  scale_y_continuous(label=comma,
                     sec.axis = sec_axis(~./scaleFac, name="Ops. Castigadas",
                                         label = comma)) + 
  theme_minimal() +
  theme(axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[2]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoOpsCast.png',width = 9,height = 6,units = "in")
#castigos relativo a CB por año
scaleFac <- max(gph$saldoCast/1000)/max(gph$SaldoCast_Rel)
ggplot(gph, aes(x = year, y = saldoCast/1000)) + 
  geom_bar(stat = 'identity', fill = paleta(12)[2])  +
  geom_line(aes(y = SaldoCast_Rel*scaleFac), size = 2, color = paleta(12)[7]) + 
  geom_label(aes(label = paste0(comma(round(saldoCast/1000)),'$'),
                 y = saldoCast/1000), position = position_stack(vjust = 0.5),
             color = paleta(12)[2], fontface = 'bold')  +
  geom_label(aes(label = paste0(round(SaldoCast_Rel,2),'%'),
                 y = SaldoCast_Rel*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  labs(x="",y="Castigos (M de USD, en barras)")+
  scale_x_continuous( breaks = seq(2015, 2022, 1)) +
  scale_y_continuous(label=comma,
                     sec.axis = sec_axis(~./scaleFac, name="Porcentaje de cartera bruta",
                                         label = comma)) + 
  theme_minimal()+
  theme(axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[2]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoOpsCast.png',width = 9,height = 6,units = "in")
# Evolución del saldo castigado desde 2015
gph <- bdcCast_clean %>% 
  ungroup() %>% 
  mutate(dateCast = ifelse(flagCast == 1, as.Date(dayDate), NA)) %>% 
  dplyr::filter(flagCast == 1) %>%
  mutate(year = year(dayDate)) %>% 
  select(year, saldoCast) %>% 
  group_by(year) %>% 
  summarise_all(.funs = list(sum =~ sum(x = ., na.rm = T),
                             count =~ n())) %>% 
  mutate(sumAcum = cumsum(sum),
         nopsAcum = cumsum(count))

scaleFac <- max(gph$sumAcum/1000)/max(gph$nopsAcum)
ggplot(gph, aes(x = year, y = sumAcum/1000)) + 
  geom_bar(stat = 'identity', position = 'dodge', fill = paleta(12)[2])  +
  geom_line(aes(y = nopsAcum*scaleFac), size = 2, color = paleta(12)[7]) + 
  scale_x_continuous( breaks = seq(2015, 2022, 1)) +
  scale_y_continuous(label=comma, name = "Castigos (M de USD, en barras)",
                     sec.axis = sec_axis(~./scaleFac, name="Ops. Castigadas",
                                         label = comma)
  ) + theme_minimal() + xlab('') +
  geom_label(aes(label = format(nopsAcum, big.mark = ','),
                 y = nopsAcum*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  geom_label(aes(label = paste0(as.character(format(round(sumAcum/1000), big.mark = ',')),'$'),
                 y = sumAcum/1000), 
             color = paleta(12)[2], fontface = 'bold',
             vjust = 2)  +
  theme(axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[2]),
        axis.title.y.right = element_text(color=paleta(12)[7]))

ggsave('D:/!bso/castigos/saldoOpsCum.png',width = 9,height = 6,units = "in")
# Evolución del total castigado
gph <- bdcCast_PR %>% 
  select(monDate, saldoCast) %>% 
  mutate(nopsCast = ifelse(saldoCast > 0 & !is.na(saldoCast), 1, 0)) %>% 
  group_by(monDate) %>% 
  summarise(sum = sum(saldoCast, na.rm = T),
            count = sum(nopsCast, na.rm = T)) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
  mutate(year = year(dayDate)) %>% 
  dplyr::filter(month(dayDate)==12 | (year==2023 & month(dayDate)==2))
  

scaleFac <- max(gph$sum/1000)/max(gph$count)
ggplot(gph, aes(x = year, y = sum/1000)) + 
  geom_bar(stat = 'identity',fill = paleta(12)[2])  +
  geom_line(aes(y = count*scaleFac), size = 2, color = paleta(12)[7]) + 
  geom_label(aes(label = format(count, big.mark = ','),y = count*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  geom_label(aes(label = paste0(comma(round(sum/1000)),'$'),y = sum/1000), 
             color = paleta(12)[2], fontface = 'bold',
             vjust = 2) +
  labs(x="","Castigos (M de USD, en barras)") +
  scale_x_continuous(breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma, 
                     sec.axis = sec_axis(~./scaleFac, name="Ops. Castigadas",
                                         label = comma)) + 
  theme_minimal()+
  theme(axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[2]),
        axis.title.y.right = element_text(color=paleta(12)[7]))

# Recuperación de castigos
gph <- bdcCast_PR %>% 
  ungroup() %>% 
  dplyr::filter(difCast < 0) %>% 
  select(CTACLIENTE, OPERACION, difCast, monDate) %>% 
  group_by(CTACLIENTE, OPERACION, monDate) %>% 
  summarise_all(sum, na.rm = T) %>% 
  mutate(difCast = difCast*-1) %>% 
  ungroup() %>%
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
  mutate(year = year(dayDate)) %>% 
  select(year, difCast) %>% 
  group_by(year) %>% 
  summarise_all(.funs = list(sum =~ sum(x = ., na.rm = T),
                             count =~ n()))
scaleFac <- max(gph$sum/1000)/max(gph$count)
ggplot(gph, aes(x = year, y = sum/1000)) + 
  geom_bar(stat = 'identity', position = 'dodge', fill = paleta(12)[2])  +
  geom_line(aes(y = count*scaleFac), size = 2, color = paleta(12)[7]) + 
  scale_x_continuous(breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma, name = "Castigos (M de USD, en barras)",
                     sec.axis = sec_axis(~./scaleFac, name="Ops. Castigadas",
                                         label = comma)
  ) + theme_minimal() + xlab('') +
  geom_label(aes(label = format(count, big.mark = ','),
                 y = count*scaleFac-4), 
             color = paleta(12)[7], fontface = 'bold', vjust = -0.25) +
  geom_label(aes(label = paste0(as.character(format(round(sum/1000), big.mark = ',')),'$')), 
             color = paleta(12)[2], fontface = 'bold',
             position = position_stack(vjust=0.5))  +
  theme(axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[2]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoOpsRec.png',width = 9,height = 6,units = "in")
#====================================================
# Deliverables
####____GETTING ASESORES NAMES____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2015:2023)
ase <- as.vector(sapply(year, function(x){paste0(mes,x)}))
ase <- ase[-c(which(ase=="Mar2023"):length(ase))]
# ase <- c("Dic2015","Dic2016","Dic2017","Dic2018","Dic2019","Dic2020","Dic2021","Dic2022","Feb2023")
# ase <- c("Mar2015","Mar2016","Mar2017","Mar2018","Mar2019","Mar2020","Mar2021","Mar2022",
#          "Jul2015","Jul2016","Jul2017","Jul2018","Jul2019","Jul2020","Jul2021","Jul2022",
#          "Dic2015","Dic2016","Dic2017","Dic2018","Dic2019","Dic2020","Dic2021","Dic2022","Feb2023")
bdcList <- list()
for (i in 1:length(ase)) {
  print(ase[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',ase[i],'.rds')) %>%
    select(ASESOR, NOMBRE_ASESOR) %>% 
    rename(AsesorDes=ASESOR) %>% 
    distinct_all()
  bdcList[[i]] <- bdc
}
bdcAse <- bind_rows(bdcList) %>% 
  group_by(AsesorDes) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()
write_rds(bdcAse,'D:/!bso/castigos/asesores.rds')
bdcAse <- readRDS('D:/!bso/castigos/asesores.rds')

####____TABLA CON AGENCIAS Y ASESORES____####
tab_stock <- bdcCast_PR %>% 
  ungroup() %>% 
  #dplyr::filter(ESTADO == 'CASTIGADA') %>% 
  select(CTACLIENTE, OPERACION, monDate, mydes, fdes, saldoCast, AGENCIA, 
         ESTADO, ASESOR, AgeOri = Agencia.Origen.Final, NOMBRE_AGENCIA, 
         difCast, flagKeep, flagCast, flagDes) %>% 
  mutate(nopsCast = ifelse(saldoCast > 0 & !is.na(saldoCast), 1, 0)) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  mutate(NOMBRE_AGENCIA = ifelse(!is.na(AgeOri) & !str_detect(AgeOri,"Normal"), AgeOri, NOMBRE_AGENCIA)) %>% 
  mutate(AgenciaDes = ifelse(length(which(flagDes==1))==0,AGENCIA,max(AGENCIA[which(flagDes==1)])),
         AsesorDes = ifelse(length(which(flagDes==1))==0,ASESOR,max(ASESOR[which(flagDes==1)]))) %>% 
  ungroup() %>% 
  mutate(Sucursal = substr(as.character(AgenciaDes), 1 ,1)) %>%
  mutate(Sucursal = ifelse(AgenciaDes >= 250 & AgenciaDes < 300, '10', Sucursal)) %>% 
  mutate(Sucursal = case_when(Sucursal == '1' ~ 'Chuquisaca',
                              Sucursal == '10' ~ 'El Alto',
                              Sucursal == '2' ~ 'La Paz',
                              Sucursal == '3' ~ 'Cochabamba',
                              Sucursal == '4' ~ 'Oruro',
                              Sucursal == '5' ~ 'Potosí',
                              Sucursal == '6' ~ 'Tarija',
                              Sucursal == '7' ~ 'Santa Cruz',
                              Sucursal == '8' ~ 'Beni',
                              Sucursal == '9' ~ 'Pando',)) %>%
  ungroup() %>% 
  select(Sucursal, AgenciaDes, AsesorDes, monDate, saldoCast, difCast, nopsCast,
         NOMBRE_AGENCIA, flagDes) %>% #Agencia.Origen.Final,NOMBRE_AGENCIA,
  mutate(SaldoCastRec = ifelse(difCast<0,difCast,0)) %>% 
  mutate(SaldoCastNew = ifelse(difCast>0,difCast,0)) %>% 
  mutate(CastRec = ifelse(difCast<0,1,0)) %>% 
  mutate(CastNew = ifelse(difCast>0,1,0)) %>% 
  group_by(Sucursal, AsesorDes, monDate, NOMBRE_AGENCIA, AgenciaDes) %>% 
  summarise_all(sum, na.rm =T) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
  # arrange(monDate, saldoCast) %>% 
  dplyr::filter(saldoCast > 0) %>%
  # arrange(Sucursal, Agencia.Origen.Final, AsesorDes, dayDate) %>% 
  glimpse()

tab_stock_asesor <- tab_stock %>% 
  left_join(bdcAse,by="AsesorDes") %>% 
  group_by(AsesorDes) %>% 
  arrange(monDate) %>% 
  mutate(cumSaldoNew=cumsum(SaldoCastNew),
         cumSaldoRec=cumsum(SaldoCastRec),
         cumOpsNew= cumsum(CastNew),
         cumOpsRec=cumsum(CastRec)) %>% 
  ungroup() 

tab_stock_asesor %>% 
  group_by(year(dayDate)) %>% 
  summarise(count=sum(CastNew),saldo=sum(SaldoCastNew)) #Check de saldo y ops castigadas por año
write.xlsx(tab_stock_asesor, 'D:/!bso/castigos/Castigos_Feb2023_v5.xlsx')  
write_rds(tab_stock_asesor, 'D:/!bso/castigos/Castigos_v2.rds')
tab_stock_asesor <- readRDS('D:/!bso/castigos/Castigos.rds')
check <- tab_stock %>% 
  ungroup() %>% 
  select(dayDate, saldoCast, nopsCast) %>% 
  group_by(dayDate) %>% 
  summarise_all(sum, na.rm = T) %>% 
  tail() %>% 
  glimpse()
################################################################################
####____EXCEL CONDONACIONES____####
ase <- c("Dic2015","Dic2016","Dic2017","Dic2018","Dic2019","Dic2020","Dic2021","Dic2022","Feb2023")
bdcList <- list()
for (i in 1:length(ase)) {
  print(ase[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rdsGAR/ec_',ase[i],'.rds')) %>%
    group_by(year) %>% 
    summarise(SaldoBSO=sum(saldous),OpsBSO=n_distinct(OPERACION))
  bdcList[[i]] <- bdc
}
bdcSaldo <- bind_rows(bdcList) %>% 
  mutate(year=as.integer(year))

AgeSucReg <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')
cond <- read_excel('D:/!bso/bases/excel/condonacionesFeb23.xlsx') %>% 
  glimpse()

process <- function(x){
  x %>% 
    select(Fecha, Cuenta, Operacion, CondCapInt_USD = `Total Cond cap + Int En $us`,
           CondInt_USD = `Cond Intereses En $us`,
           CondCap_USD = `Cond Capital En $us`,
           Cod_Asesor = Asesor,
           Nombre_Asesor = NombreAsesor,
           Nombre_Agencia= `NOMBRE DE AGENCIA`,
           AGENCIA = Sucursal_operacion) %>%
    left_join(AgeSucReg,by="AGENCIA") %>% 
    mutate(year = year(Fecha)) %>% 
    select(-Fecha, -Regional) %>% 
    rename(Cod_Agencia = AGENCIA) %>% 
    group_by(year, Sucursal, Cod_Agencia, Nombre_Agencia, NOMBRE_AGENCIA, 
             Cod_Asesor, Nombre_Asesor, Cuenta, Operacion) %>% 
    summarise_all(sum) %>% 
    ungroup() %>% 
    mutate(flagInt = ifelse(CondInt_USD>0,1,0)) %>% 
    mutate(flagCap = ifelse(CondCap_USD>0,1,0)) %>% 
    mutate(flagCapInt = ifelse(CondCapInt_USD>0,1,0)) %>% 
    mutate(Key = paste(Cuenta,Operacion,sep="-")) 
}

gph <- process(cond) %>% 
  rename(Int_Condonado = flagInt,
         Cap_Condonado = flagCap,
         IntCap_Condonado = flagCapInt) %>% 
  left_join(bdcSaldo,by="year") %>% 
  mutate(CondCapInt_Rel = CondCapInt_USD/SaldoBSO,
         CondCap_Rel = CondCap_USD/SaldoBSO,
         CondInt_Rel = CondInt_USD/SaldoBSO)

x <- gph %>% #Check of unique keys
  group_by(Key,Cuenta,Operacion) %>% 
  mutate(rep=max(row_number())) %>% 
  ungroup() %>% 
  arrange(Key,rep)
write.xlsx(gph,"D:/!bso/castigos/Condonaciones_v2.xlsx")
write_rds(gph,'D:/!bso/castigos/Condonaciones.rds')

gph <- gph %>%
  dplyr::filter(IntCap_Condonado== 1) %>% 
  group_by(year) %>% 
  summarise(Cond = sum(CondCapInt_USD), Ops = n_distinct(Key)) %>% 
  ungroup() %>% 
  mutate(Condcum = cumsum(Cond), Opscum = cumsum(Ops)) %>% 
  glimpse()
  # left_join(bdcSaldo,by="year") %>% 
  # mutate(Cond_Rel = Cond/Saldo_USD*100)

scaleFac <- max(gph$Cond/1000)/max(gph$Ops)
ggplot(gph, aes(x = year, y = Cond/1000)) + 
  geom_bar(stat = 'identity', position = 'dodge', fill = paleta(12)[2])  +
  geom_line(aes(y = Ops*scaleFac), size = 2, color = paleta(12)[7]) + 
  scale_x_continuous( breaks = seq(2015, 2023, 1)) +
  scale_y_continuous(label=comma, name = "Condonaciones (M de USD, en barras)",
                     sec.axis = sec_axis(~./scaleFac, name="Ops. Condonadas",
                                         label = comma)
  ) + theme_minimal() + xlab('') +
  geom_label(aes(label = format(Ops, big.mark = ','),
                 y = Ops*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  geom_label(aes(label = paste0(as.character(format(round(Cond/1000), big.mark = ',')),'$')), 
             color = paleta(12)[2], fontface = 'bold', position = position_stack(vjust = 0.75))  +
  theme(axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[2]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoOpsCond.png',height = 6,width = 9,units = "in")

#Condonaciones acumulado
scaleFac <- max(gph$Condcum/1000)/max(gph$Opscum)
ggplot(gph, aes(x = year, y = Condcum/1000)) + 
  geom_bar(stat = 'identity', position = 'dodge', fill = paleta(12)[2])  +
  geom_line(aes(y = Opscum*scaleFac), size = 2, color = paleta(12)[7]) + 
  scale_x_continuous( breaks = seq(2015, 2022, 1)) +
  scale_y_continuous(label=comma, name = "Condonaciones (M de USD, en barras)",
                     sec.axis = sec_axis(~./scaleFac, name="Ops. Condonadas",
                                         label = comma)
  ) + theme_minimal() + xlab('') +
  geom_label(aes(label = format(Opscum, big.mark = ','),
                 y = Opscum*scaleFac), 
             color = paleta(12)[7], fontface = 'bold') +
  geom_label(aes(label = paste0(as.character(format(round(Condcum/1000), big.mark = ',')),'$')), 
             color = paleta(12)[2], fontface = 'bold', position = position_stack(vjust = 0.75))  +
  theme(axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[2]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoOpsCondCum.png',width = 9,height = 6,units = "in")

#Condonación relativo
scaleFac <- max(gph$Cond/1000)/max(gph$Cond_Rel)
ggplot(gph, aes(x = year, y = Cond/1000))+ 
  geom_bar(stat = 'identity', position = 'dodge', fill = paleta(12)[2])+
  geom_line(aes(y = Cond_Rel*scaleFac), size = 2, color = paleta(12)[7])+ 
  geom_label(aes(label = paste0(round(Cond_Rel,2),' %'),
                 y = Cond_Rel*scaleFac), 
             color = paleta(12)[7], fontface = 'bold')+
  geom_label(aes(label = paste0(as.character(format(round(Cond/1000), big.mark = ',')),'$')), 
             color = paleta(12)[2], fontface = 'bold', position = position_stack(vjust = 0.5))+
  labs(x="",y="Condonaciones (M de USD, en barras)")+
  scale_x_continuous( breaks = seq(2015, 2023, 1))+
  scale_y_continuous(label=comma,
                     sec.axis = sec_axis(~./scaleFac, name="Porcentaje de Cartera Bruta",
                                         label = comma))+ 
  theme_minimal()+ 
  theme(axis.text.y.left = element_text(color=paleta(12)[2]),
        axis.text.y.right = element_text(color=paleta(12)[7]),
        axis.title.y.left = element_text(color=paleta(12)[2]),
        axis.title.y.right = element_text(color=paleta(12)[7]))
ggsave('D:/!bso/castigos/saldoCondRel.png',height = 6,width = 9,units = "in")

################################################################################
####____CONDONACIONES POR SALDO BSO____####
bdc <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Feb2023.rds') %>%
  left_join(AgeSucReg,by="AGENCIA") %>% 
  group_by(NOMBRE_AGENCIA) %>% 
  summarise(SaldoBSO=sum(saldous),OpsBSO=n_distinct(OPERACION))

gph <- process(cond) %>% 
  rename(Int_Condonado = flagInt,
         Cap_Condonado = flagCap,
         IntCap_Condonado = flagCapInt) %>% 
  left_join(bdc,by="NOMBRE_AGENCIA")

con_rel <- gph %>% 
  group_by(NOMBRE_AGENCIA) %>% 
  summarise(Cond=sum(CondCapInt_USD,na.rm = T),SaldoBSO=max(SaldoBSO,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(cond_rel=ifelse(!is.na(SaldoBSO) & SaldoBSO>0,Cond/SaldoBSO,0))

con_rel <- con_rel %>% 
  dplyr::filter(!str_detect(NOMBRE_AGENCIA,"Normal")) %>% 
  dplyr::filter(!str_detect(NOMBRE_AGENCIA,"Movil")) %>% 
  dplyr::filter(!str_detect(NOMBRE_AGENCIA,"Norte"))

con <- kmeans(con_rel[,-1],3,nstart = 25)
con_rel$cluster <- con$cluster
ggplot(con_rel,aes(x=SaldoBSO,y=Cond,size=cond_rel,color=factor(cluster))) +
  geom_point() +
  theme_minimal()
