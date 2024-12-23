remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
library(reshape)
library(reshape2)
library(stringr)
library(lubridate)
library(Hmisc)
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(forecast)
library(quantmod)
library(astsa)
library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(timeSeries)
library(tseries)
library(xtable)
library(openxlsx)
library(hrbrthemes)
library(viridis)
library(scales)
library(janitor)
library(RColorBrewer)
library(paletteer)
library(zoo)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

paleta <- colorRampPalette(c("slateblue4","purple4","slateblue3","darkorchid3","red3","tan2","yellow2","white"),bias=1.5)

bdcList <- data.frame(Fecha=(as.Date(character())),
                 nOps=numeric(), 
                 saldoDes=numeric(), 
                 montoDes=numeric(),
                 stringsAsFactors=FALSE) 

mos2 <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
meses <- c(1:12)
years <- c('2016', '2017', '2018', '2019','2020', '2021', '2022')

################################################################################
####Simple####
for(i in 1:length(mos2)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mos2[i],years[k]))
      bdc_hist <- readRDS(paste0('C:/!bso/girCartera/rds/ec_', mos2[i], years[k], '.rds')) %>% 
        select(saldous, montous, OPERACION, rangom, FDESEMBOLSO, cosechaY, cosechaM, 
               AGENCIA, MONEDA, saldoMora, saldoCast, saldoRepMora, monDate) %>%
        mutate(rangom=ifelse(rangom=='15k-20k', '10k-20k', rangom)) %>% 
        mutate(fdes=as.Date(FDESEMBOLSO, format= '%d/%m/%y')) %>% 
        mutate(mesDes = month(fdes)) %>% 
        mutate(yearDes=year(fdes)) %>% 
        mutate(Fecha = dmy(paste0('1-', mesDes, '-', yearDes))) %>% 
        dplyr::filter(mesDes==meses[i] & yearDes==as.numeric(years[k])) %>%
        group_by(Fecha) %>% 
        summarise(nOps = n(), saldoDes=sum(saldous), montoDes=sum(montous)) %>% 
        ungroup()
      bdcList <- bdcList%>% 
        bind_rows(bdc_hist)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

write.xlsx(bdcList, 'C:/!bso/excel/girCosecha1.xlsx', rowNames = F)
write.csv(bdcList, 'C:/!bso/excel/girCosecha.csv')

################################################################################
####Rango####
i<-1
k<-1

for(i in 1:length(mos2)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mos2[i],years[k]))
      bdc_hist <- readRDS(paste0('C:/!bso/girCartera/rds/ec_', mos2[i], years[k], '.rds')) %>% 
        select(saldous, montous, OPERACION, rangom, FDESEMBOLSO, AGENCIA, MONEDA,
               cosechaY, cosechaM, saldoMora, saldoCast, saldoRepMora, labGrupoD,
               sucursal) %>% 
        mutate(rangom=ifelse(rangom=='15k-20k', '10k-20k', rangom)) %>%
        mutate(fdes=as.Date(FDESEMBOLSO, format= '%d/%m/%y')) %>% 
        mutate(mesDes = month(fdes)) %>% 
        mutate(yearDes=year(fdes)) %>%
        mutate(Fecha = dmy(paste0('1-', mesDes, '-', yearDes))) %>% 
        dplyr::filter(mesDes==meses[i] & yearDes==as.numeric(years[k])) %>%
        group_by(Fecha, rangom) %>% 
        summarise(nOps = n(), saldoDes=sum(saldous), montoDes=sum(montous)) %>% 
        ungroup()
      bdcList <- bdcList%>% 
        bind_rows(bdc_hist)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

lastday <- data.frame(dia=seq.Date(as.Date("2016-01-01"),as.Date("2022-12-31"),by="1 day"))
lastday <- data.frame(dia=seq.Date(as.Date("2016-01-01"),as.Date("2022-12-31"),by="1 day")) %>%
  mutate(year=as.yearmon(dia)) %>%
  group_by(year) %>%
  summarise(maxdia=max(dia)) %>% 
  ungroup()

write.xlsx(bdcList2, 'C:/!bso/excel/girCosechaRango.xlsx', rowNames = F)

excelrango<-read.xlsx('C:/!bso/Cosecha/girCosechaRango.xlsx', sheet = 'Sheet 1')
   excelrango<-excelrango %>% 
     mutate(Rango=ifelse(Rango=='15k-20k', '10k-20k', Rango)) %>%
     mutate(year=as.yearmon(as.Date(Fecha, origin='1900-01-01'))) %>% 
     left_join(lastday, by='year') %>% 
     select(-year, -Fecha) %>% 
     relocate(maxdia) %>% 
     mutate(Rango= case_when(Rango=='menor500USD'~'1. Menor a 500USD',
                             Rango=='500-1k'~'2. 500k-1k',
                             Rango=='1k-3k'~'3. 1k-3k',
                             Rango=='3k-5k'~'4. 3k-5k',
                             Rango=='5k-8k'~'5. 5k-8k',
                             Rango=='8k-10k'~'6. 8k-10k',
                             Rango=='10k-20k'~'7. 10k-20k',
                             Rango=='mayor20k'~'8.Mayor a 20k'))
  

  write.xlsx(excelrango, 'C:/!bso/excel/girCosechaRango.xlsx', rowNames = F)
  
  ggplot(excelrango, aes(x=maxdia, y=N_Operaciones, color=Rango))+
    geom_line(size=1.25)+theme_minimal()+
    theme(legend.position = 'bottom')+
    labs(x='Fecha', y='Cantidad de Operaciones')+
    guides(color=guide_legend(nrow=1,byrow = TRUE))+
    scale_color_manual(values=paleta(9)[8:1])+
    geom_vline(xintercept = as.Date(c('2020-03-01', '2020-12-31')), linetype=4)
  
  ggplot(excelrango, aes(x=maxdia, y=Monto_Desembolsado, color=Rango))+
    geom_line(size=1.25)+theme_minimal()+
    theme(legend.position = 'bottom')+
    labs(x='Fecha', y='Monto Desembolsado')+
    guides(color=guide_legend(nrow=1,byrow = TRUE))+
    scale_color_manual(values=paleta(9)[8:1])+
    scale_y_continuous(labels=scales::comma)+
    geom_vline(xintercept = as.Date(c('2020-03-01', '2020-12-31')), linetype=4)
     
################################################################################
####CosechaporMEs2022####

mos2 <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
meses <- c(1:12)
years <- c('2016', '2017', '2018', '2019', '2020', '2021', '2022')


bdcList <- data.frame(Fecha=(as.Date(character())),
                      cosechaY=numeric(),
                      cosechaM=as.yearmon(as.Date(character())),
                      nOps=numeric(), 
                      saldoDes=numeric(), 
                      montoDes=numeric(),
                      PaR0=numeric(),
                      stringsAsFactors=FALSE) 


for(i in 1:length(mos2)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mos2[i],years[k]))
      bdc_hist <- readRDS(paste0('C:/!bso/girCartera/rds/ec_', mos2[i], years[k], '.rds')) %>% 
        select(saldous, montous, OPERACION, rangom, cosechaY, cosechaM, 
               AGENCIA, MONEDA, saldoMora, saldoCast, saldoRepMora) %>%
        mutate(rangom=ifelse(rangom=='15k-20k', '10k-20k', rangom)) %>%
        mutate(Fecha = dmy(paste0('1-', meses[i], '-', years[k]))) %>% 
        group_by(Fecha, cosechaY, cosechaM) %>% 
        summarise(nOps = n(), saldoDes=sum(saldous), montoDes=sum(montous), PaR0=sum(saldoMora)) %>% 
        ungroup()
      bdcList <- bdcList%>% 
        bind_rows(bdc_hist)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

bdcList2<- bdcList %>%
  dplyr::filter(PaR0!=0) %>% 
  mutate(cosechaYBin = cut(cosechaY, breaks=c(2000, 2010, 2012, 2014, 2016, 2018, 2020, 2022), 
                           labels=c('2000-2010', '2010-2012', '2012-2014', '2014-2016', '2016-2018',
                                    '2018-2020','2020-2022'))) %>% 
  select(Fecha, cosechaYBin,  montoDes, PaR0) %>% 
  group_by(Fecha, cosechaYBin) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  mutate(pct=PaR0/sum(PaR0)*100)

# group_by(Fecha) %>% 
#   summarise(PaR0=sum(PaR0))


ggplot(bdcList2, aes(x=Fecha, y=pct, fill=factor(cosechaYBin)))+
         geom_bar(stat='identity')+ theme_minimal()+
         labs(y='%', fill='Cosecha Anual')+ 
         scale_y_continuous(breaks=seq(0,100,10))+
         theme(legend.position = 'bottom')+
  scale_x_date(breaks = "6 months")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_fill_manual(values = paleta(9)[8:1])


################################################################################
####CosechaRangoyMes2022####
abd<-read_excel('C:/!bso/excel/girCosecha1.xlsx')

mos2 <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
meses <- c(1:12)
years <- c('2016', '2017', '2018', '2019', '2020', '2021', '2022')

###############################################################################
#Metodo Ahmed
for(i in 1:length(mos2)) {
  for(k in 1:length(years)) {
    tryCatch({
bdc_hist <- readRDS(paste0('C:/!bso/girCartera/rds/ec_', mos2[i], years[k], '.rds')) %>% 
  select(saldous, montous, OPERACION, cosechaY, cosechaM, opDes,
         AGENCIA, MONEDA, saldoMora, saldoCast, saldoRepMora, monDate) %>%
group_by(monDate, cosechaM) %>%
  summarise_all(sum, na.rm = T) %>%
  ungroup() %>%
  dplyr::filter(cosechaM >= 'ene. 2016') %>%
  group_by(cosechaM) %>%
  mutate(totalMdes = sum(montous),
         totalOdes = sum(opDes))

    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

write.xlsx(bdc_hist, 'C:/!bso/Cosecha/girCosechaCompletaV3.xlsx', rowNames = F)

################################################################################

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)    
sd <- as.POSIXlt(start_date)
12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

bdcList <- data.frame(FechaActual=(as.Date(character())),
                      cosechaY=numeric(),
                      Fecha=(as.Date(character())),
                      nOps=numeric(), 
                      saldoActual=numeric(), 
                      montoDes=numeric(),
                      PaR0=numeric(),
                      PosMes=numeric(),
                      ops=numeric(),
                      stringsAsFactors=FALSE) 


for(i in 1:length(mos2)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mos2[i],years[k]))
      bdc_hist <- readRDS(paste0('C:/!bso/girCartera/rds/ec_', mos2[i], years[k], '.rds')) %>% 
        select(saldous, montous, OPERACION, rangom, cosechaY, cosechaM, 
               AGENCIA, MONEDA, saldoMora, saldoCast, saldoRepMora) %>%
        mutate(rangom=ifelse(rangom=='15k-20k', '10k-20k', rangom)) %>%
        mutate(FechaActual = dmy(paste0('1-', meses[i], '-', years[k]))) %>% 
        mutate(cosechaM = as.Date(cosechaM)) %>% 
        dplyr::filter(year(cosechaM)>= 2016) %>% 
        dplyr::rename(Fecha=cosechaM) %>% 
        left_join(abd, by='Fecha') %>% 
        select(-saldoDes) %>% 
        group_by(FechaActual, cosechaY, Fecha) %>% 
        summarise(ops = n(), saldoActual=sum(saldous), PaR0=sum(saldoMora), nops=max(nOps), montoDes=max(montoDes)) %>%
        ungroup() %>% 
        mutate(PosMes = elapsed_months(FechaActual, Fecha)) %>% 
        relocate(FechaActual, cosechaY, Fecha, montoDes, nops) 
      bdcList <- bdcList%>% 
        bind_rows(bdc_hist)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

lastday <- data.frame(dia=seq.Date(as.Date("2016-01-01"),as.Date("2022-12-31"),by="1 day")) %>% 
  mutate(year=as.yearmon(dia)) %>%
  group_by(year) %>%
  summarise(maxdia=max(dia)) %>% 
  ungroup()

bdcList2<-bdcList %>% 
  mutate(year=as.yearmon(FechaActual)) %>% 
  left_join(lastday, by='year') %>% 
  select(-nOps) %>% 
  mutate(mesDes=month(Fecha))

bdcList4 <- bdcList %>% 
  group_by(cosechaM) %>% 
  mutate(montoDes=max(montoDes)) %>% 
  ungroup() %>% 
  #mutate(PaR0=sum(PaR0)) %>% 
  mutate(moraDes=ifelse(montoDes!=0, PaR0/montoDes*100, 0)) %>% 
  arrange(cosechaM, Fecha)

gph<-bdcList4 %>% 
  #dplyr::filter(year(cosechaM)!=2019) %>% 
  mutate(mes=month(cosechaM)) %>% 
  mutate(cosechaM=as.yearmon(cosechaM))

ggplot(gph[gph$cosechaY!=2019,], aes(x=PosMes, y=moraDes, color=as.factor(mes)))+
  geom_line(size=1.15)+ facet_wrap(cosechaY~.)+ theme_minimal()+ 
  guides(color=guide_legend(nrow=1,byrow = TRUE))+
  scale_color_manual(values = paleta(13)[12:1])+
  labs(y='%', x='Mes Tras Desembolso', color='Mes')+
  theme(legend.position = 'bottom')
  

write.xlsx(bdcList2, 'C:/!bso/Cosecha/girCosechaCompletaV2.xlsx', rowNames = F)

################################################################################
####COMITE####

####SUCURSAL####
mos2 <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
meses <- c(1:12)
years <- c('2016', '2017', '2018', '2019', '2020', '2021', '2022')
#years <-c('2021')

bdcList <- data.frame(Fecha=(as.Date(character())),
                      cosechaY=numeric(),
                      cosechaS=character(),
                      cosechaQ=as.yearqtr(as.Date(character())),
                      cosechaM=as.yearmon(as.Date(character())),
                      sucursal=character(),
                      nOps=numeric(), 
                      saldoDes=numeric(), 
                      montoDes=numeric(),
                      PaR0=numeric(),
                      stringsAsFactors=FALSE) 

for(i in 1:length(mos2)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mos2[i],years[k]))
      bdc_hist <- readRDS(paste0('C:/!bso/girCartera/rds/ec_', mos2[i], years[k], '.rds')) %>% 
        select(saldous, FDESEMBOLSO, montous, OPERACION, cosechaY, cosechaM, 
               AGENCIA, MONEDA, saldoMora, saldoCast, saldoRepMora, labGrupoD,
               sucursal) %>%
        mutate(fdes=as.Date(FDESEMBOLSO, format= '%d/%m/%y')) %>% 
        mutate(mesDes = month(fdes)) %>% 
        mutate(yearDes=year(fdes)) %>% 
        mutate(cosechaQ=as.yearqtr(fdes)) %>%
        mutate(cosechaS=case_when(mesDes %in% 1:6~paste0('1-', yearDes),
                                  mesDes %in% 7:12 ~paste0('2-', yearDes))) %>%
        # mutate(rangom = case_when(montous <= 1000 ~'a. Menor a 1k',
        #                           montous > 1000 & montous <= 3000 ~'b. 1k-3k',
        #                           montous > 3000 & montous <= 5000 ~'c. 3k-5k',
        #                           montous > 5000 & montous <= 8000 ~'d. 5k-8k',
        #                           montous > 8000 & montous <= 10000 ~'e. 8k-10k',
        #                           montous > 10000 & montous <= 20000 ~'f. 10k-20k',
        #                           montous > 20000 ~'g. 20k+',)) %>% 
        mutate(Fecha = dmy(paste0('1-', meses[i], '-', years[k]))) %>% 
        group_by(Fecha, cosechaY, cosechaS,  cosechaQ, cosechaM, sucursal) %>% 
        summarise(nOps = n(), saldoDes=sum(saldous), montoDes=sum(montous), PaR0=sum(saldoMora)) %>% 
        ungroup() %>% 
        relocate(Fecha, cosechaY, cosechaS,  cosechaQ, cosechaM, sucursal)
      bdcList <- bdcList%>% 
        bind_rows(bdc_hist)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

write.csv(bdcList, 'C:/!bso/Cosecha/bdcList.csv')
write.csv(bdcList, 'C:/!bso/Cosecha/bdcListRango.csv')

bdcListC<- bdcList %>%
  dplyr::filter(PaR0!=0) %>% 
  dplyr::filter(Fecha>= as.Date('2016-06-01')) %>% 
  mutate(cosechaYBin = cut(cosechaY, breaks=c(2000, 2010, 2012, 2014, 2016, 2018, 2020, 2022), 
                           labels=c('2000-2010', '2010-2012', '2012-2014', '2014-2016', '2016-2018',
                                    '2018-2020','2020-2022'))) %>% 
  select(Fecha, cosechaYBin,  montoDes, PaR0, sucursal) %>% 
  dplyr::rename(Sucursal=sucursal) %>% 
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
  group_by(Fecha, cosechaYBin, Sucursal) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  mutate(pct=PaR0/sum(PaR0)*100)

#GraficoGeneral
ggplot(bdcListC, aes(x=Fecha, y=pct, fill=factor(cosechaYBin)))+
  geom_bar(stat='identity')+
  theme_minimal()+
  labs(y='Saldo en Mora (1+ días, USD)', fill='Cosecha Anual')+ 
  scale_y_continuous(labels=scales::comma, breaks=seq(0,100, 10))+
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank())+
  scale_x_date(breaks = "8 months")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_fill_manual(values = paleta(9)[8:1])

#MoraporSucursalrelativo
bdcListSuc<-bdcListC %>% 
  dplyr::filter(Sucursal %in% c('La Paz', 'El Alto', 'Cochabamba', 'Santa Cruz')) 
ggplot(bdcListSuc, aes(x=Fecha, y=PaR0, fill=factor(cosechaYBin)))+
  geom_area(stat='identity')+ 
  facet_wrap(.~Sucursal, nrow=2) +
  theme_minimal()+
  labs(y='Saldo en Mora (1+ dias, USD)', fill='Cosecha Anual')+ 
  scale_y_continuous(labels=scales::comma, breaks=seq(0,4500000, 500000))+
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        strip.text=element_text(size=22),
        axis.text = element_text(size=14),
        axis.title = element_text(size=16))+
  scale_x_date(breaks = "2 years")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_fill_manual(values = paleta(9)[8:1])
6#GraficoconMOraRelativa
ggplot(bdcListC, aes(x=Fecha, y=PaR0, fill=factor(cosechaYBin)))+
  geom_bar(stat='identity')+ 
  facet_wrap(.~Sucursal, nrow=2) +
  theme_minimal()+
  labs(y='%', fill='Cosecha Anual')+ 
  scale_y_continuous(breaks=seq(0,100,10))+
  theme(legend.position = 'bottom')+
  scale_x_date(breaks = "6 months")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_fill_manual(values = paleta(9)[8:1])


####LabGrupoD####
mos2 <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
meses <- c(1:12)
years <- c('2016', '2017', '2018', '2019', '2020', '2021', '2022')

bdcList <- data.frame(Fecha=(as.Date(character())),
                      cosechaY=numeric(),
                      cosechaS=character(),
                      cosechaQ=as.yearqtr(as.Date(character())),
                      cosechaM=as.yearmon(as.Date(character())),
                      labGrupoD=character(),
                      nOps=numeric(), 
                      saldoDes=numeric(), 
                      montoDes=numeric(),
                      PaR0=numeric(),
                      stringsAsFactors=FALSE) 


for(i in 1:length(mos2)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mos2[i],years[k]))
      bdc_hist <- readRDS(paste0('C:/!bso/girCartera/rds/ec_', mos2[i], years[k], '.rds')) %>% 
        select(saldous, FDESEMBOLSO, montous, OPERACION, cosechaY, cosechaM, 
               AGENCIA, MONEDA, saldoMora, saldoCast, saldoRepMora, labGrupoD,
               sucursal) %>%
        mutate(fdes=as.Date(FDESEMBOLSO, format= '%d/%m/%y')) %>% 
        mutate(mesDes = month(fdes)) %>% 
        mutate(yearDes=year(fdes)) %>% 
        mutate(cosechaQ=as.yearqtr(fdes)) %>%
        mutate(cosechaS=case_when(mesDes %in% 1:6~paste0('1-', yearDes),
                                  mesDes %in% 7:12 ~paste0('2-', yearDes))) %>%
        # mutate(rangom = case_when(montous <= 1000 ~'a. Menor a 1k',
        #                           montous > 1000 & montous <= 3000 ~'b. 1k-3k',
        #                           montous > 3000 & montous <= 5000 ~'c. 3k-5k',
        #                           montous > 5000 & montous <= 8000 ~'d. 5k-8k',
        #                           montous > 8000 & montous <= 10000 ~'e. 8k-10k',
        #                           montous > 10000 & montous <= 20000 ~'f. 10k-20k',
        #                           montous > 20000 ~'g. 20k+',)) %>% 
        mutate(Fecha = dmy(paste0('1-', meses[i], '-', years[k]))) %>% 
        group_by(Fecha, cosechaY, cosechaS,  cosechaQ, cosechaM, labGrupoD) %>% 
        summarise(nOps = n(), saldoDes=sum(saldous), montoDes=sum(montous), PaR0=sum(saldoMora)) %>% 
        ungroup() %>% 
        relocate(Fecha, cosechaY, cosechaS,  cosechaQ, cosechaM, labGrupoD)
      bdcList <- bdcList%>% 
        bind_rows(bdc_hist)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

write.csv(bdcList, 'C:/!bso/Cosecha/bdcList.csv')
write.csv(bdcList, 'C:/!bso/Cosecha/bdcListRango.csv')

bdcListC<- bdcList %>%
  dplyr::filter(PaR0!=0) %>% 
  dplyr::filter(Fecha >= as.Date('2016-06-01')) %>% 
  mutate(cosechaYBin = cut(cosechaY, breaks=c(2000, 2010, 2012, 2014, 2016, 2018, 2020, 2022), 
                           labels=c('2000-2010', '2010-2012', '2012-2014', '2014-2016', '2016-2018',
                                    '2018-2020','2020-2022'))) %>% 
  select(Fecha, cosechaYBin,  montoDes, PaR0, labGrupoD) %>% 
  group_by(Fecha, cosechaYBin, labGrupoD) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(Fecha, labGrupoD) %>% 
  mutate(pct=PaR0/sum(PaR0)*100)

# group_by(Fecha) %>% 
#   summarise(PaR0=sum(PaR0))


#GraficoGeneralLab
ggplot(bdcListC, aes(x=Fecha, y=PaR0, fill=factor(cosechaYBin)))+
  geom_bar(stat='identity')+
  facet_wrap(.~labGrupoD, nrow=2)+
  theme_minimal()+
  labs(y='Saldo en Mora (1+ días, USD)', fill='Cosecha Anual')+ 
  scale_y_continuous(labels=scales::comma, breaks=seq(0,18000000, 2000000))+
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank())+
  scale_x_date(breaks = "8 months")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_fill_manual(values = paleta(9)[8:1])


#MoraporlabGrupD
bdcListlab<-bdcListC %>% 
  dplyr::filter(labGrupoD %in% c('H. Comercio', 'E. Ind. y Manu.')) 
ggplot(bdcListlab, aes(x=Fecha, y=pct, fill=factor(cosechaYBin)))+
  geom_bar(stat='identity')+ 
  facet_wrap(.~labGrupoD, nrow=1) +
  theme_minimal()+
  labs(y='Composición porcentual de saldo en mora (1+ días)', fill='Cosecha Anual')+ 
  scale_y_continuous(breaks=seq(0,100,10))+
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank())+
  scale_x_date(breaks = "2 years")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_fill_manual(values = paleta(9)[8:1])

#MoraporSucursalrelativo
bdcListlab<-bdcListC %>% 
  dplyr::filter(labGrupoD %in% c('H. Comercio', 'E. Ind. y Manu.')) 
ggplot(bdcListlab, aes(x=Fecha, y=PaR0, fill=factor(cosechaYBin)))+
  geom_area(stat='identity')+ 
  facet_wrap(.~labGrupoD, nrow=1) +
  theme_minimal()+
  labs(y='Saldo en Mora (1+ dias, USD)', fill='Cosecha Anual')+ 
  scale_y_continuous(labels=scales::comma, breaks=seq(0,10000000, 1000000))+
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(), 
        strip.text  = element_text(size = 14))+
  scale_x_date(breaks = "10 months")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_fill_manual(values = paleta(9)[8:1])

################################################################################
####EFICIENTE####

mos2 <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
meses <- c(1:12)
years <- c('2016', '2017', '2018', '2019', '2020', '2021', '2022')
#years <-c('2021')

bdcList <- data.frame(Fecha=(as.Date(character())),
                      cosechaY=numeric(),
                      cosechaS=character(),
                      cosechaQ=as.yearqtr(as.Date(character())),
                      cosechaM=as.yearmon(as.Date(character())),
                      sucursal=character(),
                      labGrupoD=character(),
                      nOps=numeric(), 
                      saldoDes=numeric(), 
                      montoDes=numeric(),
                      PaR0=numeric(),
                      stringsAsFactors=FALSE) 

for(i in 1:length(mos2)) {
  for(k in 1:length(years)) {
    tryCatch({
      print(paste0(mos2[i],years[k]))
      bdc_hist <- readRDS(paste0('C:/!bso/girCartera/rds/ec_', mos2[i], years[k], '.rds')) %>% 
        select(saldous, FDESEMBOLSO, montous, OPERACION, cosechaY, cosechaM, 
               AGENCIA, MONEDA, saldoMora, saldoCast, saldoRepMora, labGrupoD) %>%
        mutate(sucursal = substr(as.character(AGENCIA),1 ,1)) %>% 
        mutate(sucursal = ifelse(AGENCIA >= 250 & AGENCIA < 300, '10', sucursal)) %>% 
        dplyr::rename(Sucursal=sucursal) %>% 
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
        mutate(fdes=as.Date(FDESEMBOLSO, format= '%d/%m/%y')) %>% 
        mutate(mesDes = month(fdes)) %>% 
        mutate(yearDes=year(fdes)) %>% 
        mutate(cosechaQ=as.yearqtr(fdes)) %>%
        mutate(cosechaS=case_when(mesDes %in% 1:6~paste0('1-', yearDes),
                                  mesDes %in% 7:12 ~paste0('2-', yearDes))) %>%
        mutate(Fecha = dmy(paste0('1-', meses[i], '-', years[k]))) %>% 
        group_by(Fecha, cosechaY, cosechaS,  cosechaQ, cosechaM, Sucursal, labGrupoD) %>% 
        summarise(nOps = n(), saldoDes=sum(saldous), montoDes=sum(montous), PaR0=sum(saldoMora)) %>% 
        ungroup() %>% 
        relocate(Fecha, cosechaY, cosechaS,  cosechaQ, cosechaM, Sucursal, labGrupoD)
      bdcList <- bdcList%>% 
        bind_rows(bdc_hist)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

write_rds(bdcList, 'C:/!bso/Cosecha/cosechas_full.rds')
# global
gph <- bdcList %>% 
  dplyr::filter(PaR0!=0) %>% 
  dplyr::filter(Fecha>= as.Date('2016-06-01')) %>% 
  mutate(cosechaYBin = cut(cosechaY, breaks=c(2000, 2010, 2012, 2014, 2016, 2018, 2020, 2022), 
                           labels=c('2000-2010', '2010-2012', '2012-2014', '2014-2016', '2016-2018',
                                    '2018-2020','2020-2022'))) %>% 
  select(Fecha, cosechaYBin,  montoDes, PaR0) %>% 
  group_by(Fecha, cosechaYBin) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  mutate(pct=PaR0/sum(PaR0)*100)

#GraficoGeneral
ggplot(gph, aes(x=Fecha, y=pct, fill=factor(cosechaYBin)))+
  geom_bar(stat='identity')+
  theme_minimal()+
  labs(y='Saldo en Mora (1+ días, USD)', fill='Cosecha Anual')+ 
  scale_y_continuous(labels=scales::comma, breaks=seq(0,100, 10))+
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank())+
  scale_x_date(breaks = "8 months")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_fill_manual(values = paleta(9)[8:1])

# suxcursal
gph <- bdcList %>% 
  dplyr::filter(PaR0!=0) %>% 
  dplyr::filter(Fecha>= as.Date('2016-06-01')) %>% 
  mutate(cosechaYBin = cut(cosechaY, breaks=c(2000, 2010, 2012, 2014, 2016, 2018, 2020, 2022), 
                           labels=c('2000-2010', '2010-2012', '2012-2014', '2014-2016', '2016-2018',
                                    '2018-2020','2020-2022'))) %>% 
  select(Fecha, cosechaYBin,  montoDes, PaR0, Sucursal) %>% 
  group_by(Fecha, cosechaYBin, Sucursal) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(Fecha, Sucursal) %>% 
  mutate(pct=PaR0/sum(PaR0)*100) %>% 
  dplyr::filter(Sucursal %in% c('El Alto', 'Santa Cruz', 'Cochabamba', 'La Paz'))

ggplot(gph, aes(x=Fecha, y=pct, fill=factor(cosechaYBin)))+
  geom_bar(stat='identity')+ 
  facet_wrap(.~Sucursal, nrow=2) +
  theme_minimal()+
  labs(y='%', fill='Cosecha Anual')+ 
  scale_y_continuous(breaks=seq(0,100,10))+
  theme(legend.position = 'bottom')+
  scale_x_date(breaks = "6 months")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_fill_manual(values = paleta(9)[8:1])

