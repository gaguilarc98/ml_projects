####____CARGA DE LIBRERIAS Y FUNCIONES_____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(arrow)
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
library(arrow)
library(ca)
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
####____CHECK DE INTEGRIDAD____####
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Sep2023.rds')
condon <- read_xlsx("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/condonaciones/bases/Condonaciones_Ene2015Sep2023.xlsx", 
                    sheet = "Base") %>% 
  select(sucursal_tran, Modulo, Transaccion, Descripcion, Sucursal, Relacion, Fecha, Sucursal_operacion,
         Moneda, Cuenta, NombreCliente, Operacion, Rubro, capital, interes, interes_cte, interes_penal,
         formularios, gastos_jud, Asesor, NombreAsesor, Saldo, Calificacion, Monto_prevision,
         Instancia, Total_Cond_Cap = `Total Condonado Capital`, 
         Total_Cond_Int_Form_Jud = `Total Cond Intereses + Form + Gastos Jud`,
         Cond_Cap = `Cond Capital En $us`, Cond_Int = `Cond Intereses En $us`,
         Total_Cond_Cap_Int = `Total Cond cap + Int En $us`, REG=REGIONAL) %>% 
  glimpse()
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2019:2023)
my <- as.vector(sapply(year, function(x){paste0(mes,". ",x)}))
i <- 43
condList <- list()
for(i in 1:length(my)){
  condonShort <- condon %>%
    mutate(monDate = as.yearmon(Fecha)) %>% 
    dplyr::filter(monDate==my[i])
  
  condFullshort <- condFull %>% 
    mutate(monDate = as.yearmon(Fecha)) %>% 
    dplyr::filter(monDate==my[i])
  
  print(paste(nrow(condonShort)," | ",nrow(condFullshort)))
  condJoin <- condFullshort %>% 
    left_join(condonShort, by=c("Cuenta", "Operacion", "Fecha","Sucursal","Relacion"), suffix=c("_old","_new")) %>% 
    mutate(check0 = ifelse(is.na(Transaccion_new), 0, 1)) %>% 
    mutate(check1 = Total_Cond_Cap_old==Total_Cond_Cap_new) %>% 
    mutate(check2 = Total_Cond_Cap_Int_old==Total_Cond_Cap_Int_new) %>% 
    mutate(check3 = Total_Cond_Int_Form_Jud_old==Total_Cond_Int_Form_Jud_new) %>% 
    select(monDate_old, starts_with("check")) %>% 
    group_by(monDate_old) %>% 
    summarise_all(sum, na.rm=T) %>% 
    ungroup()
  
  condList[[i]] <- condJoin
}
condBig <- rbindlist(condList)
View(condBig)
####____CONSOLIDADO CONDONACIONES DESDE ENERO 2015____####
condon <- read_xlsx("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/condonaciones/bases/Condonaciones_Ene2015Sep2023.xlsx", 
                    sheet = "Base") %>% 
  select(sucursal_tran, Modulo, Transaccion, Descripcion, Sucursal, Relacion, Fecha, Sucursal_operacion,
         Moneda, Cuenta, NombreCliente, Operacion, Rubro, capital, interes, interes_cte, interes_penal,
         formularios, gastos_jud, Asesor, NombreAsesor, Saldo, Calificacion, Monto_prevision,
         Instancia, Total_Cond_Cap = `Total Condonado Capital`, 
         Total_Cond_Int_Form_Jud = `Total Cond Intereses + Form + Gastos Jud`,
         Cond_Cap = `Cond Capital En $us`, Cond_Int = `Cond Intereses En $us`,
         Total_Cond_Cap_Int = `Total Cond cap + Int En $us`, REG=REGIONAL) %>% 
  glimpse()

write_parquet(condon, "//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/condonaciones/CondFull.parquet",
              compression = "GZIP")
####____ACTUALIZAR UN MES____####
condFull <- read_parquet("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/condonaciones/CondFull.parquet")

condMes <- readxl::read_xlsx('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/condonaciones/bases/Condonaciones202310.xlsx',
                             sheet = "Base Form") %>% 
  select(sucursal_tran, Modulo, Transaccion, Descripcion, Sucursal, Relacion, Fecha, Sucursal_operacion,
         Moneda, Cuenta, NombreCliente, Operacion, Rubro, capital, interes, interes_cte, interes_penal,
         formularios, gastos_jud, Asesor, NombreAsesor, Saldo, Calificacion, Monto_prevision,
         Instancia, Total_Cond_Cap = `Total Condonado Capital`, 
         Total_Cond_Int_Form_Jud = `Total Cond Intereses + Form + Gastos Jud`,
         Cond_Cap = `Cond Capital En $us`, Cond_Int = `Cond Intereses En $us`,
         Total_Cond_Cap_Int = `Total Cond cap + Int En $us`, REG=REGIONAL) %>% 
  glimpse()

tail(condFull %>% group_by(as.yearmon(Fecha)) %>% 
       summarise(n=n()))
tail(condMes %>% group_by(as.yearmon(Fecha)) %>% 
       summarise(n=n()))
condFull <- condFull %>% 
  bind_rows(condMes)
write_parquet(condFull, "//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/condonaciones/CondFull.parquet",
              compression = "GZIP")
####____EVOLUTIVO DE CONDONACIONES____####
#A solicitud de Andrea del Castillo de Mackenzie
condFull <- read_parquet('D:/!bso/condonaciones/CondFull.parquet') %>% 
  dplyr::filter(Fecha>=as.Date("2018-01-01"))
cond_clean <- condFull %>% 
  select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int, CondCap_USD = Cond_Cap) %>%
  mutate(myCond = as.yearmon(Fecha)) %>% 
  group_by(myCond, Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(Fecha),
            across(starts_with("Cond"), ~sum(.x))) %>% 
  ungroup()

cond_clean <- cond_clean %>% 
  select(myCond, Cuenta, Operacion) %>% 
  mutate(UltMesCond=1)

mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
years <- c("2018","2019","2020","2021","2022","2023")
myrds <- as.vector(sapply(years, function(x){paste0(mes,x)}))
bdcList <- list()

for (i in 1:length(myrds)) {
  print(myrds[i])
  Cierre <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds[i],'.rds')) %>% 
    dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
    mutate(SaldoBruto = ifelse(ctaCont %in% c('131','133','134','135','136','137'), saldous, 0)) %>% 
    mutate(SaldoMora = ifelse(ctaCont %in% c('133','134','136','137'), saldous, 0)) %>% 
    mutate(SaldoVigente = ifelse(ctaCont %in% c('131','135'), saldous, 0)) %>% 
    mutate(SaldoReprog = ifelse(ctaCont %in% c('135','136','137'), saldous, 0)) %>% 
    mutate(OpsBruta = ifelse(ctaCont %in% c('131','133','134','135','136','137'), 1, 0)) %>% 
    mutate(OpsMora = ifelse(ctaCont %in% c('133','134','136','137'), 1, 0)) %>% 
    mutate(OpsVigente = ifelse(ctaCont %in% c('131','135'), 1, 0)) %>% 
    mutate(OpsReprog = ifelse(ctaCont %in% c('135','136','137'), 1, 0)) %>% 
    mutate(OpsPar0 = ifelse(par0>0, 1, 0)) %>% 
    left_join(cond_clean, by=c("monDate"="myCond","CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
    replace_na(list(UltMesCond=0)) %>% 
    mutate(Fecha = as.Date(monDate, frac=1)) %>% 
    mutate(SaldoCond = SaldoBruto*UltMesCond,
           OpsCond = OpsBruta*UltMesCond) %>%
    select(Fecha, Sucursal, ctaCont, SaldoBruto, SaldoMora, SaldoVigente,
           SaldoReprog, SaldoPar0=par0, SaldoCond, 
           OpsBruta, OpsMora, OpsVigente, OpsReprog, OpsPar0, OpsCond) %>% 
    group_by(Fecha, ctaCont, Sucursal) %>% 
    summarise_all(sum, na.rm=T) %>% 
    ungroup()
  
  bdcList[[i]] <- Cierre
}

bdcEvol <- rbindlist(bdcList)
write_xlsx(bdcEvol, paste0("D:/!bso/condonaciones/CondEvol_Ene2018Oct2023.xlsx"))
####____CONSOLIDADO CONDONACIONES DESDE ENERO 2019____####
condMes <- readxl::read_xlsx('D:/!bso/condonaciones/condon/Condonaciones202309.xlsx',
                             sheet = "Base Form") %>% 
  select(sucursal_tran, Modulo, Transaccion, Descripcion, Sucursal, Relacion, Fecha, Sucursal_operacion,
         Moneda, Cuenta, NombreCliente, Operacion, Rubro, capital, interes, interes_cte, interes_penal,
         formularios, gastos_jud, Asesor, NombreAsesor, Saldo, Calificacion, Monto_prevision,
         Instancia, Total_Cond_Cap = `Total Condonado Capital`, 
         Total_Cond_Int_Form_Jud = `Total Cond Intereses + Form + Gastos Jud`,
         Cond_Cap = `Cond Capital En $us`, Cond_Int = `Cond Intereses En $us`,
         Total_Cond_Cap_Int = `Total Cond cap + Int En $us`, REG=REGIONAL) %>% 
  glimpse()

cond <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Ago2023.rds') %>% 
  select(sucursal_tran, Modulo, Transaccion, Descripcion, Sucursal, Relacion, Fecha, Sucursal_operacion,
         Moneda, Cuenta, NombreCliente, Operacion, Rubro, capital, interes, interes_cte, interes_penal,
         formularios, gastos_jud, Asesor, NombreAsesor, Saldo, Calificacion, Monto_prevision,
         Instancia, Total_Cond_Cap, Total_Cond_Int_Form_Jud, Cond_Cap, Cond_Int, Total_Cond_Cap_Int, REG) %>% 
  glimpse()
tail(condMes %>% group_by(as.yearmon(Fecha)) %>% 
       summarise(n=n()))
tail(cond %>% group_by(as.yearmon(Fecha)) %>% 
       summarise(n=n()))
condFull <- cond %>% 
  bind_rows(condMes)

saveRDS(condFull,'D:/!bso/condonaciones/CondFull_Ene2019Sep2023.rds')
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Sep2023.rds')
####____CIERRES MENSUALES____####
cierre <- c("Dic2015","Dic2016","Dic2017","Dic2018","Dic2019","Dic2020","Dic2021","Dic2022","May2023")
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
####____PROCESAMIENTO DE CONDONACIONES____####
codAge <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')
cond <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019May2023.rds') %>% 
  glimpse()
sapply(cond, function(x){length(which(is.na(x)))})

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
####____PLOTS____####
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
  labs(x="") +
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
ggsave('D:/!bso/condonaciones/img/saldoOpsCond.png',height = 6,width = 9,units = "in")

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
ggsave('D:/!bso/condonaciones/img/saldoOpsCondCum.png',width = 9,height = 6,units = "in")

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
ggsave('D:/!bso/condonaciones/img/saldoCondRel.png',height = 6,width = 9,units = "in")

####____ANOTHER APPROACH____####
dfTotal <- readRDS("D:/!bso/features/Clientes_Ene15May23_v2.rds") %>% 
  group_by(CTACLIENTE) %>% 
  arrange(desc(fdes)) %>% 
  mutate(GENERO = GENERO[row_number()==1]) %>% 
  ungroup() %>% 
  select(monDate,CTACLIENTE, OPERACION, GENERO, MONTOUS, fdes, fueRefin, fueReprog, peorCalif) %>% 
  glimpse()

condFeatures <- cond %>% 
  mutate(year = year(Fecha)) %>% 
  process(vstep = year) %>% 
  left_join(bdcSaldo,by=c("year","Sucursal")) %>% 
  left_join(dfTotal, by = c("Cuenta"="CTACLIENTE","Operacion"="OPERACION")) %>% 
  mutate(Solucion = case_when(fueRefin==1 & fueReprog==1~'Refinanciada y Reprogramada',
                              fueRefin==1 & fueReprog==0~'Refinanciada',
                              fueRefin==0 & fueReprog==1~'Reprogramda',
                              fueRefin==0 & fueReprog==0~'Normal',
                              TRUE~'Normal')) %>% 
  mutate(cosechaY = year(fdes)) %>% 
  mutate(CondCapInt_Rel = CondCapInt_USD/SaldoBSOTot,
         CondCap_Rel = CondCap_USD/SaldoBSOTot,
         CondInt_Rel = CondInt_USD/SaldoBSOTot) %>% 
  mutate(ratioCondMonto = ifelse(MONTOUS>0,CondCapInt_USD/MONTOUS,0)) %>% 
  mutate(binCondMonto = case_when(ratioCondMonto<0.01 ~ "Ratio < 10%",
                                  ratioCondMonto>=0.01 & ratioCondMonto<0.015 ~ "10% <= Ratio < 15%",
                                  ratioCondMonto>=0.015 & ratioCondMonto<0.03 ~ "15% <= Ratio < 30%",
                                  ratioCondMonto>=0.03 ~ "Ratio >= 0.30")) %>% 
  mutate(CondCapInt_USD=CondCapInt_USD) %>% 
  glimpse() 

write_rds(bdcCond,'D:/!bso/condonaciones/CondFull_wBDCinfo.rds')

####____COMPOSICION DE CONDONACIONES____####
####____**Por cosecha del desembolso____####
condG <- condFeatures %>% 
  mutate(difyear = year-cosechaY) %>% 
  mutate(difyear = paste0("t - ", difyear))
condG <- agrupar(condG, vstep=year, vgrupo=difyear, vagre=CondCapInt_USD,pct=6,last = 1) 

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
ggsave('D:/!bso/condonaciones/img/conDifDesembolso.png',width = 9,height = 6,units = "in")

#####____**Por ratio de cond/monto____####
#El monto desembolsado no es correcto por las reprogramaciones y refinanciamientos.
condG <- agrupar(condFeatures, vstep=year, vgrupo=binCondMonto, vagre=CondCapInt_USD,pct=6,last = 2) 

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
ggsave('D:/!bso/condonaciones/img/conRatio.png',width = 9,height = 6,units = "in")
####____**Por solucion____####
condG <- agrupar(condFeatures, vstep=year, vgrupo=Solucion, vagre=CondCapInt_USD,pct=8,last = 2) 

condG$y <- condG$y %>% 
  mutate(lab = paste0(round(rat),'% (',comma(round(tot/1e3,1)),' M USD)'))  
ggplot(condG$y,aes(x=year,y=rat,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label(aes(label=lab),size=3,color="white",show.legend = F,
             position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=condG$z$year,y=condG$z$rat*1.05,
           label=comma(round(condG$z$tot,0)),size=3.5,color=paleta(12)[7])+
  labs(x="",y="Participación (%)",
       fill="Solución")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2015, 2023, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave('D:/!bso/condonaciones/img/conSolucion.png',width = 9,height = 6,units = "in")
####____**Sucursal____####
condG <- agrupar(condFeatures, vstep=year, vgrupo=Sucursal, vagre=CondCapInt_USD,pct=12,last = 2)
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
ggsave('D:/!bso/condonaciones/img/condSucursal.png', width=9, height=6, units="in")
####____**Condonado Acumulado por Sucursal____####
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
ggsave('D:/!bso/condonaciones/img/condSucursalCum.png', width=9, height=6, units="in")

####____CLUSTERS____####
lastmonth  <- "May. 2023"
shortmonth <- str_replace(lastmonth, ". ","")

bdcLast <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',shortmonth,'.rds')) %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  dplyr::filter(MODULO != 131) %>% 
  dplyr::filter(MODULO != 29) %>% 
  group_by(AGENCIA) %>% 
  summarise(saldoBSO = sum(saldous, na.rm = T)) %>% 
  left_join(codAge, by="AGENCIA") %>% 
  select(-Sucursal)
sum(bdcLast$saldoBSO, na.rm = T)

condLast <- condFeatures %>% 
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
k3$centers #Change Grupo categories based on mean of adjacent centers
clusters <- as.data.frame(k3$cluster) %>% 
  dplyr::rename(Grupo = `k3$cluster`) %>% 
  bind_cols(gph) %>% 
  mutate(Grupo = case_when(Grupo==1~'1.1 % <= Ratio < 10.3%',
                           Grupo==3~'Ratio < 1.1%',
                           Grupo==2~'Ratio >= 10.3%')) %>% 
  mutate(label = paste0(Sucursal, ', ', NOMBRE_AGENCIA, ': ',
                        as.character(round(ratio*100, 1)), '%')) %>% 
  arrange(desc(ratio)) %>% 
  mutate(label = ifelse(row_number() <= 12, label, NA))

ggplot(clusters, aes(x = CondCapInt_USD/1e3, y = saldoBSO/1e6, 
                     color = as.factor(Grupo))) + 
  geom_point(size = 3) + 
  geom_label_repel(aes(label = label, color = factor(Grupo)), size = 2.5, show.legend = F) +
  scale_color_manual(values = paleta(5)) +
  scale_x_continuous(label = comma,
                     name = 'Saldo Condonado (M USD desde 2019)') +
  scale_y_continuous(label = comma, name = paste('Saldo BSO (MM USD - ',shortmonth,')')) +
  theme_minimal() + 
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(title="Grupo"))
ggsave('D:/!bso/condonaciones/img/conScatter_May23.png', width = 9, height = 6, units = "in")

####____LISTAS DE CONDONACIONES POR AGENCIA Y SUCURSAL (ACUMULADAS DESDE 2019)____####
condExp <- cond %>% 
  mutate(year = year(Fecha)) %>% 
  mutate(monDate = as.yearmon(Fecha)) %>% 
  select(year, monDate, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int,
         CondCap_USD = Cond_Cap,
         AGENCIA = Sucursal_operacion) %>%
  left_join(codAge, by="AGENCIA") %>% 
  select(-Regional) %>%
  group_by(year, monDate, Sucursal, AGENCIA, NOMBRE_AGENCIA) %>% 
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
  dplyr::filter(rowLast==1 & monDate != lastmonth) %>% 
  mutate(CondCapInt_USD = 0,
         CondCap_USD = 0,
         CondInt_USD = 0) %>% 
  select(-rowLast) %>% 
  mutate(monDate = as.yearmon(lastmonth))

condExp <- condExp %>% 
  select(-rowLast) %>% 
  bind_rows(condExpAddLast)

bdcLast <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',shortmonth,'.rds')) %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  dplyr::filter(MODULO != 131) %>% 
  dplyr::filter(MODULO != 29) %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(year, monDate, Sucursal, AGENCIA, NOMBRE_AGENCIA) %>% 
  summarise(saldoBSO = sum(saldous, na.rm = T))
sum(bdcLast$saldoBSO, na.rm = T)

condExp <- condExp %>% 
  left_join(bdcLast, by=c("year","monDate","Sucursal","AGENCIA","NOMBRE_AGENCIA")) %>% 
  mutate(monDate = as.Date(monDate, frac=1))

write.xlsx(condExp,paste0('D:/!bso/condonaciones/Condonaciones_',shortmonth,'_v2.xlsx'))
