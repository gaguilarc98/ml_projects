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
library(arrow)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____LECTURA DE PAGOS TARDIOS Y CONDONACIONES____####
codMod <- read_xlsx("D:/!bso/bases/excel/CodModulo.xlsx")
ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Oct23.rds") %>% 
  select(myPago, Cuenta, Operacion, UltMesPT=appsU)
condFull <- read_parquet('D:/!bso/condonaciones/CondFull.parquet') %>% 
  dplyr::filter(Fecha>=as.Date("2019-01-01"))
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
####____LECTURA DE BASES EN LOOP____####
lastmonth <- "Oct. 2023"
lastmonth12 <- "Nov. 2022"
shortmonth <- str_replace(lastmonth,". ","")
shortmonth12 <- str_replace(lastmonth12,". ","")
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
years <- c("2022","2023")
myrds <- as.vector(sapply(years, function(x){paste0(mes,x)}))
myrds <- myrds[c(which(myrds==shortmonth12):which(myrds==shortmonth))]

Clientes_Ajuste <- readRDS("D:/!bso/features/Clientes_Ene15Oct23.rds") %>% 
  select(CTACLIENTE, OPERACION, fdes_original = fdes)
bdcList <- list()

for (i in 1:length(myrds)) {
  print(myrds[i])
  Cierre <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',myrds[i],'.rds')) %>% 
    left_join(Clientes_Ajuste, by=c("CTACLIENTE","OPERACION")) %>% 
    left_join(codMod, by="MODULO") %>% 
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
    mutate(OpsCastigada = ifelse(saldoCast>0, 1, 0)) %>% 
    mutate(OpsDiferida = ifelse(saldoDif>0, 1, 0)) %>% 
    mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER, "MIGR"), 1 ,0 )) %>% 
    mutate(fdes_original = if_else(esFSL==1, as.Date("2023-05-31"), fdes_original)) %>% 
    mutate(CreditoNuevo = ifelse(year(fdes_original) < 2017, '< 2017',
                               as.character(year(fdes_original)))) %>% 
    mutate(DesembolsoPandemia = case_when(fdes_original<as.Date("2020-03-01")~"1. Pre-pandemia",
                                          fdes_original<as.Date("2022-01-01")~"2. Pandemia",
                                          fdes_original>=as.Date("2022-01-01")~"3. Post-pandemia",
                                          TRUE~NA)) %>% 
    mutate(TipoCredObj = case_when(substr(TIPO_CREDITO,1,1) =='P' & str_detect(DESC_OBJCRED,"INVERSION")  ~ 'Pyme Cap. Inversión',
                                   substr(TIPO_CREDITO,1,1) =='P' & str_detect(DESC_OBJCRED,"OPERACION")  ~ 'Pyme Cap. Operación',
                                   substr(TIPO_CREDITO,1,1) =='M' & str_detect(DESC_OBJCRED,"INVERSION")  ~ 'Micro Cap. Inversión',
                                   substr(TIPO_CREDITO,1,1) =='M' & str_detect(DESC_OBJCRED,"OPERACION")  ~ 'Micro Cap. Operación',
                                   substr(TIPO_CREDITO,1,1) =='N' ~ 'Consumo',
                                   TIPO_CREDITO %in% c('H0','H1','H2') ~ 'Vivienda Normal',
                                   TIPO_CREDITO %in% c('H3','H4') ~ 'Vivienda Social',)) %>% 
    mutate(Tipo_Cartera = case_when(ctaCont %in% c('131','133','134') & OPERACION_ORI_REF==0 ~ 'Normal',
                                    ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                    OPERACION_ORI_REF!=0 ~ 'Refinanciada',)) %>%
    mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
    mutate(PlazoAnios = ifelse(floor(PLAZODIAS/365)<=7,as.character(floor(PLAZODIAS/365)),
                               "> 7")) %>% 
    left_join(ptFull, by=c("monDate"="myPago","CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
    left_join(cond_clean, by=c("monDate"="myCond","CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
    replace_na(list(UltMesPT=0, UltMesCond=0)) %>% 
    # dplyr::filter(UltMesPT==1 | UltMesCond==1) %>% 
    select(Fecha, CTACLIENTE, OPERACION, GENERO, NOMBRE_MODULO, Sucursal, rangos, rangom,  
           TipoCredObj, Sector_Destino, Sector_Actividad, ctaCont, CreditoNuevo, 
           DesembolsoPandemia, PlazoAnios, Tipo_Cartera, SaldoBruto, SaldoMora, SaldoVigente,
           SaldoReprog, SaldoDiferido=saldoDif, Par0=par0, PrevisionEspecifica=previus, 
           InteresAnual=intus, OpsBruta, OpsMora, OpsVigente, OpsReprog, OpsDiferida,
           OpsPar0, UltMesPT, UltMesCond)
  
  bdcList[[i]] <- Cierre
}

bdcEvol <- rbindlist(bdcList)
saveRDS(bdcEvol, paste0("D:/!bso/condonaciones/PTCondEvol_",shortmonth12,shortmonth,".rds"))

####____FIGURAS____####
bdcEvol <- readRDS(paste0('D:/!bso/condonaciones/PTCondEvol_',shortmonth12,shortmonth,'.rds'))
bdcProc <- bdcEvol %>% 
  mutate(monDate=as.yearmon(Fecha)) %>% 
  mutate(SaldoTardio = SaldoBruto*UltMesPT,
         OpsTardia = OpsBruta*UltMesPT) %>% 
  mutate(SaldoCond = SaldoBruto*UltMesCond,
         OpsCond = OpsBruta*UltMesCond) %>%
  mutate(SaldoTardCond = SaldoBruto*UltMesPT*UltMesCond,
         OpsTardCond = OpsBruta*UltMesPT*UltMesCond) %>%
  mutate(moraPotPT = ifelse(Par0>0, Par0, SaldoTardio)) %>% 
  mutate(moraPotCond = ifelse(Par0>0, Par0, SaldoCond)) %>% 
  mutate(moraPotPTCond = ifelse(Par0>0, Par0, SaldoTardCond)) %>% 
  select(monDate, SaldoBruto, SaldoMora, Par0, SaldoTardio, SaldoCond, SaldoTardCond,
         OpsBruta, OpsMora, OpsPar0, OpsTardia, OpsCond, OpsTardCond, starts_with("moraPot")) %>% 
  group_by(monDate) %>% 
  summarise_all(sum) %>% 
  ungroup()

#EVOLUTIVO DE SALDO Y OPERACIONES
plotEvol <- function(x, Saldo, Ops, minV=10, maxV=60, titulo){
  base <- x %>% 
    select(monDate, {{Saldo}},{{Ops}}) %>% 
    mutate(SS={{Saldo}}/1e6,
           OO={{Ops}})
  scale_fac <- max(base$SS)/max(base$OO)
  line <- ggplot(base, aes(x=monDate, y=SS))+
    geom_line(color=paleta(12)[3], linewidth=1.5, group=1)+
    geom_line(aes(y=OO*scale_fac), color=paleta(12)[7], linewidth=1.5)+
    geom_label(aes(label=comma(SS),y=SS+2), color=paleta(12)[3], size=3.5, fontface="bold")+
    geom_label(aes(label=comma(OO),y=OO*scale_fac-2), color=paleta(12)[7], size=3.5, fontface="bold")+
    labs(x="Mes",y="Saldo (MM USD)",
         title = paste0("Evolución de ",titulo," (",lastmonth12,"-",lastmonth,")"))+
    scale_y_continuous(breaks = seq(minV,maxV,5),
                       sec.axis = sec_axis(~./scale_fac,name="Nro Operaciones", 
                                           breaks = round(seq(minV,maxV,5)/scale_fac,-2)))+
    scale_x_continuous(breaks=as.numeric(base$monDate), labels=format(base$monDate,"%m/%y"))+
    theme_minimal()+
    theme(text = element_text(size=13),
          axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 0.5),
          axis.text.y.left = element_text(color=paleta(12)[3]),
          axis.text.y.right = element_text(color=paleta(12)[7]),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(size=12,face="bold", hjust = 0.5))
  return(line)
}

ptplot <- plotEvol(bdcProc, Saldo = SaldoTardio, Ops = OpsTardia, titulo="Ops. Tardías")
ggsave("D:/!bso/condonaciones/img/PTEvol.png", plot = ptplot, 
       width = 7, height=4, units = "in")
condplot <- plotEvol(bdcProc, Saldo = SaldoCond, Ops = OpsCond, minV = 10, maxV=80, titulo="Ops. Condonadas")
ggsave("D:/!bso/condonaciones/img/CondEvol.png", plot = condplot, 
       width = 7, height=4, units = "in")
ptcplot <- plotEvol(bdcProc, Saldo = SaldoTardCond, Ops = OpsTardCond, minV = 10, maxV=80, titulo="Ops. Tardías y Condonadas")
ggsave("D:/!bso/condonaciones/img/PTCEvol.png", plot = ptcplot, 
       width = 7, height=4, units = "in")

ggsave("lineEvol.png",plot=line,width = 6,height=4,units = "in" )
#EVOLUTIVO DE IM E IM AJUSTADO
bdcProc <- bdcEvol %>% 
  select(myPago,saldous,par0,opTot,saldoTot,par0Tot) %>% 
  group_by(myPago) %>% 
  summarise(saldous=sum(saldous),par0Tot=max(par0Tot),opTot=sum(opTot),
            saldoTot=max(saldoTot)) %>% 
  mutate(par0_saldotardio= (par0Tot+saldous)/saldoTot*100) %>% 
  mutate(par0_normal= (par0Tot)/saldoTot*100) %>% 
  ungroup()

plotMora <- function(x, Saldo, Mora, MoraAdj, minV=0.3, maxV=3.5){
  base <- x %>% 
    select(monDate, {{Saldo}}, {{Mora}},{{MoraAdj}}) %>% 
    mutate(S={{Saldo}}, M={{Mora}}, MA={{MoraAdj}}) %>% 
    group_by(monDate) %>% 
    summarise_all(sum, na.rm=T) %>% 
    mutate(im = M/S*100,
           ima = MA/S*100)
 
  mora <- ggplot(base,aes(x=monDate,y=ima))+
    geom_line(aes(color="Mora a día 1 y Saldo tardío"), linewidth=1.5, group=1)+
    geom_line(aes(y=im,color="Mora a día 1"), linewidth=1.5)+
    geom_label(aes(label=round(ima,2)), color=paleta(12)[7], size=3, fontface="bold")+
    geom_label(aes(y=im,label=round(im,2)),color=paleta(12)[3],size=3,fontface="bold")+
    labs(x="Mes",y="Porcentaje de Cartera Bruta (%)",
         title = paste0("Evolución de indicadores de mora (",lastmonth12,"-",lastmonth,")"))+
    scale_y_continuous(breaks=seq(minV,maxV,0.5),labels = scales::comma)+
    scale_x_continuous(breaks=as.numeric(base$monDate), labels=format(base$monDate,"%m/%y"))+
    scale_color_manual(name="Índice",breaks=c("Mora a día 1 y Saldo tardío","Mora a día 1"), values = paleta(12)[c(7,3)])+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 0.5),
          axis.text.y.left = element_text(color=paleta(8)[2]),
          axis.text.y.right = element_text(color=paleta(8)[6]),
          legend.position = "bottom",
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(size=12,face = "bold")) 
  return(mora)
}
plotMora(bdcProc, Saldo = SaldoBruto, Mora = Par0, MoraAdj = moraPotPT)
plotMora(bdcProc, Saldo = SaldoBruto, Mora = Par0, MoraAdj = moraPotCond)
plotMora(bdcProc, Saldo = SaldoBruto, Mora = Par0, MoraAdj = moraPotPTCond)

ggsave("moraEvol.png",plot=mora,width = 6,height=4,units = "in" )

####____LAST MONTH____####
generatePTCk <- function(x,y, mini, mfin){
  xx <- x %>% 
    dplyr::filter(myPago>=as.yearmon(mini) & myPago<=mfin)
  yy <- y %>% 
    dplyr::filter(myCond>=as.yearmon(mini) & myCond<=mfin)
  out <- xx %>%
    full_join(yy, by=c("myPago"="myCond","Cuenta","Operacion")) %>% 
    replace_na(list(esPT=0, esCond=0)) %>% 
    mutate(Ult12MesesPT = ifelse(myPago>=mini & myPago<=mfin & esPT==1, 1, 0)) %>% 
    mutate(Ult12MesesCond = ifelse(myPago>=mini & myPago<=mfin & esCond==1, 1, 0)) %>% 
    mutate(Ult12MesesPTC = ifelse(myPago>=mini & myPago<=mfin & esCond==1 & esPT==1, 1, 0)) %>% 
    mutate(myPT = if_else(Ult12MesesPT==1, myPago, as.yearmon("Dic. 2014"))) %>% 
    mutate(myCond = if_else(Ult12MesesCond==1, myPago, as.yearmon("Dic. 2014"))) %>% 
    mutate(myPTC = if_else(Ult12MesesPTC==1, myPago, as.yearmon("Dic. 2014"))) %>% 
    group_by(Operacion) %>% 
    mutate(Ult12MesesPT = sum(Ult12MesesPT)) %>% 
    ungroup() %>% 
    group_by(Cuenta, Operacion) %>% 
    summarise(CantPT12Meses = max(Ult12MesesPT, na.rm = T),
              CantCond12Meses = sum(Ult12MesesCond,na.rm = T),
              CantPTC12Meses = sum(Ult12MesesPTC,na.rm = T),
              myPT = max(myPT,na.rm = T),
              myCond = max(myCond,na.rm = T),
              myPTC = max(myPTC,na.rm = T)) %>% 
    ungroup()
  return(out)
}

pt_short <- ptFull %>% 
  select(myPago, Cuenta, Operacion) %>% 
  mutate(esPT = 1)
cond_short <- cond_clean %>% 
  select(myCond, Cuenta, Operacion) %>% 
  mutate(esCond = 1)

PTCGroup <- generatePTCk(pt_short,cond_short, lastmonth12, lastmonth)

Cierre <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',shortmonth,'.rds')) %>% 
  left_join(Clientes_Ajuste, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(codMod, by="MODULO") %>% 
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
  mutate(OpsCastigada = ifelse(saldoCast>0, 1, 0)) %>% 
  mutate(OpsDiferida = ifelse(saldoDif>0, 1, 0)) %>% 
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER, "MIGR"), 1 ,0 )) %>% 
  mutate(fdes_original = if_else(esFSL==1, as.Date("2023-05-31"), fdes_original)) %>% 
  mutate(CreditoNuevo = ifelse(year(fdes_original) < 2017, '< 2017',
                               as.character(year(fdes_original)))) %>% 
  mutate(DesembolsoPandemia = case_when(fdes_original<as.Date("2020-03-01")~"1. Pre-pandemia",
                                        fdes_original<as.Date("2022-01-01")~"2. Pandemia",
                                        fdes_original>=as.Date("2022-01-01")~"3. Post-pandemia",
                                        TRUE~NA)) %>% 
  mutate(TipoCredObj = case_when(substr(TIPO_CREDITO,1,1) =='P' & str_detect(DESC_OBJCRED,"INVERSION")  ~ 'Pyme Cap. Inversión',
                                 substr(TIPO_CREDITO,1,1) =='P' & str_detect(DESC_OBJCRED,"OPERACION")  ~ 'Pyme Cap. Operación',
                                 substr(TIPO_CREDITO,1,1) =='M' & str_detect(DESC_OBJCRED,"INVERSION")  ~ 'Micro Cap. Inversión',
                                 substr(TIPO_CREDITO,1,1) =='M' & str_detect(DESC_OBJCRED,"OPERACION")  ~ 'Micro Cap. Operación',
                                 substr(TIPO_CREDITO,1,1) =='N' ~ 'Consumo',
                                 TIPO_CREDITO %in% c('H0','H1','H2') ~ 'Vivienda Normal',
                                 TIPO_CREDITO %in% c('H3','H4') ~ 'Vivienda Social',)) %>% 
  mutate(Tipo_Cartera = case_when(ctaCont %in% c('131','133','134') & OPERACION_ORI_REF==0 ~ 'Normal',
                                  ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                  OPERACION_ORI_REF!=0 ~ 'Refinanciada',)) %>%
  mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
  mutate(PlazoAnios = ifelse(floor(PLAZODIAS/365)<=7,as.character(floor(PLAZODIAS/365)),
                             "> 7")) %>% 
  left_join(PTCGroup, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  replace_na(list(CantPT12Meses=0, CantCond12Meses=0, CantPTC12Meses=0)) %>% 
  mutate(UltMesPT = if_else(!is.na(myPT) & myPT==monDate,1,0)) %>% 
  mutate(UltMesCond = if_else(!is.na(myCond) & myCond==monDate,1,0)) %>% 
  mutate(UltMesPTC = if_else(!is.na(myPTC) & myPTC==monDate,1,0)) %>% 
  mutate(NXN = case_when(CantPT12Meses>=12 & CantCond12Meses>=12 ~ '12x12',
                         CantPT12Meses>=11 & CantCond12Meses>=11 ~ '11x11',
                         CantPT12Meses>=10 & CantCond12Meses>=10 ~ '10x10',
                         CantPT12Meses>=9 & CantCond12Meses>=9 ~ '09x09',
                         CantPT12Meses>=8 & CantCond12Meses>=8 ~ '08x08',
                         CantPT12Meses>=7 & CantCond12Meses>=7 ~ '07x07',
                         CantPT12Meses>=6 & CantCond12Meses>=6 ~ '06x06',
                         CantPT12Meses>=5 & CantCond12Meses>=5 ~ '05x05',
                         CantPT12Meses>=4 & CantCond12Meses>=4 ~ '04x04',
                         CantPT12Meses>=3 & CantCond12Meses>=3 ~ '03x03',
                         CantPT12Meses>=2 & CantCond12Meses>=2 ~ '02x02',
                         CantPT12Meses>=1 & CantCond12Meses>=1 ~ '01x01',)) %>% 
  select(Fecha, CTACLIENTE, OPERACION, GENERO, NOMBRE_MODULO, Sucursal, rangos, rangom,  
         TipoCredObj, Sector_Destino, Sector_Actividad, ctaCont, CreditoNuevo, 
         DesembolsoPandemia, PlazoAnios, Tipo_Cartera, SaldoBruto, SaldoMora, SaldoVigente,
         SaldoReprog, SaldoDiferido=saldoDif, Par0=par0, PrevisionEspecifica=previus, 
         InteresAnual=intus, OpsBruta, OpsMora, OpsVigente, OpsReprog, OpsDiferida,
         OpsPar0, CantPT12Meses, CantCond12Meses, CantPTC12Meses, UltMesPT, UltMesCond, 
         UltMesPTC, NXN)

Cierre %>% 
  group_by(CantPT12Meses) %>% 
  summarise(S=sum(SaldoBruto),
            N=n()) %>% 
  arrange(desc(CantPT12Meses)) %>% 
  mutate(SC=cumsum(S)) %>% 
  mutate(NC=cumsum(N))

sum(Cierre$UltMesPT)
sum(Cierre$UltMesCond)
sum(Cierre$UltMesPTC)

fwrite(Cierre, paste0('D:/!bso/condonaciones/output/CierrePT_',shortmonth,'.txt'),
       row.names = F, sep='|', quote = FALSE)
####____SINGLE PLOT FOR MORA INDEX____####
bdcProc <- bdcEvol %>% 
  mutate(monDate=as.yearmon(Fecha)) %>% 
  mutate(SaldoTardio = SaldoBruto*UltMesPT,
         OpsTardia = OpsBruta*UltMesPT) %>% 
  mutate(SaldoCond = SaldoBruto*UltMesCond,
         OpsCond = OpsBruta*UltMesCond) %>%
  mutate(SaldoTardCond = SaldoBruto*UltMesPT*UltMesCond,
         OpsTardCond = OpsBruta*UltMesPT*UltMesCond) %>%
  mutate(moraPotPT = ifelse(Par0>0, Par0, SaldoTardio)) %>% 
  mutate(moraPotCond = ifelse(Par0>0, Par0, SaldoCond)) %>% 
  mutate(moraPotPTCond = ifelse(Par0>0, Par0, SaldoTardCond)) %>% 
  select(monDate, SaldoBruto, SaldoMora, Par0, SaldoTardio, SaldoCond, SaldoTardCond,
         OpsBruta, OpsMora, OpsPar0, OpsTardia, OpsCond, OpsTardCond, starts_with("moraPot")) %>% 
  group_by(monDate) %>% 
  summarise_all(sum) %>% 
  ungroup()

bdcMora <- bdcProc %>% 
  group_by(monDate) %>% 
  mutate(im = Par0/SaldoBruto*100,
         imPT = moraPotPT/SaldoBruto*100,
         imCond = moraPotCond/SaldoBruto*100,
         imPTCond = moraPotPTCond/SaldoBruto*100) %>% 
  select(monDate, starts_with("im")) %>% 
  pivot_longer(cols = starts_with("im"),names_to = "Indice",values_to = "Valor") %>% 
  mutate(label=case_when(Indice=="im" ~ "IM (PaR0)",
                         Indice=="imPT" ~ "IM (con Pagos Tardíos)",
                         Indice=="imCond" ~ "IM (con Condonaciones)",
                         Indice=="imPTCond" ~ "IM (con Pago Tardío y Cond.)",)) %>% 
  mutate(label =fct_reorder(label,Valor))

mora <- ggplot(bdcMora,aes(x=monDate,y=Valor, color = label))+
  geom_line(linewidth=1.5)+
  geom_label(aes(label=round(Valor,2)), size=4, fontface="bold",show.legend = F)+
  labs(x="Mes",y="Porcentaje de Cartera Bruta (%)", color="Indicador",
       title = paste0("Evolución de indicadores de mora (",lastmonth12,"-",lastmonth,")"))+
  scale_y_continuous(breaks=seq(0.5,5,0.5),labels = scales::comma)+
  scale_x_continuous(breaks=as.numeric(bdcMora$monDate), labels=format(bdcMora$monDate,"%m/%y"))+
  scale_color_manual(values = paleta(12)[c(4:7)])+
  theme_minimal()+
  theme(text = element_text(size=13),
        axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 0.5),
        legend.position = "bottom",
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

ggsave("D:/!bso/condonaciones/img/IMEvol.png", plot = mora, 
       width = 7.5,height=5,units = "in")
