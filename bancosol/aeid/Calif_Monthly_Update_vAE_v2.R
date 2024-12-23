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

################################################################################
# bdcDic <- readRDS('D:/!bso/girCartera/rds_v3/ec_Dic2022.rds')

# myutf <- c('202208','202209','202210','202211','202212','202301', '202302')
# mybdc <- c("Ago2022","Sep2022","Oct2022","Nov2022","Dic2022","Ene2023", 'Feb2023')
# mybdc <- list.files('D:/!bso/girCartera/rds_v3/')
mybdc <- c('ec_Ene2018.rds', 'ec_Feb2018.rds', 'ec_Mar2018.rds',
               'ec_Abr2018.rds', 'ec_May2018.rds', 'ec_Jun2018.rds',
               'ec_Jul2018.rds', 'ec_Ago2018.rds', 'ec_Sep2018.rds',
               'ec_Oct2018.rds', 'ec_Nov2018.rds', 'ec_Dic2018.rds',
               'ec_Ene2019.rds', 'ec_Feb2019.rds', 'ec_Mar2019.rds',
               'ec_Abr2019.rds', 'ec_May2019.rds', 'ec_Jun2019.rds',
               'ec_Jul2019.rds', 'ec_Ago2019.rds', 'ec_Sep2019.rds',
               'ec_Oct2019.rds', 'ec_Nov2019.rds', 'ec_Dic2019.rds',
               'ec_Ene2020.rds', 'ec_Feb2020.rds', 'ec_Mar2020.rds',
               'ec_Abr2020.rds', 'ec_May2020.rds', 'ec_Jun2020.rds',
               'ec_Jul2020.rds', 'ec_Ago2020.rds', 'ec_Sep2020.rds',
               'ec_Oct2020.rds', 'ec_Nov2020.rds', 'ec_Dic2020.rds',
               'ec_Ene2021.rds', 'ec_Feb2021.rds', 'ec_Mar2021.rds',
               'ec_Abr2021.rds', 'ec_May2021.rds', 'ec_Jun2021.rds',
               'ec_Jul2021.rds', 'ec_Ago2021.rds', 'ec_Sep2021.rds',
               'ec_Oct2021.rds', 'ec_Nov2021.rds', 'ec_Dic2021.rds',
               'ec_Ene2022.rds', 'ec_Feb2022.rds', 'ec_Mar2022.rds',
               'ec_Abr2022.rds', 'ec_May2022.rds', 'ec_Jun2022.rds',
               'ec_Jul2022.rds', 'ec_Ago2022.rds', 'ec_Sep2022.rds',
               'ec_Oct2022.rds', 'ec_Nov2022.rds', 'ec_Dic2022.rds',
               'ec_Ene2023.rds', 'ec_Feb2023.rds', 'ec_Mar2023.rds')
# myutf <- list.files('C:/!bso/Cargamensual_Infocred/rds/')
myutf <- c('201801', '201802', '201803', '201804', '201805', '201806',
           '201807', '201808', '201809', '201810', '201811', '201812',
           '201901', '201902', '201903', '201904', '201905', '201906',
           '201907', '201908', '201909', '201910', '201911', '201912',
           '202001', '202002', '202003', '202004', '202005', '202006',
           '202007', '202008', '202009', '202010', '202011', '202012',
           '202101', '202102', '202103', '202104', '202105', '202106',
           '202107', '202108', '202109', '202110', '202111', '202112',
           '202201', '202202', '202203', '202204', '202205', '202206',
           '202207', '202208', '202209', '202210', '202211', '202212',
           '202301', '202302')
infoList <- list()
bsoList <- list()
i <- 53
for(i in 1:length(myutf)){
  tryCatch({
    print(i)
    bdcBSO_full <- readRDS(paste0('D:/!bso/girCartera/rds_v3/',
                                mybdc[i])) %>% 
      # mutate(fbase = mybdc[i]) %>%
      # mutate(mon = substr(fbase,1,3)) %>%
      # mutate(year = substr(fbase,4,7)) %>%
      # mutate(Fecha = as.yearmon(paste0(mon,'. ',year))) %>%
      mutate(Fecha = monDate) %>% 
      select(-fbase) %>%
      mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
      mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
      mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
      mutate(ctaCont = substr(RUBRO,1,3)) %>% 
      mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                   ctaCont == '134'~saldous,
                                   ctaCont == '136'~saldous,
                                   ctaCont == '137'~saldous,
                                   TRUE ~ 0)) %>% 
      # mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
      select(CTACLIENTE,OPERACION,CI, previus, saldous, saldoMora, DIASMORA, GENERO,ESTADO,MODULO,Fecha) %>%
      mutate(par0 = ifelse(DIASMORA > 0, saldous, 0))
    
    infoRaw <- readRDS(paste0('C:/!bso/Cargamensual_infocred/rds/BSO',myutf[i], '.rds'))
    infoCleanA <- infoRaw %>%
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
             saldo = `SBEF VIGENTE` + `SBEF VENCIDO` + `SBEF EJECUCION`,
             saldoMora = `SBEF VENCIDO` + `SBEF EJECUCION`,
             saldoMMC = `SBEF VENCIDO` + `SBEF EJECUCION` + `SBEF CASTIGADO`) %>%
      select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`,
             `ENTIDAD SBEF`, `FECHA INICIO OPERACION`, `SBEF VIGENTE`, DEPARTAMENTO,
             MontoOriginal, MonedaOrigen, `SBEF CALIFICACION`, `FR CALIFICACION`,
             saldo, saldoMora, saldoMMC, `SBEF CASTIGADO`,`NumeroOp`) %>%
      separate_wider_delim(NumeroOp, names = c("CTACLIENTE","OPERACION"), delim="-",
               too_few = 'align_start', too_many = 'drop') %>% 
      mutate(CTACLIENTE=as.numeric(CTACLIENTE),
             OPERACION=as.numeric(OPERACION)) %>% 
      dplyr::rename(saldoCastInfo=`SBEF CASTIGADO`) %>% 
      mutate(saldoCastInfo = as.numeric(saldoCastInfo)/6.86) %>% #Todas las cantidades están en bolivianos
      mutate(MontoOriginal = as.numeric(MontoOriginal)/6.86) %>% #sin importar el valor de moneda
      mutate(saldoVig = as.numeric(`SBEF VIGENTE`)/6.86) %>%
      mutate(saldo = saldo/6.86) %>%
      mutate(saldoMora = saldoMora/6.86) %>%
      dplyr::rename(saldoMoraInfo=saldoMora) %>% 
      # mutate(Fecha=as.yearmon(paste0(substr(mybdc[i],1,3),'. ',substr(mybdc[i],4,7)))) %>% 
      mutate(Fecha=as.yearmon(ymd(paste0(substr(myutf[i],1,4),'/',substr(myutf[i],5,8), '/01')))) %>% 
      dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% 
      mutate(esBSO=ifelse(`SIGLA SBEF`=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(`SIGLA SBEF`!='BSO',1,0)) %>%
      mutate(`SBEF CALIFICACION` = ifelse(is.na(`SBEF CALIFICACION`),"_",`SBEF CALIFICACION`)) %>% 
      group_by(CI) %>%
      dplyr::filter(max(row_number())>1) %>%
      mutate(califBSO = ifelse(`SIGLA SBEF` == 'BSO', `SBEF CALIFICACION`, '_'),
             califBSO_2 = max(califBSO, na.rm = T)) %>% # Max. calificación en BSO
      mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
             califSF_2 = max(califSF, na.rm = T)) %>%  # Max calificación en SF
      mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
             califSF_2 = max(califSF, na.rm = T)) %>%  # Max calificación en SF
      mutate(saldoNoBSO = sum(saldo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMoraNoBSO = sum(saldoMoraInfo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMMCNoBSO = sum(saldoMMC*noesBSO,na.rm=T)) %>% 
      mutate(entidadSF = ifelse(`SBEF CALIFICACION` == califSF_2, `SIGLA SBEF`, '_'),
             entidadSF_2 = max(entidadSF[which(entidadSF!='BSO')], na.rm = T)) %>%  # Max entidad en SF
      dplyr::filter(califBSO_2==califBSO & `SIGLA SBEF`=='BSO') %>% 
      select(-califBSO,-califSF,-`SBEF CALIFICACION`) %>% 
      ungroup() %>% 
      dplyr::filter(califSF_2!="_")
    
    infoJoin <- infoCleanA %>% 
      dplyr::rename(CI_info=CI) %>% 
      left_join(bdcBSO_full,by=c("CTACLIENTE","OPERACION","Fecha")) 
    infoList[[i]] <- infoJoin
    
    bdcBSO_full <- bdcBSO_full %>% 
      dplyr::filter(MODULO!=131) %>%
      dplyr::filter(ESTADO!="CASTIGADA") %>%
      group_by(Fecha) %>% 
      summarise(SaldoBSO = sum(saldous),ClientesBSO=n_distinct(CI),SaldoMoraBSO=sum(saldoMora),
                par0BSO=sum(par0)) %>% 
      ungroup()
    bsoList[[i]] <- bdcBSO_full
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

infoFull <- bind_rows(infoList)
bsoFull <- bind_rows(bsoList)

write_rds(infoFull,'C:/!bso/califClientes/infoFull_feb23.rds')
write_rds(bsoFull,'C:/!bso/califClientes/bsoFull_feb23.rds')

infoFull <- readRDS('C:/!bso/califClientes/infoFull_feb23.rds')
bsoFull <- readRDS('C:/!bso/califClientes/bsoFull_feb23.rds')

infoFull %>% 
  group_by(Fecha) %>% 
  summarise(nNAs = length(which(is.na(ESTADO)))) %>% 
  tail()

bsoList <- list()
for (i in 1:length(mybdc)) {
  print(i)
  bdcBSO_full <- readRDS(paste0('D:/!bso/girCartera/rds_v3/',
                                mybdc[i])) %>% 
    dplyr::filter(MODULO!=131) %>%
    dplyr::filter(ESTADO!="CASTIGADA") %>%
    # mutate(fbase = mybdc[i]) %>%
    # mutate(mon = substr(fbase,1,3)) %>%
    # mutate(year = substr(fbase,4,7)) %>%
    mutate(Fecha = monDate) %>%
    mutate(ctaCont = substr(RUBRO,1,3)) %>% 
    mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
    mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
    mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                 ctaCont == '134'~saldous,
                                 ctaCont == '136'~saldous,
                                 ctaCont == '137'~saldous,
                                 TRUE ~ 0)) %>% 
    mutate(par0 = ifelse(DIASMORA > 0, saldous, 0)) %>% 
    group_by(Fecha) %>% 
    summarise(SaldoBSO = sum(saldous),ClientesBSO=n_distinct(CI),SaldoMoraBSO=sum(saldoMora),
              par0BSO=sum(par0)) %>% 
    ungroup()
  bsoList[[i]] <- bdcBSO_full
}
bsoFull <- bind_rows(bsoList)


infoFull %>% group_by(Fecha) %>% 
  summarise(saldo=sum(saldous, na.rm = T),nOps=n_distinct(CI),n = n()) %>% 
  tail()

infoPerf_AGG <- infoFull %>% 
  # dplyr::filter(MODULO!=131) %>%
  # dplyr::filter(ESTADO!="CASTIGADA") %>%
  group_by(Fecha,CI,GENERO,entidadSF_2, CTACLIENTE, OPERACION) %>% 
  summarise(saldo=sum(saldous),saldoMora=sum(saldoMora),
            par0=sum(par0),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2),
            saldoNoBSO=max(saldoNoBSO),saldoMoraNoBSO=max(saldoMoraNoBSO),
            saldoMMCNoBSO=max(saldoMMCNoBSO)) %>% 
  ungroup() %>% 
  left_join(bsoFull,by="Fecha") %>%
  mutate(Fecha=as.Date(Fecha)) %>% 
  mutate(Cliente = 1) %>% 
  mutate(PeorCalif=ifelse(califBSO_2<califSF_2,1,0)) %>% 
  mutate(MejorCalif=ifelse(califBSO_2>califSF_2,1,0)) %>% 
  mutate(igualCalif=ifelse(califBSO_2==califSF_2,1,0)) %>% 
  mutate(Clientes_comp=Cliente/ClientesBSO,
         Saldo_comp=saldo/SaldoBSO,
         Saldo_peor_comp=PeorCalif*saldo/SaldoBSO,
         Saldo_peor=PeorCalif*saldo)

infoPerf_SUC <- infoFull %>% 
  # dplyr::filter(MODULO!=131) %>%
  # dplyr::filter(ESTADO!="CASTIGADA") %>%
  group_by(Fecha,CI,GENERO,entidadSF_2, CTACLIENTE, OPERACION, DEPARTAMENTO) %>% 
  summarise(saldo=sum(saldous),saldoMora=sum(saldoMora),
            par0=sum(par0),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2),
            saldoNoBSO=max(saldoNoBSO),saldoMoraNoBSO=max(saldoMoraNoBSO),
            saldoMMCNoBSO=max(saldoMMCNoBSO)) %>% 
  ungroup() %>% 
  left_join(bsoFull,by="Fecha") %>%
  mutate(Fecha=as.Date(Fecha)) %>% 
  mutate(Cliente = 1) %>% 
  mutate(PeorCalif=ifelse(califBSO_2<califSF_2,1,0)) %>% 
  mutate(MejorCalif=ifelse(califBSO_2>califSF_2,1,0)) %>% 
  mutate(igualCalif=ifelse(califBSO_2==califSF_2,1,0)) %>% 
  mutate(Clientes_comp=Cliente/ClientesBSO,
         Saldo_comp=saldo/SaldoBSO,
         Saldo_peor_comp=PeorCalif*saldo/SaldoBSO,
         Saldo_peor=PeorCalif*saldo)

write_rds(infoPerf_AGG,'C:/!bso/califClientes/infoPerf_feb23.rds')
infoPerf_short <- infoPerf_AGG %>% 
  dplyr::filter(Fecha >= '2022-06-01')
write.xlsx(infoPerf_short,'C:/!bso/califClientes/infoPerfeb23_v2.xlsx')

table_1 <- infoPerf_AGG %>% group_by(Fecha) %>% 
  summarise(saldo=sum(saldo, na.rm = T),
            saldoNoBSO = sum(saldoNoBSO, na.rm = T),
            nCli=n_distinct(CI),nOps = n(),
            SaldoBSO = mean(SaldoBSO, na.rm = T),
            ClientesBSO = mean(ClientesBSO, na.rm = T),
            nCli_peor = length(unique(CI[PeorCalif == 1])),
            Saldo_peor = sum(Saldo_peor, na.rm = T)) %>% 
  mutate(pct_Clientes_Compartidos = round(nCli/ClientesBSO*100, 1),
         pct_Saldo_Compartido = round(saldo/SaldoBSO*100, 1),
         pct_Saldo_Compartido_Peor = round(Saldo_peor/saldo*100, 1),
         pct_Clientes_Compartidos_Peor = round(nCli_peor/nCli*100, 1)) %>%
  select(-nOps) %>% 
  dplyr::rename(`Saldo en BSO de compartidos` = saldo,
                `Clientes compartidos` = nCli,
                `Saldo USD Total BSO` = SaldoBSO,
                `Clientes Totales BSO` = ClientesBSO, 
                `Saldo en SF de compartidos` = saldoNoBSO)
tail(table_1, n = 10)

table_1_suc <- infoPerf_SUC %>% group_by(Fecha, DEPARTAMENTO) %>% 
  summarise(saldo=sum(saldo, na.rm = T),
            saldoNoBSO = sum(saldoNoBSO, na.rm = T),
            nCli=n_distinct(CI),nOps = n(),
            SaldoBSO = mean(SaldoBSO, na.rm = T),
            ClientesBSO = mean(ClientesBSO, na.rm = T),
            nCli_peor = length(unique(CI[PeorCalif == 1])),
            Saldo_peor = sum(Saldo_peor, na.rm = T)) %>% 
  mutate(pct_Clientes_Compartidos = round(nCli/ClientesBSO*100, 1),
         pct_Saldo_Compartido = round(saldo/SaldoBSO*100, 1),
         pct_Saldo_Compartido_Peor = round(Saldo_peor/saldo*100, 1),
         pct_Clientes_Compartidos_Peor = round(nCli_peor/nCli*100, 1)) %>%
  select(-nOps) %>% 
  dplyr::rename(`Saldo en BSO de compartidos` = saldo,
                `Clientes compartidos` = nCli,
                `Saldo USD Total BSO` = SaldoBSO,
                `Clientes Totales BSO` = ClientesBSO, 
                `Saldo en SF de compartidos` = saldoNoBSO)
tail(table_1_suc, n = 10)
#==============================================================================
# Tablita para marketing
write.xlsx(table_1_suc, 'C:/!bso/califClientes/Clientes_Compartidos_MKT_Feb2023_xRegional.xlsx')
#==============================================================================
gph <- table_1 %>% 
  pivot_longer(!Fecha) %>% 
  dplyr::filter(str_detect(name, 'pct_')) %>% 
  group_by(name) %>% 
  arrange(name, Fecha)

ggplot(gph, aes(x = Fecha, y = value, color = name)) + 
  geom_line(linewidth = 1.15) +
  scale_y_continuous(limits = c(0, 35),
                     breaks = seq(0, 35, 5)) + 
  theme_minimal() + 
  theme(legend.position = 'bottom')+
  guides(color=guide_legend(nrow=2, byrow=TRUE)) + 
  ylab('%')
################################################################################
####____ENTIDAD COMPARTIDA____#### para todos mes - esta va a la ppt
table_1_ent <- infoPerf_AGG %>% 
  group_by(Fecha, entidadSF_2) %>% 
  summarise(saldo=sum(saldo, na.rm = T),
            nCli=n_distinct(CI),nOps = n(),
            SaldoBSO = mean(SaldoBSO, na.rm = T),
            ClientesBSO = mean(ClientesBSO, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  arrange(Fecha, desc(saldo)) %>% 
  mutate(grupoEnt = ifelse(row_number() < 7, entidadSF_2, 'Otras')) %>% 
  ungroup() %>% 
  group_by(Fecha, grupoEnt) %>% 
  summarise(saldo=sum(saldo, na.rm = T),
            nCli=sum(nCli), nOps = sum(nOps),
            SaldoBSO = mean(SaldoBSO, na.rm = T),
            ClientesBSO = mean(ClientesBSO, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pct_ClientesComp = round(nCli/ClientesBSO*100, 1),
         pct_SaldoComp = round(saldo/SaldoBSO*100, 1)) %>% 
  arrange(Fecha, desc(saldo))
tail(table_1_ent, n = 10)

gph <- table_1_ent %>% 
  pivot_longer(!c(Fecha, grupoEnt)) %>% 
  dplyr::filter(name == 'pct_SaldoComp') %>% 
  group_by(name) %>% 
  arrange(name, Fecha) %>% 
  ungroup() %>% 
  dplyr::filter(grupoEnt != 'Otras') %>% 
  ungroup() %>% 
  group_by(grupoEnt) %>% 
  arrange(grupoEnt, Fecha) %>% 
  mutate(range = max(value) - min(value)) %>% 
  dplyr::filter(range > 0.5) %>% 
  mutate(label = if_else(Fecha == max(Fecha), 
                         paste0(grupoEnt, ', Crec:', range, 'pp'),
                         NA_character_)) 

ggplot(gph, aes(x = Fecha, y = value, color = grupoEnt)) + 
  geom_line(linewidth = 1.15) +
  scale_y_continuous(breaks = seq(0,9,1), 
                     limits = c(0, 9)) + theme_minimal() +
  scale_color_manual(values = paleta(5)) + 
  theme(legend.position = 'none') +
  geom_label_repel(aes(label = label),
                   nudge_x = 25,
                   nudge_y = 1,
                   na.rm = TRUE,
                   show.legend = F,
                   size = 4.5) 
################################################################################
####____ENTIDAD COMPARTIDA____#### para un mes - esta va a la ppt
# myutf <- c('202208','202209','202210','202211','202212','202301')
# mybdc <- c("Ago2022","Sep2022","Oct2022","Nov2022","Dic2022","Ene2023")
myutf <- c('202302')
mybdc <- c("ec_Feb2023.rds")
infoList <- list()
for(i in 1:length(mybdc)){
  tryCatch({
    bdcBSO_full <- readRDS(paste0('D:/!bso/girCartera/rds_v3/',
                   mybdc[i])) %>% 
      # mutate(fbase = mybdc[i]) %>%
      # mutate(mon = substr(fbase,1,3)) %>%
      # mutate(year = substr(fbase,4,7)) %>%
      mutate(Fecha = monDate) %>%
      select(-fbase) %>%
      mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
      mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
      mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
      mutate(ctaCont = substr(RUBRO,1,3)) %>% 
      mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                                   ctaCont == '134'~saldous,
                                   ctaCont == '136'~saldous,
                                   ctaCont == '137'~saldous,
                                   TRUE ~ 0)) %>% 
      # mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
      select(CTACLIENTE,OPERACION,CI, previus, saldous, saldoMora, DIASMORA, GENERO,ESTADO,MODULO,Fecha) %>%
      mutate(par0 = ifelse(DIASMORA > 0, saldous, 0))
    
    infoRaw <- readRDS(paste0('C:/!bso/Cargamensual_infocred/rds/BSO',myutf[i], '.rds'))
    infoCleanA <- infoRaw %>%
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
             saldo = `SBEF VIGENTE` + `SBEF VENCIDO` + `SBEF EJECUCION`,
             saldoMora = `SBEF VENCIDO` + `SBEF EJECUCION`,
             saldoMMC = `SBEF VENCIDO` + `SBEF EJECUCION` + `SBEF CASTIGADO`) %>%
      select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`,
             `ENTIDAD SBEF`, `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
             MontoOriginal, MonedaOrigen, `SBEF CALIFICACION`, `FR CALIFICACION`,
             saldo, saldoMora, saldoMMC, `SBEF CASTIGADO`,`NumeroOp`) %>%
      separate_wider_delim(NumeroOp, names = c("CTACLIENTE","OPERACION"), delim="-",
                           too_few = 'align_start', too_many = 'drop') %>% 
      mutate(CTACLIENTE=as.numeric(CTACLIENTE),
             OPERACION=as.numeric(OPERACION)) %>% 
      rename(saldoCastInfo=`SBEF CASTIGADO`) %>% 
      mutate(saldoCastInfo = as.numeric(saldoCastInfo)/6.86) %>% #Todas las cantidades están en bolivianos
      mutate(MontoOriginal = as.numeric(MontoOriginal)/6.86) %>% #sin importar el valor de moneda
      mutate(saldoVig = as.numeric(`SBEF VIGENTE`)/6.86) %>%
      mutate(saldo = saldo/6.86) %>%
      mutate(saldoMora = saldoMora/6.86) %>%
      rename(saldoMoraInfo=saldoMora) %>% 
      # mutate(Fecha=as.yearmon(paste0(substr(mybdc[i],1,3),'. ',substr(mybdc[i],4,7)))) %>% 
      mutate(Fecha=as.yearmon(ymd(paste0(substr(myutf[i],1,4),'/',substr(myutf[i],5,8), '/01')))) %>% 
      dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% 
      mutate(esBSO=ifelse(`SIGLA SBEF`=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(`SIGLA SBEF`!='BSO',1,0)) %>%
      mutate(`SBEF CALIFICACION` = ifelse(is.na(`SBEF CALIFICACION`),"_",`SBEF CALIFICACION`)) %>% 
      group_by(CI) %>%
      dplyr::filter(max(row_number())>1) %>%
      mutate(califBSO = ifelse(`SIGLA SBEF` == 'BSO', `SBEF CALIFICACION`, '_'),
             califBSO_2 = max(califBSO, na.rm = T)) %>% # Max. calificación en BSO
      mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
             califSF_2 = max(califSF, na.rm = T)) %>%  # Max calificación en SF
      mutate(saldoNoBSO = sum(saldo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMoraNoBSO = sum(saldoMoraInfo*noesBSO,na.rm=T)) %>% 
      mutate(saldoMMCNoBSO = sum(saldoMMC*noesBSO,na.rm=T)) %>% 
      mutate(entidadSF = ifelse(`SBEF CALIFICACION` == califSF_2, `SIGLA SBEF`, '_'),
             entidadSF_2 = max(entidadSF[which(entidadSF!='BSO')], na.rm = T)) %>%  # Max entidad en SF
      dplyr::filter(califBSO_2==califBSO & `SIGLA SBEF`=='BSO') %>% 
      select(-califBSO,-califSF,-entidadSF) %>% 
      ungroup() %>% 
      dplyr::filter(califSF_2!="_")
    
    infoJoin <- infoCleanA %>% 
      rename(CI_info=CI) %>% 
      left_join(bdcBSO_full,by=c("CTACLIENTE","OPERACION","Fecha")) 
    infoList[[i]] <- infoJoin
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

infoFull <- bind_rows(infoList)


infoPerf <- infoJoin %>% 
  # dplyr::filter(MODULO!=131) %>%
  # dplyr::filter(ESTADO!="CASTIGADA") %>%
  group_by(Fecha,CI,GENERO,entidadSF_2) %>% 
  summarise(saldo=sum(saldous),saldoMora=sum(saldoMora),
            par0=sum(par0),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2),
            saldoNoBSO=max(saldoNoBSO),saldoMoraNoBSO=max(saldoMoraNoBSO),
            saldoMMCNoBSO=max(saldoMMCNoBSO)) %>% 
  ungroup() %>% 
  left_join(bdcBSO_full,by="Fecha") %>%
  mutate(Fecha=as.Date(Fecha)) %>% 
  mutate(Cliente = 1.0) %>% 
  mutate(PeorCalif=ifelse(califBSO_2<califSF_2,1,0)) %>% 
  mutate(MejorCalif=ifelse(califBSO_2>califSF_2,1,0)) %>% 
  mutate(igualCalif=ifelse(califBSO_2==califSF_2,1,0)) %>% 
  mutate(Clientes_comp=Cliente/ClientesBSO,
         Saldo_comp=saldo/SaldoBSO,
         Saldo_peor_comp=PeorCalif*saldo/SaldoBSO,
         Saldo_peor=PeorCalif*saldo)

write.xlsx(infoPerf,'D:/!bso/califClientes/infoPerf_ENT_Mar23.xlsx')
################################################################################
####____VAINAS INNECESARIAS____####

bdcBSO_full <- fread('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCarteraDic2022.txt',
                     encoding = 'Latin-1', fill = T) %>% 
  mutate(fbase = 'Dic2022') %>%
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
  mutate(Fecha = as.yearmon(dayDate)) %>%
  select(-fbase) %>%
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  mutate(montous = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(previus = ifelse(MONEDA == 0, as.numeric(PREVCONST)/6.86, as.numeric(PREVCONST))) %>% 
  mutate(ctaCont = substr(RUBRO,1,3)) %>% 
  mutate(saldoMora = case_when(ctaCont == '133'~saldous,
                               ctaCont == '134'~saldous,
                               ctaCont == '136'~saldous,
                               ctaCont == '137'~saldous,
                               TRUE ~ 0)) %>% 
  # mutate(saldous = ifelse(str_detect(ESTADO,'CASTIG'),0 , saldous)) %>% 
  select(CTACLIENTE,OPERACION,CI, previus, saldous, saldoMora, DIASMORA, GENERO,ESTADO,MODULO) %>%
  mutate(par0 = ifelse(DIASMORA > 0, saldous, 0)) %>%
  glimpse()
################################################################################
infoRaw <- fread(paste0('D:/!bso/califClientes/utf/BSO202212_utf8.txt'), encoding = 'UTF-8', fill = T)
infoCheck <- infoRaw %>%
  mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
         saldo = `SBEF VIGENTE` + `SBEF VENCIDO` + `SBEF EJECUCION`,
         saldoMora = `SBEF VENCIDO` + `SBEF EJECUCION`,
         saldoMMC = `SBEF VENCIDO` + `SBEF EJECUCION` + `SBEF CASTIGADO`) %>%
  select(CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora, `SIGLA SBEF`,
         `ENTIDAD SBEF`, `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
         MontoOriginal, MonedaOrigen, `SBEF CALIFICACION`, `FR CALIFICACION`,
         saldo, saldoMora, saldoMMC, `SBEF CASTIGADO`,`NumeroOp`) %>%
  separate(NumeroOp,into = c("CTACLIENTE","OPERACION"),sep="-") %>% 
  mutate(CTACLIENTE=as.numeric(CTACLIENTE),
         OPERACION=as.numeric(OPERACION)) %>% 
  rename(saldoCastInfo=`SBEF CASTIGADO`) %>% 
  mutate(saldoCastInfo = as.numeric(saldoCastInfo)/6.86) %>% #Todas las cantidades están en bolivianos
  mutate(MontoOriginal = as.numeric(MontoOriginal)/6.86) %>% #sin importar el valor de moneda
  mutate(saldoVig = as.numeric(`SBEF VIGENTE`)/6.86) %>%
  mutate(saldo = saldo/6.86) %>%
  mutate(saldoMora = saldoMora/6.86) %>%
  rename(saldoMoraInfo=saldoMora) %>% 
  mutate(fbase= '202212') %>%
  mutate(Mes=as.yearmon(as.Date(paste0(fbase, "01"), "%Y%m%d")))

infoCleanA <- infoCheck %>% 
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A - ')) %>% 
  mutate(esBSO=ifelse(`SIGLA SBEF`=='BSO',1,0)) %>%
  mutate(`SBEF CALIFICACION` = ifelse(is.na(`SBEF CALIFICACION`),"_",`SBEF CALIFICACION`)) %>% 
  group_by(CI) %>%
  dplyr::filter(max(row_number())>1) %>%
  # rename(Calif = `SBEF CALIFICACION`) %>% 
  # mutate(Calif = ifelse(is.na(Calif),'_',Calif)) %>% 
  # mutate(PeorCalifBSOG = max(Calif[esBSO==1])) %>%
  # mutate(PeorCalifSFG = max(`SBEF CALIFICACION`[which(`SIGLA SBEF`=='BSO')],na.rm = T)) %>%
  mutate(califBSO = ifelse(`SIGLA SBEF` == 'BSO', `SBEF CALIFICACION`, '_'),
         califBSO_2 = max(califBSO, na.rm = T)) %>% # Max. calificación en BSO
  # dplyr::filter(califBSO_2 != '_') %>%
  # select(-califBSO) %>%
  mutate(califSF = ifelse(`SIGLA SBEF` != 'BSO', `SBEF CALIFICACION`, '_'),
         califSF_2 = max(califSF, na.rm = T)) %>%  # Max calificación en SF
  dplyr::filter(califBSO_2==califBSO & `SIGLA SBEF`=='BSO') %>% 
  select(-califBSO,-califSF,-`SBEF CALIFICACION`) %>% 
  ungroup() %>% 
  dplyr::filter(califSF_2!="_")

bsoFull <- bdcBSO_full %>% 
  # mutate(CI_2 = gsub("[^0-9.-]", "", CI)) %>%
  # mutate(CI_2 = gsub("[^[:alnum:] ]", "", CI_2)) %>%
  # mutate(CI_2 = as.numeric(CI_2)) %>%
  mutate(CI_2 = str_extract(CI,pattern = "[0-9]+")) %>% 
  mutate(CI_2 = as.numeric(CI_2)) %>% 
  # select(CI,CI_2,GENERO) %>% 
  distinct_all() %>% 
  glimpse()

infoJoin <- infoCleanA %>% 
  select(-CI) %>% 
  left_join(bsoFull,by=c("CTACLIENTE","OPERACION")) 

bdcBSO_tab <- bsoFull %>% 
  dplyr::filter(MODULO!=131) %>%
  dplyr::filter(ESTADO!="CASTIGADA") %>%
  summarise(Saldo=sum(saldous),Clientes=n_distinct(CI))

infoPerfFiltro <- infoJoin %>% 
  dplyr::filter(MODULO!=131) %>%
  dplyr::filter(ESTADO!="CASTIGADA") %>%
  group_by(CI,GENERO) %>% 
  summarise(saldo=sum(saldous),saldoMora=sum(saldoMora),
            par0=sum(par0),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2)) %>% 
  ungroup() %>% 
  mutate(Cliente = 1.0) %>% 
  mutate(PeorCalif=ifelse(califBSO_2<califSF_2,1,0)) %>% 
  mutate(MejorCalif=ifelse(califBSO_2>califSF_2,1,0)) %>% 
  mutate(igualCalif=ifelse(califBSO_2==califSF_2,1,0)) %>% 
  mutate(SaldoBSO=bdcBSO_tab$Saldo,
         ClientesBSO=bdcBSO_tab$Clientes)

infoPerf <- infoJoin %>% 
  group_by(CI,GENERO) %>% 
  summarise(saldo=sum(saldous),saldoMora=sum(saldoMora),
            par0=sum(par0),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2)) %>% 
  ungroup() %>% 
  mutate(Cliente = 1.0) %>% 
  mutate(PeorCalif=ifelse(califBSO_2<califSF_2,1,0)) %>% 
  mutate(MejorCalif=ifelse(califBSO_2>califSF_2,1,0)) %>% 
  mutate(igualCalif=ifelse(califBSO_2==califSF_2,1,0)) %>% 
  mutate(SaldoBSO=bdcBSO_tab$Saldo,
         ClientesBSO=bdcBSO_tab$Clientes)

infos <- list(infoPerfFiltro=infoPerfFiltro,infoPerf=infoPerf)

write.xlsx(infos,'D:/!bso/califClientes/infoPerf2.xlsx')

################################################################################
bdcEne <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Ene2023.rds')
x <- bdcEne %>% 
  dplyr::filter(cosechaM==monDate) %>% 
  dplyr::filter(OPERACION_ORI_REF==0) %>% 
  summarise(saldo=sum(saldous),nOps=n())

bdcFeb <- readRDS('D:/!bso/girCartera/rdsGAR/ec_Feb2023.rds')
y <- bdcFeb %>% 
  dplyr::filter(cosechaM==monDate) %>% 
  dplyr::filter(OPERACION_ORI_REF==0) %>% 
  summarise(saldo=sum(saldous),nOps=n())
bind_rows(x,y) %>% 
  summarise(saldo=sum(saldo),nOps=sum(nOps),prom=saldo/nOps)
