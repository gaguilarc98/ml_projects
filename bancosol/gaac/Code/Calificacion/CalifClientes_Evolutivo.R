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
# require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

####____WORST CALIF____####
year <- c("2018","2019","2020","2021","2022","2023")
month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
mybdc <- as.vector(sapply(year, function(x){paste0(month,x)}))

mybdc <- c("Dic2018","Dic2019","Dic2020","Dic2021","Dic2022","Ene2023",
           "Feb2023","Mar2023","Abr2023","May2023","Jun2023","Jul2023")
infoList <- list()

lastComp <- read_xlsx("D:/!bso/califClientes/output/lastComp_Jul2023.xlsx")


codmod <- read.xlsx('//VFSNALSRV/Bases_Riesgos/MIS_SGNRFAR/analitica/keyFields/CodModulo.xlsx')
fechaajuste <- readRDS("D:/!bso/features/Clientes_Ene15Ago23.rds") %>% 
  select(CTACLIENTE, OPERACION, FDES_ORI = fdes)

for(i in 1:length(mybdc)){
  tryCatch({
    print(mybdc[i])
    #//VFSNALSRV/Bases_Riesgos/MIS_SGNRFAR/analitica/rdsGAR/
    bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mybdc[i],'.rds')) %>%
      filter(ctaCont %in% c('131','133','134','135','136','137','623','865')) %>% 
      left_join(codmod, by = 'MODULO') %>% 
      left_join(fechaajuste, by = c('CTACLIENTE', 'OPERACION')) %>% 
      mutate(SaldoBruto = ifelse(ctaCont %in% c('131','133','134','135','136','137'), saldous, 0)) %>% 
      mutate(SaldoMora = ifelse(ctaCont %in% c('133','134','136','137'), saldous, 0)) %>% 
      mutate(OpsTotal = ifelse(ctaCont %in% c('131','133','134','135','136','137','623'), 1, 0)) %>% 
      mutate(OpsBruta = ifelse(ctaCont %in% c('131','133','134','135','136','137'), 1, 0)) %>% 
      mutate(OpsMora = ifelse(ctaCont %in% c('133','134','136','137'), 1, 0)) %>% 
      mutate(OpsPar0 = ifelse(par0>0, 1, 0)) %>% 
      mutate(esfsl = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"),1,0)) %>% 
      mutate(FDES_ORI = if_else(!is.na(FDES_ORI),FDES_ORI,fdes)) %>% 
      mutate(FDES_ORI = if_else(esfsl == 1, as.Date("2023-05-31"), FDES_ORI)) %>% 
      mutate(Fecha = as.yearmon(monDate)) %>%
      mutate(AnioInicio = ifelse(year(FDES_ORI) < 2017, '< 2017', as.character(year(FDES_ORI)))) %>% 
      mutate(PlazoAnios = ifelse(floor(PLAZODIAS/365)<=7,as.character(floor(PLAZODIAS/365)),
                                 "> 7")) %>% 
      select(Fecha, CTACLIENTE, OPERACION, ctaCont, NOMBRE_MODULO, Sucursal, 
             AnioInicio, CALIFICACION, PlazoAnios, Sector_Actividad, Sector_Destino,
             SaldoTotal = saldous, SaldoBruto, SaldoMora, 
             Par0=par0, PrevisionEspecifica=previus, InteresAnual=intus, OpsTotal,
             OpsBruta, OpsMora, OpsPar0)
    
    infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',mybdc[i],'.rds'))
    
    infoClean <- infoCheck %>% 
      dplyr::filter(REGULADO=="SBEF") %>% 
      dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
      mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
      mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
      group_by(CI) %>%
      dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
      mutate(PEOR_CALIF_SF = max(CALIFICACION[esBSO==0])) %>% 
      mutate(PEOR_CALIF_BSO = max(CALIFICACION[esBSO==1])) %>% 
      mutate(saldoBSO = saldo*esBSO) %>%
      ungroup() %>% 
      dplyr::filter(PEOR_CALIF_SF!="_" & SIGLA=="BSO") %>% 
      mutate(Compartido = 1) %>% 
      select(CI, CTACLIENTE, OPERACION, CALIFICACION, PEOR_CALIF_BSO, PEOR_CALIF_SF, 
             saldoBSO, Compartido)
    
    infoJoin <- bdc %>% 
      left_join(infoClean, by=c("CTACLIENTE","OPERACION"), 
                suffix=c("_BSO","_SF")) %>% 
      replace_na(list(Compartido = 0)) %>% 
      mutate(CALIFICACION_BSO = ifelse(Compartido==1 & PEOR_CALIF_BSO %in% c('A','B','C','D','E','F'),
                                   PEOR_CALIF_BSO, CALIFICACION_BSO)) %>% 
      mutate(PeorEnSF = ifelse(Compartido==1 & PEOR_CALIF_BSO < PEOR_CALIF_SF, 1,0)) %>% 
      mutate(SaldoComp = ifelse(Compartido==1, SaldoTotal, 0)) %>% 
      mutate(OpsComp = ifelse(Compartido==1, 1, 0)) %>% 
      mutate(SaldoPeorSF = ifelse(PeorEnSF==1, SaldoTotal, 0)) %>% 
      mutate(OpsPeorSF = ifelse(PeorEnSF==1, 1, 0)) %>%
      mutate(ClientesComp = n_distinct(CI)) %>% 
      mutate(ClientesPeorSF = n_distinct(CI[PeorEnSF==1])) %>% 
      select(Fecha, ctaCont, NOMBRE_MODULO, Sucursal, AnioInicio, PlazoAnios, Sector_Actividad,
             Sector_Destino, CALIFICACION_BSO, PEOR_CALIF_SF,
             SaldoTotal,SaldoBruto, SaldoMora, Par0, SaldoComp, SaldoPeorSF, OpsTotal,
             OpsBruta, OpsMora, OpsPar0, OpsComp, OpsPeorSF, ClientesComp, ClientesPeorSF) %>% 
      group_by(Fecha, ctaCont, NOMBRE_MODULO, Sucursal, AnioInicio, PlazoAnios, Sector_Actividad,
               Sector_Destino, CALIFICACION_BSO, PEOR_CALIF_SF) %>% 
      summarise(across(c(SaldoTotal:OpsPeorSF),~sum(.x,na.rm=T)),
                across(c(ClientesComp, ClientesPeorSF),~max(.x, na.rm = T))) %>% 
      ungroup()
    
    infoList[[i]] <- infoJoin
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

infoFull <- bind_rows(infoList) %>% 
  mutate(Fecha = as.Date(Fecha, frac=1))


write_xlsx(infoFull,"D:/!bso/califClientes/TablaPeorSF.xlsx")

lC <- infoFull %>% 
  select(CTACLIENTE, OPERACION, califPeorSF)


aj <- infoJoin %>% 
  dplyr::filter(Compartido==1) %>% 
  inner_join(lC, by=c("CTACLIENTE", "OPERACION")) %>% 
  mutate(cc = califPeorSF==PEOR_CALIF_SF)

write_xlsx(infoFull, "D:/!bso/califClientes/b")
####____PEOR CALIFICACION____####
lastmonth <- "Jul. 2023"
shortmonth <- str_replace(lastmonth,". ","")

infoPerf <- infoFull %>% 
  group_by(CI_info) %>% 
  mutate(GENERO = max(GENERO)) %>% 
  ungroup() %>% 
  group_by(Fecha,CI_info,GENERO) %>% 
  summarise(across(c(Sucursal,ESTADO_bdc,siglaPeorSF:montoNoBSO,SaldoTotalBSO,ClientesBSO),~max(.x)),
            across(c(saldo,saldous,saldoMora_bdc,saldoCast_bdc),~sum(.x))) %>%
  mutate(Cliente = 1.0) %>% 
  mutate(PeorEnSF=ifelse(califPeorBSO < califPeorSF,1,0)) %>% 
  mutate(MejorEnSF=ifelse(califPeorBSO > califPeorSF,1,0)) %>% 
  mutate(igualEnSF=ifelse(califPeorBSO==califPeorSF,1,0)) %>% 
  mutate(saldoPeor=PeorEnSF*saldo) %>% 
  ungroup()

x <- infoPerf %>% 
  filter(Fecha == "Jun. 2023") %>% 
  group_by(CI_info) %>% 
  filter(max(row_number())>1) %>% 
  mutate(GENERO = max(GENERO))


check <- infoPerf %>% #Check de cuadre con meses anteriores
  group_by(Fecha) %>% 
  summarise(Clientes = sum(Cliente),saldoBSO=sum(saldous),Cast=sum(saldoCast_bdc),
            cliente_distint = n_distinct(CI_info),
            SaldoTotBSO = saldoBSO+Cast,ClientesTot=max(ClientesBSO),
            SaldoTot=max(SaldoTotalBSO),
            Clientes_perc=Clientes/max(ClientesBSO),
            SaldoBSO_perc = saldoBSO/max(SaldoTotalBSO),Peores=sum(PeorEnSF),
            SaldoPeor = sum(saldoPeor))

infoPerf %>% #Check peor entidad
  dplyr::filter(Fecha==lastmonth) %>% 
  group_by(siglaPeorSF) %>% 
  summarise(Clientes=sum(Cliente),ClientesPeores=sum(PeorEnSF),SaldoBSO=sum(saldo),SaldoPeor=sum(saldoPeor)) %>% 
  arrange(desc(SaldoBSO))

infoPerf %>% #Check por peor Calif
  dplyr::filter(Fecha==lastmonth, PeorEnSF==1) %>% 
  group_by(califPeorSF,califPeorBSO) %>% 
  summarise(Saldo=sum(saldoPeor)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = califPeorSF, values_from = Saldo) %>% 
  adorn_totals("col")

# write_rds(infoFull,'D:/!bso/califClientes/infoFull.rds')
# infoFull <- readRDS('D:/!bso/califClientes/infoFull.rds')

####____PREVISION POR CONTAGIO____####
lastCierre <- fread(paste0("D:/SCRIPTS R/Pagos tardios/lastCierrreUR_",shortmonth,".txt"),
                    encoding = "UTF-8",sep="|",fill=T) %>% 
  select(CTACLIENTE=Cuenta, OPERACION =Operacion,Instancias_UR,Ultimo_mes,Prevision_USD,CALIFICACION)

#Check de integridad para ver si las calificaciones coinciden, el resultado debe ser cero
length(which(infoFull$califPeorBSO!=infoFull$CALIFICACION))

infoPrev <- infoFull %>% 
  dplyr::filter(Fecha==lastmonth) %>% 
  rename(califPot = califPeorSF, FDES=fdes) %>% 
  prevision() %>% 
  dplyr::filter(califPot>califPeorBSO) %>% #califPot>CALIFICACION 
  select(CTACLIENTE,OPERACION,saldous,previus,previusNew,califPeorBSO,califPot) %>% 
  full_join(lastCierre,by=c("CTACLIENTE","OPERACION")) %>% 
  replace_na(list(Instancias_UR=0,Ultimo_mes=0)) %>% 
  mutate(RangoPagosTardios = case_when(Instancias_UR<6~as.character(Instancias_UR),
                                       Instancias_UR>=6~"6+",)) %>% 
  mutate(previusNew = ifelse(califPot<=califPeorBSO,0,previusNew)) %>% 
  mutate(difPrev = previusNew-previus)

#Check de integridad para ver si las calificaciones coinciden, el resultado debe ser cero
length(which(infoFull$califPeorBSO!=infoFull$CALIFICACION))
#Check de integridad para ver si el número de filas coinciden antes y después del join
nrow(lastCierre)==nrow(infoPrev)

infoPrev %>% #Check de integridad de prevision adicional
  group_by(califPot,CALIFICACION) %>% 
  summarise(difPrev=sum(previusNew)) %>% 
  pivot_wider(names_from = califPot,values_from = difPrev) %>% 
  adorn_totals('row')

infoPrev %>% #Check de Pagos tardíos
  dplyr::filter(califPot!="A") %>% 
  group_by(califPot,RangoPagosTardios) %>% 
  summarise(Saldo=sum(saldous)) %>% 
  ungroup() %>%
  pivot_wider(names_from = RangoPagosTardios,values_from = Saldo) %>% 
  adorn_totals(c("col","row"))

infoPerf <- infoPerf %>% 
  mutate(Fecha = as.Date(Fecha,frac=1))
infoPrev <- infoPrev 
Informe <- list(Perf = infoPerf, Prev = infoPrev) 

write_xlsx(Informe, paste0("D:/SCRIPTS R/Clientes compartidos/lastComp_",shortmonth,".xlsx"))

####____CALIFREADING____#### 
lastCierre <- fread(paste0("D:/!bso/mph/Oreports/lastCierrreUR_",shortmonth,".csv"),
                    encoding = "UTF-8",sep=",",fill=T) %>% 
  select(CTACLIENTE=Cuenta, OPERACION =Operacion, Sucursal, Agencia, Asesor)
Informe <- read_xlsx(paste0("D:/!bso/califClientes/output/lastComp_",shortmonth,"_v2.xlsx"), sheet = "Prev")

Informe_wSucursal <- Informe %>% 
  left_join(lastCierre,by=c("CTACLIENTE","OPERACION"))

write_xlsx(Informe_wSucursal,"D:/!bso/mph/condonados/PeorCalif_wSucursalAbr2023.xlsx")
####____ADDING CONDONACIONES____####
Informe_wSucursal_select <- infoClean %>% 
  select(CTACLIENTE, califPeorSF) %>% 
  group_by(CTACLIENTE) %>% 
  summarise(PeorCalificacion = max(califPeorSF,na.rm = T))

tabla <- read.xlsx("D:/!bso/mph/condonados/TardioCond_May2023_v2.xlsx",sheet = "Datos")

condTardioPeorCalif <- tabla %>% 
  left_join(Informe_wSucursal_select, by=c("Cuenta"="CTACLIENTE"))

write_xlsx(condTardioPeorCalif,"D:/!bso/mph/condonados/TardioCond_May2023_v3.xlsx")  
####_____COMPLETAR CON PAGOS TARDIOS____####
infoFull %>% 
  group_by(Fecha) %>% 
  summarise(nNAs = length(which(is.na(ESTADO))))

bsoList <- list()
for (i in 1:length(mybdc)) {
  bdcBSO_full <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera',
                              mybdc[i],'.txt'),encoding = 'Latin-1', fill = T) %>% 
    dplyr::filter(MODULO!=131) %>%
    dplyr::filter(ESTADO!="CASTIGADA") %>%
    mutate(fbase = mybdc[i]) %>%
    mutate(mon = substr(fbase,1,3)) %>%
    mutate(year = substr(fbase,4,7)) %>%
    mutate(Fecha = as.yearmon(paste0(mon,'. ',year))) %>%
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


infoFull %>% group_by(Fecha) %>% summarise(saldo=sum(saldo),nOps=n_distinct(CI),n = n())
infoPerf <- infoFull %>% 
  # dplyr::filter(MODULO!=131) %>%
  # dplyr::filter(ESTADO!="CASTIGADA") %>%
  group_by(Fecha,CI,GENERO,entidadSF_2) %>% 
  summarise(saldo=sum(saldous),saldoMora=sum(saldoMora),
            par0=sum(par0),
            califBSO_2=max(califBSO_2),califSF_2=max(califSF_2),
            saldoNoBSO=max(saldoNoBSO),saldoMoraNoBSO=max(saldoMoraNoBSO),
            saldoMMCNoBSO=max(saldoMMCNoBSO)) %>% 
  ungroup() %>% 
  left_join(bsoFull,by="Fecha") %>%
  mutate(Fecha=as.Date(Fecha)) %>% 
  mutate(Cliente = 1.0) %>% 
  mutate(PeorCalif=ifelse(califBSO_2<califSF_2,1,0)) %>% 
  mutate(MejorCalif=ifelse(califBSO_2>califSF_2,1,0)) %>% 
  mutate(igualCalif=ifelse(califBSO_2==califSF_2,1,0)) %>% 
  mutate(Clientes_comp=Cliente/ClientesBSO,
         Saldo_comp=saldo/SaldoBSO,
         Saldo_peor_comp=PeorCalif*saldo/SaldoBSO,
         Saldo_peor=PeorCalif*saldo)

write.xlsx(infoPerf,'D:/!bso/califClientes/infoPerfAgo22Ene23_v2.xlsx')

####____COMPARTIDOS Y EXCLUSIVOS MKT____####
mybdc <- "Mar2023"

print(mybdc)
bdcBSO_full <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mybdc,'.rds')) %>%
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  mutate(FECHA = as.Date(monDate,frac=1)) %>%
  mutate(MONTO_USD = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
  mutate(ESTADO = case_when(ESTADO %in% c("VIGENTE","SUSPENSO")~"1. VIGENTE",
                            ESTADO=="OP VENCIDA"~"2. VENCIDA",
                            ESTADO=="JUDICIAL"~"3. EJECUCION",
                            ESTADO=="CASTIGADA"~"4. CASTIGADA",)) %>% 
  mutate(SALDO_VIG = ifelse(ctaCont %in% c('131','135'),saldous,0)) %>% 
  select(FECHA, CTACLIENTE, OPERACION, CI, GENERO, ESTADO, MODULO, CALIFICACION,
         MONTO_USD, SALDO_USD = saldous, SALDO_MORA = saldoMora, SALDO_VIG,
         SALDO_CAST = saldoCast) %>% 
  # group_by(FECHA, CTACLIENTE) %>% 
  # summarise(across(c(CI, GENERO, ESTADO, CALIFICACION),~max(.x)),
  #           across(c(MONTO_USD, SALDO_USD, SALDO_MORA, SALDO_VIG, SALDO_CAST),~sum(.x))) %>% 
  glimpse()


infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',mybdc,'.rds'))

infoClean <- infoCheck %>% 
  dplyr::filter(REGULADO=="SBEF") %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
  mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
  mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
  mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
  group_by(CI,esBSO) %>% 
  mutate(maxMonto = ifelse(MontoOriginal==max(MontoOriginal),1,0)) %>% 
  mutate(peorCalif = ifelse(CALIFICACION==max(CALIFICACION),1,0)) %>% 
  ungroup() %>% 
  group_by(CI) %>% 
  dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  mutate(ENTIDAD_MAX_SF = ifelse(maxMonto==1 & esBSO==0,ENTIDAD,'_')) %>% 
  mutate(SIGLA_MAX_SF = ifelse(maxMonto==1 & esBSO==0,SIGLA,'_')) %>% 
  mutate(CALIF_MAX_SF = ifelse(maxMonto==1 & esBSO==0,CALIFICACION,'_')) %>% 
  mutate(ENTIDAD_PEOR_SF = ifelse(peorCalif==1 & esBSO==0,ENTIDAD,'_')) %>% 
  mutate(SIGLA_PEOR_SF = ifelse(peorCalif==1 & esBSO==0,SIGLA,'_')) %>% 
  mutate(CALIF_PEOR_SF = ifelse(peorCalif==1 & esBSO==0,CALIFICACION,'_')) %>% 
  mutate(across(ENTIDAD_MAX_SF:CALIF_PEOR_SF,~max(.x,na.rm=T))) %>% #Para repetir los valores en cada fila
  mutate(SALDO_USD = sum(saldo*noesBSO,na.rm=T)) %>% 
  mutate(SALDO_VIG = sum(saldoVig*noesBSO,na.rm=T)) %>% 
  mutate(SALDO_MORA = sum(saldoMora*noesBSO,na.rm=T)) %>% 
  mutate(SALDO_CAST = sum(saldoCast*noesBSO,na.rm=T)) %>% 
  mutate(MONTO_USD = sum(MontoOriginal*noesBSO,na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::filter(CALIF_MAX_SF!="_" & SIGLA=="BSO") %>% 
  select(CI, CTACLIENTE, OPERACION, ENTIDAD_MAX_SF, SIGLA_MAX_SF, CALIF_MAX_SF, 
         ENTIDAD_PEOR_SF, SIGLA_PEOR_SF, CALIF_PEOR_SF, SALDO_USD, SALDO_VIG, SALDO_MORA, 
         SALDO_CAST, MONTO_USD)


infoJoin <- bdcBSO_full %>% 
  inner_join(select(infoClean,-CI), by = c("CTACLIENTE","OPERACION"), 
             suffix=c("_BSO","_SF"))
infoAntiJoin <- bdcBSO_full %>% 
  anti_join(infoClean,by=c("CTACLIENTE","OPERACION"))


Comp <- infoJoin %>% 
  group_by(FECHA, CTACLIENTE, CI, GENERO) %>% 
  mutate(maxSaldo = ifelse(SALDO_USD_BSO==max(SALDO_USD_BSO),1,0)) %>% 
  mutate(ESTADO = ifelse(maxSaldo==1,ESTADO,'_')) %>% 
  mutate(CALIFICACION_BSO = ifelse(maxSaldo==1,CALIFICACION,'_')) %>% 
  mutate(MODULO = ifelse(maxSaldo==1,MODULO,-1)) %>% 
  mutate(across(c(ESTADO,MODULO,CALIFICACION_BSO),~max(.x,na.rm = T))) %>% 
  summarise(across(c(ESTADO, MODULO, CALIFICACION, ENTIDAD_MAX_SF, SIGLA_MAX_SF, CALIF_MAX_SF,ENTIDAD_PEOR_SF,
                     SIGLA_PEOR_SF, CALIF_PEOR_SF, SALDO_USD_SF, SALDO_MORA_SF,SALDO_CAST_SF,MONTO_USD_SF),~max(.x)),
            across(c(SALDO_USD_BSO,MONTO_USD_BSO,SALDO_MORA_BSO, SALDO_VIG, SALDO_CAST_BSO),~sum(.x)))

Exc <- infoAntiJoin %>% 
  group_by(FECHA, CI,CTACLIENTE, GENERO) %>% 
  mutate(maxSaldo = ifelse(SALDO_USD==max(SALDO_USD),1,0)) %>% 
  mutate(ESTADO = ifelse(maxSaldo==1,ESTADO,'_')) %>% 
  mutate(CALIFICACION = ifelse(maxSaldo==1,CALIFICACION,'_')) %>% 
  mutate(MODULO = ifelse(maxSaldo==1,MODULO,-1)) %>% 
  summarise(across(c(ESTADO, MODULO, CALIFICACION),~max(.x)),
            across(c(SALDO_USD,MONTO_USD,SALDO_MORA, SALDO_VIG, SALDO_CAST),~sum(.x)))

n_distinct(bdcBSO_full$CTACLIENTE)== nrow(Exc)+nrow(Comp)
lista <- list(Compartidos = Comp,Exclusivos = Exc)
write.xlsx(lista,'D:/!bso/califClientes/Compartidos_Exclusivos_Abr2023.xlsx')
