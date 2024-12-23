####____CARGA DE LIBRERIAS____####
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
####____CONVERTING TXT TO RDS FOR BETTER STORAGE____####
year <- c("2018","2019","2020","2021","2022","2023")
myutf <- "202303"

infoRaw <- fread(paste0('D:/!bso/califClientes/utf/BSO',myutf,'_utf8.txt'), encoding = 'UTF-8', fill = T)
saveRDS(infoRaw,paste0("D:/!bso/Califclientes/rds/BSO",myutf,".rds"))

####____READING RDS SINCE 2018____####
year <- c("2018","2019","2020","2021","2022","2023")
mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
myutf <- as.vector(sapply(year, function(x){paste0(x,mes)}))
myutf <- "202303"
for(i in 1:length(myutf)){
  tryCatch({
    print(myutf[i])
    infoRaw <- readRDS(paste0('D:/!bso/califClientes/rds/BSO',myutf[i],'.rds'))
    infoCleanA <- infoRaw %>%
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT),
             saldo = `SBEF VIGENTE` + `SBEF VENCIDO` + `SBEF EJECUCION`,
             saldoMora = `SBEF VENCIDO` + `SBEF EJECUCION`,
             saldoMoraCast = `SBEF VENCIDO` + `SBEF EJECUCION` + `SBEF CASTIGADO`) %>%
      mutate(ESTADO = case_when(`SBEF VIGENTE`>0 ~ '1. VIGENTE',
                                    `SBEF VENCIDO`>0 ~ '2. VENCIDA',
                                    `SBEF EJECUCION`>0 ~ '4. JUDICIAL',
                                    `SBEF CASTIGADO`>0 ~ '5. CASTIGADA',
                                    `SBEF CONTINGENTE`>0 ~ '0. CONTINGENTE')) %>% 
      select(NDOC = `NRO DOCUMENTO`, EXT, CI, `TIPO OBLIGADO SBEF`, HISTORICO, DiasMora,
             SIGLA = `SIGLA SBEF`,ENTIDAD = `ENTIDAD SBEF`, CALIFICACION = `SBEF CALIFICACION`,
             FechaInicio = `FECHA INICIO OPERACION`, MonedaOrigen, MontoOriginal,
             saldo, saldoMora, saldoMoraCast, saldoVig = `SBEF VIGENTE`, 
             saldoCast = `SBEF CASTIGADO`, saldoCont = `SBEF CONTINGENTE`, NumeroOp, ESTADO,
             TIPO_CREDITO = `TIPO CREDITO SBEF`) %>%
      separate_wider_delim(NumeroOp, names = c("CTACLIENTE","OPERACION"), delim="-",
                           too_few = 'align_start', too_many = 'merge') %>% 
      mutate(CTACLIENTE = as.numeric(CTACLIENTE)) %>% 
      mutate(OPERACION = as.numeric(OPERACION)) %>% 
      mutate(across(MontoOriginal:saldoCont,~as.numeric(.x)/6.86)) %>% 
      mutate(Fecha=as.yearmon(ymd(paste0(substr(myutf[i],1,4),'/',substr(myutf[i],5,8), '/01')))) %>% 
      mutate(esBSO=ifelse(`SIGLA SBEF`=='BSO',1,0)) %>%
      mutate(esFSL=ifelse(`SIGLA SBEF`=='FSL',1,0)) %>%
      # mutate(Fecha=as.yearmon(paste0(substr(mybdc[i],1,3),'. ',substr(mybdc[i],4,7)))) %>% 
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
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

####____CREATING FETURE LIST____####
#Para el primer lote: "D:/!bso/califClientes/BSO_FSL.xlsx"
OpsBSO <- readRDS('D:/!bso/features/Clientes_Ene15Abr23_v3.rds') %>% 
  group_by(CTACLIENTE) %>% 
  arrange(desc(monDate)) %>% 
  mutate(CI=CI[row_number()==1]) %>% 
  ungroup()

ClientesBSO <- OpsBSO %>% 
  group_by(CI) %>% 
  summarise(PeorCalif_2015 = max(peorCalif), MaxMora_2015 = max(moraMax),
            fueRefin_2015 = max(fueRefin), fueCast_2015 = max(fueCast), 
            fueReprog_2015 = max(fueReprog), NCred_2015 = n_distinct(OPERACION),
            TotalMaxMora_2015 = sum(moraMax))

####____PAGOS TARDIOS HIST____####
PagosTFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Abr23.rds")

PagosCI <- PagosTFull %>% 
  left_join(select(OpsBSO,Cuenta=CTACLIENTE,Operacion=OPERACION,CI,GENERO),by=c("Cuenta","Operacion"))

TardiosBSO <- PagosCI %>% 
  dplyr::rename(CTACLIENTE = Cuenta) %>% 
  group_by(CI) %>% 
  summarise(N_MES_TARDIOS_2018 = sum(appsU),CAP_PAGADO_TARDIO_2018 = sum(CapitalPagado))
####____CONDONACIONES____####
codAge <- read_excel('D:/!bso/bases/excel/CodAgeSucReg.xlsx')
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
    dplyr::rename(Cod_Agencia = AGENCIA) %>% 
    group_by({{vstep}}, Sucursal, Cod_Agencia, Nombre_Agencia, NOMBRE_AGENCIA, 
             Cod_Asesor, Nombre_Asesor, Cuenta, Operacion) %>% 
    summarise_all(sum) %>% 
    ungroup() %>% 
    mutate(Int_Condonado = ifelse(CondInt_USD>0,1,0)) %>% 
    mutate(Cap_Condonado = ifelse(CondCap_USD>0,1,0)) %>% 
    mutate(IntCap_Condonado = ifelse(CondCapInt_USD>0,1,0)) %>% 
    mutate(Key = paste(Cuenta,Operacion,sep="-")) 
}
condFull <- readRDS('D:/!bso/condonaciones/condon/CondFull_Ene19Mar23.rds')
gph <- condFull %>% 
  mutate(monDate = as.yearmon(Fecha)) %>% 
  process(vstep = monDate) #La agrupación de una condonación por mes se hace en esta función process

CondCI <- gph %>% 
  left_join(select(OpsBSO,Cuenta=CTACLIENTE,Operacion=OPERACION,CI,GENERO),by=c("Cuenta","Operacion")) %>% 
  dplyr::rename(CTACLIENTE=Cuenta) %>% 
  group_by(CI) %>% #Antes había GENERO en la agrupación y se generaban duplicados
  summarise(N_MES_CONDONADOS_2019 = n(),Total_CapInt_Condonado_2019 = sum(IntCap_Condonado,na.rm = T))
####____EN LISTA VIP____####
VIPCompra <- read_xlsx("D:/!bso/vipCartera/Leads_PreAprobados_CompraCartera_Marzo2023.xlsx",sheet="CompraCartera")

VIPCI <- VIPCompra %>% 
  select(-CI,-Genero) %>% 
  left_join(select(OpsBSO,CTACLIENTE,OPERACION,CI,GENERO),by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(esVIP_Mar23=1) %>% 
  select(CI,esVIP_Mar23)
  
FSLJoin <- ClientesBSO %>% 
  left_join(TardiosBSO,by="CI") %>% 
  left_join(CondCI,by="CI") %>% 
  left_join(VIPCI,by="CI") %>% 
  replace_na(list(esVIP_Mar23=0))

write_xlsx(FSLJoin,"D:/!bso/califclientes/FeaturesFULL.xlsx")
FSLJoin <- read_xlsx("D:/!bso/califclientes/FeaturesFULL.xlsx")
####____LISTA DE FSL____####
FSL <- read_xlsx("D:/!bso/califClientes/BSO_FSL_v3.xlsx",sheet = "Hoja1") 
OpsFSL <- FSL %>% 
  select(CI=IDENTIFICACION)
  select(CI=IDENTIFICACION,TIPO=TIPO_IDENTIFICACION,NOMBRE_COMPLETO, COD_TIPO_RELACION,
         GENERO,NRO_CODEUDORES,CPOP=COD_BENEFICIO_CPOP,MONTO_DES_BS,TASA,NRO_LINEA,
         TIPO_CREDITO = TIPO_CRED, tipoCred=CRED_TIPO_ASFI,MONEDA,CAEDEC_DEST=DEST_CRED,
         grupoCaedecC=GR_ACT_ECO,grupoCaedecD=GR_DEST_CRED,garantia=TIPO_GARANTIA,
         ctaCont=CTA_CONTABLE_SALDO_NO_DIF,SALDO_BS,ESTADO,DIASMORA=DIAS_INCUMPLIMIENTO,
         CALIFICACION=CALIF_ENT, PROM_DIAS_ATRASO,CREDITO_PRODUCTIVO_NO_PRODUCTIVO) %>% 
  mutate(SALDO_USD = as.numeric(SALDO_BS)/6.86,
         MONTO_USD = as.numeric(MONTO_DES_BS)/6.86) %>% 
  select(-SALDO_BS,-MONTO_DES_BS)
FSLJoin1 <- FSLJoin %>% 
  # dplyr::rename(esVIP_Mar23=esVIP) %>% 
  mutate(EnListaFSL = ifelse(CI %in% OpsFSL$CI,1,0))

FSLJoin2 <- FSLJoin[FSLJoin$CI %in% OpsFSL$CI,]

write_xlsx(FSLJoin2,"D:/!bso/califClientes/FSL_Features_v2.xlsx")


####____BACKUP Calif_Ultimate_Survivor____####
for(i in 1:length(mybdc)){
  tryCatch({
    print(myutf[i])
    infoRaw <- readRDS(paste0('D:/!bso/califClientes/rds/BSO',myutf[i],'.rds'))
    #CHECK DE INTERGRIDAD
    rep <- length(which(infoRaw$`ENTIDAD FR`!="" & infoRaw$`ENTIDAD SBEF`!=""))
    print("NRO SBEF Y FR: ", rep)
    if(rep>0){
      break
    }
    infoCheck <- infoRaw %>%
      mutate(Fecha=as.yearmon(ymd(paste0(substr(myutf[i],1,4),'/',substr(myutf[i],5,8), '/01')))) %>% 
      mutate(REGULADO = case_when(!is.na(`ENTIDAD SBEF`) ~ "SBEF",
                                  !is.na(`ENTIDAD FR`) ~ "FR",
                                  TRUE ~ "")) %>% 
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT)) %>% 
      rowwise() %>% 
      mutate(saldo = case_when(REGULADO=="SBEF" ~ sum(`SBEF VIGENTE`,`SBEF VENCIDO`,`SBEF EJECUCION`,na.rm = T),
                               REGULADO=="FR" ~ sum(`FR VIGENTE`,`FR VENCIDO`,`FR EJECUCION`,na.rm = T),
                               TRUE ~ NA) %>% 
               mutate(saldoMora = case_when(REGULADO=="FR"))  
             
             saldoMora = sum(`SBEF VENCIDO`,`SBEF EJECUCION`,na.rm = T)) %>%
      mutate(saldo_FR = sum(`FR VIGENTE`, `FR VENCIDO`, `FR EJECUCION`, `FR CASTIGADO`, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(ESTADO = case_when(`SBEF VIGENTE`>0 ~ '1. VIGENTE',
                                `SBEF VENCIDO`>0 ~ '2. VENCIDA',
                                `SBEF EJECUCION`>0 ~ '4. JUDICIAL',
                                `SBEF CASTIGADO`>0 ~ '5. CASTIGADA',
                                `SBEF CONTINGENTE`>0 ~ '0. CONTINGENTE',)) %>% 
      mutate(ESTADO_FR = case_when(`FR VIGENTE`>0 ~ '1. VIGENTE',
                                   `FR VENCIDO`>0 ~ '2. VENCIDA',
                                   `FR EJECUCION`>0 ~ '4. JUDICIAL',
                                   `FR CASTIGADO`>0 ~ '5. CASTIGADA',)) %>% 
      select(NDOC = `NRO DOCUMENTO`, EXT, CI, NOMBRE = `NOMBRE COMPLETO`, DEPARTAMENTO, 
             TIPO_OBLIGADO = `TIPO OBLIGADO SBEF`, SIGLA = `SIGLA SBEF`, ENTIDAD = `ENTIDAD SBEF`,
             TIPO_OBLIGADO_FR = `TIPO OBLIGADO FR`, SIGLA_FR = `SIGLA FR`, ENTIDAD_FR = `ENTIDAD FR`,
             CALIFICACION = `SBEF CALIFICACION`, FECHAINICIO = `FECHA INICIO OPERACION`,
             FECHAVTO, NumeroOp, ESTADO, HISTORICO, DiasMora, MonedaOrigen, MontoOriginal, 
             saldo, saldoMora, saldoVig = `SBEF VIGENTE`, saldoCast = `SBEF CASTIGADO`, 
             saldoCont = `SBEF CONTINGENTE`, TIPO_CREDITO = `TIPO CREDITO SBEF`) %>%
      mutate(NOP_ORIGINAL = NumeroOp) %>% 
      separate_wider_delim(NumeroOp, names = c("CTACLIENTE","OPERACION"), delim="-",
                           too_few = 'align_start', too_many = 'merge') %>% 
      mutate(CTACLIENTE=as.numeric(CTACLIENTE),
             OPERACION=as.numeric(OPERACION)) %>% 
      mutate(across(MontoOriginal:saldoCont,~as.numeric(.x)/6.86)) %>% 
      mutate(HISTORICO = as.character(HISTORICO)) %>% # HISTORY CONSTRUCTION
      mutate(histLast16 = substr(HISTORICO, 1, 16)) %>% 
      mutate(histLast16 = ifelse(is.na(histLast16), '0', histLast16)) %>% 
      mutate(histLast1 = substr(HISTORICO, 1, 1)) %>% 
      mutate(histLast1 = ifelse(is.na(histLast1), '0', histLast1)) %>% 
      mutate(badCredit = ifelse((str_detect(histLast16, '2') | str_detect(histLast16, '3') |
                                   str_detect(histLast16, '4')),1,0)) %>% # CHECK MEASURE
      mutate(worstCredit = ifelse(str_detect(histLast16, '4'),1,0)) %>% # CHECK MEASURE
      mutate(N_VIGENTE = str_count(histLast16,'1'),
             N_VENCIDO = str_count(histLast16,'2'),
             N_EJECUCION = str_count(histLast16,'3'),
             N_CASTIGADO = str_count(histLast16,'4')) %>% 
      ungroup() %>% 
      dplyr::filter(!(is.na(HISTORICO) & is.na(SIGLA) & is.na(TIPO_OBLIGADO)))
    
    saveRDS(infoCheck,paste0('D:/!bso/califClientes/process/comp_',mybdc[i],'.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}