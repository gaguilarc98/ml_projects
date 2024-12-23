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
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####___CREATING AND UPDATING MASTER_KEY_LIST____####
month <- c("enero","febrero","marzo","abril","mayo","junio","julio",
           "agosto","septiembre","octubre","noviembre","diciembre")
mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
year <- c(2018:2023)
myfile <- as.vector(sapply(year, function(x){paste0(mes,'_Etapa1_',month,x)}))

myfile <- c('10_Etapa1_octubre2023')

MasterKeyList <- list()
MasterKeyList <- readRDS("D:/!bso/accion/MasterKeyList.rds")
# i <- 1
for (i in 1:length(myfile)) {
  tryCatch({
    print(myfile[i])
    keys <- readxl::read_excel(paste0("C:/accion/",
                                      myfile[i],'.xlsx'), sheet = 'MS81479brllaves')
    keys_new <- keys %>% 
      select(CTACLIENTE=`Cuenta Cliente`,OPERACION=Operacion,MASCARA_CUENTA,
             MASCARA_OPERACION,LLAVEPRIMARIA) %>% 
      dplyr::filter(!is.na(LLAVEPRIMARIA)) %>% 
      distinct_all() %>% 
      dplyr::filter(!LLAVEPRIMARIA %in% MasterKeyList$LLAVEPRIMARIA)
    MasterKeyList <- MasterKeyList %>% 
      bind_rows(keys_new)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
saveRDS(MasterKeyList,'D:/!bso/accion/MasterKeyList.rds')

####____UNMASKING AND ADDING SEQUENCE SINCE 2015____####
MasterKeyList <- readRDS('D:/!bso/accion/MasterKeyList.rds') %>% 
  select(-MASCARA_CUENTA, -MASCARA_OPERACION)
dfTotal <- readRDS('D:/!bso/features/Clientes_Ene15Oct23.rds')
# moraFull <- readRDS("D:/!bso/accion/moraIntraMes_Ene2018May2023.rds") %>% 
#   mutate(`Fecha de Corte`= as.Date(monDate, frac=1)) %>% 
#   select(`Fecha de Corte`, CTACLIENTE, OPERACION, `Máxima DPD`=maximaDPD)

month <- c("enero","febrero","marzo","abril","mayo","junio","julio",
           "agosto","septiembre","octubre","noviembre","diciembre")
mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
year <- c(2018:2023)
myfile <- as.vector(sapply(year, function(x){paste0(mes,'_Etapa1_',month,x)}))

myfile <- c('10_Etapa1_octubre2023')
i <- 1
for (i in 1:length(myfile)) {
  tryCatch({
    print(myfile[i])
    # stage1 <- readxl::read_excel(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
    #                                     myfile[i],'.xlsx'),sheet='MS81479brbase')
    # garantias1 <- readxl::read_excel(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
    #                                         myfile[i],'.xlsx'),sheet='MS81479brgarantias')
    # fwrite(stage1, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
    #                           myfile[i],'.csv'),row.names = F,sep=",")
    # fwrite(garantias1, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
    #                       myfile[i],'_garantías.csv'),row.names = F,sep=",")
    
    
    # stage1 <- readxl::read_excel(paste0("D:/!bso/accion/entrega/", myfile[i],'.xlsx'), sheet = 'Sheet1')
    stage1 <- readxl::read_excel(paste0("C:/accion/", myfile[i],'.xlsx'), sheet = 'MS81479brbase')
      # dplyr::filter(ESTADO!="CASTIGADA") %>% 
    stageproc <- stage1 %>%
      dplyr::filter(MODULO!=131) %>% 
      # mutate(esFSL = ifelse(MODULO==118 | (!is.na(`Nombre del producto`) & str_detect(`Nombre del producto`,"MIGR")), 1, 0)) %>% 
      # mutate(`Días vencidos (DPD)` = ifelse(esFSL==1, 0, `Días vencidos (DPD)`)) %>% 
      # mutate(`Máxima DPD` = ifelse(esFSL==1, 0, `Máxima DPD`)) %>% 
      # mutate(`Saldo en mora` = ifelse(esFSL==1, 0, `Saldo en mora`)) %>% 
      # # mutate(ESTADO = ifelse(esFSL==1, 'VIGENTE', ESTADO)) %>% 
      # mutate(`Fecha de Corte` = as.Date(`Fecha de Corte`)) %>% 
      # relocate(esFSL, .after = `Máxima DPD`) %>% 
      left_join(MasterKeyList,by="LLAVEPRIMARIA") %>% 
      # select(-`Máxima DPD`) %>% 
      # left_join(moraFull,by=c("Fecha de Corte","CTACLIENTE","OPERACION")) %>% 
      glimpse()
      
    # stageMora <- stage1 %>% 
    #   dplyr::filter(MODULO!=131) %>% 
    #   mutate(`Fecha de Corte` = as.IDate(`Fecha de Corte`)) %>% 
    #   left_join(moraFull, by=c("Fecha de Corte","LLAVEPRIMARIA"))
    
    month_base <- str_to_title(substr(myfile[i],11,13))
    year_base <- substr(myfile[i],nchar(myfile[i])-3,nchar(myfile[i]))
    myfecha <- paste0(month_base,'. ',year_base)
    
    dfAux <- dfTotal %>% 
      mutate(mydes = as.yearmon(fdes)) %>% 
      dplyr::filter(mydes <= as.yearmon(myfecha)) %>% 
      # mutate(fueRefin = ifelse((!is.na(FechaRefin) & as.Date(FechaRefin) <= as.Date("2015-01-01")),0,fueRefin)) %>% 
      group_by(CTACLIENTE) %>% 
      summarise(N_OPNOREFIN = length(which(fueRefin==0)), N_OPREFIN = length(which(fueRefin==1)), N_TOTAL = N_OPNOREFIN+N_OPREFIN) %>% 
      select(CTACLIENTE, N_OPNOREFIN, N_OPREFIN, N_TOTAL) 
    
    stage_wseq <- stageproc %>% 
      # select(-N_OPNOREFIN, -N_OPREFIN) %>% 
      mutate(`Saldo en mora` = ifelse(ESTADO=="VIGENTE",0,`Saldo en mora`)) %>% 
      left_join(dfAux, by="CTACLIENTE") %>% 
      # select(-esFSL) %>% #-DIASMORA, -CTACLIENTE, -OPERACION,
      relocate(LLAVEPRIMARIA, .after = `Fecha de Corte`)
    
    saldo <- sum(stage_wseq[stage_wseq$ESTADO!="CASTIGADA",][["Saldo pendiente"]])
    saldoCast <- sum(stage_wseq[stage_wseq$ESTADO=="CASTIGADA",][["Saldo pendiente"]])
    saldoMora <- sum(stage_wseq[stage_wseq$ESTADO!="CASTIGADA",][["Saldo en mora"]])
    nOps <- n_distinct(stage_wseq$OPERACION)
    
    print(paste(myfile[i],"Saldo:",saldo,"| SaldoCast:",saldoCast,"| Mora:",saldoMora,"| Operaciones",nOps))
    
    ###Check Bantotal base this portion can be commented
    bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",month_base,year_base,".rds")) %>% 
      mutate(ESTADO = case_when(ctaCont %in% c("133","136") ~ "OP VENCIDA",
                                ctaCont %in% c("134","137") ~ "JUDICIAL",
                                ctaCont %in% c("865") ~ "CASTIGADA",
                                TRUE~ESTADO)) %>% 
      select(CTACLIENTE, OPERACION, saldous, saldoMora, saldoCast, ESTADO, OPERACION, AGENCIA, DIASMORA,ctaCont)
    bdc %>% group_by(ESTADO) %>% summarise(sum(saldous), sum(saldoCast))
    saldoBDC <- sum(bdc$saldous)
    CastBDC <- sum(bdc$saldoCast)
    moraBDC <- sum(bdc$saldoMora)
    nOpsBDC <- n_distinct(bdc$OPERACION)
    print(paste(month_base,year_base,"Saldo:",saldoBDC,"| SaldoCast:",CastBDC,"| Mora:",moraBDC,"| Operaciones",nOpsBDC))
    
    stage_wseq <- stage_wseq %>% #PARA MAYO CORREGIR LOS ESTADOS
      select(-ESTADO) %>%
      left_join(select(bdc,CTACLIENTE, OPERACION, ESTADO, DIASMORA), by=c("CTACLIENTE","OPERACION")) %>% 
      dplyr::relocate(ESTADO, .before = moneda)
    #VERIFICAR DIASMORA Y ESTADO Y SALDO MORA
    length(which(stageproc$`Días vencidos (DPD)` != stageproc$DIASMORA))
    stageproc %>% group_by(ESTADO) %>% summarise(sum(`Saldo pendiente`))  
    ###
    stage_wseq <- stage_wseq %>% select(-DIASMORA,-CTACLIENTE,-OPERACION)
    print(paste(myfile[i],"NOTFOUND: ",length(which(is.na(stage_wseq$N_TOTAL))) ))
    fwrite(stage_wseq,paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
                             myfile[i],'.csv'), row.names = F, sep="|",quote = FALSE)
    llaves <- readxl::read_excel(paste0("C:/accion/", myfile[i],'.xlsx'), sheet = 'MS81479brllaves') 
    garantias <- readxl::read_excel(paste0("C:/accion/", myfile[i],'.xlsx'), sheet = 'MS81479brgarantias',
                                    col_types = c('guess','guess','guess','numeric')) 
    
    fwrite(garantias,paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
                             myfile[i],'_garantias.csv'), row.names = F, sep="|",quote = FALSE)
    informe <- list(MS81479brbase=stage_wseq, MS81479brllaves = llaves, MS81479brgarantias = garantias)
    write_xlsx(informe, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
                               myfile[i],'.xlsx'))
    # fwrite(stage_wseq, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
    #                               myfile[i],'.csv'),row.names = F,sep=",")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
####____NEW APPROACH: THAT IS DO NOT CHECK, CORRECT INSTEAD____####
MasterKeyList <- readRDS('D:/!bso/accion/MasterKeyList.rds') %>% 
  select(-MASCARA_CUENTA, -MASCARA_OPERACION)
dfTotal <- readRDS('D:/!bso/features/Clientes_Ene15Sep23.rds')

myfile <- c('09_Etapa1_septiembre2023')
i <- 1
for (i in 1:length(myfile)) {
  tryCatch({
    print(myfile[i])
    
    stage1 <- readxl::read_excel(paste0("C:/accion/", myfile[i],'.xlsx'), sheet = 'MS81479brbase')
    # dplyr::filter(ESTADO!="CASTIGADA") %>% 
    stageproc <- stage1 %>%
      dplyr::filter(MODULO!=131) %>%
      left_join(MasterKeyList,by="LLAVEPRIMARIA") %>% 
      glimpse()
    
    dfAux <- dfTotal %>% 
      mutate(mydes = as.yearmon(fdes)) %>% 
      dplyr::filter(mydes <= as.yearmon(myfecha)) %>% 
      # mutate(fueRefin = ifelse((!is.na(FechaRefin) & as.Date(FechaRefin) <= as.Date("2015-01-01")),0,fueRefin)) %>% 
      group_by(CTACLIENTE) %>% 
      summarise(N_OPNOREFIN = length(which(fueRefin==0)), N_OPREFIN = length(which(fueRefin==1)), N_TOTAL = N_OPNOREFIN+N_OPREFIN) %>% 
      select(CTACLIENTE, N_OPNOREFIN, N_OPREFIN, N_TOTAL) 
    
    stage_wseq <- stageproc %>% 
      # select(-N_OPNOREFIN, -N_OPREFIN) %>% 
      left_join(dfAux, by="CTACLIENTE") %>% 
      relocate(LLAVEPRIMARIA, .after = `Fecha de Corte`)
    
    stage_wseq %>% group_by(ESTADO) %>% 
      summarise(S=sum(`Saldo pendiente`), SM=sum(`Saldo en mora`), DM=mean(`Días vencidos (DPD)`)) %>% 
      adorn_totals("row")
    
    month_base <- str_to_title(substr(myfile[i],11,13))
    year_base <- substr(myfile[i],nchar(myfile[i])-3,nchar(myfile[i]))
    myfecha <- paste0(month_base,'. ',year_base)
    
    ###Correct fields using BANTOTAL
    bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",month_base,year_base,".rds")) %>% 
      mutate(`Saldo pendiente` = saldous+saldoCast) %>% 
      mutate(`Saldo en mora` = par0+saldoCast) %>% 
      mutate(ESTADO = case_when(ctaCont %in% c("133","136") ~ "OP VENCIDA",
                                ctaCont %in% c("134","137") ~ "JUDICIAL",
                                ctaCont %in% c("865") ~ "CASTIGADA",
                                TRUE~ESTADO)) %>% 
      select(CTACLIENTE, OPERACION, `Saldo pendiente`, `Saldo en mora`, ESTADO, `Días vencidos (DPD)`= DIASMORA) #DIASMORA
    
    bdc %>% 
      group_by(ESTADO) %>% 
      summarise(S=sum(`Saldo pendiente`), M=sum(`Saldo en mora`), D=mean(`Días vencidos (DPD)`)) %>% 
      adorn_totals("row")
    
    stage_wseq <- stage_wseq %>% #PARA MAYO CORREGIR LOS ESTADOS
      select(-`Saldo pendiente`,-`Saldo en mora`, -ESTADO, -`Días vencidos (DPD)`) %>% 
      left_join(bdc, by=c("CTACLIENTE","OPERACION")) %>% 
      dplyr::relocate(`Saldo pendiente`, .after = `Score FINAL`) %>%
      dplyr::relocate(`Días vencidos (DPD)`, .before = `Monto de la Cuota`) %>% 
      dplyr::relocate(`Saldo en mora`, .before = `Intereses atrasados`) %>%
      dplyr::relocate(ESTADO, .before = moneda) 
      
    #Erase CTACLIENTE AND OPERACION
    stage_wseq <- stage_wseq %>% select(-CTACLIENTE,-OPERACION)
    print(paste(myfile[i],"NOTFOUND: ",length(which(is.na(stage_wseq$N_TOTAL)))))
    
    fwrite(stage_wseq,paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
                             myfile[i],'.csv'), row.names = F, sep="|",quote = FALSE)
    # llaves <- readxl::read_excel(paste0("C:/accion/", myfile[i],'.xlsx'), sheet = 'MS81479brllaves') 
    garantias <- readxl::read_excel(paste0("C:/accion/", myfile[i],'.xlsx'), sheet = 'MS81479brgarantias',
                                    col_types = c('guess','guess','guess','numeric')) 
    
    fwrite(garantias,paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
                            myfile[i],'_garantias.csv'), row.names = F, sep="|",quote = FALSE)
    informe <- list(MS81479brbase=stage_wseq, MS81479brllaves = llaves, MS81479brgarantias = garantias)
    write_xlsx(informe, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
                               myfile[i],'.xlsx'))
    # fwrite(stage_wseq, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
    #                               myfile[i],'.csv'),row.names = F,sep=",")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


####____BURO MASKING____####
MasterKeyList <- readRDS('D:/!bso/accion/MasterKeyList.rds')

maskcta <- MasterKeyList %>% 
  select(CTACLIENTE,MASCARA_CUENTA) %>% 
  dplyr::filter(!is.na(MASCARA_CUENTA)) %>% 
  group_by(CTACLIENTE) %>% 
  dplyr::filter(row_number()==max(row_number())) %>% 
  distinct_all()
maskope <- MasterKeyList %>% 
  select(CTACLIENTE,OPERACION,MASCARA_OPERACION,LLAVEPRIMARIA) %>% 
  dplyr::filter(!is.na(LLAVEPRIMARIA)) %>% 
  group_by(OPERACION) %>% 
  dplyr::filter(row_number()==max(row_number())) %>% 
  distinct_all()

mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
year <- c(2020:2023)
myutf <- as.vector(sapply(year, function(x){paste0(x,mes)}))

myutf <- c("202309")
infoList <- list()
# file_list <- list.files(path='//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/califClientes/rds/')
i <- 1
for (i in 1:length(myutf)) {
  tryCatch({
    print(myutf[[i]])
    infoRaw <- fread(paste0("D:/!bso/califClientes/utf/BSO",myutf[i],"_utf8.txt"), 
                     encoding = "UTF-8", sep = "|") %>% 
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT)) %>% 
      mutate(esBSO = ifelse(`SIGLA SBEF`=='BSO',1,0)) %>% 
      separate_wider_delim(NumeroOp, names = c("CTACLIENTE","OPERACION"), delim = "-",
                           too_few = "align_start", too_many = "merge") %>% 
      mutate(CTACLIENTE=as.integer(CTACLIENTE),
             OPERACION=as.integer(OPERACION)) %>% 
      mutate(esDeudorBSO = ifelse(str_detect(`TIPO OBLIGADO SBEF`,'A - ') & `SIGLA SBEF`=="BSO",1,0)) %>% 
      mutate(esCodeudorBSO = ifelse(str_detect(`TIPO OBLIGADO SBEF`,'B - ') & `SIGLA SBEF`=="BSO",1,0)) %>% 
      mutate(esGaranteBSO = ifelse(!str_detect(`TIPO OBLIGADO SBEF`,'A - ') & !str_detect(`TIPO OBLIGADO SBEF`,'B - ') & `SIGLA SBEF`=="BSO",1,0)) %>% 
      mutate(ESTADO = case_when(`SBEF VIGENTE` > 0 ~ 'VIGENTE',
                                `SBEF VENCIDO` > 0 ~ 'VENCIDO',
                                `SBEF EJECUCION` > 0 ~ 'EJECUCION',
                                `SBEF CASTIGADO` > 0 ~ 'CASTIGADO',
                                TRUE ~ 'NA'),
             SALDO = `SBEF VIGENTE` + `SBEF VENCIDO` + `SBEF EJECUCION`) %>% 
      group_by(CI) %>%
      dplyr::filter(sum(esDeudorBSO)>0) %>% #SOLO PARA QUEDARSE CON DEUDORES COMPARTIDOS
      dplyr::filter(sum(esBSO)>0) %>% 
      arrange(desc(esDeudorBSO), desc(esCodeudorBSO), desc(esBSO), desc(SALDO)) %>% #Para ordenar por esBSO
      mutate(CTACLIENTE = CTACLIENTE[row_number()==1]) %>% 
      ungroup() %>%
      select(CTACLIENTE, OPERACION, CI, HISTORICO, 
             fechaNacimiento=`FECHA NAC`, LOCALIDAD, CANTON, SECCION, PROVINCIA, DEPARTAMENTO,
             fechaDeclaracion=`FECHA DECLARACION`, Entidad_SF=`ENTIDAD SBEF`, Sigla_Entidad_SF=`SIGLA SBEF`, 
             FECHA, tipoObligado=`TIPO OBLIGADO SBEF`, tipoCredito=`TIPO CREDITO SBEF`, fechaInicio=`FECHA INICIO OPERACION`,
             MonedaOrigen, MontoOriginal, Calificacion_SF=`SBEF CALIFICACION`, SALDO, Saldo_USD_Contingente=`SBEF CONTINGENTE`,
             DiasMora, FECHAVTO, Periodo_de_Pago=`PERIODO PAGO`, 
             Saldo_USD_Vigente = `SBEF VIGENTE`, Saldo_USD_Vencido = `SBEF VENCIDO`,
             Saldo_USD_Ejecucion = `SBEF EJECUCION`, Saldo_USD_Castigado = `SBEF CASTIGADO`) %>%
      # mutate(HISTORICO = round(as.double(HISTORICO))) %>% 
      mutate(HISTORICO = as.character(HISTORICO)) %>% 
      mutate(fechaReporte = myutf[[i]]) 
    
    infoExp <- infoRaw %>% 
      left_join(maskcta,by="CTACLIENTE") %>% 
      left_join(maskope,by=c("CTACLIENTE","OPERACION")) %>% 
      select(-CTACLIENTE,-OPERACION,-CI)
    
    fwrite(infoExp, paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Buro/BSO_masked_buro_',
                           myutf[i],'.csv'), row.names = F, sep="|",quote = FALSE)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

accion <- fread("C:/accion/entrega/JulAgo2023/08_Etapa1_agosto2023.csv")
accion <- select(accion, -CTACLIENTE, -OPERACION)
fwrite(accion,paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
                         "08_Etapa1_agosto2023",'.csv'), row.names = F, sep="|",quote = FALSE)
####____COMPARACION FRECUENCIA PAGO CIC STAGE1____####

stage1 <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/accion/Entrega/Etapa1/',
                       "09_Etapa1_septiembre2023",'.csv'), sep="|",encoding = "UTF-8")
cic <- readRDS("D:/!bso/CIC/rds/cic_Sep2023.rds")

cic <- cic %>% select(CTACLIENTE, OPERACION, FormaPago) %>% left_join(MasterKeyList,by=c("CTACLIENTE","OPERACION"))

stage1Join <- stage1 %>% left_join(cic, by=c("LLAVEPRIMARIA"))

table(stage1Join$Frecuencia, stage1Join$FormaPago)
#Cuatrimestral en CIC sale como otros
#Todas as demás categorías coinciden y si noson registrados como irregular en stage1

####____FILTROS MODELO____####
stage1 <- readxl::read_excel(paste0("C:/accion/", "01_Etapa1_enero2022",'.xlsx'), sheet = 'MS81479brbase')

tipoCred <- c('101-11', '121-4', '101-12', '101-32', '101-2', '103-61', '112-1', '101-13','121-21', '101-18', '112-5', '103-64', '103-67', '121-19', '101-31', '109-3','101-70', '121-6', '101-53', '121-91', '103-70', '101-1', '101-5', '112-2','121-3', '103-60', '101-22', '101-4', '103-63', '101-20', '121-77', '109-7',
 '121-95', '101-80', '101-16', '121-22', '109-4', '103-62', '112-6', '121-20','101-19', '103-66', '121-24', '121-7', '101-21', '101-23', '103-65', '121-79',
 '121-92', '102-21','102-22', '102-32', '102-36', '121-30', '102-66','121-15', '102-74', '102-31', '109-26', '102-81', '109-24', '102-38', '121-46', '121-67',
 '121-68')


creds <- stage1 %>% 
  dplyr::filter(`ID del Producto` %in% tipoCred) %>% 
  select(`ID del Producto`, `Nombre del producto`) %>% 
  distinct_all()
