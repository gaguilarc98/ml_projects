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
library(stringr) # Working with strings
library(forcats) 
library(scales)
library(janitor)
library(openxlsx)
library(ggplot2)
library(ca)
library(openxlsx)
require(XLConnect)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____LLAVE MAESTRA____####
year <- c(2018:2019)
report <- data.frame(Nom_arch = as.character(NULL),
                     N_missing = as.numeric(),
                     N_rows = as.numeric(),
                     N_unique = as.numeric(),
                     N_new = as.numeric())
masterKeyList <- list()
system.time(
for (i in 1:length(year)) {
  arch <- list.files(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/RESULTADOS 2/",
                            year[i]))
  print(year[i])
  for (j in 1:(length(arch))) {
    action <- readxl::read_excel(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/RESULTADOS 2/",
                             year[i],'/',arch[j]),sheet = "MS81479brllaves")
    action <- readxl::read_excel(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/RESULTADOS 2/Abril_2daEntrega/",
                                        "04_Etapa1_abril2023.xlsx"),sheet = "MS81479brllaves")
    print(arch[j])
    
    # print(length(which(is.na(action$LLAVEPRIMARIA))))
    keys <- action %>% 
      select(CTACLIENTE=`Cuenta Cliente`,OPERACION=Operacion,MASCARA_CUENTA,
             MASCARA_OPERACION,LLAVEPRIMARIA) %>% 
      dplyr::filter(!is.na(LLAVEPRIMARIA)) %>% 
      distinct_all()
    keys_new <- keys %>% 
      distinct_all() %>% 
      dplyr::filter(!LLAVEPRIMARIA %in% masterKeyList$LLAVEPRIMARIA)
    masterKeyList <- masterKeyList %>% 
      bind_rows(keys_new)
    rep <- data.frame(Nom_arch = arch[j],
                      N_missing = length(which(is.na(action$LLAVEPRIMARIA))),
                      N_rows = nrow(keys),
                      N_unique = n_distinct(na.omit(keys$LLAVEPRIMARIA)),
                      N_new = nrow(keys_new))
    report <- report %>% 
      bind_rows(rep)
  }
}
)
write_rds(masterKeyList,'D:/!bso/accion/MasterKeyList_2018_2019.rds')
write_rds(report,'D:/!bso/accion/reportMasterKey_2028_2019.rds')
#For single month
write_rds(keys_new, 'D:/!bso/accion/MasterKeyList_Abril2023.rds')
####____MASTER KEY LIST____####
masterKeyList <- readRDS("D:/!bso/accion/MasterKeyList.rds")
month <- c("enero","febrero","marzo","abril","mayo","junio","julio",
           "agosto","septiembre","octubre","noviembre","diciembre")
mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
year <- c(2020:2023)
myutf <- as.vector(sapply(year, function(x){paste0(x,mes)}))
myxlsx <- as.vector(sapply(year, function(x){paste0(mes,'_Etapa1_',month,x)}))

infoList <- list()
# file_list <- list.files(path='//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/califClientes/rds/')
reporte <- data.frame(fbase = as.character(NULL),
                      N_MASKED_CTA = as.numeric(),
                      N_UNMASKED_CTA = as.numeric(),
                      N_unique_CI = as.numeric(),
                      N_unique_CTA = as.numeric(),
                      OP_NOT_FOUND = as.numeric())
for (i in 1:length(myutf)) {
  tryCatch({
    print(myutf[[i]])
    infoRaw <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/califClientes/rds/BSO',
                              myutf[i],'.rds')) %>% 
      mutate(CI = paste0(`NRO DOCUMENTO`, EXT)) %>% 
      mutate(opBSO_ = ifelse(`SIGLA SBEF` == 'BSO', NumeroOp, '-')) %>% 
      mutate(dBSO_ = ifelse(`SIGLA SBEF` == 'BSO' & str_detect(`TIPO OBLIGADO SBEF`, 'A -'), 1, 0)) %>%
      mutate(isBSO_ = ifelse(`SIGLA SBEF` == 'BSO', 1, 0)) %>%
      group_by(CI) %>% 
      mutate(dBSO_total = sum(dBSO_),
             isBSO = max(isBSO_),
             isBSO_total = sum(isBSO_)) %>%
      ungroup() %>% 
      mutate(idBSO = opBSO_) %>%
      separate(opBSO_,into = c("CTACLIENTE_all","OPERACION")) %>% 
      # separate_wider_delim(opBSO_, delim = '-',
      #                      names = c('CTACLIENTE_all', 'OPERACION'),
      #                      too_few = 'align_start',
      #                      too_many = 'merge') %>% 
      mutate(CTACLIENTE_d_ = ifelse(`SIGLA SBEF` == 'BSO' & str_detect(`TIPO OBLIGADO SBEF`, 'A -'), 
                                    as.integer(CTACLIENTE_all), 0)) %>% 
      group_by(CI) %>% 
      mutate(CTACLIENTE = max(CTACLIENTE_d_, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(CTACLIENTE = as.integer(CTACLIENTE),
             OPERACION = as.integer(OPERACION)) %>% 
      mutate(ESTADO = case_when(`SBEF VIGENTE` > 0 ~ 'VIGENTE',
                                `SBEF VENCIDO` > 0 ~ 'VENCIDO',
                                `SBEF EJECUCION` > 0 ~ 'EJECUCION',
                                `SBEF CASTIGADO` > 0 ~ 'CASTIGADO',
                                TRUE ~ 'NA'),
             SALDO = `SBEF VIGENTE` + `SBEF VENCIDO` + `SBEF EJECUCION`) %>% 
      select(CTACLIENTE, OPERACION, CI, HISTORICO, 
             `FECHA NAC`, LOCALIDAD, CANTON, SECCION, PROVINCIA, DEPARTAMENTO,
             `FECHA DECLARACION`, `ENTIDAD SBEF`,`SIGLA SBEF`, 
             FECHA, `TIPO OBLIGADO SBEF`, `TIPO CREDITO SBEF`, `FECHA INICIO OPERACION`,
             MonedaOrigen, MontoOriginal, `SBEF CALIFICACION`, SALDO, `SBEF CONTINGENTE`,
             DiasMora, FECHAVTO, `PERIODO PAGO`, idBSO) %>%
      # mutate(HISTORICO = round(as.double(HISTORICO))) %>% 
      mutate(HISTORICO = as.character(HISTORICO)) %>% 
      mutate(fechaReporte = myutf[[i]]) %>% 
      dplyr::filter(CTACLIENTE!=0)
    
    bdc <- readxl::read_xlsx(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/Entrega_1_12abr/Entrega_ti/all/',
                                    myxlsx[i],'.xlsx'),sheet = "MS81479brllaves") 
    
    maskcta <- bdc %>% 
      select(CTACLIENTE=`Cuenta Cliente`,MASCARA_CUENTA) %>% 
      dplyr::filter(!is.na(MASCARA_CUENTA)) %>% 
      distinct_all()
    maskope <- bdc %>% 
      select(CTACLIENTE=`Cuenta Cliente`,OPERACION=Operacion,MASCARA_OPERACION,LLAVEPRIMARIA) %>% 
      dplyr::filter(!is.na(LLAVEPRIMARIA)) %>% 
      distinct_all()
    
    infoOpMask <- infoRaw %>% 
      dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`,"A - ") & `SIGLA SBEF`=="BSO") %>% 
      left_join(maskope,by=c("CTACLIENTE","OPERACION"))
    
    rep <- data.frame(fbase = myutf[i],
                      N_MASKED_CTA = n_distinct(maskcta$CTACLIENTE),
                      N_UNMASKED_CTA = n_distinct(maskcta$MASCARA_CUENTA),
                      N_unique_CI = n_distinct(infoRaw$CI),
                      N_unique_CTA  = n_distinct(infoRaw$CTACLIENTE),
                      OP_NOT_FOUND = length(which(is.na(infoOpMask$LLAVEPRIMARIA))))
    reporte <- reporte %>% 
      bind_rows(rep)
    
    infoExp <- infoRaw %>% 
      left_join(maskcta,by="CTACLIENTE") %>% 
      left_join(maskope,by=c("CTACLIENTE","OPERACION")) %>% 
      select(-CTACLIENTE,-OPERACION,-CI)
    
    write_excel_csv(infoExp,paste0('D:/!bso/accion/info/BSO_masked_',myutf[i],'.csv'))
    write_rds(infoExp,paste0('D:/!bso/accion/info/BSO_masked_',myutf[i],'.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

write_rds(reporte,"D:/!bso/accion/reporteInfoMask.rds")
