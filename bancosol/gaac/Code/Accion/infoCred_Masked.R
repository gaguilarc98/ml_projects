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
####___RDS INFOCRED____####
infoCred <- fread("D:/!bso/califClientes/utf/BSO202303_utf8.txt",encoding = "UTF-8",sep = "|")

saveRDS(infoCred, '//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/califClientes/rds/BSO202303.rds')
####____ENMASCARANDO INFOCRED____####

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
myutf <- "202303"
myxlsx <- c("03_Etapa1_marzo2023")
for (i in 1:length(myutf)) {
  tryCatch({
    print(myutf[[i]])
    infoRaw_old <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/califClientes/rds/BSO',
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
             fechaNacimiento=`FECHA NAC`, LOCALIDAD, CANTON, SECCION, PROVINCIA, DEPARTAMENTO,
             fechaDeclaracion=`FECHA DECLARACION`, Entidad_SF=`ENTIDAD SBEF`, Sigla_Entidad_SF=`SIGLA SBEF`, 
             FECHA, tipoObligado=`TIPO OBLIGADO SBEF`, tipoCredito=`TIPO CREDITO SBEF`, fechaInicio=`FECHA INICIO OPERACION`,
             MonedaOrigen, MontoOriginal, Calificacion_SF=`SBEF CALIFICACION`, SALDO, Saldo_USD_Contingente=`SBEF CONTINGENTE`,
             DiasMora, FECHAVTO, Periodo_de_Pago=`PERIODO PAGO`, 
             Saldo_USD_Vigente = `SBEF VIGENTE`, Saldo_USD_Vencido = `SBEF VENCIDO`,
             Saldo_USD_Ejecucion = `SBEF EJECUCION`, Saldo_USD_Castigado = `SBEF CASTIGADO`) %>%
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
      dplyr::filter(str_detect(tipoObligado,"A - ") & Sigla_Entidad_SF=="BSO") %>% 
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
    
write_excel_csv(infoExp,paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/accion/Entrega_2_23may/deliver_Buro_tabla_4/BSO_masked_',myutf[i],'.csv'))
    # write_rds(infoExp,paste0('D:/!bso/accion/info/BSO_masked_',myutf[i],'.rds'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
