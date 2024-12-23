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
library(stringr) # Working with strings
library(forcats) 
library(scales)
library(janitor)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____CIC COLUMN NAMES____####
sheets <- excel_sheets("D:/!bso/bases/excel/MetaData_CIC.xlsx")
CICnames <- list()
for (i in 1:length(sheets)) {
  CICnames[[i]] <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = sheets[i]) %>% 
    select(2) #Hasta col 8 para 2018
}
names(CICnames) <- sheets
as.vector(CICnames$CR_K)
####____CIC FIELD CODES____####
codsheets <- excel_sheets("D:/!bso/bases/excel/codCIC.xlsx")
CICcodes <- list()
for (i in 1:length(codsheets)) {
  CICcodes[[i]] <- readxl::read_excel("D:/!bso/bases/excel/codCIC.xlsx",sheet = codsheets[i]) 
}
names(CICcodes) <- codsheets
as.vector(CICcodes$RPT004)

####____OPERACIONES CANCELADAS Y DESEMBOLSADAS____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2018:2023)
myrds <- sapply(year, function(x){paste0(mes, x)})
i <- 65
Cancel <- list()
Disb <- list()
for (i in 1:length(myrds)) {
  tryCatch({
    print(myrds[i])
    cic <- readRDS(paste0("D:/!bso/CIC/rds/cic_",myrds[i],".rds")) 
    # cicCancel <- cic%>% 
    #   dplyr::filter(!is.na(TipoCancelacion)) %>% 
    #   remove_empty("cols")
    # Cancel[[i]] <- cicCancel
    
    cicDisb <- cic %>% 
      dplyr::filter(as.yearmon(FechaInicio)==paste0(substr(myrds[i],1,3),'. ',substr(myrds[i],4,7))) %>% 
      remove_empty("cols")
    Disb[[i]] <- cicDisb
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
CancelFull <- rbindlist(Cancel, fill = TRUE) %>% 
  select(-ESTADO)
DisbFull <- rbindlist(Disb, fill = TRUE) 
saveRDS(CancelFull,"//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/cic/tables/cicCancel.rds")
saveRDS(DisbFull,"//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/cic/tables/cicDisbursement.rds")

####___ACTUALIZACION MENSUAL CANCELADOS Y DESEMBOLSADOS____####
CancelFull <- readRDS("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/cic/tables/cicCancel.rds")
DisbFull <- readRDS("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/cic/tables/cicDisbursement.rds")

my <- "Oct. 2023"
myrds <- str_replace(my, ". ","")
cic <- readRDS(paste0("D:/!bso/CIC/rds/cic_",myrds,".rds")) 
cicCancel <- cic%>%
  dplyr::filter(!is.na(TipoCancelacion)) %>%
  remove_empty("cols")

cicDisb <- cic %>% 
  dplyr::filter(as.yearmon(FechaInicio)==paste0(substr(myrds,1,3),'. ',substr(myrds,4,7))) %>% 
  remove_empty("cols")

tail(CancelFull %>% group_by(monDate) %>% summarize(N=n()))
tail(DisbFull %>% group_by(monDate) %>% summarize(N=n()))
CancelFull <- CancelFull %>% 
  bind_rows(mutate(cicCancel, CredProdNoProd=as.character(CredProdNoProd)))
DisbFull <- DisbFull %>% 
  bind_rows(mutate(cicDisb, CredProdNoProd=as.character(CredProdNoProd)))

saveRDS(CancelFull,"//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/cic/tables/cicCancel.rds")
saveRDS(DisbFull,"//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/cic/tables/cicDisbursement.rds")
####____SOLICITUD PEDORRA DE JM____####
DisbSegment <- DisbFull %>% 
  dplyr::filter(FechaCorte>=as.Date("2022-01-01") & FechaCorte<=as.Date("2023-10-31"))
write_xlsx(DisbSegment, "D:/!bso/requests/Desembolsos_Ene2022Oct2023.xlsx")
####____TABLAS DE GARANTES Y CODEUDORES____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2018:2023)
myrds <- sapply(year, function(x){paste0(mes, x)})
i <- 66
Cancel <- list()
for (i in 1:length(myrds)) {
  tryCatch({
    print(myrds[i])
    cicCancel <- readRDS(paste0("D:/!bso/CIC/rds/cic_",myrds[i],".rds")) %>% 
      dplyr::filter(!is.na(TipoCancelacion)) %>% 
      remove_empty("cols")
    Cancel[[i]] <- cicCancel
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
CancelFull <- rbindlist(Cancel, fill = TRUE) %>% 
  select(-ESTADO)

saveRDS(CancelFull,"//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/cic/tables/cicCancel.rds")

####____TABLAS AGRUPADAS____####
#CIC-G
cicg <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"G.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
#CIC-E
cice <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"E.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
####____CIC MONTHLY UPDATE____####
workdir <- "C:/CIC/"
month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
mes <- str_pad(1:12,width=2,side = 'left',pad='0')
year <- c(2018:2023)

monyear <- as.vector(sapply(year,function(x){paste0(month,x)}))
mycic <- as.vector(sapply(year,function(x){paste0(month,'. ',x)}))
mycic <- str_replace_all(as.Date(as.yearmon(mycic),frac=1),"-","")
mycic <- mycic[-c(which(mycic=="20230930"):length(mycic))]
i <- 60
38:64

for (i in 65:67) {
  tryCatch({
    #CIC-K OPERACIONES: Información específica de la operación como lugar tasas, montos, fechas clave, días de gracia
    print(mycic[i])
    cic <- readRDS(paste0("D:/!bso/CIC/rds/cic_",monyear[i],".rds"))
    
    #CIC-P OBLIGADOS: Información específica de los obligados de la operación como nombre, genero, nacimiento, ci, act econ, id interno 
    cicp <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"P.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cicp) <- CICnames$CR_P$NAME[1:ncol(cicp)]
    #CR-M CALIFICACION: Calificación del obligado
    cicm <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"M.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cicm) <- CICnames$CR_M$NAME[1:ncol(cicm)]
    #CIC-B CPOP: Beneficio C-POP
    cicb <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"B.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cicb) <- CICnames$CR_B$NAME[1:ncol(cicb)]
    
    cicb_processed <- cicb %>%
      # left_join(CICcodes$RPT159, by=c("CodigoBeneficio")) %>% 
      group_by(IdOperacion) %>% 
      arrange(FechaBeneficio) %>% 
      summarise(FechaBeneficio  = min(FechaBeneficio),
                HistorialBeneficio = paste(unique(CodigoBeneficio), collapse = "")) %>% 
      ungroup()
    
    cicm_processed <- cicm %>% 
      select(IdObligado, Calificacion=CodCalificacion, FechaCalificacion) 
    
    cicp_processed <- cicp %>% 
      left_join(cicm_processed, by="IdObligado") %>% 
      left_join(cicb_processed, by="IdOperacion") %>% 
      left_join(CICcodes$RPT049, by=c("CodTipoIdentificacion")) %>% 
      left_join(CICcodes$RPT037, by=c("CodTipoPersona")) %>% 
      left_join(CICcodes$RPT004, by=c("CodActividadEconomica")) %>% 
      left_join(CICcodes$RPT043, by=c("GrupoActEcon")) %>% 
      left_join(CICcodes$RPT040, by=c("CodTipoRelacion")) %>% 
      left_join(CICcodes$RPT039, by=c("CodTipoGarantia")) %>% 
      left_join(CICcodes$RPT201, by=c("CodGeneracionIngresos")) %>% 
      mutate(CI = paste0(NroRaizCedula, ComplementoCedula, LugarEmisionCedula)) %>% 
      mutate(Nombre = paste(Nombre, PrimerApellido, SegundoApellido, ApellidoEsposo)) %>% 
      mutate(across(c(CI,Nombre),~str_trim(.x))) %>% 
      dplyr::filter(CodTipoRelacion %in% c("02","1B")) %>% 
      select(IdOperacion, IdObligado, TipoIdentificacion, TipoPersona, NombreRazonSocial, 
             CodActEconOri = CodActividadEconomica, ActEconOri=ActividadEconomica,
             GrupoActEcoOri=GrupoActEcon, TipoObligado, PorcentajeCompromiso,
             MontoPatrimonio, GeneracionIngresos, Genero=CodGenero, FechaNacimiento, 
             IndiceActEcon=IndiceActividadEconomica,
             CI, IdObligadoAnterior, Nombre, Activo, Patrimonio, IngresosVentasServicios, 
             PersonalOcupado, FechaBeneficio, HistorialBeneficio, 
             Calificacion, FechaCalificacion, any_of(c('NroIdentInterno'))) %>% 
      separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                           too_many = "drop",too_few = "align_start") %>% 
      mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x)))
    
    cicCod <- cicp_processed %>% 
      left_join(cic, by=c("CTACLIENTE","OPERACION"), suffix=c("","repeat")) %>% 
      select(-ends_with("repeat"))
      
    
    saveRDS(cicCod, paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/cic/tables/codgar_",monyear[i],".rds"))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

####____LÓGICA DE CANCELADAS####
old <- "Dic2022"
new <- "Ene2023"
cicPrev <- readRDS(paste0("D:/!bso/CIC/rds/cic_",old,".rds")) 
cicNew <- readRDS(paste0("D:/!bso/CIC/rds/cic_",new,".rds")) 
cicAJ <- cicPrev %>% anti_join(cicNew, by=c("CTACLIENTE","OPERACION"))

bdcPrev <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",old,".rds")) 
bdcNew <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",new,".rds")) 
bdcAJ <- bdcPrev %>% anti_join(bdcNew, by=c("CTACLIENTE","OPERACION"))

cicDis <- cicNew %>% 
  dplyr::filter(as.yearmon(FechaInicio)=="Mar. 2023")
bdcDis <- bdcNew %>% 
  dplyr::filter(as.yearmon(fdes)=="Mar. 2023" 
                & !(ctaCont %in% c('135','136','137'))) %>% 
  dplyr::filter(OPERACION_ORI_REF!=OPERACION)

cicDisJoin<- cicDis %>% 
  left_join(bdcJoin, by=c("CTACLIENTE", "OPERACION"))


y <- cicNew %>% semi_join(x, by=c("CTACLIENTE","OPERACION"))
z <- bdcNew %>% semi_join(x, by=c("CTACLIENTE","OPERACION"))

length(which(!is.na(cicNew$TipoCancelacion)))==nrow(bdcAJ)
length(which(!is.na(cicPrev$TipoCancelacion)))==nrow(bdcAJ)
cicCancel <- cicNew %>% dplyr::filter(!is.na(TipoCancelacion))

x <- bdcAJ %>% anti_join(cicCancel, by=c("CTACLIENTE","OPERACION"))

####____OPERACIONES DESEMBOLSADAS____####
new <- "May2023"
cicNew <- readRDS(paste0("D:/!bso/CIC/rds/cic_",new,".rds")) 
bdcNew <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",new,".rds")) 

cicDis <- cicNew %>% 
  dplyr::filter(as.yearmon(FechaInicio)=="May. 2023")
bdcDis <- bdcNew %>% 
  dplyr::filter(as.yearmon(fdes)=="May. 2023" 
                & !(ctaCont %in% c('135','136','137'))) %>% 
  # dplyr::filter(MODULO!=118) %>% 
  dplyr::filter(OPERACION_ORI_REF!=OPERACION)
bdcDis %>% 
  dplyr::filter(ctaCont!='623') %>% 
  mutate(MontoDes = ifelse(MONEDA==0, MONTO/6.86, MONTO)) %>% 
  summarise(MontoDes = sum(MontoDes))
cicDisb %>% 
  dplyr::filter(CuentaContable.y!='623') %>%
  summarise(Monto=sum(MontoContratado), n_distinct(OPERACION))
cicDisJoin<- cicDis %>% 
  left_join(bdcDis, by=c("CTACLIENTE", "OPERACION")) %>% 
  mutate(Fbin = FechaInicio==fdes)
table(cicDisJoin$Fbin, useNA = "always")