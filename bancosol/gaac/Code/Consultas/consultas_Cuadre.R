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
library(openxlsx)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

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
paleta <- colorRampPalette(c("navyblue","steelblue4","dodgerblue4","darkorchid4","purple3",
                             "slateblue3","red2","orange","yellowgreen","yellow2","pink3","snow3"),bias=1.25)
####____CONSOLIDADO DE CONSULTAS_____####
# nmes <- c("12","11","10","09","08","07","06","05","04","03","02","01")
nmes <- c("06","07","08","09")
# year <- c(17,18,19,20,21,22,23)
year <- c(23)
mycon <- as.vector(sapply(year,function(x){paste0(nmes,x)}))
# mycon <- mycon[-1]
conList <- list()
k <- 1
for (my in 1:length(mycon)) {
  arch <- list.files("D:/!bso/Consultas/conxls/",
                     pattern = paste0("^VAR7.*.",mycon[my],".xls$"))
  arch <- sort(arch)
  for (i in 1:length(arch)) {
    tryCatch({
      print(arch[i])
      con <- read_excel(paste0("D:/!bso/Consultas/conxls/",arch[i]),
                        sheet="inf",col_names=c("TIPO","NDOC","EXT","CLIENTE","FNAC","ENTIDAD","FCON")) %>% 
        select(-FNAC) 
      k <- k+1
      print(k)
      conList[[k]] <- con
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

conFull2 <- rbindlist(conList) %>%
  # dplyr::filter(TIPO=="CI") %>%
  mutate(FechaConsulta = as.Date(FCON)) %>% 
  dplyr::filter(FechaConsulta <= as.Date("2023-08-31") 
                & FechaConsulta>=as.Date("2023-07-01")) %>% 
  rename(CI = NDOC) %>%
  mutate(ENTIDAD = str_replace_all(ENTIDAD,"\\.","")) %>% 
  # mutate(NDOC = str_replace(CI, "LP|OR|PO|CB|CH|TJ|SC|BE|PA", "")) %>% 
  group_by(FechaConsulta, ENTIDAD, CI) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  group_by(FechaConsulta, ENTIDAD) %>% 
  summarise(ClientesConsultado = n_distinct(CI),
         Clientes = n())

conFull <- rbindlist(conList) %>%
  # dplyr::filter(TIPO=="CI") %>%
  mutate(FechaConsulta = as.Date(FCON)) %>% 
  dplyr::filter(FechaConsulta <= as.Date("2023-08-31") 
                & FechaConsulta>=as.Date("2023-07-01")) %>% 
  rename(CI = NDOC) %>%
  mutate(ENTIDAD = str_replace_all(ENTIDAD,"\\.","")) %>% 
  mutate(NDOC = str_replace(CI, "LP|OR|PO|CB|CH|TJ|SC|BE|PA", "")) %>% 
  mutate(myCon = as.yearmon(FechaConsulta)) %>% 
  group_by(myCon, ENTIDAD, NDOC) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  group_by(myCon, ENTIDAD) %>% 
  summarise(ClientesConsultado = n_distinct(NDOC),
            Clientes = n())

conFull %>% group_by(myCon) %>% summarise(C=sum(Clientes))
conFull <- conFull %>% mutate(myCon = as.Date(myCon, frac=1))
write_xlsx(conFull,"D:/!bso/Consultas/ConsultasDiarias_Jul2023Ago2023_v2.xlsx")
#BRUTOS COMPILADOS

conFull_raw <- rbindlist(conList) %>% 
  mutate(Yearconsulta = year(FCON)) %>% 
  mutate(Monthconsulta = month(FCON))
write_xlsx(conFull_raw,"D:/!bso/Consultas/ConsultasRaw_Jul2023Ago2023_v3.xlsx")


bdcJul <- readRDS("D:/!bso/CIC/rds/cic_Jul2023.rds") %>% 
  select(CTACLIENTE, OPERACION, CI, CuentaContable.y) %>% 
  mutate(NDOC = str_replace(CI, "LP|OR|PO|CB|CH|TJ|SC|BE|PA", ""))

bdcX <- bdcJul %>% 
  select(NDOC) %>% 
  distinct_all()

conRaw_join <- conFull_raw %>% 
  mutate(NDOC = str_replace(NDOC, "LP|OR|PO|CB|CH|TJ|SC|BE|PA", "")) %>% 
  dplyr::filter(FCON <= as.Date("2023-07-31") 
                & FCON >=as.Date("2023-07-01")) %>% 
  inner_join(bdcX, by=c("NDOC"))

x <- conRaw_join %>% group_by(ENTIDAD) %>% summarise(Cl=n_distinct(NDOC))

####UNA SOLA ENTIDAD POR CLIENTE CONSULTADO####

conFull <- rbindlist(conList) %>%
  # dplyr::filter(TIPO=="CI") %>%
  mutate(FechaConsulta = as.Date(FCON)) %>% 
  dplyr::filter(FechaConsulta <= as.Date("2023-08-31") 
                & FechaConsulta>=as.Date("2023-07-01")) %>% 
  rename(CI = NDOC) %>%
  mutate(ENTIDAD = str_replace_all(ENTIDAD,"\\.","")) %>% 
  mutate(NDOC = str_replace(CI, "LP|OR|PO|CB|CH|TJ|SC|BE|PA", "")) %>% 
  mutate(myCon = as.yearmon(FechaConsulta)) %>% 
  group_by(myCon, NDOC) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  group_by(myCon, ENTIDAD) %>% 
  summarise(ClientesConsultado = n_distinct(NDOC),
            Clientes = n())
####____CIC DEUDORES Y CODEUDORES____####
conFull <- rbindlist(conList) %>%
  mutate(FechaConsulta = as.Date(FCON)) %>% 
  dplyr::filter(FechaConsulta <= as.Date("2023-07-31") 
                & FechaConsulta>=as.Date("2023-07-01")) %>% 
  rename(CI = NDOC) %>%
  mutate(ENTIDAD = str_replace_all(ENTIDAD,"\\.","")) %>% 
  mutate(NDOC = str_replace(CI, "LP|OR|PO|CB|CH|TJ|SC|BE|PA", "")) %>%
  mutate(myCon = as.yearmon(FechaConsulta)) %>% 
  group_by(myCon, ENTIDAD, NDOC) %>% 
  arrange(desc(FCON)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() 

%>% 
  group_by(myCon, ENTIDAD) %>% 
  summarise(ClientesConsultado = n_distinct(NDOC),
            Clientes = n())

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
####____CIC OBLIGADOS____####
mycic <- "20230731"
#CIC-P OBLIGADOS: Información específica de los obligados de la operación como nombre, genero, nacimiento, ci, act econ, id interno 
cicp <- fread(paste0("C:/CIC/",mycic,"/CR",mycic,"P.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
names(cicp) <- CICnames$CR_P$NAME[1:ncol(cicp)]

cicp_processed <- cicp %>% 
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
  mutate(esDeudor = ifelse(CodTipoRelacion %in% c("1A","4A","5A"),1,0)) %>% 
  mutate(esCodeudor = ifelse(CodTipoRelacion=="1B",1,0)) %>% 
  mutate(esGarante = ifelse(CodTipoRelacion=="02",1,0)) %>% 
  select(NroRaizCedula, esDeudor, esCodeudor, esGarante) %>% 
  group_by(NroRaizCedula) %>% 
  summarise_all(max) %>% 
  mutate(TipoDeudor = case_when(max(esDeudor)==1 ~ 'Deudor',
                                max(esCodeudor)==1 ~ 'Codeudor',
                                max(esGarante)==1 ~ 'Garante',
                                TRUE ~ 'Otros')) %>% 
  ungroup()

cicp_processed <- cicp_processed %>% 
  select(NroRaizCedula, TipoDeudor)

cicp_check <- cicp_processed %>% 
  dplyr::filter(!is.na(CI)) %>% 
  group_by(CI) %>% 
  dplyr::filter(n_distinct(TipoDeudor)>1) %>% 
  arrange(CI)

#JOIN CON CONSULTAS
conFull_Join <- conFull %>% 
  mutate(NDOC = as.numeric(NDOC)) %>% 
  anti_join(cicp_processed, by=c("NDOC"="NroRaizCedula")) 

conFull_CI <- conFull %>% 
  mutate(NDOC = str_replace(NDOC,"^E-","")) %>% 
  mutate(NDOC = str_replace(NDOC,"^0+",""))
  