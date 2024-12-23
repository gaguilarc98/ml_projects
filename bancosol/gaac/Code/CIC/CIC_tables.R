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
####____APERTURA DE ARCHIVO CIC____####
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
year <- c(2018:2023)
for (k in year) {
  for (i in month) {
    dir.create(paste0("C:/CIC/",k,i))
  }
}
workdir <- "C:/CIC/202301/"
workdir <- "//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/13_CIC/CIC 2023/CIRC ENVIO 2023.01 ENERO/"
#CIC-K OPERACIONES: Información específica de la operación como lugar tasas, montos, fechas clave, días de gracia
namesk <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-K") %>% 
  select(1:2)

cick <- fread(paste0(workdir,"CR20230131K.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = namesk$NAME)

#CIC-P OBLIGADOS: Información específica de los obligados de la operación como nombre, genero, nacimiento, ci, act econ, id interno 
namesp <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-P") %>% 
  select(1:2)

cicp <- fread(paste0(workdir,"CR20230131P.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)

#CR-H INFORMACIÓN COMPLEMENTARIA DE LA OPERACION: Información cualitativa de los créditos
namesh <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-H") %>% 
  select(1:2)

cich <- fread(paste0(workdir,"CR20180131H.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = namesh$NAME)

#CR-M CALIFICACION: Calificación del obligado
namesm <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-M") %>% 
  select(1:2)

cicm <- fread(paste0(workdir,"CR20230131M.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = namesm$NAME)

#CR-L GARANTIAS: Calificación del obligado
namesl <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-L") %>% 
  select(1:2)

cicl <- fread(paste0(workdir,"CR20230131L.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = namesl$NAME)

#CIC-B CPOP: Beneficio C-POP
namesb <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-B") %>% 
  select(1:2)

cicb <- fread(paste0(workdir,"CR20230131B.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = namesb$NAME)

#CR-O CUENTA CONTABLE: Saldo por cuenta contable  y moneda de todas las operaciones
nameso <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-O") %>% 
  select(1:2)

cico <- fread(paste0(workdir,"CR20230131O.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = nameso$NAME)

#CIC-A  REPORTE DE CUOTAS DE CRÉDITOS CON RETRASO EN EL PAGO: Días de retraso
namesa <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CC-A") %>% 
  select(1:2)

cica <- fread(paste0(workdir,"CC20230131A.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = namesa$NAME)
#CIC-R  OPERACIONES GENERADAS EN EL PERÍODO: CodTipoGeneracion: 1 Refin de una o varias ops de la entidad
#2 Refin de una o varias ops en otras entidades, 3 Refin de una o varias ops en la entidad y otras, 4. Nueva operación
namesr <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-R") %>% 
  select(1:2)

cicr <- fread(paste0(workdr,"CR20230131R.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = namesr$NAME)
#CIC-U  OPERACIONES REFINANCIADAS: Origen, Saldo y Tipo de Refinanciamiento
namesu <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-U") %>% 
  select(1:2)

cicu <- fread(paste0(workdir,"CR20230131U.IBBSO"),
              encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = namesu$NAME)
####____TABLAS DECODIFICADORAS____####
cDpto <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT203") %>% 
  select(CodDpto = clclz, CodLocal = ccdad, Sucursal = tdcdd) %>% 
  mutate(CodLocal = str_remove(CodLocal,"^0+"))
cFormaPago <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT009") %>% 
  select(CodFormaPago = cfpgo, FormaPago = tsfpg)
cTipoInteres <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT036") %>% 
  select(CodTipoInteres = ctint, TipoInteres = tstin)
cTipoOperacion <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT035") %>% 
  select(CodTipoOperacion = ctop, TipoOperacion = tstop)
cTipoCancelacion <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT034") %>% 
  select(CodTipoCancelacion = ctccr, TipoCancelacion = tstcc)
cMonedaCuenta <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT019") %>% 
  select(CodMonedaCuenta = cmnda, Moneda = tsmnd)
cActividadEconomica <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT004") %>% 
  select(CodActividadEconomica = caecn, GrupoActEcon = cgaec)
cPlanPagos <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT015") %>% 
  select(CodPlanPagos = ctppg, PlanPagos = tdppg)
cObjetoCredito <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT139") %>% 
  select(CodObjetoCredito = cocre, ObjetoCredito = tsocr)

cTipoIdentificacion <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT049") %>% 
  select(CodTipoIdentificacion = ctidn, TipoIdentificacion = tsidn)
cTipoPersona <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT037") %>% 
  select(CodTipoPersona = ctprs, TipoPersona = tdtob)
cTipoRelacion <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT040") %>% 
  select(CodTipoRelacion = ctobl, TipoRelacion = tdtob)
cTipoGarantia <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT039") %>% 
  select(CodTipoGarantia = ctgrn, TipoGarantia = tdtgr)
cGeneracionIngresos <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT201") %>% 
  select(CodGeneracionIngresos = cging, GeneracionIngresos = tging)
cBeneficio <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT159") %>% 
  select(CodigoBeneficio, BeneficioCPOP = Descripcion)

cDepar <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT038") %>% 
  select(CodDpto = clclz, Departamento = tdlcl)
cTipoGarantia <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT039") %>% 
  select(CodTipoGarantia = ctgrn, TipoGarantia = tdtgr)
cReduccionGarantia <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT137") %>% 
  select(CodReduccionGarantia = crgrn, ReduccionGarantia = tdrgr)

####____TRATAMIENTO DE TABLAS____####
cick_processed <- cick %>% 
  separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                       too_many = "drop",too_few = "align_start") %>% 
  mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
  mutate(across(c(CodLocalidadSucursal, CodLocalidadOtorga),~as.character(.x))) %>% 
  left_join(cDpto, by=c("CodDptoSucursal"="CodDpto", "CodLocalidadSucursal"="CodLocal")) %>% 
  left_join(cFormaPago, by=c("CodFormaPago")) %>% 
  left_join(cTipoInteres, by=c("CodTipoInteres")) %>% 
  left_join(cTipoOperacion, by=c("CodTipoOperacion")) %>% 
  left_join(cTipoCancelacion, by=c("CodTipoCancelacion")) %>% 
  left_join(cMonedaCuenta, by=c("CodMonedaCuenta")) %>% 
  left_join(cActividadEconomica, by=c("CodActividadEconomica")) %>% 
  left_join(cPlanPagos, by=c("CodPlanPagos")) %>% 
  left_join(rename(cDpto, SucursalOtorga = Sucursal), by=c("CodDptoOtorga"="CodDpto", "CodLocalidadOtorga"="CodLocal")) %>% 
  left_join(cObjetoCredito, by=c("CodObjetoCredito")) %>% 
  mutate(TipoCredito = case_when(substr(CodTipoCredito,1,1)=='C' ~ "Empresarial",
                                 substr(CodTipoCredito,1,1)=='H' ~ "Vivienda",
                                 substr(CodTipoCredito,1,1)=='M' ~ "Micro",
                                 substr(CodTipoCredito,1,1)=='N' ~ "Consumo",
                                 substr(CodTipoCredito,1,1)=='P' ~ "Pyme",)) %>% 
  mutate(CtaCont = paste0(CodCapituloCuenta,CodGrupoCuenta,CodCuenta)) %>% 
  mutate(across(starts_with("Monto"),~ifelse(Moneda=='MN',as.numeric(.x)/6.86,as.numeric(.x)))) %>% 
  select(FechaCorte, FechaInicio, CTACLIENTE, OPERACION, Sucursal, FechaReprogramacion, 
         CantidadReprogramaciones, CantidadDiasGracia, TasaInteres, FechaVencimiento, 
         MontoContratado, MontoComputable, FormaPago, CantidadDiasPlazo, TipoInteres, 
         TipoOperacion, TipoCredito, TipoCancelacion, CantidadCuotas, CtaCont, 
         SubCtaCont = CodSubCuenta, Moneda, FechaCancelacion, GrupoActEconDest = GrupoActEcon, 
         PlanPagos, MontoCuotaFija, FechaVencePrimeraCuota, FechaIncumplimiento, 
         AnioInicioLineaCarta, SucursalOtorga, ObjetoCredito, IngresosFinancieros, 
         MontoComputableNoDiferido)

cicp_processed <- cicp %>% 
  separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                       too_many = "drop",too_few = "align_start") %>% 
  mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
  left_join(cTipoIdentificacion, by=c("CodTipoIdentificacion")) %>% 
  left_join(cTipoPersona, by=c("CodTipoPersona")) %>% 
  left_join(cActividadEconomica, by=c("CodActividadEconomica")) %>% 
  left_join(cTipoRelacion, by=c("CodTipoRelacion")) %>% 
  left_join(cTipoGarantia, by=c("CodTipoGarantia")) %>% 
  left_join(cGeneracionIngresos, by=c("CodGeneracionIngresos")) %>% 
  mutate(CI = paste0(NroRaizCedula, ComplementoCedula, LugarEmisionCedula)) %>% 
  mutate(Nombre = paste(Nombre, PrimerApellido, SegundoApellido, ApellidoEsposo)) %>% 
  select(FechaCorte, FechaInicio, CTACLIENTE, OPERACION, IdObligado, TipoIdentificacion,
         TipoPersona, NombreRazonSocial, GrupoActEconOrigen = GrupoActEcon, TipoRelacion, 
         TipoGarantia, PorcentajeCompromiso, MontoPatrimonio, GeneracionIngresos, 
         IndiceActividadEconomica, CI, Nombre, IdObligadoAnterior, Activo, Patrimonio, 
         IngresosVentasServicios, PersonalOcupado, NroIdentInterno)

cich_processed <- cich %>% 
  separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                       too_many = "drop",too_few = "align_start") %>% 
  mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
  left_join(cMonedaCuenta, by=c("CodMoneda"="CodMonedaCuenta")) %>% 
  select(FechaCorte, FechaInicio, CTACLIENTE, OPERACION, Moneda, TipoCambio, MontPromCuotaVariab,
         FechaPrimerPagoCap, FechaPrimerPagoInt, FechaProxPagoCap, FechaProxPagoInt,
         FechaUltPagoCap, FechaUltPagoInt, TipoProducto, Asesor = NombreOficialResp,
         NivelAutorizacion, CodPondActivos, PrevCiclica, PromDiasAtraso, IntSuspenso,
         CredProdNoProd, CuotasDiferidas, MontCapitDiferido, MontoInteresDiferido)
  
cico_processed <- cico %>% 
  separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                       too_many = "drop",too_few = "align_start") %>% 
  mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
  left_join(cMonedaCuenta, by=c("CodMoneda"="CodMonedaCuenta")) %>% 
  mutate(CtaCont = paste0(CodCapitulo,CodGrupo,CodCuenta)) %>% 
  select(FechaCorte, FechaInicio, CTACLIENTE, OPERACION, CtaCont, SubCtaCont = CodSubCuenta,
         Moneda, FechaIngresoEstado, MontoSaldo)

cicm_processed <- cicm %>% 
  select(FechaCorte, IdObligado, CodCalificacion, FechaCalificacion)

cicb_processed <- cicb %>% 
  separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                       too_many = "drop",too_few = "align_start") %>% 
  mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
  left_join(cBeneficio, by=c("CodigoBeneficio")) %>% 
  left_join(rename(cBeneficio, BeneficioInicial = BeneficioCPOP), by=c("BeneficioInicial"="CodigoBeneficio")) %>% 
  select(FechaCorte, FechaInicio, CTACLIENTE, OPERACION, BeneficioCPOP, BeneficioInicial, FechaBeneficio)

cicl_processed <- cicl %>% 
  separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                       too_many = "drop",too_few = "align_start") %>% 
  mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
  left_join(rename(cDepar, DptoGarantia = Departamento), by=c("CodDptoGarantia"="CodDpto")) %>% 
  left_join(cTipoGarantia, by=c("CodTipoGarantia")) %>%
  left_join(cReduccionGarantia, by=c("CodReduccionGarantia")) %>%
  left_join(cMonedaCuenta, by=c("CodMoneda"="CodMonedaCuenta")) %>% 
  select(FechaCorte, FechaInicio, CTACLIENTE, OPERACION, DptoGarantia, TipoGarantia,
         ReduccionGarantia, MonedaGarantia = Moneda, MontoGarantiaOtras,
         MontoGarantiaEntidad, MontoGarantiaNeto, CodEnvioFondoGarantia)

cica_processed <- cica %>% 
  separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                       too_many = "drop",too_few = "align_start") %>% 
  mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
  select(FechaCorte, CTACLIENTE, OPERACION, NombreRazonSocial, NumeroCuota, FechaProgramada, 
         FechaPago, DiasRetraso)

#ARREGLAR TABLA R
cicr_processed <- cicr %>% 
  separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                       too_many = "drop",too_few = "align_start") %>% 
  mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
  select(FechaCorte, FechaInicio, CTACLIENTE, OPERACION, CodTipoGeneración,
         MontoAdicional, MontoTotal, NumeroOperaciones)

#ARREGLAR TABLA U
cicu_processed <- cicu %>% 
  separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                       too_many = "drop",too_few = "align_start") %>% 
  separate_wider_delim(IdOperacionOrigen, names = c("CTACLIENTE_ORI_REF","OPERACION_ORI_REF"), delim = "-",
                       too_many = "drop",too_few = "align_start") %>% 
  mutate(across(c(CTACLIENTE, OPERACION, CTACLIENTE_ORI_REF, OPERACION_ORI_REF),~as.numeric(.x))) %>% 
  select(FechaCorte, FechaInicio, CTACLIENTE, OPERACION, OPERACION_ORI_REF, FechaInicio, 
         FechaInicioOrigen, SaldoOperacion, TipoRefinanciamiento, FechaRefinanciamiento, MontoAdicional)

####____2018: No posee la variable 49 MontoComputableNoDiferido
####____2019: 
####____READING CIC NAMES____####
namesk <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-K") %>% 
  select(1:2) #Hasta col 48 para 2018
namesp <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-P") %>% 
  select(1:2) #Hasta col 31 para 2018
namesh <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-H") %>% 
  select(1:2)
namesm <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-M") %>% 
  select(1:2)
namesl <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-L") %>% 
  select(1:2)
namesb <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-B") %>% 
  select(1:2)
nameso <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-O") %>% 
  select(1:2)
namesa <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CC-A") %>% 
  select(1:2)
namesr <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-R") %>% 
  select(1:2)
namesu <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-U") %>% 
  select(1:2) #Hasta col 8 para 2018
sheets <- excel_sheets("D:/!bso/bases/excel/MetaData_CIC.xlsx")
CICnames <- list()
for (i in 1:length(sheets)) {
  CICnames[[i]] <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = sheets[i]) %>% 
    select(2) #Hasta col 8 para 2018
}
names(CICnames) <- sheets
as.vector(CICnames$CR_K)
####____CIC MONTHLY UPDATE____####
workdir <- "C:/CIC/"
month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
mes <- str_pad(1:12,width=2,side = 'left',pad='0')
year <- c(2018:2023)

mycic <- as.vector(sapply(year,function(x){paste0(month,'. ',x)}))
mycic <- str_replace_all(as.Date(as.yearmon(mycic),frac=1),"-","")
mycic <- mycic[-c(which(mycic=="20230630"):length(mycic))]
i <- 19
for (i in 1:length(mycic)) {
  #CIC-K OPERACIONES: Información específica de la operación como lugar tasas, montos, fechas clave, días de gracia
  print(mycic[i])
  cick <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"K.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
  names(cick) <- namesk$NAME[1:ncol(cick)]
  #CIC-P OBLIGADOS: Información específica de los obligados de la operación como nombre, genero, nacimiento, ci, act econ, id interno 
  cicp <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"P.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
  names(cicp) <- namesp$NAME[1:ncol(cicp)]
  
  #CIC-H
  cich <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"P.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
  names(cich) <- namesp$NAME[1:ncol(cicp)]
  #CR-H INFORMACIÓN COMPLEMENTARIA DE LA OPERACION: Información cualitativa de los créditos
  # cich <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"H.IBBSO"),
  #               encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE, col.names = namesh$NAME)
  #CR-M CALIFICACION: Calificación del obligado
  cicm <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"M.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
  names(cicm) <- namesm$NAME[1:ncol(cicm)]
  #CR-L GARANTIAS: Calificación del obligado
  cicl <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"L.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
  names(cicl) <- namesl$NAME[1:ncol(cicl)]
  #CIC-B CPOP: Beneficio C-POP
  cicb <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"B.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
  names(cicb) <- namesb$NAME[1:ncol(cicb)]
  #CR-O CUENTA CONTABLE: Saldo por cuenta contable  y moneda de todas las operaciones
  cico <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"O.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
  names(cico) <- nameso$NAME[1:ncol(cico)]
  #CIC-A  REPORTE DE CUOTAS DE CRÉDITOS CON RETRASO EN EL PAGO: Días de retraso
  cica <- fread(paste0(workdir,mycic[i],"/CC",mycic[i],"A.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
  names(cica) <- namesa$NAME[1:ncol(cica)]
  #CIC-R  OPERACIONES GENERADAS EN EL PERÍODO: CodTipoGeneracion: 1 Refin de una o varias ops de la entidad
  #2 Refin de una o varias ops en otras entidades, 3 Refin de una o varias ops en la entidad y otras, 4. Nueva operación
  cicr <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"R.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
  names(cicr) <- namesr$NAME[1:ncol(cicr)]
  #CIC-U  OPERACIONES REFINANCIADAS: Origen, Saldo y Tipo de Refinanciamiento
  cicu <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"U.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
  names(cicu) <- namesu$NAME[1:ncol(cicu)]
  #CIC-H  Información adicional
  cich <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"H.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
  names(cich) <- namesh$NAME[1:ncol(cich)]
  
  cick_processed <- cick %>% 
    separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                         too_many = "drop",too_few = "align_start") %>% 
    mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
    mutate(across(c(CodLocalidadSucursal, CodLocalidadOtorga),~as.character(.x))) %>% 
    left_join(cDpto, by=c("CodDptoSucursal"="CodDpto", "CodLocalidadSucursal"="CodLocal")) %>% 
    left_join(cFormaPago, by=c("CodFormaPago")) %>% 
    left_join(cTipoInteres, by=c("CodTipoInteres")) %>% 
    left_join(cTipoOperacion, by=c("CodTipoOperacion")) %>% 
    left_join(cTipoCancelacion, by=c("CodTipoCancelacion")) %>% 
    left_join(cMonedaCuenta, by=c("CodMonedaCuenta")) %>% 
    left_join(cActividadEconomica, by=c("CodActividadEconomica")) %>% 
    left_join(cPlanPagos, by=c("CodPlanPagos")) %>% 
    left_join(rename(cDpto, SucursalOtorga = Sucursal), by=c("CodDptoOtorga"="CodDpto", "CodLocalidadOtorga"="CodLocal")) %>% 
    left_join(cObjetoCredito, by=c("CodObjetoCredito")) %>% 
    mutate(TipoCredito = case_when(substr(CodTipoCredito,1,1)=='C' ~ "Empresarial",
                                   substr(CodTipoCredito,1,1)=='H' ~ "Vivienda",
                                   substr(CodTipoCredito,1,1)=='M' ~ "Micro",
                                   substr(CodTipoCredito,1,1)=='N' ~ "Consumo",
                                   substr(CodTipoCredito,1,1)=='P' ~ "Pyme",)) %>% 
    mutate(CtaCont = paste0(CodCapituloCuenta,CodGrupoCuenta,CodCuenta)) %>% 
    select(FechaCorte, FechaInicio, CTACLIENTE, OPERACION, Sucursal, FechaReprogramacion, 
           CantidadReprogramaciones, CantidadDiasGracia, TasaInteres, FechaVencimiento, 
           MontoContratado, MontoComputable, FormaPago, CantidadDiasPlazo, TipoInteres, 
           TipoOperacion, TipoCredito, TipoCancelacion, CantidadCuotas, CtaCont, 
           SubCtaCont = CodSubCuenta, Moneda, FechaCancelacion, 
           CodActividadEconomica, GrupoActEconDest = GrupoActEcon, 
           PlanPagos, MontoCuotaFija, FechaVencePrimeraCuota, FechaIncumplimiento, 
           AnioInicioLineaCarta, SucursalOtorga, ObjetoCredito, IngresosFinancieros)
  
  cicp_processed <- cicp %>% 
    separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                         too_many = "drop",too_few = "align_start") %>% 
    mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
    left_join(cTipoIdentificacion, by=c("CodTipoIdentificacion")) %>% 
    left_join(cTipoPersona, by=c("CodTipoPersona")) %>% 
    left_join(cActividadEconomica, by=c("CodActividadEconomica")) %>% 
    left_join(cTipoRelacion, by=c("CodTipoRelacion")) %>% 
    left_join(cTipoGarantia, by=c("CodTipoGarantia")) %>% 
    left_join(cGeneracionIngresos, by=c("CodGeneracionIngresos")) %>% 
    mutate(CI = paste0(NroRaizCedula, ComplementoCedula, LugarEmisionCedula)) %>% 
    mutate(Nombre = paste(Nombre, PrimerApellido, SegundoApellido, ApellidoEsposo)) 
  
  cicp_credit <- cicp_processed %>% 
    dplyr::filter(CodTipoRelacion %in% c('1A','4A','5A')) %>% 
    select(CTACLIENTE, OPERACION, IdObligado, TipoIdentificacion,
           TipoPersona, NombreRazonSocial, GrupoActEconOrigen = GrupoActEcon, TipoRelacion, 
           TipoGarantia, PorcentajeCompromiso, MontoPatrimonio, GeneracionIngresos, 
           IndiceActividadEconomica, CI, Nombre, IdObligadoAnterior, Activo, Patrimonio, 
           IngresosVentasServicios, PersonalOcupado)
  
  cico_processed <- cico %>% 
    separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                         too_many = "drop",too_few = "align_start") %>% 
    mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
    left_join(cMonedaCuenta, by=c("CodMoneda"="CodMonedaCuenta")) %>% 
    mutate(CtaCont = paste0(CodCapitulo,CodGrupo,CodCuenta)) %>% 
    # mutate(MontoSaldo = ifelse(Moneda=='MN',MontoSaldo/6.86,MontoSaldo)) %>% 
    select(CTACLIENTE, OPERACION, CtaCont, SubCtaCont = CodSubCuenta,
           Moneda, FechaIngresoEstado, MontoSaldo) %>% 
    dplyr::filter(CtaCont %in% c('131','133','134','135','136','137','623','865')) %>% 
    mutate(MontoDiferido = ifelse(SubCtaCont>=50,MontoSaldo,0)) %>% 
    group_by(CTACLIENTE,OPERACION,CtaCont) %>% 
    summarise(MontoSaldo = sum(MontoSaldo), MontoDiferido = sum(MontoDiferido)) %>% 
    arrange(desc(MontoSaldo)) %>% 
    dplyr::filter(row_number()==1) %>% 
    ungroup()
  
  cicm_processed <- cicm %>% 
    select(IdObligado, CodCalificacion, FechaCalificacion) %>% 
    group_by(IdObligado) %>% 
    summarise(Calificacion = max(CodCalificacion))
  
  cicb_processed <- cicb %>% 
    separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                         too_many = "drop",too_few = "align_start") %>% 
    mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
    left_join(cBeneficio, by=c("CodigoBeneficio")) %>% 
    left_join(rename(cBeneficio, BeneficioInicial = BeneficioCPOP), by=c("BeneficioInicial"="CodigoBeneficio")) %>% 
    select(CTACLIENTE, OPERACION, BeneficioCPOP, FechaBeneficio) %>% 
    group_by(CTACLIENTE, OPERACION) %>% 
    dplyr::filter(row_number()==1) %>% 
    ungroup()
  
  cicl_processed <- cicl %>% 
    separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                         too_many = "drop",too_few = "align_start") %>% 
    mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
    left_join(rename(cDepar, DptoGarantia = Departamento), by=c("CodDptoGarantia"="CodDpto")) %>% 
    left_join(cTipoGarantia, by=c("CodTipoGarantia")) %>%
    left_join(cReduccionGarantia, by=c("CodReduccionGarantia")) %>%
    left_join(cMonedaCuenta, by=c("CodMoneda"="CodMonedaCuenta")) %>% 
    select(CTACLIENTE, OPERACION, DptoGarantia, TipoGarantia, CodReduccionGarantia,
           ReduccionGarantia, MonedaGarantia = Moneda, MontoGarantiaOtras,
           MontoGarantiaEntidad, MontoGarantiaNeto, CodEnvioFondoGarantia) %>% 
    mutate(across(starts_with('Monto'),~ifelse(MonedaGarantia=='MN',.x/6.86,.x))) %>% 
    mutate(CodReduccionGarantia = paste0('R',CodReduccionGarantia)) %>% 
    group_by(CTACLIENTE,OPERACION, CodReduccionGarantia) %>% 
    summarise(MontoGarantiaEntidad=sum(MontoGarantiaEntidad)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = CodReduccionGarantia, values_from = MontoGarantiaEntidad, values_fill = 0)
  
  cica_processed <- cica %>% 
    separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                         too_many = "drop",too_few = "align_start") %>% 
    mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
    select(CTACLIENTE, OPERACION, NumeroCuota, FechaProgramada, 
           FechaPago, DiasRetraso) %>% 
    group_by(CTACLIENTE, OPERACION) %>% 
    arrange(desc(FechaProgramada)) %>% 
    dplyr::filter(row_number()==1) %>% 
    ungroup() 
  
  #ARREGLAR TABLA R
  cicr_processed <- cicr %>% 
    separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                         too_many = "drop",too_few = "align_start") %>% 
    mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
    select(CTACLIENTE, OPERACION, CodTipoGeneración,
           MontoAdicional, MontoTotal, NumeroOperaciones)
  #ARREGLAR TABLA U
  cicu_processed <- cicu %>% 
    separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                         too_many = "drop",too_few = "align_start") %>% 
    separate_wider_delim(IdOperacionOrigen, names = c("CTACLIENTE_ORI_REF","OPERACION_ORI_REF"), delim = "-",
                         too_many = "drop",too_few = "align_start") %>% 
    mutate(across(c(CTACLIENTE, OPERACION, CTACLIENTE_ORI_REF, OPERACION_ORI_REF),~as.numeric(.x))) %>% 
    select(-FechaCorte,-FechaInicio,-CodEnvio,-CodEnvioOrigen) %>% 
    group_by(CTACLIENTE, OPERACION) %>% 
    arrange(desc(FechaInicioOrigen)) %>% 
    mutate(NOrigen = n()) %>% 
    dplyr::filter(row_number()==1)
  
  cich_processed <- cich %>% 
    separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                         too_many = "drop",too_few = "align_start") %>% 
    mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
    left_join(cMonedaCuenta, by=c("CodMoneda"="CodMonedaCuenta")) %>% 
    select(CTACLIENTE, OPERACION, TipoCambio, MontPromCuotaVariab,
           FechaPrimerPagoCap, FechaPrimerPagoInt, FechaProxPagoCap, FechaProxPagoInt,
           FechaUltPagoCap, FechaUltPagoInt, TipoProducto, Asesor = NombreOficialResp,
           NivelAutorizacion, CodPondActivos, PrevCiclica, PromDiasAtraso, IntSuspenso,
           CredProdNoProd, CuotasDiferidas, MontCapitDiferido, MontoInteresDiferido)
  
  cic_all <- cick_processed %>% 
    left_join(cicp_credit, by=c("CTACLIENTE","OPERACION")) %>% 
    left_join(cich_processed, by=c("CTACLIENTE","OPERACION")) %>% 
    left_join(cicb_processed, by=c("CTACLIENTE","OPERACION")) %>% 
    left_join(cicm_processed, by="IdObligado") %>%
    left_join(cica_processed, by=c("CTACLIENTE","OPERACION")) %>% 
    left_join(cicr_processed, by=c("CTACLIENTE","OPERACION")) %>% 
    left_join(cicu_processed, by=c("CTACLIENTE","OPERACION")) %>% 
    left_join(cico_processed, by=c("CTACLIENTE","OPERACION")) %>% 
    rowwise() %>% 
    mutate(MontoAdicional = ifelse(is.na(MontoAdicional.x) & is.na(MontoAdicional.y),
                                   NA,max(MontoAdicional.x, MontoAdicional.y,na.rm = T))) %>% 
    ungroup() %>% 
    mutate(across(c(starts_with("Monto"),SaldoOperacion),~ifelse(Moneda=='MN',as.numeric(.x)/6.86,as.numeric(.x)))) %>% #PARA SALDOS Y MONTOS 
    select(-CtaCont.x, -MontoAdicional.x, -MontoAdicional.y) %>% 
    rename(CtaCont = CtaCont.y) 
}

write_xlsx(cic_all,"D:/!bso/CIC/cic_Ene2023.xlsx")

####____PRODUCTIVO____####

cic_prod <- cic_all %>% 
  select(CTACLIENTE, OPERACION, GrupoActEcon = GrupoActEconDest,GrupoActEconOrigen, TipoCredito,
         CodActividadEconomica, CredProdNoProd,CtaCont, TipoProducto) %>% 
  mutate(CAEDEC_DEST = as.numeric(CodActividadEconomica)) %>% 
  mutate(PRODUC = case_when(!(TipoCredito %in% c('Micro','Pyme')) ~ 2,
                            GrupoActEcon %in% c('A','B','C','D','E','F','G') ~ 1,
                            CAEDEC_DEST %in% cods ~ 1,
                            TRUE~2)) %>% 
  mutate(PRODUC = if_else(CtaCont %in% c('865','623') & PRODUC ==1,2,PRODUC)) %>% 
  # mutate(PRODUC = if_else(GrupoActEcon=='P',1,PRODUC)) %>% 
  glimpse()
  
table(cic_prod$CredProdNoProd,cic_prod$PRODUC)

####____SOLO GARANTIAS ALE____####
namesl <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = "CR-L") %>% 
  select(1:2)

cMonedaCuenta <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT019") %>% 
  select(CodMonedaCuenta = cmnda, Moneda = tsmnd)
cDepar <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT038") %>% 
  select(CodDpto = clclz, Departamento = tdlcl)
cTipoGarantia <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT039") %>% 
  select(CodTipoGarantia = ctgrn, TipoGarantia = tdtgr)
cReduccionGarantia <- readxl::read_excel("D:/!bso/bases/excel/RPT_CIC.xlsx", sheet = "RPT137") %>% 
  select(CodReduccionGarantia = crgrn, ReduccionGarantia = tdrgr)

workdir <- "C:/CIC/"
month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
mes <- str_pad(1:12,width=2,side = 'left',pad='0')
year <- c(2018:2023)

mycic <- as.vector(sapply(year,function(x){paste0(month,'. ',x)}))
mycic <- str_replace_all(as.Date(as.yearmon(mycic),frac=1),"-","")
mycic <- mycic[-c(which(mycic=="20230630"):length(mycic))]

for (k in 1:length(mycic)) {
  dir.create(paste0("C:/CIC/",mycic[k]))
}

garantiasList <- list()

for (i in 1:length(mycic)) {
  print(mycic[i])
  #CR-L GARANTIAS: Calificación del obligado
  cicl <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"L.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
  names(cicl) <- namesl$NAME[1:ncol(cicl)]
  
  cicl_processed <- cicl %>% 
    separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                         too_many = "drop",too_few = "align_start") %>% 
    mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
    left_join(rename(cDepar, DptoGarantia = Departamento), by=c("CodDptoGarantia"="CodDpto")) %>% 
    left_join(cTipoGarantia, by=c("CodTipoGarantia")) %>%
    left_join(cReduccionGarantia, by=c("CodReduccionGarantia")) %>%
    left_join(cMonedaCuenta, by=c("CodMoneda"="CodMonedaCuenta")) %>% 
    select(CTACLIENTE, OPERACION, DptoGarantia, TipoGarantia, CodReduccionGarantia,
           ReduccionGarantia, MonedaGarantia = Moneda, MontoGarantiaOtras,
           MontoGarantiaEntidad, MontoGarantiaNeto, CodEnvioFondoGarantia) %>% 
    mutate(across(starts_with('Monto'),~ifelse(MonedaGarantia=='MN',.x/6.86,.x))) %>% 
    mutate(CodReduccionGarantia = paste0('R',CodReduccionGarantia)) %>% 
    group_by(CTACLIENTE,OPERACION, CodReduccionGarantia) %>% 
    summarise(MontoGarantiaEntidad=sum(MontoGarantiaEntidad)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = CodReduccionGarantia, values_from = MontoGarantiaEntidad, values_fill = 0)
  
  garantiasList[[i]] <- cicl_processed
}

garantiasFull <- rbindlist(garantiasList)

saveRDS(garantiasFull, "//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/Garantias_Ene2018May2023.rds")