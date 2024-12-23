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
####____CUADRE CON BI____####
CapReg <- read_xlsx("D:/!bso/bases/excel/CapReg.xlsx") %>% 
  mutate(monDate = as.yearmon(FechaCorte)+1/12)

mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2022:2023)
my <- as.vector(sapply(year, function(x){paste0(mes,x)}))
i <- 13
for (i in 1:length(my)) {
  tryCatch({
    print(my[i])
    
    bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",my[i],".rds")) %>% 
      mutate(DebGar = ifelse(TIPO_CREDITO %in% c('C2','H0','H2','H3','M1','M2','M4','M6',
                                                   'M8','M9','N1','N2','P3','P4','P6','P9'), 1, 0)) %>% 
      mutate(Productivo = ifelse(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada","2.Otra prod. Controlada","3.C2.Sector Turismo","4.C3.Prod Intelectual",
                                                        "5.C4.Fab,Ens.,Vent.MaqAutHib", "7.Prod.Agropec.No Controlada","8.Otra Prod.No Controlada"), "S","N")) 
    Daily <- readRDS("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/ec_20230131.rds")
    
    DailyShort <- Daily %>% 
      select(CTACLIENTE, OPERACION, PROD=PRODUCTIVO)
    
    bdc <- bdc %>% 
      left_join(DailyShort, by=c("CTACLIENTE","OPERACION"))
    
    bdc %>% dplyr::filter(MODULO!=29) %>% 
      left_join(CapReg, by="monDate") %>% 
      dplyr::filter(DebGar==0) %>% 
      group_by(monDate, PROD) %>% 
      summarise(Saldo=sum(saldous)/(2*max(CapRegulatorio_USD)), Margen = 2*max(CapRegulatorio_USD)-sum(saldous)) 
    
    bdc %>% 
    l <- length(which(cic$CuentaContable.x!=cic$CuentaContable.y))
    print(paste("CTACONT DIFF:",l))
    difCtaCont <- c(difCtaCont, l)
    
    cicCuadre <- cic %>% 
      select(monDate, FechaCorte, CuentaContable.x, CuentaContable.y, SaldoBruto, SaldoContingente, 
             SaldoCastigado, SaldoDiferido, PrevEspecifica, PrevEspecificaDif, 
             Devengado, DevengadoDif) %>% 
      group_by(monDate, FechaCorte, CuentaContable.x, CuentaContable.y) %>% 
      summarise_all(sum, na.rm=T) %>% 
      ungroup()
    cicList[[i]] <- cicCuadre
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

####____CIC CUADRE DE EEFF____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2018:2023)

mycic <- as.vector(sapply(year, function(x){paste0(mes,x)}))
i <- 70
difCtaCont <- NULL
cicList <- list()
for (i in 67:69) {
  tryCatch({
    print(mycic[i])
    cic <- readRDS(paste0("D:/!bso/CIC/rds/cic_",mycic[i],".rds")) 
    
    l <- length(which(cic$CuentaContable.x!=cic$CuentaContable.y))
    print(paste("CTACONT DIFF:",l))
    difCtaCont <- c(difCtaCont, l)
    
    cicCuadre <- cic %>% 
      select(monDate, FechaCorte, CuentaContable.x, CuentaContable.y, SaldoBruto, SaldoContingente, 
             SaldoCastigado, SaldoDiferido, PrevEspecifica, PrevEspecificaDif, 
             Devengado, DevengadoDif) %>% 
      group_by(monDate, FechaCorte, CuentaContable.x, CuentaContable.y) %>% 
      summarise_all(sum, na.rm=T) %>% 
      ungroup()
    cicList[[i]] <- cicCuadre
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
cicFull <- rbindlist(cicList) %>% 
  ungroup()
write_xlsx(cicFull, "D:/!bso/CIC/CIC_cuadreEEFF.xlsx")
# write_xlsx(cicCuadre, "D:/!bso/CIC/CIC_cuadreEEFFnov22.xlsx")

####____ADD A MONTH____####
cicFull <- read_xlsx("D:/!bso/CIC/CIC_cuadreEEFF.xlsx") %>% 
  # dplyr::filter(FechaCorte!=as.Date("2023-09-30")) %>% 
  ungroup()

tail(cicFull %>% group_by(FechaCorte) %>% summarise(SB=sum(SaldoBruto)))
#Correr el loop anterior para el mes correspondiente y continuar 
cicCuadre %>% group_by(FechaCorte) %>% summarise(SB=sum(SaldoBruto))

cicFull <- cicFull %>% 
  bind_rows(select(cicCuadre, -monDate))

write_xlsx(cicFull, "D:/!bso/CIC/CIC_cuadreEEFF.xlsx")
#Actualizar el archivo CIC_cueadreEEFF_process en el excel actualizando la fuente

####____LIMITES NO DEBIDAMENTE GARANTIZADOS____####
#En el Artículo 2, Sección 7, Capítulo IV, Título II de la RNSF (Riesgo Crediticio)
#Indica que la parte del saldo que no cuente con cobertura de garantía real
#no será considerada como debidamente garantizada para efectos de control de límites.
#La sumatoria de los saldos de operaciones de crédito de las entidades bancarias que 
#no se encuentren debidamente garantizadas, no podrá exceder 2 veces el capital regulatorio
#Dicho límite podrá ser ampliado hasta 4 veces el capital regulatorio de la entidad
#Siempre y cuando el exceso se origine por créditos al sector productivo.

#El artículo 3 indica qué se considera como garantía real.
#1. Hipotecas, 2. Garantías prendarias, 3. Bonos de prenda, 
#4. Avales fianzas o cartas de crédito, 5. Valores endosados en favor de la EIF
#6. Garantías autoliquidables, 7. Documentación que respalda las operaciones de importación
#8. Documentación que respalda las operaciones de exportación. 9. Documentación que respalda
#la otorgación de garantía emitida por un Fondo de Inversión Cerrado, 10. Respaldo de la otorgación
#de una garantía de crédito emitida por un Fondo de Garantía

CapReg <- read_xlsx("D:/!bso/bases/excel/CapReg.xlsx") %>% 
  mutate(monDate = as.yearmon(FechaCorte)+1/12)

cic <- readRDS("D:/!bso/CIC/rds/cic_Ene2023.rds")

cicDG <- cic %>% 
  mutate(DebGar = ifelse(CodTipoCredito %in% c('C2','H0','H2','H3','M1','M2','M4','M6',
                                                      'M8','M9','N1','N2','P1','P3','P4','P6','P9'), 1, 0)) %>% 
  mutate(NoDebGar = ifelse(!CodTipoCredito %in% c('C2','H0','H2','H3','M1','M2','M4','M6',
                                                        'M8','M9','N1','N2','P1','P3','P4','P6','P9'), 1, 0)) %>% 
  mutate(SaldoSinCobertura = ifelse(DebGar==1 & SaldoBruto-GarantiaNeto>0, SaldoBruto-GarantiaNeto, 0)) %>% 
  mutate(SaldoConCobertura = case_when(DebGar==1 & SaldoBruto-GarantiaNeto<=0 ~ SaldoBruto,
                                       DebGar==1 & SaldoBruto-GarantiaNeto>0 ~ GarantiaNeto,
                                       TRUE ~ 0)) %>% 
  mutate(SaldoNDG = case_when(NoDebGar==1 ~ SaldoBruto,
                              SaldoSinCobertura>0 ~ SaldoSinCobertura,
                              TRUE ~ 0)) %>% 
  mutate(SaldoDG = case_when(DebGar==1 & SaldoConCobertura==0 ~ SaldoBruto,
                              DebGar==1 & SaldoConCobertura>0 ~ SaldoConCobertura,
                              TRUE ~ 0))

sum(cicDG$SaldoBruto,na.rm = T)
sum(cicDG$SaldoNDG, na.rm = T)+sum(cicDG$SaldoDG, na.rm = T)  

sum(cicDG$SaldoNDG[cicDG$CredProdNoProd=="2"], na.rm = T)

Daily <- readRDS("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/ec_20230331.rds")

DailyShort <- Daily %>% 
  select(CTACLIENTE, OPERACION, PRODUCTIVO)
cicDG <- cicDG %>% 
  left_join(DailyShort, by=c("CTACLIENTE","OPERACION"))

cicDG %>% dplyr::filter(NoDebGar==1) %>% 
  group_by(CodTipoCredito, CredProdNoProd) %>% 
  summarise(S=sum(SaldoBruto, na.rm=T))
####____CIC COLUMN NAMES____####
sheets <- excel_sheets("D:/!bso/bases/excel/MetaData_CIC.xlsx")
CICnames <- list()
for (i in 1:length(sheets)) {
  CICnames[[i]] <- readxl::read_excel("D:/!bso/bases/excel/MetaData_CIC.xlsx",sheet = sheets[i]) %>% 
    select(2) #Hasta col 8 para 2018
}
names(CICnames) <- sheets
####____CIC FIELD CODES____####
codsheets <- excel_sheets("D:/!bso/bases/excel/codCIC.xlsx")
CICcodes <- list()
for (i in 1:length(codsheets)) {
  CICcodes[[i]] <- readxl::read_excel("D:/!bso/bases/excel/codCIC.xlsx",sheet = codsheets[i]) 
}
names(CICcodes) <- codsheets

####____CIC MONTHLY UPDATE____####
workdir <- "C:/CIC/"
month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
mes <- str_pad(1:12,width=2,side = 'left',pad='0')
year <- c(2018:2023)

monyear <- as.vector(sapply(year,function(x){paste0(month,x)}))
mycic <- as.vector(sapply(year,function(x){paste0(month,'. ',x)}))
mycic <- str_replace_all(as.Date(as.yearmon(mycic),frac=1),"-","")
mycic <- mycic[-c(which(mycic=="20231031"):length(mycic))]
i <- 61
38:64
for (i in 65:67) {
  tryCatch({
    #CIC-K OPERACIONES: Información específica de la operación como lugar tasas, montos, fechas clave, días de gracia
    print(mycic[i])
    cick <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"K.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cick) <- CICnames$CR_K$NAME[1:ncol(cick)]
    #CR-L GARANTIAS: Calificación del obligado
    cicl <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"L.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cicl) <- CICnames$CR_L$NAME[1:ncol(cicl)]
    
    cicl_processed <- cicl %>% 
      left_join(CICcodes$RPT137, by="CodReduccionGarantia") %>%
      left_join(CICcodes$RPT019, by="CodMoneda") %>% 
      left_join(CICcodes$RPT039, by="CodTipoGarantia") %>% 
      mutate(across(starts_with('Monto'),~ifelse(Moneda=='MN',.x/6.86,.x))) %>% 
      rename(GarantiaOtras = MontoGarantiaOtras, 
             GarantiaNeto = MontoGarantiaNeto) %>% 
      mutate(GReal = ifelse(CodTipoGarantia %in% c("HI1","HO1","HR1","HT1","OT4","OT5","OT6","OT7",
                                                   "P01","P03","P04","P06","P09"), 1, 0)) %>% 
      mutate(GarantiaReal = MontoGarantiaEntidad*GReal) %>% 
      select(IdOperacion, starts_with("Garantia")) %>% 
      group_by(IdOperacion) %>% 
      summarise_all(sum) %>% 
      ungroup() %>% 
      separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                           too_many = "drop",too_few = "align_start") %>% 
      mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x)))
    
    cicGar <- cic %>% 
      left_join(cicl_processed, by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(DebGar = ifelse(CodTipoCredito %in% c('C2','H0','H2','H3','M1','M2','M4','M6',
                                                   'M8','M9','N1','N2','P1','P3','P4','P6','P9'), 1, 0)) %>% 
      mutate(NoDebGar = ifelse(!CodTipoCredito %in% c('C2','H0','H2','H3','M1','M2','M4','M6',
                                                      'M8','M9','N1','N2','P1','P3','P4','P6','P9'), 1, 0)) %>% 
      mutate(SaldoSinCobertura = ifelse(DebGar==1 & SaldoBruto-GarantiaReal>0, SaldoBruto-GarantiaReal, 0)) %>% 
      mutate(SaldoConCobertura = case_when(DebGar==1 & SaldoBruto-GarantiaReal<=0 ~ SaldoBruto,
                                           DebGar==1 & SaldoBruto-GarantiaReal>0 ~ GarantiaReal,
                                           TRUE ~ 0)) %>% 
      mutate(SaldoNDG = case_when(NoDebGar==1 ~ SaldoBruto,
                                  SaldoSinCobertura>0 ~ SaldoSinCobertura,
                                  TRUE ~ 0)) %>% 
      mutate(SaldoDG = case_when(DebGar==1 & SaldoConCobertura==0 ~ SaldoBruto,
                                 DebGar==1 & SaldoConCobertura>0 ~ SaldoConCobertura,
                                 TRUE ~ 0))
    
    cicGar %>% dplyr::filter(DebGar==1) %>% 
      group_by(CodTipoCredito, CredProdNoProd) %>% 
      summarise(S=sum(SaldoBruto, na.rm=T))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}