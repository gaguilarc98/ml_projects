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
mycic <- mycic[-c(which(mycic=="20231130"):length(mycic))]
i <- 70
38:64
for (i in 65:67) {
  tryCatch({
    #CIC-K OPERACIONES: Información específica de la operación como lugar tasas, montos, fechas clave, días de gracia
    print(mycic[i])
    cick <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"K.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cick) <- CICnames$CR_K$NAME[1:ncol(cick)]
    #CIC-P OBLIGADOS: Información específica de los obligados de la operación como nombre, genero, nacimiento, ci, act econ, id interno 
    cicp <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"P.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cicp) <- CICnames$CR_P$NAME[1:ncol(cicp)]
    #CR-M CALIFICACION: Calificación del obligado
    cicm <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"M.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cicm) <- CICnames$CR_M$NAME[1:ncol(cicm)]
    #CR-L GARANTIAS: Calificación del obligado
    cicl <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"L.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cicl) <- CICnames$CR_L$NAME[1:ncol(cicl)]
    #CIC-B CPOP: Beneficio C-POP
    cicb <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"B.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cicb) <- CICnames$CR_B$NAME[1:ncol(cicb)]
    #CR-O CUENTA CONTABLE: Saldo por cuenta contable  y moneda de todas las operaciones
    cico <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"O.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cico) <- CICnames$CR_O$NAME[1:ncol(cico)]
    #CIC-A  REPORTE DE CUOTAS DE CRÉDITOS CON RETRASO EN EL PAGO: Días de retraso
    cica <- fread(paste0(workdir,mycic[i],"/CC",mycic[i],"A.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cica) <- CICnames$CC_A$NAME[1:ncol(cica)]
    #CIC-R  OPERACIONES GENERADAS EN EL PERÍODO: CodTipoGeneracion: 1 Refin de una o varias ops de la entidad
    #2 Refin de una o varias ops en otras entidades, 3 Refin de una o varias ops en la entidad y otras, 4. Nueva operación
    cicr <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"R.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cicr) <- CICnames$CR_R$NAME[1:ncol(cicr)]
    #CIC-U  OPERACIONES REFINANCIADAS: Origen, Saldo y Tipo de Refinanciamiento
    cicu <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"U.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cicu) <- CICnames$CR_U$NAME[1:ncol(cicu)]
    #CIC_T Operaciones recibidas o transferidas
    cict <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"T.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    names(cict) <- CICnames$CR_T$NAME[1:ncol(cict)]
    #CIC-Q Plan de Pagos
    # cicq <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"Q.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
    # names(cicq) <- CICnames$CR_Q$NAME[1:ncol(cicq)]
    
    cick_processed <- cick %>% 
      mutate(across(c(CodLocalidadSucursal, CodLocalidadOtorga),~as.character(.x))) %>% 
      left_join(CICcodes$RPT038, by=c("CodDptoSucursal"="CodDpto")) %>% 
      left_join(CICcodes$RPT009, by=c("CodFormaPago")) %>% 
      left_join(CICcodes$RPT036, by=c("CodTipoInteres")) %>% 
      left_join(CICcodes$RPT053, by=c("CodTipoCredito")) %>% 
      left_join(CICcodes$RPT035, by=c("CodTipoOperacion")) %>% 
      left_join(CICcodes$RPT034, by=c("CodTipoCancelacion")) %>% 
      left_join(CICcodes$RPT019, by=c("CodMonedaCuenta"="CodMoneda")) %>% 
      left_join(CICcodes$RPT004, by=c("CodActividadEconomica")) %>% 
      left_join(CICcodes$RPT043, by=c("GrupoActEcon")) %>% 
      left_join(CICcodes$RPT015, by=c("CodPlanPagos")) %>% 
      left_join(CICcodes$RPT203, by=c("CodDptoOtorga"="CodDpto", "CodLocalidadOtorga"="CodLocal")) %>% 
      left_join(CICcodes$RPT139, by=c("CodObjetoCredito")) %>% 
      mutate(CuentaContable = paste0(CodCapituloCuenta,CodGrupoCuenta,CodCuenta)) %>% 
      mutate(monDate = as.yearmon(FechaCorte)) %>% 
      # select(any_of(c("monDate","FechaCorte","FechaInicio","IdOperacion","Departamento","Localidad",
      #                 "FechaReprogramacion","CantidadReprogramaciones","CantidadDiasGracia","TasaInteres",
      #                 "FechaVencimiento","MontoContratado","MontoComputable","FormaPago","CantidadDiasPlazo", 
      #                 "TipoInteres","CodTipoCredito","TipoCredito","EspecieCredito","TipoOperacion", 
      #                 "TipoCancelacion","CantidadCuotas","CuentaContable","CodSubCuenta","Moneda", 
      #                 "FechaCancelacion",GrupoActEconDest = GrupoActEcon, ActEconDest = ActividadEconomica, 
      #                 "PlanPagos","MontoCuotaFija","FechaVencePrimeraCuota","FechaIncumplimiento", 
      #                 "AnioInicioLineaCarta","IdOperacionLineaCarta","ObjetoCredito","CodPAF", 
      #                 "IngresosFinancieros","MontoComputableNoDiferido")))
      select(monDate, FechaCorte, FechaInicio, IdOperacion, Departamento, Localidad,
             FechaReprogramacion, CantidadReprogramaciones, CantidadDiasGracia, TasaInteres,
             FechaVencimiento, MontoContratado, MontoComputable, FormaPago, CantidadDiasPlazo, 
             TipoInteres, CodTipoCredito, TipoCredito, EspecieCredito, TipoOperacion, 
             TipoCancelacion, CantidadCuotas, CuentaContable, CodSubCuenta, Moneda, 
             FechaCancelacion, CodActEconDest=CodActividadEconomica, GrupoActEconDest = GrupoActEcon, 
             ActEconDest = ActividadEconomica, PlanPagos, MontoCuotaFija, FechaVencePrimeraCuota, 
             FechaIncumplimiento, AnioInicioLineaCarta, IdOperacionLineaCarta, ObjetoCredito, 
             CodPAF, IngresosFinancieros, any_of(c('MontoComputableNoDiferido')))
    
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
      group_by(IdOperacion) %>% 
      mutate(Garantes = sum(CodTipoRelacion=="02")) %>% 
      mutate(Codeudores = sum(CodTipoRelacion=="1B")) %>% 
      ungroup() %>% 
      dplyr::filter(str_detect(CodTipoRelacion, "A")) %>% 
      select(IdOperacion, TipoIdentificacion, TipoPersona, NombreRazonSocial, 
             CodActEconOri = CodActividadEconomica, ActEconOri=ActividadEconomica,
             GrupoActEcoOri=GrupoActEcon, TipoObligado, PorcentajeCompromiso,
             MontoPatrimonio, GeneracionIngresos, Genero=CodGenero, FechaNacimiento, 
             IndiceActEcon=IndiceActividadEconomica,
             CI, IdObligadoAnterior, Nombre, Activo, Patrimonio, IngresosVentasServicios, 
             PersonalOcupado, Garantes, Codeudores, FechaBeneficio, HistorialBeneficio, 
             Calificacion, FechaCalificacion, any_of(c('NroIdentInterno'))) 
    
    cico_processed <- cico %>% 
      left_join(CICcodes$RPT019, by="CodMoneda") %>% 
      dplyr::filter(MontoSaldo>0) %>% 
      mutate(CuentaContable = paste0(CodCapitulo,CodGrupo,CodCuenta)) %>% 
      mutate(MontoSaldo = ifelse(Moneda=='MN',MontoSaldo/6.86,MontoSaldo)) %>%
      mutate(SaldoBruto = ifelse(CuentaContable %in% c('131','133','134','135','136','137'), MontoSaldo, 0)) %>% 
      mutate(SaldoMora = ifelse(CuentaContable %in% c('133','134','136','137'), MontoSaldo, 0)) %>% 
      mutate(SaldoCastigado = ifelse(CuentaContable =='865' 
                                     & CodSubCuenta %in% c(1,2,3), MontoSaldo, 0)) %>% 
      mutate(SaldoContingente = ifelse(CuentaContable =='623', MontoSaldo, 0)) %>% 
      mutate(SaldoDiferido = ifelse(CuentaContable %in% c('131','133','134','135','136','137') 
                                    & CodSubCuenta>=50, MontoSaldo, 0)) %>% 
      mutate(PrevEspecifica = ifelse(CuentaContable=='139',MontoSaldo,0)) %>% 
      mutate(PrevEspecificaDif = ifelse(CuentaContable=='139' & CodSubCuenta>=50, MontoSaldo, 0)) %>% 
      mutate(Devengado = ifelse(CuentaContable=='138', MontoSaldo, 0)) %>% 
      mutate(DevengadoDif = ifelse(CuentaContable=='138' & CodSubCuenta>=50, MontoSaldo, 0)) %>% 
      mutate(CuentaContable = ifelse(CuentaContable %in% c('131','133','134','135','136','137','623','861','865'),
                                     CuentaContable,'999')) %>%
      select(IdOperacion, CuentaContable, starts_with("Saldo"), starts_with("Prev"), starts_with("Dev")) %>%
      group_by(IdOperacion) %>% 
      summarise(CuentaContable = min(CuentaContable),
                across(c(starts_with("Saldo"), starts_with("Prev"), starts_with("Dev")), ~sum(.x))) %>% 
      ungroup() %>% 
      dplyr::filter(CuentaContable %in% c('131','133','134','135','136','137','623','865')) 
    
    cicl_processed <- cicl %>% 
      left_join(CICcodes$RPT137, by="CodReduccionGarantia") %>%
      left_join(CICcodes$RPT019, by="CodMoneda") %>% 
      mutate(across(starts_with('Monto'),~ifelse(Moneda=='MN',.x/6.86,.x))) %>% 
      rename(GarantiaOtras = MontoGarantiaOtras, 
             GarantiaNeto = MontoGarantiaNeto) %>% 
      mutate(GarantiaEntidadRed0 = ifelse(PorcRedGarantia==0, MontoGarantiaEntidad, 0)) %>% 
      mutate(GarantiaEntidadRed50 = ifelse(PorcRedGarantia==50, MontoGarantiaEntidad, 0)) %>% 
      mutate(GarantiaEntidadRed55 = ifelse(PorcRedGarantia==55, MontoGarantiaEntidad, 0)) %>% 
      mutate(GarantiaEntidadRed100 = ifelse(PorcRedGarantia==100, MontoGarantiaEntidad, 0)) %>% 
      select(IdOperacion, starts_with("Garantia")) %>% 
      group_by(IdOperacion) %>% 
      summarise_all(sum) %>% 
      ungroup()
    
    cica_processed <- cica %>% 
      select(IdOperacion, AnioInicioOperacion, NumeroCuota,
             FechaProgramada, FechaPago, DiasRetraso) %>% 
      group_by(IdOperacion, AnioInicioOperacion, NumeroCuota) %>% 
      dplyr::filter(row_number()==1) %>% 
      ungroup() %>% 
      group_by(IdOperacion) %>% 
      summarise(CantidadPagosConRetraso = n(),
                MáximoRetraso = max(DiasRetraso))
    
    #ARREGLAR TABLA R
    cicr_processed <- cicr %>% 
      select(IdOperacion, CodTipoGeneración,
             MontoAdicional, MontoTotal, NumeroOperaciones)
    #ARREGLAR TABLA U
    cicu_processed <- cicu %>% 
      group_by(IdOperacion) %>% 
      arrange(desc(FechaInicioOrigen)) %>% 
      mutate(OpsOrigen = n_distinct(IdOperacionOrigen)) %>% 
      dplyr::filter(row_number()==1) %>% 
      ungroup() %>% 
      select(IdOperacion, IdOperacionOrigen, SaldoOperacion, OpsOrigen,
             any_of(c('FechaRefinanciamiento','TipoRefinanciamiento')))
    
    cict_processed <- cict %>%
      select(IdOperacion, IdRecibidaTransferida, SaldoOrigen=SaldoOperacion, IdOperacionSF=IdOperacionOrigen,
             FechaInicioOrigen, CodEnvioOrigen, FechaIngreso=FechaInicio)
    
    cic_all <- cick_processed %>% 
      left_join(cicp_processed, by="IdOperacion") %>% 
      left_join(cicl_processed, by="IdOperacion") %>% 
      left_join(cica_processed, by="IdOperacion") %>% 
      left_join(cicr_processed, by="IdOperacion") %>% 
      left_join(cicu_processed, by="IdOperacion") %>% 
      left_join(cict_processed, by="IdOperacion") %>%
      ungroup()
    
    if(i>=31){
      #CR-H INFORMACIÓN COMPLEMENTARIA DE LA OPERACION: Información cualitativa de los créditos
      cich <- fread(paste0(workdir,mycic[i],"/CR",mycic[i],"H.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
      names(cich) <- CICnames$CR_H$NAME[1:ncol(cich)]
      
      cich_processed <- cich %>% 
        left_join(CICcodes$RPT019, by="CodMoneda") %>% 
        select(IdOperacion, TipoCambio, MontPromCuotaVariab, DiasGraciaInter,
               FechaPrimerPagoCap:FechaUltPagoInt, TipoProducto, NombreOficialResp,
               NivelAutorizacion, CodPondActivos, PrevCiclica, PromDiasAtraso, IntSuspenso,
               CredProdNoProd, CuotasDiferidas, MontCapitDiferido, MontoInteresDiferido)
      cic_all <- cic_all %>% 
        left_join(cich_processed, by="IdOperacion") %>% 
        mutate(PrevCiclica = ifelse(Moneda=="MN", PrevCiclica/6.86, PrevCiclica)) %>% 
        mutate(IntSuspenso = ifelse(Moneda=="MN", IntSuspenso/6.86, IntSuspenso))
    }
    cic_all <- cic_all %>% 
      mutate(across(c(starts_with("Monto"),SaldoOperacion),~ifelse(Moneda=='MN',as.numeric(.x)/6.86,as.numeric(.x)))) %>% #PARA SALDOS Y MONTOS 
      mutate(across(starts_with("Fecha"),~as.Date(.x))) %>% 
      mutate(across(c(Activo, Patrimonio, IngresosVentasServicios),~.x/6.86)) %>% 
      left_join(cico_processed, by="IdOperacion") %>%
      separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                           too_many = "drop",too_few = "align_start") %>% 
      mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
      ungroup() %>% 
      mutate(DiasMora = ifelse(!is.na(FechaIncumplimiento),FechaCorte-FechaIncumplimiento,0)) %>% 
      mutate(ESTADO = case_when(CuentaContable.y %in% c('131','135') ~ 'VIGENTE',
                                CuentaContable.y %in% c('133','136') ~ 'VENCIDA',
                                CuentaContable.y %in% c('134','137') ~ 'EJECUCION',
                                CuentaContable.y %in% c('865') ~ 'CASTIGADA',
                                CuentaContable.y %in% c('623') ~ 'CONTINGENTE',
                                TRUE~'OTRO')) 
    
    saveRDS(cic_all, paste0("D:/!bso/CIC/rds/cic_",monyear[i],".rds"))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


x <- cic_all %>% 
  mutate(MontoComp = case_when(GarantiaEntidadRed50>0 & GarantiaEntidadRed50<=SaldoBruto ~ SaldoBruto-0.50*GarantiaEntidadRed50,
                               GarantiaEntidadRed50>0 & GarantiaEntidadRed50>SaldoBruto ~ SaldoBruto-0.50*SaldoBruto,
                               GarantiaEntidadRed55>0 & GarantiaEntidadRed55<=SaldoBruto ~ SaldoBruto-0.55*GarantiaEntidadRed55,
                               GarantiaEntidadRed55>0 & GarantiaEntidadRed55>SaldoBruto ~ SaldoBruto-0.55*SaldoBruto,
                               GarantiaEntidadRed100>0 & GarantiaEntidadRed100<=SaldoBruto ~ SaldoBruto-1*GarantiaEntidadRed100,
                               GarantiaEntidadRed100>0 & GarantiaEntidadRed100>SaldoBruto ~ SaldoBruto-1*SaldoBruto,
                               TRUE~SaldoBruto)) 


write_xlsx(select(cic_all,-monDate),"D:/!bso/CIC/cic_Sep2023.xlsx")
####CUADRE BDC####
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Ene2023.rds")
bdc <- select(bdc, CTACLIENTE, OPERACION, saldous, montous, SALDO, MONTO, MONEDA, ctaCont, ESTADO) %>% 
  mutate(MONTOUS = ifelse(MONEDA==0, MONTO/6.86, MONTO))

cicJoin <- cic_all %>% 
  dplyr::filter(CuentaContable.x %in% c('131','133','134','135','136','137','623','865')) %>% 
  dplyr::filter(is.na(FechaCancelacion)) %>% 
  select(CTACLIENTE,OPERACION,starts_with("Cuenta"),starts_with("Monto"),starts_with("Saldo"),Moneda) %>% 
  left_join(bdc, by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(difMonto=MontoContratado-MONTOUS)

####___SHIT DE ALIWI____####
mycic <- "20230731"
cic <- readRDS("D:/!bso/CIC/rds/cic_Jul2023.rds")
cicl <- fread(paste0('C:/CIC/',mycic,"/CR",mycic,"L.IBBSO"),encoding="Latin-1", sep = ",", header = FALSE,fill = TRUE)
names(cicl) <- CICnames$CR_L$NAME[1:ncol(cicl)]

cicl_processed <- cicl %>% 
  left_join(CICcodes$RPT137, by="CodReduccionGarantia") %>%
  left_join(CICcodes$RPT019, by="CodMoneda") %>% 
  dplyr::filter(CodTipoGarantia %in% c("BM9")) %>%
  mutate(across(starts_with('Monto'),~ifelse(Moneda=='MN',.x/6.86,.x))) %>% 
  mutate(GarantiaEntidadRed0 = ifelse(PorcRedGarantia==0, MontoGarantiaEntidad, 0)) %>% 
  mutate(GarantiaEntidadRed50 = ifelse(PorcRedGarantia==50, MontoGarantiaEntidad, 0)) %>% 
  mutate(GarantiaEntidadRed55 = ifelse(PorcRedGarantia==55, MontoGarantiaEntidad, 0)) %>% 
  mutate(GarantiaEntidadRed100 = ifelse(PorcRedGarantia==100, MontoGarantiaEntidad, 0)) %>% 
  select(IdOperacion, CodTipoGarantia, starts_with("Garantia")) %>% 
  group_by(IdOperacion, CodTipoGarantia) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  pivot_wider(names_from = CodTipoGarantia, values_from = c(GarantiaEntidadRed100)) %>% 
  separate_wider_delim(IdOperacion, names = c("CTACLIENTE","OPERACION"), delim = "-",
                       too_many = "drop",too_few = "align_start") %>% 
  mutate(across(c(CTACLIENTE, OPERACION),~as.numeric(.x))) %>% 
  select(-starts_with("Garantia"))

x <- cicl_processed %>% 
  inner_join(cic, by=c("CTACLIENTE","OPERACION")) 
write_xlsx(x,"D:/!bso/CIC/GarantiasBancarias_Jul2023.xlsx")
