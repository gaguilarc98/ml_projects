####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
library(stringr)
library(lubridate)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(quantmod)
library(stringr)    # Working with strings
library(xtable)
library(openxlsx)
library(scales)
library(janitor)
library(arrow)
library(data.table)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____CONSTRUCCION HISTORICA DE SAPOS____####
ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Oct23.rds")
checkpt <- ptFull %>% 
  group_by(myPago) %>% 
  summarise(NObs= n(),NOps=n_distinct(Operacion), check=NObs==NOps)

condFull <- read_parquet('D:/!bso/condonaciones/CondFull.parquet')
cond_clean <- condFull %>% 
  select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int, CondCap_USD = Cond_Cap) %>%
  mutate(myCond = as.yearmon(Fecha)) %>% 
  group_by(myCond, Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(Fecha),
            across(starts_with("Cond"),~sum(.x))) %>% 
  ungroup()
codMod <- read_xlsx("D:/!bso/bases/excel/CodModulo.xlsx")
####____CONSTRUCCION DE TABLA GIR____####
mys <- c("Dic. 2022","Jul. 2023")
meses <- c("Dic2022","Jul2023")
girList <- list()
Clientes_Ajuste <- readRDS("D:/!bso/features/Clientes_AjusteRef_Ene15Jul23.rds") %>% 
  select(CTACLIENTE,OPERACION,fdes_original)
for(i in 1:length(meses)) {
  tryCatch({
    print(paste0(meses[i]))
    
    pt <- ptFull %>% 
      dplyr::filter(myPago<= as.yearmon(mys[i]) & myPago>(as.yearmon(mys[i])-1)) %>% 
      mutate(Corte = as.yearmon(mys[i])) %>% 
      group_by(Operacion) %>% 
      mutate(CantPagosTardios12m = n()) %>% 
      ungroup() %>% 
      group_by(Corte, Cuenta, Operacion) %>% 
      summarise(CantPagosTardios12m = max(CantPagosTardios12m)) %>% 
      ungroup()
    
    ct <- cond_clean %>% 
      dplyr::filter(myCond<= as.yearmon(mys[i]) & myCond>(as.yearmon(mys[i])-1)) %>% 
      mutate(Corte = as.yearmon(mys[i])) %>% 
      mutate(CantCond12m = 1) %>% 
      group_by(Corte, Cuenta, Operacion) %>% 
      summarise(CantCond12m = sum(CantCond12m)) %>% 
      ungroup()
    
    bdcNC <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', meses[i],'.rds')) 
    if(as.numeric(substr(meses[i],4,7))<=2020){
      bdcNC <- bdcNC %>% 
        mutate(SALDO_CAPITAL_DIFERIDO = 0) %>%
        mutate(SALDO_INT_CAPITAL_DIFERIDO =  0) %>%
        mutate(saldoDifFranz =  0) %>%
        mutate(saldoDif =  0) %>%
        mutate(par0Dif = ifelse(DIASMORA > 0, saldoDif, 0)) %>%
        mutate(par30Dif = ifelse(DIASMORA > 30, saldoDif, 0)) %>%
        mutate(par0DifFranz = ifelse(DIASMORA > 0, saldoDifFranz, 0)) %>%
        mutate(par30DifFranz = ifelse(DIASMORA > 30, saldoDifFranz, 0)) %>%
        mutate(saldoReprogDif = ifelse(MODULO == 121 & saldoDif >0 , saldoReprog, 0)) %>%
        mutate(saldoRefinDif = ifelse(REFINANCIAMIENTO_GENUINO != '-' & saldoDif >0, 
                                      saldoReprog, 0)) %>%
        mutate(saldoReprogDifMora = 0) %>% 
        mutate(saldoReprogDifMora30 =  0) %>% 
        mutate(saldoRefinDifMora = 0) %>% 
        mutate(saldoRefinDifMora30 = 0)
    }
    girNew <- bdcNC %>%
      left_join(codMod, by="MODULO") %>% 
      dplyr::filter(MODULO!=29) %>% 
      left_join(select(pt,-Corte), by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
      left_join(select(ct, -Corte), by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
      replace_na(list(CantCond12m=0, CantPagosTardios12m=0)) %>% 
      left_join(Clientes_Ajuste, by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(Objeto = case_when(str_detect(DESC_OBJCRED,"INVERSION")~"INVERSION",
                                 str_detect(DESC_OBJCRED,"OPERACION")~"OPERACION",
                                 TRUE~"OTROS")) %>% 
      mutate(Tipo_Credito = case_when(substr(TIPO_CREDITO,1,1) =='P' ~ 'Pyme',
                                      substr(TIPO_CREDITO,1,1) =='M' ~ 'Micro',
                                      substr(TIPO_CREDITO,1,1) =='N' ~ 'Consumo',
                                      TIPO_CREDITO %in% c('H0','H1','H2') ~ 'Vivienda Normal',
                                      TIPO_CREDITO %in% c('H3','H4') ~ 'Vivienda Social',)) %>% 
      mutate(PTyCond = case_when(CantCond12m>=4 & CantPagosTardios12m>=4 ~ "1. Alerta 4x4",
                                 CantCond12m>0 | CantPagosTardios12m>0 ~ "2. Tiene PT o Cond",
                                 TRUE ~ "3. Sin Alerta")) %>% 
      # mutate(ConteoPTyCond = case_when(CantCond12m>=12 & CantPagosTardios12m>=12 ~ '12x12',
      #                                      CantCond12m>=11 & CantPagosTardios12m>=11 ~ '11x11',
      #                                      CantCond12m>=10 & CantPagosTardios12m>=10 ~ '10x10',
      #                                      CantCond12m>=9 & CantPagosTardios12m>=9 ~ '9x9',
      #                                      CantCond12m>=8 & CantPagosTardios12m>=8 ~ '8x8',
      #                                      CantCond12m>=7 & CantPagosTardios12m>=7 ~ '7x7',
      #                                      CantCond12m>=6 & CantPagosTardios12m>=6 ~ '6x6',
      #                                      CantCond12m>=5 & CantPagosTardios12m>=5 ~ '5x5',
      #                                      CantCond12m>=4 & CantPagosTardios12m>=4 ~ '4x4',
      #                                      CantCond12m>=3 & CantPagosTardios12m>=3 ~ '3x3',
      #                                      CantCond12m>=2 & CantPagosTardios12m>=2 ~ '2x2',
      #                                      CantCond12m>=1 & CantPagosTardios12m>=1 ~ '1x1',)) %>% 
      mutate(CreditoNuevo = ifelse(year(fdes_original)==2023,'1. Nuevo 2023','2. Anterior a 2023')) %>% 
      mutate(DesembolsoPandemia = case_when(fdes_original<as.Date("2021-03-01")~"1. Pre-pandemia",
                                            fdes_original<as.Date("2022-01-01")~"2. Pandemia",
                                            fdes_original>=as.Date("2022-01-01")~"3. Post-pandemia",
                                            TRUE~NA)) %>% 
      mutate(Periodo_Desembolso = year(fdes_original)) %>%
      mutate(Periodo_Desembolso = year(monDate)-Periodo_Desembolso) %>% 
      mutate(Periodo_Desembolso = ifelse(Periodo_Desembolso>4,"<t-4",paste0('t-',Periodo_Desembolso))) %>% 
      mutate(Tipo_Cartera = case_when(ctaCont %in% c('131','133','134') & OPERACION_ORI_REF==0 ~ 'Normal',
                                      ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                      OPERACION_ORI_REF!=0 ~ 'Refinanciada',)) %>%
      mutate(Es_Diferida = ifelse(saldoDif > 0, 'Diferida', 'No Diferida')) %>%
      mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
      mutate(categ=ifelse(SECTOR_CARTERA=='9.Vivienda No controlada','Otros',categ)) %>% 
      mutate(PlazoAnios = ifelse(floor(PLAZODIAS/365)<=7,as.character(floor(PLAZODIAS/365)),
                                 "> 7")) %>% 
      select(Fecha, Sucursal, rangos, rangom,  tipoCred, Objeto,
             Sector_Destino, Sector_Actividad, ctaCont, Periodo_Desembolso, CreditoNuevo, 
             DesembolsoPandemia, PTyCond, PlazoAnios, Tipo_Cartera, Es_Diferida,
             saldous, montous, previus, intus, opDes, opTot, saldoMora, par0, 
             saldoCast, saldoReprog, saldoRepVig, par0Reprog, par30Reprog, 
             saldoDifFranz,saldoDif,saldoRef,par0Ref,par30Ref, par0Dif, par30Dif,
             par0DifFranz,par30DifFranz,saldoReprogDif,saldoRefinDif,
             saldoReprogDifMora,saldoReprogDifMora30,saldoRefinDifMora,saldoRefinDifMora30) %>% 
      # mutate(GENERO=ifelse(is.na(GENERO), 'J', GENERO)) %>% 
      # mutate(categPar30 = ifelse(is.na(categPar30), 'Normal', categPar30)) %>%
      # mutate(categPar1=case_when(par0Ref >0 ~ 'Par1Refin',
      #                            par0Reprog >0~ 'Par1Reprog',
      #                            par0Dif>0~'par1dif')) %>% 
      # mutate(categPar1 = ifelse(is.na(categPar1), 'Normal', categPar1)) %>%
      group_by(Fecha, categ, Sucursal, rangos, rangom, tipoCred, 
               Sector_Destino, Sector_Actividad, ctaCont, 
               Periodo_Desembolso, CreditoNuevo, DesembolsoPandemia,
               PTyCond, PlazoAnios, Tipo_Cartera, Es_Diferida) %>%
      summarise_all(sum) %>% 
      ungroup()
    girList[[i]] <- girNew
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

girFull <- rbindlist(girList) %>% 
  ungroup() %>% 
  dplyr::rename(Sector_Cartera = categ,
                Rango_Desembolso = rangom,
                Rango_Saldo_Actual = rangos,
                Cartera_Bruta = saldous,
                Monto_Desembolsado = montous,
                Previsión_USD = previus,
                Interés_Anual = intus,
                Operaciones_Desembolsadas = opDes,
                Operaciones_Totales = opTot,
                Cartera_Reprogramada = saldoReprog,
                PaR_30_Bruta = saldoMora,
                PaR_30_Reprogramada = par30Reprog,
                Cartera_Castigada = saldoCast,
                PaR_1_Bruta = par0,
                Cartera_Reprogramada_Vigente = saldoRepVig,
                PaR_1_Reprogramada = par0Reprog,
                Cartera_Diferida_ASFI = saldoDif,
                Cartera_Diferida_RC = saldoDifFranz,
                PaR_1_Refinanciada = par0Ref,
                PaR_30_Refinanciada = par30Ref,
                Cartera_Refinanciada = saldoRef,
                PaR_1_Diferida_ASFI = par0Dif,
                PaR_30_Diferida_ASFI = par30Dif,
                PaR_1_Diferida_RC = par0DifFranz,
                PaR_30_Diferida_RC = par30DifFranz,
                Tipo_Credito = tipoCred) %>%
  select(-starts_with('saldo')) %>% 
  glimpse()

girExp <- girFull %>% 
  dplyr::filter((month(Fecha)==12 & year(Fecha)<=2020) | year(Fecha)>2020) %>% 
  mutate(GENERO=ifelse(GENERO!='F' & GENERO!='M', 'J', GENERO)) %>% 
  mutate(Cartera_Bruta_Normal=Cartera_Bruta-Cartera_Diferida_ASFI-Cartera_Reprogramada-
           Cartera_Refinanciada,
         Par_30_Normal = PaR_30_Bruta-PaR_30_Reprogramada - PaR_30_Diferida_ASFI - PaR_30_Refinanciada,
         Par_1_Normal = PaR_1_Bruta-PaR_1_Reprogramada - PaR_1_Diferida_ASFI - PaR_1_Refinanciada)

write_xlsx(girFull,'D:/!bso/girCartera/tablaGIR_mora_Descriptive.xlsx')

####____NEW APPROACH____####
mys <- c("Dic. 2022","Oct. 2023")
meses <- str_replace(mys, ". ", "")
girList <- list()
Clientes_Ajuste <- readRDS("D:/!bso/features/Clientes_Ene15Oct23.rds") %>% #Clientes_AjusteRef_Ene15Ago23
  select(CTACLIENTE,OPERACION,fdes_original = fdes)
for(i in 1:length(meses)) {
  tryCatch({
    print(paste0(meses[i]))
    
    pt <- ptFull %>% 
      dplyr::filter(myPago<= as.yearmon(mys[i]) & myPago>(as.yearmon(mys[i])-1)) %>% 
      mutate(Corte = as.yearmon(mys[i])) %>% 
      group_by(Operacion) %>% 
      mutate(CantPagosTardios12m = n()) %>% 
      ungroup() %>% 
      group_by(Corte, Cuenta, Operacion) %>% 
      summarise(CantPagosTardios12m = max(CantPagosTardios12m)) %>% 
      ungroup()
    
    ct <- cond_clean %>% 
      dplyr::filter(myCond<= as.yearmon(mys[i]) & myCond>(as.yearmon(mys[i])-1)) %>% 
      mutate(Corte = as.yearmon(mys[i])) %>% 
      mutate(CantCond12m = 1) %>% 
      group_by(Corte, Cuenta, Operacion) %>% 
      summarise(CantCond12m = sum(CantCond12m)) %>% 
      ungroup()
    
    bdcNC <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', meses[i],'.rds')) 
     
    girNew <- bdcNC %>%
      left_join(codMod, by="MODULO") %>% 
      dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
      mutate(SaldoBruto = ifelse(ctaCont %in% c('131','133','134','135','136','137'), saldous, 0)) %>% 
      mutate(SaldoMora = ifelse(ctaCont %in% c('133','134','136','137'), saldous, 0)) %>% 
      mutate(SaldoVigente = ifelse(ctaCont %in% c('131','135'), saldous, 0)) %>% 
      mutate(SaldoReprog = ifelse(ctaCont %in% c('135','136','137'), saldous, 0)) %>% 
      mutate(OpsBruta = ifelse(ctaCont %in% c('131','133','134','135','136','137'), 1, 0)) %>% 
      mutate(OpsMora = ifelse(ctaCont %in% c('133','134','136','137'), 1, 0)) %>% 
      mutate(OpsVigente = ifelse(ctaCont %in% c('131','135'), 1, 0)) %>% 
      mutate(OpsReprog = ifelse(ctaCont %in% c('135','136','137'), 1, 0)) %>% 
      mutate(OpsPar0 = ifelse(par0>0, 1, 0)) %>% 
      mutate(OpsCastigada = ifelse(saldoCast>0, 1, 0)) %>% 
      mutate(OpsDiferida = ifelse(saldoDif>0, 1, 0)) %>% 
      left_join(select(pt,-Corte), by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
      left_join(select(ct, -Corte), by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
      replace_na(list(CantCond12m=0, CantPagosTardios12m=0)) %>% 
      left_join(Clientes_Ajuste, by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(Objeto = case_when(str_detect(DESC_OBJCRED,"INVERSION")~"INVERSION",
                                str_detect(DESC_OBJCRED,"OPERACION")~"OPERACION",
                                TRUE~"OTROS")) %>% 
      mutate(Tipo_Credito = case_when(substr(TIPO_CREDITO,1,1) =='P' ~ 'Pyme',
                                      substr(TIPO_CREDITO,1,1) =='M' ~ 'Micro',
                                      substr(TIPO_CREDITO,1,1) =='N' ~ 'Consumo',
                                      TIPO_CREDITO %in% c('H0','H1','H2') ~ 'Vivienda Normal',
                                      TIPO_CREDITO %in% c('H3','H4') ~ 'Vivienda Social',)) %>% 
      mutate(PTyCond = case_when(CantCond12m>=9 & CantPagosTardios12m>=9 ~ "5. Alerta 9x9",
                                 CantCond12m>=4 & CantPagosTardios12m>=4 ~ "4. Alerta 4x4",
                                 CantCond12m>=1 & CantPagosTardios12m>=1 ~ "3. Alerta 1x1",
                                 CantCond12m>0 | CantPagosTardios12m>0 ~ "2. Tiene PT o Cond",
                                 TRUE ~ "1. Sin Alerta")) %>% 
      # mutate(ConteoPTyCond = case_when(CantCond12m>=12 & CantPagosTardios12m>=12 ~ '12x12',
      #                                      CantCond12m>=11 & CantPagosTardios12m>=11 ~ '11x11',
      #                                      CantCond12m>=10 & CantPagosTardios12m>=10 ~ '10x10',
      #                                      CantCond12m>=9 & CantPagosTardios12m>=9 ~ '9x9',
      #                                      CantCond12m>=8 & CantPagosTardios12m>=8 ~ '8x8',
      #                                      CantCond12m>=7 & CantPagosTardios12m>=7 ~ '7x7',
      #                                      CantCond12m>=6 & CantPagosTardios12m>=6 ~ '6x6',
      #                                      CantCond12m>=5 & CantPagosTardios12m>=5 ~ '5x5',
      #                                      CantCond12m>=4 & CantPagosTardios12m>=4 ~ '4x4',
      #                                      CantCond12m>=3 & CantPagosTardios12m>=3 ~ '3x3',
      #                                      CantCond12m>=2 & CantPagosTardios12m>=2 ~ '2x2',
    #                                      CantCond12m>=1 & CantPagosTardios12m>=1 ~ '1x1',)) %>% 
      mutate(CreditoNuevo = ifelse(year(fdes_original) < 2017, '< 2017',
                                   as.character(year(fdes_original)))) %>% 
      mutate(DesembolsoPandemia = case_when(fdes_original<as.Date("2020-03-01")~"1. Pre-pandemia",
                                            fdes_original<as.Date("2022-01-01")~"2. Pandemia",
                                            fdes_original>=as.Date("2022-01-01")~"3. Post-pandemia",
                                            TRUE~NA)) %>% 
      mutate(Tipo_Cartera = case_when(ctaCont %in% c('131','133','134') & OPERACION_ORI_REF==0 ~ 'Normal',
                                      ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                      OPERACION_ORI_REF!=0 ~ 'Refinanciada',)) %>%
      mutate(Fecha = as.Date(monDate, frac = 1)) %>% 
      mutate(categ = ifelse(SECTOR_CARTERA=='9.Vivienda No controlada','Otros',categ)) %>% 
      mutate(PlazoAnios = ifelse(floor(PLAZODIAS/365)<=7,as.character(floor(PLAZODIAS/365)),
                                 "> 7")) %>% 
      select(Fecha, GENERO, NOMBRE_MODULO, Sucursal, rangos, rangom,  Tipo_Credito, Objeto,
             Sector_Destino, Sector_Actividad, ctaCont, CreditoNuevo, DesembolsoPandemia,
             PTyCond, PlazoAnios, Tipo_Cartera, SaldoBruto, SaldoMora, SaldoVigente,
             SaldoReprog, SaldoDiferido=saldoDif, Par0=par0, PrevisionEspecifica=previus, 
             InteresAnual=intus, OpsBruta, OpsMora, OpsVigente, OpsReprog, OpsDiferida,
             OpsPar0) %>% 
      mutate(GENERO=ifelse(is.na(GENERO), 'J', GENERO)) %>% 
      group_by(Fecha, GENERO, NOMBRE_MODULO, Sucursal, rangos, rangom,  Tipo_Credito, 
               Objeto, Sector_Destino, Sector_Actividad, ctaCont, CreditoNuevo, 
               DesembolsoPandemia, PTyCond, PlazoAnios, Tipo_Cartera) %>%
      summarise_all(sum) %>% 
      ungroup()
    girList[[i]] <- girNew
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

girFull <- rbindlist(girList)
write_xlsx(girFull,'D:/!bso/girCartera/tablaGIR_mora_Desc_Oct2023.xlsx')
####____METRICAS POR CATEGORIA____#####
Sucursal <- girFull %>% 
  group_by(Fecha, Sucursal) %>% 
  summarise(across(c(SaldoBruto, Par0, OpsBruta, OpsPar0),~sum(.x)),
            IM0 = Par0/SaldoBruto) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = Sucursal, values_from = c(SaldoBruto, Par0), 
              names_from = Fecha)

####____NIVEL APROBACION CIC____####
cic <- readRDS("D:/!bso/CIC/rds/cic_Ago2023.rds")
cicNew <- cic %>% 
  mutate(NivelAutorizacionL = case_when(str_detect(NivelAutorizacion, coll("asesor", TRUE))~"ASESOR DE NEGOCIOS",
                                        str_detect(NivelAutorizacion, coll("encarg", TRUE))~"ENCARGADO DE NEGOCIOS",
                                        str_detect(NivelAutorizacion, fixed("SUBGERENTE", TRUE))~"SUBGERENTE REGIONAL PRODUCTIVO/ DE CREDITO",
                                        str_detect(NivelAutorizacion, coll("GERENTE DE AGENCIA", TRUE))~"GERENTE DE AGENCIA",
                                        str_detect(NivelAutorizacion, coll("GERENTE NACIONAL", TRUE))~"GERENTE NACIONAL/REGIONAL",
                                        str_detect(NivelAutorizacion, coll("GERENTE REGIONAL", TRUE))~"GERENTE NACIONAL/REGIONAL",
                                        str_detect(NivelAutorizacion, coll("Gerente", TRUE))~"GERENTE NACIONAL/REGIONAL",
                                        str_detect(NivelAutorizacion, coll("JEFE REGIONAL AGROPECUARIO", TRUE))~"JEFE REGIONAL AGROPECUARIO",
                                        str_detect(NivelAutorizacion, coll("RIESGO", TRUE))~"MIGRACION",
                                        TRUE ~ "ENCARGADO DE NEGOCIOS")) %>% 
  select(CTACLIENTE, OPERACION, NivelAutorizacion, NivelAutorizacionL)

table(cicNew$NivelAutorizacion)
table(cicNew$NivelAutorizacionL)
####____RatioMujeres CIC____####
cic <- readRDS("D:/!bso/CIC/rds/cic_Dic2022.rds")
cic <- cic %>% 
  dplyr::filter(CuentaContable.y %in% c('131','133','134','135','136','137')) %>% 
  dplyr::filter(is.na(FechaCancelacion)) %>% 
  mutate(PrevCiclica = ifelse(Moneda=="MN",PrevCiclica/6.86, PrevCiclica))

bdc <- readRDS("D:/!bso/girCartera/rds/ec_Dic2022.rds") %>% 
  dplyr:::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  select(CTACLIENTE, OPERACION, GENERO, par0, saldoMora, saldous, previus) %>% 
  left_join(cic, by=c("CTACLIENTE","OPERACION"))

ratioMujeres <- bdc %>% 
  group_by(Genero) %>% 
  summarise(SaldoBruto=sum(SaldoBruto),
            par0 = sum(par0),
            PrevCiclica = sum(PrevCiclica),
            PrevEspecifica = sum(PrevEspecifica))
ratioMujeres

####____SHIT THE FABIAN____####
bdc <- data.frame(id=1:30, cat = sample(c("a","b","c"),30,replace = T), 
                  Saldo1=sample(c(1:5,NA),30,replace = T),
                  Saldo2=sample(c(1:5,NA),30,replace = T),
                  Saldo3=sample(c(1:5,NA),30,replace = T),
                  Saldo4=sample(c(1:5,NA),30,replace = T))

bdclong <- bdc %>% 
  pivot_longer(cols = starts_with("Saldo"),names_to = "Fecha", values_drop_na = T)

bdc %>% 
  rowwise(data = dmy("20231031"))

####____SUCURSAL WORST AGENCIES____####
bdcDic <- readRDS("D:/!bso/girCartera/rds/ec_Dic2022.rds") %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  mutate(SaldoBruto = ifelse(ctaCont %in% c('131','133','134','135','136','137'), saldous, 0)) %>% 
  mutate(SaldoMora = ifelse(ctaCont %in% c('133','134','136','137'), saldous, 0)) %>% 
  mutate(SaldoVigente = ifelse(ctaCont %in% c('131','135'), saldous, 0)) %>% 
  mutate(SaldoReprog = ifelse(ctaCont %in% c('135','136','137'), saldous, 0)) %>% 
  mutate(OpsBruta = ifelse(ctaCont %in% c('131','133','134','135','136','137'), 1, 0)) %>% 
  mutate(OpsMora = ifelse(ctaCont %in% c('133','134','136','137'), 1, 0)) %>% 
  mutate(OpsVigente = ifelse(ctaCont %in% c('131','135'), 1, 0)) %>% 
  mutate(OpsReprog = ifelse(ctaCont %in% c('135','136','137'), 1, 0)) %>% 
  mutate(OpsPar0 = ifelse(par0>0, 1, 0)) %>% 
  mutate(OpsCastigada = ifelse(saldoCast>0, 1, 0)) %>% 
  mutate(OpsDiferida = ifelse(saldoDif>0, 1, 0)) %>% 
  select(CTACLIENTE, OPERACION, Sucursal, NOMBRE_AGENCIA, SaldoBruto, SaldoMora, par0,
         SaldoVigente, SaldoReprog, OpsBruta, OpsMora, OpsVigente, OpsReprog, OpsPar0)
bdcAgo <- readRDS("D:/!bso/girCartera/rds/ec_Ago2023.rds") %>% 
  dplyr::filter(ctaCont %in% c('131','133','134','135','136','137')) %>% 
  mutate(SaldoBruto = ifelse(ctaCont %in% c('131','133','134','135','136','137'), saldous, 0)) %>% 
  mutate(SaldoMora = ifelse(ctaCont %in% c('133','134','136','137'), saldous, 0)) %>% 
  mutate(SaldoVigente = ifelse(ctaCont %in% c('131','135'), saldous, 0)) %>% 
  mutate(SaldoReprog = ifelse(ctaCont %in% c('135','136','137'), saldous, 0)) %>% 
  mutate(OpsBruta = ifelse(ctaCont %in% c('131','133','134','135','136','137'), 1, 0)) %>% 
  mutate(OpsMora = ifelse(ctaCont %in% c('133','134','136','137'), 1, 0)) %>% 
  mutate(OpsVigente = ifelse(ctaCont %in% c('131','135'), 1, 0)) %>% 
  mutate(OpsReprog = ifelse(ctaCont %in% c('135','136','137'), 1, 0)) %>% 
  mutate(OpsPar0 = ifelse(par0>0, 1, 0)) %>% 
  mutate(OpsCastigada = ifelse(saldoCast>0, 1, 0)) %>% 
  mutate(OpsDiferida = ifelse(saldoDif>0, 1, 0)) %>% 
  select(CTACLIENTE, OPERACION, Sucursal, NOMBRE_AGENCIA, SaldoBruto, SaldoMora, par0,
         SaldoVigente, SaldoReprog, OpsBruta, OpsMora, OpsVigente, OpsReprog, OpsPar0)
  

bdcJoin <- bdcDic %>% 
  full_join(bdcAgo, by=c("CTACLIENTE","OPERACION"),
            suffix=c("_old","_new"))

write_xlsx(bdcJoin, "D:/!bso/girCartera/tablaMora_Agencias.xlsx")
