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
library(data.table)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____FUNCION TREATMENT____####
ResumenCartera <- function(x){
  cols <- c(SALDO_CAPITAL_DIFERIDO=0, saldoDif=0, sex=0)
  x <- x %>% 
    add_column(!!!cols[setdiff(names(cols),names(x))]) %>% 
    dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','623','865')) %>% 
    mutate(Sector_Actividad = case_when(Sector_Actividad=='H. Comercio' & divCaedecC %in% c('50','51')~'H1. Ventas al por mayor',
                                        Sector_Actividad=='H. Comercio' & divCaedecC %in% c('52')~'H2. Ventas al por menor',
                                        TRUE~Sector_Actividad)) %>% 
    mutate(Sector_Destino = case_when(Sector_Destino=='H. Comercio' & divCaedecD %in% c('50','51')~'H1. Ventas al por mayor',
                                        Sector_Destino=='H. Comercio' & divCaedecD %in% c('52')~'H2. Ventas al por menor',
                                        TRUE~Sector_Destino)) %>% 
    mutate(Tipo_Credito = case_when(substr(TIPO_CREDITO,1,1) =='C' ~ 'Empresarial',
                                    substr(TIPO_CREDITO,1,1) =='P' ~ 'Pyme',
                                    substr(TIPO_CREDITO,1,1) =='M' ~ 'Micro',
                                    substr(TIPO_CREDITO,1,1) =='N' ~ 'Consumo',
                                    TIPO_CREDITO %in% c('H0','H1','H2') ~ 'Vivienda Normal',
                                    TIPO_CREDITO %in% c('H3','H4') ~ 'Vivienda Social',)) %>% 
    mutate(Deb_Garantizado = ifelse(TIPO_CREDITO %in% c('C2','H0','H2','H3','M1','M2','M4','M6',
                                                        'M8','M9','N1','N2','P3','P4','P6','P9'), 1, 0)) %>% 
    mutate(Rango_Saldo = case_when(saldous <= 7e3 ~ '1. < 7 MM USD',
                                   saldous <= 15e3 ~ '2. 7 a 15 MM USD',
                                   saldous > 15e3 ~ '3. > 15 MM USD',)) %>% 
    mutate(Sector_Cartera = case_when(SECTOR_CARTERA %in% c("1.Prod. Agropec. Controlada","2.Otra prod. Controlada","3.C2.Sector Turismo",
                                                            "4.C3.Prod Intelectual","5.C4.Fab,Ens.,Vent.MaqAutHib") ~ 'Productivo TC',
                                      SECTOR_CARTERA %in% c("7.Prod.Agropec.No Controlada","8.Otra Prod.No Controlada") ~ 'Productivo TNC',
                                      SECTOR_CARTERA == "6.Vivienda Controlada" ~ "Vivienda TC",
                                      TRUE ~ 'Otros')) %>% 
    mutate(Productivo = ifelse(Sector_Cartera %in% c("Productivo TC","Productivo TNC"), 'S', 'N')) %>% 
    mutate(Tipo_Cartera = case_when(ctaCont %in% c('131','133','134') & OPERACION_ORI_REF==0 ~ 'Normal',
                                    ctaCont %in% c('135','136','137') & OPERACION_ORI_REF==0 ~ 'Reprogramada',
                                    OPERACION_ORI_REF!=0 ~ 'Refinanciada',
                                    ctaCont == '865' ~ 'Castigada')) %>% 
    mutate(Es_Diferida = ifelse(saldoDif > 0, 1, 0)) %>% 
    mutate(Es_Refinanciada = ifelse(OPERACION_ORI_REF != 0, 1, 0)) %>% 
    mutate(Es_Reprogramada = ifelse(ctaCont %in% c('135','136','137'), 1, 0)) %>% 
    mutate(Desembolsos_Genuinos = ifelse(cosechaM==monDate & OPERACION_ORI_REF==0 & 
                                           ctaCont %in% c('131','133','134'), 1, 0)) %>% 
    mutate(Desembolsos_Genuinos_Ref_sDif = ifelse(cosechaM==monDate & (OPERACION_ORI_REF==0 | OPERACION_ORI_REF!=OPERACION) & 
                                           ctaCont %in% c('131','133','134'), 1, 0)) %>% 
    mutate(Monto_Total = ifelse(MONEDA == 0, as.numeric(MONTO)/6.86, as.numeric(MONTO))) %>%
    mutate(Monto_Desembolsado_Genuino = ifelse(Desembolsos_Genuinos==1, Monto_Total, 0)) %>%
    mutate(Operaciones_Castigadas = ifelse(ctaCont=='865',1,0)) %>% 
    select(monDate,  CTACLIENTE, OPERACION, FFINALIZA, ctaCont, Sector_Actividad, 
           Sector_Destino, Tipo_Credito, Deb_Garantizado, Sucursal, Sector_Cartera, 
           Productivo, Tipo_Cartera, Es_Diferida, Es_Refinanciada, Es_Reprogramada,
           Monto_Total, Monto_Desembolsado_Genuino,Monto_Desembolsado = montous, 
           Operaciones_Totales=opTot, Operaciones_Desembolsadas=opDes, 
           Desembolsos_Genuinos, Desembolsos_Genuinos_Ref_sDif,
           Operaciones_Castigadas, Cartera_Bruta = saldous, Cartera_Castigada=saldoCast,
           Cartera_Diferida = saldoDif, PaR0_Bruta=par0, PaR30_Contable=saldoMora, 
           Previsión_USD=previus, Interés_Anual=intus)
  return(x)
}
####____CONSTRUCCION DE TABLA GIR____####
year <- c(2019:2023)
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
i <- 58
bdcList <- list()
for (i in 13:length(myrds)) {
  tryCatch({
    print(myrds[i])
    if(i==13){
      bdcNC <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i-1],'.rds')) 
      bdc_prev <- bdcNC %>% 
        ResumenCartera()  
    }else{
      bdc_prev <- bdc_next
    }
    bdcNC <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[i],'.rds')) 
    bdc_next <- bdcNC %>%
      ResumenCartera()  
    
    bdc_next_new <- bdc_prev %>% 
      anti_join(bdc_next, by = c("CTACLIENTE","OPERACION")) %>% 
      # dplyr::filter(ctaCont!='865') %>% 
      mutate(monDate = monDate+1/12) %>% 
      mutate(Salida_Regular = ifelse(as.yearmon(FFINALIZA)==monDate & ctaCont!='865', 1, 0)) %>% 
      mutate(Salida_Anticipada = ifelse(as.yearmon(FFINALIZA)>monDate & ctaCont!='865', 1, 0)) %>% 
      mutate(Salida_Intempestiva = ifelse(as.yearmon(FFINALIZA)<monDate & ctaCont!='865', 1, 0)) %>%
      mutate(Salida_Castigo = ifelse(ctaCont=='865', 1, 0)) %>%
      mutate(Saldo_Salida_Regular = ifelse(Salida_Regular==1, Cartera_Bruta, 0)) %>% 
      mutate(Saldo_Salida_Anticipada = ifelse(Salida_Anticipada==1, Cartera_Bruta, 0)) %>% 
      mutate(Saldo_Salida_Intempestiva = ifelse(Salida_Intempestiva==1, Cartera_Bruta, 0)) %>%
      mutate(Saldo_Salida_Castigo = ifelse(ctaCont=='865', Cartera_Castigada, 0)) %>%
      mutate(across(c(Monto_Total:Interés_Anual),~ 0)) %>% 
      bind_rows(bdc_next)
    
    x <- bdc_next_new %>% 
      left_join(select(bdc_prev, CTACLIENTE, OPERACION, ctaCont, Cartera_Bruta),
                by = c("CTACLIENTE","OPERACION"), suffix=c("_new","_old")) %>% 
      mutate(Saldo_Pagado = Cartera_Bruta_old-Cartera_Bruta_new) %>% 
      mutate(Saldo_Adicional = ifelse(Saldo_Pagado<0, -Saldo_Pagado,0)) %>% 
      mutate(Saldo_Pagado = ifelse(Saldo_Pagado>0, Saldo_Pagado,0)) %>% 
      mutate(Saldo_Castigo_Nuevo = ifelse(ctaCont_old !='865' & ctaCont_new=='865',Cartera_Bruta_old,0)) %>% 
      mutate(Castigo_Nuevo = ifelse(ctaCont_old !='865' & ctaCont_new=='865',1,0)) %>% 
      select(-Cartera_Bruta_old, -ctaCont_old) %>% 
      rename(ctaCont=ctaCont_new, Cartera_Bruta = Cartera_Bruta_new) 
      
    bdc_RC <- x %>% 
      select(-CTACLIENTE, -OPERACION, -FFINALIZA) %>% 
      group_by(monDate,ctaCont, Sector_Actividad, Sector_Destino, Tipo_Credito, 
               Deb_Garantizado, Sucursal, Sector_Cartera, Productivo, 
               Tipo_Cartera, Es_Diferida, Es_Refinanciada, Es_Reprogramada) %>% 
      summarise_all(sum, na.rm=T) %>% 
      ungroup()
    
    bdcList[[i]] <- bdc_RC
      
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

bdcFull <- rbindlist(bdcList)

check <- bdcFull %>% 
  group_by(monDate) %>% 
  summarise(CB=sum(Cartera_Bruta),Nops=sum(Operaciones_Totales),
            PaR0=sum(PaR0_Bruta), PaR30=sum(PaR30_Contable), CR=sum(Salida_Regular),
            CA=sum(Salida_Anticipada), CI=sum(Salida_Intempestiva),
            OpsCast=sum(Operaciones_Castigadas), CC=sum(Salida_Castigo),
            SCR= sum(Saldo_Salida_Regular), SCA=sum(Saldo_Salida_Anticipada),
            SCI=sum(Saldo_Salida_Intempestiva), SCC= sum(Saldo_Salida_Castigo),
            SCancel=SCR+SCA+SCI+SCC, CN=sum(Castigo_Nuevo), SCN=sum(Saldo_Castigo_Nuevo))

#OJO QUE ESTO NO COINCIDE CON EL SALDOCANCEL DE CONSULTAS Y COMPRA DE CARTERA
#PORQUE SE EXCLUYEN BOLETAS DE GARANTÍA
bdcFull <- bdcFull %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  rename(Fecha=monDate)
  
write_xlsx(bdcFull,"D:/!bso/girCartera/tablaGIR/Tabla_RC_Ago2023.xlsx")

bdcprev %>% summarise(NCTAS=n_distinct(CTACLIENTE), NCI = n_distinct(CI), NCI_fixed = n_distinct(CI_fixed))  
####____ACTUALIZACION DE UN MES____####
bdcFull <- read_xlsx("D:/!bso/girCartera/tablaGIR/Tabla_RC_Sep2023.xlsx")
#Correr el loop para el mes correspondiente

bdc_RC <- bdc_RC %>% 
  mutate(monDate = as.Date(monDate, frac=1)) %>% 
  rename(Fecha=monDate)

bdcFull <- bdcFull %>% 
  bind_rows(bdc_RC)

write_xlsx(bdcFull,"D:/!bso/girCartera/tablaGIR/Tabla_RC_Oct2023.xlsx")
####____ALE____####
n <- length(mes)
i <- 6
k <- 7
girList <- list()
bdcprev <- readRDS("D:/!bso/girCartera/rds/ec_Jun2023.rds") %>% 
  dplyr::filter(ctaCont %in% c("131",'133','134','135','136','137')) %>% 
  mutate(NDOC = str_replace(CI, "LP$|OR$|PO$|CB$|CH$|TJ$|SC$|BE$|PA$","")) %>%
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"),1,0)) %>% 
  group_by(NDOC) %>%
  arrange(desc(FDESEMBOLSO)) %>%
  mutate(CI_fixed = CI[row_number()==1]) %>%
  ungroup()

####CUADRE CDGNP CDGP
y <- bdc_next %>% 
  group_by(monDate,ctaCont, TIPO_CREDITO, Tipo_Credito, Productivo, Deb_Garantizado) %>% 
  summarise(Saldo = sum(Cartera_Bruta))

write_xlsx(y, "D:/!bso/TIPOCRED_dic2022.xlsx")
