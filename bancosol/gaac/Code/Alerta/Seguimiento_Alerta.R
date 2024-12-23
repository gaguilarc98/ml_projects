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
library(arrow)
library(ggplot2)
library(ggrepel)
library(openxlsx)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____LISTA DE CLIENTES EN ALERTA_____####
#El objetivo es obtener un historial de operaciones en alerta con corte
#al momento de ingreso y el último corte de cada operación.
####____PAGOS TARDIOS Y CONDONACION____####
ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Oct23.rds")
condFull <- read_parquet('D:/!bso/condonaciones/CondFull.parquet')
cond_clean <- condFull %>% 
  select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int, CondCap_USD = Cond_Cap) %>%
  mutate(myCond = as.yearmon(Fecha)) %>% 
  group_by(myCond, Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(Fecha),
            across(starts_with("Cond"), ~sum(.x))) %>% 
  ungroup()
generatePTk <- function(x, mini, mfin){
  x %>% 
    dplyr::filter(myPago>=as.yearmon(mini) & myPago<=mfin) %>% 
    group_by(Operacion) %>% 
    mutate(Ult12MesesPagoTardio = n()) %>% 
    ungroup() %>% 
    mutate(Cierre = as.yearmon(mfin)) %>% 
    group_by(Cierre, Cuenta, Operacion) %>% 
    summarise(CantPT12Meses = max(Ult12MesesPagoTardio)) %>% 
    ungroup()
}
generateCondk <- function(x, mini, mfin){
  x <- cond_clean %>% 
    dplyr::filter(myCond>=as.yearmon(mini) & myCond<=mfin) %>% 
    mutate(Cierre = as.yearmon(mfin)) %>% 
    group_by(Cierre, Cuenta, Operacion) %>% 
    summarise(CantCond12Meses = n()) %>% 
    ungroup()
}
####____HISTORIAL____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
myfecha <- as.vector(sapply(year,function(x){paste0(mes,". ",x)})) #lista de meses-años para abrir

i <- 18
for (i in 17:22) {
  tryCatch({
    print(myrds[i])
    
    pt_last12 <- generatePTk(ptFull, myfecha[i-11], myfecha[i])
    cond_last12 <- generateCondk(cond_clean, myfecha[i-11], myfecha[i])
    
    infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',myrds[i],'.rds'))
    infoClean <- infoCheck %>% 
      dplyr::filter(REGULADO=="SBEF") %>% 
      dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>% 
      mutate(esBSO=ifelse(SIGLA=='BSO',1,0)) %>%
      mutate(noesBSO=ifelse(SIGLA!='BSO',1,0)) %>%
      mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>%
      group_by(CI) %>%
      dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
      mutate(califPeorSF = max(CALIFICACION[esBSO == 0])) %>% 
      mutate(ESTADOPeorSF = max(ESTADO[esBSO == 0])) %>% 
      mutate(MaxDiasMoraSF = max(DiasMora[esBSO == 0])) %>% 
      ungroup() %>% 
      dplyr::filter(califPeorSF!="_" & SIGLA=="BSO") %>% 
      select(CTACLIENTE, OPERACION, califPeorSF, ESTADOPeorSF, MaxDiasMoraSF)
    
    if(i==17){
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                            myrds[i],'.rds')) %>% 
        dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>% 
        left_join(infoClean, by=c("CTACLIENTE","OPERACION")) %>% 
        left_join(pt_last12, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
        left_join(cond_last12, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
        mutate(monDate = as.Date(monDate, frac=1)) %>% 
        mutate(saldous = ifelse(ctaCont=='865', saldoCast, saldous)) %>% 
        mutate(Migrada = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"), 1, 0)) %>% 
        mutate(NXN = case_when(CantPT12Meses>=12 & CantCond12Meses>=12 ~ '12x12',
                               CantPT12Meses>=11 & CantCond12Meses>=11 ~ '11x11',
                               CantPT12Meses>=10 & CantCond12Meses>=10 ~ '10x10',
                               CantPT12Meses>=9 & CantCond12Meses>=9 ~ '09x09',
                               CantPT12Meses>=8 & CantCond12Meses>=8 ~ '08x08',
                               CantPT12Meses>=7 & CantCond12Meses>=7 ~ '07x07',
                               CantPT12Meses>=6 & CantCond12Meses>=6 ~ '06x06',
                               CantPT12Meses>=5 & CantCond12Meses>=5 ~ '05x05',
                               CantPT12Meses>=4 & CantCond12Meses>=4 ~ '04x04',
                               CantPT12Meses>=3 & CantCond12Meses>=3 ~ '03x03',
                               CantPT12Meses>=2 & CantCond12Meses>=2 ~ '02x02',
                               CantPT12Meses>=1 & CantCond12Meses>=1 ~ '01x01',)) %>% 
        mutate(Alerta_AR = ifelse(NXN %in% c("04x04","05x05","06x06","07x07","08x08","09x09","10x10","11x11","12x12") 
                               | (!is.na(ESTADOPeorSF) & ESTADOPeorSF %in% c("2. VENCIDA","3. EJECUCION","4. CASTIGADA")), 1, 0)) %>% 
        dplyr::filter(Alerta_AR == 1) %>% 
        select(CTACLIENTE, OPERACION, Sucursal, AGENCIA, NOMBRE_AGENCIA, ASESOR, 
               NOMBRE_ASESOR, Migrada, FechaIngreso=monDate, CantPT12MesesIngreso=CantPT12Meses,
               CantCond12MesesIngreso=CantCond12Meses, NXNIngreso=NXN, PeorEstadoSFIngreso=ESTADOPeorSF,
               PeorCalifSFIngreso=califPeorSF, Alerta_ARIngreso=Alerta_AR, SaldoIngreso=saldous, 
               MoraIngreso=par0, CtaContIngreso = ctaCont) 
      dfTotal <- df1 %>%
        mutate(FechaUltimo = FechaIngreso) %>% 
        mutate(CantPT12MesesUltimo = CantPT12MesesIngreso) %>% 
        mutate(CantCond12MesesUltimo = CantCond12MesesIngreso) %>% 
        mutate(NXNUltimo = NXNIngreso) %>% 
        mutate(PeorEstadoSFUltimo = PeorEstadoSFIngreso) %>% 
        mutate(PeorCalifSFUltimo = PeorCalifSFIngreso) %>% 
        mutate(Alerta_ARUltimo = Alerta_ARIngreso) %>% 
        mutate(SaldoUltimo = SaldoIngreso) %>%
        mutate(MoraUltimo = MoraIngreso) %>% 
        mutate(CtaContUltimo = CtaContIngreso)
    }else{
      df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',
                            myrds[i],'.rds')) %>% 
        dplyr::filter(ctaCont %in% c('131','133','134','135','136','137','865')) %>% 
        left_join(infoClean, by=c("CTACLIENTE","OPERACION")) %>% 
        left_join(pt_last12, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
        left_join(cond_last12, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
        mutate(monDate = as.Date(monDate, frac=1)) %>% 
        mutate(saldous = ifelse(ctaCont=='865', saldoCast, saldous)) %>% 
        mutate(Migrada = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"), 1, 0)) %>% 
        mutate(NXN = case_when(CantPT12Meses>=12 & CantCond12Meses>=12 ~ '12x12',
                               CantPT12Meses>=11 & CantCond12Meses>=11 ~ '11x11',
                               CantPT12Meses>=10 & CantCond12Meses>=10 ~ '10x10',
                               CantPT12Meses>=9 & CantCond12Meses>=9 ~ '09x09',
                               CantPT12Meses>=8 & CantCond12Meses>=8 ~ '08x08',
                               CantPT12Meses>=7 & CantCond12Meses>=7 ~ '07x07',
                               CantPT12Meses>=6 & CantCond12Meses>=6 ~ '06x06',
                               CantPT12Meses>=5 & CantCond12Meses>=5 ~ '05x05',
                               CantPT12Meses>=4 & CantCond12Meses>=4 ~ '04x04',
                               CantPT12Meses>=3 & CantCond12Meses>=3 ~ '03x03',
                               CantPT12Meses>=2 & CantCond12Meses>=2 ~ '02x02',
                               CantPT12Meses>=1 & CantCond12Meses>=1 ~ '01x01',)) %>% 
        mutate(Alerta_AR = ifelse(NXN %in% c("04x04","05x05","06x06","07x07","08x08","09x09","10x10","11x11","12x12") 
                                  | (!is.na(ESTADOPeorSF) & ESTADOPeorSF %in% c("2. VENCIDA","3. EJECUCION","4. CASTIGADA")), 1, 0)) %>% 
        # dplyr::filter(Alerta_AR == 1) %>% 
        select(CTACLIENTE, OPERACION, Sucursal, AGENCIA, NOMBRE_AGENCIA, ASESOR, 
               NOMBRE_ASESOR, Migrada, monDate, CantPT12Meses, CantCond12Meses,
               NXN, ESTADOPeorSF, califPeorSF, Alerta_AR, saldous, par0, ctaCont)
      dfNew <- df1 %>% 
        anti_join(dfTotal, by = c("CTACLIENTE","OPERACION")) %>% 
        dplyr::filter(Alerta_AR == 1) %>% 
        rename(FechaIngreso=monDate, CantPT12MesesIngreso=CantPT12Meses,
               CantCond12MesesIngreso=CantCond12Meses, NXNIngreso=NXN, PeorEstadoSFIngreso=ESTADOPeorSF,
               PeorCalifSFIngreso=califPeorSF, Alerta_ARIngreso=Alerta_AR, SaldoIngreso=saldous, 
               MoraIngreso=par0, CtaContIngreso = ctaCont) %>% 
        mutate(FechaUltimo = FechaIngreso) %>% 
        mutate(CantPT12MesesUltimo = CantPT12MesesIngreso) %>% 
        mutate(CantCond12MesesUltimo = CantCond12MesesIngreso) %>% 
        mutate(NXNUltimo = NXNIngreso) %>% 
        mutate(PeorEstadoSFUltimo = PeorEstadoSFIngreso) %>% 
        mutate(PeorCalifSFUltimo = PeorCalifSFIngreso) %>% 
        mutate(Alerta_ARUltimo = Alerta_ARIngreso) %>% 
        mutate(SaldoUltimo = SaldoIngreso) %>%
        mutate(MoraUltimo = MoraIngreso) %>% 
        mutate(CtaContUltimo = CtaContIngreso)
        
      dfUpdate <- df1 %>% 
        select(-Migrada)
      dfOld <- dfTotal %>% 
        inner_join(dfUpdate,by=c("CTACLIENTE","OPERACION"), suffix = c('_old','_new')) %>% 
        mutate(FechaUltimo = monDate) %>% 
        mutate(CantPT12MesesUltimo = CantPT12Meses) %>% 
        mutate(CantCond12MesesUltimo = CantCond12Meses) %>% 
        mutate(NXNUltimo = NXN) %>% 
        mutate(PeorEstadoSFUltimo = ESTADOPeorSF) %>% 
        mutate(PeorCalifSFUltimo = califPeorSF) %>% 
        mutate(Alerta_ARUltimo = Alerta_AR) %>% 
        mutate(SaldoUltimo = saldous) %>%
        mutate(MoraUltimo = par0) %>% 
        mutate(CtaContUltimo = ctaCont) %>% 
        select(CTACLIENTE, OPERACION, Sucursal=Sucursal_new, AGENCIA=AGENCIA_new,
               NOMBRE_AGENCIA=NOMBRE_AGENCIA_new, ASESOR=ASESOR_new, NOMBRE_ASESOR=NOMBRE_ASESOR_new,
               Migrada, ends_with("Ingreso"),ends_with("Ultimo"))
      dfCancel <- dfTotal %>% 
        anti_join(df1,by=c("CTACLIENTE","OPERACION"))
      
      dfTotal <- dfCancel %>% 
        bind_rows(dfOld) %>% 
        bind_rows(dfNew)
    }
    if(i>=21){
      write_xlsx(dfTotal, paste0('D:/!bso/alerta/OpsAlerta_May2023',myrds[i],'.xlsx'))
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
