tm_ops <- read.csv("D:/!bso/transMat/Oreports/tmAll_Jun23.csv") %>% 
  mutate(monDate = as.yearmon(monDate)) %>% 
  # dplyr::filter(monDate!="Abr. 2023") %>% 
  glimpse()
#TRANSICION PREVIA
myrds <- c("May2023","Jun2023") #Colocar los meses adecuados de acuerdo con el lag

df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[1], '.rds')) %>%  #se abre mes anterior
  # dplyr::filter(!(MODULO==118 | (MODULO ==121 & str_detect(TIPO_OPER,"MIGR")))) %>% #Sin Cartera Migrada
  dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>% 
  mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S", CALIFICACION)) %>%
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"), 1, 0)) %>% 
  mutate(yearDes = year(FDESEMBOLSO)) %>% 
  mutate(yearmonDes = as.yearmon(FDESEMBOLSO)) %>% 
  mutate(TipoCartera = case_when(OPERACION_ORI_REF!=0~'Refin',
                                 ctaCont %in% c('135','136','137')~'Repro',
                                 TRUE~'Normal')) %>% 
  select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus, esFSL, 
         yearDes, yearmonDes, TipoCartera, tipoCred, Sucursal)

df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[2], '.rds')) %>%  #se abre mes posterior 
  # dplyr::filter(!(MODULO==118 | (MODULO ==121 & str_detect(TIPO_OPER,"MIGR")))) %>% #Sin Cartera Migrada
  dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>%
  mutate(saldous=ifelse(saldoCast>0, saldoCast, saldous)) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S",CALIFICACION)) %>% 
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"), 1, 0)) %>% 
  mutate(yearDes = year(FDESEMBOLSO)) %>% 
  mutate(yearmonDes = as.yearmon(FDESEMBOLSO)) %>% 
  mutate(TipoCartera = case_when(OPERACION_ORI_REF!=0~'Refin',
                                 ctaCont %in% c('135','136','137')~'Repro',
                                 TRUE~'Normal')) %>% 
  select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus, esFSL, 
         yearDes, yearmonDes, TipoCartera, tipoCred, Sucursal)

dfCancel <- df1 %>% 
  anti_join(df2,by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(CALIFICACION = "Z") %>% 
  mutate(saldous = 0) %>% 
  mutate(previus = 0) %>% 
  mutate(monDate = monDate+1/12) %>% 
  bind_rows(df2)

dfTrans_prev <- df1 %>% 
  left_join(dfCancel, by=c("CTACLIENTE","OPERACION"), suffix = c("_ini","_fin")) %>% 
  mutate(trans = paste(CALIFICACION_ini, CALIFICACION_fin,sep="-")) %>% 
  mutate(difPrev = previus_fin - previus_ini) %>% 
  mutate(difSaldo = saldous_fin - saldous_ini) %>% 
  mutate(one = 1) %>% 
  select(CTACLIENTE, OPERACION, trans_prev=trans)
#TRANSICION ACTUAL


myrds <- c("Jun2023","Jul2023") #Colocar los meses adecuados de acuerdo con el lag

df1 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[1], '.rds')) %>%  #se abre mes anterior
  # dplyr::filter(!(MODULO==118 | (MODULO ==121 & str_detect(TIPO_OPER,"MIGR")))) %>% #Sin Cartera Migrada
  dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>% 
  mutate(saldous = ifelse(saldoCast>0, saldoCast, saldous)) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S", CALIFICACION)) %>%
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"), 1, 0)) %>% 
  mutate(yearDes = year(FDESEMBOLSO)) %>% 
  mutate(yearmonDes = as.yearmon(FDESEMBOLSO)) %>% 
  mutate(TipoCartera = case_when(OPERACION_ORI_REF!=0~'Refin',
                                 ctaCont %in% c('135','136','137')~'Repro',
                                 TRUE~'Normal')) %>% 
  select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus, esFSL, 
         yearDes, yearmonDes, TipoCartera, tipoCred, Sucursal)

df2 <- readRDS(paste0('D:/!bso/girCartera/rds/ec_', myrds[2], '.rds')) %>%  #se abre mes posterior 
  # dplyr::filter(!(MODULO==118 | (MODULO ==121 & str_detect(TIPO_OPER,"MIGR")))) %>% #Sin Cartera Migrada
  dplyr::filter(CALIFICACION %in% c('A','B','C','D','E','F')) %>%
  mutate(saldous=ifelse(saldoCast>0, saldoCast, saldous)) %>% 
  mutate(CALIFICACION = ifelse(ESTADO == "CASTIGADA","S",CALIFICACION)) %>% 
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER,"MIGR"), 1, 0)) %>% 
  mutate(yearDes = year(FDESEMBOLSO)) %>% 
  mutate(yearmonDes = as.yearmon(FDESEMBOLSO)) %>% 
  mutate(TipoCartera = case_when(OPERACION_ORI_REF!=0~'Refin',
                                 ctaCont %in% c('135','136','137')~'Repro',
                                 TRUE~'Normal')) %>% 
  select(OPERACION, CTACLIENTE, CALIFICACION, monDate, saldous, previus, esFSL, 
         yearDes, yearmonDes, TipoCartera, tipoCred, Sucursal)

dfCancel <- df1 %>% 
  anti_join(df2,by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(CALIFICACION = "Z") %>% 
  mutate(saldous = 0) %>% 
  mutate(previus = 0) %>% 
  mutate(monDate = monDate+1/12) %>% 
  bind_rows(df2)

dfTrans <- df1 %>% 
  left_join(dfCancel, by=c("CTACLIENTE","OPERACION"), suffix = c("_ini","_fin")) %>% 
  mutate(trans = paste(CALIFICACION_ini, CALIFICACION_fin,sep="-")) %>% 
  mutate(difPrev = previus_fin - previus_ini) %>% 
  mutate(difSaldo = saldous_fin - saldous_ini) %>% 
  mutate(one = 1) %>% 
  left_join(dfTrans_prev,by=c("CTACLIENTE","OPERACION"))

x <- dfTrans %>% 
  dplyr::filter(CALIFICACION_ini=="B") %>% 
  mutate(TotalOps = n()) %>% 
  group_by(trans, trans_prev) %>% 
  summarise(Ops = n()/max(TotalOps)) %>% 
  pivot_wider(names_from = trans_prev, values_from = Ops, values_fill = 0) %>% 
  adorn_totals("col")

dfTransExp <- dfTrans %>% 
  select(monDate_ini, trans,  CALIFICACION_ini, CALIFICACION_fin, esFSL_ini, yearDes_ini, yearmonDes_ini, TipoCartera_ini, tipoCred_ini, 
         Sucursal_ini, trans_prev, saldous_ini, one) %>% 
  mutate(monDate_ini= as.Date(monDate_ini, frac=1)) %>% 
  group_by(monDate_ini, trans,  CALIFICACION_ini, CALIFICACION_fin, esFSL_ini, yearDes_ini, yearmonDes_ini, 
           TipoCartera_ini, tipoCred_ini, Sucursal_ini, trans_prev) %>% 
  summarise_all(sum) 

write_xlsx(dfTransExp, "D:/!bso/transMat/Explicacion_highBA.xlsx")
