bdc <- readRDS("D:/!bso/girCartera/rds/ec_Jun2023.rds")

Calif_FSL <- bdc %>%
  group_by(CTACLIENTE) %>% 
  mutate(CantidadOperaciones = n()) %>% 
  mutate(PeorCalificacionBSO = max(CALIFICACION,na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::filter(MODULO==118 | (MODULO==121 & str_detect(TIPO_OPER,'MIGR'))) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION, MODULO, TIPO_OPER, FVEN_ULTPAGO,
         FVEN_PROXPAGO, FULT_PAGO, TIPO_CREDITO, DIASMORA, ctaCont, SECTOR_CARTERA,
         CantidadOperaciones, PeorCalificacionBSO, SALDO) %>% 
  # group_by(CALIFICACION) %>% 
  # summarise(Operaciones = n()) %>% 
  glimpse()

write_xlsx(Calif_FSL,"D:/!bso/Calificacion_Migrados_Jun2023.xlsx")
