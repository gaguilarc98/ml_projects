daily <- readRDS(paste0("D:/!bso/vipCartera/dailyMM/dailyMM_Abr2023.rds"))

moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Jun2023.rds")) %>% 
  dplyr::filter(monDate!="jun. 2023") %>% #Filtro para reproducir meses anteriores
  dplyr::filter(!is.na(maximaDPD)) %>% 
  group_by(CTACLIENTE) %>% 
  # mutate(mesDiasMora = if_else(maximaDPD==0, 2014, year(monDate))) %>%
  mutate(tuvoMora = ifelse(maximaDPD>0, 1, 0)) %>% 
  summarise(meanMoraIM_cl = mean(maximaDPD),
            tuvoMora_pct = sum(tuvoMora)/n()) %>% #lastMoraIM_cl = max(mesDiasMora), maxMoraIM_cl = max(maximaDPD)
  ungroup()

dAux <- readRDS(paste0("//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/vipCartera/rds/cons_202304.rds")) %>% 
  select(CTACLIENTE, OPERACION, DIASMORA) %>% 
  group_by(CTACLIENTE, OPERACION) %>% 
  summarise(maxMoraIM_cl = max(DIASMORA, na.rm = T)) %>% 
  ungroup()

moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Jun2023.rds")) %>% 
  dplyr::filter(monDate=="abr. 2023")

MoraJoin <- moraIntraMes %>% 
  left_join(dAux, by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(difMora = maxMoraIM_cl-maximaDPD) %>% 
  mutate(checkMora = maxMoraIM_cl!=maximaDPD) 

bdc <- readRDS(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/MIS_SGNRFAR/analitica/rds_Diario/ec_20230420.rds')) %>% 
  mutate(dayDate = as.Date(dayDate))