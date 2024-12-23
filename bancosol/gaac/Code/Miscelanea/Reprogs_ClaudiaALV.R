mybdc <- c('Abr2023','May2023','Jun2023')
reproList <- list()
for (i in 1:length(mybdc)) {
  bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",mybdc[i],".rds")) %>% 
    dplyr::filter(ctaCont %in% c('135','136','137')) %>% 
    mutate(FECHA = as.Date(monDate, frac=1)) %>% 
    mutate(NOMBRE = paste(NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT)) %>% 
    select(FECHA, CTACLIENTE, OPERACION, CI, NOMBRE, CTA_CONTABLE=ctaCont)
  
  reproList[[i]] <- bdc
}

reproFull <- rbindlist(reproList)
write_xlsx(reproFull, "D:/!bso/requests/Reprogramados_Abr2023Jun2023.xlsx")
