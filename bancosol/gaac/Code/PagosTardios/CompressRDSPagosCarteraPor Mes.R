flist <- list.files("D:/!bso/mph/rds/")
PagosCartera <- read
for (i in 1:length(flist)) {
  Pagos <- readRDS(paste0("D:/!bso/mph/rds/",flist[i]))
  saveRDS(Pagos, paste0("D:/!bso/mph/rds/",flist[i]))
}

bases <- c("Ene2018_Jun2018","Jul2018_Dic2018","Ene2019_Jun2019","Jul2019_Dic2019",
           "Ene2020_Jun2020","Jul2020_Dic2020","Ene2021_Jun2021","Jul2021_Dic2021",
           "Ene2022_Oct2022")
PagosTList <- list()
for (i in 1:length(bases)){
  print(bases[i])
  P2full <- readRDS(paste0("D:/!bso/mph/rds2/PagosCartera_",bases[i],".rds"))
  
  pmes <- sort(unique(P2full$myPago))
  P2uh <- P2full %>%
    getTardios()
  PTardios <- P2uh %>% 
    dplyr::filter(appsU==1)
  PagosTList[[i]] <- PTardios
}

PagosTFull <- rbindlist(PagosTList)

saveRDS(PagosTFull,"D:/!bso/firstTimes/PagosHist_Ene18Mar23.rds")