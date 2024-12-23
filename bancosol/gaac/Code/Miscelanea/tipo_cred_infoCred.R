year <- c("2018","2019","2020","2021","2022","2023")
mes <- c("01","02","03","04","05","06","07","08","09","10","11","12")
month <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
myutf <- as.vector(sapply(year, function(x){paste0(x,mes)}))
mybdc <- as.vector(sapply(year, function(x){paste0(month,x)}))
i <- 60
tipoCredList <- list()
for(i in 1:length(myutf)){
  tryCatch({
    print(myutf[i])
    infoRaw <- readRDS(paste0('D:/!bso/califClientes/rds/BSO',myutf[i],'.rds')) %>% 
      dplyr::filter(nchar(`NOMBRE COMPLETO`)<100)
    if(i==1){
      tipoCredList <- infoRaw %>% 
        select(TIPO_CREDITO = `TIPO CREDITO SBEF`) %>% 
        distinct_all()
    }else{
      Aux <- infoRaw %>% 
        select(TIPO_CREDITO=`TIPO CREDITO SBEF`) %>% 
        distinct_all() %>% 
        dplyr::filter(!TIPO_CREDITO %in% tipoCredList$TIPO_CREDITO)
      tipoCredList <- tipoCredList %>% 
        bind_rows(Aux)
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

tipoCredList <- tipoCredList %>% 
  dplyr::filter(TIPO_CREDITO!="") %>% 
  arrange(TIPO_CREDITO)
write_xlsx(tipoCredList, "D:/!bso/bases/excel/tipoCred_infoCred.xlsx")
