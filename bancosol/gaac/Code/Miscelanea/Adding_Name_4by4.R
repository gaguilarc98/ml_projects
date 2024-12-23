lastCond <- read_excel("D:/!bso/mph/condonados/TardioCond_May2023_v3.xlsx")

bdc <- readRDS("D:/!bso/girCartera/rds/ec_May2023.rds") %>% 
  mutate(NOMBRE_CLIENTE = paste(NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT, sep= " ")) %>% 
  mutate(NOMBRE_CLIENTE = str_trim(NOMBRE_CLIENTE)) %>% 
  select(CTACLIENTE, OPERACION, NOMBRE_CLIENTE)

bdcprev <- readRDS("D:/!bso/girCartera/rds/ec_Abr2023.rds") %>% 
  mutate(NOMBRE_CLIENTE = paste(NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT, sep= " ")) %>% 
  select(CTACLIENTE, OPERACION, NOMBRE_CLIENTE)

bdcnext <- readRDS("D:/!bso/girCartera/rds/ec_Jun2023.rds") %>% 
  mutate(NOMBRE_CLIENTE = paste(NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT, sep= " ")) %>% 
  select(CTACLIENTE, OPERACION, NOMBRE_CLIENTE)

bdcmar <- readRDS("D:/!bso/girCartera/rds/ec_Mar2023.rds") %>% 
  mutate(NOMBRE_CLIENTE = paste(NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT, sep= " ")) %>% 
  mutate(NOMBRE_CLIENTE = str_trim(NOMBRE_CLIENTE)) %>% 
  select(CTACLIENTE, OPERACION, NOMBRE_CLIENTE)

bdcfeb <- readRDS("D:/!bso/girCartera/rds/ec_Feb2023.rds") %>% 
  mutate(NOMBRE_CLIENTE = paste(NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT, sep= " ")) %>% 
  mutate(NOMBRE_CLIENTE = str_trim(NOMBRE_CLIENTE)) %>% 
  select(CTACLIENTE, OPERACION, NOMBRE_CLIENTE)
#ALGUNOS NOMBRES ESTÁN VACIOS
lastCond <- lastCond %>% 
  left_join(bdc, by=c("Cuenta"="CTACLIENTE","Operacion"="OPERACION")) %>% 
  relocate(NOMBRE_CLIENTE, .after = Cuenta)

#AGREGANDO NOMBRES DE MES ANTERIOR
lastCond <- lastCond %>% 
  left_join(bdcprev, by=c("Cuenta"="CTACLIENTE","Operacion"="OPERACION"), suffix=c("","_abr")) %>% 
  relocate(NOMBRE_CLIENTE_abr, .after = NOMBRE_CLIENTE)

lastCond <- lastCond %>% 
  left_join(bdcnext, by=c("Cuenta"="CTACLIENTE","Operacion"="OPERACION"), suffix=c("","_jun")) %>% 
  relocate(NOMBRE_CLIENTE_jun, .after = NOMBRE_CLIENTE_abr)

lastCond <- lastCond %>% 
  left_join(bdcmar, by=c("Cuenta"="CTACLIENTE","Operacion"="OPERACION"), suffix=c("","_mar")) %>% 
  relocate(NOMBRE_CLIENTE_mar, .after = NOMBRE_CLIENTE_abr)

lastCond <- lastCond %>% 
  left_join(bdcfeb, by=c("Cuenta"="CTACLIENTE","Operacion"="OPERACION"), suffix=c("","_feb")) %>% 
  relocate(NOMBRE_CLIENTE_feb, .after = NOMBRE_CLIENTE_mar)

lastCond2 <- lastCond %>% 
  mutate(across(c(starts_with("NOMBRE_CLIENTE")),~str_trim(.x))) %>% 
  mutate(NOMBRE_CLIENTE = case_when(NOMBRE_CLIENTE=="" & !is.na(NOMBRE_CLIENTE_abr) & NOMBRE_CLIENTE_abr!=""~NOMBRE_CLIENTE_abr,
                                    NOMBRE_CLIENTE=="" & !is.na(NOMBRE_CLIENTE_jun)& NOMBRE_CLIENTE_jun!=""~NOMBRE_CLIENTE_jun,
                                    NOMBRE_CLIENTE=="" & !is.na(NOMBRE_CLIENTE_mar)& NOMBRE_CLIENTE_mar!=""~NOMBRE_CLIENTE_mar,
                                    NOMBRE_CLIENTE=="" & !is.na(NOMBRE_CLIENTE_feb)& NOMBRE_CLIENTE_feb!=""~NOMBRE_CLIENTE_feb,
                                    TRUE~NOMBRE_CLIENTE))

lastCond2 <- lastCond2 %>% 
  mutate(NOMBRE_CLIENTE = str_trim(NOMBRE_CLIENTE)) %>% 
  select(-NOMBRE_CLIENTE_abr, -NOMBRE_CLIENTE_jun)

write_xlsx(lastCond2, )
x <- lastCond2 %>% 
  dplyr::filter(NOMBRE_CLIENTE=="")
write_xlsx(lastCond, )

dfTotal <- readRDS("D:/!bso/features/Clientes_Ene15Jun23_v2.rds")

mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023)
myrds <- as.vector(sapply(year,function(x){paste0(mes,x)})) #lista de meses-años para abrir
for(i in 1:length(myrds)) {
  tryCatch({
    print(myrds[i])
    bdc <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",myrds[i],".rds")) %>% 
      mutate(NOMBRE_CLIENTE = paste(NOMBRE_TIT, PATERNO_TIT, MATERNO_TIT, sep= " ")) %>% 
      mutate(NOMBRE_CLIENTE = str_trim(NOMBRE_CLIENTE)) %>% 
      select(CTACLIENTE, OPERACION, NOMBRE_CLIENTE)
    lastCond <- lastCond %>% 
      left_join(bdc, by = c("Cuenta"="CTACLIENTE","Operacion"="OPERACION"), suffix = c("","_bdc")) %>% 
      mutate(NOMBRE_CLIENTE = ifelse(NOMBRE_CLIENTE=="" & !is.na(NOMBRE_CLIENTE_bdc) & NOMBRE_CLIENTE_bdc!="", NOMBRE_CLIENTE_bdc, NOMBRE_CLIENTE)) %>% 
      select(-NOMBRE_CLIENTE_bdc)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

length(which(lastCond$NOMBRE_CLIENTE==""))

write_xlsx(lastCond, "D:/!bso/mph/condonados/TardioCond_May2023_v4.xlsx")
