mybdc <- c("Ene2023","Feb2023")
mybdc <- c("Dic2022")
bdcList <- list()
for (i in 1:length(mybdc)) {
  fbase <- as.yearmon(paste0(substr(mybdc[i],1,3),'. ',substr(mybdc[i],4,7)))
  bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
                      mybdc[i],'.txt'), encoding = 'Latin-1', fill = T) %>% 
    mutate(fdes = dmy(FDESEMBOLSO)) %>% 
    mutate(cosechaY = year(fdes)) %>% 
    mutate(cosechaM = as.yearmon(fdes)) %>% 
    dplyr::filter(cosechaY==2022)
  bdcList[[i]] <- bdc
}
bdcJoin <- bind_rows(bdcList)

length(which((bdcJoin$OPERACION_ORI_REF!=0)))/nrow(bdcJoin)

table(bdcJoin$REFINANCIAMIENTO_GENUINO,useNA = "ifany")
################################################################################
####____CAMPOS REQUERIMIENTO____####
bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
                    mybdc[i],'.txt'), encoding = 'Latin-1', fill = T)
################################################################################
gar <- read.xlsx('D:/!bso/8mar_keys/01/base_riesgos.xlsx')
glimpse(gar)

assign1 <- read.xlsx('D:/!bso/8mar_keys/assign/bdk_assign_full_01.xlsx') %>% 
  glimpse() 

assign <- assign1 %>% 
  select(-LLAVEPRIMARIA) %>% 
  rename(LLAVEPRIMARIA=LLAVEPRIMARIA_2)
gar2 <- gar %>% 
  left_join(assign,by="LLAVEPRIMARIA")

length(which(is.na(gar2$Cuenta.Cliente)))
length(which(is.na(gar2$Operacion)))

assign <- assign1 %>% 
  select(-LLAVEPRIMARIA) %>% 
  rename(LLAVEPRIMARIA=LLAVEPRIMARIA_2) %>% 
  left_join(gar,by="LLAVEPRIMARIA") %>% 
  rename(saldokey=Saldo.pendiente,
         fdeskey=Fecha.de.desembolso,
         nameasesorkey=`Nombre.del.oficial./.gestor.de.crédito`,
         asesorkey=`Identificación.del.oficial/gestor.de.crédito`,
         modulokey=MODULO,
         estadokey=ESTADO,
         CTACLIENTE = Cuenta.Cliente,
         OPERACION = Operacion)

mybdc <- "Ene2023"
bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
                    mybdc,'.txt'), encoding = 'Latin-1', fill = T)

bdcJoin <- bdc %>% 
  mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
  left_join(assign,by=c("CTACLIENTE","OPERACION")) %>% 
  mutate(fdeskey = as.Date(fdeskey,"%Y-%m-%d"),
         fdes = as.Date(FDESEMBOLSO,"%d/%m/%y")) %>% 
  mutate(ftrue = ifelse(fdes==fdeskey,1,0),
         atrue = ifelse(ASESOR==asesorkey,1,0),
         mtrue = ifelse(MODULO==modulokey,1,0),
         etrue = ifelse(ESTADO==estadokey,1,0)) %>% 
  mutate(check = ifelse(ftrue==1 & atrue==1 & mtrue==1 & etrue==1, 1, 0)) %>% 
  dplyr::filter(fdes!=fdeskey | ASESOR!=asesorkey | MODULO!=modulokey | ESTADO!=estadokey) %>% 
  select(CTACLIENTE, OPERACION, fdes, fdeskey, ASESOR, asesorkey, MODULO, modulokey, ESTADO, estadokey)
  summarise(nChecks=sum(check),nObs=n())

################################################################################
  ####____CHECKS DE MASK BASE DE CARTERA_____####
myfile <- c('02','03','04','05','06','07','08','09','10','11','12','01')
mybdc <- c('Feb2022','Mar2022','Abr2022','May2022','Jun2022','Jul2022','Ago2022','Sep2022',
           'Oct2022','Nov2022','Dic2022','Ene2023')
bdcList <- list()
  
for (i in 1:length(myfile)) {
  tryCatch({
    assign1 <- readRDS(paste0('D:/!bso/accion/assign/bdk_assign_',myfile[i],'.rds'))
    gar <- readRDS(paste0('D:/!bso/accion/rds/',myfile[i],'/base_riesgos.rds'))
    assign <- assign1 %>% 
      select(-LLAVEPRIMARIA) %>% 
      rename(LLAVEPRIMARIA=LLAVEPRIMARIA_2) %>% 
      left_join(gar,by="LLAVEPRIMARIA") %>% 
      rename(saldokey=Saldo.pendiente,
             fdeskey=Fecha.de.desembolso,
             nameasesorkey=`Nombre.del.oficial./.gestor.de.crédito`,
             asesorkey=`Identificación.del.oficial/gestor.de.crédito`,
             modulokey=MODULO,
             estadokey=ESTADO,
             CTACLIENTE = Cuenta.Cliente,
             OPERACION = Operacion)
    l <- length(which(is.na(assign$CTACLIENTE)))
    print(paste0("Nro NAs join mask y xlsx ",mybdc[i],": ",l))
    bdc <- fread(paste0('//VFSNALSRV/RiesgoCrediticioOFN/Bases_Riesgos/05_Base_Cartera_Mensual/BaseCartera', 
                        mybdc[i],'.txt'), encoding = 'Latin-1', fill = T)
    bdcJoin <- bdc %>% 
      mutate(saldous = ifelse(MONEDA == 0, as.numeric(SALDO)/6.86, as.numeric(SALDO))) %>% 
      left_join(assign,by=c("CTACLIENTE","OPERACION")) %>% 
      mutate(fdeskey = as.Date(fdeskey,"%Y-%m-%d"),
             fdes = as.Date(FDESEMBOLSO,"%d/%m/%y")) %>% 
      mutate(ftrue = ifelse(fdes==fdeskey,1,0),
             atrue = ifelse(ASESOR==asesorkey,1,0),
             mtrue = ifelse(MODULO==modulokey,1,0),
             etrue = ifelse(ESTADO==estadokey,1,0)) %>% 
      mutate(check = ifelse(ftrue==1 & atrue==1 & mtrue==1 & etrue==1, 1, 0)) %>% 
      dplyr::filter(fdes!=fdeskey | ASESOR!=asesorkey | ESTADO!=estadokey) %>% 
      select(Fecha.de.Corte,CTACLIENTE, OPERACION, fdes, fdeskey, ASESOR, asesorkey, MODULO, modulokey, ESTADO, estadokey)
    m <- length(which(is.na(bdcJoin$MODULO)))
    print(paste0("Nro. Nas join mask bdc ",mybdc[i],": ",m))
    bdcList[[i]] <- bdcJoin
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

bdcFull <- bind_rows(bdcList)

bdcFull %>% 
  count(Fecha.de.Corte)
####____CHECKS DE MASK BASE DE GARANTIAS____####
myfile <- c('02','03','04','05','06','07','08','09','10','11','12','01')
mybdc <- c('Feb2022','Mar2022','Abr2022','May2022','Jun2022','Jul2022','Ago2022','Sep2022',
           'Oct2022','Nov2022','Dic2022','Ene2023')
bdcList <- list()

for (i in 1:length(myfile)) {
  tryCatch({
    assign1 <- readRDS(paste0('D:/!bso/accion/assign/bdk_assign_',myfile[i],'.rds'))
    gar <- readRDS(paste0('D:/!bso/accion/rds/',myfile[i],'/base_garantias.rds'))
    n <- length(which(is.na(gar$Valor.garantía)))
    print(paste0("Nro NAS before join in base_garantias ",mybdc[i],": ",n))
    assign <- gar %>% 
      rename(LLAVEPRIMARIA_2=LLAVEPRIMARIA) %>% 
      left_join(assign1,by="LLAVEPRIMARIA_2") 
    l <- length(which(is.na(assign$CTACLIENTE)))
    print(paste0("Nro NAs in CTACLIENTE after join ",mybdc[i],": ",l))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
####____GETTING LINEAS DE MODULO____####
myfile <- c('02','03','04','05','06','07','08','09','10','11','12','01')
mybdc <- c('Feb2022','Mar2022','Abr2022','May2022','Jun2022','Jul2022','Ago2022','Sep2022',
           'Oct2022','Nov2022','Dic2022','Ene2023')
modList <- list()
for (i in 1:length(myfile)) {
  tryCatch({
    print(mybdc[i])
    gar <- readRDS(paste0('D:/!bso/accion/rds/',myfile[i],'/base_riesgos.rds')) %>% 
      select(MODULO,Nombre.Modulo) %>% 
      mutate(Nombre.Modulo = str_trim(Nombre.Modulo)) %>% 
      distinct_all()
    modList[[i]] <- gar
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

modFull <- bind_rows(modList) %>% 
  distinct_all() %>% 
  group_by(MODULO) %>% 
  arrange(desc(Nombre.Modulo)) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup()

write.xlsx(modFull,'D:/!bso/bases/excel/modNames.xlsx')
