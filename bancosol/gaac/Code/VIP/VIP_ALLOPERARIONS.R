####____LEADS PREAPROBADOS CON EL ÚLTIMO CIERRE MENSUAL____####
bdcLast <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',shortmonth,'.rds')) %>% 
  dplyr::filter(!MODULO %in% c(131,29)) %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  mutate(montous = ifelse(MONEDA==0,as.numeric(MONTO)/6.86,as.numeric(MONTO))) %>% 
  mutate(montous = ifelse(montous<saldous, saldous, montous)) %>% 
  mutate(NOMBRE_CLIENTE = str_trim(paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT))) %>% 
  mutate(CAEDEC_DC = cases(grupoCaedecC,c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','Z'),
                           c('5. Agropecuario','5. Agropecuario','4. Productivo','4. Productivo','5. Ind. Manufacturera',
                             '4. Productivo','3. Construcción','2. Comercio','1. Servicios','1. Servicios','1. Servicios','1. Servicios',
                             '1. Servicios','1. Servicios','1. Servicios','1. Servicios','1. Servicios','1. Servicios'))) %>% 
  mutate(GENERO = ifelse(!GENERO %in% c('F', 'M'),'J',GENERO)) %>% 
  mutate(Seg_Merc_Asalariado = ifelse(DESC_SEGMERC=="Asalariado",1,0)) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(Seg_Merc_Asalariado = ifelse(max(Seg_Merc_Asalariado)>0,'Es asalariado','No es asalariado')) %>% 
  ungroup() %>% 
  select(OPERACION, CI, CTACLIENTE, NOMBRE_CLIENTE, GENERO, NOMBRE_ASESOR, NOMBRE_AGENCIA, 
         FDESEMBOLSO, CAEDEC_DC, CPOP, SECTOR_CARTERA, DESC_SEGMERC,`Categoría` = categ, 
         `Tipo_Crédito` = tipoCred, Sucursal, Rango_Monto = rangom, MONTO_USD = montous,
         intus, SALDO_USD = saldous,ctaCont, Seg_Merc_Asalariado)

vipDescFull <- bdcLast %>% 
  left_join(finalVIP, by = 'CTACLIENTE') %>%
  left_join(select(dfScore, CTACLIENTE, prob_GC), by = 'CTACLIENTE') %>%
  mutate(pctSaldo = SALDO_USD/MONTO_USD) %>% 
  mutate(Amortización = 1 - pctSaldo) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(vipViable = ifelse(target == 1 & pctSaldo <= 0.8, 1, 0)) %>% #Criterio de amortización de al menos el 20%
  mutate(oportunity_raw = MONTO_USD-SALDO_USD) %>% 
  mutate(oportunity = ifelse(target == 1, MONTO_USD - SALDO_USD, 0)) %>% 
  mutate(oportunityViable = ifelse(vipViable == 1, oportunity, 0)) %>% 
  mutate(Cliente_Viable = max(vipViable)) %>% 
  ungroup() %>%
  mutate(ctileProb = ntile(prob_GC, n = 100)) %>% 
  mutate(dtileProb = ntile(prob_GC, n = 10)) %>%
  mutate(Lista_GNN = ifelse(excelenteNegocios == 1, 'En Lista', 'Fuera de Lista'),
         VIP_AR = ifelse(target==1, 'VIP-AR', 'Regular'),
         VIP_Viable = ifelse(vipViable == 1, 'Viable', 'No viable'),
         Cliente_Viable = ifelse(Cliente_Viable == 1, "Cliente Viable", "Cliente No Viable")) %>%
  # dplyr::filter(VIP_RC == 'VIP-RC') %>% # filtro de VIPs
  glimpse() %>% 
  select(OPERACION, CI, CTACLIENTE, GENERO, NOMBRE_CLIENTE, NOMBRE_ASESOR, Sucursal, ctaCont,
         NOMBRE_AGENCIA, FDESEMBOLSO, CAEDEC_DC, CPOP, SECTOR_CARTERA, `Categoría`, `Tipo_Crédito`,
         Rango_Monto, MONTO_USD, SALDO_USD, Lista_GNN, `Amortización`, Probabilidad_VIP = prob_GC, 
         Decil_Prob = dtileProb, Percentil_Prob = ctileProb, Cliente_Viable, 
         VIP_Viable, Oportunidad_cruda = oportunity_raw,Oportunidad = oportunity, Oportunidad_Viable = oportunityViable, VIP_AR, moraMax,
         maxMoraIM_cl, meanMoraIM_cl, maxMoraIMclBin, meanMoraIMclBin,totalBC, totalWC,  minMonto, maxMonto, montoRat, 
         totalLoanDays, tuvoMora_pct, tuvoMoraIMclBin, Seg_Merc_Asalariado,
         starts_with("CR_")) %>%
  mutate(Score = Probabilidad_VIP * Oportunidad_Viable) %>% 
  glimpse() 

nrow(vipDescFull)==nrow(bdcLast)
n_distinct(vipDescFull$CTACLIENTE[vipDescFull$VIP_AR=='VIP-AR'])
table(vipDescFull$VIP_AR)
table(vipDescFull$VIP_Viable)
sum(vipDescFull$Oportunidad_Viable)
table(vipDescFull$Lista_GNN,vipDescFull$VIP_Viable)

write_xlsx(vipDescFull, "D:/!bso/vipCartera/Criterios_Leads_Sep2023.xlsx")
####____AJUSTE DE COMPRA CON TODOS LOS CRÉDITOS EN SF____####
bdcAgo <- readRDS("D:/!bso/girCartera/rds/ec_Ago2023.rds") %>% 
  mutate(esFSL = ifelse(MODULO==118 | str_detect(TIPO_OPER, "MIGR"),1,0)) %>% 
  select(CTACLIENTE, OPERACION, esFSL)

lastcred <- "Ago. 2023" #Se selecciona un mes anterior al cierre de cartera
shortcred <- str_replace(lastcred,". ","") #La última información de infocred tiene un mes de rezago

infoCheck <- readRDS(paste0('D:/!bso/califClientes/process/comp_',shortcred,'.rds'))
infoClientes <- infoCheck %>% 
  dplyr::filter(REGULADO=="SBEF") %>% 
  dplyr::filter(str_detect(TIPO_OBLIGADO, 'A - ')) %>%  #Para conservar solo deudores
  mutate(esBSO = ifelse(SIGLA=='BSO',1,0)) %>%
  mutate(noesBSO = ifelse(SIGLA!='BSO',1,0)) %>%
  mutate(CALIFICACION = ifelse(is.na(CALIFICACION),"_", CALIFICACION)) %>% 
  left_join(bdcAgo, by=c("CTACLIENTE","OPERACION")) %>% 
  replace_na(list(esFSL=0)) %>% 
  group_by(CI) %>%
  dplyr::filter(sum(esBSO)>0 & sum(noesBSO)>0) %>% #Para ver si es compartido
  arrange(desc(esBSO),esFSL) %>% 
  mutate(CTACLIENTE = CTACLIENTE[row_number()==1]) %>% #Le asignamos la CTACLIENTE dentro de BSO a los créditos fuera de BSO %>% 
  dplyr::filter(esBSO==0) %>%
  ungroup() %>% 
  # dplyr::filter(ESTADO != "0. CONTINGENTE") %>% 
  select(CTACLIENTE, NOMBRE, TIPO_OBLIGADO, SIGLA, ENTIDAD, 
         TIPO_CREDITO, CALIFICACION, ESTADO, DiasMora, FECHAINICIO, FECHAVTO, MontoOriginal, 
         Saldo_USD_SF = saldo,
         Saldo_Vig_USD_SF = saldoVig, SaldoMora_USD_SF = saldoMora, SaldoCast_USD_SF = saldoCast,
         SaldoCont_USD_SF = saldoCont, Periodo_Pago) #DEPARTAMENTO, HISTORICO = histLast16

compraCartera <- vipDescFull %>% 
  select(CTACLIENTE, CI, GENERO, Lista_GNN, Probabilidad_VIP, Cliente_Viable, Seg_Merc_Asalariado,
         meanMoraIM_cl, meanMoraIMclBin, totalBC, totalWC, minMonto, maxMonto, totalLoanDays,
         tuvoMora_pct, tuvoMoraIMclBin, Lista_GNN, VIP_AR) %>% 
  distinct_all() %>% 
  inner_join(infoClientes,by="CTACLIENTE") 

LeadsYCompra <- list(Leads=vipDescFull, Compra=compraCartera)
write_xlsx(LeadsYCompra, "D:/!bso/vipCartera/Criterios_Leads_Sep2023.xlsx")

####____AÑADIR DIASMORA Y MORA INTRA-MES____####
bdc <- readRDS("D:/!bso/girCartera/rds/ec_Sep2023.rds")
bdcMora <- bdc %>% select(CTACLIENTE, OPERACION, DIASMORA)
write_xlsx(bdcMora,"D:/!bso/vipCartera/diasmora_Sep2023.xlsx")

moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Sep2023.rds")) %>% 
  dplyr::filter(monDate=="Sep. 2023") %>% 
  select(monDate, CTACLIENTE, OPERACION, maximaDPD_2) %>% 
  dplyr::filter(!is.na(maximaDPD_2))
moraIntraMes <- moraIntraMes %>% 
  select(CTACLIENTE, OPERACION, maximaDPD_2)

write_xlsx(moraIntraMes, "D:/!bso/vipCartera/diasMIM_Sep2023.xlsx")

####____COMPARANDO SEP 2023____####
vipold <- read_xlsx("D:/!bso/vipCartera/Entrega_Leads_PreAprobados_CompraCartera_Sep2023.xlsx") %>% 
  select(CTACLIENTE) %>% 
  distinct_all()
vipnew <- read_xlsx("D:/!bso/vipCartera/Entrega_Leads_PreAprobados_CompraCartera_Sep2023_v2.xlsx") %>% 
  select(CTACLIENTE) %>% 
  distinct_all()

vipNot <- vipold %>% 
  anti_join(vipnew, by="CTACLIENTE")
write_xlsx(vipNot, "D:/!bso/vipCartera/VipNot.xlsx")
