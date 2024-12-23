####____CARGA DE PAQUETES____####
remove(list = ls())
gc()
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
library(ggplot2)
library(openxlsx)
library(janitor)
library(ggrepel)
remove(list = ls())
options("encoding" = "UTF-8")
options(scipen = 999)
cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
paleta <- colorRampPalette(c("slateblue4","purple4","slateblue3","darkorchid3","red3","tan2","yellow2","white"),bias=1.5)
# Cosechas
# Last month update
####____CREATING BDC FULL____####
mes <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
year <- c(2018)
year <- c(2015:2023)
mybdc <- as.vector(sapply(year, function(x){paste0(mes,x)}))
mybdc <- mybdc[-c(which(mybdc=="Abr2023"):length(mybdc))]
i <- 1
bdcList <- list()
for (i in 1:length(mybdc)) {
  print(mybdc[i])
  bdc <- readRDS(paste0('D:/!bso/girCartera/rds/ec_',mybdc[i],'.rds')) %>% 
    select(CTACLIENTE, OPERACION, ESTADO, DIASMORA, CALIFICACION, AGENCIA, monDate,
           fdes, cosechaM, cosechaY, saldous, montous, previus, MONTO, MONEDA, 
           saldoCast, saldoRepVig, CIU, CAEDEC_DEST, tipoCred, OBJETO_CRED, 
           ctaCont, OPERACION_ORI_REF) %>% 
    mutate(OPERACION_ORI_REF = as.integer(OPERACION_ORI_REF))
  bdcList[[i]] <- bdc # almacenado en lista
}
#quietly(gc())
bdcFull <- bind_rows(bdcList) %>% # apilado en DF
  # mutate(mon = substr(fbase,1,3)) %>%
  # mutate(year = substr(fbase,4,7)) %>%
  # mutate(mes = case_when(mon == 'Ene'~'jan',
  #                        mon == 'Feb'~'feb',
  #                        mon == 'Mar'~'mar',
  #                        mon == 'Abr'~'apr',
  #                        mon == 'May'~'may',
  #                        mon == 'Jun'~'jun',
  #                        mon == 'Jul'~'jul',
  #                        mon == 'Ago'~'aug',
  #                        mon == 'Sep'~'sep',
  #                        mon == 'Oct'~'oct',
  #                        mon == 'Nov'~'nov',
  #                        mon == 'Dic'~'dec',)) %>%
  # mutate(dayDate = dmy(paste0('1-', mes, '-', year))) %>%
  # mutate(monDate = as.yearmon(dayDate)) %>%
  # select(-dayDate, -mon, -year, -mes) %>%
  # mutate(cosechaM = as.yearmon(fdes)) %>% 
  arrange(CTACLIENTE, OPERACION, monDate, cosechaM) 
write_rds(bdcFull,"D:/!bso/cosechas/bdcFullCosechas_Ene15Mar23.rds")
####____FAST CHECK____####
bdcCheck <- bdcFull %>% 
  mutate(opDes = ifelse(montous > 0, 1, 0)) %>% 
  group_by(cosechaY) %>% 
  summarise(totalMdes=sum(montous),totalOdes = sum(opDes))
####____ADDING A MONTH TO BDCFULL COSECHAS___####
bdcOld <- readRDS('D:/!bso/cosechas/bdcFullCosechas_Ene15Mar23.rds')
bdcUpdate <- readRDS('D:/!bso/girCartera/rds/ec_Abr2023.rds') %>% 
  select(CTACLIENTE, OPERACION, ESTADO, DIASMORA, CALIFICACION, AGENCIA, monDate,
         fdes, cosechaM, cosechaY, saldous, montous, previus, MONTO, MONEDA, 
         saldoCast, saldoRepVig, CIU, CAEDEC_DEST, tipoCred, OBJETO_CRED, 
         ctaCont, OPERACION_ORI_REF) %>% 
  mutate(OPERACION_ORI_REF = as.integer(OPERACION_ORI_REF)) 

bdcFull_update <- bdcOld %>% 
  bind_rows(bdcUpdate) %>% 
  arrange(CTACLIENTE, OPERACION, monDate, cosechaM) 

write_rds(bdcFull_update, 'D:/!bso/cosechas/bdcFullCosechas_Ene15Abr23.rds')
bdcOld <- NULL
gc()
####____CREATING MORA POR COSECHA____####
remove(list = ls())
gc()
bdcFull <- readRDS('D:/!bso/cosechas/bdcFullCosechas_Ene15Abr23.rds')

####____PRUEBA CON AÑO 2018____####
#Hacer correr bdcFull con 2018
#Y correr features/ListaClientes con 2018
bdcFull <- readRDS("D:/!bso/cosechas/bdcFull2018.rds")
dfTotal <- readRDS("D:/!bso/cosechas/dfTotal2018.rds")

tail(bdcFull %>% 
       # dplyr::filter(montous>0) %>% 
       group_by(year(fdes)) %>% 
       summarise(mont=sum(montous),ops=n_distinct(OPERACION)))

tail(dfTotal %>% 
  group_by(year(fdes)) %>% 
  summarise(monto=sum(MONTOUS),nOps=n_distinct(OPERACION)))

coseY %>% 
  group_by(cosechaY) %>% 
  summarise(mont=max(totalMdes),ops=max(totalOdes))

x <- bdcFull %>% 
  dplyr::filter(montous>0)
y <- dfTotal %>% 
  dplyr::filter(year(fdes)==2018)
z <- x$OPERACION[which(!x$OPERACION %in% y$OPERACION)]

write_rds(bdcFull, "D:/!bso/cosechas/bdcFull2018.rds")
write_rds(dfTotal, "D:/!bso/cosechas/dfTotal2018.rds")
####____NORMAL____####
coseY <- bdcFull %>% 
  mutate(par0 = ifelse(DIASMORA > 0, saldous, 0),
         opsPar0 = ifelse(par0 > 0, 1, 0)) %>% 
  select(montous, starts_with('saldo'), par0, monDate, opDes, opsPar0, cosechaM) %>% 
  ungroup() %>% 
  mutate(cosechaY = as.Date(cosechaM, frac = 1),
         cosechaY = year(cosechaY)) %>% # Creación de la cosecha anual OBSERVADA
  select(-cosechaM) %>% 
  group_by(monDate, cosechaY) %>% 
  summarise_all(sum, na.rm = T) %>% 
  ungroup() %>% 
  #dplyr::filter(cosechaM >= 'ene. 2016') %>% 
  group_by(cosechaY) %>% # Agrupación para cálculo de totales
  arrange(cosechaY, monDate) %>% 
  mutate(totalMdes = sum(montous),
         totalOdes = sum(opDes),
         saldoCancel = saldous - dplyr::lag(saldous),
         saldoCancel = ifelse(is.na(saldoCancel), 0, saldoCancel),
         saldoCancel = cumsum(saldoCancel)) %>% 
  ungroup() %>% 
  group_by(monDate) %>% 
  mutate(totalSaldo = sum(saldous)) %>% 
  ungroup() %>% 
  arrange(cosechaY, monDate) %>% 
  dplyr::filter(totalMdes > 0) %>% # Filtro para cosechas enteras (A partir de ene-2015)
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
  mutate(par0Rel = par0/totalMdes*100,
         castRel = saldoCast/totalMdes,
         sumH = saldoCast + saldous + par0 + saldoRepVig - saldoCancel,
         dif = totalMdes -sumH,
         pctDif = dif/totalMdes*100,
         pctVigRel = (saldous - par0 - saldoCast)/totalMdes*100,
         cancelRel = saldoCancel/totalMdes*-100,
         sumPct = par0Rel + pctVigRel + castRel + cancelRel) %>% 
  group_by(cosechaY) %>% 
  arrange(cosechaY, monDate) %>% 
  mutate(mesPos = row_number(),
         groupR = case_when(mesPos <= 12 ~ 'a. 12 meses',
                            mesPos > 12 & mesPos <= 24 ~ 'b. 13-24 meses',
                            mesPos > 24 & mesPos <= 48 ~ 'c. 25-48 meses',
                            mesPos > 48 ~ 'd. 48+ meses',)) %>% # Agrupación para analítica/descriptivos
  ungroup() %>% 
  group_by(groupR) %>% 
  mutate(maxPar0 = max(par0Rel)) %>% 
  arrange(groupR, desc(par0Rel)) %>% 
  mutate(par0Rank = row_number()) %>% 
  ungroup() %>% 
  group_by(cosechaY) %>%
  arrange(cosechaY, monDate) %>% # lo que sigue será utilizado en analítica luego
  mutate(grw_par0_1 = par0Rel - dplyr::lag(par0Rel, 1),
         grw_par0_3 = par0Rel - dplyr::lag(par0Rel, 3),
         grw_par0_6 = par0Rel - dplyr::lag(par0Rel, 6),
         grw_par0_12 = par0Rel - dplyr::lag(par0Rel, 12),
         grw_par0_18 = par0Rel - dplyr::lag(par0Rel, 18),
         grw_par0_24 = par0Rel - dplyr::lag(par0Rel, 24),
         grw_par0_30 = par0Rel - dplyr::lag(par0Rel, 30),
         grw_par0_36 = par0Rel - dplyr::lag(par0Rel, 36),
         grw_par0_48 = par0Rel - dplyr::lag(par0Rel, 48)) %>% 
  ungroup() %>% 
  group_by(cosechaY) %>% 
  arrange(cosechaY, monDate) %>% 
  mutate(maxPar0_cose = max(par0Rel),
         ttm = ifelse(par0Rel == maxPar0_cose, mesPos, 0),
         ttm = sum(ttm)) %>% # time to max mora
  ungroup()

####____MORA POR AÑO DE DESEMBOLSO ORIGINAL____####
# DF con cosechas asignadas a la fecha de desembolso original
coseY_ori <- bdcFull %>% 
  mutate(par0 = ifelse(DIASMORA > 0, saldous, 0),
         opsPar0 = ifelse(par0 > 0, 1, 0)) %>% 
  select(montous, starts_with('saldo'), par0, monDate, opDes, opsPar0, cosechaM,
         OPERACION, OPERACION_ORI_REF, CTACLIENTE, fdes) %>% 
  ungroup() %>% 
  mutate(cosechaY = as.Date(cosechaM, frac = 1),
         cosechaY = year(cosechaY)) %>% 
  ungroup() %>% 
  group_by(CTACLIENTE) %>% 
  mutate(hasRefin = sum(OPERACION_ORI_REF),
         yearRefin = ifelse(OPERACION_ORI_REF > 0, year(fdes), 0),
         maxRefin = max(yearRefin)) %>% # identificación de refinanciados
  #dplyr::filter(maxRefin == 2021) %>% 
  mutate(OPERACION_hist = ifelse(OPERACION_ORI_REF > 0, OPERACION_ORI_REF, OPERACION)) %>% # Identificación de # de operación original
  ungroup() %>% 
  group_by(OPERACION_hist) %>% 
  mutate(cosechaY_ori = min(cosechaY)) %>% # La cosecha original es la menor de las dos fechas (desembolso original y refinanciamiento)
  ungroup() %>% 
  mutate(cosechaY = cosechaY_ori) %>% # Para no modificar el resto del código, nada más
  select(-cosechaM, -fdes,-OPERACION,-CTACLIENTE, -OPERACION_ORI_REF,
         -hasRefin, maxRefin, -yearRefin, -OPERACION_hist, 
         -cosechaY_ori) %>% 
  group_by(monDate, cosechaY) %>% 
  summarise_all(sum, na.rm = T) %>% 
  ungroup() %>% 
  #dplyr::filter(cosechaM >= 'ene. 2016') %>% 
  group_by(cosechaY) %>% 
  arrange(cosechaY, monDate) %>% 
  mutate(totalMdes = sum(montous),
         totalOdes = sum(opDes),
         saldoCancel = saldous - dplyr::lag(saldous),
         saldoCancel = ifelse(is.na(saldoCancel), 0, saldoCancel),
         saldoCancel = cumsum(saldoCancel)) %>% 
  ungroup() %>% 
  group_by(monDate) %>% 
  mutate(totalSaldo = sum(saldous)) %>% 
  ungroup() %>% 
  arrange(cosechaY, monDate) %>% 
  dplyr::filter(totalMdes > 0) %>% 
  mutate(dayDate = as.Date(monDate, frac = 1)) %>% 
  mutate(par0Rel = par0/totalMdes*100,
         castRel = saldoCast/totalMdes,
         sumH = saldoCast + saldous + par0 + saldoRepVig - saldoCancel,
         dif = totalMdes -sumH,
         pctDif = dif/totalMdes*100,
         pctVigRel = (saldous - par0 - saldoCast)/totalMdes*100,
         cancelRel = saldoCancel/totalMdes*-100,
         sumPct = par0Rel + pctVigRel + castRel + cancelRel) %>% 
  group_by(cosechaY) %>% 
  arrange(cosechaY, monDate) %>% 
  mutate(mesPos = row_number(),
         groupR = case_when(mesPos <= 12 ~ 'a. 12 meses',
                            mesPos > 12 & mesPos <= 24 ~ 'b. 13-24 meses',
                            mesPos > 24 & mesPos <= 48 ~ 'c. 25-48 meses',
                            mesPos > 48 ~ 'd. 48+ meses',)) %>% 
  ungroup() %>% 
  group_by(groupR) %>% 
  mutate(maxPar0 = max(par0Rel)) %>% 
  arrange(groupR, desc(par0Rel)) %>% 
  mutate(par0Rank = row_number()) %>% 
  ungroup() %>% 
  group_by(cosechaY) %>%
  arrange(cosechaY, monDate) %>% 
  mutate(grw_par0_1 = par0Rel - dplyr::lag(par0Rel, 1),
         grw_par0_3 = par0Rel - dplyr::lag(par0Rel, 3),
         grw_par0_6 = par0Rel - dplyr::lag(par0Rel, 6),
         grw_par0_12 = par0Rel - dplyr::lag(par0Rel, 12),
         grw_par0_18 = par0Rel - dplyr::lag(par0Rel, 18),
         grw_par0_24 = par0Rel - dplyr::lag(par0Rel, 24),
         grw_par0_30 = par0Rel - dplyr::lag(par0Rel, 30),
         grw_par0_36 = par0Rel - dplyr::lag(par0Rel, 36),
         grw_par0_48 = par0Rel - dplyr::lag(par0Rel, 48)) %>% 
  ungroup() %>% 
  group_by(cosechaY) %>% 
  arrange(cosechaY, monDate) %>% 
  mutate(maxPar0_cose = max(par0Rel),
         ttm = ifelse(par0Rel == maxPar0_cose, mesPos, 0),
         ttm = sum(ttm)) %>% 
  ungroup()

write_rds(coseY, 'D:/!bso/cosechas/coseY_mar2023.rds')
write_rds(coseY_ori, 'D:/!bso/cosechas/coseY_ori_mar2023.rds')
####____PPT PLOTS____####
# Gráficas
coseY <- readRDS('D:/!bso/cosechas/coseY_mar2023.rds')
coseY_ori <- readRDS('D:/!bso/cosechas/coseY_ori_mar2023.rds')
gph <- coseY %>% 
  dplyr::filter(cosechaY >= 2015) %>% 
  select(mesPos, par0Rel, cosechaY) %>%
  mutate(par0Rel = round(par0Rel, 2)) %>% 
  ungroup() %>% 
  group_by(cosechaY) %>% 
  mutate(label = if_else(par0Rel == max(par0Rel), 
                         paste0(as.character(cosechaY),': ',as.character(par0Rel), '%, ', as.character(mesPos), ' meses'),
                         NA_character_))

ggplot(gph[gph$cosechaY <= 2022,], aes(x = mesPos, y = par0Rel, color = as.factor(cosechaY))) + 
  geom_line(aes(size = as.factor(cosechaY))) + 
  labs(x="Meses post-desembolso",y="Mora 0 días/Desembolso (%)") +
  scale_y_continuous(breaks = seq(0,0.9,0.1), limits=c(0,0.9))+
  scale_x_continuous(breaks = seq(0,96,12)) +
  scale_color_manual(values = paleta(12)[c(3:5,10,9,8,6,7)]) + 
  theme_minimal() + 
  theme(text = element_text(size=12),
        legend.position = 'none',
        panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(nrow = 2, title="Año desemboloso")) + 
  geom_text_repel(aes(label=label), force=1, point.padding=unit(1,'lines'), 
                  vjust=-4.5, 
                  direction='y',
                  hjust=-2.5, 
                  segment.size=0.5,
                  size=4.61)+
  scale_size_manual(values = c(1, 1, 1, 1, 1, 1, 3, 5, 1 ))

ggsave('D:/!bso/cosechas/cosechasAnuales_SinAjuste_Mar23.png',
       width = 9, height = 6, units = 'in')

gph <- coseY_ori %>% 
  dplyr::filter(cosechaY >= 2015) %>% 
  select(mesPos, par0Rel, cosechaY) %>%
  mutate(par0Rel = round(par0Rel, 2)) %>% 
  ungroup() %>% 
  group_by(cosechaY) %>% 
  mutate(label = if_else(par0Rel == max(par0Rel), 
                         paste0(as.character(cosechaY),': ',as.character(par0Rel), '%, ', as.character(mesPos), ' meses'),
                         NA_character_))

ggplot(gph[gph$cosechaY <= 2022,], aes(x = mesPos, y = par0Rel, color = as.factor(cosechaY))) + 
  geom_line(aes(size = as.factor(cosechaY))) +
  labs(x="Meses post-desembolso",y="Mora 0 días/Desembolso (%)") +
  scale_y_continuous(breaks = seq(0,0.9,0.1), limits=c(0,0.9)) +
  scale_x_continuous(breaks = seq(0,96,12)) +
  scale_color_manual(values = paleta(12)[c(3:5,10,9,8,6,7)]) + 
  theme_minimal() + 
  theme(text = element_text(size=12),
        legend.position = 'none',
        panel.grid.minor = element_blank()) + 
  guides(colour = guide_legend(nrow = 2, title="Año desemboloso")) +
  geom_text_repel(aes(label=label), force=1, point.padding=unit(1,'lines'), 
                  vjust=-4.5, 
                  direction='y',
                  hjust=-2.5, 
                  segment.size=0.5,
                  size=4.61)+
  scale_size_manual(values = c(1, 1, 1, 1, 1, 1, 3, 5, 1 ))

ggsave('D:/!bso/cosechas/cosechasAnuales_ConAjuste_Mar23.png',
       width = 9, height = 6, units = 'in')

ggsave('C:/!bso/cosechas/cosechasAnuales_SinAjuste_Mar23.png')

gph <- coseY_ori %>% 
  dplyr::filter(cosechaY >= 2015) %>% 
  select(mesPos, par0Rel, cosechaY) %>%
  mutate(par0Rel = round(par0Rel, 2)) %>% 
  ungroup() %>% 
  group_by(cosechaY) %>% 
  mutate(label = if_else(par0Rel == max(par0Rel), 
                         paste0(as.character(cosechaY),': ',as.character(par0Rel), '%, ', as.character(mesPos), ' meses'),
                         NA_character_))

ggplot(gph[gph$cosechaY <= 2022,], aes(x = mesPos, y = par0Rel, color = as.factor(cosechaY))) + 
  geom_line(aes(size = as.factor(cosechaY))) + theme_minimal() + 
  scale_y_continuous(breaks = seq(0,0.7,0.1)) +
  scale_x_continuous(breaks = seq(0,96,12)) +
  xlab('Meses post-desembolso') + ylab('Mora 0 días/Desembolso (%)') + ylim(0,0.8)+
  scale_color_manual(values = paleta(13)) + 
  theme(legend.position = 'none',
        panel.grid.minor = element_blank()) + 
  guides(colour = guide_legend(nrow = 2, title="Año desemboloso")) + 
  geom_label_repel(aes(label = label),
                   nudge_y = 0.1,
                   nudge_x = 75,
                   na.rm = TRUE,
                   size = 6,
                   point.padding = NA)+
  scale_size_manual(values = c(1, 1, 1, 1, 1, 1, 3, 5, 1 ))
ggsave('C:/!bso/cosechas/cosechasAnuales_ConAjuste_Mar23.png')

# Tabla resumen
lastmonth <- "Mar. 2023"
tab <- coseY %>% 
  dplyr::filter(monDate==lastmonth) %>% 
  group_by(cosechaY) %>% 
  summarise(saldo=sum(saldo))

tab <- coseY %>% 
  ungroup() %>% 
  group_by(cosechaY) %>% 
  dplyr::filter(cosechaY >= 2015) %>%
  summarise(totalMdes = mean(totalMdes), totalOdes = mean(totalOdes),
            ttm = mean(ttm), maxPar0_cose = mean(maxPar0_cose)) %>% 
  dplyr::rename(Cosecha = cosechaY,
                `Monto Anual (USD)` = totalMdes,
                `Ops. Anuales` = totalOdes,
                `Meses p/Máx. Mora` = ttm,
                `Mora máxima (o días, %)` = maxPar0_cose,) 

write.xlsx(tab, 'C:/!bso/cosechas/tablaResumen_mar2023.xlsx')

kable(tab, row.names = F, 'simple',
      digits = 2, format.args = list(decimal.mark = ".", big.mark = ","),
      linesep = "")
####____TABLAS PPT____####
# Tabs PPT
pct_sinAjuste <- bdcFull %>% 
  dplyr::filter(fbase == 'Mar2023') %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  dplyr::filter(MODULO != 131) %>% 
  mutate(par0 = ifelse(DIASMORA > 0, saldous, NA),
         cosechaY = year(fdes)) %>% 
  ungroup() %>% 
  mutate(Cosecha = as.character(cosechaY)) %>% 
  # mutate(Cosecha = as.character(cosechaY),
  #        Cosecha = ifelse(cosechaY < 2019, '1. Antes de 2019', Cosecha)) %>% 
  select(Cosecha, saldous, par0) %>% 
  group_by(Cosecha) %>% 
  summarise_all(.funs = list(sum = ~ sum(x = ., na.rm = T),
                             count = ~ sum(!is.na(x=.)))) %>% 
  arrange(Cosecha) %>% 
  mutate(across(2:5, ~ round(.x/sum(.x)*100,1), .names = "pct_{.col}")) %>% 
  mutate(across(2:5, ~ round(.x))) %>% 
  adorn_totals('row')

pct_conAjuste <- bdcFull %>% 
  ungroup() %>% 
  mutate(cosechaY = year(fdes)) %>% 
  group_by(CTACLIENTE) %>% 
  mutate(hasRefin = sum(OPERACION_ORI_REF),
         yearRefin = ifelse(OPERACION_ORI_REF > 0, year(fdes), 0),
         maxRefin = max(yearRefin)) %>% # identificación de refinanciados
  #dplyr::filter(maxRefin == 2021) %>% 
  mutate(OPERACION_hist = ifelse(OPERACION_ORI_REF > 0, OPERACION_ORI_REF, OPERACION)) %>% # Identificación de # de operación original
  ungroup() %>% 
  group_by(OPERACION_hist) %>% 
  mutate(cosechaY_ori = min(cosechaY),
         fdes_ori = min(fdes)) %>% # La cosecha original es la menor de las dos fechas (desembolso original y refinanciamiento)
  ungroup() %>% 
  dplyr::filter(fbase == 'Feb2023') %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  dplyr::filter(MODULO != 131) %>% 
  mutate(cosechaY = year(fdes)) %>% 
  mutate(par0 = ifelse(DIASMORA > 0, saldous, NA)) %>% 
  ungroup() %>% 
  mutate(Cosecha = as.character(cosechaY_ori),
         Cosecha = ifelse(cosechaY_ori < 2019, '1. Antes de 2019', Cosecha)) %>% 
  select(Cosecha, saldous, par0) %>% 
  group_by(Cosecha) %>% 
  summarise_all(.funs = list(sum = ~ sum(x = ., na.rm = T),
                             count = ~ sum(!is.na(x=.)))) %>% 
  arrange(Cosecha) %>% 
  mutate(across(2:5, ~ round(.x/sum(.x)*100,1), .names = "pct_{.col}")) %>% 
  mutate(across(2:5, ~ round(.x))) %>% 
  adorn_totals('row')
####____EXPORTANDO A EXCEL____####
# excel exporting
tab_1 <- pct_sinAjuste %>% 
  select(Cosecha, starts_with('pct_')) %>% 
  dplyr::rename(Saldo = pct_saldous_sum,
                `Mora 0 dìas` = pct_par0_sum,
                Operaciones = pct_saldous_count,
                `Ops. Mora 0 días` = pct_par0_count)

tab_2 <- pct_conAjuste %>% 
  select(Cosecha, starts_with('pct_')) %>% 
  dplyr::rename(Saldo = pct_saldous_sum,
                `Mora 0 dìas` = pct_par0_sum,
                Operaciones = pct_saldous_count,
                `Ops. Mora 0 días` = pct_par0_count)

tab_3 <- pct_sinAjuste %>% 
  select(Cosecha, !starts_with('pct_')) %>% 
  dplyr::rename(`Saldo USD` = saldous_sum,
                `Mora 0 dìas USD` = par0_sum,
                Operaciones = saldous_count,
                `Ops. Mora 0 días` = par0_count)

tab_4 <- pct_conAjuste %>% 
  select(Cosecha, !starts_with('pct_')) %>% 
  dplyr::rename(`Saldo USD` = saldous_sum,
                `Mora 0 dìas USD` = par0_sum,
                Operaciones = saldous_count,
                `Ops. Mora 0 días` = par0_count)
tabList = list(pct_saldo_SA = tab_1, pct_saldo_CA = tab_2, 
               saldo_SA = tab_3, saldo_CA = tab_4)
write.xlsx(tabList, 'C:/!bso/cosechas/ppt/ppt_cosechas_mar23.xlsx')

####____EXPERIMENTAL FIGURES____####
# Trying figures
gph <- pct_sinAjuste %>% 
  select(Cosecha, pct_saldous_sum, pct_par0_sum) %>% 
  pivot_longer(!Cosecha) %>% 
  dplyr::filter(Cosecha != 'Total') %>% 
  mutate(name = ifelse(name == 'pct_par0_sum', 'Mora USD 0 días', 'Saldo USD'))
ggplot(gph, aes(x = Cosecha, y = value/100, fill = name)) +
  geom_col(width=0.8, position = position_dodge(width=0.9)) + 
  scale_fill_manual(values = paleta(4), name = '') +
  geom_label(aes(label = paste0(as.character(value), '%')),
             colour = "white", size = 4,
             vjust = 0.5, position = position_dodge(1.1),
             show.legend = F, fontface = "bold") +
  theme_minimal() + ylab('Participación sobre el total') +
  scale_y_continuous(labels=scales::percent_format(),
                     limits = c(0, 0.6)) +
  theme(legend.position = 'bottom') 
ggsave('C:/!bso/cosechas/ppt/comp_moraysaldo_sinajuste_feb23.png')

gph <- pct_conAjuste %>% 
  select(Cosecha, pct_saldous_sum, pct_par0_sum) %>% 
  pivot_longer(!Cosecha) %>% 
  dplyr::filter(Cosecha != 'Total') %>% 
  mutate(name = ifelse(name == 'pct_par0_sum', 'Mora USD 0 días', 'Saldo USD'))
ggplot(gph, aes(x = Cosecha, y = value/100, fill = name)) +
  geom_col(width=0.8, position = position_dodge(width=0.9)) + 
  scale_fill_manual(values = paleta(4), name = '') +
  geom_label(aes(label = paste0(as.character(value), '%')),
             colour = "white", size = 4,
             vjust = 0.5, position = position_dodge(1.1),
             show.legend = F, fontface = "bold") +
  theme_minimal() + ylab('Participación sobre el total') +
  scale_y_continuous(labels=scales::percent_format(),
                     limits = c(0, 0.6)) +
  theme(legend.position = 'bottom') 
ggsave('C:/!bso/cosechas/ppt/comp_moraysaldo_conajuste_feb23.png')

gph <- pct_sinAjuste %>% 
  select(Cosecha, pct_saldous_count, pct_par0_count) %>% 
  pivot_longer(!Cosecha) %>% 
  dplyr::filter(Cosecha != 'Total') %>% 
  mutate(name = ifelse(name == 'pct_par0_count', 'Ops. Mora USD 0 días', 'Operaciones'))
ggplot(gph, aes(x = Cosecha, y = value/100, fill = name)) +
  geom_col(width=0.8, position = position_dodge(width=0.9)) + 
  scale_fill_manual(values = paleta(4), name = '') +
  geom_label(aes(label = paste0(as.character(value), '%')),
             colour = "white", size = 4,
             vjust = 0.5, position = position_dodge(1.1),
             show.legend = F, fontface = "bold") +
  theme_minimal() + ylab('Participación sobre el total') +
  scale_y_continuous(labels=scales::percent_format(),
                     limits = c(0,0.6)) +
  theme(legend.position = 'bottom') 
ggsave('C:/!bso/cosechas/ppt/comp_OPSmoraysaldo_sinajuste_feb23.png')

gph <- pct_conAjuste %>% 
  select(Cosecha, pct_saldous_count, pct_par0_count) %>% 
  pivot_longer(!Cosecha) %>% 
  dplyr::filter(Cosecha != 'Total') %>% 
  mutate(name = ifelse(name == 'pct_par0_count', 'Ops. Mora USD 0 días', 'Operaciones'))
ggplot(gph, aes(x = Cosecha, y = value/100, fill = name)) +
  geom_col(width=0.8, position = position_dodge(width=0.9)) + 
  scale_fill_manual(values = paleta(4), name = '') +
  geom_label(aes(label = paste0(as.character(value), '%')),
             colour = "white", size = 4,
             vjust = 0.5, position = position_dodge(1.1),
             show.legend = F, fontface = "bold") +
  theme_minimal() + ylab('Participación sobre el total') +
  scale_y_continuous(labels=scales::percent_format(),
                     limits = c(0, 0.6)) +
  theme(legend.position = 'bottom') 
ggsave('C:/!bso/cosechas/ppt/comp_OPSmoraysaldo_conajuste_feb23.png')

####____PROYECCION DE MORA____####
# Proyección mora
coseY <- readRDS('C:/!bso/cosechas/coseY_feb2023.rds')
write.xlsx(coseY, 'C:/!bso/cosechas/cosechas_SinAjuste_feb2023.xlsx')

coseY_ori <- readRDS('C:/!bso/cosechas/coseY_ori_feb2023.rds')
write.xlsx(coseY_ori, 'C:/!bso/cosechas/cosechas_ConAjuste_feb2023.xlsx')

avg_grw <- coseY %>% 
  select(mesPos, par0Rel, cosechaY, grw_par0_1) %>%
  group_by(cosechaY) %>% 
  arrange(cosechaY, mesPos) %>% 
  mutate(difPar0 = par0Rel - dplyr::lag(par0Rel, 1)) %>% 
  ungroup() %>% 
  select(mesPos, grw_par0_1) %>% 
  group_by(mesPos) %>% 
  summarise_all(mean, na.rm = T) %>% 
  dplyr::rename(avg_grw_par0 = grw_par0_1)

ggplot(avg_grw, aes(x = mesPos, y = avg_grw_par0)) + geom_line(group=1, size = 1.15)

avg_grw_gr <- coseY %>% 
  dplyr::filter(cosechaY < 2023) %>%
  select(mesPos, par0Rel, cosechaY, grw_par0_1) %>%
  group_by(cosechaY) %>% 
  arrange(cosechaY, mesPos) %>% 
  mutate(difPar0 = par0Rel - dplyr::lag(par0Rel, 1)) %>% 
  ungroup() %>% 
  mutate(groupCose = ifelse(cosechaY < 2021, 'avg_2015_2020', 'avg_2021_2022')) %>% 
  select(-cosechaY, -par0Rel, -difPar0) %>% 
  group_by(mesPos, groupCose) %>% 
  summarise_all(mean, na.rm = T) %>% 
  pivot_wider(names_from = groupCose, values_from = grw_par0_1)

fcst_coseY <- coseY %>% 
  ungroup() %>% 
  select(cosechaY, mesPos, par0Rel, totalMdes) %>% 
  pivot_wider(names_from = cosechaY, values_from = c(par0Rel, totalMdes)) %>% 
  left_join(avg_grw, by = 'mesPos') %>% 
  left_join(avg_grw_gr, by = 'mesPos') 
  
for (i in 1:10){
  j = i-1
  fcst_coseY$par0Rel_2016[86+i] = fcst_coseY$par0Rel_2016[86+j] * (1 +  fcst_coseY$avg_grw_par0[86+j])
  fcst_coseY$par0Rel_2017[74+i] = fcst_coseY$par0Rel_2017[74+j] * (1 +  fcst_coseY$avg_grw_par0[74+j])
  fcst_coseY$par0Rel_2018[62+i] = fcst_coseY$par0Rel_2018[62+j] * (1 +  fcst_coseY$avg_grw_par0[62+j])
  fcst_coseY$par0Rel_2019[50+i] = fcst_coseY$par0Rel_2019[50+j] * (1 +  fcst_coseY$avg_grw_par0[50+j])
  fcst_coseY$par0Rel_2020[38+i] = fcst_coseY$par0Rel_2020[38+j] * (1 +  fcst_coseY$avg_grw_par0[38+j])
  fcst_coseY$par0Rel_2021[26+i] = fcst_coseY$par0Rel_2021[26+j] * (1 +  fcst_coseY$avg_grw_par0[26+j])
  fcst_coseY$par0Rel_2022[14+i] = fcst_coseY$par0Rel_2022[14+j] * (1 +  fcst_coseY$avg_grw_par0[14+j])
}
