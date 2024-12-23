#-------------------------
remove(list = ls())
gc()
options("encoding" = "UTF-8")
library(dplyr)
library(foreign)
library(reshape)
library(reshape2)
library(stringr)
library(lubridate)
library(Hmisc)
library(data.table)
library(gt)
library(tidyverse)
library(readxl)
library(writexl)
library(xts)
library(forecast)
library(quantmod)
library(astsa)
library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
library(timetk)     # For consistent time series coercion functions
library(stringr)    # Working with strings
library(forcats)    # Working with factors/categorical data
library(timeSeries)
library(tseries)
library(xtable)
library(openxlsx)
library(hrbrthemes)
library(viridis)
library(scales)
library(janitor)
library(RColorBrewer)
library(glue)
library(paletteer)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)

cbp1 <- c("#BE33FF", "#FF9F33")

bdcExp <- read.csv('C:/!bso/girCartera/GIRconOperaciones.csv')
bdcAgg <- bdcExp %>%
  dplyr::filter(Fecha == '2022-06-30') %>% 
  ungroup() %>%
  # select(-Rango_Desembolso, -Sector_Cartera,
  #        -Tipo_Credito, -Sector_Destino, -Sector_Actividad) %>%
  select(Fecha, Cartera_Bruta, PaR_1_Bruta, Cartera_Castigada,
         Operaciones_Totales, PaR_30_Bruta, Operaciones_PaR0, Operaciones_PaR30, 
         Operaciones_Castigadas, NumOperacionesDif, NumOperacionesDifASFI, Cartera_Reprogramada_Vigente,
         Cartera_Diferida_ASFI, NumOperacionesReprog,AGENCIA)%>%
  group_by(Fecha, AGENCIA) %>%
  dplyr::filter(AGENCIA==734) %>% 
  summarise_all(sum, na.rm = T) %>%
  mutate(moraRel = PaR_1_Bruta/Cartera_Bruta*100) %>% 
  mutate(mmc = (PaR_1_Bruta+Cartera_Castigada)/Cartera_Bruta*100) %>% 
  mutate(moraRel30 = PaR_30_Bruta/Cartera_Bruta*100)%>% 
  mutate(mmc30 = (PaR_30_Bruta+Cartera_Castigada)/Cartera_Bruta*100) %>% 
  
  mutate(moraRelOp = Operaciones_PaR0/Operaciones_Totales*100) %>% 
  mutate(mmcOp = (Operaciones_PaR0+Cartera_Castigada)/Operaciones_Totales*100) %>% 
  mutate(moraRelOp = Operaciones_PaR0/Operaciones_Totales*100) %>% 
  mutate(mmcOp = (Operaciones_PaR0+Cartera_Castigada)/Operaciones_Totales*100) %>%
  
  select(Cartera_Bruta, Operaciones_Totales, PaR_1_Bruta, Operaciones_PaR0, 
         moraRel, moraRelOp, Cartera_Castigada, Operaciones_Castigadas,
         mmc, mmcOp, NumOperacionesDif, NumOperacionesDifASFI, Cartera_Reprogramada_Vigente,
         Cartera_Diferida_ASFI, NumOperacionesReprog, AGENCIA) %>% 
  setNames(paste0('BSO_', names(.))) %>% 
  
  glimpse()


bdcAggs <- bdcExp %>%
  dplyr::filter(Fecha == '2022-06-30') %>% 
  ungroup() %>%
  # select(-Rango_Desembolso, -Sector_Cartera,
  #        -Tipo_Credito, -Sector_Destino, -Sector_Actividad) %>%
  dplyr::filter(AGENCIA==734) %>%
  select(Fecha, Cartera_Bruta, PaR_1_Bruta, Cartera_Castigada,
         Operaciones_Totales, PaR_30_Bruta, Operaciones_PaR0, Operaciones_PaR30, 
         Operaciones_Castigadas, NumOperacionesDif, NumOperacionesDifASFI, 
         Cartera_Reprogramada_Vigente, Cartera_Diferida_ASFI,NumOperacionesReprog)%>%
  
  group_by(Fecha) %>%
  summarise_all(sum, na.rm = T) %>%
  mutate(moraRel = PaR_1_Bruta/Cartera_Bruta*100) %>% 
  mutate(mmc = (PaR_1_Bruta+Cartera_Castigada)/Cartera_Bruta*100) %>% 
  mutate(moraRel30 = PaR_1_Bruta/Cartera_Bruta*100)%>% 
  mutate(mmc30 = (PaR_30_Bruta+Cartera_Castigada)/Cartera_Bruta*100) %>% 
  
  mutate(moraRelOp = Operaciones_PaR0/Operaciones_Totales*100) %>% 
  mutate(mmcOp = (Operaciones_PaR0+Cartera_Castigada)/Operaciones_Totales*100) %>% 
  #mutate(moraRelOp = Operaciones_PaR30/Operaciones_Totales*100) %>% 
  mutate(mmcOp = (Operaciones_PaR30+Cartera_Castigada)/Operaciones_Totales*100) %>%
  
  mutate(parcbs = 100*Cartera_Bruta/bdcAgg$BSO_Cartera_Bruta) %>%
  mutate(parcbo = 100*Operaciones_Totales/bdcAgg$BSO_Operaciones_Totales) %>%
  mutate(parcms = 100*PaR_1_Bruta/bdcAgg$BSO_PaR_1_Bruta) %>%
  mutate(parcmo = 100*Operaciones_PaR0/bdcAgg$BSO_Operaciones_PaR0) %>%
  mutate(parccs = 100*Cartera_Castigada/bdcAgg$BSO_Cartera_Castigada) %>%
  mutate(parcco = 100*Operaciones_Castigadas/bdcAgg$BSO_Operaciones_Castigadas) %>%
  mutate(parcds = 100*Cartera_Diferida_ASFI/bdcAgg$BSO_Cartera_Diferida_ASFI) %>%
  mutate(parcdo = 100*NumOperacionesDifASFI/bdcAgg$BSO_NumOperacionesDifASFI) %>%
  mutate(parcrs = 100*Cartera_Reprogramada_Vigente/bdcAgg$BSO_Cartera_Reprogramada_Vigente) %>%
  mutate(parcro = 100*NumOperacionesReprog/bdcAgg$BSO_NumOperacionesReprog) %>%
  mutate(parims = NA) %>%
  mutate(parimo = NA) %>%
  mutate(parimcs = NA) %>%
  mutate(parimco = NA) %>%
  
  mutate(rankcbs = rank(-Cartera_Bruta))%>%
  mutate(rankcbo = rank(-Operaciones_Totales))%>%
  mutate(rankcms = rank(PaR_1_Bruta))%>%
  mutate(rankcmo = rank(Operaciones_PaR0))%>%
  mutate(rankims = rank(mmc))%>%
  mutate(rankimo = rank(moraRelOp))%>%
  mutate(rankcas = rank(Cartera_Castigada))%>%
  mutate(rankcao = rank(Operaciones_Castigadas))%>%
  mutate(rankimcs = rank(mmc30))%>%
  mutate(rankimco = rank(mmcOp)) %>%
  mutate(rankcds = rank(Cartera_Diferida_ASFI))%>%
  mutate(rankcdo = rank(NumOperacionesDifASFI)) %>% 
  mutate(rankcrs = rank(Cartera_Reprogramada_Vigente))%>%
  mutate(rankcro = rank(NumOperacionesReprog)) %>% 
  
  #mutate(Sucursal=str_replace(Sucursal," ","")) %>% 
  select(Cartera_Bruta, Operaciones_Totales, PaR_1_Bruta, Operaciones_PaR0, 
         moraRel, moraRelOp, Cartera_Castigada, Operaciones_Castigadas,
         mmc, mmcOp, Cartera_Reprogramada_Vigente, NumOperacionesReprog, 
         Cartera_Diferida_ASFI, NumOperacionesDifASFI, 
         rankcbs,rankcbo,rankcms,rankcmo,rankims,rankimo,
         rankcas,rankcao,rankimcs,rankimco,rankcds,rankcdo,rankcrs,rankcro,
         parcbs,parcbo,parcms,parcmo,parccs,parcco,parcds,parcdo,parcrs,parcro,
         parims,parimo,parimcs,parimco)

  
# df <- bdcAggs %>% 
#   mutate(group=1) %>% 
#   spread(Sucursal,Cartera_Bruta)
#   #pivot_wider(names_from = Sucursal,values_from = Cartera_Bruta)
bdcAggs2 <- bdcAggs %>%
  select(rankcbs,rankcbo,rankcms,rankcmo,rankims,rankimo,
         rankcas,rankcao,rankimcs,rankimco,rankcds,rankcdo,rankcrs,rankcro)  
  #mutate(Sucursal=paste0(Sucursal, '_Ranking'))

bdcAggs3 <- bdcAggs %>%
  select(Cartera_Bruta, Operaciones_Totales, PaR_1_Bruta, Operaciones_PaR0, 
         moraRel, moraRelOp, Cartera_Castigada, Operaciones_Castigadas,
         mmc, mmcOp, Cartera_Diferida_ASFI, NumOperacionesDifASFI,
         Cartera_Reprogramada_Vigente, NumOperacionesReprog) 
  #mutate(Sucursal=paste0(Sucursal, '_Monto USD'))
bdcAggs4 <- bdcAggs %>%
  select(parcbs,parcbo,parcms,parcmo,parims,parimo,parccs,parcco,
         parimcs,parimco,parcds,parcdo,parcrs,parcro) 
  #mutate(Sucursal=paste0(Sucursal,'_Participacion'))
  
# df3 <- df2 %>% 
#   pivot_wider(names_from = Sucursal,values_from = c(Cartera_Bruta, Operaciones_Totales, PaR_1_Bruta, Operaciones_PaR0, 
#                                                     moraRel, moraRelOp, Cartera_Castigada, Operaciones_Castigadas,
#                                                   rankcas,rankcao,rankimcs,rankimco))
bdcAggs3 %>% gt()
df2 <- t(bdcAggs2[-c(1,2,3)])
df3 <- t(bdcAggs3[-c(1,2,3)])
df4 <- t(bdcAggs4[-c(1,2,3)])
df <- bind_cols(df2,df3,df4)
#df <- as.data.frame(cbind(df2,df3))
colnames(df) <- c(bdcAggs2[[2]],bdcAggs3[[2]],bdcAggs4[[2]])


# deptos <- bdcAggs$Sucursal
# df %>% gt() %>%
#   # tab_spanner(label = deptos[2],columns = ends_with(deptos[2]))
#   for (i in 1:length(deptos)) {
#     tab_spanner(label=deptos[i], columns = ends_with(deptos[i]))
#   }
df %>% gt()

df <- df %>% 
  select(sort(names(.))) %>% 
  mutate(names= rep(c("Saldo","Operaciones"),nrow(df)/2))%>%
  mutate(carteras=c(rep("Cartera Bruta",2),rep("Cartera en Mora",2),rep("Índice de Mora",2),
         rep("Cartera Castigada",2),rep("IM Castigada",2),rep("Cartera Diferida",2),rep("Cartera Reprogramada",2)))


df %>%
  gt(rowname_col = "names",groupname_col = "carteras") %>%
  tab_options(row_group.as_column = T) %>% 
  fmt_number(columns = ends_with("USD"),decimals = 2) %>% 
  fmt_number(columns = ends_with("cion"),decimals = 2) %>% 
  fmt_missing(columns = ends_with("cion"), missing_text = '') %>% 
  tab_spanner_delim(delim = "_") %>%
  tab_header(title = "Al 30 de Septiembre de 2022") %>%   
  opt_stylize(style = 6,color = "red")
  # tab_style(locations = cells_title(groups = "title"),
  #           style = list(cell_text(weight = "bold",size = 15))) %>% 
  # tab_style(locations = cells_column_labels(columns = everything()),
  #           style = list(cell_borders(sides = "bottom",weight = px(3)),
  #                        cell_text(weight = "bold"),cell_fill(color = cbp1[2]))) %>% 
  # tab_style(locations = cells_column_spanners(spanners = everything()),
  #           style = list(cell_text(weight = "bold"),cell_fill(color = cbp1[1]))) %>% 
  # tab_style(locations = cells_body(columns = everything(),rows = 1),
  #           style = list(cell_fill(color = cbp1[1])))
  # data_color(columns = everything(),colors = cbp1,apply_to = "fill")
  # tab_style(locations = cells_title(groups = "title"),
  #   style = list(cell_text(color = cbp1)))





df <- bdcAggs %>% 
  group_split(Sucursal) 
  
#setNames(paste0("Sucursal", names(.))) %>%
  #select(-Sucursal) %>%

sucursales <- bdcAggs$Sucursal
lista <- vector(mode="list",length = length(sucursales))
for (i in 1:length(sucursales)) {
  lista[[i]] <- df[[i]] %>% 
    setNames(paste0(sucursales[i], names(.)))
}


  setNames(paste0('BSO_', names(.))) %>%
  
###############################################################################
###############################################################################
df <- bdcAgg %>% 
  select(Fecha, Cartera_Bruta, PaR_1_Bruta, Cartera_Castigada,
         Operaciones_Totales, PaR_30_Bruta, Operaciones_PaR0, Operaciones_PaR30, 
         Operaciones_Castigadas, Sucursal)%>%
  
# create matrix with 3 columns
data = matrix(c(1:10), ncol=5)

# specify row and column names 
rownames(data) = c('Fruits', 'vegetables')
colnames(data) = c('apple', 'banana', 'lemon')

# convert matrix to table
data = as.table(data)

# display 
data