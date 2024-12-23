
library(dplyr)
library(foreign)
library(reshape)
library(reshape2)
library(stringr)
library(lubridate)
library(Hmisc)
library(data.table)
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
library(paletteer)
library(plotly)
library(kableExtra)
library(glmnet)

remove(list = ls())
options("encoding" = "UTF-8")
#Sys.setlocale("LC_MESSAGES", 'es_MX.UTF-8')
options(scipen = 999)
cbp1 <- c("#4198B5", "#246D94", "#083554", "#D43B1B",
          "#E96732", "#FB9263")
paleta <- colorRampPalette(c("slateblue4","purple4","slateblue3","darkorchid3","red3","tan2","yellow2","white"),bias=1.5)

#===============================================================================
#vipDescFull_cc <- readRDS( 'D:/!bso/vipRC/vipDescriptives_CompleteList_Dic2022_allVars.rds')
dailyMMcl <- readRDS('C:/!bso/vipCartera/dailyMM_feb2023.rds')

vipDescFull_cc <- readRDS('D:/!bso/vipRC/vipDescriptives_CompleteList_Feb2023_allVars_v2.rds')
vipDescFull_ccFeb <- read.xlsx('D:/!bso/vipRC/vipDescriptives_CompleteList_Feb2023_allVars.xlsx')

miss <- vipDescFull_ccEne %>% 
  dplyr::filter(!(CI %in% vipDescFull_cc$CI)) %>% 
  select(CI, OPERACION) %>% 
  left_join(dailyMMcl, by = c('CI', 'OPERACION'))
#===============================================================================
# infocred
infoRaw <- fread('C:/!bso/Cargamensual_Infocred/BSO202301_utf8.txt', encoding = 'UTF-8')
nrowInfo <- nrow(infoRaw)

infoPerf_Compra <- infoRaw %>% 
  mutate(CI = paste0(`NRO DOCUMENTO`, EXT)) %>% 
  mutate(opBSO_ = ifelse(`SIGLA SBEF` == 'BSO', NumeroOp, '-')) %>% 
  mutate(dBSO_ = ifelse(`SIGLA SBEF` == 'BSO' & str_detect(`TIPO OBLIGADO SBEF`, 'A -'), 1, 0)) %>%
  mutate(isBSO_ = ifelse(`SIGLA SBEF` == 'BSO', 1, 0)) %>%
  group_by(CI) %>% 
  mutate(dBSO_total = sum(dBSO_),
         isBSO = max(isBSO_),
         isBSO_total = sum(isBSO_)) %>%
  ungroup() %>% 
  #dplyr::filter(dBSO == 1) %>%
  mutate(idBSO = opBSO_) %>%
  separate_wider_delim(opBSO_, delim = '-',
                       names = c('CTACLIENTE_all', 'OPERACION'),
                       too_few = 'align_start',
                       too_many = 'merge') %>% 
  mutate(CTACLIENTE_d_ = ifelse(`SIGLA SBEF` == 'BSO' & str_detect(`TIPO OBLIGADO SBEF`, 'A -'), 
                                as.integer(CTACLIENTE_all), 0)) %>% 
  group_by(CI) %>% 
  mutate(CTACLIENTE = max(CTACLIENTE_d_, na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::filter(str_detect(`TIPO OBLIGADO SBEF`, 'A -')) %>% 
  dplyr::filter(`SIGLA SBEF` != 'BSO') %>%
  dplyr::filter(`SBEF CALIFICACION` == 'A') %>%
  dplyr::filter(`SBEF VIGENTE` > 0) %>% 
  select(CTACLIENTE, `TIPO OBLIGADO SBEF`, DiasMora, `SIGLA SBEF`, 
         `ENTIDAD SBEF`,  `FECHA INICIO OPERACION`, `SBEF VIGENTE`,
         MontoOriginal, MonedaOrigen, `TIPO CREDITO SBEF` ,
         `SBEF VIGENTE`, `SBEF CALIFICACION`, `FECHA DECLARACION`,
         idBSO, dBSO_total) %>% 
  mutate(MontoOriginal = ifelse(MonedaOrigen == 'MN', as.numeric(MontoOriginal)/6.86, as.numeric(MontoOriginal))) %>% 
  mutate(saldoVig = ifelse(MonedaOrigen == 'MN',  as.numeric(`SBEF VIGENTE`)/6.86,   as.numeric(`SBEF VIGENTE`))) %>%
  relocate(starts_with('CTACLIEN')) %>% 
  dplyr::filter(!is.na(CTACLIENTE)) %>%
  dplyr::filter(CTACLIENTE > 0) %>% 
  relocate(CTACLIENTE, idBSO)

  
cc <- vipDescFull_cc %>% 
  left_join(infoPerf_Compra, by = 'CTACLIENTE') %>% 
  dplyr::filter(!is.na( `SBEF VIGENTE`)) %>% 
  dplyr::filter(`SBEF VIGENTE` > 0) %>% 
  dplyr::filter(VIP_RC == 'VIP-RC') %>% 
  group_by(CTACLIENTE) %>% 
  dplyr::filter(row_number()==1) %>% 
  ungroup() %>% 
  dplyr::rename(Saldo_USD_SF = `SBEF VIGENTE`,
                Calificacion_SF = `SBEF CALIFICACION`,
                Entidad = `ENTIDAD SBEF`,
                Tipo_Credito_SF = `TIPO CREDITO SBEF`,
                Sigla_Entidad_SF = `SIGLA SBEF`) %>% 
  mutate(Saldo_USD_SF = Saldo_USD_SF/6.86) %>% 
  dplyr::filter(Calificacion_SF == 'A')
sum(cc$Saldo_USD_SF)
table(cc$Lista_GNN)
table(cc$Calificacion_SF)

#===============================================================================
# tables
gph <- cc %>% 
  select(Lista_GNN, VIP_Viable, Saldo_USD_SF, Saldo_USD) %>% 
  group_by(Lista_GNN, VIP_Viable) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF), Saldo_USD = sum(Saldo_USD),
            Operaciones = n()) %>% 
  arrange(Lista_GNN, VIP_Viable, desc(Saldo_USD_SF)) %>% 
  adorn_totals('row') %>% 
  mutate(Promedio = Saldo_USD_SF/Operaciones) %>% 
  glimpse()

kable(gph, row.names = F, format = 'latex', booktabs = T,
      digits = 0, format.args = list(decimal.mark = ".", big.mark = ","))
write.xlsx(gph, 'C:/!bso/vipCartera/compra_tab_1.xlsx')

gph <- cc %>% 
  select(Lista_GNN, VIP_Viable, Tipo_Credito_SF,  Saldo_USD_SF, Saldo_USD) %>% 
  mutate(Tipo_Credito_SF = case_when(substr(Tipo_Credito_SF,1,1) == 'M'~'Micro',
                                     substr(Tipo_Credito_SF,1,1) == 'P'~'PyMe',
                                     substr(Tipo_Credito_SF,1,1) == 'H'~'Hipotecario',
                                     substr(Tipo_Credito_SF,1,1) == 'N'~'Consumo',)) %>% 
  group_by(Lista_GNN, VIP_Viable, Tipo_Credito_SF) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF), Saldo_USD = sum(Saldo_USD),
            Operaciones = n()) %>% 
  adorn_totals('row') %>%
  mutate(Promedio = Saldo_USD_SF/Operaciones) %>% 
  arrange(Lista_GNN, VIP_Viable, desc(Saldo_USD_SF)) %>% 
  glimpse()
kable(gph, row.names = F, format = 'latex', booktabs = T,
      digits = 0, format.args = list(decimal.mark = ".", big.mark = ","))
write.xlsx(gph, 'C:/!bso/vipCartera/compra_tab_2.xlsx')

gph <- cc %>% 
  select( Sigla_Entidad_SF,  Saldo_USD_SF, Saldo_USD) %>% 
  group_by(Sigla_Entidad_SF) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF), Saldo_USD = sum(Saldo_USD),
            Operaciones = n()) %>%
  dplyr::filter(Operaciones > 10) %>% 
  arrange(desc(Operaciones)) %>% 
  arrange(desc(Saldo_USD_SF)) %>% 
  select(Sigla_Entidad_SF, Saldo_USD_SF) %>% 
  glimpse()
write.xlsx(gph, 'C:/!bso/vipCartera/compra_tab_3.xlsx')

ggplot(gph, aes(x = reorder(Sigla_Entidad_SF, Saldo_USD_SF), y = Saldo_USD_SF)) + 
  geom_bar(stat = 'identity') + coord_flip() + theme_minimal() +
  xlab('Entidad') + ylab('Saldo USD') +
  scale_y_continuous(label=comma)
ggsave('D:/!bso/compraCartera/oportXEntidad.png')


gph <- cc %>% 
  select( Sigla_Entidad_SF,  Saldo_USD_SF, Saldo_USD, Tipo_Credito_SF) %>% 
  group_by(Sigla_Entidad_SF, Tipo_Credito_SF) %>% 
  mutate(Tipo_Credito_SF = case_when(substr(Tipo_Credito_SF,1,1) == 'M'~'Micro',
                                     substr(Tipo_Credito_SF,1,1) == 'P'~'PyMe',
                                     substr(Tipo_Credito_SF,1,1) == 'H'~'Hipotecario',
                                     substr(Tipo_Credito_SF,1,1) == 'N'~'Consumo',)) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF), Saldo_USD = sum(Saldo_USD),
            Operaciones = n()) %>%
  dplyr::filter(Operaciones > 10) %>% 
  arrange(desc(Operaciones)) %>% 
  arrange(desc(Saldo_USD_SF)) %>% 
  select(Sigla_Entidad_SF, Saldo_USD_SF, Tipo_Credito_SF) %>% 
  ungroup() %>% 
  mutate(Entidad2 = ifelse(Saldo_USD_SF < 1000000, 'Otras',Sigla_Entidad_SF)) %>% 
  ungroup() %>% 
  select(-Sigla_Entidad_SF) %>% 
  group_by(Entidad2, Tipo_Credito_SF) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_by(Entidad2) %>% 
  mutate(saldoTot = sum(Saldo_USD_SF)) %>% 
  glimpse()
write.xlsx(gph, 'C:/!bso/vipCartera/compra_tab_4.xlsx')


ggplot(gph, aes(x = reorder(Entidad2, saldoTot), y = Saldo_USD_SF, fill = Tipo_Credito_SF)) + 
  geom_bar(stat = 'identity') + coord_flip() + theme_minimal() +
  xlab('Entidad') + ylab('Saldo USD') +
  scale_y_continuous(label=comma) + 
  scale_fill_manual(values = paleta(4))+ 
  theme(legend.position = 'bottom')+ guides(fill = guide_legend(nrow = 1)) + 
  geom_text(aes(label = round(Saldo_USD_SF/1000000, 2)), 
            position = position_stack(vjust = 0.5))

  
ggsave('D:/!bso/compraCartera/oportXEntidad_tipoCred.png')

gph <- cc %>% 
  select( Sigla_Entidad_SF,  Saldo_USD_SF, Saldo_USD, Sucursal) %>% 
  group_by(Sigla_Entidad_SF, Sucursal) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF), Saldo_USD = sum(Saldo_USD),
            Operaciones = n()) %>%
  dplyr::filter(Operaciones > 10) %>% 
  arrange(desc(Operaciones)) %>% 
  arrange(desc(Saldo_USD_SF)) %>% 
  select(Sigla_Entidad_SF, Saldo_USD_SF, Sucursal) %>% 
  ungroup() %>% 
  mutate(Entidad2 = ifelse(Saldo_USD_SF < 500000, 'Otras',Sigla_Entidad_SF)) %>% 
  ungroup() %>% 
  select(-Sigla_Entidad_SF) %>% 
  group_by(Entidad2) %>% 
  mutate(saldoTot = sum(Saldo_USD_SF)) %>% 
  glimpse()
write.xlsx(gph, 'C:/!bso/vipCartera/compra_tab_5.xlsx')


ggplot(gph[gph$Entidad2 != 'Otras',], aes(x = reorder(Entidad2, saldoTot), y = Saldo_USD_SF, fill = Sucursal)) + 
  geom_bar(stat = 'identity') + coord_flip() + theme_minimal() +
  xlab('Entidad') + ylab('Saldo USD') +
  scale_y_continuous(label=comma) + scale_fill_manual(values = paleta(7)) + 
  theme(legend.position = 'bottom')+ guides(fill = guide_legend(nrow = 1)) + 
  geom_text(aes(label = round(Saldo_USD_SF/1000000, 1)), 
            position = position_stack(vjust = 0.5), size = 3.5)

ggsave('D:/!bso/compraCartera/oportXEntidad_sucursal.png')

#=============================================================================
# Joining vip & lead sets
# base sexo
bdc_I_Wanna_Have_Sex <- readRDS('D:/!bso/girCartera/rds_v3/ec_feb2023.rds') %>% 
  select(CTACLIENTE, GENERO) %>% 
  dplyr::rename(Genero = GENERO) %>% 
  distinct_all()

ccExp <- cc %>% 
  select(Calificacion_SF, Saldo_USD_SF, MontoOriginal, Entidad,
         Sigla_Entidad_SF, `TIPO OBLIGADO SBEF`, CTACLIENTE) %>% 
  dplyr::rename(MontoOriginal_SF = MontoOriginal,
                Tipo_Obligado_SF = `TIPO OBLIGADO SBEF`) %>% 
  glimpse()

# Final join

vipLead_final <- vipDescFull_cc %>% 
  left_join(ccExp, by = 'CTACLIENTE') %>%
  left_join(bdc_I_Wanna_Have_Sex, by = 'CTACLIENTE') %>%
  dplyr::rename(Historial_SF = histStr2,
                Entidad_SF = Entidad) %>% 
  glimpse()

write.xlsx(vipLead_final, 'D:/!bso/compraCartera/AllLeads_final_feb2023_ae_v2.xlsx')

leadsJoin <- vipLead_final %>% 
  select(CTACLIENTE, OPERACION, ends_with('_SF'))
write_rds(leadsJoin, 'C:/!bso/vipCartera/compra_feb2023_ae.rds')

# Checking sums and table by gender
check <- vipLead_final %>% 
  ungroup() %>% 
  group_by(CI) %>% 
  arrange(desc(CI)) %>% 
  mutate(Saldo_USD_SF = ifelse(row_number()>1, 0, Saldo_USD_SF)) %>% 
  ungroup() %>% 
  select(Saldo_USD_SF, Oportunidad_Viable, VIP_Viable, Genero) %>% 
  mutate(Op_Viable = ifelse(Oportunidad_Viable>0,1,0),
         Op_Compra = ifelse(Saldo_USD_SF>0,1,0)) %>% 
  group_by(VIP_Viable, Genero) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF, na.rm = T),
            Oportunidad_Viable = sum(Oportunidad_Viable, na.rm = T),
            N_Ops_Preaprobadas = sum(Op_Viable, na.rm = T),
            N_Ops_Compra = sum(Op_Compra, na.rm = T)) %>% 
  adorn_totals('row') %>% 
  glimpse()

write.xlsx(check, 'D:/!bso/compraCartera/tabla_genero_dic2022.xlsx')

check2 <- vipLead_final %>% 
  ungroup() %>% 
  group_by(CI) %>% 
  arrange(desc(CI)) %>% 
  mutate(Saldo_USD_SF = ifelse(row_number()>1, 0, Saldo_USD_SF)) %>% 
  ungroup() %>% 
  select(Saldo_USD_SF, Oportunidad_Viable, VIP_Viable, Genero) %>% 
  mutate(Op_Viable = ifelse(Oportunidad_Viable>0,1,0),
         Op_Compra = ifelse(Saldo_USD_SF>0,1,0)) %>% 
  group_by(VIP_Viable, Genero) %>% 
  summarise(Saldo_USD_SF = sum(Saldo_USD_SF, na.rm = T),
            Oportunidad_Viable = sum(Oportunidad_Viable, na.rm = T),
            N_Ops_Preaprobadas = sum(Op_Viable, na.rm = T),
            N_Ops_Compra = sum(Op_Compra, na.rm = T)) %>% 
  adorn_totals('row') %>% 
  glimpse()

write.xlsx(check, 'D:/!bso/compraCartera/tabla_genero_dic2022.xlsx')
