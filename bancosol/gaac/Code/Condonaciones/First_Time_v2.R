####____CARGA DE PAQUETES, FUNCIONES Y OTROS____####
remove(list = ls())
gc()
options("encoding" = "UTF-8")
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
library(ggrepel)
library(ca)
remove(list = ls())
gc()
options("encoding" = "UTF-8")
options(scipen = 999)
####____FEATURES AT MOMENT OF DISBURSEMENT____####
dfTotal <- readRDS('D:/!bso/features/Operaciones_Ene15Jul23.rds')
dfTotal_2019 <- dfTotal %>%
  mutate(Operaciones=1) %>% 
  select(-NOMBRE, -GENERO, -CALIFICACION, -DIASMORA, -moraMax) %>% 
  dplyr::filter(fdes>=as.Date("2019-01-01")) 
####_____TIMES TO FIRST____####
ptFull <- readRDS("D:/!bso/firstTimes/PagosHist_Ene18Jul23.rds")
checkpt <- ptFull %>% 
  group_by(myPago) %>% 
  summarise(NObs= n(),NOps=n_distinct(Operacion), check=NObs==NOps)

pt_grouped <- ptFull %>% 
  dplyr::filter(myPago>"Dic. 2018" & myPago <"Jul.2023") %>% 
  mutate(UltMesPagoTardio = ifelse(myPago == "Jun. 2023",1,0)) %>% 
  mutate(Ult12MesesPagoTardio = ifelse(myPago >= "Ago. 2022",1,0)) %>% 
  group_by(Operacion) %>% 
  mutate(Ult12MesesPagoTardio = sum(Ult12MesesPagoTardio)) %>% 
  ungroup() %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(FechaFirstTardio = min(FechaPago),
            UltMesPagoTardio = max(UltMesPagoTardio),
            Ult12MesesPagoTardio = max(Ult12MesesPagoTardio)) %>% 
  ungroup()

# bdcPt <- bdc %>% 
#   inner_join(pt_grouped, by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
#   dplyr::filter(Ult12MesesPagoTardio>0)
####____CONDONACIONES____####
condFull <- readRDS('D:/!bso/condonaciones/CondFull_Ene2019Jun2023.rds')
zeros <- condFull %>%  
  dplyr::filter(Total_Cond_Cap_Int==0) %>% 
  group_by(as.yearmon(Fecha)) %>% summarise(nOps=n())

cond_clean <- condFull %>% 
  select(Fecha, Cuenta, Operacion, CondCapInt_USD = Total_Cond_Cap_Int,
         CondInt_USD = Cond_Int, CondCap_USD = Cond_Cap) %>%
  mutate(myCond = as.yearmon(Fecha)) %>% 
  group_by(myCond, Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(Fecha),
            across(starts_with("Cond"),~sum(.x))) %>% 
  ungroup()

checkcond <- cond_clean %>% group_by(myCond) %>% 
  summarise(NObs=n(), NOps=n_distinct(Operacion), check=NObs==NOps)  

cond_grouped <- cond_clean %>% 
  mutate(UltMesCondonado = ifelse(myCond == "Jun. 2023",1,0)) %>% 
  mutate(Ult12MesesCondonado = ifelse(myCond >= "Jul. 2022",1,0)) %>% 
  group_by(Cuenta, Operacion) %>% 
  summarise(FechaFirstCond = min(FechaFirstCond),
            UltMesCondonado = max(UltMesCondonado),
            Ult12MesesCondonado = sum(Ult12MesesCondonado)) %>% 
  ungroup()

####____MORA AL CIERRE____####
dfOps <- readRDS('D:/!bso/features/Historial_Operaciones.rds') %>% 
  mutate(tuvoMora = ifelse(DIASMORA>0, 1, 0)) %>% 
  glimpse()

measuresMora <- dfOps %>% 
  select(CTACLIENTE, OPERACION, monDate, fdes, DIASMORA, MONTOUS, tuvoMora) %>% 
  group_by(CTACLIENTE, OPERACION) %>% #Se agrupa por operación de cada cliente
  summarise(meanMora = mean(DIASMORA, na.rm = T),
            IntMoraBSO = sum(tuvoMora)/n(),
            fdes_ori = min(fdes),#Fecha más antigua para reprogramadas
            lastM = as.Date(max(monDate), frac = 1)) %>% #último mes en las bdc
  ungroup() %>% 
  mutate(loanDays = as.integer(lastM - fdes_ori)) #días de préstamo

FirstMora <- dfOps %>% 
  select(monDate, CTACLIENTE, OPERACION, DIASMORA, tuvoMora) %>% 
  dplyr::filter(tuvoMora>0) %>% 
  group_by(CTACLIENTE, OPERACION) %>% #Se agrupa por operación de cada cliente
  summarise(FechaFirstMora = min(monDate)) 

remove(list=c("dfOps"))
####____MORA_IM_2018____####
moraIntraMes <- readRDS(paste0("D:/!bso/accion/moraIntraMes_Ene2018Jun2023.rds")) %>% 
  dplyr::filter(!is.na(maximaDPD)) %>% 
  mutate(tuvoMora = ifelse(maximaDPD>0, 1, 0)) 

measuresMoraIM <- moraIntraMes %>% 
  # dplyr::filter(monDate!="jun. 2023") %>% #Filtro para reproducir meses anteriores
  group_by(CTACLIENTE, OPERACION) %>% 
  # mutate(mesDiasMora = if_else(maximaDPD==0, 2014, year(monDate))) %>%
  summarise(meanMoraIM = mean(maximaDPD),
            IntMoraIM = sum(tuvoMora)/n()) %>% #lastMoraIM_cl = max(mesDiasMora), maxMoraIM_cl = max(maximaDPD)
  ungroup()

FirstMoraIM <- moraIntraMes %>% 
  group_by(CTACLIENTE, OPERACION) %>% #Se agrupa por operación de cada cliente
  dplyr::filter(tuvoMora>0) %>% 
  mutate(monDate = as.Date(monDate, frac=1))
sapply(FirstMoraIM, function(x){length(which(is.na(x)))})
FirstMoraIM <- FirstMoraIM %>% 
  mutate(UltMesMoraIM = ifelse(as.yearmon(monDate) == "Jun. 2023" & tuvoMora==1,1,0)) %>% 
  group_by(CTACLIENTE, OPERACION) %>%
  # mutate(FVEN_ULTPAGO = ifelse(is.na(FVEN_ULTPAGO), monDate, FVEN_ULTPAGO)) %>% 
  summarise(FechaFirstMoraIM = min(FVEN_ULTPAGO),
            MesFirstMoraIM = min(monDate)) %>% 
  ungroup()
dfJoin <- dfTotal_2019 %>% 
  left_join(cond_grouped,by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(pt_grouped,by=c("CTACLIENTE"="Cuenta","OPERACION"="Operacion")) %>% 
  left_join(FirstMora, by=c("CTACLIENTE","OPERACION")) %>% 
  left_join(FirstMoraIM, by=c("CTACLIENTE","OPERACION")) 
  

# dfJoinFull <- dfTotal %>%
#   left_join(condFull_2019,by=c("CTACLIENTE","OPERACION")) %>% 
#   left_join(ptardioFull_2019,by=c("CTACLIENTE","OPERACION"))

# condFull_2019$OPERACION[!condFull_2019$OPERACION %in% dfTotal$OPERACION]
# ptardioFull_2019$OPERACION[!ptardioFull_2019$OPERACION %in% dfTotal$OPERACION]
####____CALCULO DE DIFERENCIAS A LA PRIMERA FECHA____####
dfCalc <- dfJoin %>% 
  dplyr::filter(ctaCont!='623') %>% 
  mutate(cosechaM = as.yearmon(fdes)) %>% 
  mutate(cosechaY = year(fdes)) %>% 
  mutate(myFirstTardio = as.yearmon(FechaFirstTardio),
         myFirstCond = as.yearmon(FechaFirstCond),
         myFirstMora = as.yearmon(FechaFirstMora),
         myFirstMoraIM = as.yearmon(FechaFirstMoraIM)) %>%
  mutate(FechaFirstCond = as.Date(FechaFirstCond)) %>% 
  mutate(FechaFirstMora = as.Date(FechaFirstMora, frac=1)) %>% 
  group_by(cosechaY) %>% 
  mutate(MONTODES = sum(MONTOUS,na.rm = T)) %>% 
  mutate(OPDES = n_distinct(OPERACION)) %>% 
  # dplyr::filter(!is.na(myfirstcond)) %>% 
  mutate(Days_to_first_cond = as.numeric(FechaFirstCond-fdes)) %>% 
  mutate(Months_to_first_cond = round((myFirstCond-cosechaM)*12)) %>% 
  mutate(Days_to_first_tardio = as.numeric(FechaFirstTardio-fdes)) %>% 
  mutate(Months_to_first_tardio = round((myFirstTardio-cosechaM)*12)) %>% 
  mutate(Days_to_first_mora = as.numeric(FechaFirstMora-fdes)) %>% 
  mutate(Days_to_first_mora_IM = as.numeric(FechaFirstMoraIM-fdes)) %>% 
  mutate(Days_to_first_mora_IM = if_else(!is.na(Days_to_first_mora) &Days_to_first_mora<Days_to_first_mora_IM,
                                         Days_to_first_mora, Days_to_first_mora_IM)) %>% 
  mutate(Months_to_first_mora = round((myFirstMora-cosechaM)*12)) %>% 
  mutate(Months_to_first_moraIM = round((myFirstMoraIM-cosechaM)*12)) %>% 
  select(-Days_to_first_mora)

summary(dfCalc)

dfCalcExp <- dfCalc %>% 
  mutate(Months_to_first_cond_bin = case_when(Months_to_first_cond<=3~'1. <3 meses',
                                              Months_to_first_cond<=6~'2. (3, 6] meses',
                                              Months_to_first_cond<=12~'3. (6, 12] meses',
                                              Months_to_first_cond<=24~'4. (12, 24] meses',
                                              Months_to_first_cond>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_tardio_bin = case_when(Months_to_first_tardio<=3~'1. <3 meses',
                                              Months_to_first_tardio<=6~'2. (3, 6] meses',
                                              Months_to_first_tardio<=12~'3. (6, 12] meses',
                                              Months_to_first_tardio<=24~'4. (12, 24] meses',
                                              Months_to_first_tardio>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_mora_bin = case_when(Months_to_first_mora<=3~'1. <3 meses',
                                              Months_to_first_mora<=6~'2. (3, 6] meses',
                                              Months_to_first_mora<=12~'3. (6, 12] meses',
                                              Months_to_first_mora<=24~'4. (12, 24] meses',
                                              Months_to_first_mora>24~'5. >24 meses',)) %>% 
  mutate(Months_to_first_moraIM_bin = case_when(Months_to_first_moraIM<=3~'1. <3 meses',
                                              Months_to_first_moraIM<=6~'2. (3, 6] meses',
                                              Months_to_first_moraIM<=12~'3. (6, 12] meses',
                                              Months_to_first_moraIM<=24~'4. (12, 24] meses',
                                              Months_to_first_moraIM>24~'5. >24 meses',)) %>% 
  mutate(PLAZODIAS_bin = case_when(floor(PLAZODIAS/365)==0 ~'0. < 1 año',
                                   floor(PLAZODIAS/365)==1 ~'1. 1 año',
                                   floor(PLAZODIAS/365)==2 ~'2. 2 años',
                                   floor(PLAZODIAS/365)==3 ~'3. 3 años',
                                   floor(PLAZODIAS/365)==4 ~'4. 4 años',
                                   floor(PLAZODIAS/365)>4 ~'5. > 4 años',)) %>% 
  mutate(monto_bin = case_when(MONTOUS<=1000 ~ '1. <= 1 M USD',
                               MONTOUS<=3000 ~ '2. <= 3 M USD',
                               MONTOUS<=5000 ~ '3. <= 5 M USD',
                               MONTOUS<=8000 ~ '4. <= 8 M USD',
                               MONTOUS<=10000 ~ '5. <= 10 M USD',
                               MONTOUS<=20000 ~ '6. <= 20 M USD',
                               MONTOUS>20000 ~ '7. > 20 M USD',)) %>% 
  mutate(across(c(monDate,FechaFirstMora, cosechaM, starts_with("myFirst")),~as.Date(.x,frac=1)))

saveRDS(dfCalcExp, "D:/!bso/firstTimes/firstTimes_Jun2023.rds")

y <- dfCalcExp %>% group_by(cosechaM)%>% mutate(OPDES2=n(),MONTODES2=sum(MONTOUS)) %>% 
  group_by(cosechaM,monto_bin) %>% 
  summarise(MontoDes=max(MONTODES2),OpsDes=max(OPDES2),
            Perc=n()/max(OPDES2),PercM=sum(MONTOUS)/max(MONTODES2)) 

dfCalcExp %>% group_by(cosechaY,monto_bin) %>% 
  summarise(Perc=n()/max(OPDES),PercM=sum(MONTOUS)/max(MONTODES)) 

#EXCLUYENDO OPERACIONES MIGRADAS DE FSL
my <- c("May2023","Jun2023","Jul2023")
fslList <- list()
for (i in 1:length(my)) {
  bdcfsl <- readRDS(paste0("D:/!bso/girCartera/rds/ec_",my[i],".rds")) %>% 
    dplyr::filter(MODULO==118 | str_detect(TIPO_OPER, "MIGR")) %>% 
    select(OPERACION, CTACLIENTE, MODULO, TIPO_OPER, fdes)
  fslList[[i]] <- bdcfsl
}
fslFull <- rbindlist(fslList) 


dfTotal_2019 <- dfTotal_2019 %>% 
  anti_join(fslFull, by=c("CTACLIENTE","OPERACION"))

write_xlsx(dfCalcExp, "D:/!bso/firstTimes/firstTimes_Jun2023_v2.xlsx")
####____EXPLORATORY ANALYSIS____####


####____PLOTS____####
dfEvol <- dfCalc %>% 
  mutate(fueCond = ifelse(is.na(Months_to_first_cond),0,1)) %>% 
  mutate(fueTardio = ifelse(is.na(Months_to_first_tardio),0,1)) %>% 
  group_by(cosechaY,Months_to_first_cond) %>% 
  summarise(cond_rel = sum(fueCond)/max(OPDES)*100) %>% 
  dplyr::filter(!is.na(Months_to_first_cond))

ggplot(dfEvol,aes(x=Months_to_first_cond, y=cond_rel,color=factor(cosechaY)))+
  geom_line(size=1.25)+
  labs(x="Meses hasta la primera condonación",y="Operaciones relativas",
       color="Año de desembolso")+
  scale_y_continuous(labels = percent)+
  theme_light()
  
dfEvol_pt <- dfCalc %>% 
  mutate(pt_bin = case_when(is.na(Months_to_first_tardio)~'0. Sin PT',
                            Months_to_first_tardio<=3~'1. 0-3 meses',
                            Months_to_first_tardio<=6~'2. 4-6 meses',
                            Months_to_first_tardio<=12~'3. 6-12 meses',
                            Months_to_first_tardio<=18~'4. 12-18 meses',
                            Months_to_first_tardio<=24~'5. 18-24 meses',
                            Months_to_first_tardio<=48~'6. 24-48 meses',
                            Months_to_first_tardio>48~'7. >48 meses',TRUE~NA)) %>% 
  group_by(cosechaY, pt_bin) %>% 
  summarise(OPS_REL = n()/max(OPDES),
            OPS_DES = n(),
            MONTO_REL = sum(MONTOUS)/max(MONTODES),
            MONTOUS = sum(MONTOUS))

dfEvol_pt %>% 
  dplyr::filter(pt_bin!='0. Sin PT') %>% 
  ggplot(aes(x = cosechaY, y=OPS_REL, color=pt_bin)) +
  geom_line(size=1.5)+
  theme_light()

dfEvol_cond <- dfCalc %>% 
  mutate(cond_bin = case_when(is.na(Months_to_first_cond)~'0. Sin Cond',
                            Months_to_first_cond<=3~'1. 0-3 meses',
                            Months_to_first_cond<=6~'2. 4-6 meses',
                            Months_to_first_cond<=12~'3. 6-12 meses',
                            Months_to_first_cond<=18~'4. 12-18 meses',
                            Months_to_first_cond<=24~'5. 18-24 meses',
                            Months_to_first_cond<=48~'6. 24-48 meses',
                            Months_to_first_cond>48~'7. >48 meses',TRUE~NA)) %>% 
  group_by(cosechaY, cond_bin) %>% 
  summarise(OPS_REL = n()/max(OPDES),
            MONTO_REL = sum(MONTOUS)/max(MONTODES))

dfEvol_cond %>% 
  dplyr::filter(cond_bin!='0. Sin Cond') %>% 
  ggplot(aes(x = cosechaY, y=OPS_REL, color=cond_bin)) +
  geom_line(size=1.5)+
  theme_light()


dfEvol_MIM <- dfCalc %>% 
  mutate(cond_bin = case_when(is.na(Months_to_first_moraIM)~'0. Sin moraIM',
                              Months_to_first_moraIM<=3~'1. 0-3 meses',
                              Months_to_first_moraIM<=6~'2. 4-6 meses',
                              Months_to_first_moraIM<=12~'3. 6-12 meses',
                              Months_to_first_moraIM<=18~'4. 13-18 meses',
                              Months_to_first_moraIM<=24~'5. 19-24 meses',
                              Months_to_first_moraIM<=48~'6. 25-48 meses',
                              Months_to_first_moraIM>48~'7. >48 meses',TRUE~NA)) %>% 
  group_by(cosechaM, cond_bin) %>% 
  summarise(OPS_REL = n()/max(OPDES),
            OPS_DES = n(),
            OPSTOT = max(OPDES),
            MONTO_REL = sum(MONTOUS)/max(MONTODES),
            MONTOUS = sum(MONTOUS),
            MONTOT = max(MONTODES))

dfEvol_MIM %>% 
  dplyr::filter(cond_bin!='0. Sin moraIM') %>% 
  dplyr::filter(cosechaM<="Dic. 2022") %>% 
  ggplot(aes(x = cosechaM, y=OPS_REL, color=cond_bin)) +
  geom_line(size=1.5) +
  theme_light()
