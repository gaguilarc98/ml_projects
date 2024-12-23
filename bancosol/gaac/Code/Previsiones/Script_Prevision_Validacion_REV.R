#-----------------------------------------#
#         Previsiones Julio 2020          #
#-----------------------------------------#

#-----------------------------------------
# cargamos las librerias
library(dplyr)
library(readxl)
library(ggplot2)
library(stringr)
library(psych)
library(readr)

#install.packages(c("readxl","ggplot2",
#                   "stringr","psych",
#                   "readr","dplyr"), dependencies = T)

#-----------------------------------------
# cargamos las bases de datos
#bd_car_01 <- read.csv("F:/Prevision_Validacion/BaseCartera_20211129.txt",header=T, sep="|")
#bd_car_01 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211229.txt",header=T, sep="|")

bd_car_01 <- read.csv("F:/Prevision_Validacion/BaseCarteraEne2022.txt",header=T, sep="|")

# seleccionamos la base de castigos
bd_car_01_Sel <- bd_car_01 %>% 
  filter(MODULO !=131) %>% 
  filter(MODULO !=29) %>% 
  filter(ESTADO =="CASTIGADA") %>% 
  select(CTACLIENTE, OPERACION, SALDO, MONEDA)

dim(bd_car_01_Sel)

#guardamos las operaciones casitgadas
setwd("F:/Prevision_Validacion")
write.csv(bd_car_01_Sel, '20220131_BD_Castigadas.csv')

# verificamos los indicadores

# cartera bruta
bd_car_01_Sel <- bd_car_01 %>% 
  filter(MODULO !=131) %>% 
  filter(MODULO !=29) %>% 
  filter(ESTADO !="CASTIGADA") %>% 
  mutate(SALDO_USD = case_when(MONEDA == 0 ~ SALDO / 6.86,
                               MONEDA != 0 ~ SALDO ))
  

nrow(bd_car_01_Sel)
sum(bd_car_01_Sel$SALDO_USD)/1000

# cartera mora
bd_car_01_Sel <- bd_car_01 %>% 
  filter(MODULO !=131) %>% 
  filter(MODULO !=29) %>% 
  filter(ESTADO !="CASTIGADA") %>% 
  filter(DIASMORA > 1) %>% 
  mutate(SALDO_USD = case_when(MONEDA == 0 ~ SALDO / 6.86,
                               MONEDA != 0 ~ SALDO ))


nrow(bd_car_01_Sel)
sum(bd_car_01_Sel$SALDO_USD)/1000

# cartera castigada
bd_car_01_Sel <- bd_car_01 %>% 
  filter(MODULO !=131) %>% 
  filter(MODULO !=29) %>% 
  filter(ESTADO =="CASTIGADA") %>% 
  mutate(SALDO_USD = case_when(MONEDA == 0 ~ SALDO / 6.86,
                               MONEDA != 0 ~ SALDO ))


nrow(bd_car_01_Sel)
sum(bd_car_01_Sel$SALDO_USD)/1000


#----------------------------------

library(readxl)

bd_car_01_Sel <- bd_car_01 %>% 
  filter(MODULO !=131) %>% 
  filter(MODULO !=29) %>% 
  filter(ESTADO !="CASTIGADA") %>% 
  mutate(SALDO_USD = case_when(MONEDA == 0 ~ SALDO / 6.86,
                               MONEDA != 0 ~ SALDO ))


bd_car_01_Mor <- bd_car_01 %>% 
  filter(MODULO !=131) %>% 
  filter(MODULO !=29) %>% 
  filter(ESTADO !="CASTIGADA") %>% 
  filter(DIASMORA > 1) %>% 
  mutate(SALDO_USD = case_when(MONEDA == 0 ~ SALDO / 6.86,
                               MONEDA != 0 ~ SALDO ))

sum(bd_car_01_Sel$SALDO_USD)

sum(bd_car_01_Sel$CLIENTES)


sum(bd_car_01_Mor$SALDO_USD)/sum(bd_car_01_Sel$SALDO_USD)*100
#Observaciones_Prevision <- read_excel("F:/Prevision_Validacion/Observaciones_Prevision.xlsx", 
#                                      sheet = "Hoja2")

# fusionamos solo con el campo de sub operacion de la base de cartera
#bd_car_01_sel <-  select(bd_car_01, CTACLIENTE, OPERACION, SUBOPERACION)

#dim(Observaciones_Prevision)
#bd_cons = left_join(Observaciones_Prevision,bd_car_01_sel, by = c("CTACLIENTE", "OPERACION") )
#dim(bd_cons)

#-----------------------------------------
# consideraciones
# la información a al corte del 29 de noviembre de 2021
#-----------------------------------------



bd_car_01 <- read.csv("F:/Prevision_Validacion/BaseCarteraMar2022.txt",header=T, sep="|")



bd_car_01 <- read.csv("F:/Prevision_Validacion/BaseCarteraMay2022.txt",header=T, sep="|")

#-----------------------------------------
#-----------------------------------------
# seleccionamos operaciones validas
bd_car_01 <- bd_car_01 %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  filter(MODULO != 131) 

dim(bd_car_01)
# 326753 total de cartera en marzo sin castigada sin 131 y 29

# elimnamos los espacios vacios de las variables
table(bd_car_01$TAM_ACTIV)
bd_car_01$TAM_ACTIV <-gsub(" ", "", bd_car_01$TAM_ACTIV)
table(bd_car_01$TAM_ACTIV)

#-----------------------------------------
#-----------------------------------------
# generamos el campo de sector productivo
N_Campos=read.csv("F:/Prevision_Validacion/Nuevos_Campos_Caedec.csv",header=T, sep=";")

N_Campos_2=select(N_Campos,CAEDEC_DEST,Clasificacion_otros_CAEDEC=Clasificacion.Otros,
                  Sector_Def_Banco_CAEDEC=Sector.definido.del.Banco,
                  Cat_CAEDEC)

bd_car_01=left_join(bd_car_01,N_Campos_2, by="CAEDEC_DEST")




#-----------------------------------------
#-----------------------------------------
# generamos los campos de Produccion Intelectual, turismo e hibridos
base_final_1=filter(bd_car_01,CAEDEC_DEST==55101|CAEDEC_DEST==55102|CAEDEC_DEST==55103|
                      CAEDEC_DEST==55201|CAEDEC_DEST==60100|CAEDEC_DEST==60212|
                      CAEDEC_DEST==60222|CAEDEC_DEST==61200|CAEDEC_DEST==62101|
                      CAEDEC_DEST==60221|CAEDEC_DEST==71110|CAEDEC_DEST==71120|
                      CAEDEC_DEST==63041|CAEDEC_DEST==63042|CAEDEC_DEST==92320|
                      CAEDEC_DEST==92330)

# capital de inversión, adicionar
# OC1 


Turismo_Prop_Intelectual=rep("01. Turismo",nrow(base_final_1))

base_final_1=cbind(base_final_1,Turismo_Prop_Intelectual)

base_final_2=filter(bd_car_01,CAEDEC_DEST==72200|CAEDEC_DEST==73101|CAEDEC_DEST==73102|
                      CAEDEC_DEST==73200|CAEDEC_DEST==92110|CAEDEC_DEST==92141)

Turismo_Prop_Intelectual=rep("02. Produccion Intelectual",nrow(base_final_2))

base_final_2=cbind(base_final_2,Turismo_Prop_Intelectual)


base_final_3=filter(bd_car_01,CAEDEC_DEST==31600|CAEDEC_DEST==31700|CAEDEC_DEST==51508|
                      CAEDEC_DEST==52592|CAEDEC_DEST==34400|CAEDEC_DEST==34500|CAEDEC_DEST==50103)

Turismo_Prop_Intelectual=rep("03. Electrico e Hibridos",nrow(base_final_3))

base_final_3=cbind(base_final_3,Turismo_Prop_Intelectual)



base_final=rbind(base_final_1,base_final_2, base_final_3)

# seleccionamos las variables
base_final=select(base_final,CTACLIENTE,OPERACION,Turismo_Prop_Intelectual)

# unimos con la base principal
bd_car_01=left_join(bd_car_01,base_final,by=c("CTACLIENTE","OPERACION"))



#-----------------------------------------
#-----------------------------------------
# estrateficamos la cartera
Estr_Cartera=rep(0,nrow(bd_car_01))
bandera1=0
bandera2=0

bd_car_01$Cat_CAEDEC=as.character(bd_car_01$Cat_CAEDEC)
bd_car_01$Cat_CAEDEC[is.na(bd_car_01$Cat_CAEDEC)] <- 0

table(bd_car_01$TAM_ACTIV)
table(is.na(bd_car_01$CAEDEC_DEST))

# que hace en caso de encontrar un dato perdido
x <- filter(bd_car_01, is.na(bd_car_01$CAEDEC_DEST)==T)
# CTACLIENTE 1818804 OPERACION 3056768
# solo para esta oportunidad se excluye esa operacion
dim(bd_car_01)
bd_car_01 <- filter(bd_car_01, is.na(bd_car_01$CAEDEC_DEST)!=T)
dim(bd_car_01)

for(i in 1:nrow(bd_car_01)){ 
  bandera1=0
  bandera2=0
  if(is.na(bd_car_01$Cat_CAEDEC[i])){
    bd_car_01$Cat_CAEDEC[i]="0"
  }
  if(bd_car_01$TIPO_CREDITO[i]=="H0"|bd_car_01$TIPO_CREDITO[i]=="H1"|
     bd_car_01$TIPO_CREDITO[i]=="H2"){
    Estr_Cartera[i]="6. VIVIENDA NO CONTROLADA"
  }
  
  if(bd_car_01$TIPO_CREDITO[i]=="H3" |bd_car_01$TIPO_CREDITO[i]=="H4"){ 
    if( bd_car_01$MONEDA[i]==0){
      Estr_Cartera[i]="3. VIVIENDA CONTROLADA"  
    }  
  }
  
  if(bd_car_01$TIPO_CREDITO[i]=="N0"|bd_car_01$TIPO_CREDITO[i]=="N1"|
     bd_car_01$TIPO_CREDITO[i]=="N2"){
    Estr_Cartera[i]="9. CONSUMO"
  }
  
  if(bd_car_01$TIPO_CREDITO[i]=="M0" |bd_car_01$TIPO_CREDITO[i]=="M1"|
     bd_car_01$TIPO_CREDITO[i]=="M2" |bd_car_01$TIPO_CREDITO[i]=="M7"|
     bd_car_01$TIPO_CREDITO[i]=="M8" |bd_car_01$TIPO_CREDITO[i]=="P1"|
     bd_car_01$TIPO_CREDITO[i]=="P8" |bd_car_01$TIPO_CREDITO[i]=="P9"|
     bd_car_01$TIPO_CREDITO[i]=="P2" |bd_car_01$TIPO_CREDITO[i]=="P3"){  
    
    if(bd_car_01$Cat_CAEDEC[i]=="I"|bd_car_01$Cat_CAEDEC[i]=="J"|
       bd_car_01$Cat_CAEDEC[i]=="K"|bd_car_01$Cat_CAEDEC[i]=="L"|
       bd_car_01$Cat_CAEDEC[i]=="M"|bd_car_01$Cat_CAEDEC[i]=="N"|
       bd_car_01$Cat_CAEDEC[i]=="O"|bd_car_01$Cat_CAEDEC[i]=="P"|
       bd_car_01$Cat_CAEDEC[i]=="Q"|bd_car_01$Cat_CAEDEC[i]=="Z"){
      Estr_Cartera[i]="8. SERVICIOS"
      
    }
    
    if( bd_car_01$Cat_CAEDEC[i]=="H"){
      Estr_Cartera[i]="7. COMERCIO"  
    }
    
    if( bd_car_01$Cat_CAEDEC[i]=="A"|bd_car_01$Cat_CAEDEC[i]=="B"){  
      if(bd_car_01$TIPOTASA[i]=="F" & bd_car_01$MONEDA[i]==0){
        if(bd_car_01$TAM_ACTIV[i]=="Micro" & bd_car_01$TASAACT[i]<=11.5){
          Estr_Cartera[i]="1. PRODUCCIÓN AGROPECUARIA CONTROLADA"
          bandera1=1
        }
        if(bd_car_01$TAM_ACTIV[i]=="Pequeña" & bd_car_01$TASAACT[i]<=7){#if(bd_car_01$TAM_ACTIV[i]=="PequeÃ±a" & bd_car_01$TASAACT[i]<=7){
          Estr_Cartera[i]="1. PRODUCCIÓN AGROPECUARIA CONTROLADA"
          bandera1=1
        }
        if(bd_car_01$TAM_ACTIV[i]=="DMediana" & bd_car_01$TASAACT[i]<=6){
          Estr_Cartera[i]="1. PRODUCCIÓN AGROPECUARIA CONTROLADA"
          bandera1=1
        }
        if(bd_car_01$TAM_ACTIV[i]=="Grande" & bd_car_01$TASAACT[i]<=6){
          Estr_Cartera[i]="1. PRODUCCIÓN AGROPECUARIA CONTROLADA"
          bandera1=1
        }
      }
      if(bandera1==0){
        Estr_Cartera[i]="4. PRODUCCIÓN AGROPECUARIA NO CONTROLADA"  
      }
    }
    if(bd_car_01$Cat_CAEDEC[i]=="C"|bd_car_01$Cat_CAEDEC[i]=="D"|
       bd_car_01$Cat_CAEDEC[i]=="E"|bd_car_01$Cat_CAEDEC[i]=="F"|
       bd_car_01$Cat_CAEDEC[i]=="G"){ 
      if(bd_car_01$TIPOTASA[i]=="F" & bd_car_01$MONEDA[i]==0){
        if(bd_car_01$TAM_ACTIV[i]=="Micro" & bd_car_01$TASAACT[i]<=11.5){
          Estr_Cartera[i]="2. OTRA PRODUCCIÓN CONTROLADA"
          bandera2=1
        }
        if(bd_car_01$TAM_ACTIV[i]=="Pequeña" & bd_car_01$TASAACT[i]<=7){#if(bd_car_01$TAM_ACTIV[i]=="PequeÃ±a" & bd_car_01$TASAACT[i]<=7){
          Estr_Cartera[i]="2. OTRA PRODUCCIÓN CONTROLADA"
          bandera2=1
        }
        if(bd_car_01$TAM_ACTIV[i]=="DMediana" & bd_car_01$TASAACT[i]<=6){
          Estr_Cartera[i]="2. OTRA PRODUCCIÓN CONTROLADA"
          bandera2=1
        }
        if(bd_car_01$TAM_ACTIV[i]=="Grande" & bd_car_01$TASAACT[i]<=6){
          Estr_Cartera[i]="2. OTRA PRODUCCIÓN CONTROLADA"
          bandera2=1
        }
      } 
      if(bandera2==0){
        Estr_Cartera[i]="5. OTRA PRODUCCIÓN NO CONTROLADA"
      }
    }
    if(bd_car_01$DESC_OBJCRED[i]=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
       bd_car_01$DESC_OBJCRED[i]=="CAPITAL DE INVERSION ACT. SECUNDARIAS"){
      if(bd_car_01$CAEDEC_DEST[i]==55101|bd_car_01$CAEDEC_DEST[i]==55102|
         bd_car_01$CAEDEC_DEST[i]==55103|bd_car_01$CAEDEC_DEST[i]==55201|
         bd_car_01$CAEDEC_DEST[i]==60100|bd_car_01$CAEDEC_DEST[i]==60212|
         bd_car_01$CAEDEC_DEST[i]==60222|bd_car_01$CAEDEC_DEST[i]==61200|
         bd_car_01$CAEDEC_DEST[i]==62101|bd_car_01$CAEDEC_DEST[i]==60221|
         bd_car_01$CAEDEC_DEST[i]==71110|bd_car_01$CAEDEC_DEST[i]==71120|
         bd_car_01$CAEDEC_DEST[i]==63041|bd_car_01$CAEDEC_DEST[i]==63042|
         bd_car_01$CAEDEC_DEST[i]==92320|bd_car_01$CAEDEC_DEST[i]==92330){
        if(bd_car_01$TIPOTASA[i]=="F" & bd_car_01$MONEDA[i]==0){
          if(bd_car_01$TAM_ACTIV[i]=="Micro" & bd_car_01$TASAACT[i]<=11.5){
            Estr_Cartera[i]="2. OTRA PRODUCCIÓN CONTROLADA"
          }
          if(bd_car_01$TAM_ACTIV[i]=="Pequeña" & bd_car_01$TASAACT[i]<=7){#if(bd_car_01$TAM_ACTIV[i]=="PequeÃ±a" & bd_car_01$TASAACT[i]<=7){
            Estr_Cartera[i]="2. OTRA PRODUCCIÓN CONTROLADA"
          }
          if(bd_car_01$TAM_ACTIV[i]=="DMediana" & bd_car_01$TASAACT[i]<=6){
            Estr_Cartera[i]="2. OTRA PRODUCCIÓN CONTROLADA"
          }
          if(bd_car_01$TAM_ACTIV[i]=="Grande" & bd_car_01$TASAACT[i]<=6){
            Estr_Cartera[i]="2. OTRA PRODUCCIÓN CONTROLADA"
          }
        }
      }
    }
    if(bd_car_01$DESC_OBJCRED[i]=="CAPITAL DE INVERSION ACT. PRINCIPAL"|
       bd_car_01$DESC_OBJCRED[i]=="CAPITAL DE INVERSION ACT. SECUNDARIAS"|
       bd_car_01$DESC_OBJCRED[i]=="CAPITAL DE OPERACION ACT. PRINCIPAL"|
       bd_car_01$DESC_OBJCRED[i]=="CAPITAL DE OPERACION ACT. SECUNDARIAS"){
      if(bd_car_01$CAEDEC_DEST[i]==72200|bd_car_01$CAEDEC_DEST[i]==73101|
         bd_car_01$CAEDEC_DEST[i]==73102|bd_car_01$CAEDEC_DEST[i]==73200|
         bd_car_01$CAEDEC_DEST[i]==92110|bd_car_01$CAEDEC_DEST[i]==92141){
        if(bd_car_01$TIPOTASA[i]=="F" & bd_car_01$MONEDA[i]==0){
          if(bd_car_01$TAM_ACTIV[i]=="Micro" & bd_car_01$TASAACT[i]<=11.5){
            Estr_Cartera[i]="2. OTRA PRODUCCIÓN CONTROLADA"
          }
          if(bd_car_01$TAM_ACTIV[i]=="Pequeña" & bd_car_01$TASAACT[i]<=7){#if(bd_car_01$TAM_ACTIV[i]=="PequeÃ±a" & bd_car_01$TASAACT[i]<=7){
            Estr_Cartera[i]="2. OTRA PRODUCCIÓN CONTROLADA"
          }
          if(bd_car_01$TAM_ACTIV[i]=="DMediana" & bd_car_01$TASAACT[i]<=6){
            Estr_Cartera[i]="2. OTRA PRODUCCIÓN CONTROLADA"
          }
          if(bd_car_01$TAM_ACTIV[i]=="Grande" & bd_car_01$TASAACT[i]<=6){
            Estr_Cartera[i]="2. OTRA PRODUCCIÓN CONTROLADA"
          }
        }
      }
    }
    
  }
  if(bd_car_01$CAEDEC_DEST[i]==31600|bd_car_01$CAEDEC_DEST[i]==31700|
     bd_car_01$CAEDEC_DEST[i]==51508|bd_car_01$CAEDEC_DEST[i]==52592|
     bd_car_01$CAEDEC_DEST[i]==34400|bd_car_01$CAEDEC_DEST[i]==34500|
     bd_car_01$CAEDEC_DEST[i]==50103){
        Estr_Cartera[i]="2. OTRA PRODUCCIÓN CONTROLADA"
      
    }
} 

bd_car_01=cbind(bd_car_01,Estr_Cartera)

dim(bd_car_01)
#-----------------------------------------
# generamos la variable de cartera controlada
bd_car_01_select_1 = filter(bd_car_01, Estr_Cartera == "1. PRODUCCIÓN AGROPECUARIA CONTROLADA"|
                         Estr_Cartera =="2. OTRA PRODUCCIÓN CONTROLADA"|
                         Estr_Cartera =="3. VIVIENDA CONTROLADA")

Cart_Controlada_NoControlada=rep("01. CARTERA CONTROLADA",nrow(bd_car_01_select_1))
bd_car_01_final_1=cbind(bd_car_01_select_1,Cart_Controlada_NoControlada)


bd_car_01_select_2 = filter(bd_car_01, Estr_Cartera != "1. PRODUCCIÓN AGROPECUARIA CONTROLADA"&
                         Estr_Cartera !="2. OTRA PRODUCCIÓN CONTROLADA"&
                         Estr_Cartera !="3. VIVIENDA CONTROLADA")

Cart_Controlada_NoControlada=rep("02. CARTERA NO CONTROLADA",nrow(bd_car_01_select_2))
bd_car_01_final_2=cbind(bd_car_01_select_2,Cart_Controlada_NoControlada)

bd_car_01=rbind(bd_car_01_final_1,bd_car_01_final_2)


#-----------------------------------------
# generamos la variable para sector productivo
bd_car_01_select=filter(bd_car_01,Estr_Cartera=="1. PRODUCCIÓN AGROPECUARIA CONTROLADA"|
                     Estr_Cartera=="2. OTRA PRODUCCIÓN CONTROLADA"|
                     Estr_Cartera=="4. PRODUCCIÓN AGROPECUARIA NO CONTROLADA"|
                     Estr_Cartera=="5. OTRA PRODUCCIÓN NO CONTROLADA")

bd_car_01_select_2=filter(bd_car_01,Estr_Cartera!="1. PRODUCCIÓN AGROPECUARIA CONTROLADA"&
                       Estr_Cartera!="2. OTRA PRODUCCIÓN CONTROLADA"&
                       Estr_Cartera!="4. PRODUCCIÓN AGROPECUARIA NO CONTROLADA"&
                       Estr_Cartera!="5. OTRA PRODUCCIÓN NO CONTROLADA")

Sector_productivo=rep("SI",nrow(bd_car_01_select))
bd_car_01_final_1=cbind(bd_car_01_select,Sector_productivo)

Sector_productivo=rep("NO",nrow(bd_car_01_select_2))
bd_car_01_final_2=cbind(bd_car_01_select_2,Sector_productivo)


bd_car_01 <- rbind(bd_car_01_final_1,bd_car_01_final_2)

names(bd_car_01)

table(bd_car_01$DESC_OBJCRED)
table(bd_car_01$OBJETO_CRED)

table(bd_car_01$Turismo_Prop_Intelectual,bd_car_01$Sector_productivo)


#-----------------------------------------
#-----------------------------------------
# modificamos los sectores de turismo, produccion intelectual y actividades economicas electricas e hibridas como productivo

#bd_car_01 = bd_car_01 %>% 
#  mutate(Sector_productivo = case_when(Turismo_Prop_Intelectual == "01. Turismo" &
#                                         OBJETO_CRED == 1 ~ "SI",|# capital de inversion
#                                         Turismo_Prop_Intelectual == "01. Turismo" &
#                                         OBJETO_CRED == 2 ~ "SI",|# capital de inversion
#                                         Turismo_Prop_Intelectual == "01. Turismo" &
#                                         OBJETO_CRED == 3 ~ "SI",|# capital de inversion
#                                         Turismo_Prop_Intelectual == "01. Turismo" &
#                                         OBJETO_CRED == 4 ~ "SI",|# capital de inversion
#                                        
#                                       Turismo_Prop_Intelectual == "02. Produccion Intelectual" |
#                                         Turismo_Prop_Intelectual == "03. Electrico e Hibridos" ~  "SI",
#                                       
#                                       Turismo_Prop_Intelectual != "01. Turismo" &
#                                         Turismo_Prop_Intelectual != "02. Produccion Intelectual" &
#                                         Turismo_Prop_Intelectual != "03. Electrico e Hibridos" ~  Sector_productivo,
#                                       
#                                       is.na(Turismo_Prop_Intelectual) == T ~  Sector_productivo
#                                       ),
#         Tipo_Cartera = case_when(MODULO == 29 ~ 1,
#                                  MODULO != 29 ~ 0)
#         )


bd_car_01 = bd_car_01 %>% 
    mutate(Tipo_Cartera = case_when(MODULO == 29 ~ 1,
                                    MODULO != 29 ~ 0)
     )




names(bd_car_01)
#-----------------------------------------
# seleccionamos las variables
bd_car_01 <- bd_car_01 %>% 
  select(CTACLIENTE,
         OPERACION,
         SUBOPERACION_CAR = SUBOPERACION,
         MODULO_CAR = MODULO,
         MONEDA_CAR = MONEDA,
         MONTO_CAR = MONTO,
         SALDO_CAR = SALDO,
         ESTADO_CAR = ESTADO,
         FDESEMBOLSO_CAR = FDESEMBOLSO,
         RUBRO_CAR = RUBRO,
         DIASMORA_CAR = DIASMORA,
         CALIFICACION_CAR = CALIFICACION,
         PREVCONST_CAR = PREVCONST,
         TIPO_CREDITO_CAR = TIPO_CREDITO,
         
         REFINANCIAMIENTO_GENUINO,
         
         CAEDEC_DEST,
         Cat_CAEDEC,
         Turismo_Prop_Intelectual,
         DESC_OBJCRED,
         OBJETO_CRED,
         TIPOTASA,
         TASAACT,
         TAM_ACTIV,
         INDICE_TAM,
         PRODUCTIVO_CAR = PRODUCTIVO,
         SECTOR_CARTERA,
         Estr_Cartera,
         Sector_productivo,
         Tipo_Cartera
         )

#-----------------------------------------
# añadimos los tiempos de la fecha de desembolso
if(stringr::str_length(bd_car_01$FDESEMBOLSO_CAR[1])==8){
  bd_car_01$FDESEMBOLSO2 = as.POSIXct(strptime(bd_car_01$FDESEMBOLSO_CAR, format = "%d/%m/%y"))  
}else{
  bd_car_01$FDESEMBOLSO2 = as.POSIXct(strptime(bd_car_01$FDESEMBOLSO_CAR, format = "%d/%m/%Y"))
}

bd_car_01$dia=format( bd_car_01$FDESEMBOLSO2, format="%d" )
bd_car_01$mes=format( bd_car_01$FDESEMBOLSO2, format="%m" )
bd_car_01$anio=format( bd_car_01$FDESEMBOLSO2, format="%Y" )

bd_car_01$dia = as.numeric(bd_car_01$dia)
bd_car_01$mes = as.numeric(bd_car_01$mes)
bd_car_01$anio = as.numeric(bd_car_01$anio) 

#-----------------------------------------
# tratamos la información de Rubro
bd_car_01=dplyr::mutate(bd_car_01,RUBRO_3=RUBRO_CAR%/%10000000000)

#-----------------------------------------
# añadimos el campo de cambio de calificación
library(readxl)
#Cambio_Calificacion <- read_excel("F:/Prevision_Validacion/PrevisionesMtoComp.xlsx", 
#                                 sheet = "Hoja1")

#names(Cambio_Calificacion)

#dim(Cambio_Calificacion)

#Cambio_Calificacion <- Cambio_Calificacion %>% 
#  select(CTACLIENTE) %>% 
#  distinct()

#dim(Cambio_Calificacion)

#Cambio_Calificacion$Cambio_Calificacion <- rep("SI", nrow(Cambio_Calificacion))



#fusionamos con la base principal
#bd_car_01 <-left_join(bd_car_01, Cambio_Calificacion, by=c("CTACLIENTE"))

#bd_car_01 <- bd_car_01 %>% 
#  mutate(Cambio_Calificacion = case_when(is.na(Cambio_Calificacion) == T ~ "NO",
#                                         is.na(Cambio_Calificacion) != T ~ Cambio_Calificacion
#    
#  ))


#table(bd_car_01$Cambio_Calificacion)
#table(is.na(bd_car_01$Cambio_Calificacion))

# añadimos el cambio de calificación realizado
#Cambio_Calificacion_01 <- read_csv("20211129_BD_Cambio_Calificacion.csv")
#Cambio_Calificacion_01 <- read_csv("20211227_BD_Cambio_Calificacion.csv")
#Cambio_Calificacion_01 <- read_csv("20211229_BD_Cambio_Calificacion.csv")

#Cambio_Calificacion_01 <- read_csv2("F:/Prevision_Validacion/CamposAdicionales_202202.csv")
Cambio_Calificacion_01 <- read.csv("F:/Prevision_Validacion/CamposAdicionales_202205.csv", header=T, sep=";") # sep=";")

names(Cambio_Calificacion_01)

Cambio_Calificacion_01 <- Cambio_Calificacion_01 %>% 
  select(CTACLIENTE, OPERACION, Calificacion_Mayor = CambioCalificacion)

table(Cambio_Calificacion_01$Calificacion_Mayor)
table(is.na(Cambio_Calificacion_01$Calificacion_Mayor))

# asignamos a la base
bd_car_01 <-left_join(bd_car_01, Cambio_Calificacion_01, by=c("CTACLIENTE", "OPERACION"))

dim(bd_car_01)
dim(Cambio_Calificacion_01)

names(bd_car_01)

#x <- filter(bd_car_01, is.na(bd_car_01$Calificacion_Mayor)==T)

#setwd("F:/Prevision_Validacion")
#write.csv(x, '20220331_BD_Observados_Calificacion.csv')
#View(x)


table(bd_car_01$Calificacion_Mayor)
table(is.na(bd_car_01$Calificacion_Mayor))

dim(bd_car_01)
# solo para el mes de marzo los datos perdidos lo registramos con 0 y modificamos los que tienen cambio de calificaciones por uno

bd_car_01_01 <- filter(bd_car_01, is.na(bd_car_01$Calificacion_Mayor)==T)
bd_car_01_01$Calificacion_Mayor = rep(0, nrow(bd_car_01_01))

bd_car_01_02 <- filter(bd_car_01, is.na(bd_car_01$Calificacion_Mayor)==F)

dim(bd_car_01)

bd_car_01 = rbind(bd_car_01_01, bd_car_01_02)
dim(bd_car_01)

class(bd_car_01$Calificacion_Mayor)

bd_car_01 <- bd_car_01 %>% 
  mutate(Calificacion_Mayor = case_when(
    
    OPERACION != 3502637    &
      OPERACION != 3541845    &
      OPERACION != 3496164    &
      OPERACION != 3497938    &
      OPERACION != 3531110    &
      OPERACION != 3570589    &
      OPERACION != 3586764    &
      OPERACION != 3568942    &
      OPERACION != 3500156 ~ Calificacion_Mayor,
    
    OPERACION == 3502637 ~ 1,
    OPERACION == 3541845 ~ 1,
    OPERACION == 3496164 ~ 1,
    OPERACION == 3497938 ~ 1,
    OPERACION == 3531110 ~ 1,
    OPERACION == 3570589 ~ 1,
    OPERACION == 3586764 ~ 1,
    OPERACION == 3568942 ~ 1,
    OPERACION == 3500156 ~ 1
    
    
    
  ))



#bd_car_01 <- bd_car_01 %>% 
#  mutate(Cambio_Calificacion = case_when(is.na(Cambio_Calificacion) == T ~ "NO",
#                                         is.na(Cambio_Calificacion) != T ~ Cambio_Calificacion
#    
#  ))

table(bd_car_01$Calificacion_Mayor)
table(is.na(bd_car_01$Calificacion_Mayor))

names(bd_car_01)
dim(bd_car_01)
#-----------------------------------------
#asignamos el monto computable
#Monto_Computable <- read_excel("F:/Prevision_Validacion/PrevisionesMtoComp.xlsx", 
#                                  sheet = "Hoja2")

#Monto_Computable <- read_excel("F:/Prevision_Validacion/Base Previsiones Clasificacion.xlsx", 
#                               sheet = "Hoja2")

#Monto_Computable <- read_excel("F:/Prevision_Validacion/Base Previsiones271221.xlsx", 
#                               sheet = "Hoja2")

#Monto_Computable <- read_excel("F:/Prevision_Validacion/Base Previsiones281221_Final.xlsx", 
#                               sheet = "Base Completa")

#Monto_Computable <- read_excel("F:/Prevision_Validacion/Base Previsiones291221.xlsx", 
#                               sheet = "Hoja3")

Monto_Computable <- read_excel("F:/Prevision_Validacion/Reporte_Verificar_Previsiones_202205.xlsx", 
                               sheet = "Hoja1")

names(Monto_Computable)

dim(Monto_Computable)
dim(bd_car_01)

#seleccionamos los campos necesarios
Monto_Computable <- select(Monto_Computable, 
                            CTACLIENTE = "01_CTACLIENTE",
                            OPERACION = "02_OPERACION",
                            Monto_Computable = "12_Monto_Computable",
                            Prev_Const_TI_Usd = "12_Prev_Const_$us",
                           Saldo_TI_Sus = "07_Saldo_Sus"
                            )


#Monto_Computable <- Monto_Computable[1:29]

bd_car_01 = left_join(bd_car_01,Monto_Computable, by=c("CTACLIENTE","OPERACION") )

dim(bd_car_01)
names(bd_car_01)


table(bd_car_01$REFINANCIAMIENTO_GENUINO)
table(is.na(bd_car_01$REFINANCIAMIENTO_GENUINO))

# verificamos si la operación refinanciada es la misma de su operación

#consideraciones
# si la operación actual es igual a la operación refinanciada no se considera como nuevo

bd_car_01 <- bd_car_01 %>% 
  mutate(nuevo_refis = case_when(
    REFINANCIAMIENTO_GENUINO != "-" &
      SUBOPERACION_CAR > 90 ~ 1 ,
    REFINANCIAMIENTO_GENUINO == "-" ~ 0,
    is.na(REFINANCIAMIENTO_GENUINO) == T ~ 0
  )
  ) %>% 
  mutate(nuevo_refis = case_when(
    is.na(nuevo_refis) == T ~ 0,
    is.na(nuevo_refis) != T ~ nuevo_refis
  )
  )


table(bd_car_01$REFINANCIAMIENTO_GENUINO,
      bd_car_01$nuevo_refis)

table(bd_car_01$CALIFICACION_CAR,bd_car_01$nuevo_refis)
#-----------------------------------------
#-----------------------------------------
# Realizamos el calculo de la previsión
bd_car_01$Por_Pre=rep(0,nrow(bd_car_01))
bd_car_01$Nuevo_Excepcion=rep("NO",nrow(bd_car_01))


attach(bd_car_01)
i=74

# microcredito y pyme aplicar los caedec del sector turismo y producccion intelectual


for (i in 1:nrow(bd_car_01)){
  if(is.na(bd_car_01$MONEDA_CAR[i])==F){
    if(bd_car_01$MONEDA_CAR[i]==0){
      if(bd_car_01$TIPO_CREDITO_CAR[i]=="M0"|bd_car_01$TIPO_CREDITO_CAR[i]=="M1"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="M2"|bd_car_01$TIPO_CREDITO_CAR[i]=="M3"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="M4"|bd_car_01$TIPO_CREDITO_CAR[i]=="M7"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="M8"|bd_car_01$TIPO_CREDITO_CAR[i]=="M9"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="P1"|bd_car_01$TIPO_CREDITO_CAR[i]=="P2"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="P3"|bd_car_01$TIPO_CREDITO_CAR[i]=="P4"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="P8"|bd_car_01$TIPO_CREDITO_CAR[i]=="P9"){
        if(bd_car_01$Sector_productivo[i] == "SI"){#if(bd_car_01$Sector_productivo[i] == "SI"){
          if(bd_car_01$CALIFICACION_CAR[i]== "A"){
            bd_car_01$Por_Pre[i]=0
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "B"){
            bd_car_01$Por_Pre[i]=0.025
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "C"){
            bd_car_01$Por_Pre[i]=0.2
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "D"){
            bd_car_01$Por_Pre[i]=0.5
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "E"){
            bd_car_01$Por_Pre[i]=0.8
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "F"){
            bd_car_01$Por_Pre[i]=1
          }
        }else{
          if(bd_car_01$Sector_productivo[i] == "NO"){#if(bd_car_01$Sector_productivo[i] == "NO"){
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre[i]=0.0025
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre[i]=0.05
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre[i]=1
            }
          }
        }
      }
      
      if(bd_car_01$TIPO_CREDITO_CAR[i]=="H0"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="H3"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="H4"){
        
        if(bd_car_01$CALIFICACION_CAR[i]== "A"){
          bd_car_01$Por_Pre[i]=0.0025
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "B"){
          bd_car_01$Por_Pre[i]=0.05
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "C"){
          bd_car_01$Por_Pre[i]=0.2
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "D"){
          bd_car_01$Por_Pre[i]=0.5
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "E"){
          bd_car_01$Por_Pre[i]=0.8
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "F"){
          bd_car_01$Por_Pre[i]=1
        }
      }
      
      if(bd_car_01$TIPO_CREDITO_CAR[i]=="H1"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="H2"){
        
        if(bd_car_01$CALIFICACION_CAR[i]== "A"){
          bd_car_01$Por_Pre[i]=0.03
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "B"){
          bd_car_01$Por_Pre[i]=0.065
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "C"){
          bd_car_01$Por_Pre[i]=0.2
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "D"){
          bd_car_01$Por_Pre[i]=0.5
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "E"){
          bd_car_01$Por_Pre[i]=0.8
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "F"){
          bd_car_01$Por_Pre[i]=1
        }
      }
      
      #i=7
      if(bd_car_01$TIPO_CREDITO_CAR[i]=="N0"|bd_car_01$TIPO_CREDITO_CAR[i]=="N1"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="N2"){
        if(((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])<=20091217){
          if(bd_car_01$CALIFICACION_CAR[i]== "A"){
            bd_car_01$Por_Pre[i]=0.0025
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "B"){
            bd_car_01$Por_Pre[i]=0.05
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "C"){
            bd_car_01$Por_Pre[i]=0.2
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "D"){
            bd_car_01$Por_Pre[i]=0.5
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "E"){
            bd_car_01$Por_Pre[i]=0.8
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "F"){
            bd_car_01$Por_Pre[i]=1
          }
        }
        if(((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])>=20091217){ 
          if(((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])<=20101216){
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre[i]=0.015
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre[i]=0.065
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre[i]=1
            }
          }
        }
        
        if( ((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])>=20101217){
          
          if(bd_car_01$CALIFICACION_CAR[i]== "A"){
            bd_car_01$Por_Pre[i]=0.03
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "B"){
            bd_car_01$Por_Pre[i]=0.065
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "C"){
            bd_car_01$Por_Pre[i]=0.2
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "D"){
            bd_car_01$Por_Pre[i]=0.5
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "E"){
            bd_car_01$Por_Pre[i]=0.8
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "F"){
            bd_car_01$Por_Pre[i]=1
          }
        }  
      }
      
      if(bd_car_01$TIPO_CREDITO_CAR[i]=="M0"|bd_car_01$TIPO_CREDITO_CAR[i]=="M1"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="M2"|bd_car_01$TIPO_CREDITO_CAR[i]=="M3"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="M4"|bd_car_01$TIPO_CREDITO_CAR[i]=="M7"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="M8"|bd_car_01$TIPO_CREDITO_CAR[i]=="M9"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="P1"|bd_car_01$TIPO_CREDITO_CAR[i]=="P2"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="P3"|bd_car_01$TIPO_CREDITO_CAR[i]=="P4"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="P8"|bd_car_01$TIPO_CREDITO_CAR[i]=="P9"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="H0"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="H3"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="H4"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="H1"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="H2"
      ){
        
        # transitorio
        if( ((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])>=20210802){
          if( ((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])<=20220729){
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              if(bd_car_01$Calificacion_Mayor[i]==0){#if(bd_car_01$Cambio_Calificacion[i]=="NO"){
                if(bd_car_01$MODULO_CAR[i]!= 121){
                  if(bd_car_01$nuevo_refis[i] == 0){# si cambio de operacion original
                  bd_car_01$Por_Pre[i]=0.00
                  bd_car_01$Nuevo_Excepcion[i] = "SI"
                  }
                }
              }
            }
            
          }
        }
      }
      # incorporar la nuevas desde el mismo rango de fecha
      
      
      
    }else{
      if(bd_car_01$MONEDA_CAR[i]==101){
        if(bd_car_01$Tipo_Cartera[i]==1){
          if(bd_car_01$TIPO_CREDITO_CAR[i]=="M0"|bd_car_01$TIPO_CREDITO_CAR[i]=="M1"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="M2"|bd_car_01$TIPO_CREDITO_CAR[i]=="M3"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="M4"|bd_car_01$TIPO_CREDITO_CAR[i]=="M7"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="M8"|bd_car_01$TIPO_CREDITO_CAR[i]=="M9"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="P1"|bd_car_01$TIPO_CREDITO_CAR[i]=="P2"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="P3"|bd_car_01$TIPO_CREDITO_CAR[i]=="P4"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="P8"|bd_car_01$TIPO_CREDITO_CAR[i]=="P9"){
            
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre[i]=0.01
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre[i]=0.05
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre[i]=1
            }
          }
        }
        
        if(bd_car_01$Tipo_Cartera[i]==0){
          if(bd_car_01$TIPO_CREDITO_CAR[i]=="M0"|bd_car_01$TIPO_CREDITO_CAR[i]=="M1"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="M2"|bd_car_01$TIPO_CREDITO_CAR[i]=="M3"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="M4"|bd_car_01$TIPO_CREDITO_CAR[i]=="M7"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="M8"|bd_car_01$TIPO_CREDITO_CAR[i]=="M9"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="P1"|bd_car_01$TIPO_CREDITO_CAR[i]=="P2"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="P3"|bd_car_01$TIPO_CREDITO_CAR[i]=="P4"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="P8"|bd_car_01$TIPO_CREDITO_CAR[i]=="P9"){
            
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre[i]=0.025
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre[i]=0.05
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre[i]=1
            }
          }
        }
        
        
        if(bd_car_01$TIPO_CREDITO_CAR[i]=="H0"|
           bd_car_01$TIPO_CREDITO_CAR[i]=="H3"|
           bd_car_01$TIPO_CREDITO_CAR[i]=="H4"){
          
          if(bd_car_01$CALIFICACION_CAR[i]== "A"){
            bd_car_01$Por_Pre[i]=0.025
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "B"){
            bd_car_01$Por_Pre[i]=0.05
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "C"){
            bd_car_01$Por_Pre[i]=0.2
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "D"){
            bd_car_01$Por_Pre[i]=0.5
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "E"){
            bd_car_01$Por_Pre[i]=0.8
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "F"){
            bd_car_01$Por_Pre[i]=1
          }
        }
        
        
        if(bd_car_01$TIPO_CREDITO_CAR[i]=="H1"|
           bd_car_01$TIPO_CREDITO_CAR[i]=="H2"){
          
          if(bd_car_01$CALIFICACION_CAR[i]== "A"){
            bd_car_01$Por_Pre[i]=0.07
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "B"){
            bd_car_01$Por_Pre[i]=0.12
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "C"){
            bd_car_01$Por_Pre[i]=0.2
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "D"){
            bd_car_01$Por_Pre[i]=0.5
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "E"){
            bd_car_01$Por_Pre[i]=0.8
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "F"){
            bd_car_01$Por_Pre[i]=1
          }
        }
        
        if(bd_car_01$TIPO_CREDITO_CAR[i]=="N0"|bd_car_01$TIPO_CREDITO_CAR[i]=="N1"|
           bd_car_01$TIPO_CREDITO_CAR[i]=="N2"){
          
          if(((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])<20091217){
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre[i]=0.025
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre[i]=0.05
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre[i]=1
            }
          }
          if(((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])>=20091217 &
             ((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])<=20101216){
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre[i]=0.05
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre[i]=0.08
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre[i]=1
            }
          }
          
          if(((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])>=20101217){
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre[i]=0.07
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre[i]=0.12
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre[i]=1
            }
          }
          
        }
        
      } 
    }
  }
}

names(bd_car_01)



# CON LA ANTIGUA CALIFICACIÓN


bd_car_01$Por_Pre_02=rep(0,nrow(bd_car_01))
attach(bd_car_01)
i=90

# microcredito y pyme aplicar los caedec del sector turismo y producccion intelectual


for (i in 1:nrow(bd_car_01)){
  if(is.na(bd_car_01$MONEDA_CAR[i])==F){
    if(bd_car_01$MONEDA_CAR[i]==0){
      if(bd_car_01$TIPO_CREDITO_CAR[i]=="M0"|bd_car_01$TIPO_CREDITO_CAR[i]=="M1"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="M2"|bd_car_01$TIPO_CREDITO_CAR[i]=="M3"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="M4"|bd_car_01$TIPO_CREDITO_CAR[i]=="M7"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="M8"|bd_car_01$TIPO_CREDITO_CAR[i]=="M9"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="P1"|bd_car_01$TIPO_CREDITO_CAR[i]=="P2"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="P3"|bd_car_01$TIPO_CREDITO_CAR[i]=="P4"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="P8"|bd_car_01$TIPO_CREDITO_CAR[i]=="P9"){
        if(bd_car_01$Sector_productivo[i] == "SI"){#if(bd_car_01$Sector_productivo[i] == "SI"){
          if(bd_car_01$CALIFICACION_CAR[i]== "A"){
            bd_car_01$Por_Pre_02[i]=0
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "B"){
            bd_car_01$Por_Pre_02[i]=0.025
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "C"){
            bd_car_01$Por_Pre_02[i]=0.2
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "D"){
            bd_car_01$Por_Pre_02[i]=0.5
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "E"){
            bd_car_01$Por_Pre_02[i]=0.8
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "F"){
            bd_car_01$Por_Pre_02[i]=1
          }
        }else{
          if(bd_car_01$Sector_productivo[i] == "NO"){#if(bd_car_01$Sector_productivo[i] == "NO"){
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre_02[i]=0.0025
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre_02[i]=0.05
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre_02[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre_02[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre_02[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre_02[i]=1
            }
          }
        }
      }
      
      if(bd_car_01$TIPO_CREDITO_CAR[i]=="H0"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="H3"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="H4"){
        
        if(bd_car_01$CALIFICACION_CAR[i]== "A"){
          bd_car_01$Por_Pre_02[i]=0.0025
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "B"){
          bd_car_01$Por_Pre_02[i]=0.05
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "C"){
          bd_car_01$Por_Pre_02[i]=0.2
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "D"){
          bd_car_01$Por_Pre_02[i]=0.5
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "E"){
          bd_car_01$Por_Pre_02[i]=0.8
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "F"){
          bd_car_01$Por_Pre_02[i]=1
        }
      }
      
      if(bd_car_01$TIPO_CREDITO_CAR[i]=="H1"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="H2"){
        
        if(bd_car_01$CALIFICACION_CAR[i]== "A"){
          bd_car_01$Por_Pre_02[i]=0.03
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "B"){
          bd_car_01$Por_Pre_02[i]=0.065
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "C"){
          bd_car_01$Por_Pre_02[i]=0.2
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "D"){
          bd_car_01$Por_Pre_02[i]=0.5
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "E"){
          bd_car_01$Por_Pre_02[i]=0.8
        }
        if(bd_car_01$CALIFICACION_CAR[i]== "F"){
          bd_car_01$Por_Pre_02[i]=1
        }
      }
      
      #i=7
      if(bd_car_01$TIPO_CREDITO_CAR[i]=="N0"|bd_car_01$TIPO_CREDITO_CAR[i]=="N1"|
         bd_car_01$TIPO_CREDITO_CAR[i]=="N2"){
        if(((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])<=20091217){
          if(bd_car_01$CALIFICACION_CAR[i]== "A"){
            bd_car_01$Por_Pre_02[i]=0.0025
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "B"){
            bd_car_01$Por_Pre_02[i]=0.05
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "C"){
            bd_car_01$Por_Pre_02[i]=0.2
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "D"){
            bd_car_01$Por_Pre_02[i]=0.5
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "E"){
            bd_car_01$Por_Pre_02[i]=0.8
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "F"){
            bd_car_01$Por_Pre_02[i]=1
          }
        }
        if(((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])>=20091217){ 
          if(((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])<=20101216){
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre_02[i]=0.015
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre_02[i]=0.065
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre_02[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre_02[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre_02[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre_02[i]=1
            }
          }
        }
        
        if( ((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])>=20101217){
          
          if(bd_car_01$CALIFICACION_CAR[i]== "A"){
            bd_car_01$Por_Pre_02[i]=0.03
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "B"){
            bd_car_01$Por_Pre_02[i]=0.065
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "C"){
            bd_car_01$Por_Pre_02[i]=0.2
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "D"){
            bd_car_01$Por_Pre_02[i]=0.5
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "E"){
            bd_car_01$Por_Pre_02[i]=0.8
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "F"){
            bd_car_01$Por_Pre_02[i]=1
          }
        }  
      }
    }else{
      if(bd_car_01$MONEDA_CAR[i]==101){
        if(bd_car_01$Tipo_Cartera[i]==1){
          if(bd_car_01$TIPO_CREDITO_CAR[i]=="M0"|bd_car_01$TIPO_CREDITO_CAR[i]=="M1"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="M2"|bd_car_01$TIPO_CREDITO_CAR[i]=="M3"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="M4"|bd_car_01$TIPO_CREDITO_CAR[i]=="M7"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="M8"|bd_car_01$TIPO_CREDITO_CAR[i]=="M9"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="P1"|bd_car_01$TIPO_CREDITO_CAR[i]=="P2"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="P3"|bd_car_01$TIPO_CREDITO_CAR[i]=="P4"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="P8"|bd_car_01$TIPO_CREDITO_CAR[i]=="P9"){
            
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre_02[i]=0.01
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre_02[i]=0.05
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre_02[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre_02[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre_02[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre_02[i]=1
            }
          }
        }
        
        if(bd_car_01$Tipo_Cartera[i]==0){
          if(bd_car_01$TIPO_CREDITO_CAR[i]=="M0"|bd_car_01$TIPO_CREDITO_CAR[i]=="M1"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="M2"|bd_car_01$TIPO_CREDITO_CAR[i]=="M3"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="M4"|bd_car_01$TIPO_CREDITO_CAR[i]=="M7"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="M8"|bd_car_01$TIPO_CREDITO_CAR[i]=="M9"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="P1"|bd_car_01$TIPO_CREDITO_CAR[i]=="P2"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="P3"|bd_car_01$TIPO_CREDITO_CAR[i]=="P4"|
             bd_car_01$TIPO_CREDITO_CAR[i]=="P8"|bd_car_01$TIPO_CREDITO_CAR[i]=="P9"){
            
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre_02[i]=0.025
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre_02[i]=0.05
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre_02[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre_02[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre_02[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre_02[i]=1
            }
          }
        }
        
        
        if(bd_car_01$TIPO_CREDITO_CAR[i]=="H0"|
           bd_car_01$TIPO_CREDITO_CAR[i]=="H3"|
           bd_car_01$TIPO_CREDITO_CAR[i]=="H4"){
          
          if(bd_car_01$CALIFICACION_CAR[i]== "A"){
            bd_car_01$Por_Pre_02[i]=0.025
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "B"){
            bd_car_01$Por_Pre_02[i]=0.05
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "C"){
            bd_car_01$Por_Pre_02[i]=0.2
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "D"){
            bd_car_01$Por_Pre_02[i]=0.5
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "E"){
            bd_car_01$Por_Pre_02[i]=0.8
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "F"){
            bd_car_01$Por_Pre_02[i]=1
          }
        }
        
        
        if(bd_car_01$TIPO_CREDITO_CAR[i]=="H1"|
           bd_car_01$TIPO_CREDITO_CAR[i]=="H2"){
          
          if(bd_car_01$CALIFICACION_CAR[i]== "A"){
            bd_car_01$Por_Pre_02[i]=0.07
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "B"){
            bd_car_01$Por_Pre_02[i]=0.12
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "C"){
            bd_car_01$Por_Pre_02[i]=0.2
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "D"){
            bd_car_01$Por_Pre_02[i]=0.5
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "E"){
            bd_car_01$Por_Pre_02[i]=0.8
          }
          if(bd_car_01$CALIFICACION_CAR[i]== "F"){
            bd_car_01$Por_Pre_02[i]=1
          }
        }
        
        if(bd_car_01$TIPO_CREDITO_CAR[i]=="N0"|bd_car_01$TIPO_CREDITO_CAR[i]=="N1"|
           bd_car_01$TIPO_CREDITO_CAR[i]=="N2"){
          
          if(((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])<20091217){
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre_02[i]=0.025
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre_02[i]=0.05
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre_02[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre_02[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre_02[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre_02[i]=1
            }
          }
          if(((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])>=20091217 &
             ((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])<=20101216){
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre_02[i]=0.05
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre_02[i]=0.08
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre_02[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre_02[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre_02[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre_02[i]=1
            }
          }
          
          if(((bd_car_01$anio[i]*100+bd_car_01$mes[i])*100+bd_car_01$dia[i])>=20101217){
            if(bd_car_01$CALIFICACION_CAR[i]== "A"){
              bd_car_01$Por_Pre_02[i]=0.07
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "B"){
              bd_car_01$Por_Pre_02[i]=0.12
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "C"){
              bd_car_01$Por_Pre_02[i]=0.2
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "D"){
              bd_car_01$Por_Pre_02[i]=0.5
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "E"){
              bd_car_01$Por_Pre_02[i]=0.8
            }
            if(bd_car_01$CALIFICACION_CAR[i]== "F"){
              bd_car_01$Por_Pre_02[i]=1
            }
          }
          
        }
        
      } 
    }
  }
}





# transformamos el monto computable a bolivianos
#bd_car_01 <- bd_car_01 %>% 
#  mutate(Monto_Computable_Bs = case_when(MONEDA_CAR == 0 ~ Monto_Computable,
#                                         MONEDA_CAR != 0 ~ Monto_Computable*6.86))


bd_car_01=dplyr::mutate(bd_car_01, Prev_Cal=round(bd_car_01$Monto_Computable*Por_Pre,2))#`Monto computable`

names(bd_car_01)
dim(bd_car_01)

# convertimos en bolivianos el saldo y las previsiones
bd_car_01 <- bd_car_01 %>% 
  mutate(Saldo_Bs = case_when(MONEDA_CAR == 0 ~ SALDO_CAR,
                               MONEDA_CAR != 0 ~ SALDO_CAR*6.86),
         Prev_Cons_RIE = Prev_Cal*6.86,
         Prev_Cons_CAR = case_when(MONEDA_CAR == 0 ~ PREVCONST_CAR,
                                   MONEDA_CAR != 0 ~ PREVCONST_CAR*6.86),
         Prev_Cons_TI = Prev_Const_TI_Usd*6.86,
         Saldo_TI_Bs = Saldo_TI_Sus*6.86,
         RUBRO_3=RUBRO_CAR%/%10000000000
         )

#-------------------------------------
# reeerdonamos las vriables
names(bd_car_01)

bd_car_01_01 <- bd_car_01 %>% 
  select(
    CTACLIENTE,
    OPERACION,
    SUBOPERACION_CAR,
    REFINANCIAMIENTO_GENUINO,
    #OPERACION_ORI_REF,
    nuevo_refis,
    dia,
    mes,
    anio,
    Tipo_Cartera,
    RUBRO_3,
    #CATCATEG,
    #desde,
    #hasta,
    #Cambio_Calificacion,
    Calificacion_Mayor,
    #Nuevo_Excepcion,
    
    MODULO_CAR,
    #MODULO_TI = "Modulo",#"Módulo",
    MONEDA_CAR,
    #MONEDA_TI = Moneda,
    MONTO_CAR,
    SALDO_CAR,
    #SALDO_TI = Saldo,
    
    FDESEMBOLSO_CAR,
    #FDESEMBOLSO_TI = fechaApertura,#"Fecha Apertura",
    CALIFICACION_CAR,
    #CALIFICACION_TI = Calificacion,
    #LOG_CALIFICACION = "Log Calificacion",
    
    #VALOR_HIPOTECARIO = "Valor hipotecario",
    #VALOR_AUTOLIQUIDABLE = "Valor Autoliquidable",
    MONTOCOMPUTABLE_TI = Monto_Computable, #"Monto computable",
    PORCENTAJE_PREV_CAL = Por_Pre,
    PORCENTAJE_PREV_CAL_02 = Por_Pre_02,
    #PORCENTAJE_PREV_TI = porcentaje,
    Prev_Cons_CAR,
    Prev_Cons_TI,
    Prev_Cons_RIE,
    
    TIPO_CREDITO_CAR,
    #TIPO_CREDITO_TI = JSBY105cla,
    #CAEDEC_TI = CAEDEC,
    CAEDEC_DEST,
    #Cat_CAEDEC,
    Turismo_Prop_Intelectual,
    DESC_OBJCRED,
    OBJETO_CRED,
    TIPOTASA,
    TASAACT,
    TAM_ACTIV,
    SECTOR_CARTERA,
    Estr_Cartera,
    Sector_productivo,
    #INDICE_TAM,
    #Sector_productivo_CAR = Sector_productivo,
    #Sector_productivo_TI = ProductivoPrev, #Sector, #"Sector Productivo",
    #CodigoClasif,
    #Clasificacion,
    #detalle,
    #ART21DT,
    Saldo_Bs,
    #Prev_Cons_RIE,
    #Prev_Cons_CAR,
    #Prev_Cons_TI
    Saldo_TI_Bs
      )

names(bd_car_01_01)

setwd("F:/Prevision_Validacion")
write.csv(bd_car_01_01, '20220331_BD_Previsiones_Calculada_02.csv')

# guardamos el consolidado por RUBRO
Rubro_2 <- bd_car_01_01%>%
  group_by(RUBRO_3)%>%
  summarise(Prev_Cons_CAR = round(sum(Prev_Cons_CAR),2),
            Prev_Cons_RIE = round(sum(Prev_Cons_RIE),2),
            Prev_Cons_TI = round(sum(Prev_Cons_TI),2),
            Saldo_Bs = sum(Saldo_Bs),
            Saldo_TI_Bs = sum(Saldo_TI_Bs))

sum(Rubro_2$Saldo_Bs)
sum(Rubro_2$Saldo_TI_Bs)
sum(Rubro_2$Prev_Cons_CAR)
sum(Rubro_2$Prev_Cons_RIE)
sum(Rubro_2$Prev_Cons_TI)


# eliminamos la fila de NAs
Rubro_2 <- na.omit(Rubro_2)

# seleccionamos los rubrosde cartera
Rubro_2 <- filter(Rubro_2, RUBRO_3 == 131 |
                    RUBRO_3 == 133 |
                    RUBRO_3 == 134 |
                    RUBRO_3 == 135 |
                    RUBRO_3 == 136 |
                    RUBRO_3 == 137 |
                    RUBRO_3 == 623 )

sum(Rubro_2$Saldo_Bs)
sum(Rubro_2$Saldo_TI_Bs)
sum(Rubro_2$Prev_Cons_CAR)
sum(Rubro_2$Prev_Cons_RIE)
sum(Rubro_2$Prev_Cons_TI)

setwd("F:/Prevision_Validacion")
write.csv(Rubro_2, '20220331_Rubro_Previsiones_Calculada.csv ')

#--------------------------------------------
#--------------------------------------------





table(bd_car_01$TAM_ACTIV)

table(is.na(bd_car_01$TAM_ACTIV))

x <- filter(bd_car_01, OPERACION==3059143
)

x$TAM_ACTIV
x$INDICE_TAM

View(bd_car_01)

#----------------------------------------------------------
#----------------------------------------------------------
bd_car_01 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211231.txt",header=T, sep="|")

bd_car_01 <- bd_car_01 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_01 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_01) %>% 
  distinct()

bd_car_02 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211128.txt",header=T, sep="|")

bd_car_02 <- bd_car_02 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_02 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_02) %>% 
  distinct()

bd_car_03 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211127.txt",header=T, sep="|")

bd_car_03 <- bd_car_03 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_03 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_03) %>% 
  distinct()

bd_car_04 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211126.txt",header=T, sep="|")

bd_car_04 <- bd_car_04 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_04 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_04) %>% 
  distinct()


bd_car_05 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211125.txt",header=T, sep="|")

bd_car_05 <- bd_car_05 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_05 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_05) %>% 
  distinct()


bd_car_06 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211124.txt",header=T, sep="|")

bd_car_06 <- bd_car_06 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_06 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_06) %>% 
  distinct()


bd_car_07 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211123.txt",header=T, sep="|")

bd_car_07 <- bd_car_07 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_07 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_07) %>% 
  distinct()


bd_car_08 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211122.txt",header=T, sep="|")

bd_car_08 <- bd_car_08 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_08 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_08) %>% 
  distinct()


bd_car_09 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211121.txt",header=T, sep="|")

bd_car_09 <- bd_car_09 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_09 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_09) %>% 
  distinct()


bd_car_10 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211120.txt",header=T, sep="|")

bd_car_10 <- bd_car_10 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_10 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_10) %>% 
  distinct()

bd_car_11 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211119.txt",header=T, sep="|")

bd_car_11 <- bd_car_11 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_11 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_11) %>% 
  distinct()

bd_car_12 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211118.txt",header=T, sep="|")

bd_car_12 <- bd_car_12 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_12 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_12) %>% 
  distinct()

bd_car_13 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211117.txt",header=T, sep="|")

bd_car_13 <- bd_car_13 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_13 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_13) %>% 
  distinct()

bd_car_14 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211116.txt",header=T, sep="|")

bd_car_14 <- bd_car_14 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_14 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_14) %>% 
  distinct()

bd_car_15 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211115.txt",header=T, sep="|")

bd_car_15 <- bd_car_15 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_15 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_15) %>% 
  distinct()

bd_car_16 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211114.txt",header=T, sep="|")

bd_car_16 <- bd_car_16 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_16 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_16) %>% 
  distinct()

bd_car_17 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211113.txt",header=T, sep="|")

bd_car_17 <- bd_car_17 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_17 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_17) %>% 
  distinct()

bd_car_18 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211112.txt",header=T, sep="|")

bd_car_18 <- bd_car_18 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_18 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_18) %>% 
  distinct()

bd_car_19 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211111.txt",header=T, sep="|")

bd_car_19 <- bd_car_19 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_19 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_19) %>% 
  distinct()

bd_car_20 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211110.txt",header=T, sep="|")

bd_car_20 <- bd_car_20 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_20 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_20) %>% 
  distinct()

bd_car_21 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211109.txt",header=T, sep="|")

bd_car_21 <- bd_car_21 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_21 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_21) %>% 
  distinct()

bd_car_22 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211108.txt",header=T, sep="|")

bd_car_22 <- bd_car_22 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_22 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_22) %>% 
  distinct()

bd_car_23 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211107.txt",header=T, sep="|")

bd_car_23 <- bd_car_23 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_23 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_23) %>% 
  distinct()

bd_car_24 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211106.txt",header=T, sep="|")

bd_car_24 <- bd_car_24 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_24 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_24) %>% 
  distinct()

bd_car_25 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211105.txt",header=T, sep="|")

bd_car_25 <- bd_car_25 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_25 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_25) %>% 
  distinct()

bd_car_26 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211104.txt",header=T, sep="|")

bd_car_26 <- bd_car_26 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_26 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_26) %>% 
  distinct()

bd_car_27 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211103.txt",header=T, sep="|")

bd_car_27 <- bd_car_27 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_27 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_27) %>% 
  distinct()

bd_car_28 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211102.txt",header=T, sep="|")

bd_car_28 <- bd_car_28 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_28 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_28) %>% 
  distinct()

bd_car_29 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211101.txt",header=T, sep="|")

bd_car_29 <- bd_car_29 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_29 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_29) %>% 
  distinct()

#----------------------------------------------------------
bd_car_30 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211031.txt",header=T, sep="|")

bd_car_30 <- bd_car_30 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_30 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_30) %>% 
  distinct()

bd_car_31 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211030.txt",header=T, sep="|")

bd_car_31 <- bd_car_31 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_31 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_31) %>% 
  distinct()



bd_car_32 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211029.txt",header=T, sep="|")

bd_car_32 <- bd_car_32 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_32 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_32) %>% 
  distinct()

bd_car_33 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211028.txt",header=T, sep="|")

bd_car_33 <- bd_car_33 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_33 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_33) %>% 
  distinct()

bd_car_34 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211027.txt",header=T, sep="|")

bd_car_34 <- bd_car_34 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_34 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_34) %>% 
  distinct()

bd_car_35 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211026.txt",header=T, sep="|")

bd_car_35 <- bd_car_35 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_35 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_35) %>% 
  distinct()


bd_car_36 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211025.txt",header=T, sep="|")

bd_car_36 <- bd_car_36 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_36 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_36) %>% 
  distinct()


bd_car_37 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211024.txt",header=T, sep="|")

bd_car_37 <- bd_car_37 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_37 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_37) %>% 
  distinct()


bd_car_38 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211023.txt",header=T, sep="|")

bd_car_38 <- bd_car_38 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_38 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_38) %>% 
  distinct()


bd_car_39 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211022.txt",header=T, sep="|")

bd_car_39 <- bd_car_39 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_39 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_39) %>% 
  distinct()


bd_car_40 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211021.txt",header=T, sep="|")

bd_car_40 <- bd_car_40 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_40 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_40) %>% 
  distinct()


bd_car_41 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211020.txt",header=T, sep="|")

bd_car_41 <- bd_car_41 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_41 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_41) %>% 
  distinct()

bd_car_42 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211019.txt",header=T, sep="|")

bd_car_42 <- bd_car_42 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_42 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_42) %>% 
  distinct()

bd_car_43 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211018.txt",header=T, sep="|")

bd_car_43 <- bd_car_43 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_43 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_43) %>% 
  distinct()

bd_car_44 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211017.txt",header=T, sep="|")

bd_car_44 <- bd_car_44 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_44 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_44) %>% 
  distinct()

bd_car_45 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211016.txt",header=T, sep="|")

bd_car_45 <- bd_car_45 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_45 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_45) %>% 
  distinct()

bd_car_46 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211015.txt",header=T, sep="|")

bd_car_46 <- bd_car_46 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_46 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_46) %>% 
  distinct()

bd_car_47 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211014.txt",header=T, sep="|")

bd_car_47 <- bd_car_47 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_47 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_47) %>% 
  distinct()

bd_car_48 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211013.txt",header=T, sep="|")

bd_car_48 <- bd_car_48 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_48 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_48) %>% 
  distinct()

bd_car_49 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211012.txt",header=T, sep="|")

bd_car_49 <- bd_car_49 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_49 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_49) %>% 
  distinct()

bd_car_50 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211011.txt",header=T, sep="|")

bd_car_50 <- bd_car_50 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_50 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_50) %>% 
  distinct()

bd_car_51 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211010.txt",header=T, sep="|")

bd_car_51 <- bd_car_51 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_51 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_51) %>% 
  distinct()

bd_car_52 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211009.txt",header=T, sep="|")

bd_car_52 <- bd_car_52 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_52 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_52) %>% 
  distinct()

bd_car_53 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211008.txt",header=T, sep="|")

bd_car_53 <- bd_car_53 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_53 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_53) %>% 
  distinct()

bd_car_54 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211007.txt",header=T, sep="|")

bd_car_54 <- bd_car_54 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_54 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_54) %>% 
  distinct()

bd_car_55 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211006.txt",header=T, sep="|")

bd_car_55 <- bd_car_55 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_55 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_55) %>% 
  distinct()

bd_car_56 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211005.txt",header=T, sep="|")

bd_car_56 <- bd_car_56 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_56 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_56) %>% 
  distinct()

bd_car_57 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211004.txt",header=T, sep="|")

bd_car_57 <- bd_car_57 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_57 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_57) %>% 
  distinct()

bd_car_58 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211003.txt",header=T, sep="|")

bd_car_58 <- bd_car_58 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_58 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_58) %>% 
  distinct()

bd_car_59 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211002.txt",header=T, sep="|")

bd_car_59 <- bd_car_59 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_59 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_59) %>% 
  distinct()

bd_car_60 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211001.txt",header=T, sep="|")

bd_car_60 <- bd_car_60 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_60 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_60) %>% 
  distinct()



#----------------------------------------------------------
bd_car_61 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210930.txt",header=T, sep="|")

bd_car_61 <- bd_car_61 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_61 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_61) %>% 
  distinct()



bd_car_62 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210929.txt",header=T, sep="|")

bd_car_62 <- bd_car_62 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_62 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_62) %>% 
  distinct()

bd_car_63 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210928.txt",header=T, sep="|")

bd_car_63 <- bd_car_63 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_63 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_63) %>% 
  distinct()


bd_car_64 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210926.txt",header=T, sep="|")

bd_car_64 <- bd_car_64 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_64 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_64) %>% 
  distinct()


bd_car_65 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210925.txt",header=T, sep="|")

bd_car_65 <- bd_car_65 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_65 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_65) %>% 
  distinct()


bd_car_66 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210924.txt",header=T, sep="|")

bd_car_66 <- bd_car_66 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_66 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_66) %>% 
  distinct()


bd_car_67 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210923.txt",header=T, sep="|")

bd_car_67 <- bd_car_67 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_67 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_67) %>% 
  distinct()


bd_car_68 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210922.txt",header=T, sep="|")

bd_car_68 <- bd_car_68 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_68 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_68) %>% 
  distinct()


bd_car_69 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210921.txt",header=T, sep="|")

bd_car_69 <- bd_car_69 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_69 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_69) %>% 
  distinct()


bd_car_70 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210920.txt",header=T, sep="|")

bd_car_70 <- bd_car_70 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_70 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_70) %>% 
  distinct()

bd_car_71 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210919.txt",header=T, sep="|")

bd_car_71 <- bd_car_71 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_71 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_71) %>% 
  distinct()

bd_car_72 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210918.txt",header=T, sep="|")

bd_car_72 <- bd_car_72 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_72 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_72) %>% 
  distinct()

bd_car_73 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210917.txt",header=T, sep="|")

bd_car_73 <- bd_car_73 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_73 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_73) %>% 
  distinct()

bd_car_74 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210916.txt",header=T, sep="|")

bd_car_74 <- bd_car_74 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_74 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_74) %>% 
  distinct()

bd_car_75 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210915.txt",header=T, sep="|")

bd_car_75 <- bd_car_75 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_75 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_75) %>% 
  distinct()

bd_car_76 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210914.txt",header=T, sep="|")

bd_car_76 <- bd_car_76 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_76 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_76) %>% 
  distinct()

bd_car_77 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210913.txt",header=T, sep="|")

bd_car_77 <- bd_car_77 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_77 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_77) %>% 
  distinct()

bd_car_78 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210912.txt",header=T, sep="|")

bd_car_78 <- bd_car_78 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_78 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_78) %>% 
  distinct()

bd_car_79 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210911.txt",header=T, sep="|")

bd_car_79 <- bd_car_79 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_79 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_79) %>% 
  distinct()

bd_car_80 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210910.txt",header=T, sep="|")

bd_car_80 <- bd_car_80 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_80 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_80) %>% 
  distinct()

bd_car_81 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210909.txt",header=T, sep="|")

bd_car_81 <- bd_car_81 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_81 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_81) %>% 
  distinct()

bd_car_82 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210908.txt",header=T, sep="|")

bd_car_82 <- bd_car_82 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_82 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_82) %>% 
  distinct()

bd_car_83 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210907.txt",header=T, sep="|")

bd_car_83 <- bd_car_83 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_83 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_83) %>% 
  distinct()

bd_car_84 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210906.txt",header=T, sep="|")

bd_car_84 <- bd_car_84 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_84 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_84) %>% 
  distinct()

bd_car_85 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210905.txt",header=T, sep="|")

bd_car_85 <- bd_car_85 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_85 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_85) %>% 
  distinct()


bd_car_86 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210903.txt",header=T, sep="|")

bd_car_86 <- bd_car_86 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_86 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_86) %>% 
  distinct()

bd_car_87 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210902.txt",header=T, sep="|")

bd_car_87 <- bd_car_87 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_87 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_87) %>% 
  distinct()

bd_car_88 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210901.txt",header=T, sep="|")

bd_car_88 <- bd_car_88 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_88 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_88) %>% 
  distinct()


#----------------------------------------------------------
bd_car_89 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210831.txt",header=T, sep="|")

bd_car_89 <- bd_car_89 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_89 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_89) %>% 
  distinct()

bd_car_90 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210830.txt",header=T, sep="|")

bd_car_90 <- bd_car_90 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_90 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_90) %>% 
  distinct()



bd_car_91 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210829.txt",header=T, sep="|")

bd_car_91 <- bd_car_91 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_91 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_91) %>% 
  distinct()


bd_car_92 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210826.txt",header=T, sep="|")

bd_car_92 <- bd_car_92 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_92 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_92) %>% 
  distinct()


bd_car_93 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210825.txt",header=T, sep="|")

bd_car_93 <- bd_car_93 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_93 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_93) %>% 
  distinct()


bd_car_94 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210824.txt",header=T, sep="|")

bd_car_94 <- bd_car_94 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_94 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_94) %>% 
  distinct()


bd_car_95 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210823.txt",header=T, sep="|")

bd_car_95 <- bd_car_95 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_95 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_95) %>% 
  distinct()


bd_car_96 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210822.txt",header=T, sep="|")

bd_car_96 <- bd_car_96 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_96 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_96) %>% 
  distinct()



bd_car_97 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210820.txt",header=T, sep="|")

bd_car_97 <- bd_car_97 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_97 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_97) %>% 
  distinct()

bd_car_98 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210819.txt",header=T, sep="|")

bd_car_98 <- bd_car_98 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_98 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_98) %>% 
  distinct()

bd_car_99 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210818.txt",header=T, sep="|")

bd_car_99 <- bd_car_99 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_99 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_99) %>% 
  distinct()

bd_car_100 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210817.txt",header=T, sep="|")

bd_car_100 <- bd_car_100 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_100 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_100) %>% 
  distinct()

bd_car_101 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210816.txt",header=T, sep="|")

bd_car_101 <- bd_car_101 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_101 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_101) %>% 
  distinct()

bd_car_102 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210815.txt",header=T, sep="|")

bd_car_102 <- bd_car_102 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_102 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_102) %>% 
  distinct()



bd_car_103 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210810.txt",header=T, sep="|")

bd_car_103 <- bd_car_103 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_103 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_103) %>% 
  distinct()

bd_car_104 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210809.txt",header=T, sep="|")

bd_car_104 <- bd_car_104 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_104 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_104) %>% 
  distinct()

bd_car_105 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210808.txt",header=T, sep="|")

bd_car_105 <- bd_car_105 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_105 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_105) %>% 
  distinct()

bd_car_106 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210807.txt",header=T, sep="|")

bd_car_106 <- bd_car_106 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_106 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_106) %>% 
  distinct()

bd_car_107 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210806.txt",header=T, sep="|")

bd_car_107 <- bd_car_107 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_107 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_107) %>% 
  distinct()

bd_car_108 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210805.txt",header=T, sep="|")

bd_car_108 <- bd_car_108 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_108 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_108) %>% 
  distinct()

bd_car_109 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210804.txt",header=T, sep="|")

bd_car_109 <- bd_car_109 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_109 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_109) %>% 
  distinct()

bd_car_110 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210803.txt",header=T, sep="|")

bd_car_110 <- bd_car_110 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_110 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_110) %>% 
  distinct()

bd_car_111 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20210802.txt",header=T, sep="|")

bd_car_111 <- bd_car_111 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_111 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_111) %>% 
  distinct()




# cargamos la información de la base de cartera diaria

bd_car_112 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211129.txt",header=T, sep="|")

bd_car_112 <- bd_car_112 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_112 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_112) %>% 
  distinct()


bd_car_113 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211130.txt",header=T, sep="|")

bd_car_113 <- bd_car_113 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_113 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_113) %>% 
  distinct()


bd_car_114 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211201.txt",header=T, sep="|")

bd_car_114 <- bd_car_114 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_114 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_114) %>% 
  distinct()

bd_car_115 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211202.txt",header=T, sep="|")

bd_car_115 <- bd_car_115 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_115 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_115) %>% 
  distinct()

bd_car_116 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211203.txt",header=T, sep="|")

bd_car_116 <- bd_car_116 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_116 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_116) %>% 
  distinct()


bd_car_117 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211204.txt",header=T, sep="|")

bd_car_117 <- bd_car_117 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_117 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_117) %>% 
  distinct()

bd_car_118 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211205.txt",header=T, sep="|")

bd_car_118 <- bd_car_118 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_118 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_118) %>% 
  distinct()

bd_car_119 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211206.txt",header=T, sep="|")

bd_car_119 <- bd_car_119 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_119 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_119) %>% 
  distinct()


bd_car_120 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211207.txt",header=T, sep="|")

bd_car_120 <- bd_car_120 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_120 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_120) %>% 
  distinct()



bd_car_121 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211208.txt",header=T, sep="|")

bd_car_121 <- bd_car_121 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_121 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_121) %>% 
  distinct()


bd_car_122 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211209.txt",header=T, sep="|")

bd_car_122 <- bd_car_122 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_122 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_122) %>% 
  distinct()


bd_car_123 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211210.txt",header=T, sep="|")

bd_car_123 <- bd_car_123 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_123 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_123) %>% 
  distinct()


bd_car_124 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211211.txt",header=T, sep="|")

bd_car_124 <- bd_car_124 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_124 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_124) %>% 
  distinct()


bd_car_125 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211212.txt",header=T, sep="|")

bd_car_125 <- bd_car_125 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_125 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_125) %>% 
  distinct()


bd_car_126 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211213.txt",header=T, sep="|")

bd_car_126 <- bd_car_126 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_126 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_126) %>% 
  distinct()

x <- filter(bd_car_126, OPERACION==3541035)
y <- filter(bd_car_125, OPERACION==3541035)

x$CALIFICACION
x$CTACLIENTE
x$CTACLIENTE
View(bd_car_126)

y$CALIFICACION

bd_car_127 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211214.txt",header=T, sep="|")

bd_car_127 <- bd_car_127 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_127 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_127) %>% 
  distinct()


bd_car_128 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211215.txt",header=T, sep="|")

bd_car_128 <- bd_car_128 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_128 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_128) %>% 
  distinct()


bd_car_129 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211216.txt",header=T, sep="|")

bd_car_129 <- bd_car_129 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_129 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_129) %>% 
  distinct()


bd_car_130 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211217.txt",header=T, sep="|")

bd_car_130 <- bd_car_130 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_130 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_130) %>% 
  distinct()

bd_car_131 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211218.txt",header=T, sep="|")

bd_car_131 <- bd_car_131 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_131 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_131) %>% 
  distinct()


bd_car_132 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211219.txt",header=T, sep="|")

bd_car_132 <- bd_car_132 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_132 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_132) %>% 
  distinct()

bd_car_133 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211220.txt",header=T, sep="|")

bd_car_133 <- bd_car_133 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_133 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_133) %>% 
  distinct()


bd_car_134 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211221.txt",header=T, sep="|")

bd_car_134 <- bd_car_134 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_134 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_134) %>% 
  distinct()



bd_car_135 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211222.txt",header=T, sep="|")

bd_car_135 <- bd_car_135 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_135 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_135) %>% 
  distinct()


bd_car_136 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211223.txt",header=T, sep="|")

bd_car_136 <- bd_car_136 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_136 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_136) %>% 
  distinct()

bd_car_137 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211225.txt",header=T, sep="|")

bd_car_137 <- bd_car_137 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_137 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_137) %>% 
  distinct()

bd_car_138 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211226.txt",header=T, sep="|")

bd_car_138 <- bd_car_138 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_138 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_138) %>% 
  distinct()



bd_car_139 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211227.txt",header=T, sep="|")

bd_car_139 <- bd_car_139 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_139 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_139) %>% 
  distinct()



bd_car_140 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211228.txt",header=T, sep="|")

bd_car_140 <- bd_car_140 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_140 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_140) %>% 
  distinct()

bd_car_141 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211229.txt",header=T, sep="|")

bd_car_141 <- bd_car_141 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_141 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_141) %>% 
  distinct()

bd_car_142 <- read.csv("F:/Prevision_Validacion/Datos/BaseCartera_20211230.txt",header=T, sep="|")

bd_car_142 <- bd_car_142 %>% 
  filter(MODULO != 131) %>% 
  filter(ESTADO != "CASTIGADA") %>% 
  mutate(CALIFICACION_142 = case_when(
    CALIFICACION == "A" ~ 1,
    CALIFICACION == "B" ~ 2,
    CALIFICACION == "C" ~ 3,
    CALIFICACION == "D" ~ 4,
    CALIFICACION == "E" ~ 5,
    CALIFICACION == "F" ~ 6
  )) %>% 
  select(CTACLIENTE, OPERACION, CALIFICACION_142) %>% 
  distinct()
#----------------------------------------------------------

dim(bd_car_01)
dim(bd_car_02)
dim(bd_car_03)
dim(bd_car_04)
dim(bd_car_05)
dim(bd_car_06)
dim(bd_car_07)
dim(bd_car_08)
dim(bd_car_09) # son similares
dim(bd_car_10) # son similares

dim(bd_car_11)
dim(bd_car_12)
dim(bd_car_13)
dim(bd_car_14)
dim(bd_car_15)

dim(bd_car_16)
dim(bd_car_17)

dim(bd_car_18)
dim(bd_car_19)
dim(bd_car_20)
dim(bd_car_21)
dim(bd_car_22)
dim(bd_car_23)
dim(bd_car_24)
dim(bd_car_25)
dim(bd_car_26)
dim(bd_car_27)

dim(bd_car_28)
dim(bd_car_29)

dim(bd_car_30)
dim(bd_car_31)

dim(bd_car_32)
dim(bd_car_33)
dim(bd_car_34)
dim(bd_car_35)
dim(bd_car_36)

dim(bd_car_37)
dim(bd_car_38)

dim(bd_car_39)
dim(bd_car_40)
dim(bd_car_41)
dim(bd_car_42)
dim(bd_car_43)
dim(bd_car_44)
dim(bd_car_45)
dim(bd_car_46)
dim(bd_car_47)
dim(bd_car_48)
dim(bd_car_49)
dim(bd_car_50)
dim(bd_car_51)
dim(bd_car_52)
dim(bd_car_53)
dim(bd_car_54)
dim(bd_car_55)
dim(bd_car_56)
dim(bd_car_57)

dim(bd_car_58)
dim(bd_car_59)

dim(bd_car_60)
dim(bd_car_61)
dim(bd_car_62)
dim(bd_car_63)

dim(bd_car_64)
dim(bd_car_65)

dim(bd_car_66)
dim(bd_car_67)
dim(bd_car_68)
dim(bd_car_69)
dim(bd_car_70)
dim(bd_car_71)
dim(bd_car_72)
dim(bd_car_73)
dim(bd_car_74)
dim(bd_car_75)
dim(bd_car_76)
dim(bd_car_77)
dim(bd_car_78)
dim(bd_car_79)
dim(bd_car_80)

dim(bd_car_80)
dim(bd_car_81)
dim(bd_car_82)
dim(bd_car_83)
dim(bd_car_84)
dim(bd_car_85)
dim(bd_car_86)
dim(bd_car_87)
dim(bd_car_88)
dim(bd_car_89)
dim(bd_car_90)
dim(bd_car_91)
dim(bd_car_92)
dim(bd_car_93)
dim(bd_car_94)
dim(bd_car_95)
dim(bd_car_96)
dim(bd_car_97)
dim(bd_car_98)
dim(bd_car_99)

dim(bd_car_100)
dim(bd_car_101)
dim(bd_car_102)
dim(bd_car_103)
dim(bd_car_104)

dim(bd_car_105)
dim(bd_car_106)
dim(bd_car_107)
dim(bd_car_108)
dim(bd_car_109)
dim(bd_car_110)
dim(bd_car_111)

dim(bd_car_112)
dim(bd_car_113)
dim(bd_car_114)
dim(bd_car_115)
dim(bd_car_116)
dim(bd_car_117)
dim(bd_car_118)
dim(bd_car_119)
dim(bd_car_120)

dim(bd_car_121)
dim(bd_car_122)
dim(bd_car_123)
dim(bd_car_124)
dim(bd_car_125)
dim(bd_car_126)
dim(bd_car_127)
dim(bd_car_128)
dim(bd_car_129)
dim(bd_car_130)
dim(bd_car_131)
dim(bd_car_132)
dim(bd_car_133)
dim(bd_car_134)
dim(bd_car_135)

dim(bd_car_136)
dim(bd_car_137)
dim(bd_car_138)
dim(bd_car_139)
dim(bd_car_140)








# fusionamos las bases de noviembre

bd_consolidado <- left_join(bd_car_01, bd_car_02, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_03, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_04, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_05, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_06, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_07, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_08, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_09, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_10, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_11, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_12, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_13, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_14, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_15, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_16, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_17, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_18, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_19, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_20, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_21, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_22, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_23, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_24, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_25, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_26, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_27, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_28, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_29, by = c("CTACLIENTE", "OPERACION"))

bd_consolidado <- left_join(bd_consolidado, bd_car_30, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_31, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_32, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_33, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_34, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_35, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_36, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_37, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_38, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_39, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_40, by = c("CTACLIENTE", "OPERACION"))

bd_consolidado <- left_join(bd_consolidado, bd_car_41, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_42, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_43, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_44, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_45, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_46, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_47, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_48, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_49, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_50, by = c("CTACLIENTE", "OPERACION"))

bd_consolidado <- left_join(bd_consolidado, bd_car_51, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_52, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_53, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_54, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_55, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_56, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_57, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_58, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_59, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_60, by = c("CTACLIENTE", "OPERACION"))

bd_consolidado <- left_join(bd_consolidado, bd_car_61, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_62, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_63, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_64, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_65, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_66, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_67, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_68, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_69, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_70, by = c("CTACLIENTE", "OPERACION"))

bd_consolidado <- left_join(bd_consolidado, bd_car_71, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_72, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_73, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_74, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_75, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_76, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_77, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_78, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_79, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_80, by = c("CTACLIENTE", "OPERACION"))

bd_consolidado <- left_join(bd_consolidado, bd_car_81, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_82, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_83, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_84, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_85, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_86, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_87, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_88, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_89, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_90, by = c("CTACLIENTE", "OPERACION"))

bd_consolidado <- left_join(bd_consolidado, bd_car_91, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_92, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_93, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_94, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_95, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_96, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_97, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_98, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_99, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_100, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_101, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_102, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_103, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_104, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_105, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_106, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_107, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_108, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_109, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_110, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_111, by = c("CTACLIENTE", "OPERACION"))

bd_consolidado <- left_join(bd_consolidado, bd_car_112, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_113, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_114, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_115, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_116, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_117, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_118, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_119, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_120, by = c("CTACLIENTE", "OPERACION"))

bd_consolidado <- left_join(bd_consolidado, bd_car_121, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_122, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_123, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_124, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_125, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_126, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_127, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_128, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_129, by = c("CTACLIENTE", "OPERACION"))

bd_consolidado <- left_join(bd_consolidado, bd_car_130, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_131, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_132, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_133, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_134, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_135, by = c("CTACLIENTE", "OPERACION"))

bd_consolidado <- left_join(bd_consolidado, bd_car_136, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_137, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_138, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_139, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_140, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_141, by = c("CTACLIENTE", "OPERACION"))
bd_consolidado <- left_join(bd_consolidado, bd_car_142, by = c("CTACLIENTE", "OPERACION"))


#-----------------------------------------------------------------------------------


names(bd_consolidado)
ncol(bd_consolidado)

#bd_consolidado_01[,3]
# verificamos si existe un cambio de calificación
bd_consolidado_01 <- bd_consolidado %>%
  #replace(is.na(.), 0) %>% 
  mutate(Total = rowSums (.[3:ncol(bd_consolidado)], na.rm = TRUE)/(ncol(bd_consolidado)-2- rowSums(is.na(.[3:ncol(bd_consolidado)])))) %>% 
  mutate(Calificacion_Mayor = case_when(Total == CALIFICACION_01 ~ 0,
                                        Total != CALIFICACION_01 ~ 1)) #%>%
  #select(CTACLIENTE,
  #       OPERACION,
  #       Total,
  #       Calificacion_Mayor)

write.csv(bd_consolidado_01, '20211231_BD_Cambio_Calificacion_Entero.csv')

names(bd_consolidado_01)
#View(bd_consolidado_01)

bd_consolidado_01 <- bd_consolidado %>%
  #replace(is.na(.), 0) %>% 
  mutate(Total = rowSums (.[3:ncol(bd_consolidado)], na.rm = TRUE)/(ncol(bd_consolidado)-2- rowSums(is.na(.[3:ncol(bd_consolidado)])))) %>% 
  mutate(Calificacion_Mayor = case_when(Total == CALIFICACION_01 ~ 0,
                                        Total != CALIFICACION_01 ~ 1)) %>%
select(CTACLIENTE,
       OPERACION,
       Total,
       Calificacion_Mayor)


setwd("F:/Prevision_Validacion")
write.csv(bd_consolidado_01, '20211231_BD_Cambio_Calificacion.csv')



#-----------------------------------------------------------
#-----------------------------------------------------------
#aumentamos la información del 30 de noviembre

# cargamos la información consolidada antes del 30 de noviembre
bd_consolidado <- read.csv("F:/Prevision_Validacion/20211129_BD_Cambio_Calificacion_Entero.csv",header=T, sep=",")

names(bd_consolidado)
dim(bd_consolidado)

# eeliminamos las dos ultimas columnas

bd_consolidado <- bd_consolidado[1:114]








#----------------------------------------------------------
#----------------------------------------------------------

# leer todos los archivos de un txt
library(tidyverse)
library(kableExtra)
library(knitr)
library(stringr)
library(readxl)

# establecemos el espacio de trabajo
setwd("F:/Prevision_Validacion")

# cargamos las bases
#datos <- map(fs::dir_ls('Datos'), ~ read_csv) %>% 
#  bind_rows(.id = 'Periodo')


# otro metodo para leer multiples archivos
setwd("F:/Prevision_Validacion/Datos")

# revisamos que existe dentro de la carpeta
# mucahs veces puede contener un mgran cantidad de información txt. csv. xlx
list.files()

# con esta función solo leemos archivos especificos
list.files(pattern = ".\\csv\\.zip")

# podemos leer solo txt
list.files(pattern = ".\\.txt")

# guardamos la variable con archivos
archivos <- list.files(pattern = ".\\.txt")

archivos <- list.files(pattern = ".\\.rar")

archivos
archivos

a <-read_csv("BaseCartera_20211101.rar")
View(a)





nchar(111)
i = 1
for (i in 1:2) {#28
  if(nchar(i)>1{
    aux <- paste0("base_",i) 
    aux <- read.csv("F:/Prevision_Validacion/BaseCartera_20211129.txt",header=T, sep="|")
  }else{
    paste0("base_0",i)  
  }
  
  #bd_car_01 <- read.csv("F:/Prevision_Validacion/BaseCartera_20211129.txt",header=T, sep="|")
  print(archivos[i])
}

# otra forma
setwd("F:/Prevision_Validacion/Datos")
archivos <- list.files(pattern = ".\\.txt")
myfiles<-lapply(archivos, read.delim)

class(myfiles)
dim(myfiles)
names(myfiles)

myfiles[1]

#------------------
files <- list.files(path="F:/Prevision_Validacion/Datos")
tmp <- lapply(files, read.delim, header = FALSE)
matriz <- do.call(rbind, tmp)
header <- scan(files[1], nlines = 1, sep = ",")
names(matriz) <- header




