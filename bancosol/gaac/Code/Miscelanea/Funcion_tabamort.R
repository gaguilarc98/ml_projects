#____EJEMPLO CON DATA FRAME____
fini <- c("02/04/2022","05/10/2022","17/09/2022")
ffin <- c("03/10/2022","06/06/2023","18/10/2023")
cap_ini <- c(6000,2000,15000)
r <- c(0.16,0.24,0.36)
m <- c(2,4,12)
df <- data.frame(A=cap_ini, redito=r,fini=fini,ffin=ffin,nropagos=m)
df$i <- df$redito/df$nropagos
df$fini <- as.Date(df$fini,format="%d/%m/%Y")
df$ffin <- as.Date(df$ffin,format="%d/%m/%Y")
df$m <- floor(as.numeric((df$ffin-df$fini)*12/365.25))
#____FUNCION TABLA DE AMORTIZACION____
#A: Capital inicial
#r: Redito anual
#n: Número de pagos
#m: Descuentos por año
#fini: Fecha inicial de crédito
tab_amort <- function(A,r,n,m=12){
    i <- r/m
    C <- A*i/(1-(1+i)^(-n))
    tabla <- matrix(0,nrow=n+1,ncol=5)
    tabla[1,] <- c(0,0,0,0,A)
    for (k in 2:(n+1)) {
      interes <- tabla[k-1,5]*i
      amort <- C-tabla[k-1,5]*i
      tabla[k,] <- c(k-1,C,interes,amort,tabla[k-1,5]-amort)
    }
    tabla[,5] <- round(tabla[,5],1)
    colnames(tabla)<- c("Mes","Cuota","Interés","Amortización","Saldo")
    return(tabla)
}
tab_amort(1000000,0.24,12,12)
#____APLICACION DE LA FUNCION A DATAFRAME
mapply(tab_amort, df$A,df$redito,df$nropagos,df$m)
#____CONSIDERANDO FECHA
tab_amort <- function(A,r,n,fini,m=12){
  i <- r/m
  C <- A*i/(1-(1+i)^(-n))
  tabla <- matrix(0,nrow=n+1,ncol=6)
  tabla[1,] <- c(0,0,0,0,A,fini+30)
  for (k in 2:(n+1)) {
    interes <- tabla[k-1,5]*i
    amort <- C-tabla[k-1,5]*i
    tabla[k,] <- c(k-1,C,interes,amort,tabla[k-1,5]-amort,fini+k*30)
  }
  tabla[,5] <- round(tabla[,5],1)
  colnames(tabla)<- c("Mes","Cuota","Interés","Amortización","Saldo","Fecha")
  return(tabla)
}
tab_amort(1000000,0.24,12,as.Date("2022-04-08"),12)

hola <- mapply(tab_amort, df$A,df$redito,df$nropagos,df$fini,df$m)
do.call(rbind,hola)
df %>% 
  dplyr::filter(row_number()==1)
library(ggplot2)
