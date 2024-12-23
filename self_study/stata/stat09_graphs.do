clear
set obs 100
set seed 12922
gen x=invnorm(uniform())
gen y=2+3*x+invnorm(uniform())
twoway (scatter y x), ytitle(Simulated y) xtitle(Simulated x)
clear
cd "C:\Files\Data"
import excel senamhi, firstrow case(lower)
rename (temp_min temp_max) (tmin tmax)
keep if tmin>(min(tmax,tmax)-8.5)
twoway (scatter tmax tmin) (lfit tmax tmin), /*
*/ ytitle(Temperatura max) xtitle(Temperatura min) /*
*/ legend(order(1 "Observed" 2 "Fitted"))
gen group=cond(_n<15,1,2)
replace tmax=tmax+3 if group==2
twoway (scatter tmax tmin if group==1,msymbol(0)) /*
*/ (lfit tmax tmin if group==1, clpat(solid)) /*
*/ (scatter tmax tmin if group==2,msymbol(0h)) /*
*/ (lfit tmax tmin if group==2, clpat(dash)), /*
*/ ytitle(y simulado) xtitle(xsimulado) /*
*/ legend(order(1 2 "Grupo 1" 3 4 "Grupo 2"))

label define gr 1 "Grupo 1" 2 "Grupo 2"
label values group gr
twoway scatter tmax tmin, by(group) /*
*/ xtitle(Temperatura min) ytitle(Temperatura max)

local a = min(tmin, tmin)
display `a'>200
matrix a=(1,2\3,4)
matrix list a
matrix eigenvalues x v = a
matrix list a
