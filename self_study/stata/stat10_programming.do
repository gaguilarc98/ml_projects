clear
cd C:\Files\Data
use pobreza.dta
list
bysort pobreza: gen var1 = lugar*frecuencia
preserve
keep if(f>=30)
bysort lugar (pobreza): keep if _n>=2
list
restore
preserve 
collapse (mean) meanx=f (sd) sdx=f (count) mun=f, by(lugar)
list
restore
list
