clear
cd "C:\Files\Data"
import excel senamhi, firstrow case(lower)
rename temp_min tmin
rename temp_max tmax
gen fecha2 = date(fecha,"YMD"), before(tmin)
drop fecha
rename fecha2 fecha
keep if tmin>(min(tmax,tmax)-8)
list

use pobreza.dta, clear
list
reshape wide frecuencia, i(pobreza) j(lugar)
list
reshape long frecuencia, i(pobreza) j(lugar)
list
preserve
collapse (sum) slugar = frecuencia (sd) sdlugar = frecuencia, by(pobreza)
list
restore
bysort lugar (frecuencia): keep if _n==1
list
