clear
cd "C:\Files\Data"
import delimited test.dat, delimiters("\t")
list
generate var4=1 in 1
replace var4=2 in 2
replace var4=3 in 3
rename var4 sex
label define s 1 "male" 2 "female"
label values sex s
gen id=_n
list
rename v# time#
/*O también
 forvalues i=1/3{
  rename v`i' time`i'
 }
*/
reshape long time, i(id) j(ocassion)
list
egen dd=mean(time), by(id)
gen d = time-dd
drop dd
list
bysort id: drop if(_n==3 & id==2)
list
