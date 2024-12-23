clear
cd "C:\Files\Data"
use pobreza, clear
keep p-f
drop if(f>=56 | p==2)
replace f=8 if _n==2
list
replace f=9 in 2
list
list f in -2/l
