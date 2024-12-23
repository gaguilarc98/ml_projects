clear
cd "C:\Files\Data"
import excel senamhi, firstrow case(lower) clear
keep if temp_max<295 & temp_min>281.5
drop in 3/20
format temp_min %3.2g
list
