clear
cd "C:\Files\Data"
import excel senamhi, firstrow case(lower)
rename (temp_min temp_max) (tmin tmax)
keep if tmin>(min(tmax,tmax)-8)
regress tmax tmin
gen categ = cond((tmax-tmin)<6.5,1,0)
replace categ=2 if (tmax-tmin)>7.5
list
xi: regress tmax i.categ
xi: regress tmax tmin i.categ*tmin
egen frec = seq(), f(2) t(4)
xi: regress tmax tmin [fweight=frec]
