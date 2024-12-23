clear
cd "C:\Files\Data"
import delimited electricity.csv, delimiters(";")
keep if (electricity>2000 & electricity<10000)
generate fecha = date(month,"YMD",1995)
list
generate x = cond(e/f>1,1,0)
egen media = mean(e), by(x)
recode x 1=2 0=1
list e-media
