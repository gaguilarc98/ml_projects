cd "C:\Files\Data"
import delimited electricity.csv, delimiters(";") clear
rename ïmonth month
export delimited electricity.csv, delimiter(";") replace
export excel electricity.xlsx
display date("1995-08-01","YMD")
list
