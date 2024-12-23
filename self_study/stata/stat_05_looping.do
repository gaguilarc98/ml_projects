clear
cd "C:\Files\Data"
use pobreza
keep if f>10
keep l-f
sort lugar
by lugar: summarize frecuencia
bysort pobreza: summarize frecuencia
sort lugar pobreza
by lugar: list p-f if _n==_N
foreach variable in l-f {
	list `variable'
}
foreach number of numlist 1 2 3 {
	display `number'
}
forvalues i=1/5 {
	disp `i'
}
local i = 1
while i<=3{
	display `i'
	local i = `i'+1
}
