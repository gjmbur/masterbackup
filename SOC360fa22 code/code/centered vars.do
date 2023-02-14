sysuse auto, clear
foreach var of varlist price mpg weight rep78 length turn { 
	qui sum `var'
	gen `var'_c = `var' - r(mean)
}
