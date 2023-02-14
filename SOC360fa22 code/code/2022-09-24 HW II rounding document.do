use "/Users/gjmb/Desktop/SOC360fa22/data/coral.dta", clear
foreach var of varlist seasurfacetemperature growth { 
	qui sum `var', d
	gen devs`var' = round(`var' - r(mean), 0.01)
	gen sqrddevs`var' = devs`var'^2
	qui sum sqrddevs`var', d
	scalar s_`var' = sqrt(round(r(sum), 0.01)/5) 
	di s_`var'
}
rename seasurfacetemp temp	
foreach var of varlist temp growth { 
	qui sum `var', d
	gen round_`var'_z = round((`var' - round(r(mean), .01))/round(r(sd), 0.01), 0.01)
}

foreach var of varlist temp growth { 
	qui sum `var', d
	gen `var'_z = (`var' - r(mean))/r(sd)
}
gen z_prods = temp_z*growth_z
gen round_z_prods = round_temp_z*round_growth_z
list z_prods round_z_prods

foreach var of varlist z_prods round_z_prods { 
	qui sum `var', d
	scalar r_`var' = r(sum)/5
	scalar faker_`var' = r(sum)/6
	di r_`var'
	di faker_`var'
}
