* Lecture 1. 

	cd ~/desktop/soc360fa22
	use ./original_data/gss2018, clear
	svyset [pweight=wtssall]

	* bivariate reg line example 

	scatter speduc educ, jitter(10) mcolor(red%40) /// 
		|| lfit speduc educ, lcolor(blue%90) legend(off) ///
		ytitle("Spouse's highest education achieved (years)") ///
		xtitle("Number of years of school completed") ///
		title("Relationship between respondent's education and that of their spouse", ///
		size(medium)) note("Source: GSS 2018, {it:n} = 987", size(small)) 
		
	* area under a curve example

	clear all
	set obs 500
	gen day = _n
	gen population = day^2
	dydx p d, gen(derivative)
	line p da, title("Population growth over time", size(medium)) name(pop, replace)
	line deriv day, title("Rate of growth of population", size(medium)) ///
		ytitle("Rate of growth of population (people per day)") name(dydx, replace)
	gr combine pop dydx


	* Aduhelm example: 99 CI 
	
	* LB
	di (0 -  .0054645) - 2.58*(.0054645*(1-.0054645)/536)

	* UB
	di (0 -  .0054645) + 2.58*(.0054645*(1-.0054645)/536)

* Lecture 2. 
	use ./original_data/cps2019, clear
	
	*  Qualitative variable stuff (used CPS since race is better and not worth
	* going into details of GSS on this so early).
	svyset [pweight = fnlwgt]
	set scheme s2color
	
	graph pie, over(wbhaom) title("Distribution of race in the US") ///
		plabel(_all percent, format(%2.0f) gap(20)) ///
		note("Source: CPS 2019, {it:n} = 291,390") saving(races, replace)
	
	graph pie, over(ind_m03) title("Distribution of occupations in the US") ///
		plabel(_all percent, format(%2.0f) gap(20)) ///
		note("Source: CPS 2019, {it:n} = 186,344 ") saving(occs, replace)
		
	graph combine races.gph occs.gph
	
	graph hbar, over(ind_m03,  sort(1) descending) ///
		title("Distribution of occupations in the US") ///
		note("Source: CPS 2019, {it:n} = 186,344 ")
	
	set scheme neon
		
	hist wage1, percent note("Source: CPS 2019, {it:n} = 89,883") ///
		title("Distribution of wages for hourly workers in the US") ///
		xlab(0(10)100)
	
	gen logwages = ln(wage1)
	
	hist logwages, percent note("Source: CPS 2019, {it:n} = 89,883") ///
		title("Distribution of logged wages for hourly workers in the US") ///
		xtitle("Natural logarithm of hourly wage") xlab(-1(1)6)
	
	* Histogram information
	
	set scheme tufte
	
	use ./original_data/gss2018 , clear
	svyset [pweight=wtssall]
	
	hist educ, percent bin(10) note("Source: GSS 2018, {it:n} = 2345") ///
		color(green%30) title("Distribution of education in the US") ///
		xlab(0(2)20)
		
	* Histogram types
	
	use ./original_data/cps2019, clear
	svyset [pweight = fnlwgt]
	
	set scheme tufte 
	set graphics off
	hist wage1, freq color(green%30) name(wagefreq, replace)
	hist wage1, frac color(red%30) name(wagefrac, replace)
	hist wage1, color(eltblue%30) name(wagedens, replace)
	hist wage1, percent color(orange%30) name(wageperc, replace)
	
	set graphics on
	
	graph combine wagefreq wagefrac wagedens wageperc, ///
		title("Various types of histograms showing distribution of wages") ///
		note("Source: CPS 2019, {it:n} = 89,848")
	

	set scheme s2color
	ssc install eqprhistogram
	eqprhistogram wage1, title("Distribution of wages") ///
		xlab(0(5)100, angle(32)) note("Source: CPS 2019, {it:n} = 89,848")
		
	gen catincome = . 
	replace catincome = 1 if wage1 < 20 
	replace catincome = 2 if wage1>20 & wage1 <50
	replace catincome = 3 if wage1>50 & ~missing(wage1)
	label define catinc 1 "low-wage" ///
		2 "medium-wage" 3 "high-wage"
	label values catincome catinc
	
	set scheme tufte
	
	hist wage1, percent color(orange%30) title("Distribution of wages") ///
		name(wageperc2, replace)
	
	graph bar, over(catincome) title("Distribution of wages") ///
		ytitle("Percent") blabel(, size(small)) ///
		note("Low-wage: <$20" "Medium wage: $20-$50" "High wage: >$50") ///
		ylab(0(10)100) bar(1, color(edkblue%30)) name(catinc, replace)
	
	gr combine catinc wageperc2, title("Bar graph vs. histogram") ///
		note("Source: CPS 2019, {it:n} = 89,848")

	* histogram vs. bar graph
	
	use ./original_data/gss2018, clear
	svyset [pweight=wtssall]
	set scheme neon
	graph bar educ, over(paeduc) b1title("Father's education") ///
		ytitle("Respondent's education") ///
		title("Relationship between father's education and child's")
		
	set scheme tufte
	hist relig, percent disc title("Badly-visualized distribution of religion") ///
		xlab(0(2)16) color(red%30) name(badhist, replace) ///
		note("Result of command {stMono:hist relig, percent disc}")
		
	
	use ./original_data/cps2019, clear
	svyset [pweight = fnlwgt]
		
	label define unionmem 0 "not a member" 1 "member" 
	label values union unionmem
	count if ~missing(union) & ~missing(wage1)
	local samplesz = r(N)
	graph bar (p25) wage1 (p50) wage1 (p75) wage1, over(union) /// 
		b1title("Union status", size(medsmall)) ytitle("Hourly wage") title( ///
		"Relationship between r's union status and income", size(medium)) ///
		blab(, angle(32)) note(`"Source: CPS 2019, {it:n} = `samplesz'"') ///
		legend(order(1 "first quartile" 2 "median" 3 "third quartile"))
	
	graph bar, over(wage1) title("Badly-visualized distribution of wages") ///
		bar(1, color(red%30)) name(badbar, replace) ///
		note("Result of command {stMono:graph bar, over(wage1)}")
		
	gr combine badhist badbar
	
	* time-series
	
	import excel using ./original_data/Series_2021-09, sheet("UR") firstrow clear
	rename A date
		* This gives us some good practice remembering our import commands. Here,
		* we use "firstrow" again to get variable names and we use a cell-range 
		* to avoid some extraneous information contained in the top of the file. 

	
	rename AL-WY UR=
		* To add a prefix to a set of variables, we simply put the set of vars
		* as the first input to the command and for the second input, we have
		* [the prefix]=. For a suffix, we'd do the same thing but reversed, i.e.
		* "rename [vars] =[the suffix]". 
		
		* Some stubs might be found inside of the names of variables, in which
		* case, we can use the general syntax that follows:
			* reshape long abc@def, i(id) j(new variable)

	* Finally, we are ready to issue the -reshape- command. Lastly, we need to
	* add the "string" option to j() because our information on states is a 
	* string (the USPS two-character abbreviation). 
	
	reshape long UR, i(date) j(state, string) 
		* Note that the number of observations increases here to 51*548, which is
		* the old number of observations (just dates) times the number of cols.
		* minus one (which is just the ID variable, which stays where it is). 

	* And, for the sake of ease-of-use, let's order the variables so that they 
	* are more rational, with IDs first...
	order date state UR
	
	* And now we can sort in a couple of ways, with either of the two perspectives
	* enumerated above serving as our organizing scheme. 
	sort date state
	
	* Let's also merge these with some other data for more realism. First, save.
	save ./modified_data/state_UR, replace
	
	* ...and also, note in passing that once you've reshaped, as long as you
	* don't then modify your data too much, getting them back into the original
	* shape is pretty easy, too. 
	reshape wide
	
	* We'll just import another sheet from our same Excel file. 
	
	import excel using ./original_data/Series_2021-09, sheet("OTOT") firstrow clear
	* Do a bit of basic data cleaning...
	rename A date
	rename AL-WY inc=
	
	* And now it's time to reshape. Remember, the general syntax is 
		* reshape long [stub name], i(ID variable) j(new variable)
	reshape long inc, i(date) j(state, string) 
	
	merge 1:1 date state using ./modified_data/state_UR
	
	sort date state
	order date state inc UR
	keep if _merge == 3 // Let's only keep merged observations for simplicity.
	drop _merge
	
	* Finally, let's declare that we have time-series data and make some lines.
	* First, let's encode "state". We didn't do this before because the best 
	* way to encode a string requires making a new variable, but Stata can get
	* confused about the shape of your data if you change identifying variables
	* before merging or reshaping. 
	encode state, gen(state_num)
	xtset state_num date, daily
		
	#delimit ; 	
	set scheme tufte; 
	twoway (line UR date if state == "WI", legend(off)
		ytitle("Percent") xtitle("") 
		title("Unemployment rate in Wisconsin, 1976-present")
		note("Source: FRED-SD database, quarterly data, {it:n} = 182")
		xlab(5844(1400)22493, alternate angle(45)) legend(ring(0) pos(11) col(1)))
		(bar UR date if state == "WI", legend(off) color(red%30)
		name(WItsUR, replace));
	
	
	hist UR if state == "WI", percent xlab(0(2)16) color(red%30)
		title("Distribution of unemployment rates in WI, 1976-present")
		note("Source: FRED-SD database, quarterly data, {it:n} = 182")
		name(WImargUR, replace);
		
	gr combine WItsUR WImargUR;
		
	#delimit cr
	
* Lecture 3

	* Median calculation
	
	clear all
	set obs 10
	gen X = runiformint(0, 100)
	list
	replace X = 27 if _n == 2
	list
	gen Y = runiformint(0, 100)
	replace Y = . if _n == 7
	list
	sum X Y, d
	
	* Median vs. mean
	
	clear all 
	set scheme tufte
	set obs 100
	gen Z = rexponential(10)
	qui sum Z, d
	local zmean = r(mean)
	local zmed = r(p50)
	local meancoord =  round((`zmean'+10), .1)
	local medcoord =  round((`zmed'-10), .1)
	set obs 110
	qui sum Z
	replace Z = (r(max))*5 if _n >= 101
	qui sum Z, d
	local zmean2 = r(mean)
	local zmedtwo = r(p50)
	di `zmed'
	di `zmean'
	di `zmed2'
	di `zmean2'
	di `zmedtwo'
	local meancoord2 =  round((`zmean2'+10), .1)
	local medcoord2 =  round((`zmed2'-10), .1)
	
	hist Z, percent color(mint%30) bin(100) ///
		xline(`zmean', lpattern(solid) lcolor(black)) 
		
	twoway /// 
	(hist Z, percent color(mint%30) bin(100) ///
		xline(`zmean', lpattern(solid) lcolor(black)) ///
		xline(`zmed', lp(solid) lcolor(red)) /// 
		xline(`zmean2', lpattern(longdash) lcolor(black)) ///
		xline(`zmedtwo', lp(longdash) lcolor(red)) ///
		title("Histograms of exponential variable with and without extreme outliers", ///
		size(medsmall))) ///
	(hist Z if _n<101, percent color(orange%20) bin(20) xlab(0(100)200) ///
		text(20 200 "originals: solid lines" "new distribution: dashed" ///
		"medians: red" "means: black", box fcolor(white)) ///
		legend(order(2 "baseline" 1 "with extreme outliers")))
	
	set obs 101
	qui sum Z
	replace Z = (r(max))*5 if _n == 101
	qui sum Z, d
	local zmean2 = r(mean)
	local zmedtwo = r(p50)
	di `zmed'
	di `zmean'
	di `zmed2'
	di `zmean2'
	di `zmedtwo'
	local meancoord2 =  round((`zmean2'+5), .1)
	local medcoord2 =  round((`zmed2'-5), .1)
	 
	hist Z, percent color(edkblue%30) ///
		xline(`zmean2', lpattern(longdash) lcolor(black)) ///
		xline(`zmedtwo', lp(solid) lcolor(red)) ///
		text(12 `meancoord2' "new mean") text(18 `medcoord2' "new median") ///
		title("Histogram of an exponentially-distributed variable with major new outlier") ///
		name(skew, replace) bin(9)
		
	graph combine noskew skew, cols(1)  
		
	twoway (hist Z if _n<1001, percent color(mint%30) ///
		xline(`zmean', lpattern(longdash) lcolor(black)) ///
		xline(`zmed', lp(solid) lcolor(red)) /// 
		text(12 `meancoord' "original mean") text(18 `medcoord' "original median")) ///
	(hist Z, percent color(ltblue%30) ///
		xline(`zmean2', lpattern(longdash) lcolor(black)) ///
		xline(`zmedtwo', lp(solid) lcolor(red)) ///
		text(40 `meancoord2' "new mean") text(60 `medcoord2' "new median"))
	
	* General histogram stuff.
	
	/* twoway (hist educ, percent discrete color(mint%30) ///
		xline(`edmean', lpattern(longdash) lcolor(mint)) ///
		xline(`edmed', lp(solid) lcolor(mint))) ///
		(hist yearsjob, percent color(orange%20) ///
		xline(`tenuremean', lpattern(longdash) lcolor(orange)) ///
		xline(`tenuremed', lp(solid) lcolor(orange)) ///
		title("Time spent in various institutions") legend( ///
		order(1 "education" 2 "current job")) ///
		text(30 40 "Solid: median" "Dashed: mean", box fcolor(white))) ///
		name( */ 
	
	use ./original_data/gss2018, clear
	svyset [pweight=wtssall]
	set scheme lean2
	qui sum wordsum, d
	local wordmean = round(r(mean), .1)
	local wordmed = round(r(p50), .1)
	local nvocab = r(N)
	qui sum yearsjob, d
	local tenuremean = round(r(mean), .1)
	local tenuremed = round(r(p50), .1)
	local ntenure = r(N)
	
	hist yearsjob, percent color(mint%30) ///
		xline(`tenuremean', lpattern(longdash) lcolor(mint)) ///
		xline(`tenuremed', lp(solid) lcolor(mint)) ///
		title("Time spent at current job") ///
		text(30 40 "Solid: median" "Dashed: mean", box fcolor(white)) ///
		note(`"Source: GSS 2018, {it:n} = `ntenure'"' "mean: `tenuremean'" ///
		"median: `tenuremed'") name(tenurehist, replace)
	
	qui sum wordsum, d 
	hist wordsum, percent discrete color(orange%30) ///
		xline(`wordmean', lpattern(longdash) lcolor(orange)) ///
		xline(`wordmed', lp(solid) lcolor(orange)) ///
		title("Score on a 10-question vocabulary test") ///
		text(17 2 "Solid: median" "Dashed: mean", box fcolor(white)) ///
		note(`"Source: GSS 2018, {it:n} = `nvocab'"' "mean: `wordmean'" ///
		"median: `wordmed'") name(wordsumhist, replace)
		
	di `tenuremean' 
	di `tenuremed' 
	di `wordmean' 
	di `wordmed'
		
	gr combine tenurehist wordsumhist
	
	* quantile shown
	ssc install cdfplot
	set scheme tufte
	qui sum height, d
	local q1 = r(p25)
	local q2 = r(p50)
	local q3 = r(p75)
	local nheight = r(N)
	cdfplot height, xline(`q1', lpattern(longdash) lcolor(orange)) ///
		xline(`q2', lpattern(longdash) lcolor(red)) ///
		xline(`q3', lpattern(longdash) lcolor(green)) ylab(0(.1)1) ///
		ytitle("Cumulative probability") xtitle("Height of respondent") ///
		title("Cumulative distribution of height in US") ///
		note(`"Source: GSS 2018, {it:n} = `nheight'"') ///
		yline(.25, lpattern(longdash) lcolor(orange)) ///
		yline(.50, lpattern(longdash) lcolor(red)) ///
		yline(.75, lpattern(longdash) lcolor(green)) xlab(50(5)80) ///
		text(.9 55 "Orange: Q1" "Red: Q2" "Green: Q3", box fcolor(white))
	sum height, d
	gen stdheight = (height - r(mean))/r(sd)
	local myheight = 74
	qui sum height, d
	local n = r(N)
	local zscore = (74-r(mean))/r(sd)
	di `zscore'
	count if height > 74 & ~missing(height)
	local greater = r(N)
	local pval = 1-(`greater'/`n')
	count if stdheight>`zscore' & ~missing(stdheight)
	local pval2 = 1 - (r(N)/`n')
	
	cdfplot height, xline(`myheight', lpattern(longdash) lcolor(orange)) ///
		ylab(0(.1)1) ///
		ytitle("Cumulative probability") xtitle("Height of respondent") ///
		title("Cumulative distribution of height in US", size(medium)) ///
		yline(`pval', lpattern(longdash) lcolor(orange)) xlab(50(5)80) ///
		text(.9 60 "Orange line: my height", box fcolor(white)) ///
		name(heightunstd, replace)
	
	cdfplot stdheight, xline(`zscore', lpattern(longdash) lcolor(red)) ///
		ytitle("Cumulative probability") xtitle("Standardized height") ///
		title("Cumulative distribution of standardized height in US", ///
			size(medium)) ///
		yline(`pval2', lpattern(longdash) lcolor(red)) ///
		text(.9 -2 "Red line: my {it:z}-score", box fcolor(white)) ///
		name(heightstd, replace)
		
	gr combine heightunstd heightstd, ///
		note(`"Source: GSS 2018, {it:n} = `n'"') 
	
	use ./data/cps2019, clear
	qui sum wage1, d
	local q1 = r(p25)
	local q2 = r(p50)
	local q3 = r(p75)
	local min = r(min)
	local max = r(max)
	local n = r(N)
	cdfplot wage1, xline(`q1', lpattern(longdash) lcolor(orange)) ///
		xline(`q2', lpattern(longdash) lcolor(red)) ///
		xline(`q3', lpattern(longdash) lcolor(green)) ylab(0(.1)1) ///
		ytitle("Cumulative probability") xtitle("Wage of respondent") ///
		title("Cumulative distribution of wages in US") ///
		note("Source: CPS 2019, {it:n} = `n'") ///
		yline(.25, lpattern(longdash) lcolor(orange)) ///
		yline(.50, lpattern(longdash) lcolor(red)) ///
		yline(.75, lpattern(longdash) lcolor(green)) xlab(`min'(5)`max') ///
		text(.9 55 "Orange: Q1" "Red: Q2" "Green: Q3", box fcolor(white))
	
		 
	* Box-plots
	set scheme tufte
	sum yearsjob, d
	local q1 = r(p25)
	local q2 = r(p50)
	local q3 = r(p75)
	local IQR = `q3' - `q1'
	local upperwhisker = (1.5*`IQR')+`q3'
	local ntenure = r(N)
	graph box yearsjob, box(1, color(cyan)) marker(1, mcolor(cyan)) ///
		title("Distribution of job tenure") ylab(0(10)60) name(box1, replace) 
	graph box yearsjob, box(1, color(cyan)) marker(1, mcolor(cyan)) ///
		title("Distribution of job tenure") ///
		ylab(0(10)60) ///
		yline(`q1', lpattern(longdash) lcolor(black)) ///
		yline(`q2', lpattern(longdash) lcolor(black)) ///
		yline(`q3', lpattern(longdash) lcolor(black)) ///
		yline(`upperwhisker', lpattern(longdash) lcolor(black)) ///
		name(boxannotated, replace)
	gr combine box1 boxannotated, note(`"Source: GSS 2018, {it:n} = `ntenure'"')
	
	
	graph box yearsjob, box(1, color(cyan)) marker(1, mcolor(cyan)) ///
		title("Distribution of job tenure", size(medium)) ylab(0(10)60) name(box1, replace) 
	sum yearsjob, d
	local q1 = r(p25)
	local q2 = r(p50)
	local q3 = r(p75)
	local IQR = `q3' - `q1'
	local upperwhisker = (1.5*`IQR')+`q3'
	local ntenure = r(N)
	graph box yearsjob, box(1, color(cyan)) marker(1, mcolor(cyan)) ///
		title("Distribution of job tenure [annotated box]", size(medium)) ///
		ylab(0(10)60) ///
		text(`q1' 10 "Q1") text(`q2' 10 "Q2") text(`q3' 10 "Q3") ///
		text(`upperwhisker' 10 "(1.5 * IQR)+Q3") ///
		marker(10, 
		note("NB that Stata uses minima and maxima for location of whiskers" ///
		"if those are larger (smaller) than the formula suggests") ///
		name(boxannotated, replace)
	gr combine box1 boxannotated, note(`"Source: GSS 2018, {it:n} = `ntenure'"')
	
	* outliers
	use ~/desktop/soc365sp22/original_data/cps2019, clear
	gen random = runiform()
	sort random
	keep if _n <5000
	drop if hourslw>150
	sum hourslw, d
	local q1 = r(p25)
	local q2 = r(p50)
	local q3 = r(p75)
	local IQR = `q3' - `q1'
	local upperwhisker = (1.5*`IQR')+`q3'
	local n = r(N)
	di `n'
	local wrkwk = 40
	local hrsweek = (24*7)
	local physmax = (16*7)
	local schormax = 70
	local wkcoord = `wrkwk' + 5
	local physcoord = `physmax' + 5
	local schorcoord = `schormax' + 5
	svyset [pweight = fnlwgt]
	graph box hourslw, box(1, color(cyan)) marker(1, mcolor(cyan)) ///
	title("Distribution of weekly hours worked", size(medium)) ///
		ylab(0(10)120) yline(`wrkwk', lpa(longdash) lcol(black)) ///
		 yline(`schormax', lpa(longdash) lcol(orange)) ///
		 yline(`physmax', lpa(longdash) lcol(red)) ///
		 note(`"Source: random subsample of CPS 2019, {it:n} = `n'"') ///
		 text(`wkcoord' 8 "standard work-week", size(small)) ///
		 text(`physcoord' 10 "physiological maximum", size(small)) ///
		 text(`schorcoord' 14 "likely human history maximum" "(Schor 1991)", size(small))
	
	qui sum wage1 if wage1>=7.25, d
	local nwage = r(N)
	qnorm wage1 if wage1 >=7.25, mcolor(red) ///
		grid title("Quantile-quantile plot of wages with Normal line for reference", ///
		size(medium)) name(qnormwage, replace) ytitle("Quantiles of actual data") ///
		xtitle("Quantiles if data were Normal") note( ///
		`"Source: random subsample of CPS 2019, {it:n} = `nwage'"' ///
		"Earners making below minimum wage omitted" ///
		"Grid lines are 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, and 0.95 quantiles")
		
	qui sum hourslw, d
	local nhours = r(N)
	local schormax = 70
	qnorm hourslw if hourslw < `schormax', mcolor(red) grid ///
		title("Quantile-quantile plot with Normal line for reference", ///
		size(medium)) name(qnormhours, replace) ///
		xtitle("Quantiles if data were Normal") ytitle("Quantiles of actual data")
	
	gr combine qnormwage qnormhours, note( ///
	"Source: random subsample of CPS 2019" ///
	`"{it:n} = `nwage' and `hours', respectively"')
	
	

* Lecture four
	
	* CDF/PMF
	use ./original_data/gss2018, clear
	svyset [pweight=wtssall]
	set scheme tufte
	qui sum childs, d
	local max = r(max)
	local n = r(N)
	cdfplot childs, xlab(0(1)`max') ///
		ytitle("Cumulative probability") ///
		xtitle("Number of children {it:r} has") ///
		title("Cumulative distribution of children in US", size(medium)) ///
		name(cdfkids, replace)
		
	hist childs, frac bin(`max') xlab(0(1)`max') ylab(0.(0.05).3) ///
		ytitle("Probability") xtitle("Number of children {it:r} has") ///
		title("Probability of having {it:x} children in US", size(medium)) ///
		color(mint%30) name(pmfkids, replace)
	
	gr combine cdfkids pmfkids, note(`"Source: GSS 2018, {it:n} = `n'"') ///
		title("CDF and PMF (estimated) of number of children in the US, 2018" ///
		, size(medium))
	
	* CDF/PDF
		
	qui sum height, d
	local max = r(max)
	local min = r(min)
	local n = r(N)
	cdfplot height, normal xlab(`min'(5)`max') ///
		ytitle("Cumulative probability") ///
		xtitle("{it:r}'s height") ///
		title("Cumulative distribution of height in US", size(medium)) ///
		name(cdfheight, replace)
		
	kdensity height, normal xlab(`min'(5)`max') ///
		xtitle("{it:r}'s height") ///
		title("Estimate of the PDF of height in US", size(medium)) ///
		color(mint%80) name(pdfheight, replace) note("")
	
	gr combine cdfheight pdfheight, note(`"Source: GSS 2018, {it:n} = `n'"') ///
		title("CDF and PDF (estimated) of height in the US, 2018" ///
		, size(medium))
	
	* Normal
	set scheme lean2
	twoway function y=normalden(x), range(-4 4) ytitle("Density") ///
		title("Standard Normal") xtitle("{it:z}")
	
	* Normal areas
	twoway function y=normalden(x), range(-4 4) ytitle("Density") || ///
	function y=normalden(x), range(-1 1) recast(area) color(red%70) || ///
	function y=normalden(x), range(-1.96 -1) recast(area) color(red%50) || ///
	function y=normalden(x), range(1 1.95) recast(area) color(red%50) || ///
	function y=normalden(x), range(1.96 3) recast(area) color(red%30) || ///
	function y=normalden(x), range(-3 -1.96) recast(area) color(red%30) ///
	xtitle("{it:z}") ///
	ytitle("Density") title("Critial Values for Standard Normal") ///
	subtitle("Two-tailed test and {&alpha}=.05") ///
	legend(off) xlabel(-1.96 0 1.96)

	
	twoway function y=normalden(x), range(-4 4) ytitle("Density") || ///
	function y=normalden(x), range(-1 1) recast(area) color(red%80) || ///
	function y=normalden(x), range(1 1.96) recast(area) color(red%40) || ///
	function y=normalden(x), range(-1.96 -1) recast(area) color(red%40) || ///
	function y=normalden(x), range(-3 -1.96) recast(area) color(red%20) || ///
	function y=normalden(x), range(1.96 3) recast(area) color(red%20) ///
		legend(col(2) order(2 "Out to here = 68%" 3 "Out to here = 95%" 5 ///
		"Out to here = 99.7%")) ///
		xtitle("{it: Z}") ytitle("Density") ///
		title("Areas under Standard Normal") ///
		xlab(-4 -3 -1.96 -1 0 1 1.96 3 4)
		
	* arbitrary Normal
	clear all
	set obs 10000
	gen X = rnormal(9, 15)
	qui sum X, d
	local sigma = r(sd)
	local mu = r(mean)
	local rangemin = `mu'-(4*`sigma')
	local rangemax = `mu'+(4*`sigma')
	twoway function y=normalden(X), range(`rangemin' `rangemax')
	+`mu')) ytitle("Density") || ///
	function y=normalden(x), range(-1 1) recast(area) color(red%80) || ///
	function y=normalden(x), range(1 1.96) recast(area) color(red%40) || ///
	function y=normalden(x), range(-1.96 -1) recast(area) color(red%40) || ///
	function y=normalden(x), range(-3 -1.96) recast(area) color(red%20) || ///
	function y=normalden(x), range(1.96 3) recast(area) color(red%20) ///
		legend(col(2) order(2 "Out to here = 68%" 3 "Out to here = 95%" 5 ///
		"Out to here = 99.7%")) ///
		xtitle("{it: x}") ytitle("Density") ///
		title("Areas under Standard Normal") ///
		xlab(-4 -3 -1.96 -1 0 1 1.96 3 4)
		
	* Normal in practice
	set scheme tufte
	local mu = 9
	local sigma = 15
	local rangemin = `mu' - (4*`sigma')
	local rangemax = `mu' + (4*`sigma')
	local twosigma = (`sigma'*1.96)
	local LB95 = `mu' - `twosigma'
	local UB95 = `mu' + `twosigma'
	local xcoordtxtL = `LB95' -`sigma'
	local xcoordtxtR = `UB95' +`sigma'
	twoway function y=normalden(x, 9, 15), range(`rangemin' `rangemax') ///
		xlab(`rangemin'(`sigma')`rangemax') xline(`mu', lpattern(longdash)) ///
		ytitle("Density") xline(`LB95', lpattern(dash)) ///
		xline(`UB95', lpattern(dash)) || ///
	function y=normalden(x, 9, 15), range(`LB95' `UB95') ///
		recast(area) color(red%80) legend(order(2 "Middle 95 percent")) ///
		text(0.022 `xcoordtxtL' "9-(1.96*15) {&rarr}" "=-20.4") ///
		text(0.022 `xcoordtxtR' " {&larr}9+(1.96*15)" "=38.4") ///
		title("All Normal curves follow the 68-95-99.7 rule", size(medium)) ///
		note("{&mu}=9, {&sigma}=15", size(medium))

	* CDF/PDF AUC
	clear all
	set obs 100000
	gen zeta = rnormal()
	qui sum zeta, d
	local max = r(max)
	local min = r(min)
	local n = r(N)
	local mu = r(mean)
	local sigma = r(sd)
	local twosigma = (`sigma'*1.96)
	local LB95 = `mu' - `twosigma'
	local UB95 = `mu' + `twosigma'
	local xcoordtxtL = `LB95' -`sigma'
	local xcoordtxtR = `UB95' +`sigma'
	cdfplot zeta, normal xlab(-4(1)4) ///
		xline(`LB95', lpattern(longdash) lcolor(red)) /// 
		xline(`UB95',  lpattern(longdash) lcolor(red)) ///
		ytitle("Cumulative probability") ///
		xtitle("{it:Z}") ylab(0.025 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 0.975 1) ///
		title("CDF of a Normally-distributed random variable {it:Z}", size(medium)) ///
		note("{&mu}=0, {&sigma}=1", size(medium)) ///
		yline(0.025, lpattern(longdash) lcolor(red)) yline(0.975, ///
			lpattern(longdash) lcolor(red)) name(cdfnormal, replace) legend(off) ///
		text(0.75 `xcoordtxtL' "-1.96 {&rarr}") ///
		text(0.75 `xcoordtxtR' "{&larr} 1.96")
		
	twoway kdensity zeta || function y=normalden(x), range(-1.96 1.96) ///
		recast(area) color(red%80) note("{&mu}=0, {&sigma}=1", size(medium)) ///
		ytitle("Density") ///
		xtitle("{it:Z}") ///
		xline(`LB95', lpattern(longdash) lcolor(red)) /// 
		xline(`UB95',  lpattern(longdash) lcolor(red)) ///
		title("PDF of a Normally-distributed random variable {it:Z}", size(medium)) ///
		name(pdfnormal, replace) legend(order(2 "middle 95 percent")) ///
		text(0.35 `xcoordtxtL' "-1.96 {&rarr}") ///
		text(0.35 `xcoordtxtR' "{&larr} 1.96")
		
		
	gr combine cdfnormal pdfnormal, ///
		title("Difference in heights of CDF between {it:Z} = c & {it:Z} = d" ///
		"...is equal to area under curve of PDF between {it:Z} = c & {it:Z} = d" ///
		, size(medium))
		
	* height e.g. 
	set scheme tufte
	use ./original_data/gss2018, clear
	svyset [pweight=wtssall]
	sum height, d
	local n = r(N)
	local mean = r(mean)
	local sd = r(sd)
	local score = 74
	local zscore = (`score' - `mean')/`sd'
	local unstdxcoord = `score' + 6
	local stdxcoord = `zscore' + 1.5
	gen stdheight = (height - `mean')/(`sd')
	kdensity height, nograph gen(kdhtX kdhtY)
	kdensity stdheight, nograph gen(stdkdhtX stdkdhtY)
	gen zero = 0 
	twoway kdensity height, xline(`score', lpattern(longdash) lcolor(green)) || ///
		rarea kdhtY zero kdhtX if kdhtX >=74, sort ///
		name(unstdheightAUC, replace) legend(off) color(green%30) ///
		xtitle("{it:r}'s height") ytitle("Density") ///
		title("Area under estimated density curve" "for unstandardized height", ///
		size(medium)) text(0.09 `unstdxcoord'  "{&larr} 74 inches")
	twoway kdensity stdheight, xline(`zscore', lpattern(longdash) lcolor(red)) || ///
		rarea stdkdhtY zero stdkdhtX if stdkdhtX >=1.65, sort ///
		name(stdheightAUC, replace) legend(off) ///
		color(red%30) xtitle("{it:r}'s height") ytitle("Density") ///
		title("Area under estimated density curve" "for standardized height", ///
		size(medium)) text(0.35 `stdxcoord'  "{&larr} {it:z} = 1.65")
	gr combine unstdheightAUC stdheightAUC, ///
		note(`"Source: GSS 2018, {it:n} = `n'"') 
	
	count if height>74 & ~missing(height)
	local tall = r(N)
	di `tall'/`n'
	
	* MCAT
	set scheme tufte
	local mu = 500
	local sigma = 10.6
	local rangemin = `mu' - (4*`sigma')
	local rangemax = `mu' + (4*`sigma')
	local criticalvalue = -(invnorm(0.1))
	local LB80 = round(`mu' - `criticalvalue'*`sigma', .1)
	local UB80 = round(`mu' + `criticalvalue'*`sigma', .1)
	local xcoordL = round(`LB80' -`sigma', .1)
	local xcoordR = round(`UB80' +`sigma', .1)
	twoway function y=normalden(x, 500, 10.6), range(`rangemin' `rangemax') ///
		xlab(`rangemin'(`sigma')`rangemax') xline(`mu', lpattern(longdash)) ///
		ytitle("Density") xline(`LB80', lpattern(dash)) ///
		xline(`UB80', lpattern(dash)) || ///
	function y=normalden(x, 500, 10.6), range(`LB80' `UB80') ///
		recast(area) color(red%80) legend(order(2 "Middle 80 percent")) ///
		text(0.03 `xcoordL' `"`LB80' {&rarr}"') ///
		text(0.03 `xcoordR' `"{&larr} `UB80'"') ///
		title("Middle 80 percent of the distribution of MCAT scores", size(medium)) ///
		note(`"{&mu}=`mu', {&sigma}=`sigma'"', size(medium))
		
	
	* violin plots
	count if ~missing(height) & ~missing(sex) & sex == 1
	local nm = r(N)
	count if ~missing(educ) & ~missing(sex) & sex == 2
	local nf = r(N)
	vioplot height, over(sex) ///
		note("Source: GSS2018" `"{it:n}{subscript: men} = `nm'"' ///
		`"{it:n}{subscript: women} = `nf'"') title( ///
		"Distribution of height in the US by biological sex") ///
		ytitle("Height (inches)") density(fcolor(green%80))
		
	* Comparing box-plots
	
	clear all
	use ./data/NHANES_merged2, clear
	set scheme tufte
	gen adult = ridageyr>17
	label define old 0 "child (0-18)" 1 "adult (18+)"
	label values adult old
	forvalues num = 0/1 {
		qui sum bmxht if adult == `num'
		local nadult`num' = r(N)
		}
	graph hbox bmxht, over(adult) box(1, color(cyan%50)) ///
		box(2, color(red%50)) asyvars ///
		legend(ring(0) bplacement(swest) region(ls(longdash))) ///
		note("Source: NHANES 2017-18" ///
		`"{it:n}{subscript:child} = `nadult0'"' ///
		`"{it:n}{subscript:adult} = `nadult1'"') ///
		title("Distribution of height among children and adults, US") 
	forvalues num = 0/1 {
		qui sum bmxht if adult == `num'
		local nadult`num' = r(N)
		}
	twoway (hist bmxht if adult == 0, color(cyan%30)) (hist bmxht if adult ==1, ///
		color(red%30) legend(order(1 "kids" 2 "adults")) ///
		legend(ring(0) region(ls(longdash)) bplacement(nwest)) ///
		note("Source: NHANES 2017-18" ///
		`"{it:n}{subscript:child} = `nadult0'"' ///
		`"{it:n}{subscript:adult} = `nadult1'"') ///
		title("Distribution of height among children and adults, US")) 


* Lecture 5
	
	* Transposed scatterplots
	
	cd ~/desktop/SOC360fa22
	view browse https://tinyurl.com/NHANES365i 
	
	import sasxport5 ./data/DEMO_J.xpt, clear
	
		* It can be hard to remember where we got stuff if we plan to rename
		* the data-set to a better name and ditch the original file, so let's
		* do that now (this is not just a formality: I pulled these data a few
		* months ago, and I remembered that I did this while I was prepping the
		* lecture notes...and forgot exactly where I got the data and spent 15 
		* minutes tracking the exact source down :( . 
		
		label data "NHANES 2017-18 Demographic Data" 
		
		note: Original source is CDC website, SAS XPORT file DEMO_J.xpt ///
			https://tinyurl.com/NHANES365i 
		
	* Let's also manually make a source variable as this will help us later. 
	gen from_demog = 1
		 
	* And keep just a few variables for simplicity 
	keep seqn riagendr ridageyr from_demog wt*
	
	d, short 
		* Let's check out the matrix of data. Look like we have a matrix of
		* dimensions 9254x4 (9,254 rows and four columns). 
	save ./data/NHANES17_demog_short, replace
	* That looks good. So, now let's get the measurement data-set loaded. 
	
	view browse https://tinyurl.com/NHANES365ii
	
	import sasxport5 ./data/DXX_J.xpt, clear
	
		label da "NHANES 2017-18 Dual-Energy X-ray Absorptiometry - Whole Body" 
	
		note: Original source is CDC website, SAS XPORT file DXX_J.xpt at ///
		https://tinyurl.com/NHANES365ii
		
	* Let's again manually make a source variable. 
	gen from_xray = 1
	save ./data/NHANES_2017_whole_body_xray, replace
	
	* Let's verify that "seqn" identifies obs. uniquely here, too.
	
	isid seqn
	duplicates report seqn
	scalar exam_N = r(unique_value) 
		* OK, so, we have fewer observations, but that may not be problematic and
		* we're in the same ballpark; we would expect fewer people to have full
		* body exam. data than to have basic demographic data. Let's finally 
		* examine the dimensions of our data matrix and then try the merge.
	
	* And again just drop most variables
	keep seqn dxdtopf from_xray dxdtofat dxdtole
	save ./data/NHANES17_xray_short, replace
	
	d, short // Note that we have a 5114x3 matrix this time. 
		* So, we should expect a merger with a matrix of dim. 9254x4 to result
		* in a matrix with no more than 9254 rows and seven columns (you might
		* expect six since one variable must be shared, but -merge- automatically
		* makes a "_merge" variable). 
	browse // Sometimes it is helpful to see this to get a tactile sense.
	
	merge 1:1 seqn using ./data/NHANES17_demog_short
		* OK, great! We have matches on about half the IDs. 
		* Note that all unmatched people came from the "using" data-set, indicating
		* that everyone who was found in the body examination set had their basic
		* demographic variables collected, but the reverse was not true. 
	save ./data/NHANES17_temp
	
	* We can also check out who might have been absent in one set vs the other. 
	tab _merge, sum(ridageyr) nost nofre
		* Looks like the mean age in the basic data-set is significantly older. 
	
	* And of course, it is helpful to check the dimensions of the matrix we 
	* made and then examine the data directly. 
	d, short 
	br
	
	* Let's just briefly enjoy the fruits of our labor and do something fun. 
	* Content warning for discussion of weight/body-fat. 
	
	gen sex = riagendr-1 // recode to conventional dummy var. 
		label define sx 0 "male" 1 "female" // make value label
		label values sex sx // apply 
	rename dxdtopf percentfat // convenience 
	svyset [pweight=wtint2yr]
	
	import sasxport5 ./data/BMX_J, clear

	merge 1:1 seqn using ./data/NHANES17_temp, nogen
	save ./data/NHANES_merged2, replace
	
	clear all
	use ./data/NHANES_merged2, clear
	set scheme tufte
	gen labelvar = (bmxht>180 & bmxleg>=54) 
	replace labelvar = 2 if (bmxht<115 & bmxleg <30)
	label define lv 0 "" 1 "outlier 1" 2 "outlier 2"
	label values labelvar lv
	sum bmxht bmxleg if ~missing(bmxht) & ~missing(bmxleg), d
	local n = r(N)
	scatter bmxht bmxleg, mcolor(mint%30) name(htvarm, replace) ///
		xtitle("Leg length (cm)") ytitle("Standing height (cm)") ///
		title("Relationship between height and leg length", size(medium)) ///
		|| scatter bmxht bmxleg if (labelvar==1 | labelvar ==2), ///
		mlabel(labelvar) mlabpos(9) legend(off)
	scatter bmxleg bmxht, mcolor(mint%30) name(armvht, replace) ///
		xtitle("Standing height (cm)") ytitle("Leg length (cm)") ///
		title("Relationship between height and leg length", size(medium)) ///
		|| scatter bmxleg bmxht if (labelvar==1 | labelvar ==2), ///
		mlabel(labelvar) mlabpos(9) legend(off)
		
	gr combine htvarm armvht, note(`"Source: NHANES 2017-18, {it:n} = `n'"')
	
	
	* Basic scatterplot
	
	qui sum bmxarml bmxleg if ~missing(bmxarml) & ~missing(bmxleg), d
	local n = r(N)
	scatter bmxarml bmxleg, mcolor(mint%30) name(armvleg, replace) ///
		xtitle("Arm length (cm)") ytitle("Leg length (cm)") ///
		title("Relationship between arm and leg length", size(medium)) ///
		note(`"Source: NHANES 2017-18, {it:n} = `n'"') 
	
	* Trickier scatterplot
	qui sum bmxarmc bmxarml if ~missing(bmxarmc) & ~missing(bmxarml), d
	local n = r(N)
	scatter bmxarmc bmxarml, mcolor(mint%30) name(armlvcirc, replace) ///
		xtitle("Arm circumference (cm)") ytitle("Arm length (cm)") ///
		title("Relationship between arm circumference and length", size(medium)) ///
		note(`"Source: NHANES 2017-18, {it:n} = `n'"') 
		
	* Outliers
	use ~/desktop/SOC365sp22/original_data/cps2019, clear
	gen random = runiform(0, 1)
	keep if random<0.25
	sum weekpay if ~missing(hourslw) & ~missing(weekpay)
	local n = r(N)
	scatter weekpay hourslw, mcolor(green%30) ///
		title("Relationship between hours worked and weekly pay") ///
		note(`"Source: CPS 2019, random subsample. {it:n} = `n'"') ///
		ytitle("Weekly income ($)")
	
	qui sum weekpay, d
	local LBwp = r(p25) - (1.5*(r(p75)-r(p25)))
	local UBwp = r(p75) + (1.5*(r(p75)-r(p25)))
	gen outlier = (weekpay>`UBwp' & ~missing(weekpay)) | (weekpay<`LBwp')
	qui sum hourslw, d
	local LBh = r(p25) - (1.5*(r(p75)-r(p25)))
	local UBh = r(p75) + (1.5*(r(p75)-r(p25)))
	replace outlier = 1 if (hourslw>`UBh' & ~missing(`UBh')) | (hourslw<`LBh')
	count if outlier == 0 & ~missing(hourslw) & ~missing(weekpay)
	local n = r(N)
	
	scatter weekpay hourslw if outlier == 0, mcolor(green%30) ///
		title("Relationship between hours worked and weekly pay") ///
		note(`"Source: CPS 2019, random subsample. {it:n} = `n'"' ///
		"outliers removed using Tukey's rule") ///
		ytitle("Weekly income ($)")
	
	corr weekpay hourslw 
	corr weekpay hourslw if outlier == 0
	
	* jitter
	use ~/desktop/SOC365sp22/original_data/gss2018, clear
	count if ~missing(educ) & ~missing(speduc)
	local n = r(N)
	scatter speduc educ, mcolor(green%50) ///
		title("Relation between {it:r}'s education and spouse's", ///
		size(medium)) ///
		ytitle("Spouse's education (years)") legend(off) name(dottysp, replace)
	scatter speduc educ, jitter(5) mcolor(green%50) ///
		title("Relation between {it:r}'s education and spouse's", ///
		size(medium)) ///
		ytitle("Spouse's education (years)") legend(off) name(bettersp, replace)
		
	gr combine dottysp bettersp, title("Better and worse scatterplots") ///
		note(`"Source: GSS 2018, {it:n} = `n'"') 
	
	* contour 
	kdens2 speduc educ, levels(20) ///
		title("Relation between {it:r}'s education and spouse's", ///
		size(medium)) ytitle("Spouse's education (years)")
		
	kdens2 educ paeduc, levels(20) ///
		title("Relation between {it:r}'s education and their fathers's", ///
		size(medium)) ytitle("R's education (years)")
	
	* crude multivariate analysis
	count if ~missing(educ) & ~missing(speduc) & ~missing(sex) & sex == 1
	local nm = r(N)
	count if ~missing(educ) & ~missing(speduc) & ~missing(sex) & sex == 2
	local nf = r(N)
	twoway (scatter educ paeduc if sex == 1, jitter(10) mcolor(red%50)) ///
		(scatter educ paeduc if sex == 2, jitter(10) mcolor(green%50) ///
		title("Relation between father's education and {it:r}'s", ///
		size(medium)) ytitle("respondent's education (years)")) ///
		(lfit speduc educ if race == 1) (lfit speduc educ if race == 2,  ///
		legend(order(1 "men" 2 "women" 3 "men" 4 "women") cols(2)) ///
		xtitle("Father's education") ///
		note("Source: GSS2018" `"{it:n}{subscript: men} = `nm'"' ///
		`"{it:n}{subscript: women} = `nf'"'))
		
	reg educ c.paeduc##i.sex // no actual effect of gender
	
	* correlation
	clear all 
	set obs 1000
	set seed 4823
	gen A = runiform(0, 1000)
	gen B = A + rnormal(0, 100)
	gen C = A + rnormal(0, 500)
	gen D = runiform(0, 1000)
	corr A B C D 
	
	/* foreach var1 of varlist A-D { 
		foreach var2 of varlist A-D {
			qui corr `var1' `var2'
			local `var1'_`var2'_r = round(r(rho), 0.1)
		}
	} 
	graph matrix A B C D, msymbol(Oh) mcolor(red%50) ///
		diagonal(`"{it:r} = `A_B_r'"' `"{it:r}{subscript:AB} = `A_B_r'"' ///
		`"{it:r}{subscript:AC} = `A_C_r'"' ///
		`"{it:r}{subscript: BC} = `B_C_r'"') */
	
	graph matrix A B C D, msymbol(Oh) mcolor(red%50) half ///
		title("Scatterplots of randomly-generated variables") ///
		note("{it:n}=1000") maxes(xla(,angle(32)))
		
	* homework 4.10
	use coral, clear
	rename seasurfacetemperature temp
	sum
	foreach var of varlist temp growth {
		qui sum `var', d
		gen std`var' = (`var' - r(mean))/r(sd)
		}
	gen zproducts = stdtemp*stdgrowth
	list zproducts
	sum zproducts, d
	di r(sum)/(r(N)-1)
	corr temp growth
	gen zprodcheck = round(zprod, 0.01)
	list zprodcheck
	sum zprodc, d
	di r(sum)/(r(N)-1)
	
	* correlation derivation
	use ./data/randomsmalldata, clear
	scatter Y X, xlab(0(1)10) ylab(0(1)10) title("Random small data-set") ///
		note("{it:n} =10") mcolor(red%80)
	gen prod = X*Y
	list prod
	gen Xplusa = X + 2
	gen Yplusb = Y + 1
	gen OGprod = Xplusa*Yplusb
	sum OGprod
	di r(sum)/9
	twoway (scatter Yplusb Xplusa, xlab(0(1)10) ylab(0(1)10) ///
		title("Random small data-sets 1 and 2 ") ///
		note("{it:n} =20") mcolor(green%80)) ///
		(scatter Y X, xlab(0(1)10) ylab(0(1)10) mcolor(red%80) ///
		legend(order(2 "Original data" 1 "Arbitrary constants added")) ///
		xtitle("X") ytitle("Y"))
	foreach var of varlist X Y Xplusa Yplusb { 
		qui sum `var'
		gen `var'dev = `var' - r(mean)
	}
	gen devprodorig = (Xdev*Ydev)
	gen devprodnew = (Xplusadev * Yplusbdev)
	sum devprod*, d
	corr X Y, cov
	corr Xplusa Yplusb, cov
	sum devprodorig, d
	di r(sum)/9
	sum devprodnew, d
	di r(sum)/9
	
	corr X Y, cov
	corr Xplusa Yplusb, cov
	local cov = strltrim("`: display %10.2f r(cov_12)'")
	twoway (scatter Ydev Xdev,  ///
		title("Deviations from mean of random small data-sets 1 and 2 ") ///
		note("{it:n} =20" "small jitter added so identical points are distinct") ///
		mcolor(green%80)) ///
		(scatter Yplusbdev Xplusadev, jitter(2) mcolor(red%80) ///
		legend(order(2 "Original data deviations" 1 "Deviations for data with arbitrary constants added")) ///
		xtitle("X") ytitle("Y")  text(-2 2 "COV for both data-sets = `cov'", ///
			box))
	gen Xbya = X*.5
	gen Ybyb = Y*1.5
	
	twoway (scatter Y X,  ///
		title("Deviations from mean of random small data-sets 1 and 2 ") ///
		note("{it:n} =20" "X is multiplied by 0.5, Y by 1.5") ///
		mcolor(red%80)) ///
		(scatter Ybyb Xbya, mcolor(green%80) ///
		legend(order(1 "Original data" 2 "Arbitrary constants {it:multiplied}")) ///
		xtitle("X") ytitle("Y"))
	
	foreach var of varlist X Y Xbya Ybyb { 
		qui sum `var'
		gen `var'std = (`var' - r(mean))/r(sd)
	}
	
	twoway (scatter Ystd Xstd,  ///
		title("{it:Z}-scores of random small data-sets 1 and 2 ") ///
		note("{it:n} =20" "X is multiplied by 0.5, Y by 1.5" ///
		"small jitter added so identical points are distinct") ///
		mcolor(green%80)) ///
		(scatter Ybybstd Xbyastd, jitter(2) mcolor(red%80) ///
		legend(order(2 "Original data" 1 "Arbitrary constants {it:multiplied}")) ///
		xtitle("X") ytitle("Y"))
	
	* correlation tricks
	clear all 
	set obs 1000
	set seed 435
	gen P = runiform(0, 1000)
	gen A = P +rnormal(0, 100)
	reg A P
	predict Q, residuals
	label var Q "Q"
	gen R = sin(_n)
	qui sum P
	gen S = (P-r(mean)+ runiform(-50, 50))^2 
	corr P Q R S
	graph matrix P Q R S, msymbol(Oh) mcolor(green%50) half ///
		title("Scatterplots of randomly-generated variables") ///
		note("{it:n}=1000") maxes(xla(,angle(32)))
		
	* robustness
	set scheme tufte
	gen daysjob = yearsjob*365
	count if ~missing(daysjob) & ~missing(cohort)
	local n = r(N)
	corr daysjob cohort
	local r1 = round(r(rho), .01)
	corr yearsjob age
	local r2 = round(r(rho), .01)
	scatter daysjob cohort, mcolor(mint%50) ytitle("Days") xtitle("Year born") ///
		title("Relationship between days spent at current job and year born", ///
		size(medsmall)) text(15000 1990 `"{it:r} = `r1'"', box fcolor(white)) ///
		name(dayscohort, replace)
	scatter yearsjob age, mcolor(red%50) ytitle("Years") xtitle("Age") ///
		title("Relationship between years spent at current job and age", ///
		size(medsmall)) text(50 30 `"{it:r} = `r2'"', box fcolor(white)) ///
		name(yearsage, replace)
	gr combine dayscohort yearsage, note(`"Source: GSS 2018, {it:n} = `n'"')
	
	* commute/earnings correlation
	
	clear all
	use ./data/commutepay
	foreach var of varlist commute earnings { 
		qui sum `var'
		gen `var'_z = (`var' - r(mean))/r(sd)
	}
	gen zprods = commute_z * earnings_z
	list commute_z earnings_z zprods
	qui sum zprods
	di r(sum)/4
	
	clear all
	use ./data/commutepay
	set scheme tufte
	qui corr earn com
	local r = strltrim("`: display %10.2f r(rho)'")

	scatter earnings commute, title("Relationship between commute and earnings", ///
		size(medium)) mcolor(red) ///
		legend(off) ytitle("Yearly earnings ($, thousands)") ///
		text(42 5 "{it:r} = `r'", box) name(earncomcorr, replace)
	
* Lecture 6

	* Commute and earnings regression
	clear all
	use ./data/commutepay
	set scheme tufte
	qui reg earn com
	matrix coeffs = e(b)
	local slope = round(coeffs[1, 1], .01)
	local intercept = round(coeffs[1, 2], .01)
	local r = round(sqrt(e(r2)), 0.001)
	scatter earnings commute, title("Relationship between commute and earnings", ///
		size(medium)) mcolor(red) || lfit earnings commute, ///
		legend(off) ytitle("Yearly earnings ($, thousands)") ///
		text(42 5 `"{it:Y} = `slope'{it:X} + `intercept'"' ///
		`"{it:r} = `r'"', box) name(earncom, replace)
	
	qui reg com earn
	matrix coeffs = e(b)
	local slope = round(coeffs[1, 1], .01)
	local intercept = round(coeffs[1, 2], .01)
	local r = round(sqrt(e(r2)), 0.001)
	scatter commute earnings, title("Relationship between earnings and commute", ///
		size(medium)) mcolor(red) || lfit earnings commute, ///
		legend(off) xtitle("Yearly earnings ($, thousands)") ///
		text(42 5 `"{it:Y} = `slope'{it:X} + `intercept'"' ///
		`"{it:r} = `r'"', box) ytitle("Commute (mi.)") name(comearn, replace)
		
	* Regression cautions 
	gr combine earncom comearn, note("Source: fictional example") ///
		title("Regressing {it:Y} on {it:X} != regressing {it:X} on {it:Y}!")
	
	clear all
	use ./data/NHANES_merged2, clear
	
	sum bmxht bmxwt if ~missing(bmxht) & ~missing(bmxwt), d
	local n = r(N)
	di `n'
	corr bmxwt bmxht
	local r = round((r(rho)^2), 0.01)
	scatter bmxwt bmxht, mcolor(mint%30) ///
		ytitle("Weight (kg)") xtitle("Standing height (cm)") ///
		title("Relationship between weight and height", size(medium)) ///
		|| lfit bmxwt bmxht, legend(off) ///
		note(`"Source: NHANES 2017-18, {it:n} = `n'"' `"{it:r} {superscript:2} = `r'"')
	
	clear all
	use ./data/commutepay
	set scheme tufte
	qui reg earn com
	matrix coeffs = e(b)
	local slope = round(coeffs[1, 1], .01)
	local intercept = round(coeffs[1, 2], .01)
	local r = round(sqrt(e(r2)), 0.001)
	scatter earnings commute, title("Relationship between commute and earnings", ///
		size(medium)) mcolor(red) || lfit earnings commute, ///
		legend(off) ytitle("Yearly earnings ($, thousands)") ///
		text(42 5 `"{it:Y} = `slope'{it:X} + `intercept'"' ///
		`"{it:r} = `r'"', box) name(earncom, replace)
		
	set obs 6
	replace commute = 70 if _n == 6
	replace earnings = 30 if _n == 6
	
	qui reg earn com
	matrix coeffs = e(b)
	local slope = round(coeffs[1, 1], .01)
	local intercept = round(coeffs[1, 2], .01)
	local r = round(sqrt(e(r2)), 0.001)
	scatter earnings commute, title("Relationship between commute and earnings", ///
		size(medium)) mcolor(red) || lfit earnings commute, ///
		legend(off) ytitle("Yearly earnings ($, thousands)") ///
		text(55 40 `"{it:Y} = `slope'{it:X} + `intercept'"' ///
		`"{it:r} = `r'"', box) name(earncom2, replace) ///
		note("Here I add one extra point at [70, 30]")
	
	gr combine earncom earncom2, note("Source: fictional example") ///
		title("Outliers can change regression substantially!")

	* More examples
	
	clear all
	use ~/desktop/SOC365sp22/original_data/gss2018, clear
	set scheme tufte
	local ythenx = "educ paeduc"
	qui reg `ythenx'
	matrix coeffs = e(b)
	local slope = round(coeffs[1, 1], .01)
	local intercept = round(coeffs[1, 2], .01)
	local r = round(sqrt(e(r2)), 0.001)
	scatter `ythenx', jitter(10) ///
		title("Relationship between education and father's education", ///
		size(medium)) mcolor(red) || lfit educ paeduc, ///
		legend(off) ytitle("Respondent's education (years)") ///
		text(2.5 15 `"{it:Y} = `slope'{it:X} + `intercept'"' ///
		`"{it:r} = `r'"', box) name(paed, replace) ///
		|| lfit `ythenx', legend(off)
		
	use "~/desktop/soc365sp22/modified_data/2022-03-07 biomarker exploratory", clear
		
			* Let's look at the good work index, first cleaning it up. 
			rename RA4QSO_GW goodwork
			inspect goodwork 
			sum goodwork, d
			// Just investigating a bit
			label list RA4QSO_GW // Look for MV
			tab goodwork, mis
			recode goodwork (8 = .)
			tab goodwork, mis
			* Now let's look at the stress variable 
			rename RA4QPS_PS stress
			inspect stress
			label list RA4QPS_PS 
			sum stress, d
			tab stress, mis
			replace stress = .m if stress == 98
			tab stress, mis
			
			format stress %4.2f
			* Let's also add a jitter and line of best fit while we're at it. 
			reg stress goodwork
			local n = e(N)
			matrix coeffs = e(b)
			local slope = round(coeffs[1, 1], .01)
			local intercept = round(coeffs[1, 2], .01)
			local r = -round(sqrt(e(r2)), 0.001)
			scatter stress goodwork, jitter(10) mcolor(red) || lfit stress goodwork, ///
				ytitle("Stress score") xtitle("Good work score") legend(off) ///
				title("Relationship between quality of work and stress", size(medium)) ///
				note(`"Source: MIDUS Biomarker Project, 2012-2016, {it:n} = `n'"') ///
				text(45 2 `"{it:Y} = `slope'{it:X} + `intercept'"' ///
				`"{it:r} = `r'"', box fcolor(white))

	* Non-linearity
	
	clear all
	use ./data/NHANES_merged2, clear
	svyset [pweight = wtint2yr]
	gen selecter = runiform(0, 1)
	drop if selecter >0.1
	gen lnwt = ln(bmxwt)
	gen rtwt = sqrt(bmxwt)
	gen quarterwt = .25*bmxwt
	qui sum rtwt if ~missing(bmxht)
	local n = r(N)
	qui reg rtwt bmxht
	local plainr2 = round(e(r2), 0.01)
	qui reg lnwt bmxht
	local better2 = round(e(r2), 0.01)
	set scheme tufte
	
	#delimit ;
	scatter lnwt bmxht, mcolor(red) || 
		scatter rtwt bmxht, mcolor(green) legend(ring(0) pos(11) 
		order(1 "natural log of weight" 2 "root of weight for scale") 
		region(lcolor(black))) xlab(80(25)200) 
		note(`"Source: NHANES random subset for visibility, {it:n} = `n'"'
		`"fit for root: {it:R} {superscript:2} = `plainr2'"' 
		`"fit for log: {it:R} {superscript:2} = `better2'"'
		"recall that the original {it:R} {superscript:2} was 0.59")
		|| lfit lnwt bmxht, lpattern(longdash) || lfit rtwt bmxht, lpattern(dash_dot)
		title("Relationship between height and weight", size(medium)) ytitle("kg");
		
	
	reg rtwt bmxht;
	predict residuals, residuals;
	qui reg rtwt bmxht;
	local n = e(N);
	scatter residuals bmxht, mcolor(red) || lfit residuals bmxht || 
		fpfit residuals bmxht, est(degree(3)) legend(order(2 "linear regression"
		3 "nonlinear regression") ring(0) pos(11) region(lcolor(black))) xlab(80(25)200) 
		title("Residuals from regression of weight on height vs. height", 
		size(medium))
		ytitle("Distance of observed {it:Y}{subscript:i} from regression line")
		note(`"Source: NHANES random subset for visibility, {it:n} = `n'"');

	
	* Seeing how regression can miss local violations of zero-conditional mean	
	egen heightbins = cut(bmxht), group(100)
	bysort heightbins: egen meanwt = mean(rtwt)
	line meanwt bmxht || lfit rtwt bmxht
	
	* residual diagnostics
	reg bmxwt bmxht
	predict uhati, residuals
	qnorm uhati
	hist uhati
	
	use ~/desktop/soc365sp22/original_data/gss2018, clear
	svyset [pweight=wtssall]
	qui reg educ paeduc
	local n = e(N)
	predict uhati, residuals
	
	#delimit ;
	scatter uhati paeduc, mcolor(red) jitter(10) ||
		fpfit uhati paeduc, est(degree(3)) legend(order(2 
		"nonlinear regression") ring(0) pos(11) region(lcolor(black))) 
		title("Residuals from regression of education on father's ed vs. father's ed.", 
		size(medium))
		ytitle("Distance of observed {it:Y}{subscript:i} from regression line")
		note(`"Source: NHANES random subset for visibility, {it:n} = `n'"');
	
	* controlling 

	use ~/desktop/soc365sp22/original_data/gss2018, clear
	svyset [pweight=wtssall]
	
	set scheme plottig
	
	reg masei10 paeduc pasei10 maeduc
	predict macontrolresid, residuals
	reg educ paeduc pasei10 maeduc
	predict educcontrolresid, residuals
	reg macontrolresid educcontrolresid
	local n = e(N)
	
	reg educ masei10
	local n = e(N)
	
	#delimit ;
	scatter educ masei10, mcolor(red) jitter(10) 
		title("Respondent's education as a function of mother's SES", 
		size(medsmall)) ytitle("Education (years)")
		|| lfit educ masei10, legend(off)
		name(masei10, replace);
		
	#delimit ;
	scatter macontrolresid educcontrolresid, mcolor(red) jitter(10) 
		title("Respondent's education as a function of mother's SES"
		`"Both variables "cleaned" of variation of set of controls"' , 
		size(medsmall)) ytitle("Education (residuals from regression on controls)") 
		xtitle("Mother's SES (residuals from regression on controls)")
		|| lfitci macontrolresid educcontrolresid, acol(green%50)
		name(fwlreg, replace) legend(off);
	
	
	gr combine masei10 fwlreg, note(`"Source: GSS, {it:n} = `n'"');

	* Leverage
	
	use ~/desktop/soc365sp22/original_data/cps2019, clear
	svyset [pweight = fnlwgt]
	gen dropper = runiform(0, 1)
	keep if ((dropper >0.99) | hourslw>50)
	reg weekpay hourslw
	scatter weekpay hourslw if hourslw<100, mcolor(mint%50) || ///
		scatter weekpay hourslw if hourslw>=100, mcolor(red%50) || ///
		lfit weekpay hourslw || lfit weekpay hourslw if hourslw <100, ///
		legend(order(3 "All observations" ///
		4 "Obs. >100 hours removed")) ///
		ytitle("Weekly pay ($)") xtitle("Hours last week") ///
		title("Weekly pay vs hours worked last week", ///
		size(medsmall)) name(tworegs, replace)
	reg weekpay hourslw
	local n = e(N)
	gen hourslabel = hourslw if hourslw>100
	lvr2plot, mlabel(hourslabel) mcolor(red) name(lvr2plothours, replace) ///
		title("Possible influence vs. residuals plot", size(medium))
	
	gr combine tworegs lvr2plothours, note("Source: random subsample of CPS" ///
		`"{it:n} = `n'"') 

* L7. two-way tables. 
	* regression review
	
	mkdir ~/desktop/soc360sp23
	rmdir
	view browse https://tinyurl.com/NHANES365i 
	
	import sasxport5 ./data/DEMO_J.xpt, clear
	
		* It can be hard to remember where we got stuff if we plan to rename
		* the data-set to a better name and ditch the original file, so let's
		* do that now (this is not just a formality: I pulled these data a few
		* months ago, and I remembered that I did this while I was prepping the
		* lecture notes...and forgot exactly where I got the data and spent 15 
		* minutes tracking the exact source down :( . 
		
		label data "NHANES 2017-18 Demographic Data" 
		
		note: Original source is CDC website, SAS XPORT file DEMO_J.xpt ///
			https://tinyurl.com/NHANES365i 
		
	* Let's also manually make a source variable as this will help us later. 
	gen from_demog = 1
		 
	* And keep just a few variables for simplicity 
	keep seqn riagendr ridageyr from_demog wt*
	
	d, short 
		* Let's check out the matrix of data. Look like we have a matrix of
		* dimensions 9254x4 (9,254 rows and four columns). 
	save ./data/NHANES17_demog_short, replace
	* That looks good. So, now let's get the measurement data-set loaded. 
	
	view browse https://tinyurl.com/NHANES365ii
	
	import sasxport5 ./data/DXX_J.xpt, clear
	
		label da "NHANES 2017-18 Dual-Energy X-ray Absorptiometry - Whole Body" 
	
		note: Original source is CDC website, SAS XPORT file DXX_J.xpt at ///
		https://tinyurl.com/NHANES365ii
		
	* Let's again manually make a source variable. 
	gen from_xray = 1
	save ./data/NHANES_2017_whole_body_xray, replace
	
	* Let's verify that "seqn" identifies obs. uniquely here, too.
	
	isid seqn
	duplicates report seqn
	scalar exam_N = r(unique_value) 
		* OK, so, we have fewer observations, but that may not be problematic and
		* we're in the same ballpark; we would expect fewer people to have full
		* body exam. data than to have basic demographic data. Let's finally 
		* examine the dimensions of our data matrix and then try the merge.
	
	* And again just drop most variables
	keep seqn dxdtopf from_xray dxdtofat dxdtole
	save ./data/NHANES17_xray_short, replace
	
	d, short // Note that we have a 5114x3 matrix this time. 
		* So, we should expect a merger with a matrix of dim. 9254x4 to result
		* in a matrix with no more than 9254 rows and seven columns (you might
		* expect six since one variable must be shared, but -merge- automatically
		* makes a "_merge" variable). 
	browse // Sometimes it is helpful to see this to get a tactile sense.
	
	merge 1:1 seqn using ./data/NHANES17_demog_short
		* OK, great! We have matches on about half the IDs. 
		* Note that all unmatched people came from the "using" data-set, indicating
		* that everyone who was found in the body examination set had their basic
		* demographic variables collected, but the reverse was not true. 
	save ./data/NHANES17_temp
	
	* We can also check out who might have been absent in one set vs the other. 
	tab _merge, sum(ridageyr) nost nofre
		* Looks like the mean age in the basic data-set is significantly older. 
	
	* And of course, it is helpful to check the dimensions of the matrix we 
	* made and then examine the data directly. 
	d, short 
	br
	
	* Let's just briefly enjoy the fruits of our labor and do something fun. 
	* Content warning for discussion of weight/body-fat. 
	
	gen sex = riagendr-1 // recode to conventional dummy var. 
		label define sx 0 "male" 1 "female" // make value label
		label values sex sx // apply 
	rename dxdtopf percentfat // convenience 
	svyset [pweight=wtint2yr]
	
	import sasxport5 ./data/BMX_J, clear

	merge 1:1 seqn using ./data/NHANES17_temp, nogen
	save ./data/NHANES_merged2, replace
	
	use ./data/NHANES_merged2, clear
	svyset [pweight=wtint2yr]
	
	set scheme plottig
	gen logweight = ln(bmxwt)
	corr logweight bmxht, cov
	local vars = "logweight bmxht"
	reg `vars'
	matrix coeffs = e(b)
	local realslope = coeffs[1, 1]
	local realintercept = coeffs[1, 2]
	local slope = round(coeffs[1, 1], .01)
	local intercept = round(coeffs[1, 2], .01)
	local r = round(sqrt(e(r2)), 0.01)
	scatter `vars', title("Log-lin model", ///
		size(medium)) mcolor(green%10) || lfit `vars', ///
		legend(off) ytitle("Weight (kg, natural log)") ///
		text(5.5 75 `"{it:Y} = `slope'{it:X} + `intercept'"' ///
		`"{it:r} = `r'"', box fcolor(white)) name(loglin, replace)
	di exp(`realslope')
	di exp(`realintercept')
	gen yhats = exp(`realslope'*bmxht + `realintercept') 
	scatter bmxwt bmxht, mcolor(green%10) || line yhats bmxht, sort ///
		ytitle("Weight (kg)") title("Original outcome with fitted values") ///
		name(og, replace) legend(off) text(225 100 ///
		"`=ustrunescape("\u0059\u0302")'= {it:e}{superscript: 1.0227*height + 1.7913}") 
	gr combine loglin og, note("Source: NHANES 2017-18, {it: n} = `e(N)'") ///
		title("Relationship between weight and height")
		
		
		
	
	* basic two-way tables
	cd ~/desktop/SOC360fa22
	use ./data/gss2018 , clear
	svyset [pweight=wtssall]
	
	tab marital happy, col row
	label define mar2 1 "married" 2 "widow" 3 "div" 4 "sep" 5 "never"
	gen marital2 = marital
	label values marital2 mar2
	tab marital2 happy
	spineplot happy marital2, ///
		xtitle("", axis(2)) xtitle("Marital status", axis(1)) ///
		title("Relationship between marital status and happiness", size(medium)) ///
		note("Source: GSS 2018, {it:n} = `r(N)'")
		
	* another example
	
	ssc install scheme-burd
	set scheme burd
	label define firmown 1 "state-owned" 2 "employee-owned" 3 "capitalist-owned"
	gen firm_preference = company
	label values firm_preference firmown
	label list POLVIEWS
	recode polviews (1/3 = 1) (4 = 2) (5/7 = 3), gen(political_views)
	label define pol 1 "liberal" 2 "moderate" 3 "conservative"
	label values political_views pol
	label var political_views "political views"
	label var firm_preference "preference for firm-ownership"
	tab political_views firm_preference
	tab political_views firm_preference, row 
	tab political_views firm_preference, col 
	tab firm_preference political_views, col 
	spineplot firm_preference political_views, b1title("")

* Exam 1

	use ./data/nhanes_merged2, clear
	recode ridageyr (0/12 = 0) (13/19 = 1) (20/200 = 2), gen(agebkt)
	label define agebkt 0 "0-12" 1 "13-19" 2 "20-100"
	label values agebkt agebkt
	set scheme plottig
	qui sum bmxht if ~missing(ridageyr)
	graph hbox bmxarml, over(agebkt) asyvars ///
		box(1, color(red%50)) box(2, color(green%50)) box(3, color(orange%50)) ///
		title("Distribution of upper-arm length by age-bracket") ///
		note("Source: NHANES 2017-18, {it:n} = `r(N)'")
	
	hist bmxarml, by(agebkt) 
	asyvars ///
		box(1, color(red%50)) box(2, color(green%50)) box(3, color(orange%50)) ///
		title("Distribution of upper-arm length by age-bracket") ///
		note("Source: NHANES 2017-18, {it:n} = `r(N)'")
	
	save ./data/nhanes_merged2, replace
	
	
	import sasxport5  "/Users/gjmb/Desktop/SOC365sp22/original_data/DR1TOT_J.XPT", clear
	merge 1:1 seqn using ./data/nhanes_merged2, nogen
	gen outlier = dr1tprot>200
	gen fatshare = (dr1ttfat*9)/dr1tkcal
	scatter dxdtopf fatshare if dr1ttfat<200
	corr bmxarmc dr1tprot if dr1tprot <200 & agebkt == 0
	
	qui sum dr1tprot if riagendr == 1
	local nmen = r(N)
	qui sum dr1tprot if riagendr == 2
	local nwomen = r(N)
	graph hbox dr1tprot if outlier == 0, over(riagendr) asyvars ///
		box(1, color(red%50)) box(2, color(green%50)) m(1, msy(Dh)) m(2, msy(Oh)) ///
		title("Distribution of protein consumed by sex") ///
		note("Source: NHANES 2017-18" ///
		"{it:n}{subscript:men} = `nmen', {it:n}{subscript:wommen} = `nwomen'") ///
		legend(order(1 "male" 2 "female") ring(0) pos(11) region(ls(longdash)))
	
	twoway (hist dr1tprot if riagendr == 1, color(red%30)) ///
		 (hist dr1tprot if riagendr == 2, color(green%30))
		 
	use ~/desktop/soc365sp22/original_data/cps2019, clear
	set scheme tufte
	gen random = runiform()
	sort random
	keep if _n <25000
	
	kdensity wage1 if female==0, bw(2) nograph gen(mwX mwY)
	qui sum wage1 if female == 0
	local nmen = r(N)
	kdensity wage1 if female==1, bw(2) nograph gen(wX wY)
	qui sum wage1 if female == 1
	local nwomen = r(N)
	gen zero = 0
	twoway (rarea mwY zero mwX if female == 0, color(red%30)) ///
		(rarea wY zero wX if female == 1, color(green%30) ///
		title("Distribution of wages by sex") ytitle("Density") ///
		note("Source: CPS 2019" ///
		"{it:n}{subscript:men} = `nmen', {it:n}{subscript:wommen} = `nwomen'") ///
		legend(order(1 "male" 2 "female") ring(0) pos(3) region(ls(longdash))))

* review

	use "~/desktop/soc365sp22/modified_data/2022-03-07 biomarker exploratory", clear
	
	rename RA4QSO_GW goodwork
		
	// Just investigating a bit
	label list RA4QSO_GW // Look for MV
	tab goodwork, mis
	recode goodwork (8 = .)
	tab goodwork, mis
	* Now let's look at the stress variable 
	rename RA4QPS_PS stress
	inspect stress
	label list RA4QPS_PS 
	sum stress, d
	tab stress, mis
	replace stress = .m if stress == 98
	tab stress, mis
	sum RA1PRSEX,d 
	gen female = RA1PRSEX -1 if ~missing(RA1PRSEX)
	label define fem 0 "male" 1 "female"
	label values female fem
	format stress %4.0f
	bysort female: sum stress, d
	forvalues i = 0/1 {
		qui sum stress if female == `i'
		local nfem`i' = r(N)
		}
	
	twoway (histogram stress if female==0, color(red%30)) ///        
		(histogram stress if female==1, color(green%30)), ///
		legend( label(1 "men") label(2 "women")) title("Stress by sex") ///
		note("Source: MIDUS Biomarker Project, 2012-2016" ///
		"{it:n}{subscript:male} = `nfem0', {it:n}{subscript:female} = `nfem1'")
		
	gen teeth = RA4P8A == 32
	replace teeth = 0 if RA4P8A<32
	replace teeth = . if RA4P8A>32
	set scheme tufte
	forvalues i = 0/1 {
		qui sum stress if teeth == `i'
		local teeth`i' = r(N)
		}
	graph hbox stress, over(teeth) asyvars box(1, color(red%50)) ///
		box(2, color(green%50)) legend(order(2 "has all teeth" ///
		1 "missing at least one tooth") region(lc(black))) ///
		title("Stress by dental integrity") medline(lcolor(black)) ///
		note("Source: MIDUS Biomarker Project, 2012-2016" ///
		"{it:n}{subscript:all} = `teeth1', {it:n}{subscript:missing} = `teeth0'") 
	
	scatter stress RA4P8A if RA4P8A<33
	
	use ./data/nhanes_merged2, clear
	import sasxport5  "/Users/gjmb/Desktop/SOC365sp22/original_data/DR1TOT_J.XPT", clear
	merge 1:1 seqn using ./data/nhanes_merged2, nogen
	import sasxport5 ./data/DXX_J.xpt, clear
	merge 1:1 seqn using ./data/nhanes_merged2, nogen
	sum dxdhepf, d
	gen stdhf = (dxdhepf-r(mean))/r(sd)
	kdensity dxdhepf, nograph gen(hfx hfy)
	kdensity stdhf, nograph gen(shfx shfy)
	gen zero = 0
	
	qui sum dxdhepf
	local mean = round(r(mean), 0.01)
	local sd = round(r(sd), 0.01)
	local n = r(N)
	kdensity dxdhepf, lcolor(black) title("Distribution of percent fat of head") ///
		ytitle("Density") xtitle("Percent fat") ///
		note("Source: NHANES 2017-18, {it:n} = `r(N)'") ///
		text(.6 26 "mean = `mean'" "standard deviation = `sd'", box fcolor(white))
	
	twoway (rarea hfy zero hfx if (hfx<25.01 & hfx>=22.99), color(red%30)) ///
		(kdensity dxdhepf, lcolor(black) title("Distribution of percent fat of head") ///
		ytitle("Density") xtitle("Percent fat") xla(22(1)28) ///
		note("Source: NHANES 2017-18, {it:n} = `n'") legend(off) ///
		text(.6 26 "mean = `mean'" "standard deviation = `sd'", box fcolor(white)))
	
	qui sum stdhf
	local mean = round(r(mean), 0.01)
	local sd = round(r(sd), 0.01)
	local n = r(N)
	
	twoway (rarea shfy zero shfx if (shfx<1.39 & shfx>=-1.78), color(red%30)) ///
		(kdensity stdhf, lcolor(black) ///
		title("Distribution of percent fat of head (standardized)") ///
		ytitle("Density") xtitle("Percent fat") ///
		note("Source: NHANES 2017-18, {it:n} = `n'") legend(off) ///
		text(.3 4 "mean = `mean'" "standard deviation = `sd'", box fcolor(white)))
		
		
	cdfplot dxdhepf, opt1(lc(black)) xline(23, lpattern(longdash) lcolor(orange)) ///
		xline(25, lpattern(longdash) lcolor(mint)) ///
		ytitle("Cumulative probability") xtitle("Percent fat of head") ///
		title("Cumulative distribution of head fat percentage in US") ///
		note("Source: NHANES 2017-18, {it:n} = `n'") ///
		yline(.0375, lpattern(longdash) lcolor(orange)) ///
		yline(.9192, lpattern(longdash) lcolor(mint)) ///
		ylab(0(0.2)1 0.0375 0.9192, add) xla(22(1)28)
	
	gen marker = dxdrlle>17500
	label define mrker 0 " " 1 "possible influential point", modify
	label values marker mrker
	qui corr dxdrale dxdrlle
	scatter dxdrale dxdrlle, mcolor(red%20) ///
		title("Relationship of lean mass in right leg and arm") ///
		note("Source: NHANES 2017-18, {it:n} = `r(N)'") mlab(marker) mlabp(7)
		
	clear all
	set seed 20221005
	set obs 1000
	gen X = runiform(-100, 100)
	gen Y = -X + rnormal(0, 20)
	scatter Y X, name(test1) title("Relationship of {it:Y} to {it:X}")
	
	gen Z = -sqrt(X^2) + sin(X^2) + rnormal(0, 20)
	scatter Z X, mcolor(green) title("Relationship of {it:Z} to {it:X}") name(test2)
	
	
	
	gr combine test1 test2, note("Fictitious example data") ///
		title("Two functions of {it:X}, {it:Y} and {it:Z}, plotted against it")
	
	gen P = -Y+50
	scatter P X, name(test3, replace) title("Relationship of {it:P} to {it:X}")
	gr combine test1 test3, note("Fictitious example data" ///
		"{it:P} is a linear transformation of {it:Y} : -{it:Y} + 50") ///
		title("Two functions of {it:X}, {it:Y} and {it:P}, plotted against it")
		
	gen Q = X + 20*rt(5) - 5*sin(X)
	scatter Q X, name(test1) title("Relationship of {it:Y} to {it:X}")
	
	local mu = 19.9
	local sigma = 7.2
	local lb = `mu' - `sigma'*4
	local ub = `mu' + `sigma'*4
	local score = 25
	
	twoway function y=normalden((`mu'-x)/`sigma'), range(`lb' `ub') ///
	xla(`lb'(`sigma')`ub' `score', add alt) ///
	ytitle("Density") lcolor(black) || ///
	function y=normalden((`mu'-x)/`sigma'), range(`score' `ub') recast(area) color(red%70) ///
	xtitle("ACT English score") title("Distribution of ACT English scores") ///
	legend(off) note("Stylized data. {&mu} = `mu', {&sigma} = `sigma'")

	local mu = 19.9
	local sigma = 7.2
	local lb = `mu' - `sigma'*4
	local ub = `mu' + `sigma'*4
	local lower16 = `mu'-`sigma'
	local upper16 = `mu'+`sigma'
	
	twoway function y=normalden((`mu'-x)/`sigma'), range(`lb' `ub') ///
	xla(`lb'(`sigma')`ub' `score', add alt) ///
	ytitle("Density") lcolor(black) || ///
	function y=normalden((`mu'-x)/`sigma'), range(`lower16' `lb') recast(area) color(green%70) ///
	|| 	function y=normalden((`mu'-x)/`sigma'), range(`upper16' `ub') recast(area) color(green%70) ///
	xtitle("ACT English score") title("Distribution of ACT English scores") ///
	legend(off) note("Stylized data. {&mu} = `mu', {&sigma} = `sigma'")

* L8: sampling

	use ~/desktop/soc365sp22/original_data/cps2019, clear
	set scheme gg_hue
	set seed 20221005
	gen dropper = runiform(0, 1)
	sort dropper
	keep if _n <20001
	keep wage2
	gen wage5 = wage2 if wage2<250 &!missing(wage2)
	sum wage5, d
	hist wage5, normal normop(lcolor(black) lp(longdash)) kdensity color(red%30) ///
		title("Distribution of wages for salaried workers", size(medium)) ///
		xtitle("Earnings amortized to hour") ytitle("Density") ///
		note("Source: random subsample of CPS 2019, n = `r(N)'" ///
		"Black line: Normal curve for reference; blue line: density estimate")
	graph save histyi, replace

	use ~/desktop/soc365sp22/original_data/cps2019, clear
	keep wage2
	gen wage5 = wage2 if wage2<250 &!missing(wage2)
	tempfile means3
	quietly bootstrap samplemeans3=r(mean), saving(`means3') size(100) ///
	reps(500): summarize wage5 if !missing(wage5)
	use `means3', clear
	sum samplemeans3, d
	hist samplemeans3, normal normop(lcolor(black) lp(longdash)) kdensity color(red%30) ///
		title("Mean income for salaried workers across 500 samples", ///
			size(medium)) ///
		xtitle("Mean of sample") ytitle("Density") ///
		note("Source: random subsample of CPS 2019, n = `r(N)'" ///
		"Black line: Normal curve for reference; blue line: density estimate")
	graph save smiii, replace
	

	graph combine histyi.gph smiii.gph
	


	use ./data/gss2018, clear
	sum vote12 if vote12<3
	graph hbar if vote12<3, over(vote12) title("Distribution of voting behavior") ///
		b1title("Did {it:r} vote in the 2012 US presidential election?") ///
		asyvars ylab(0(10)70) ///
		note("Source: GSS 2018, {it:n} = `r(N)'" "Eligible voters only")
	
	use ./original_data/gss2018, clear // We'll use this as an example 
	tempfile bargraphdata // This makes what is basically a temporary data-set
	save `bargraphdata', emptyok // we'll save this
	preserve 
		// I showed preserve just a couple times very briefly. It basically makes
		// it possible to do some destructive to the data-set (which might be useful
		// in the short-run, as it is here), then restore it. 
	contract natfare
		// What this does is reduce the data-set to just two variables, abany (which
		// now has as "observations" the distinct possible responses) and _freq 
		// (which has as "observations" the number of people in each bucket). 
	local var_label: variable label natfare
		// Here, I use the "extended macro function" called "variable label" to store
		// the variable label for abany in the the local "var_label". 
	egen sh_natfare = pc(_freq)
		// This calculates percentages from those frequencies. 
	label var sh_natfare "share of `var_label'"
		// Now we label the variable we just made with the old var label
	drop _freq // We can safely drop frequency
	rename natfare category // And we'll rename the variable "category" 
	save `bargraphdata', replace // save
	restore // and restore our data. 

	* Now we're ready to run a loop. Sometimes we have to do the first step of a loop
	* in the "pre-loop" stage to get them to make sense. Here, we needed to have
	* the tempfile have an existing variable called "category". Now we can include
	* the other variables in a loop and do the same thing, merging and saving the
	* updated data. 
	foreach Xj of varlist natfarey { 
			preserve
			contract `Xj'
			local var_label: variable label `Xj'
			egen sh_`Xj' = pc(_freq)
			label var sh_`Xj' "share of `var_label'"
			drop _freq
			rename `Xj' category 
			merge 1:1 category using `bargraphdata', nogen
			save `bargraphdata', replace
			graph bar sh_*, over(category)
			restore
		}
	use `bargraphdata', clear
	set scheme gg_hue // I love this scheme--the colors really pop. 
	graph bar sh_*, over(category) ytitle("percent") ///
		title("Opinions on welfare") ///
		legend(cols(1) order(1 `""Assistance to the poor""' ///
		2 `""Welfare""') ring(0) pos(2) box fcolor(white) region(lc(black))) ///
		note("Source: GSS2018")
		
		// The stuff with the legend: cols(1) forces these into one column, which
		// is easier on the eyes. The elements of the legend again use the variable
		// label extended macro function. Just replace the names of the variables
		// if you want this -- or feel free to edit by hand. 
		


	* Sources I combined to make this work: 
	* 1. https://archive.ph/sIF01
	* 2. https://archive.ph/LoWgm
	* 3. https://archive.ph/b4VGp

* L10 Dummy variables

	* Proving the variance property 
	
	sysuse auto, clear
	local variables = "price mpg rep78 headroom trunk weight length turn displacement gear_ratio foreign"
	foreach var of local variables {
	qui sum `var'
	gen dummy`var' = `var' > r(mean)
	qui sum dummy`var', d
	di r(Var) 
	di r(mean)*(1-r(mean))*r(N)/(r(N)-1)
}

	* Basic creation 
	
	capture log close
	log using classeg, text replace
	use ./data/gss2018, clear
	describe wrkstat
	label list LABDE 
	gen LFP = wrkstat<5
	replace LFP = . if missing(wrkstat)
	tab wrkstat LFP, mis
	label define LFPlab 0 "not a LFP" 1 "LFP"
	label values LFP LFPlab
	tab LFP
	sum LFP, d
	di r(mean)*(1-r(mean))*(r(N)/(r(N)-1))
	di r(Var)

	* Conditional means and dummy means
	
	* First some fancy merging 
	#delimit ; 
	copy https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT ./data/DEMO_J.XPT
		, replace; 
	import sasxport5 ./data/DEMO_J.XPT;
	save ./data/nhanes_bigmerge, replace;
	local filenames "ALQ_J BMX_J BPX_J CDQ_J PAQ_J SMQ_J SMQFAM_J 
		MCQ_J SMQRTU_J SMQSHS_J UM_J"; 
	#delimit cr
	foreach name of local filenames {
		di `"https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/`name'.XPT"'
		copy https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/`name'.XPT ///
			./data/nhanes/`name'.XPT, replace
		import sasxport5 ./data/nhanes/`name'.XPT, clear
		merge 1:1 seqn using ./data/nhanes_bigmerge, nogen
		save ./data/nhanes_bigmerge, replace
		}
		
	label define race 1 "Mex-Am" 2 "'other Hisp.'" 3 "NH white" ///
		4 "NH Black" 5 "'other race'/mult."
	label values ridreth1 race
	egen urinemetals = rowmean(urx*)
	set scheme gg_hue
	
	vers 16: table ridreth1, c(mean urinemetals)
	graph hbar urinemetals, over(ridreth1, sort(urinemetals)) ///
		title("Mean metals in urine ({&mu}g)/L by race") ///
		b1title("") ytitle("") ///
		legend(ring(0) region(ls(longdash)) bplacement(nwest)) ///
		asyvars bar(1, color(red%30)) bar(2, color(green%30)) ///
		bar(3, color(mint%30)) bar(4, color(pink%30)) bar(5, color(orange%30)) ///
		note("Source: NHANES 2017-18, {it:n} = `r(N)'")

	gen white = ridreth1 == 3
	replace white = . if missing(ridreth1)
	label define white 0 "non-white" 1 "white"
	label values white white
	
	vers 16: table white, c(mean urinemetals)
	local n = r(N)
	local overline = uchar(773)
	forvalues i = 0/1 { 
		qui sum urinemetals if white == `i'
		local Ybar`i' = strltrim("`: display %10.2f r(mean)'")
		}
	graph hbar urinemetals, over(white, sort(urinemetals)) ///
		title("Mean metals in urine ({&mu}g)/L by race") ///
		b1title("") ytitle("") ///
		legend(ring(0) region(ls(longdash)) bplacement(nwest)) ///
		asyvars bar(1, color(red%30)) bar(2, color(green%30)) ///
		bar(3, color(mint%30)) bar(4, color(pink%30)) bar(5, color(orange%30)) ///
		note("Source: NHANES 2017-18, {it:n} = `n'") ylab(0(1)8) ///
		text(7 9 "Y`overline'{subscript:non-white} = `Ybar0'" ///
		"Y`overline'{subscript:white} = `Ybar1'", lp(longdash) lc(black) box ///
		fcolor(white) linegap(medsmall) width(30) height(15))

	* Logit
	
	clear all
	use ./data/nhanes_bigmerge
	gen heartprob = (mcq160e == 1) | (mcq160b == 1) | (mcq160c==1)
	replace heartprob = . if (mcq160e >2) | (mcq160b>2) | (mcq160c>2) 
	label define hp 0 "no" 1 "yes"
	label values heartprob hp
	scatter heartprob ridageyr 
	logit heartprob ridageyr
	predict yhat
	
	scatter heartprob ridageyr || line yhat ridageyr, sort
	
	gen cancer = mcq220 == 1
	replace cancer = . if mcq220>2
	label values cancer hp
	scatter cancer ridageyr
	logit cancer ridageyr
	predict cancer_yhat
	scatter cancer ridageyr || line cancer_yhat ridageyr, sort || lfit cancer ridagey

	gen cigmonth = smd641 if smd641<90
	replace cigmonth = 0 if smd641 == 2
	reg cancer cigmonth
	
	egen SBP = rowmean(bpxsy1 bpxsy2 bpxsy3 bpxsy4)
	gen HBP = SBP>120 &  ~missing(SBP)
	reg HBP bmxbmi
	logit HBP bmxbmi
	capture drop HBPprob
	predict HBPprob
	set scheme gg_hue
	
	local vars "HBP bmxbmi"
	reg `vars'
	matrix coeffs = e(b)
	local realslope = coeffs[1, 1]
	local realintercept = coeffs[1, 2]
	local slope = round(coeffs[1, 1], .01)
	local intercept = round(coeffs[1, 2], .01)
	local r = round(sqrt(e(r2)), 0.01)
	local n = e(N)
	
	scatter HBP bmxbmi, mcolor(mint%30) ///
		xtitle("BMI") ytitle("Has high blood pressure (mean systolic>120)") ///
		title("Relationship between BMI and hypertension", size(medium)) ///
		note(`"Source: NHANES 2017-18, {it:n} = `n'"') || ///
		lfit HBP bmxbmi, lcolor(orange) lp(dash_dot) legend(off) ///
		text(1.75 90 `"{it:Y} = `slope'{it:X} + `intercept'"' ///
		`"{it:r} = `r'"', box fcolor(white))
	
	local vars "HBP bmxbmi"
	reg `vars'
	local n = e(N)
	
	scatter HBP bmxbmi, mcolor(mint%30) ///
		xtitle("BMI") ytitle("Has high blood pressure (mean systolic>120)") ///
		title("Relationship between BMI and hyptertension", size(medium)) ///
		note(`"Source: NHANES 2017-18, {it:n} = `n'"') || ///
		line HBPprob bmxbmi, lcolor(red) lp(longdash) sort ///
		legend(order(2 "Predicted probability") ring(0) pos(3) ///
		region(lp(longdash) lc(black)) box fcolor(white))
	
	clear all
	set seed 20221010
	set obs 1000
	gen X = runiform(-1000, 1000)
	gen p = (((X/1000) + rnormal(0, .2))+1)/2
	sum X
	gen p2 = normal(X + r(mean)/r(sd)) + rnormal(0, .2)
	drop if p>1 | p<0
	drop if p2>1 | p2<0
	gen logodds = ln(p/(1-p))
	gen logoddsp2 = ln(p2/(1-p2))
	gen D = p>=0.5
	gen D2 = p2>=0.5
	corr D D2

	list in 1/50
	reg p X
	scatter D X, mcolor(red%20) ||  scatter p X, mcolor(green%20) ///
		legend(ring(0) pos(3) region(lp(longdash) lc(black) fcolor(white)) ///
		order(1 "indicator outcome" 2 "unobserved probability" /// 
		3 "regression of {it:p} on {it:X}" )) ///
		title("Observed dummy outcome and underlying propensity", size(medium)) ///
		ytitle("Probability") note("Fictious example data, {it:n} = 1000" ///
		, box fcolor(white) lc(black) lp(solid)) ///
		|| lfit p X, lc(black) lp(longdash) ///
		|| lfit D X, lc(black) lp(longdash) name(pdreg, replace)
	
	scatter D X, mcolor(red%20) ||  scatter p2 X, mcolor(green%20) ///
		legend(ring(0) pos(3) region(lp(longdash) lc(black) fcolor(white)) ///
		order(1 "indicator outcome" 2 "unobserved probability" /// 
		3 "regression of {it:p} on {it:X}" )) ///
		title("Observed dummy outcome and underlying propensity", size(medium)) ///
		ytitle("Probability") note("Fictious example data, {it:n} = 1000" ///
		, box fcolor(white) lc(black) lp(solid)) ///
		|| lfit p2 X, lc(blac) lp(longdash) name(pdreg2, replace)
	
	scatter D X, mcolor(red%20) ||  scatter p X, mcolor(green%20) ///
		legend(ring(0) pos(5) region(lp(longdash) lc(black) fcolor(white)) ///
		order(1 "indicator outcome" 2 "unobserved probability" 3 "log odds")) ///
		title("Observed dummy outcome and underlying propensity", size(medium)) ///
		ytitle("") note("Fictious example data, {it:n} = 1000") || ///
		scatter logodds X, mcolor(orange%30) ///
		legend(ring(0) pos(2) region(lp(longdash) lc(black) fcolor(white))) || ///
		lfit D X, lcolor(black) lp(longdash) || lfit p X, lc(black) lp(solid) ///
		|| lfit logodds X, lc(orange) lp(dash_dot)
	
	reg logodds X
	predict yhat, xb
	replace yhat = exp(yhat) / (1+exp(yhat))
	
	reg logoddsp2 X 
	predict hats, xb
	replace hats = exp(hats) / (1+exp(hats))
	
	scatter D X, mc(red%20)|| scatter p2 X, mc(green%20)  ///
		|| lfit p2 X, lc(black) lp(dash_dot) ///
		note("Fictious example data, {it:n} = 1000") || ///
		line yhat X, lc(black) lp(solid) sort ///
		title("Observed dummy outcome and underlying propensity", size(medium)) ///
		ytitle("") legend(order(1 "indicator outcome" 2 "unobserved probability" ///
		3 "LPM predictions" 4 "logistic predictions") box fcolor(white) ///
		region(ls(longdash)) ring(0) pos(5)) 

	* strat
	
	use ./data/gss2018, clear
	
	tab race, gen(races)
	rename races1 white
	rename races2 Black
	rename races3 otherrace
	local vars "yearsjob age"
	reg `vars' Black otherrace
	local n = e(N)
	predict yhat, xb
	gen yhatB = yhat- .0487167 
	gen yhatO = yhat - -.6546251  
	
	scatter `vars', jitter(10) mc(red%20) ///
		|| line yhat age, sort lc(red) lp(longdash) ///
		|| line yhatB age, sort lc(green) lp(dash_dot) ///
		|| line yhatO age, sort lc(orange) lp(solid) ///
		note("Source: GSS 2018, {it:n} = `n'") ///
		title("Relationship between years on job and education by race", size(medium)) ///
		ytitle("Years spent on job") xtitle("Education") /// 
		legend(order(4 "white" 5 "Black" 6 `""other""') ///
		box fcolor(white) region(ls(longdash)) ring(0) pos(5))
		
	scatter `vars' if white ==1, jitter(10) mc(red%20) ///
		|| scatter `vars' if Black ==1, mc(green%20) jitter(10) ///
		|| scatter `vars' if otherrace == 1, mc(orange%20) jitter(10) ///
		|| lfit `vars' if white == 1, lc(red) lp(longdash) ///
		|| lfit `vars' if Black == 1, lc(green) lp(dash_dot) ///
		|| lfit `vars' if otherrace == 1, lc(orange) lp(solid) ///
		note("Source: GSS 2018, {it:n} = `n'") ///
		title("Relationship between years on job and education by race", size(medium)) ///
		ytitle("Years spent on job") xtitle("Education") /// 
		legend(order(4 "white" 5 "Black" 6 `""other""') ///
		box fcolor(white) region(ls(longdash)) ring(0) pos(5))

* L11. Probability. 
		
	* HW data-set
	clear all
	set obs 1000 // Let's use 1000 observations
	gen n = _n // we need this as an index
	gen cancer = n <10 // We first make 9 of the 10 cancer-having individuals.
	label define carcinogen 0 "no cancer" 1 "cancer"
	label values cancer carcinogen
	gen test = n <101 // We'll stipulate that 100/1000 (ten percent) test positive.
	label define testing 0 "negative" 1 "positive"
	label values test testing
	replace cancer =1 if n ==101
	* Let's randomize these and then examine some observations
	gen sortorder = runiform()
	sort sortorder
	gen ID = _n 
		// now let's make an ID variable that's independent of the assignment of 
		// treatment and control above
	drop n // and get rid of the original ID var
	list ID cancer test in 1/100

	/* Now, let's calculate row conditional probabilities. Here, we find the prob. 
	of testing positive given your various cancer statuses. Notably, this makes it
	seem as though a positive test very likely means that you have cancer! Only 10
	percent of people who don't have cancer test positive.*/ 

	tab cancer test, row

	/* But, this is a specific case of conditioning on the wrong thing. We want to 
	think about conditioning instead on what we know: that you tested positive for 
	cancer. We can do that by conditioning on test status. Here, it is clearer that,
	conditional having a positive cancer test, your probability of cancer ...
	... p(cancer | +test) = nine percent. */ 

	tab cancer test, col

	/* This example is a little more confusing because it's not immediately clear 
	why the first way to think about it is wrong. One way to see this is that while
	it is true that only a small percentage of cancer-free people test positive, 
	there is no reason that it shouldn't be any particular person (assuming that
	false positives are randomly-distributed). So, it is improbable for any specific
	person to test positive if they are cancer free, but -someone- has to be one of
	the false positives, right? One way to think about it is to assume that all of
	these people are tested at the same hospital. If you were an oncologist at the
	hospital, you should *not* look at a woman's chart and say "ah! the probability 
	of her testing positive if she's cancer-free is only 10 percent! panic!" And that
	would be counterintuitive *from that standpoint*. Instead, you'd say "OK, well, 
	we get 100 positive tests a day and 91 are false positives. No big deal. The false
	positives have to come from somewhere. It was unlikely to be any specific person,
	because positive tests are rare overall, but given that you have a positive test,
	you still probably don't have cancer". */ 

	/* Example 3. 
	Here's a little analogy that might help, which I often use. Suppose that we turn
	to a happier subject, excellence in women's sports. What's the probability of any
	woman athlete being a top-ten percent basketball player in high school?  
	Obviously, it's 10 percent. We can actually just re-label the items in the 
	cancer example to show this. We'll let "positive test" be "top tier HS athlete" 
	and "has cancer" turn into a happier rarity, being a WNBA player. */ 

	gen topHSplayer = test
	label define hsp 0 "not top tier" 1 "top tier"
	label values topHSplayer hsp

	gen WNBA = cancer
	label define wnba 0 "not in league" 1 "pro athlete"
	label values WNBA wnba

	tab WNBA topHSplayer, row

	/* Now, what's the probability, conditional on your not-being in the WNBA, of
	still having-been a top-tier athlete? 

	Technically that is a very low probability as well: 9.19 percent. That said, it
	would still clearly be unwise to conclude, if someone says "Back in HS, I could 
	throw a basketball over those mountains", that they are probably currently in the
	WNBA. This is a bit more intuitive because it's familiar: there's not a lot of
	great amateur athletes out there, but that still doesn't mean that any hotshot
	at the pickup game is, was, or will be a pro. You can picture it like pouring
	rocks through two filters, the first one with small holes and the second with
	tiny holes, above a tub that is almost-perfectly sealed.

	Yes, it's unlikely for any particular rock to get through the first
	filter, and yes almost no rocks get into the tub without going through either 
	sieve, but that says nothing about whether rocks that get through the first
	filter can easily get through a second filter.

	And, once again, we see that no matter which way you calculate the two-way table,
	there is no "wrong" way to do it, just a confusing way.*/  

	tab WNBA topHSplayer, col

	/* For example, this column conditional probability does correctly indicate that 
	being a great HS basketball player does, in fact, makes you *relatively* much 
	more likely to play in the pros -- about nine times more probable, if we want
	to be really exact! However, in these epidemiological and life-course settings, 
	people are often more interested in the absolute p(x) and interested in their 
	own outcome rather than in how unlikely it is for anyone to be in their shoes.
	So, instead of saying "well, p(+test | cancer) is much higher than 
	p(+test | no cancer)" or "it is unlikely for anyone to be a great HS basketball
	player", they want to know "is p(cancer | +test) something I should worry about"
	and "do I have a chance to play pro ball if I am really good at HS ball". In this
	case, the answers to the first and second sets of questions are different. */ 

	keep topHSplayer WNBA
	
	label define yesno 0 "no" 1 "yes"
	label values topHSplayer WNBA yesno
	rename topHSplayer HSstar

	save ./data/WNBA 
	
	* Probability examples.
	
	twoway function y=normalden(x), range(-4 4) ytitle("Density") || ///
	function y=normalden(x), range(-4 4) recast(area) color(red%70)  ///
	xtitle("{it:z}") ytitle("Density") title("Standard Normal PDF") ///
	legend(off) xlabel(-3 -1.96 -1 0 1 1.96 3) note("Total AUC is 1")
	
	twoway function y=normalden(x), range(-4 4) ytitle("Density") || ///
	function y=normalden(x), range(-4 -1) recast(area) color(red%70) || ///
	function y=normalden(x), range(1 4) recast(area) color(red%70) ///
	xtitle("{it:z}") ytitle("Density") title("Standard Normal PDF") ///
	legend(off) xlabel(-3 -1.96 -1 0 1 1.96 3) note("Shaded area = 32 percent")
	
	twoway function y=normalden(x), range(-4 4) ytitle("Density") || ///
	function y=normalden(x), range(-4 -1) recast(area) color(mint%70) || ///
	function y=normalden(x), range(-1 4) recast(area) color(red%70) ///
	xtitle("{it:z}") ytitle("Density") title("Standard Normal PDF") ///
	legend(off) xlabel(-3 -1.96 -1 0 1 1.96 3) ///
	note("Mint area = 15.87 percent" "From this we infer red area is 84.13")

	
	* Probability functions

	set scheme gg_hue
	use ./data/gss2018, clear
	describe wrkstat
	label list LABDE 
	gen LFP = wrkstat<5
	replace LFP = . if missing(wrkstat)
	tab wrkstat LFP, mis
	label define LFPlab 0 "not a LFP" 1 "LFP"
	label values LFP LFPlab
	qui sum LFP
	
	hist LFP, percent  ///
		title("[Estimated] PMF of labor force participation in the US" ///
		, size(medium)) note("Source: GSS 2019, {it:n} = `r(N)'") ///
		ylab(0(10)90) xla(0(1)1) ///
		text(70 0.5 "PMF({it:D}) = {it:p}{superscript:{it:D}} * (1-{it:p}){superscript:1-{it:D}}" ///
		"PMF(0) = 0.6522{superscript:0} * (0.3478){superscript:1} = 0.3478" ///
		"PMF(1) = 0.6522{superscript:1} * (0.3478){superscript:0} = 0.6522", box fcolor(white))
	
	qui sum LFP
	
	qui sum chldidel
	hist chldidel, percent discrete ///
		title("[Estimated] PMF of number of children {it:r} thinks is ideal in the US" ///
		, size(medium)) note("Source: GSS 2019, {it:n} = `r(N)'") ///
		ylab(0(10)50) xlab(0(1)7 8 "no limit", add)
		
	use ./data/nhanes_bigmerge, clear
	
	qui sum bmxht if ridageyr >17
	hist bmxht if ridageyr >17, kdensity /// 
		legend(ring(0) region(ls(longdash)) bplacement(nwest)) ///
		note("Source: NHANES 2017-18, {it:n} = `r(N)'") xlab(140(10)200) ///
		title("[Estimated] PDF of height adults, US", size(medium))
		
* L12: binomial distribution. 
		
	* binomial theorem
	
	use ./data/gss2018, clear
	drop if missing(relig)
	gen catholic = relig == 2
	keep catholic
	tab catholic
	hist catholic, percent  ///
		title("[Estimated] PMF of Catholic individuals in the US" ///
		, size(medsmall)) note("Source: GSS 2019, {it:n} = `r(N)'") ///
		ylab(0(10)90) xla(0(1)1) ///
		saving(Bernpmf, replace)
	
	tempfile counts
	bootstrap samplecounts=r(sum), saving(`counts') size(100) ///
		reps(1000) noisily: summarize catholic
	use `counts', clear
	
	hist samplecounts, normal discrete percent xlab(10(2)36) ///
		title("[Estimated] PMF of number of Catholics in many samples" ///
		, size(medsmall)) note("{it:n} for each trial is 100" ///
		"{it:n} of reps is 1000") xtitle("Count of Catholics in sample") ///
		saving(Binpmf, replace)
	
	gr combine "Bernpmf" "Binpmf"
	
	gr "Bernpmf"

	clear all
	set scheme tufte
	set obs 1000000
	foreach i of numlist 1 2 5 50 100 500 1000 { 
		gen sample`i' = rbinomial(`i', 0.5)
		gen prop`i' = sample`i'/`i'
	}

	twoway (hist prop1, color(red%50) frac)  ///
		(hist prop5, color(cyan%50) frac) (hist prop100, color(orange%50) frac) ////
		(hist prop1000, color(green%50) frac ///
		, title("The binomial distribution in action") legend(order(1 "{it:n} = 1" ///
		2 "{it:n} = 5" 3 "{it:n} = 100" 4 "{it:n} = 1000") ring(0) pos(3) ///
		region(lp(dash_dot) lc(black))) ytitle("Fraction of samples") ///
		note("Simulation of 1,000,000 random samples from binomial distributions of size {it:n}"))

	twoway (hist prop1, color(red%50))  ///
		(hist prop5, color(orange%50)) (hist prop100, color(mint%50)) ////
		(hist prop1000, color(green%50) ///
		, title("The binomial distribution in action") legend(order(1 "{it:n} = 1" ///
		2 "{it:n} = 5" 3 "{it:n} = 100" 4 "{it:n} = 1000") ring(0) pos(3) ///
		region(lp(dash_dot) lc(black))) ytitle("") ///
		note("Simulation of 1,000,000 random samples from binomial distributions of size {it:n}"))

	twoway (hist sample1, color(red%50) percent)  ///
		(hist sample5, color(orange%50) percent) (hist sample100, color(mint%50) percent) ////
		(hist sample1000, color(green%50) percent ///
		, title("The binomial distribution in action") legend(order(1 "{it:n} = 1" ///
		2 "{it:n} = 5" 3 "{it:n} = 100" 4 "{it:n} = 1000") ring(0) pos(3) ///
		region(lp(dash_dot) lc(black))) ytitle("") ///
		note("Simulation of 1,000,000 random samples from binomial distributions of size {it:n}"))

	foreach i of numlist 1 5 100 500 { 
		hist sample`i', color(red%50) percent discrete note("{it:n} = `i'") ///
			xtitle("Number of successes") name(bin`i', replace)
		}
	hist sample1, color(red%50) percent note("{it:n} = 1") ///
			xtitle("Number of successes") name(bin1, replace)
	gr combine bin1 bin5 bin100 bin500, ///
		title("The binomial distribution in action") ///
		note("Simulation of 1,000,000 random samples from different binomial distributions")
		
	tempfile binomialresponses
	copy https://tinyurl.com/binomialresponses `binomialresponses', replace
	import excel `binomialresponses', clear firstrow
	rename Pleasewriteasanumberhowman k 
	qui sum
	hist k, color(red%50) percent discrete note("Samples of size = 25" ///
		"Class simulation from SOC360fa22, number of replications = `r(N)'") ///
		xtitle("Number of successes") name(bin1, replace) ///
		title("A simulation of the binomial distribution")
	
	clear all
	set obs 11
	gen X = _n-1
	gen Y = (binomial(10,X,0.7))
	forvalues k = 1/11 { 
		replace Y = (binomial(10,`k',0.7))-(binomial(10,`k'-1,0.7)) if X == `k'
	}
	scatter Y X, sort xtitle("{it:k} (number of successes)") ///
		title("Binomial distribution for {it:n} = 10, {it:p} = 0.7") ///
		ytitle("{it:P}({it:X} = {it:k})") ylab(0(0.05)0.3) xlab(0(1)10)

	* Review for Quiz 2. 
		
	use "~/Downloads/ICPSR_36158/DS0001/36158-0001-Data.dta", clear
	
	format SCWM_CWHI %4.2f
	local vars = "SCEM_STRSI SCWM_CWHI"
	drop if missing(SCEM_STRSI) |  missing(SCWM_CWHI)
	corr `vars', cov
	corr `vars'
	local r = round(r(rho), 0.01)
	sum `vars' 
	sum `vars'
	reg `vars'
	local n = e(N)
	matrix coeffs = e(b)
	local realslope = coeffs[1, 1]
	local realintercept = coeffs[1, 2]
	local slope = round(coeffs[1, 1], .01)
	local intercept = round(coeffs[1, 2], .01)
	scatter `vars', title("Stress as a function of control over work hours", ///
		size(medium)) mcolor(green%20) jitter(10) || lfit `vars', ///
		legend(off) ytitle("Weight (kg, natural log)") ///
		text(17 1.5 `"{it:Y} = `slope'{it:X} + `intercept'"' ///
		`"{it:r} = `r'"', box fcolor(white)) ///
		ytitle("Stress") xtitle("Control over work hours") ///
		note("Work-hours control scale derived from Thomas and Ganster (1995)" ///
		"Stress scale from Cohen {it: et al}. (1983)" ///
		"Source: Work, Family, and Health Study 2009-12, {it:n} = `n'") 
		
	cd ~/desktop/soc360fa22
	use ./data/gss2018, clear
	tab satjob race, row nofre
	tab satjob race, col nofre
	drop if missing(race)
	gen white = race == 1
		
	use ~/desktop/soc365sp22/original_data/cps2019, clear
	drop if missing(wbho)
	gen white = wbho == 1
	sum white
	reg wage4 white
	
	set scheme gg_hue
	gen dropper = runiform(0, 1)
	drop if dropper<0.9
	graph hbar wage4, over(white) ///
		title("Mean wage ($/hr.) by race") ///
		b1title("") ytitle("") ///
		legend(ring(0) region(ls(longdash)) bplacement(nwest) ///
		order(1 "non-white" 2 "white")) ylab(0(2)30) ///
		asyvars bar(1, color(red%30)) bar(2, color(green%30)) /// 
		note("Source: CPS 2019, {it:n} = `e(N)'") 
		
	
	clear all
	set obs 1175
	gen X = _n-1
	gen Y = (binomial(11175,X,0.37))
	forvalues k = 1/1175 { 
		replace Y = (binomial(1175,`k',0.37))-(binomial(1175,`k'-1,0.37)) if X == `k'
	}
	gen zero = 0
	local mu = 435
	local sigma = 16.55
	scatter Y X, sort xtitle("{it:k} (number of successes)") msymbol(p) ///
		title("Binomial distribution for {it:n} = 1175, {it:p} = 0.37") ///
		xlab(0(300)1200) scheme(tufte) ytitle("{it:P}({it:X} = {it:k})")
	
	clear all
	set obs 5
	gen X = _n-1
	gen Y = (binomial(5,X,0.5))
	forvalues k = 1/5 { 
		replace Y = (binomial(5,`k',0.5))-(binomial(5,`k'-1,0.5)) if X == `k'
	}
	scatter Y X, sort xtitle("{it:k} (number of successes)") msymbol(O) mc(blac) ///
		title("Binomial distribution for {it:n} = 5, {it:p} = 0.5") ///
		ytitle("{it:P}({it:X} = {it:k})")
	
* L14. Binomial + CLT. 

	* Left-handedness e.g. 
	clear all
	local p = 0.05
	local n = 1000
	set obs 1000
	gen X = _n-1
	gen Y = (binomial(`n', X, `p'))
	forvalues k = 1/`n' { 
		replace Y = (binomial(`n',`k',`p'))-(binomial(`n',`k'-1,`p')) if X == `k'
	}
	scatter Y X if X<70, sort xtitle("{it:k} (number of successes)") msymbol(point) mc(black) ///
		title("Binomial distribution for {it:n} = `n', {it:p} = `p'") ///
		ytitle("{it:P}({it:X} = {it:k})") ///
		|| 	scatter Y X if X>=70, sort xtitle("{it:k} (number of successes)") ///
		msymbol(point) mc(red) ///
		title("Binomial distribution for {it:n} = `n', {it:p} = `p'") ///
		ytitle("{it:P}({it:X} = {it:k})") legend(off)
		
	local ki = 69
	di 1-binomial(`n', `ki', `p')
	di 1-normal((`ki'-(`n'*`p'))/(sqrt(`p'*(1-`p')*`n')))
	
	local min = 0
	local max = 1000
	local mu = `n'*`p'
	local s = sqrt(`p'*(1-`p')*`n')
	di `s'
	twoway function y=normalden((x-`mu')/`s'), range(`min' `max') ///
		ytitle("Density") xtitle("X") || ///
		function y=normalden((x-`mu')/`s'), range(`ki' `max') ///
		recast(area) color(red%80) /// 
		legend(order(2 "AUC = 0.0029") ///
		region(lp(longdash) fcolor(white) lc(black)) ///
		ring(0) pos(3)) xlab(0(100)1000) xlab(50, add) ///
		title("Normal approximation of left-handed example") 
		
	* College e.g. 
	
	clear all
	local p = 0.37
	local n = 1175
	set obs 1175
	gen X = _n-1
	gen Y = (binomial(`n', X, `p'))
	forvalues k = 1/`n' { 
		replace Y = (binomial(`n',`k',`p'))-(binomial(`n',`k'-1,`p')) if X == `k'
	}
	local min = 0
	local max = 1200
	local mu = `n'*`p'
	local s = sqrt(`p'*(1-`p')*`n')
	local ki = 450
	di 1-binomial(`n', `ki', `p')
	di 1-normal((`ki'-(`n'*`p'))/(sqrt(`p'*(1-`p')*`n')))
	twoway function y=normalden((x-`mu')/`s'), range(`min' `max') ///
		ytitle("Density") xtitle("X") || ///
		function y=normalden((x-`mu')/`s'), range(`ki' `max') ///
		recast(area) color(red%80) /// 
		legend(order(2 "AUC = 0.1635") ///
		region(lp(longdash) fcolor(white) lc(black)) ///
		ring(0) pos(3)) xlab(0 100 200 300 434 600 700 800 900 1000 1100 1200) ///
		title("Normal approximation of PMF of X")
	

	scatter Y X if X<451, sort xtitle("{it:k} (number of successes)") msymbol(p) ///
		title("Binomial distribution for {it:n} = 1175, {it:p} = 0.37") ///
		xlab(0(300)1200) scheme(tufte) ytitle("{it:P}({it:X} = {it:k})") || ///
		scatter Y X if X>450, mcolor(red) ///
		sort xtitle("{it:k} (number of successes)") msymbol(p) ///
		title("Binomial distribution for {it:n} = 1175, {it:p} = 0.37") ///
		xlab(0(300)1200) ytitle("{it:P}({it:X} = {it:k})") legend(off)
		
	use ~/desktop/soc365sp22/original_data/cps2019, clear
	drop if wage4>500
	sum wage4
	hist wage4, normal normop(lcolor(black) lp(longdash)) ///
		kdensity kdenop(lc(mint) lp(longdash)) bin(100) ///
		title("Income for all Springfield workers", size(medium)) ///
		xtitle("$/hour (imputed)") ytitle("Density") ///
		note("Black line: Normal curve for reference; blue line: density estimate")
	graph save wages, replace
	tempfile CPSwagemeans
	noisily bootstrap samplemeans=r(mean), saving(`CPSwagemeans') size(10000) ///
		reps(500): summarize wage4 
	use `CPSwagemeans', clear
	sum samplemeans, d
	hist samplemeans, normal normop(lcolor(black) lp(longdash)) ///
		kdensity kdenop(lc(mint) lp(longdash)) ///
		title("Mean income for Springfield workers across 5000 samples", ///
			size(medium)) bin(25) ///
		xtitle("Mean of sample") ytitle("Density") ///
		note("Source: random subsamples of Springfield, {it:n} = 10,000" ///
		"Black line: Normal curve for reference; blue line: density estimate")
	sum
	graph save sm, replace
	
	save ~/desktop/soc360fa22/data/samplemeans, replace
	
	gr combine wages.gph sm.gph
	
* L15 on CIs. 

	* Basic CI idea
	clear all
	set scheme tufte
	local mu = 0
	local sigma = 1
	local rangemin = `mu' - (4*`sigma')
	local rangemax = `mu' + (4*`sigma')
	local twosigma = (`sigma'*1.96)
	local LB95 = `mu' - `twosigma'
	local UB95 = `mu' + `twosigma'
	local xcoordtxtL = `LB95' -`sigma'
	local xcoordtxtR = `UB95' +`sigma'
	twoway function y=normalden(x, `mu', `sigma'), range(`rangemin' `rangemax') ///
		xlab(`rangemin'(`sigma')`rangemax') xline(`mu', lpattern(longdash)) ///
		ytitle("Density") xline(`LB95', lpattern(dash)) ///
		xline(`UB95', lpattern(dash)) || ///
	function y=normalden(x, `mu', `sigma'), range(`LB95' `UB95') ///
		recast(area) color(red%80) legend(order(2 "Middle 95 percent") ///
		region(lp(longdash) fcolor(white) lc(black)) ring(0) pos(10)) ///
		text(0.25 `xcoordtxtL' "95 of samples ..." "{&rarr}") ///
		text(0.25 `xcoordtxtR' "...are in this range" "{&larr}") ///
		xtitle("Z") note("This is the most common level of confidence") ///
		title("Standardized sampling distribution of all possible sample means", size(medium)) 
	
	* Showing common CIs
	
	local z90 = round(invnorm(.95), 0.01)
	local z95 = round(invnorm(.975), 0.01)
	local z99 = round(invnorm(.995), 0.01)
	twoway function y=normalden(x), range(-4 4) ytitle("Density") || ///
	function y=normalden(x), range(-`z90' `z90') recast(area) color(red%80) || ///
	function y=normalden(x), range(-`z95' -`z90') recast(area) color(red%40) || ///
	function y=normalden(x), range(-`z99' -`z95') recast(area) color(red%20) || ///
	function y=normalden(x), range(`z90' `z95') recast(area) color(red%40) || ///
	function y=normalden(x), range(`z95' `z99') recast(area) color(red%20) ///
		legend(order(2 "90 percent CI" 3 "95 percent CI" 4 ///
		"99 percent CI") ring(0) pos(10) /// 
		region(lp(longdash) fcolor(white) lc(black))) ///
		xtitle("Standardized sample means") ytitle("Density")  scheme(tufte) ///
		title("Three common levels of confidence visualized", size(medium)) ///
		xlab(-`z99' -`z95' -`z90' -1 0 1 `z90' `z95' `z99', alt) 
		
	* Class demo CIs
	set scheme gg_hue
	tempfile binomialresponses
	copy https://tinyurl.com/binomialresponses `binomialresponses', replace
	import excel `binomialresponses', clear firstrow
	rename Pleasewriteasanumberhowman k 
	qui sum
	hist k, color(red%50) percent discrete note("Samples of size = 25" ///
		"Class simulation from SOC360fa22, number of replications = `r(N)'") ///
		xtitle("Number of successes") name(bin1, replace) ///
		title("A simulation of many samples from the binomial distribution with {it:n} = 25, {it:p} = 0.5", /// 
	size(medium)) ytitle("{it:P}({it:X} = {it:k})")
	set scheme tufte
	gen sample_number = _n
	label var sample_number "Sample number"
	gen propheads = k/25
	local sigma = sqrt(.5*.5/25)
	local zstar = invnorm(0.05)
	gen LB90 = propheads - `zstar'*`sigma'
	gen UB90 = propheads + `zstar'*`sigma'
	local vars = "LB90 UB90 sample_number"
	local cond = "(LB90<0.5 | UB90>0.5)"
	qui sum propheads, d 
	scatter `vars', yline(0.5) || rspike `vars' if !`cond', legend(off) ///
		title("`r(N)' CIs (90 percent) using class coin flips", size(medium)) ///
		xla(0(5)`r(N)') ytitle("Sample proportion of heads") || ///
		rspike `vars' if `cond', lc(red) yla(0.1(.1)1) yla(0.5 "{&mu} =0.5", add) /// 
		note("Note that most but not all intervals capture {&mu}")

	* MOE example
	use ~/desktop/soc360fa22/data/samplemeans, clear
	local z95 = round(invnorm(.975), 0.01)
	di `z95'
	sum
	local mu = round(r(mean), 0.01)
	local SE = round(r(sd), 0.01)
	local MOE = `z95' * `SE'
	di `MOE'
	local LB = round(`mu' - `MOE', 0.01)
	local UB = round(`mu' + `MOE', 0.01)
	local min = `mu' - `MOE'*2 
	di `min'
	local max = `mu' + `MOE'*2 
	di `max'
	local leftcoord = `LB' - .5*`SE'
	local rightcoord = `UB' + .5*`SE'
	set scheme tufte
	twoway function y=normalden(x, `mu', `SE'), range(`min' `max') title("Density") ///
	|| function y=normalden(x, `mu', `SE'), range(`LB' `UB') recast(area) color(red%40) ///
		xtitle("Sample means ($/hour)") ytitle("Density")  scheme(tufte) ///
		title("Sampling distribution of mean wage for 'Springfield'", size(medium)) ///
		xlab(`LB' `mu' `UB') legend(off) xline(`LB' `UB', lp(longdash)) ///
		text(1.5 `leftcoord' "Lower bound of 95 CI" "{&rarr}") ///
		text(1.5 `rightcoord' "Upper bound of 95 CI" "{&larr}") ///
		note("LB and UB are given by {&mu} +/- 1.96*{&sigma}/{&radic}{it:n}")
	
	gen LB = samplemeans - `MOE'
	gen UB = samplemeans + `MOE'
	gen rando = runiformint(-1, 1)
	
	sort rando
	local z95 = round(invnorm(.975), 0.01)
	di `z95'
	sum samplemeans
	local mu = round(r(mean), 0.01)
	local SE = round(r(sd), 0.01)
	local MOE = `z95' * `SE'
	di `MOE'
	local LB = round(`mu' - `MOE', 0.01)
	local UB = round(`mu' + `MOE', 0.01)
	local min = `mu' - `MOE'*2 
	di `min'
	local max = `mu' + `MOE'*2 
	di `max'
	local leftcoord = `LB' - .5*`SE'
	local rightcoord = `UB' + .5*`SE'
	forvalues i = 1/4 { 
		local xbari = samplemeans[`i']
		sum 
		local lb = round(LB[`i'], 0.01)
		local ub = round(UB[`i'], 0.01)
		local mupos = `mu' - 0.5*`SE'
		local leftcoord = `lb' - .5*`SE'
		local rightcoord = `ub' + .5*`SE'
		twoway function y=normalden(x, `mu', `SE'), range(`min' `max') title("Density") ///
		xtitle("Sample means ($/hour)") ytitle("") scheme(tufte) ///
		legend(off) xline(`lb' `xbari' `ub', lp(longdash) lc(red)) ///
		text(1.5 `leftcoord' "LB 95 CI" "{&rarr}") ///
		text(1.5 `rightcoord' "UB 95 CI" "{&larr}") ///
		text(1.5 `xbari' "sample" "mean") ///
		text(0.5 `mupos' "{&mu} {&rarr}") ///
		xline(`mu', lc(black)) ///
		ylab(, nolabels) ///
		name(CI`i', replace)
	}
	local overline = uchar(773)
	gr combine CI1 CI2 CI3 CI4, ///
		title("CIs from several random samples", size(medium)) ///
		note("LB and UB are given by X`overline' +/- 1.96*{&sigma}/{&radic}{it:n}") 
		
	* Worked e.g. 
	
	use ./data/gss2018, clear
	label data "GSS 2018"
	save ./data/gss2018, replace
	local var sei10
	local lab : var la sei10
	local datalab : data label
	qui sum `var'
	local xbar = round(r(mean), 0.01)
	local s = round(r(sd), 0.01)
	local note "Source: `datalab', {it:n} = `r(N)'"
	local overline = uchar(773)
	hist sei10, title("`lab'") note("`note'") text(0.03 60 ///
		"X`overline' = `xbar'" "{it:s} = `s'", box fcolor(white) ///
		lp(longdash) lc(black))
	
	local z95 = round(invnorm(.975), 0.01)
	di `z95'
	sum sei10
	local mu = round(r(mean), 0.01)
	local s = round(r(sd), 0.01)
	di `mu' 
	di `s'
	local SE = round(`s'/sqrt(r(N)), 0.0001)
	di `SE'
	local MOE = round(`z95' * `SE', 0.0001)
	local MOE = strltrim("`: display %10.4f `MOE''")
	di `MOE'
	local LB = round(`mu' - `MOE', 0.01)
	local UB = round(`mu' + `MOE', 0.01)
	local min = `mu' - `MOE'*2 
	di `min'
	local max = `mu' + `MOE'*2 
	di `max'
	local leftcoord = `LB' - .6*`SE'
	local rightcoord = `UB' + .6*`SE'
	set scheme tufte
	twoway function y=normalden(x, `mu', `SE'), range(`min' `max') title("Density") ///
	|| function y=normalden(x, `mu', `SE'), range(`LB' `UB') recast(area) color(red%40) ///
		xtitle("Sample means of SEI") ytitle("Density")  scheme(tufte) ///
		title("Sampling distribution of SEI", size(medium)) ///
		xlab(, nolabels) legend(off) ///
		xline(`LB' `mu' `UB', lp(longdash)) ///
		text(0.7 `leftcoord' "{&mu} - MOE" "={&mu}-`MOE'" "{&rarr}") ///
		text(0.7 `rightcoord' "{&mu} + MOE" "={&mu}+`MOE'" "{&larr}") ///
		text(0.7 `mu' "{&mu} = ??")  ///
		note("MOE is given by 1.96*{&sigma}/{&radic}{it:n}") text(0.03 60 ///
		"X`overline' = `xbar'" "{it:s} = `s'")
	
	* possible worlds e.g.
	use ./data/gss2018, clear
	keep sei10
	drop if missing(sei10)
	sum 
	local SE = r(sd)/sqrt(r(N))
	di `SE'
	sum
	gen devs = rnormal(0, `SE')
	gen possiblemu = r(mean) + devs
	forvalues i = 1/4 { 
		local z95 = round(invnorm(.975), 0.01)
		local mu = possiblemu[`i']
		sum sei10
		local xbari = round(r(mean), 0.01)
		local s = round(r(sd), 0.01)
		local SE = round(`s'/sqrt(r(N)), 0.0001)
		di `SE'
		local MOE = round(`z95' * `SE', 0.0001)
		local MOE = strltrim("`: display %10.4f `MOE''")
		local LB = round(`xbari' - `MOE', 0.01)
		local UB = round(`xbari' + `MOE', 0.01)
		local mupos = `mu' - 0.5*`SE'
		local leftcoord = `LB' - .5*`SE'
		local rightcoord = `UB' + .5*`SE'
		local min = `mu' - `MOE'*2 
		local max = `mu' + `MOE'*2
		local overline = uchar(773)
		local xbarpos = `xbari'-0.5*`SE'
		local mugraph = strltrim("`: display %10.2f `mu''")
		local xbargraph = strltrim("`: display %10.2f `xbari''")
		twoway function y=normalden(x, `mu', `SE'), range(`min' `max') title("") ///
			xtitle("Sample means (SEI10)") ytitle("") scheme(tufte) ///
			legend(off) xline(`LB' `xbari' `UB', lp(longdash) lc(red)) ///
			text(0.7 `leftcoord' "LB 95 CI" "{&rarr}") ///
			text(0.7 `rightcoord' "UB 95 CI" "{&larr}") ///
			text(0.7 `xbarpos' "X`overline'" "{&rarr}") ///
			text(0.4 `mupos' "{&mu} = `mugraph'") ///
			xline(`mu', lc(black)) ///
			name(CI`i', replace)
	}
	
	gr combine CI1 CI2 CI3 CI4, ///
		title("Several possible scenarios for a given CI", size(medium)) 

* L17 hypothesis tests

	* From CIs to hypothesis tests
		gsort -possiblemu
		
		sum sei10
		local s = round(r(sd), 0.01)
		local SE = round(`s'/sqrt(r(N)), 0.0001)
		di (r(mean) - (`SE'*1.96))
		local z95 = round(invnorm(.975), 0.01)
		local mu = possiblemu[50]
		di `mu'
		sum sei10
		local xbari = round(r(mean), 0.01)
		local s = round(r(sd), 0.01)
		local SE = round(`s'/sqrt(r(N)), 0.0001)
		di `SE'
		local MOE = round(`z95' * `SE', 0.0001)
		local LB = round(`xbari' - `MOE', 0.01)
		local UB = round(`xbari' + `MOE', 0.01)
		local mupos = `mu' - 0.5*`SE'
		local leftcoord = `LB' - .5*`SE'
		di `leftcoord'
		local rightcoord = `UB' + .5*`SE'
		local min = `mu' - `MOE'*2 
		di `min'
		local max = `mu' + `MOE'*2
		local overline = uchar(773)
		local xbarpos = `xbari'-0.5*`SE'
		local mugraph = strltrim("`: display %10.2f `mu''")
		local xbargraph = strltrim("`: display %10.2f `xbari''")
		local uppercut = `mu' + `MOE'
		di `uppercut'
		local lowercut = `mu' - `MOE'
		local uppercutpos = `uppercut' + .5*`SE'
		di `uppercutpos'
		local lowercutpos = `lowercut' - .5*`SE'
		local MOE = strltrim("`: display %10.4f `MOE''")
		twoway function y=normalden(x, `mu', `SE'), range(`min' `max') title("") ///
			xtitle("Sample means (SEI10)") ytitle("") scheme(tufte) ///
			legend(off) xline(`LB' `xbari' `UB', lp(longdash) lc(red)) ///
			text(0.7 `leftcoord' "=X`overline'" ///
				"- z{subscript:C}*{&sigma}/{&radic}{it:n}" "{&rarr}", color(red) ///
				box fcolor(white)) ///
			text(0.7 `rightcoord' "=X`overline'" ///
				"+ z{subscript:C}*{&sigma}/{&radic}{it:n}" "{&larr}", color(red) ///
				box fcolor(white)) ///
			text(0.7 `xbarpos' "X`overline'" "{&rarr}", color(red)) ///
			text(0.4 `mupos' "{&mu}{subscript:0} =" "`mugraph'" "{&rarr}") ///
			xline(`mu') xline( `uppercut' `lowercut', lp(longdash)) ///
			text(0.4 `uppercutpos' "{&mu}{subscript:0} +" ///
				"z{subscript:C}*{&sigma}/{&radic}{it:n}" "{&larr}", /// 
				box fcolor(white)) ///
			text(0.4 `lowercutpos' "{&mu}{subscript:0} -" ///
				"z{subscript:C}*{&sigma}/{&radic}{it:n}" "{&rarr}", ///
				box fcolor(white)) ///
			title("From CI to hypothesis test") ///
		|| function y=normalden(x, `mu', `SE'), range(`min' `xbari') ///
			recast(area) color(mint%70) ///
		note("Shaded area is {it:p}(sample mean this small or smaller | {&mu}{subscript:0}).")
		
		
		twoway function y=normalden(x, `mu', `SE'), range(`min' `max') title("") ///
			xtitle("Sample means (SEI10)") ytitle("") scheme(tufte) ///
			legend(off) xline(`LB' `xbari' `UB', lp(longdash) lc(red)) ///
			text(0.7 `leftcoord' "=X`overline'" ///
				"- z{subscript:C}*{&sigma}/{&radic}{it:n}" "{&rarr}", color(red) ///
				box fcolor(white)) ///
			text(0.7 `rightcoord' "=X`overline'" ///
				"+ z{subscript:C}*{&sigma}/{&radic}{it:n}" "{&larr}", color(red) ///
				box fcolor(white)) ///
			text(0.7 `xbarpos' "X`overline'" "{&rarr}", color(red)) ///
			text(0.4 `mupos' "{&mu}{subscript:0} =" "`mugraph'" "{&rarr}") ///
			xline(`mu') xline( `uppercut' `lowercut', lp(longdash)) ///
			text(0.4 `uppercutpos' "{&mu}{subscript:0} +" ///
				"z{subscript:C}*{&sigma}/{&radic}{it:n}" "{&larr}", /// 
				box fcolor(white)) ///
			text(0.4 `lowercutpos' "{&mu}{subscript:0} -" ///
				"z{subscript:C}*{&sigma}/{&radic}{it:n}" "{&rarr}", ///
				box fcolor(white)) ///
			title("From CI to hypothesis test") ///
		|| function y=normalden(x, `mu', `SE'), range(`min' `xbari') ///
			recast(area) color(mint%70) ///
		|| function y=normalden(x, `mu', `SE'), range(`min' `lowercut') ///
			recast(area) color(red%30) ///
		|| function y=normalden(x, `mu', `SE'), range(`uppercut' `max') ///
			recast(area) color(red%30) 
	
	* numpets hypothesis test example
	
	use ./data/gss2018, clear
	sum numpets, d
	local mu = 1.5
	local zstar = invnorm(.975)
	local SE = r(sd)/sqrt(r(N))
	local min = round(`mu' - `SE'*4, 0.01)
	local max = round(`mu' + `SE'*4, 0.01)
	local mupos = 1.45
	local SEpos = 1.65
	local xbari = r(mean)
	local negxbari = `mu'-(`xbari'- `mu')
	di `min'
	di `max'
	local MOE = `zstar'*`SE'
	local LB = `mu' - `MOE'
	local UB = `mu' + `MOE'
	local overline = uchar(773)
	local zscore = (`xbari' - `mu')/`SE'
	local negz = -1*`zscore'
	local critz = invnorm(0.975)
	local negcritz = invnorm(0.025)
	set scheme gg_hue

		local xbarpos = `xbari'-0.5*`SE'
		local mugraph = strltrim("`: display %10.2f `mu''")
		local xbargraph = strltrim("`: display %10.2f `xbari''")
		local SEgraph = strltrim("`: display %10.2f `SE''")
		local uppercut = `mu' + `MOE'
		di `uppercut'
		local lowercut = `mu' - `MOE'
		local uppercutpos = `uppercut' + .5*`SE'
		di `uppercutpos'
		local lowercutpos = `lowercut' - .5*`SE'
	twoway function y=normalden(x, `mu', `SE'), xla(`min'(.1)`max') ///
		xtitle("Sample means ({stMono:numpets})") ytitle("") scheme(tufte) ///
		legend(off) xline(`mu', lc(black)) range(`min' `max') ///
		title("Sampling distribution under H{subscript:0}") ///
		text(4.5 `mupos' "{&mu}{subscript:0} = 1.5" "{&rarr}") ///
		text(3.5 `SEpos' "{&sigma}/{&radic}{it:n} = `SEgraph'""{&larr}") 
		
	twoway function y=normalden(x, `mu', `SE'), xla(`min'(.1)`max') ///
		xtitle("Sample means ({stMono:numpets})") ytitle("") scheme(tufte) ///
		legend(off) xline(`mu', lc(black)) range(`min' `max') ///
		title("Sampling distribution under H{subscript:0}") ///
		text(4.5 `mupos' "{&mu}{subscript:0} = 1.5" "{&rarr}") ///
		text(3.5 `SEpos' "{&sigma}/{&radic}{it:n} = `SEgraph'""{&larr}") ///
		text(2.5 `xbarpos' "X`overline' = `xbargraph'" "{&rarr}") ///
		xline(`xbari', lp(longdash) lc(black))
			
	twoway function y=normalden(x, `mu', `SE'), xla(`min'(.1)`max') ///
		xtitle("Sample means ({stMono:numpets})") ytitle("") scheme(tufte) ///
		xline(`mu', lc(black)) range(`min' `max') ///
		title("Sampling distribution under H{subscript:0}") ///
		text(4.5 `mupos' "{&mu}{subscript:0} = 1.5" "{&rarr}") ///
		text(3.5 `SEpos' "{&sigma}/{&radic}{it:n} = `SEgraph'""{&larr}") ///
		text(2.5 `xbarpos' "X`overline' = `xbargraph'" "{&rarr}") ///
		xline(`xbari', lp(longdash) lc(black)) ///
		|| function y=normalden(x, `mu', `SE'), range(`xbari' `max') ///
		recast(area) color(red%70) || ///
		function y=normalden(x, `mu', `SE'), range(`min' `negxbari') ///
		recast(area) color(red%70) ///
		legend(order(2 "{it:p}(data this weird | {&mu}{subscript:0})") ///
		ring(0) pos(9) region(ls(longdash)))
		
		local zpos = `zscore'+0.25
		local negzpos = `negz' - 0.5
		local zstarpos = `critz'-.5
		local negzstarpos = (-1*`critz')+.5
		twoway function y=normalden(x), range(-4 4) ///
		xtitle("Sample means ({stMono:numpets})") ytitle("") scheme(tufte) ///
		xline(0, lc(black))  xline(-1.96 1.96, lc(orange)) ///
		title("Standardized sampling distribution under H{subscript:0}") ///
		xline(`zscore', lp(longdash) lc(black)) ///
		xline(`negz', lp(longdash) lc(black)) ///
		|| function y=normalden(x), range(`zscore' 4) ///
		recast(area) color(red%70) || ///
		function y=normalden(x), range(-4 `negz') ///
		recast(area) color(red%70) ///
		|| function y=normalden(x), range(`critz' 4) ///
		recast(area) color(orange%30) || ///
		function y=normalden(x), range(-4 `negcritz') ///
		recast(area) color(orange%30) ///
		legend(order(2 "{it:p}(data this weird | {&mu}{subscript:0})" ///
		4 "rejection region") ring(0) pos(9) region(ls(longdash))) ///
		text(.35 `zpos' "{it:z} = 2.44" "{&larr}") ///
		text(.35 `negzpos' "{it:-z} = -2.44" "{&rarr}") ///
		text(.25 `zstarpos' "{it:z*} = 1.96" "{&rarr}") ///
		text(.25 `negzstarpos' "{it:-z*} = -1.96" "{&larr}")
	
	twoway function y=normalden(x, `mu', `SE'), range(`min' `max') title("") ///
		xtitle("Sample means ({stMono:numpets})") ytitle("") scheme(tufte) ///
		legend(off) xline(`LB' `mu' `UB', lp(longdash) lc(red)) ///
		text(4 `uppercutpos' "{&mu}{subscript:0} +" ///
			"z{subscript:C}*{&sigma}/{&radic}{it:n}" "{&larr}", /// 
			box fcolor(white)) ///
		text(4 `lowercutpos' "{&mu}{subscript:0} -" ///
			"z{subscript:C}*{&sigma}/{&radic}{it:n}" "{&rarr}", ///
			box fcolor(white)) ///
		xla(`lowercut' `mu' `uppercut')	
	* CI comparison
	use ./data/gss2018, clear	
	local z95 = round(invnorm(.975), 0.01)
		local mu = 1.5
		sum numpets
		local xbari = round(r(mean), 0.01)
		local s = round(r(sd), 0.01)
		local SE = round(`s'/sqrt(r(N)), 0.0001)
		di `SE'
		local MOE = round(`z95' * `SE', 0.0001)
		local MOE = strltrim("`: display %10.4f `MOE''")
		local LB = round(`xbari' - `MOE', 0.01)
		local LBgraph = strltrim("`: display %10.2f `LB''")
		local UB = round(`xbari' + `MOE', 0.01)
		local UBgraph = strltrim("`: display %10.2f `UB''")
		local mupos = `mu' - 0.5*`SE'
		local leftcoord = `LB' - .5*`SE'
		local rightcoord = `UB' + .5*`SE'
		local min = `mu' - `MOE'*2 
		local max = `mu' + `MOE'*4
		local overline = uchar(773)
		local xbarpos = `xbari'-0.5*`SE'
		local mugraph = strltrim("`: display %10.2f `mu''")
		local xbargraph = strltrim("`: display %10.2f `xbari''")
		twoway function y=normalden(x, `mu', `SE'), range(`min' `max') title("") ///
			xtitle("Sample means ({stMono:numpets})") ytitle("") scheme(tufte) ///
			legend(off) xline(`LB' `xbari' `UB', lp(longdash) lc(red)) ///
			text(2.5 `leftcoord' "LB 95 CI=" "`LBgraph'" "{&rarr}", color(red)) ///
			text(2.5 `rightcoord' "UB 95 CI=" "`UBgraph'" "{&larr}", color(red)) ///
			text(2.5 `xbarpos' "X`overline' = `xbargraph'" "{&rarr}", color(red)) ///
			text(0.4 `mupos' "{&mu}{subscript:0} = `mugraph'") ///
			xline(`mu', lc(black)) ///
			title("95 CI for {stMono:numpets} with sampling distribution under {&mu}{subscript:0}", size(medium))
			
	* TV example
	use ./data/detroitareastudy, clear
	keep V78 
	keep if V78<80
	sum V78, d
	local mu = 3.1
	local zstar = invnorm(.975)
	local SE = r(sd)/sqrt(r(N))
	local min = round(`mu' - `SE'*5, 0.01)
	local max = round(`mu' + `SE'*5, 0.01)
	local mupos = 3.0
	local SEpos = 3.25
	local xbari = r(mean)
	local xbarpos = `xbari' - .5*`SE'
	local negxbari = `mu'-(`xbari'- `mu')
	di `min'
	di `max'
	local MOE = `zstar'*`SE'
	local LB = `mu' - `MOE'
	local UB = `mu' + `MOE'
	local overline = uchar(773)
	local zscore = (`xbari' - `mu')/`SE'
	local negz = -1*`zscore'
	local critz = invnorm(0.05)
	local negcritz = -1*`critz'
	set scheme gg_hue

		local xbarpos = `xbari'-0.5*`SE'
		local mugraph = strltrim("`: display %10.2f `mu''")
		local xbargraph = strltrim("`: display %10.2f `xbari''")
		local SEgraph = strltrim("`: display %10.2f `SE''")
		local uppercut = `mu' + `MOE'
		di `uppercut'
		local lowercut = `mu' - `MOE'
		local uppercutpos = `uppercut' + .5*`SE'
		di `uppercutpos'
		local lowercutpos = `lowercut' - .5*`SE'
	twoway function y=normalden(x, `mu', `SE'), xla(`min'(.1)`max') ///
		xtitle("Sample means (hours of TV,  {stMono:V78})") ytitle("") scheme(tufte) ///
		legend(off) xline(`mu', lc(black)) range(`min' `max') ///
		title("Sampling distribution under H{subscript:0}") ///
		text(4.5 `mupos' "{&mu}{subscript:0} = `mu'" "{&rarr}") ///
		text(3.5 `SEpos' "{&sigma}/{&radic}{it:n} = `SEgraph'""{&larr}") ///
		text(4.5 `mupos' "{&mu}{subscript:0} = `mu'" "{&rarr}") ///
		text(3.5 `SEpos' "{&sigma}/{&radic}{it:n} = `SEgraph'""{&larr}") 
		
	twoway function y=normalden(x, `mu', `SE'), xla(`min'(.1)`max') ///
		xtitle("Sample means (hours of TV,  {stMono:V78})") ytitle("") scheme(tufte) ///
		legend(off) xline(`mu', lc(black)) range(`min' `max') ///
		title("Sampling distribution under H{subscript:0}") ///
		text(4.5 `mupos' "{&mu}{subscript:0} = `mu'" "{&rarr}") ///
		text(3.5 `SEpos' "{&sigma}/{&radic}{it:n} = `SEgraph'""{&larr}") ///
		text(2.5 `xbarpos' "X`overline' = `xbargraph'" "{&rarr}") ///
		xline(`xbari', lp(longdash) lc(black))
			
	twoway function y=normalden(x, `mu', `SE'), xla(`min'(.1)`max') ///
		xtitle("Sample means (hours of TV,  {stMono:V78})") ytitle("") scheme(tufte) ///
		xline(`mu', lc(black)) range(`min' `max') ///
		title("Sampling distribution under H{subscript:0}") ///
		text(4.5 `mupos' "{&mu}{subscript:0} = `mu'" "{&rarr}") ///
		text(3.5 `SEpos' "{&sigma}/{&radic}{it:n} = `SEgraph'""{&larr}") ///
		text(4 `xbarpos' "X`overline' = `xbargraph'" "{&rarr}") ///
		xline(`xbari', lp(longdash) lc(black)) ///
		|| function y=normalden(x, `mu', `SE'), range(`min' `xbari') ///
		recast(area) color(red%70) ///
		legend(order(2 "{it:p}(data this weird | {&mu}{subscript:0})") ///
		ring(0) pos(9) region(ls(longdash)))
		
		
		local zpos = `zscore'-.5
		local zstarpos = `critz'+.5
		twoway function y=normalden(x), range(-4 4) ///
		xtitle("Sample means (hours of TV,  {stMono:V78})") ytitle("") scheme(tufte) ///
		xline(0, lc(black)) xline(-1.64, lc(orange)) /// ///
		title("Standardized sampling distribution under H{subscript:0}") ///
		xline(`zscore', lp(longdash) lc(black)) ///
		|| function y=normalden(x), range(-4 `zscore') ///
		recast(area) color(red%70) || ///
		|| function y=normalden(x), range(-4 `critz') ///
		recast(area) color(orange%30) ///
		legend(order(2 "{it:p}(data this weird | {&mu}{subscript:0})" ///
		3 "rejection region") ring(0) pos(9) region(ls(longdash))) ///
		text(.35 `zpos' "{it:z} = -1.875" "{&rarr}") ///
		text(.35 `zstarpos' "{it:z*} = -1.64" "{&larr}")
		
		local zpos = `zscore'-.5
		local zstarpos = `critz'+.5
		local zprimepos = `zpos'-.5
		twoway function y=normalden(x), range(-4 4) ///
		xtitle("Sample means (hours of TV,  {stMono:V78})") ytitle("") scheme(tufte) ///
		xline(0, lc(black)) xline(-1.64, lc(orange)) xline(-1.96, lc(mint)) ///
		title("Standardized sampling distribution under H{subscript:0}") ///
		xline(`zscore', lp(longdash) lc(black)) ///
		|| function y=normalden(x), range(-4 `zscore') ///
		recast(area) color(red%70) || ///
		|| function y=normalden(x), range(-4 `critz') ///
		recast(area) color(orange%30) ///
		|| function y=normalden(x), range(-1.96 `critz') ///
		recast(area) color(mint%30) ///
		legend(order(2 "{it:p}(data this weird | {&mu}{subscript:0})" ///
		3 "rejection region" 4 "zone of ambiguity") ring(0) pos(9) region(ls(longdash))) ///
		text(.35 `zpos' "{it:z} = -1.875" "{&rarr}") ///
		text(.35 `zstarpos' "{it:z*} = -1.64" "{&larr}") ///
		text(.3 `zprimepos' "{it:z**} = -1.96" "{&rarr}", color(mint)) 
		
		
		local zpos = `zscore'-.5
		local zstarpos = `negz'+.5
		twoway function y=normalden(x), range(-4 4) ///
		xtitle("Sample means (hours of TV,  {stMono:V78})") ytitle("") scheme(tufte) ///
		xline(0, lc(black)) xline(1.64, lc(orange)) /// ///
		title("Standardized sampling distribution under H{subscript:0}") ///
		xline(`zscore', lp(longdash) lc(black)) ///
		|| function y=normalden(x), range(`zscore' 4) ///
		recast(area) color(red%30) || ///
		|| function y=normalden(x), range(`negcritz' 4) ///
		recast(area) color(orange%30) ///
		legend(order(2 "{it:p}(data this weird | {&mu}{subscript:0})" ///
		3 "rejection region") ring(0) pos(9) region(ls(longdash))) ///
		text(.35 `zpos' "{it:z} = -1.875" "{&rarr}") ///
		text(.35 `zstarpos' "{it:z*} = 1.64" "{&larr}")

* L19 

	*binomial
	clear all
	set scheme tufte
	local p = 0.9
	local n = 2347
	local se = sqrt(`p'*(1-`p')*`n')
	set obs 2347
	gen X = _n-1
	gen Y = (binomial(`n', X, `p'))
	forvalues k = 1/`n' { 
		replace Y = (binomial(`n',`k',`p'))-(binomial(`n',`k'-1,`p')) if X == `k'
		}
	scatter Y X if X>1999, sort xtitle("{it:k} (number of successes)" ///
		"i.e., possible sample counts") msymbol(point) mc(black) ///
		title("Binomial distribution for {it:n} = `n', {&theta} = `p'") ///
		ytitle("{it:P}({it:X} = {it:k})") xla(2000(50)2300) ///
		|| 	scatter Y X if X<2087 & X>1999, sort xtitle("{it:k} (number of successes)" ///
		"i.e., possible sample counts") msymbol(point) mc(red) ///
		note("Heights in red correspond to k < 2087") legend(off)

	*grandparents
	use ./data/gss2018, clear
	local var granborn
	local lab : var la `var'
	local datalab : data label
	qui sum `var'
	local xbar = round(r(mean), 0.01)
	local s = round(r(sd), 0.01)
	local note "Source: `datalab', {it:n} = `r(N)'"
	local overline = uchar(773)
	hist `var', title("`lab'") note("`note'") text(0.5 3 ///
		"X`overline' = `xbar'" "{it:s} = `s'", box fcolor(white) ///
		lp(longdash) lc(black)) discrete
	
	* possible worlds e.g.
	use ./data/gss2018, clear
	keep granborn
	drop if missing(granborn)
	sum granborn
	local SE = r(sd)/sqrt(r(N))
	di `SE'
	gen devs = rnormal(0, `SE')
	sum granborn
	gen possiblemu = r(mean) + devs
	forvalues i = 1/4 { 
		local z99 = round(invnorm(.995), 0.01)
		local mu = possiblemu[`i']
		sum gran
		local xbari = round(r(mean), 0.01)
		local s = round(r(sd), 0.01)
		local SE = round(`s'/sqrt(r(N)), 0.0001)
		di `SE'
		local MOE = round(`z99' * `SE', 0.0001)
		local LB = round(`xbari' - `MOE', 0.01)
		local UB = round(`xbari' + `MOE', 0.01)
		local mupos = `mu' - 0.5*`SE'
		local leftcoord = `LB' - .5*`SE'
		di `leftcoord'
		local rightcoord = `UB' + .5*`SE'
		local min = `mu' - `MOE'*2 
		local max = `mu' + `MOE'*2
		local overline = uchar(773)
		local xbarpos = `xbari'-0.5*`SE'
		local mugraph = strltrim("`: display %10.2f `mu''")
		local xbargraph = strltrim("`: display %10.2f `xbari''")
		twoway function y=normalden(x, `mu', `SE'), range(`min' `max') title("") ///
			xtitle("Sample means ({stMono:granborn})") ytitle("") scheme(tufte) ///
			legend(off) xline(`LB' `xbari' `UB', lp(longdash) lc(red)) ///
			text(12 `leftcoord' "LB 99 CI" "{&rarr}") ///
			text(12 `rightcoord' "UB 99 CI" "{&larr}") ///
			text(12 `xbarpos' "X`overline'" "{&rarr}") ///
			text(8 `mupos' "{&mu} = `mugraph'") ///
			xline(`mu', lc(black)) ///
			name(CI`i', replace)
	}
	
	gr combine CI1 CI2 CI3 CI4, ///
		title("Several possible scenarios for a given CI", size(medium))

	* Binomial exact review
	
	clear all
	set obs 5
	local n = 4
	local p = 0.5
	gen X = _n-1
	gen Y = binomial(4, X, 0.5) - binomial(4, X-1, 0.5)
	list X
	scatter Y X, sort xtitle("{it:k} (number of successes)") ///
		title("Binomial distribution for {it:n} = `n', {it:{&theta}} = `p'") ///
		ytitle("{it:P}({it:X} = {it:k})") scheme(tufte)
		
	* Likelihood function 
	
	clear all
	local n = 10
	local k = 7
	range theta 0.01 1 100
	gen Y = binomial(`n', `k', theta-0.01) - binomial(`n', `k', theta)
	scatter Y theta, sort xtitle("{&theta}") ///
		title("Likelihood function for {it:n} = `n', {it:k} = `k'") ///
		ytitle("{it:L}({&theta})") scheme(tufte) name(likelihoodeg)
		
	range k2 0 10 11 
	local p = 0.07
	gen Y_2 = binomial(`n', k2, 0.7) - binomial(`n', k2-1, 0.7) 
	scatter Y_2 k2, sort xtitle("{it:k} (number of successes)") ///
		title("Binomial distribution for {it:n} = `n', {it:{&theta}} = `p'") ///
		ytitle("{it:P}({it:X} = {it:k})") scheme(tufte) name(binomialeg)
		
	gr combine binomialeg likelihoodeg
		
* L20 
	
	* tdistros 
		
	twoway function y=tden(1, x), range(-4 4) ytitle("Density") || ///
	function y=tden(10, x), range(-4 4)  || ///
	function y=tden(100, x), range(-4 4) || ///
	function y=normalden(x), range(-4 4) lc(red) ///
		legend(order(1 "df = 1" 2 ///
		"df = 10" 3 "df = 100" 4 "Normal") ring(0) pos(10) /// 
		region(lp(solid) fcolor(white) lc(black))) ///
		xtitle("Standardized sample means") ytitle("Density")  scheme(tufte) ///
		title("{it:t}-distributions with various degrees of freedom", size(medium)) 

	* Bolivia example
	local df = 100
	local xbar = 24
	local mu = 23
	local s = 8
	local n = 101
	local SE = `s'/sqrt(`n')
	local t = (`xbar' - `mu')/`SE'
	local min = -4
	local max = 4
	local tcoord = `t' + 0.75
	local tgraph = strltrim("`: display %10.2f `t''")
	twoway function y=tden(`df', x), range(`min' `max') ///
		xtitle("{it:t}, standardized sample means (hours to count)") ytitle("") ///
		xline(`mu', lc(black)) scheme(tufte)   ///
		xline(`t', lc(black) lp(longdash))  ///
		title("Sampling distribution under H{subscript:0}") ///
		|| function y=tden(`df', x), range(`t' `max') ///
		recast(area) color(red%70) ///
		legend(order(2 "{it:p}(data this weird | {&mu}{subscript:0})") ///
		ring(0) pos(9) region(lp(longdash) lc(black))) ///
		text(.3 `tcoord'  "{it:t} =`tgraph'" "{&larr}") ///
		|| function y=tden(`df', x), range(`min' -`t') ///
		recast(area) color(red%70) 
	
	di 2*(1-t(100, 1.28))

* Exam II
	* NHANES weighted
	use ./data/nhanes_bigmerge, clear
	svyset sdmvpsu [pweight = wtint2yr], strata(sdmvstra) ///
		vce(linearized) singleunit(missing)
	drop if ridageyr <18
	svy: mean bmxht
	estat sd

	egen systolic = rowmean(bpxsy*)
	drop if systolic >180
	svy: mean systolic
	estat sd
	di (122.65-120)/(16.75/sqrt(5158))

	egen diastolic = rowmean(bpxdi*)
	svy: mean diastolic
	
	use ./data/gss2018, clear
	sum hrsrelax
	di normal((r(mean)-3.75)/(r(sd)/sqrt(r(N))))
	
	sum yearsjob
	y

* L21.5 (two sample t-tests)

	* Stata's default is not to use error bar graphs, which is, as you'll see,
	* a really good idea: it un-automates a very tempting mistake. But let's
	* get those error bars up anyways...
	net install catcibar, from("https://aarondwolf.github.io/catcibar")
	* Load data
	use ./data/gss2018, clear
	* Let's just focus on working class people (I picked this deliberately since 
	* it is a real example where the numbers are borderline). 
	keep if class == 2 & !missing(maeduc) & !missing(paeduc)
		* We'll also drop ^ missings to enforce equal ns for each group. 
	* First, note the overlap among the bars numerically and graphically.

	* We'll do a 68 CI to keep the arithmetic simple; this is common enough. It makes
	* our critical z simply equal to 1. 
	ci mean maeduc paeduc, level(68)
	catcibar maeduc paeduc, cil(68) title("A misleading bar graph") ///
		subtitle("68 CIs for both variables, i.e. each whisker is 1 SE away from the mean") ///
		yla(10(.25)11.5) cio(lc(black)) note("Source: GSS 2018, {it:n} = 635")

	* Now let's get the most exact answer; we'll just directly tell Stata to find a
	* 68 CI for the difference, which happens to be a single-sample CI for a variable
	* that records the difference between variables, demonstrated below. 
	ttest maeduc == paeduc, level(68) // t-test command includes a CI
	gen malesspa = maeduc - paeduc
	ci mean malesspa, l(68) // same results as above

	* We can also show this graphically. Clearly zero is not in the 68 CI. 
	catcibar malesspa, cil(68) yla(-.3(0.1)0.5) ///
		title("A spare but accurate bar graph") ///
		subtitle("Difference between r's mother's and father's education" ///
		"68 CI, i.e. each whisker is 1 SE away from the mean") ///
		xtitle("Mean difference") xlabel(, nolabels) ///
		cio(lc(black)) note("Source: GSS 2018, {it:n} = 635")

	* Let's go ahead and do a two-sample t-test. Probably the easiest way to do it
	* here is to use Stata's "immediate" two-sample t syntax rather than reshape
	* our data. So, we'll do that now. First, we'll get all the relevant numbers
	* into convenient locals. Run everything from here onward as one chunk of the
	* do-file so that the local macros don't expire. 
	foreach var of varlist maeduc paeduc {
		qui sum `var'
		di r(mean)
		local `var'mean = r(mean)
		di r(sd)
		local `var'sd = r(sd)
		di r(N)
		local `var'n = r(N)
		}
	* Then run an immediate two-sample t-test (note the "i" appended to "ttest"). 
	ttesti `maeducn' `maeducmean' `maeducsd' `paeducn' `paeducmean' `paeducsd', level(68) unequal
		* The unequal option is necessary or else Stata uses the incorrect 
		* "pooled" option. 

	* ^ Here, we note that the test-statistic is 1 (rounded to the hundredths), which
	* is substantially larger than what we'd get if we used as the SE the simple 
	* square of the summed SEs (which includes the extra 2*SEm*SEp term). 

	di (`maeducmean' - `paeducmean') /// <- raw diff. 
		/sqrt((((`maeducsd'/sqrt(`maeducn'))+(`paeducsd'/sqrt(`paeducn')))^2)) // <-SE
	* ^ Here, the t-statistic is a paltry 0.7, nowhere near a traditional cutoff. 

	* Finally, let's do the most-correct test, a paired t-test, whose standard error
	* is lowest of all since it estimates the quantity sqrt(var(M) + var(P) - 2cov(M,P))
	ttest maeduc == paeduc, level(68)
	* Here, we have a test-statistic that is significant at a common alpha.

	* Married wage t-test
	
	use ./data/cps2019, clear
	gen marriedwageWI = wage1 if marstat ==1 & state ==35
	bysort female: sum wage1 if marstat ==1 & state ==35
	ttest wage1 if marstat ==1 & state ==35, by(female) unequal
	reg MWWIdiff female

	scalar SE =r(se)
	di invttail(694.738, 0.01)
	scalar tstar=2.3317273
	di (tstar*SE)+2
	scalar criticalvalue = 3.8325557

	twoway function y=tden(694.738, (x-2)/0.785992), range(-2 6) color(dknavy) || ///
	function y=tden(694.738, (x-2)/0.785992), range(2.9884 3.8326) recast(area) color(dknavy) || ///
	function y=tden(694.738, (x-2)/0.785992), range(3.8326 6) recast(area) color(red) ///
	xtitle("{&mu}{subscript:1} - {&mu}{subscript:2}") ///
	ytitle("Density") title("") yscale(range(0 0.4)) ///
	subtitle("One-tailed test and {&alpha}=0.01") ///
	legend(off) xlabel(-1 0 1 2 2.99 3.83) saving(difftwo1, replace)
	
	twoway function y=tden(1, x), range(-4 4) ytitle("Density") || ///
	function y=tden(10, x), range(-4 4)  || ///
	function y=tden(100, x), range(-4 4) || ///
	function y=normalden(x), range(-4 4) lc(red) ///
		legend(order(1 "df = 1" 2 ///
		"df = 10" 3 "df = 100" 4 "Normal") ring(0) pos(10) /// 
		region(lp(solid) fcolor(white) lc(black))) ///
		xtitle("Standardized sample means") ytitle("Density")  scheme(tufte) ///
		title("{it:t}-distributions with various degrees of freedom", size(medium)) 
	
	ttest wage1 if marstat ==1 & state ==35, by(female) unequal
	local df = 100
	local diff = 2.988392
	local SE = r(se)
	local t = r(t)
	local negt = -`t'
	local min = -5
	local max = 5
	local tcoord = `t' + 0.75
	local negtcoord = `negt' - 0.75
	local tgraph = strltrim("`: display %10.2f `t''")
	local negtgraph = strltrim("`: display %10.2f `negt''")
	local mu = 0
	local critt = invttail(`df', 0.005)
	local negcritt = -`critt'
	local crittgraph = strltrim("`: display %10.2f `critt''")
	local negcrittgraph = strltrim("`: display %10.2f `negcritt''")
	twoway function y=tden(`df', x), range(`min' `max') ///
		xtitle("{it:t}, standardized sample mean difference") ytitle("") ///
		xline(`mu', lc(black)) scheme(tufte)   ///
		xline(`t', lc(black) lp(longdash))  ///
		xline(`negt', lc(black) lp(longdash)) ///
		xline(`critt', lc(red) lp(longdash)) ///
		xline(`negcritt', lc(red) lp(longdash)) ///
		title("Sampling distribution under H{subscript:0}") ///
		xla(-5 `negcrittgraph' 0 `crittgraph') ///
		|| function y=tden(`df', x), range(`t' `max') ///
		recast(area) color(red%70) legend(off) ///
		text(.3 `tcoord'  "{it:t} =`tgraph'" "{&larr}") ///
		|| function y=tden(`df', x), range(`min' `negt') ///
		recast(area) color(red%70) text(.3 `negtcoord'  "{it:t} =`negtgraph'" "{&rarr}") 
* L22 Proportions. 

use ~/desktop/soc360fa22/data/cps2019, clear
	sum married
	label define mar 0 "unmarried" 1 "married" 
	label values married mar
	graph bar, over(married) ///
		title("Marital status for all Springfield workers", size(medium)) ///
		ytitle("Percent") 
	graph save married, replace
	tempfile CPSmarstat
	noisily bootstrap samplemeans=r(mean), saving(`CPSmarstat') size(1000) ///
		reps(500): summarize married 
	use `CPSmarstat', clear
	sum samplemeans, d
	hist samplemeans, normal normop(lcolor(black) lp(longdash)) ///
		kdensity kdenop(lc(mint) lp(longdash)) ///
		title("Proportion married of Springfield workers across 500 samples", ///
			size(medsmall)) bin(25) ///
		xtitle("Proportion of sample") ytitle("Density") ///
		note("Source: random subsamples of Springfield, {it:n} = 1,000" ///
		"Black line: Normal curve for reference; blue line: density estimate")
	graph save marriedclt, replace
	gr combine married.gph marriedclt.gph 
	
	use ./data/gss2018, clear
	codebook helpsick /* inspect variable */ 
	gen publichealth = . /* we create a new variable that is an empty shell */
	replace publichealth = 1 if helpsick <6 & helpsick >3 & !missing(helpsick) 
		/* Using the codebook,  we saw that "helpsick" is coded as 4 or 5 if r 
		thinks that the state should not help pay for medical care, so we mark them
		“1” on this new variable “publichealth” since we want the proportion of 
		people who oppose state spending. */ 
	replace publichealth = 0 if helpsick <4 & !missing(helpsick)
		/* We also saw that "helpsick" is coded as 1, 2, or 3 if r think the 
		state should help pay for medical care to an extent, so we mark them 
		zero on this variable.*/
	tab publichealth helpsick 
		/* Our work checks out. See dummy var lecture for why. */ 
	label define ph 0 "state should help" 1 "your problem, buddy" 
		/* We make a label */
	label values publichealth ph 
		/* We apply the label */ 
	tab publichealth 
		/* We show the result*/ 

	sum publichealth
	local sampleprop = r(mean)
	prtest publichealth == 0.15
	local null = 0.15
	local SE = r(se)
	local criticalz = -invnorm(0.005)
	local MOE = `criticalz'*`SE'
	di `MOE'
	local lb = `null' - `MOE'
	local ub = `null' + `MOE'
	di `ub' 
	di `lb'
	local min = `null' - 5*`SE'
	local max = `null' + 5*`SE'
	local negzcoord = `lb' - .5*`MOE'
	local zcoord = `ub' + .5*`MOE'
	local lbgraph = strltrim("`: display %10.2f `lb''")
	local ubgraph = strltrim("`: display %10.2f `ub''")
	local negsampleprop = `null'-(`sampleprop'-`null')
	di `negsampleprop' 
	di `sampleprop'
	local samplegraph = strltrim("`: display %10.2f `sampleprop''")
	local negsamplegraph = strltrim("`: display %10.2f `negsampleprop''")
	local sampleproppos = `sampleprop' + 0.75*`SE'
	twoway function y=normalden(x, `null', `SE'), range(`min' `max') ///
		xtitle("sample proportions") ytitle("") ///
		xline(`null', lc(black)) scheme(tufte)   ///
		xline(`sampleprop', lc(black) lp(longdash))  ///
		xline(`negsampleprop', lc(black) lp(longdash)) ///
		xline(`lb', lc(red) lp(longdash)) ///
		xline(`ub', lc(red) lp(longdash)) ///
		title("Sampling distribution under H{subscript:0}") ///
		|| function y=normalden(x, `null', `SE'), range(`sampleprop' `max') ///
		recast(area) color(red%70) legend(off) ///
		text(40 `sampleproppos'  "sample" "proportion" "{&larr}") ///
		|| function y=normalden(x, `null', `SE'), range(`min' `negsampleprop') ///
		recast(area) color(red%70) xla(0.15, add)
		
	sum publichealth
	local n = r(N)
	local mu = 0.15
	local sigma = sqrt(`mu'*(1-`mu')/`n')
	local min = `mu' - (4*`sigma')
	local max = `mu' + (4*`sigma')
	local criticalvalue = -(invnorm(0.005))
	local LB = round(`mu' - `criticalvalue'*`sigma', .1)
	local UB = round(`mu' + `criticalvalue'*`sigma', .1)
	local xcoordL = round(`LB' -`sigma', .1)
	local xcoordR = round(`UB' +`sigma', .1)
	twoway function y=normalden(x, `mu', `sigma'), range(`min' `max') ///
		xlab(`rangemin'(`sigma')`rangemax') xline(`mu', lpattern(longdash)) ///
		ytitle("Density") xline(`LB', lpattern(dash)) ///
		xline(`UB', lpattern(dash)) || ///
	function y=normalden(x, `mu', `sigma'), range(`min' `LB') ///
		recast(area) color(red%80) legend(order(2 "Middle 80 percent")) ///
		text(0.03 `xcoordL' `"`LB80' {&rarr}"') ///
		text(0.03 `xcoordR' `"{&larr} `UB80'"') ///
		title("Middle 80 percent of the distribution of MCAT scores", size(medium)) 
		
* L 23 Chi-square. 	
	twoway function y=chi2den(1, x), range(0 30) xla(0(2)30) lc(black) ytitle("Density") || ///
	function y=chi2den(2, x), lc(red) range(0 30)  || ///
	function y=chi2den(4, x), lc(orange) range(0 30)  || ///
	function y=chi2den(8, x), lc(yellow) range(0 30) ///
		xla(, grid glc(black%50)) yla(, grid glc(black%50)) scheme(tufte)  ///
		legend(order(1 "df = 1" 2 ///
		"df = 2" 3 "df = 4" 4 "df = 10") ring(0) pos(10) /// 
		region(lp(solid) fcolor(white) lc(black))) ///
		xtitle("{&chi}{superscript:2} test-statistics") ytitle("Density") ///
		title("{&chi}{superscript:2}-distributions with various degrees of freedom", size(medium)) 
	
	local crit = invchi2tail(9, 0.05)
	di `crit'
	set scheme tufte
	twoway function y=chi2den(9, x), range(0 100) lc(black) ytitle("Density") || ///
	function y=chi2den(9, x), recast(area) color(red%50) range(`crit' 100) ///
		legend(order(2 "rejection region") ring(0) pos(2) /// 
		region(lp(solid) fcolor(white) lc(black))) ///
		xline(83.43, lpattern(dash)) text(0.06 75 "test statistic" "= 83.43 {&rarr}") ///
		xtitle("sample {&chi}{superscript:2} test-statistics") ytitle("Density") ///
		title("{&chi}{superscript:2}-distribution under H{subscript:0}", size(medium)) 
		
* L 24 regression. 

	use ./data/gss2018, clear
	reg size educ
	local df = e(N)-2
	local xbar = 4.52
	local mu = 0
	local n = e(N)
	local SE = 8.05
	local t = (`xbar' - `mu')/`SE'
	local min = -4
	local max = 4
	local tcoord = `t' + 0.75
	local tgraph = strltrim("`: display %10.2f `t''")
	twoway function y=tden(`df', x), range(`min' `max') ///
		xtitle("standardized sample regression slopes") ytitle("") ///
		xline(`mu', lc(black)) scheme(tufte)   ///
		xline(`t', lc(black) lp(longdash))  ///
		title("Sampling distribution under H{subscript:0}") ///
		|| function y=tden(`df', x), range(`t' `max') ///
		recast(area) color(red%70) ///
		legend(order(2 "{it:p}(data this weird | {&mu}{subscript:0})") ///
		ring(0) pos(9) region(lp(longdash) lc(black))) ///
		text(.35 `tcoord'  "{it:t} =`tgraph'" "{&larr}") ///
		|| function y=tden(`df', x), range(`min' -`t') ///
		recast(area) color(red%70) 
