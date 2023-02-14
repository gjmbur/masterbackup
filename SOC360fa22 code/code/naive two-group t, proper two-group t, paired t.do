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
scalar 
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


// Provisional -- I don't think I'm actually estimating the COV(Ybar, Zbar) right>>
* We can note that the SE for the difference is smaller than the SE for the 
* two-sample t-test by roughly 2COV(SE)
gen mapa = maeduc*paeduc
qui sum mapa
local Emapa = r(mean)
qui sum maeduc
local Ema = r(mean)
qui sum paeduc
local Epa = r(mean)
local covmapa = `Emapa' - (`Ema'*`Epa')
di `covmapa'

