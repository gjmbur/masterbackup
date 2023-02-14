* Here are some useful schemes (IMHO)
ssc install scheme_tufte
net install schemepack, ///
		from("https://raw.githubusercontent.com/asjadnaqvi/Stata-schemes/main/schemes/") replace

use gss2018, clear

* Quant/quant outcome. 

scatter educ paeduc, jitter(5) mcolor(red) legend(off) ///
		xtitle("Father's education (years)") ///
		ytitle("Respondent's education (years)")  ///
		title("Relationship of father's and child's education", size(medium)) ///
		note("Source: GSS 2018. {it:n} = 1687") ///
		text(5 15 "Estimated regression equation" ///
		"{it:Y} = 0.29{it:X} + 10.67" ///
		"{it:R} {superscript:2} = 0.17", box fcolor(white)) || lfit educ paeduc
		
		* First, we explain jitter above. "mcolor(colorname)" sets the color
		* of markers; a list of colors you can invoke is here: 	
			* https://www.stata.com/manuals/g-4colorstyle.pdf#g-4colorstyle
		* "legend(off)" removes the legend, which is generally not too helpful
		* relative to simply labeling the axes, which is what "xtitle("text")",
		* etc. do. "note("text")" adds an explanatory note to the graph, while
		* the option "text(ycoord xcoord "line1text" "line2text") puts text inside
		* of the graph itself, which helps economize on space. Finally, the use
		* of braces, e.g. {it:text} puts text in italics, superscripts text, etc.
		
* Qual outcome, qual predictor. 

	* First we make the dummy variable. 
	gen protestant = relig == 1 
	
	* Important point here: in virtually all programming languages, a single
	* equals sign is the assignment operator--it *tells* the computer to make
	* A equal B--while a double-equals sign is the evaluation operator (it checks
	* whether some condition is true). Very common source of bugs in code IME. 
	
	* Now, all we need to do is remember the key "gotcha" about missing values
	* in Stata: they are sometimes understood by Stata as arbitrarily-large
	* constants. So, people who are *not* Protestants because their religion
	* was missing should not be assigned to Protestant or not; they should 
	* remain missings. 
	
	replace protestant = . if missing(relig)
	
	* Finally, to prettify things, let's make a new value label for our new
	* variable. The general syntax is ... 
	
	* label define valuei "labeli"
	* and when finished...
	* label values varlist valuelabel

	label define protestantlab 0 "non-Protestant"  1 "Protestant" 
		* You can technically make the value-label the exact same thing as the
		* variable name because Stata stores these and variables in separate
		* "spaces", but I find it easier to make it clear which is which.

	label values protestant protestantlab
	
	* Finally, let's check our work. Here, we're going to cross-tab the old and
	* new variable. Of course, this is not really an *analysis* -- these two 
	* vars basically encode the same information -- but it is a way to make sure
	* that we haven't goofed up. 
	
	* We'll also add the ", mis" option to include missings (usually excluded by
	* default) to make sure that all MVs were handled correctly. 
	
	tab relig protestant, mis 
	
	* Now let's produce the graph. 
	
	/* Notice that "spineplot" nicely interprets "cappun"'s values, but some
	other commands we'll use later need it to be a true dummy variable. Right now,
	it is a dichotomous variable, but not one whose numeric outcomes are the 
	numbers 0 and 1. Let's do that now with convenient -recode- command (this is
	yet one more way of making a new variable). */
	
	* General syntax: recode oldvar (numeric rule: oldvalue = newvalue "label"),
		* gen(newvar) [I would not recommend overwriting the old variable]. 
	
	recode cappun (1 = 1 "favor") (2 = 0 "oppose"), gen(deathpenalty)
	
	tab cappun deathpen, mis 
		// we check our work in the same manner as above: loks good. 
	
	/* Now we can interpret our variable's "mean" as a proportion of respondents
	and the variance as roughly an estimate of p(1-p) with a slight bias term
	that I discuss in my lecture but which is negligible in practice. 
	
	Note that we couldn't immediately do that with cappun, the original variable, 
	since it was not scaled to be a 0/1 ("true dummy") variable. Again, leaving
	it in non-dummy form is fine for many purposes, but it is inconvenient for
	some. Best practice: use true Boolean variables: 0 means "absence of property
	implied by variable name", 1 means "presence of property implied". 
	
	You should generally use true dummy variable coding if you have any variables
	that are binary in the literal sense; it's a good idea to recode them.
	
	Let's wrap up with a bit of toggling of our key graph. Again, I explain the
	basic outlines in the "publication-quality graphics" video. 
	
	But, one trick here is that -spineplot- is a user-written command that is a
	bit wonky. So, we might want to edit our variable labels to help us avoid
	too much toggling of the graph later. */ 
	
	label var deathpen "Opinion of death penalty in case of murder"
	
	tab deathpen protestant
		* Let's get some useful return values into Stata's working memory. 
	spineplot deathpen protestant, ///
		title("Relationship between religion and opinion of death penalty", ///
		size(medium)) note("Source: GSS 2018, {it:n} = `r(N)'." ///
		"Note: context is for a defendant convicted of murder") ///
		ylab(0(.1)1, axis(2)) xlab(0(.1)1, axis(1)) xtitle("", axis(2)) ///
		xtitle("fraction", axis(1)) ytitle("Approval (fraction)", axis(2)) ///
		legend(ring(0) pos(10) region(lp(longdash) lc(black)) fcolor(white))
		
	
		* Quick reivew of what's happening here. We add a title, modify its 
		* size (note the multiple layers of parentheses), add a note with a source
		* which refers to the most recent command (which is why I quietly ran
		* a cross-tab--to get r(N) in memory), make the Y-axis ticks more precise
		* (note that in this case, we have two labeled X and Y axes, so we need
		* to specify the axis.  I also omit the uninformative lower X-title,
		* change the top one to be more concise, and do the same for the left Y.
		* Finally, I move the legend inside the figure ("ring(0)") to economize
		* on space, put it at 10 o'clock ("pos(10)") add a border around it
		* ("region(linepattern(style)) linecolor(color)") with dashed lines 
		* and finally color it white for contrast.
		
* Qual predictor, quant outcome

	tab protestant, sum(rincom16)

	* Finally, let's show the "professional version". See graphics video for
	* guidelines on how to produce something like this. 
	
	count if ~missing(protestant) & ~missing(rincom16)
	graph hbar (mean) rincom16, over(protestant) ///
		title("Relationship between religion and income (GSS scale)", ///
		size(medium)) note("Source: GSS 2018, {it:n} = `r(N)'.") ///
		ylab(0(1)20) ytitle("Mean income (GSS scale)") 
		
		* The main things to note here: first, as noted above, graph bar 
		* interprets the main argument as the thing to put on the Y; here, we
		* actually *do* want that filled in, with the conditioning variable
		* being protestant. Then, we add a title (modifying the size), make the
		* Y-axis go from 0 to 20 in increments of 1 and change the Y-title. 
