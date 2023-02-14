/*	
SOC 360, Fall 2022, DAP I.	
task: demonstrate analysis of qualitative varaibles
author: Griffin JM Bur, 2022-10-10 */

********* DAP I *********


************ Coding etiquette / basic set-up ************ 

	/* See the first video in this tutorial series for an explanation of all
	of these steps. */
	
	capture log close 

	clear all 
		
	set more off 

	cd ~/desktop/soc360fa22 
	
	use ./data/GSS2018, clear

	log using ./360qualprojects, text replace 
	
/* II. Two categorical variables. */ 

	* Let's begin with the simplest case involving qualitative variables, that 
	* of the analysis of two categorical variables. Let's suppose that we're 
	* interested in the analysis of religion and one's opinion of capital 
	* punishment. We can begin by inspecting the variables individually. 
	
	* UNIVARIATE ANALYSIS.
	
	* For qualitative variables, univariate analysis is simple: we really just
	* want to look at the frequency table and produce some bar graphs. 

	codebook relig 
		
		* How much information this provides depends somewhat on the data-set;
		* for the GSS, this is relatively informative. It "does what it says
		* on the tin": produces a brief simulation of the print or PDF codebook.
	
	tab relig
		
		* This produces a frequency table with 1) raw frequencies, 2) an estimate
		* of the PMF (the percentages), and 3) an estimate of the CDF (the 
		* column titled "cumulative".)

	graph bar, over(relig) 
		
		* Wow, this is a mess! Notice that the GSS has many different categories
		* of religion. We *will* learn later how to test whether the relationship 
		* between two polytomous* variables is statistically significant or not, 
		* but for now, we may want to simplify things. We'll get to that after we
		* look at the distribution of our outcome variable, however. In the 
		* mean-time, a simple graphical improvement is...
		
	graph hbar, over(relig, sort(1) descending) 
		* This rotates the graph 90 degrees clockwise and orders the bars. 
		
		* For an explanation of how to get this graph looking *vastly* nicer, 
		* please see the video entitled. I'll show an example at the end, but
		* in the interest of time--and not overwhelming anyone--I am going to 
		* produce simple graphs that resemble the ones you can make for now.
		
	* Now let's return to an analysis of the capital punishment variable before
	* going onwards. I'm leaving the conclusions out of the do-file; they're 
	* pretty obvious in the case of simple qualitative variable. 
	
	codebook cappun
	tab cappun
	graph bar, over(cappun)

	* BIVARIATE ANALYSIS
	
	* Bivariate numeric analysis.
	
	* A two-way table (AKA contingency table or cross-tabulation) is the most 
	* obvious choice for the analysis of two qualitative variables. We can first
	* pull up the table with raw frequencies only...
	
	tab relig cappun
	
		* N.b. that the transposition of rows and columns is purely aesthetic
		* until we calculate percentages. E.g., what follows is equally acceptable.
		
		tab cappun relig
	
	* But, we should now think about calculating conditional probabilities for
	* the values of the predictor, Pr(Y = y | X = x). We can do that by simply
	* adding percentages (sometimes called "percentaging the table") with the
	* addition of the option ", row" or ", column" or both. But, a good rule of
	* thumb is to calculate probabilities conditional on the thing that is the
	* more-plausibly causal variable -- even though doing it the opposite way
	* is not disastrous (it does ask that the reader be comfortable comparing
	* conditional and marginal distributions). 
	
	* Since religion is more plausibly the cause of opinions on capital punishment,
	* let's now calculate row conditional probabilities. 
	
	tab relig cappun, row
	
	/* This is definitely easier make sense of. But we also notice, immediately,
	that many of the values that the variable can take are fairly problematic 
	anyways, because the GSS -- for continuity -- sticks with the same division
	of variables, for the most part, that it has always used, which often reflect
	outmoded understandings (e.g., some of the lumping of "non-Western" religions,
	the spelling of "Muslim" which is now outdated). 
	
	A lot of research on the unique social structure of the US focuses on the role
	played by Protestant variants of Christianity. "Protestant" is still a 
	somewhat problematic catch-all category, but at the same time, many people
	suggest that all variants of Protestantism have some common features. Let's
	assume this is minimally true and look at this example. 
	
	To do that, we'll need to make dummy variables. Here is a fast way to do
	that using Boolean expressions. */
	
	* First, we'll tell Stata to make a variable that records response to the 
	* logical statement "relig == 1", i.e. that the person is Protestant. But
	* what if we don't know the numeric values that correspond to the value-labels
	* of our variable? To find that out, let's first -describe- the variable...
	
	describe relig
	
	* ... notice and then display the value label (for a value-label, you need
	* to use the specific command -label list-. * 
	
	label list RELIG
	
	* ... note that the numeric value of Protestant is 1 and craft our variable.
	
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
		
		* This is a good sign: everyone who was not a Protestant but had an 
		* observed religion was assigned to "non-Protestant"; everyone who was
		* a Protestant ended up a Protestant; all missings remained missing 
		* ("DK" and "NA" mean "don't know" and "not applicable"). 
		
	* We can wrap up the analysis by recalling that a dummy variable's summary
	* statistics are easy to interpret (unlike those of a nominal variable): the
	* sample mean is the proportion of people in the sample whose outcome on the
	* variable is 1, while the variance is nearly p (the mean) * 1-p (there's a
	* small bias term that disappears with sample size; see my lectures if you
	* really want to know more). 
	
	* This property of the variance is more important in proving a fact about
	* regression on a dummy variable that becomes useful to know later. 
	
	sum protestant // looks like about 49 percent of the sample were Protestant.
		
	* We can now go ahead and look at this simplied output. 
	* It's probably a little more conventional to use row conditional probability...

	tab protestant cappun, row 

	tab cappun protestant, column // ... but this works too  
	
	* What do we make of this? Protestants generally seem to favor the death
	* penalty when a defendant is convicted of murder. 
	
	* One more important note here. If you *do* want to make a variable that
	* you can't easily express in terms of binary conditions, it might be simpler 
	* to try the following method. Let's say we want to make a variabl that
	* indicates whether someone has *ever* been married (which you *could*
	* indicate with Boolean expressions, but let's pretend otherwise for a 
	* second). Then, we could say...
	
	d marital
	label list MARITAL // only fives are never married
	gen ever_married = . // we first make a new, "empty shell" variable
	replace ever_married = 1 if marital < 5 
		// we pile all people numbered 1-4 into the 1s.
	replace ever_married = 0 if marital == 5 
		// We write this very precisely -- since we're taking the time to write
		// longer expressions, let's save ourselves the hassle of having to later
		// reassign missings (see above on this). 
	* Now we'll make a value label and check our work. 
	label define EM 0 "never married" 1 "has been married"
	label values ever_married EM
	tab marital ever_married, mis
	
	* Bivariate graphical analysis.
	
	* Generally, spine-plots compress the most information here. If you have two
	* qualitative variables which have relatively-few possible outcomes y, this
	* is often a good choice. 
	
	* First, let's install it. This works for just about any command you might
	* have heard about which doesn't ship with Stata. 
	
	ssc install spineplot // no trouble if already installed, BTW. 
	
	spineplot cappun protestant 
	
		* This is an effective visualization in this case. Note that using the
		* original variables produces a real eye-sore. That's one more reason to
		* think about effective information compression (which is, in one way,
		* what *almost all* data analysis does--it might sound negative, but 
		* there's generally a sweet spot between oversimplifying data and drawing
		* wild conclusions and "overfitting" the data and taking every separate
		* point as though it is manna rather than a realization of a random variable). 
		
		spineplot cappun relig
			* --> again, do not do this. 
	
	/* Notice that "spineplot" nicely interprets "cappun"'s values, but some
	other commands we'll use later need it to be a true dummy variable. Right now,
	it is a dichotomous variable, but not one whose numeric outcomes are the 
	numbers 0 and 1. Let's do that now with convenient -recode- command (this is
	yet one more way of making a new variable). */
	
	* General syntax: recode oldvar (numeric rule: oldvalue = newvalue "label"),
		* gen(newvar) [I would not recommend overwriting the old variable]. 
	
	recode cappun (1 = 1 "favor") (2 = 0 "oppose"), gen(deathpenalty)
	
	tab protestant deathpen, mis 
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
	
	* Let's get a cool scheme going. I personally prefer the graphics that R
	* gives in its ggplot2 package, so let's imitate them in Stata. 
	
	net install schemepack, ///
		from("https://raw.githubusercontent.com/asjadnaqvi/Stata-schemes/main/schemes/") replace
	set scheme gg_hue 
	
	* Now let's produce the graph. 
	
	qui tab deathpen protestant
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
	
