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

	cd /users/gjmb/desktop/soc360fa22 
	
	use ./data/GSS2018, clear

	log using ./code/logs/360qualprojects, text replace 
	
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
		* column titled "cumulative" -- although note that this is not in any 
		* sense a very meaningful CDF since the ordering of the outcomes does
		* not matter. 

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
	
	sum protestant // looks like about 49 percent of the sample were protestant.
		
	* We can now go ahead and look at this simplied output. 
	* It's probably a little more conventional to use row conditional probability...

	tab protestant cappun, row 

	tab cappun protestant, column // ... but this works too  
	
	* What do we make of this? Protestants generally seem to favor the death
	* penalty when a defendant is convicted of murder. 
	
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
	other commands we'll use later need it to be relabelled. Let's do that now 
	with convenient -recode- command (there are dozens of ways to do this). 
	Ensuring that we have a true dummy coding of our variable. */
	
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
	
	* Let's get a cool scheme going. I personally prefer the graphics that R
	* gives in its ggplot2 package, so let's imitate them in Stata. 
	
	net install schemepack, ///
		from("https://raw.githubusercontent.com/asjadnaqvi/Stata-schemes/main/schemes/") replace
	set scheme gg_hue 
	
	* Now let's produce the graph. 
	
	qui tab deathpen protestant
	
	spineplot deathpen protestant, ///
		title("Relationship between religion and opinion of death penalty", ///
		size(medium)) note("Source: GSS 2018, {it:n} = `r(N)'." ///
		"Note: context is for a defendant convicted of murder") ///
		ylab(0(.1)1, axis(2)) xlab(0(.1)1, axis(1)) xtitle("", axis(2)) ///
		xtitle("fraction", axis(1)) ytitle("Approval (fraction)", axis(2)) ///
		legend(ring(0) pos(10) region(lp(longdash) lc(black)) fcolor(white))
		
		* Quick review of what's happening here. We add a title, modify its 
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
	
* III. Interval/ratio outcome, categorical predictor */ 

	* Let's suppose that we're interested in the relationship between income
	* and whether or not someone is Protestant. 

	* UNIVARIATE ANALYSIS.
	
	* If you end up doing this type of project, please have a quick look at the
	* material on the *univariate analysis* from the regression video (for the
	* quantitative variable) and the *univariate analysis* from the 2x2 table
	* video (for the qualitative variable). I'll be quite concise here. 
	
	* For qualitative variables, univariate analysis is simple: we really just
	* want to look at the frequency table and produce some bar graphs. 
	
	* Univariate numeric analysis.
	
	// We'll first print some basic information about our variable's meaning. 
	
	codebook rincom16
	
	* Now let's tabulate our vaiable. This is a bad idea with truly quant.
	* vars., but as we'll see, the GSS measures income in a funny way that
	* causes it *not* to be a real continuous variable. But, since many people
	* will want to use this, I am licensing its use as a quantitative var. 
	
	tab rincom16
		
		* We can see that the researchers used some rather unwieldy buckets. 
	
	* We can print our standard summary statistics, though I want to issue the
	* warning that these are not as meaningful for income on the GSS. 
	
	sum rincom16, detail
	
	* I've already covered in a previous video both the univariate analysis of
	* a qualitative variable in general and the creation and interpretation of 
	* a dummy variable; please go see that video and do-file now. 
	
	* The main things to note here are that 1) you don't *need* a dummy variable
	* in this context, but it might help; 2) the interpretation of a dummy
	* variable's summary statistics are as follows: 1) the mean is the proportion
	* of observations taking on a 1; 2) the variance is roughly the sample 
	* proportion p * (1-p) (with a small bias term that fades as n--> OO). Again
	* that last property is more important for a proof of what it means to regress
	* a quantitative variable on a dummy. 
	
	sum protestant
	
	* Univariate graphic analysis.
	
	* We can consider our income variable here as (at best) a highly-discrete 
	* quant. var. and thus put together a discrete percent histogram. 
	
	hist rincom16, percent discrete
	
	* Sometimes, you'll want to toggle the bin width. 
	
	hist rincom16, percent bin(20)
	
	* We can also look at a boxplot for income. 
	
	graph box rincom16
	
	* Finally, a simple bar graph will do for -protestant-. 
	
	graph bar, over(protestant) 
	
	* BIVARIATE ANALYSIS.
	
	* Bivariate numeric analysis. 
	
	* In the case of a qualitative predictor and a quantitative outcome, we're 
	* effectively going to take means for each group and compare them. This is
	* not a very complex idea, fortunately, though I will show you that it is
	* actually conceivable as a subset of some more-complex ideas we know about.
	
	* Let's start by finding the conditional means. There are a number of ways
	* to do this in Stata; probably the simplest is to put them into a table.
	* Here, I'm recommending the "version 16" option: Stata degraded the quality
	* of the -table- command in version 17 (IMHO). 
	
	vers 16: table protestant, c(mean rincom16)
	
	* It is also perfectly meaningful to take the correlation coefficient in the
	* case of a dummy variable and a quant. outcome. Let's do that now. 
	
	corr rincom16 protestant
	
		* OK, we find that the correlation is not that strong. That's not too
		* big a surprise; we just report our findings honestly and move on. 
		
	* Note that we can also use regression to directly recover the difference in
	* means and mean of the reference group (allowing us to find the mean of the
	* "included group" (the 1s). Remember to NOT do this if the predictor is 
	* truly nominal; we should be extremely careful if the variable is ordinal, 
	* which is true of income here.
	
	reg rincom16 protestant 
	
	* How do we interpret the coefficient here? The same as how we always do it: 
	* it's the change in the outcome variable associated with a unit change in
	* the predictor. We just slightly modify that logic here: the coefficient 
	* represents the change in the outcome associated with a move from the 
	* "reference category" (whatever the meaning of the value 0 is on our dummy 
	* variable, which would here be "NOT PROTESTANT") to the value that is 1 on 
	* our dummy variable, which is conventionally coded to mean "presence of the 
	* thing named in the variable" -- here, that the respondent IS A PROTESTANT.
	* This is also the group difference in means. 
	
	* An intuitive justification for this: if we only want our regression line 
	* to pass through two points, the least-squares minimizing value of Y,
	* conditional on X, is of course just the mean. You can also think about the
	* algebraic proof, shown in my lectures above. The trick is to realize that
	* B-hat can be written as COV(X, Y)/VAR(X). Then, recall that VAR(X) is almost
	* p*(1-p). Partition the covariance into two sums; one for the 0s and one for
	* the 1s. The sum for the 0s is -p*(n for 0s)*(Ybar for 0s). The sum for the 
	* 1s is 1-p*(n for 1s)*(Ybar for 1s). Rewrite the subgroups as respectively
	* (1-p)*(n) and (p)*(n). The rest is algebra. 
	
	* Bivariate graphical analysis. 
	
	graph bar (mean) rincom16, over(protestant) /* Bar graph of diffs. in group mean 
	shows some evidence of difference */ 
	
	scatter rincom16 protestant || lfit rincom16 protestant /* Notice that this
	looks a little silly because there is a very limited range of x-values possible
	here, but it's not a totally terrible way to visualize this */ 
	
	tab rincom16 protestant /* Notice that this isn't especially informative here
	although we could, in theory, break up income into just a few categories and
	try this command again. That would be an option that would put us back
	into the realm of projects in II. above. */ 
	
	
	
	
IV. Categorical outcome, interval/ratio predictor */ 

	/* Let's have a look at an interesting categorical outcome: job satisfaction */ 
	
	codebook satjob /* See above for codebook information */ 
	
	tab satjob
	
	sum satjob, detail /* Again, this isn't super helpful even for an ordinal
	categorical variable */ 
	
	graph bar, over(satjob) /* Wow, people felt optimistic in 2018! */ 
	
	/* What if we consider making job satisfaction into a dummy variable? It
	throws out some information but allows us some easier analyses */ 
	
	gen jobsat = . /* See above for creating dummy variables */ 
	
	replace jobsat = 1 if satjob <3
	
	replace jobsat = 0 if satjob >2 & !missing(satjob)
	
	label define js1 0 "unsatisfied" 1 "satisfied"
	
	label values jobsat js1

	tab jobsat satjob /* again, just checking our work here */  
	
	/* We can even try correlation and regression here using our original 
	income category, although regression on a binary outcome variable is 
	tricky and forms a major area of higher level study in 
	applied statistics */ 
	
	corr jobsat rincom16
	
	reg jobsat rincom16 /* There's some debate about whether we should use 
	ordinary regression on so-called "limited dependent variables", but it
	is used by some serious scholars and it's fine to use at this level
	of the course. How would you interpret the coefficient? (Think about
	the language of probability of observing the outcome). */










