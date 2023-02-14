/* II. Two categorical variables. */ 

	/* Sample research query: Does religious sentiment influence an individual's
	approval of capital punishment? */ 

	codebook cappun 
	
	tab relig

	graph bar, over(cappun)

	codebook relig
	
	tab relig

	graph bar, over(relig) /* Wow, this is a mess! Notice that the GSS has
	many different categories of religion. We *will* learn later how to test
	whether the relationship between two polytomous* variables is statistically
	significant or not, but for now, we may want to simplify things. First, 
	let's look at some other options.
	
	*/ 
	
	graph hbar, over(relig, sort(1) descending) 
	
	tab relig cappun, row
	
	/* This is definitely easier to look at. But we might also notice, immediately,
	that many of the values that the variable can take are fairly problematic 
	anyways, because the GSS -- for continuity -- sticks with the same division
	of variables, for the most part, that it has always used, which often reflect
	outmoded understandings 
	
	A lot of research on the unique social structure of the US focuses on the role
	played by Protestant variants of Christianity. "Protestant" is still a 
	somewhat problematic catch-all category, but at the same time, many people
	suggest that all variants of Protestantism have some common features. Let's
	assume this is minimally true and look at this example */

	gen protestant = . /* we create a new variable that is basically an empty shell */

	replace protestant = 1 if relig == 1 /* Using the GSS codebook, we saw that
	"relig" is coded as 1 if r is a Protestant, so our new dummy variable, i.e. 
	a variable which can take on only two values (in this case, 1 if Protestant 
	or 0 if not) is coded 1 for yes if religion was also 1 for Protestant (that 
	both had a numerical value of one is just a coincidence). */ 

	replace protestant = 0 if relig != 1 & !missing(relig) /* All non-Protestant 
	groups take a value of zero on the variable "protestant */ 

	label define protestant1 0 "Not Protestant"  1 "Protestant" /* This part is
	a little technical; it creates a "label variable" called "protestant1" 
	that assigns the real names of the values of our variable, "Not Protestant"
	and "Protestant", to the dummy variable we just created */ 

	label values protestant protestant1 /* this step maps our label variable
	to the actual variable we created */ 

	tab relig protestant /* This two-way table lets us see that 
	our relabeling worked correctly -- no person is in both columns */ 
	
	graph bar, over(protestant) /* We look at our new variable -- looks good */
	
	/* Let's look at some joint distributions */ 

	tab cappun protestant /* here, looking down the columns gives us what we want 
	based on what we assume the causal variable to be. */ 

	tab protestant cappun, row /* it's probably a little more conventional to
	use row conditional probability */ 

	tab cappun protestant, column /* but this works too  
	
	Just for the heck of it, let's see what a way of visualizing a much more 
	general hypothesis -- that religion and capital punishment views are in some
	sense related -- might look like */ 
	
	ssc install spineplot /* Tip: You can install virtually all user-written Stata
	commands using "ssc install [command]" */ 

	spineplot cappun relig /* Uh oh, this is a little messy. */ 
	
	spineplot cappun relig if relig <5 /* We can either go in an manually remove 
	labels or just look at a subset of religions, but notice that this involves
	an implicit change to our thesis. */ 

	spineplot cappun protestant /* This command also works well to show our 
	modified thesis */ 
	
	/* Notice that "spineplot" nicely interprets "cappun"'s values, but some
	other commands will need it to be relabelled. Let's do that now. */
	
	gen cappun2 = . 
	
	replace cappun2 = 1 if cappun == 1
	
	replace cappun2 = 0 if cappun ==2
	
	graph bar (mean) cappun2, over(protestant)
	
	graph bar cappun2, over(protestant)
	
	/* Now we can interpret cappun2's "mean" as a proportion of respondents. 
	Note that we couldn't do that with cappun, the original variable, since it
	was not scaled to be a 0/1 ("dummy") variable. E.g. */ 
	
	graph bar cappun, over(protestant) /* (Don't do this. Do what I did with
	cappun2 above. Just showing you that this is harder to interpret). 
	
* III. Interval/ratio outcome, categorical predictor */ 

	codebook rincom16 /* See above for commentary on codebook command and its uses.
	Notice that this is a very limited range of values. Let's hunt around in the
	codebook to see if anything is a little more detailed. 
	
	OK, rincom16 looks good */ 
	
	tab rincom16
	
	sum rincom16, detail
	
	hist rincom16, percent discrete
	
	/* We might want slightly larger bins ... */ 
	
	hist rincom16, percent bin(20)
	
	/* OK, time for some bivariate analysis */ 
	
	graph bar (mean) rincom16, over(protestant) /* Bar graph of diffs. in group mean 
	shows some evidence of difference */ 
	
	scatter rincom16 protestant || lfit rincom16 protestant /* Notice that this
	looks a little silly because there is a very limited range of x-values possible
	here, but it's not a totally terrible way to visualize this */ 
	
	tab rincom16 protestant /* Notice that this isn't especially informative here
	although we could, in theory, break up income into just a few categories and
	try this command again. That would be an option that would put us back
	into the realm of projects in II. above. */ 
	
	corr rincom16 protestant
	
	reg rincom16 protestant /* Now, notice that we're regressing a quantitative
	outcome variable on a categorical predictor. Is that allowed? Well, usually
	not if the variable is nominal, and we should be extremely careful if the 
	variable is ordinal. But what if the variable just has *two* possible values?
	It turns out that, even if the variable is nominal, this works out (and in
	fact it is actually identical to taking the group difference in means between
	each group, which is of course a very simple technique). This is known as
	"regression on a dummy variable". How do we interpret the coefficient here?
	The same as how we always do it: it's the change in the outcome variable
	associated with a unit change in the predictor. We just slightly modify
	that logic here: the coefficient represents the change in the outcome 
	associated with a move from the "reference category" (whatever the meaning
	of the value 0 is on our dummy variable, which would here be "NOT PROTESTANT")
	to the value that is 1 on our dummy variable, which is conventionally coded
	to mean "presence of the thing named in the variable" -- here, that the
	respondent IS A PROTESTANT. This is also the group difference in means. 
	
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
