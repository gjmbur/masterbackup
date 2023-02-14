/*	
SOC 360, Fall 2022, DAP II.	
task: demonstrate analysis of qualitative varaibles
author: Griffin JM Bur, 2022-11-30 */

************ DAP II ************

************ Header ************ 

	/* See the first do-file in this tutorial series for an explanation of all
	of these steps. */
	
	capture log close 

	clear all 
		
	set more off 

	cd ~/desktop/soc360fa22 
	
	use ./data/GSS2018, clear

	log using ./DAPIIsamplecode, text replace 
		* You should rename this to match your file. 

/* I. Quantitative outcome, quantitative predictor

	i. Univariate analysis

		This is the simplest case, though it is worth looking at this for 
		all students since a) regression is the most common technique, by far, at
		higher levels and b) some of the techniques below build on it. 

		Regression is most useful when at least the outcome variable is 
		quantitative. In such a case, we can do some simple inference about 
		each variable's population mean before looking at the relationship 
		between the variables.

		Let's revisit our simple, very common example from Part I. 
		(Recall that, in that .do-file, I showed you all how to 
		regress respondent's education on father's education). Let's do some 
		inference about the likely population parameter for each variable. 

		Typically, a confidence interval is best for doing inference about the 
		likely population parameter's value unless we have a specific reason 
		to test some specific hypothetical value of the population. That kind
		of test is, in this case, more common in the bivariate analysis, 
		where we typically report both the t-statistic and the CI. Usually a CI 
		is sufficient for the univariate analysis with regression. */

		ci mean educ

		ci mean paeduc

		/* What do these intervals mean? We are using a procedure that captures 
		the population parameter in its bounds 95 percent of the time (in the
		long-run),and we can say that the likely population parameter for 
		education, e.g. -- here, the mean --  lies between 13.6 and 13.9 years. 
		For fathers, it lies between 11.7 and 12.1.
		
		You can change the level of confidence by using the , level(PERCENT) 
		option, e.g. */ 
		
		ci mean educ, level(80) 
		
		/* If we want to, we can check our work by hand. First, we grab the sample
		statistic (I'll just show this for education since the procedure is 
		the same in both cases). This is optional; you can conduct this test
		by hand if you like: we will consider it to be evidence that you went
		"above and beyond" on the project.*/  
		
		sum educ 
		
		/* OK, we can see the mean and standard deviation there. First, I'll show
		how to do this using return values. */ 
		
		di r(mean) - invttail(2344, 0.025)*(r(sd)/sqrt(r(N)))
		di r(mean) + invttail(2344, 0.025)*(r(sd)/sqrt(r(N)))
		
		* And, here's how to do this by hand copying down the values by hand. 
		* Since our n is very large, we can just the Normal approximation. 
		
		di 13.73177 - 1.96*(2.974313 /sqrt(2345)) 
		
		di 13.73177 + 1.96*(2.974313 /sqrt(2345)) 
	
	/* ii. Bivariate analysis

		OK, let's do regression for our analysis of the relationship between
		the two variables.

		(As a friendly reminder, the idea of regression can be traced all the 
		way back to two of our simplest measures of a distribution, the mean 
		and the standard deviation. Correlation is basically just the 
		covariance of two variables, only divided by the product of their SDs. 
		The regression slope is then the correlation multiplied by SDy/SDx, where 
		y is whatever variable you consider the outcome and x is whatever 
		variable you consider the predictor). 

		Now, we're going to see how Stata reports some useful statistics when we
		tell it to regress father's education on education. We haven't yet had our 
		lecture on doing inference from regression, but the concept is 
		a very straightforward extension of what we already know about 
		inference. Let's get the output up first. */ 

		reg educ paeduc

		/* Now, notice how, in the output, we see a t-statistic on both the
		educ coefficient and the "constant coefficient"? (this apparent oxymoron
		makes sense if you know a bit of linear algebra; if not, don't worry)
		And notice how they each have a unique p-value as well?  Well, it turns 
		out that (forgive the all-caps; this is a key point)...
		
		REGRESSION COEFFICIENTS, JUST LIKE MEANS AND PROPORTIONS, ARE SAMPLE 
		STATISTICS THAT CAN BE CONSIDERED TO HAVE BEEN DRAWN FROM 
		NORMAL-APPROXIMATING SAMPLING DISTRIBUTIONS. 
		
		THEREFORE, WE CAN CONDUCT HYPOTHESIS TESTS ON WHETHER A REGRESSION 
		COEFFICIENT DIFFERS FROM SOME HYPOTHETICAL VALUE, SAY, ZERO. THE T-STAT 
		IN STATA OUTPUT SIMPLY TELLS US THE TEST-STATISTIC -- THE STANDARDIZED 
		VALUE OF OUR ACTUAL SAMPLE STATISTIC, IN THIS CASE THE REGRESSION 
		COEFFICIENT -- FOR THE NULL HYPOTHESIS THAT THE POPULATION REGRESSION 
		SLOPE IS ZERO. THE P-VALUE CAN BE IMMEDIATELY INTERPRETED AS A 
		TWO-TAILED HYPOTHESIS TEST (to get a one-sided, just divide the p-value 
		reported by Stata by two, though I am unofficially discouraging you all 
		from using one-sided hypothesis tests -- at least report both p-values). 

		Notice also that Stata automatically reports confidence intervals. I'd 
		like you all to report and interpret these if you do regression. 

		So, let's interpret both results. We observe from an SRS 
		of size 1687 a regression slope of 0.29 when r's education is regressed 
		on father's education ; this would virtually never occur if the slope in 
		the population were zero, i.e. the two variables had no linear 
		relationship. Using a procedure which captures the true population 
		parameter in its bounds 95 percent of the time, we can say that the likely 
		range of values for the population parameter is 0.26 to 0.32. In other 
		words, a unit change in the years of education that a respondent's father
		has changes their education by somewhere between 0.26 and 0.32 years with
		95 percent confidence. 
		
		Our interpretation of R-squared does not change: variation in father's ed
		explains about 17 percent of variation in respondent's ed.

		Easy enough! That's inference from regression in a nutshell. It is 
		used constantly by researchers in the social sciences. You will likely
		use it more than the following tests if you go on to do further 
		statistical work. To export your results, there are a variety of fairly
		clunky user-written routines, but at this level, copying and pasting the
		relevant components of the output as we did for part I. */
		
		* FOR GRAPHS, SEE DAP I CODE. 

/* II. Categorical outcome, categorical predictor

	1. Two possible values of outcome and of predictor
	
	i. Univariate analysis, two possible values of two categorical vars (a 
	situation that we can refer to as having two dummy variables). 
	
		Let's suppose here that we either do not have a dummy variable
		to begin with, or that we do have a dummy variable, but it is not
		coded as taking on values of zero and one in the GSS file. This is 
		an easy fix. Remember that a dummy variable is one which takes on
		only one of two possible values, 0 and 1, which stand for SOME KEY
		QUALITATIVE VALUE (coded as 1) and everything but that (coded as 0). 
		The variable might naturally split into two groups like that -- e.g., 
		was r alive in 1940 -- or it might be a reasonable reconfiguration 
		of such a variable. 

		Let's work with the subjective class variable, which is just given to us
		directly by the GSS, and let's try to relate it to home ownership. 
		
		Let's change class into a dummy from its four-possible-value form. 
		Here, I've decided to brick the three "popular classes" -- lower, 
		working, middle -- into one group and to separate out the elite. */ 

		gen realclass = class <= 3
		replace realclass = . if missing(class)
		tab class realclass, mis
		
		/* Notice that what we're doing here, in general, is assigning all
		existing values of a variable to either "1" or "0" to a new variable.
		You should assign values in a way that makes sense by grouping like
		items together: e.g., here, I have put the three "lowest" classes 
		together and separated out the highest class according to a social 
		theory that  says that many self-identified middle-class people are 
		in fact working class and many middle-class people are involved in 
		certain relationships with big capital that resembles the 
		worker-capitalist polarity.
		
		Any variable for which you are especially interested in ONE value
		can be turned into a dummy in this way; see the lecture on dummy 
		variables or the first .do-file for an example of this using the 
		religion variable and the new dummy variable "protestant". 

		Let's get those labels looking nice and clean. I recommend doing 
		this both for aesthetic reasons and so that you don't have to keep 
		the meaning of 0 and 1 in your head the whole time. [I used to get
		griped at for not doing this. Learn from my mistake :) ].*/

		label define classkey 0 "elite" 1 "popular classes"
		label values realclass classkey 
		
		* Let's look at the home ownership variable. 
		
		codebook dwelown

		/* OK, now notice that although our outcome is almost a
		binary variable, it's not in "classical" dummy format even if
		we simply drop the very small "other" category -- this is because the
		numerical values for the categorical outcomes are not
		0 and 1, and therefore Stata gets grumpy with us when we try to use
		this variable in a proportion test. 
		
		Simple fix: recode the variable. Here, we're sort of just ignoring
		the possible value "other", which is unfortunate but probably the
		least of all evils here since there is not much making head or tail of 
		it. Here's another way to make a dummy variable in Stata for reference. */ 

		gen homeowner = 1 if dwelown == 1
		replace homeowner = 0 if dwelown == 2
		label define homeowner2 0 "renter" 1 "owner"
		label values homeowner homeowner2

		tab homeowner dwelown /* Remember that this is just a check to make 
		sure that we did our re-code correctly. It is NOT a real two-way
		table because it is not two different variables, but one variable 
		re-coded. Use this to check your work *only*.
		
		Now, we want to finally get around to doing inference about the value
		of each parameter. In both the case of our outcome and our predictor,
		doing a proportion test makes the most sense. 
		
		So, let's get the tests for each proportion going. */ 
		
		ci proportion realclass, wald /* You can ignore the meaning of the 
		"wald" option here; it just tells Stata to calculate the CI in the way
		that we have been doing by hand. Here, our proportion is pretty close
		to 100, but the standard error is very small, so the approximation isn't
		far off. */ 
		
		ci proportion homeowner, wald
		
		/* How to interpret each test? Using a procedure that captures the 
		population parameter in its bounds 95 percent of the time (in the
		long-run), we can say that the likely population parameter for 
		class -- here, the proportion that is part of the self-identified
		popular classes -- lies between 95.6 and 97.2 percent of the 
		population. 
		
		What about home ownership? Using a procedure ... the proportion of 
		individuals who own their own home ranges between 59 and 64 percent. 
		
		How would we do this by hand? Remember that we want to use the 
		observed sample proportion in the case of a CI (and the null hypothesis
		proportion in the case of an hypothesis test). So, grab the observed
		proportion (0.9644) and add and subtract to it the critical value 
		(here, 1.96 will work since our sample is so large) times the SE, which
		is going to be sqrt(theta-hat[1-theta-hat]/n). */
		
		disp 0.9644235 - 1.96*sqrt(0.9644235*(1-0.9644235)/2333)
		
		disp 0.9644235 + 1.96*sqrt(0.9644235*(1-0.9644235)/2333) /* Checks out!*/
		
		
	/* ii. Bivariate analysis, two possible values of two categorical vars.

		Now, let's do our bivariate analysis. I'm going to show how to do this
		by hand first, which is OPTIONAL. */ 
		
		tab realclass homeowner
		
		/* This is a true two-way table that shows us how two categorical 
		variables relate. Let's conduct a test of the null hypothesis that there
		is no difference in proportions, using a two-tailed test. 
		
		First, we solve by hand. Again, this is optional*/ 

		disp (44/50)-(891/1470) /* Write down the raw diff. in prop. */ 
		disp (935/1520) /* Write down the pooled proportion */
		disp 1 - .61513158 /* Write down 1 - pooled */ 
		disp (1/50 + 1/1470) /* Write down 1/n1 + 1/n2 */ 
		disp (.27387755)/sqrt((.61513158*.38486842)*(.02068027)) /* Calculate 

		Here's the Stata code for that same test*/ 
		
		prtest homeowner, by(realclass) 
		
		/* Results check out! Let's interpret the results: we would see a 
		difference in sample proportions this extreme or more sheerly by 
		chance only 0.01 percent  of the time if there really were no 
		difference in the population proportions. 
		
		You can export these results, too, if you like, although a lot of this
		output might be overkill -- you *must* explain the parts of it that
		you think are relevant, so it may be safer to just report the numbers
		in a simple table (I'd create it in Excel or Sheets and then copy/paste
		it into Word/Docs) and comment on the numbers. Also, if the tables
		are off-center or otherwise garbled, you *must* present them in a way
		that is easy for us to read -- we were very generous with this for 
		part I, but if the table is clearly illegible to any reader when it is
		turned in, we will have to take more points off this time -- please do
		not turn in anything that you, yourself, cannot make sense of. */ 
		
		/* Notice also that a CI of the difference in proportions is given to
		you by the prtest output above. We can check the results by hand. */ 
		
		di .2738776 - (1.96)*sqrt(((.6061224*(1-.6061224))/1470)+((.88*.12)/50))
		
		di .2738776 + (1.96)*sqrt(((.6061224*(1-.6061224))/1470)+((.88*.12)/50))
		
		/* This checks out! */
		
		* FOR GRAPHS, SEE DAP I CODE. 
		
	/* 2. >2 possible outcomes of categorical predictor and/or outcome 
	
	i. Univariate analysis for categorical variable with more than two
	possible values of either predictor, outcome, or both.
	
		Let's keep this part concise. How about we go back to our old friend,
		the religion variable, which is a prime example of a polytomous 
		variable with many categories and no possible ranking of the variables 
		even if we give them numerical values just to keep track of them. 
		I am NOT using my dummy "protestant" variable here *precisely because* 
		the chi-square test is useful in cases where we DON'T want to reduce
		a polytomous variable to a dummy variable.
		
		What if we want to test the independence assumption of two such 
		variables, say religion and our ORIGINAL class variable, which, 
		you should recall, is polytomous (our recoded variable "realclass" 
		turns it into a dummy). 
		
		Let's first get a rough sense of what the population distribution 
		is like for each variable. 
		
		To construct a confidence interval for a nominal, polytomous variable, 
		we'll need to use a new command, -proportion-. */ 
		
		proportion class relig, citype(normal)
		
		/* You can also carry out a goodness-of-fit test with some hypothetical
		population proportions for each variable if you like. To fully 
		understand this test, it will actually be easier to show how the 
		bivariate test works, so I'll hold off on a full explanation until
		we get to the bivariate analysis. 
		
		To do the one-sample test, we first need to install the goodness-of-fit 
		command. */ 
		
		search csgof /* Now, go ahead and actually install it, which I can't
		show in code but which should be pretty intuitive using point-and-click.  
		
		Now we can tell Stata to test whether or not the values of our
		variable have a particular probability distribution. Here's the generic
		code: 
		
		csgof VARIABLE, expperc(PROPORTION-OF-VAR-CODED-AS-1 [space]
		PROPORTION-OF-VAR-CODED-AS-2 ... etc.)
		
		Note that, in this rare case, Stata accepts integers to represent 
		percentages, even though we usually use decimals. So, use "25" for 
		25 percent and not "0.25" as we typically do when interfacing
		with computers. */ 
		
		csgof class, expperc(25 25 25 25) /* As the careful observer might
		notice through ah investigation of American society, we don't live 
		in a  society where class is a finely-graded ladder with people 
		spread out more or less evenly. 
		
		We could do the same thing for religion but in this case, there
		are so many possible expected percents that it might seem kind of
		pointless. As we are about to see, however, this large number of 
		possible values will pose some problems for our chi-square test, 
		which is not infinitely flexible. In general, in deciding between
		a CI or a CSGOF test, use a judgment call
		if you want to do a chi-square goodness-of-fit test for the 
		univariate analysis: how many  possible values of the variable 
		are there? Is my goodness-of-fit null a reasonable hypothesis or 
		just a bunch of random guesses? 
		
		Let's do a preliminary check to see if we can do the chi-square
		on these two variables without modifying either. We'll use the 
		"expected" option on the tab[ulate] command to see if our
		expected counts meet our two rules: no more than 1/5 of the cells
		should be <5 and none should be less than 1. */ 
		
		tab relig class, expected 
	
		/* Rats! We have way too many cells with an expected count below 
		five and several cells with expected counts of zero. Let's restrict
		our test to the largest denominations by number */ 
	
		gen bigrelig = relig if relig <= 4
		tab bigrelig relig, mis
		
		label define bigrelig1 1 "Protestant" 2 "Catholic" 3 "Jewish" 4 "none"
		label values bigrelig bigrelig1 /* This only uses the four biggest
		categories; it sucks to have to leave out other groups, but the math
		becomes unreliable if we have too many groups that have very small
		total counts. */ 		
		
		tab bigrelig class, expected /* OK, our expected count rules look 
		fulfilled. Now let's go ahead and try our goodness-of-fit stats
		on this new variable. Here, I'm just using some rough guesses: 
		maybe Protestant folk are 50 percent, Catholic folk 30, Jewish 
		folk 5 and a-religious folk 15 percent of the big religions? This 
		is not a scientific process, just a rough guess. */
		
		csgof bigrelig, expperc(50 30 5 15)
		
		/* OK, so, we can say that there is basically no evidence for 
		my loose expectations written up above. In particular, Protestants
		and a-religious folk are significantly larger than our guess. 
		
	i. Bivariate analysis for categorical variable with more than two
	possible values of either predictor, outcome, or both. 
	
		Here's the code for a chi-square test in Stata */ 
			
		tab bigrelig class, chi2
		
		/* OK, clearly these results are significant. And, we can use a 
		hand-analysis to see why. We can go back to our expected counts and 
		look at the areas of the largest divergence... */ 
		
		tab bigrelig class, expected
		
		/* (What's driving the non-independence of categories? More lower-class
		Protestants than expected. More working-class a-religious people than 
		expected. More working-class and elite Catholics than expected) 
		
		... or we can have Stata do it for us */ 
		
		tab bigrelig class, cchi2
		
		asdoc tab bigrelig class, cchi2
		
		/* This command actually calculates each cell's chi-square statistic to 
		show us how much each cell contributes (notice that underneath the table 
		total is the sum of the standard scores of each cell, which is the same 
		thing we get as our total chi-square stat. in the simple command shown 
		above). This is a useful way to do some further investigation of the 
		chi-square output and I would like for those of you conducting this 
		test to do this. */ 
		
		* FOR GRAPHS, SEE DAP I CODE. 
		
		/* BONUS: let's also test our homeownership and class variables from
		above using the chi-square. Recall that our original test was ...*/ 
		
		prtest homeowner, by(realclass)
		
		/* and now we try ... */ 
		
		tab homeowner realclass, chi2
		
		/* Note that the two test statistics are not directly comparable in
		this case, unlike in some other parallel cases shown above, because
		they are different test statistics that come from differently-shaped
		sampling distributions. But their upshot is the same. We can also just
		test our original two variables without the re-code. */ 
		
		tab dwelown class, chi2
		
		/* Notice that this involves a subtle change to our hypothesis: we 
		now test whether or not a finely-gradated measure of class is 
		independent of a measure of home ownership that includes a rather
		mysterious "other" category. But, this is another way of testing the
		same basic null as above, that the two variables are not related. 
	
III. Categorical outcome, quant. predictor (only two possible values of
outcome; the one case we're just not going to learn for now is >2 possible
values of a categorical outcome; recode your outcome variable into a dummy).

	i. Univariate analysis.

		This is a simpler one. Doing this with a polytomous (many-category 
		qualitative variable) outcome is very hard; doing it with a dummy 
		(two-category qual. var.) is easy. We're going to take the easy way out
		and convert any polytomous outcomes, if your predictor is quant., to 
		dummies. 
		
		Let's suppose we want to know if income predicts whether you are married.
		This is a topic that fascinates demographers. Marriage is a classic
		polytomous category: there are NATURALLY more than two categories and
		they cannot be ranked. Widowed is not larger or smaller than unmarried. 
		
		Let's suppose that we are willing to group together the widowed, separated,
		and the divorced with the married to get a dummy variable for whether
		someone has EVER been married. */ 
		
		gen evermarried = 1 if marital <= 4
		replace evermarried = 0 if marital ==5
		tab marital evermarried, mis
		label define em 0 "never married" 1 "has married before"
		label values evermarried em
		
		/* First we need to do our univariate analysis. Using a CI for a mean
		makes more sense in the predictor's case, and, in turn, it makes more
		sense to use a CI for a proportion in the case of the outcome. */ 
		
		ci proportion evermarried, wald
		
		ci mean rincom16
		
		/* How do we interpret these? Using a procedure that captures the 
		population parameter in its bounds 95 percent of the time (in the
		long-run), we can say that the likely population parameter for 
		ever married -- here, the proportion -- lies between 69.6 and 73.3
		percent. The same general logic applies to income, but note the unique
		categories on the GSS. 
	
	ii. Bivariate analysis. 
	
		Here, we can just do a regression. */ 
		
		reg evermarried rincom16
		
		/* OK, cool! We definitely see a relationship here. Notice that we can 
		interpret the t-statistic just as before: we have a test statistic (and
		remember that correlation is basically just an arithmetic manipulation
		of the mean, the deviations, and the SD) that comes from a sampling 
		distribution, and the t-test tells us the probability of observing
		a coefficient this large or larger purely by chance if the coefficient
		really is zero (which is our default null hypothesis).
		
		The difference in this case is that we interpret our outome as the
		probability of marriage (a more sophisticated version of doing a 
		regression on binary or dummy variable is called logit, and it's a lot
		of fun mathematically; see the first tutorial for more details, although
		for our class, we're going to stick with this "linear probability model").
		Don't do this with categorical variables with more than two outcomes; 
		that requires multinomial logit which is well beyond the bounds of our
		course. Convert your outcome variable to a dummy if you want to do 
		regression of a qualitative variable. 
		
		Generally, you can interpret the regression coefficient as "a 
		unit increase in the predictor leads to a y-percent increase in 
		the probability of the outcome". 
		
		You should also interpret the CI output that's automatically included 
		here; see part I above for information on how to do that. 
	
IV. Categorical predictor, quant. outcome 

	1. Only two values of the predictor
		
		i. Univariate analysis: means and proportions
		
		Here, we can use our same techniques as above with no modification. */ 
		
		ci mean rincom16
		
		ci proportion realclass, wald
		
		/* ii. Bivariate analysis: regression on a dummy variable. 
		
		This is relatively simple: we just regress our outcome on a variable
		that is naturally in dummy form or can be reasonably converted to such. 
		I show above in several places how to convert a naturally polytomous
		variable (one that takes on multiple qualitative values) to a dummy; 
		head to the top of the file and CTRL+f for "dummy", then read what
		you find :). Here's the regression (general interpretation of 
		regression is covered above which is, as I noted elsewhere, something 
		that everyone should watch the short video for.
		
		BTW, the theory we're testing here might seem trivial, but there are
		actually some interesting reasons why class and income might only be
		loosely correlated. Here, we investigate whether this holds up. */
		
		reg rincom16 realclass
		
		/* How to interpret the t-statistic? the p-value? The same as
		always! See Part I above for lots of detail.
		
		Notice again (discussed above in part II) that this basically is 
		the same thing as a t-test of the difference in two group means 
		because there is so little variation in the predictor. E.g., notice 
		that in the above, the t-stat is almost exactly the same as the 
		difference in means in the t-test is the same as the regression slope.
		In fact, it *would* be the same if we assumed that the variance of the
		two groups were the same. E.g., here, our t-statistic is the same as
		for regression. */
		
		ttest rincom16, by(realclass) 
		
		/* The problem is that we really should use the unequal variance 
		option here, which is shown below. There are several equivalents to
		doing this for regression -- the problem of unequal variance for 
		different values of the independent variable is generally known as 
		heteroskedasticity -- and they are treated at length in higher levels
		of this sequence. We're going to keep it simpler with respect to
		regression since our book doesn't treat this topic with the same
		complexity that it treats the t-test. */ 
		
		ttest rincom16, by(realclass) unequal /* In fact, we will need to use 
		the unequal option or else Stata will assume that we want to pool
		observations,which will tend to overstate how significant our 
		results are. */
		
		* FOR GRAPHS, SEE DAP I CODE. 
	
/*	2. Multiple nominal values of categorical predictor
	
		i. Univariate analysis
		
		Again, we can do a set of confidence intervals for the values of the variable
		or a CSGOF test. Let's try to do both of those with our predictor
		variable here, which is a question that ask the respondent about
		their opinion on the adequacy of current criminal justice penalties,
		in particular whether they are harsh enough. */ 
		
		proportion courts, citype(normal)
		
		codebook courts
		
		csgof courts, expperc(33 33 33) /* OK, this looks fairly different to
		our prediction and has a p-value of zero. Super informative? No, not 
		really.
		
		We can use our regular CI for the mean of our outcome. Let's
		pick a fun one just to change things up */
		
		ci mean numpets /* Guidelines for interpretation are discussed
		exhaustively above  
		
		ii. Bivariate analysis
		
		This is more complicated. 
		
		A simpler method is to either convert your predictor to a dummy 
		variable, which I've shown several times in this .do-file and the 
		previous, or to use multiple regression. */  
		
		reg numpets i.courts
		
			* The "i." notation tells Stata to treat the regression as a regression
			* on a set of dummy variables defined by the values of "courts", 
			* excluding one category (here, "too harsh"). If you omit this, 
			* Stata will incorrectly treat -courts- as a quantitative variable--
			* make sure not to do this!
		
		* We interpret the values of the non-intercept coefficients as giving the
		* difference of the group means to the intercept. Importantly, the associated
		* test statistics test whether the coefficients are different from zero, 
		* but for the "included" groups, this coefficient is their difference to
		* the intercept (the mean for the excluded group). 
		
		* To fix that, we can ask Stata to directly carry out a set of all 
		* possible tests. We use the primary command -pwcompare- right after 
		* regression, adding the option "group" to give us a simplified set of
		* results and the option "eff" to show the full set of pairwise comparisons.
		* Level allows us to modify C/alpha. 
		
		pwcompare i.courts, group eff level(95)
		
		* To represent this graphically, I'd recommend using the bar graph method 
		* from DAP I. 
	
