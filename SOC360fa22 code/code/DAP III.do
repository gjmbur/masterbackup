/*	
SOC 360, Fall 2022, DAP I.	
task: demonstrate analysis of interval/ratio outcome, categorical predictor
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

	log using ./DAPIII, text replace 
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
	* of the -table- command in version 17 (IMHO) and also changed the syntax;
	* since a fair number of people are probably using version 16 and others
	* version 17, I'm going to benchmark this at the earlier version. 
	
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
	
	* We basically have two options for producing a graphical analysis here. 
	* The first would be to show the conditional means with a bar graph, which
	* is the better of the two. Below is the syntax. 
	
		* graph bar (stat) [Yvar], over(qualXvar)
	
	graph bar (mean) rincom16, over(protestant) 
	
	* We _could_ technically show this as a scatter-plot, but note that this is
	* going to look silly for somewhat obvious reasons.
	
	* For more on the syntax here, see the quant./quant. video. 
	
		* Basic syntax --> scatter Yvar Xvar || lfit Yvar Xvar 
	
	scatter rincom16 protestant || lfit rincom16 protestant 
	
	* Finally, let's show the "professional version". See graphics video for
	* guidelines on how to produce something like this. 
	
	count if ~missing(protestant) & ~missing(rincom16)
	graph bar (mean) rincom16, over(protestant) ///
		title("Relationship between religion and income (GSS scale)", ///
		size(medium)) note("Source: GSS 2018, {it:n} = `r(N)'.") ///
		ylab(0(1)20) ytitle("Mean income (GSS scale)") 
