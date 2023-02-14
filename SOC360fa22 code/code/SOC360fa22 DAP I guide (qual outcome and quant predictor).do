/*	
SOC 360, Fall 2022, DAP I.	
task: demonstrate analysis of qualitative variables
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

	log using ./360dummyregression, text replace 

* IV. Categorical outcome, interval/ratio predictor 

	* This is a slightly trickier approach, but it might be of interest to some 
	* students. The basic approach -- find some way to do regression on a binary
	* (Boolean, bernoulli, dummy, indicator, etc. -- lots of terms for this!) 
	* outcome -- is at the heart of logistic regression, which is the workhorse
	* technique in epidemiology. We're going to stick to a simpler model, known
	* as the linear probability model, which has some known flaws (see L10 for 
	* what is hopefully an interesting discussion of this), but which is fine
	* as a first pass at these ideas. 

	* Let's return to our example from the first part of this project. Suppose
	* that we are only interested in whether somone graduated high school. Let's
	* try to predict that as a function of father's education. 
	
	* UNIVARIATE ANALYSIS. 
	
	* Univariate numeric analysis. 
	
	* Here, I'll generally pass quickly over paeduc -- if you're doing this type
	* of project, you should be comfortable with basic regression; I go over all
	* of this above. I'm just printing the relevant commands for reference. 
	
	sum paeduc, d
	tab paeduc
	
	* Now, let's make the outcome variable. I'll show this even for advanced 
	* students since this requires a bit of practice. 
	d degree
	label list LABAM
	gen HSgrad = degree>0 & !missing(degree)
		* Basic idea here is that we want to write a Boolean expression so that
		* people who have graduated are 1s and all others are 0s
	tab HSgrad degree, mis 
		* Our check of our work looks good; let's make a value-label. 
	
	label define HS 0 "no" 1 "yes"
	label values HSgrad HS
	label var HS "r graduated HS"
	
	* Now let's print summary statistics for HS. Interp: mean is proportion
	* of 1s out of total; variance is roughly p(1-p).
	
	sum HSgrad, d
	
	* I'm going to pass over the description of father's education; this is 
	* bread-and-butter stuff that is shown in the first video at some length. 
	* I'll also omit the discussion of basic graphical analysis -- again, you 
	* should only consider this type of project if you feel good about the 
	* basics (but if you do, by all means try this). 
	
	* BIVARIATE ANALYSIS.
	
	* Finally, let's get to the interesting stuff. So, firstly, it _is_ possible
	* to take the correlation between this dummy and the quantitative predictor
	* and we'll actually see a surprisingly high correlation here! 
	
	corr HSgrad paeduc
	
	* We can also directly predict whether one graduated HS. The interpretation
	* of the coefficient in this case is relatively straightforward, although
	* it can be slightly problematic: we assume that the underlying probability
	* of D is a linear function of the predictor: p(X) = B0 + B1*X, and so we
	* say that the coefficient gives a unit increase in the probability of 
	* observing the outcome D = 1. 
	
	* One obvious problem here is that this can lead to nonsensical predictions;
	* it is not possible to have p(X) > 1 or p(X) < 0. Still, at our level, this
	* is fine. 
	
	reg HSgrad paeduc
	
	* Here, our conclusion is that a year increase in your father's education 
	* leads to about a two-percent increase in your probability of completing
	* high school. 
	
	* We can put this up on a scatter-plot, though note that it is not 
	* particularly informative. If you try this project, I'd just report 
	* the numeric output, honestly. 
	
	set scheme gg_hue
	scatter HSgrad paeduc, mcolor(black) jitter(10) || lfit HSgrad paeduc
	
	* Finally, I did want to briefly mention the major alternative, known as 
	* logistic regression ("logit"). The details get a bit complicated, but here
	* is the basic idea. Instead of supposing that the underlying probability--
	* which is bounded between 0 and 1--is linearly related to X, what if it has
	* a sort of sigmoid shape? That would mean that what X actually *does* have
	* a linear relationship to is the log-odds of probability: ln(p/(1-p)) is a 
	* function which maps the set of real numbers to 0 =< p <= 1. 
	
	* Then, when we exponentiate both sides, our model B0 + B1*X1 actually ends
	* up in the exponent of e, so we have a nicely non-linear set of predictions
	* for something we think is probably non-linear! I show, in L10, a simulation
	* where I presume that we could somehow know the unobserved probabilities. 
	
	* Here's what that looks like. 
	
	logit HSgrad paeduc
	predict yhats
	twoway (scatter HSgrad paeduc) (lfit HSgrad paeduc, lc(black) lp(dash_dot)) ///
		(scatter yhats paeduc, mcolor(black) msym(Th) ///
		legend(order(2 "linear prediction" 3 "logistic prediction") ///
		ring(0) pos(3) region(ls(longdash))) ///
		title("Relationship between father's education and {it:p} of graduating HS", ///
		size(medium)))
		
	* What makes this tricky is that we have to find a way to get that unobserved
	* p. The way we do that is the following...this is just a heuristic guide
	* rather than a rigorous proof, but it gives you the intuition. 
	
		* 1. PMF(Yk) = nCk * p(X)^k * (1-p(X))^(n-k)
		* 2. The sample fixes the values of n and k, so we can ignore the 
		* binomial coefficient since it's just a scalar. Now, recall that the 
		* model is that the *log* of the *odds* of Y are a linear function of X.
		* So, take the log of the Ys and do a bit of algebra until you have the 
		* log of the odds on the right hand side; swap that out for the linear
		* combination B0 + B1*X1 since we stipulated from the outset that we 
		* believed ln(p/(1-p)) to be a linear function of B0 + B1*X1. 
		* 3. Finally, find values of the B0 and B1 that maximize the value of the
		* "log-likelihood" function (this method, of taking the sample as fixed
		* and passing through possible values of the parameter to see which ones
		* maximize the probability of observing the data, is called "maximum
		* likelihood estimation"; the likelihood function is essentially just the
		* PMF or PDF but with the sample values (e.g. Xs) and pass through possible
		* values of the parameter. This turns out to require an approximate
		* solution (using Newton's method), which is a level of mathematical
		* detail that is well beyond this course--but just FYI. 
