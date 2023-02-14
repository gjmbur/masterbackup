/*	
SOC 360, Fall 2022, DAP I.	
task: discuss ways of structuring the same information 
author: Griffin JM Bur, 2022-09-14 */

********* DAP I *********


	/* This .do-file shows you four basic ways of approaching your data analysis
	project, on the basis of the four possible permutations of your input-outcome
	pair of variables.


	************ Coding etiquette / basic set-up ************ 

	This is all the same as in the ongoing dicussion do.-file. I left the 
	notes in so that you can review why we do this data hygiene. But it's not
	fundamentally part of the *analysis*, just here for reference */
	
	capture log close 
	
		/* Tells Stata to close a log if any is open. "Capture" is a rare
		command that tells Stata to proceed without error if none is open. */ 

	clear all 
		/* We remove any data that we've been using. Note that this command does
		not stop to ask you whether you want to save your data. In general, the  
		safest thing to do is actually to NOT save your data. Make any changes 
		that you want to make to your data in code so that you have a record of 
		how you transformed a data-set that other people have access to. Then, 
		you don't need to, and SHOULD NOT, save your data (the changes you make
		in a .do-file will be nonsensical if you save the changes, then try to
		apply them again. Just keep the original data file. 
		
		Once you transform your data-set, it is YOUR data-set and you are 
		responsible for justifying the transformations in it. By contrast, if 
		you make all changes to the data in Stata, as long as you justify your
		coding (as I am doing here), you can blame the place whence the data came
		for data problems, haha. */ 

	set more off 
		/* "tells Stata to run the commands continuously without worrying about 
		the capacity of the Results window to display the results". */ 

	cd /users/gjmb/desktop/soc360fa22 
		/* This tells Stata where to save stuff and also where to pull data from, 
		allowing us to use the "use" command to load data (if it's in the same 
		folder). You obviously need to change this file pathway so that it goes 
		to something on your hardware, not mine. */ 

	/* Now, let's load the GSS data */ 

	use ./data/GSS2018, clear

	/* We want to create a log of our data, i.e. basically a text file of 
	results that lives in our directory. This is how we do that --> */

	log using ./code/logs/360project, text replace 
	
* I. Two quantitative variables. 

	* Let's look at the relationship between r's education and father's ed.

	* UNIVARIATE ANALYSIS. 
	
	/* First, we should look at the univariate distribution of each one. Let's
	start with r's education, using numeric and then graphical methods. */

	codebook educ 
		
		// This gives some basic information about the variable. 

	tab educ 
	
		// Tabulating the frequencies is OK with highly-discrete quant. vars. as
		// as means of getting a pictuer of the distribution, but be careful--
		// this will give massive outputs for other variables, e.g. age. 

	sum educ, detail
	
		// This prints basic summary statistics for the variable. Reporting the
		// five-number summary, mean, and standard deviation would be great.
		
	// Let's now get a first pass at the variable's graphical distribution.
	// Later, I show how you can get these into a nice, clean format. 

	* Let's pull up a graph. We'll first look at a histogram. See Lecture 2
	* for a detailed analysis of what histograms tell us. 
	
	hist educ, percent discrete 
		/* the discrete option tells Stata that we have a variable which takes
		on a finite or countably infinite number of possible values, and that 
		those values are appropriate bins for a histogram, making this basically
		an estimate of the probability mass function (PMF). This is not 
		appropriate for continuous variables, such as age, unless your sample is
		very discrete (e.g., you have just a few points). */

	/* Now, let's examine r's father's education (we often use father's education 
	as a predictor because our society had highly unequal rates of entry into 
	higher education by gender until relatively recently, a feature of our 
	stratified social structure. Again, we'll start with numeric analysis. */ 

	codebook paeduc 
		/* OK, much more missing data -- that's predictable enough since some 
		people's parents may be long-since deceased and r may be unable to 
		estimate the level -- but in your actual case, you may want to investigate
		this a little more in the codebook -- some questions might be part of a 
		"module" that only some respondents answered, which is another reasonable
		cause of missing data -- it's just worth investigating. In this case, 
		we should consider that dropping these rs may result in us dropping 
		disproportionately marginalized respondents. */

	tab paeduc
		
		// See above for explanations of these techniques. 
		
	sum paeduc, detail

	hist paeduc, percent discrete 
	
	* If we wanted to combine the two histograms, we could write names for them....
	hist educ, percent discrete name(reduc)
	hist paeduc, percent discrete name(paeduc)
	graph combine paeduc reduc
	
	// Let's move on to some more-advanced graphical techniques. Now, I'll show
	// a means of plotting an estimate of the PDF for each variable. First, 
	// we can just get the curves on their own. 
	
	kdensity educ // this puts education on its own.
	
		* By the way, the way that kernel density estimation works is much like
		* a moving average: Stata takes basically a moving average of the height
		* of the actual data (which are necessarily discrete) to get a smooth
		* curve. It's like a histogram except, much like a moving average, an
		* individual point can be counted multiple times. There are also weights
		* for individual points based on their distance from the center of the 
		* "window" of the moving average. 
		
		* It is arguably not totally appropriate for a variable we'd probably 
		* call discrete, but it is fun to look at. Note that kdensity plots are 
		* also sensitive to the underlying mathematical assumptions we use to 
		* generate them, like histograms. 
	
	kdensity paeduc  // here's father's education
	
	// In this case, we might even want to combine them since they're measured
	// on the same scale. NOTE: this is not a bivariate analysis, but 
	// merely a way of visualizing the two individual distributions simultaneously, 
	// which may not make sense if your variables aren't on the same scale (although
	// you can always standardize them if you like). 
	
	// This analysis can't tell us if the variables are related because there is no
	// link between Xi and Yi here, unlike, for instance, in a scatterplot. 
	
	twoway (kdensity educ) (kdensity paeduc) 
		// this combines them. Some commands will let you simply enter multiple
		// variables as arguments, but if that doesn't work, most plots can be
		// combine with the -twoway- prefix. 
	
	// We can also combine these plots with histograms to get some nice graphs. 
	
	hist educ, discrete kdensity 
	
	hist paeduc, discrete kdensity

	// Let's look at some more graphical measures of the individual distributions.
	// Now, we'll focus on box plots. 
	
	graph box educ
		
		/* The most useful thing this does that a histogram does not is that this
		gives us a way to visually identify the quartiles and outliers. 
		
		Should we drop them? Probably not since it is plausible that some people 
		simply have very low levels of formal education. We may later want to 
		run regressions with and without them. */ 
		
	* See below for an addendum on identifying outliers numerically which uses
	* explanation of some slightly more-advanced techniques. Most people will 
	* simply want to note their presence; the GSS is a pretty "clean" data-set
	* and so most outliers are simply unusual values that should not be changed.

	graph box paeduc

	* Now, let's compare the two.  
	
	graph box paeduc educ 
	
	* We can also get this horizontal for (possibly) easier visualization.
	
	graph hbox paeduc educ
	
	* Finally, combining density curves and box plots is often useful. 
	* First, we'll need to install a command. Don't worry -- this is very easy
	* to do in stata. Most user-written commands are uploaded to the "Boston
	* Archive" (technically called the Statistical Software Components archive)
	* and so we type "ssc install [command]". 
	
	ssc install vioplot
		
		* This won't do any harm if you already have this installed. 
	
	vioplot educ paeduc, horizontal // horizontal often looks better
		
		* We've already explained what these two things do above; here we just
		* combine the two. 

	* BIVARIATE ANALYSIS. 
	
	* Let's begin with a simple graphical analysis. For quantitative variables,
	* that's as easy as just putting up a scatter plot. 

	scatter educ paeduc, jitter(5) 
	
		* Note two things here. First, the Y-variable comes first in this 
		* command's syntax. Second, the jitter option is useful when we have 
		* discrete data to put a little random noise in there; the number after 
		* "jitter(" tells us what percent of the graph should be noise; see 
		* "help scatter" for more. 
		
	* We could also run a contour plot to simulate the joint density. 
		
	ssc install kdens2 // see above about installing
	
	kdens2 educ paeduc, levels(10) 
		
		* These look nice on their own, though it's hard to add a regression line.
		* You can generally read this like a literal contour map: more concentric
		* circles <--> a taller peak of a three-dimensional density plot. 

	* But, I would recommend at a minimum indluding the scatterplot since this
	* allows us to add a regression line. 
	
	* We'll give some more analysis when we discuss the numeric aspect of 
	* regression; the point of plotting the line is just to give a sense of 
	* what the best linear fit is. 
	
	twoway (scatter educ paeduc, jitter(5)) (lfit educ paeduc)  
	
	* Let's move onto the numeric analysis. First, we'll get r.   

	corr educ paeduc // wow, fairly strong!

	* We can calculate R-squared on its own before we turn to the regression 
	* analysis. There are two ways to do this. The first is better from a 
	* programming standpoint: we can call the return value from the last 
	* command -- we basically ask Stata to do something using its working memory,
	* so to speak. The list of the return values from any command are in its
	* help file; just write "help [commandname]"
	
	* Here, that happens to be "r(rho)". So, we can tell Stata to just square
	* and return that to us. 
	
	display r(rho)^2
		* note that "disp" turns Stata "into a glorified hand calculator, per 
		* the help file for "display". 
		
		* The alternative is to just copy/paste the value we want, but 1) this
		* forces more rounding on us than we might want, and 2) the code is 
		* less elegant since it requires us to manually copy/paste specific
		* numeric values into our code. Using "r(rho)" makes it clear what we
		* are doing conceptually in a way that is easier to remember if someone
		* else is looking at your code (or if you come back to it and say to 
		* yourself "why the hell is 0.17045629 in my .do-file?"

	* We interpret this as "about 17 percent of the variation in our outcome 
	* variable is explained by variation in our predictor". 
	

	* We can now turn and look directly at the regression output.
	
	reg educ paeduc, nohead 
		// The option "nohead" suppresses some output we don't need right away.
		
	* The coefficient column tells us that for a unit increase in father's ed.,
	* we should expect a 0.290 increase in respondent's education. 
	
	* This is getting a little ahead of ourselves, but the column with the "t" 
	* and the "P" will be very important later -- you want a big t-value 
	* and a small p-value if you expect there to be a statistically significant 
	* relationship, which we in fact have! We'll eventually learn what all of 
	* these pieces mean. 

	* We can also briefly look at the full regression output, which prints
	* the model, residual, and total sums of squares shown in lecture. 
	
	reg educ paeduc
	
	* We notice that MSS/TSS also gives a measure of the goodness-of-fit, which
	* we can again calculate just by using return values. Stata doesn't store
	* the TSS, but since MSS + RSS = TSS, we can calculate it quickly.
	
	di e(mss) / (e(mss) + e(rss))
	
		* Note the slightly tricky bit that we now write "e" for programming
		* reasons that aren't worth going into here. 
		
		/* Incidentally, the link on the following line for interpreting output 
		is excellent -- I began looking at this page myself back when I 
		understood none of the many pieces of the output.
		https://stats.idre.ucla.edu/stata/output/regression-analysis/ */ 
		
	* Finally, we can conduct some basic analysis of the residuals. 
	
	rvfplot, jitter(5)	
	
	* Already, we can see evidence of heteroskedasticity here -- the spread of
	* residuals is definitely not the same throughout the plot, having a much
	* looser fit for lower values of father's education (which makes a great 
	* deal of sense: there are other variables that we are definitely omitting
	* from the analysis. One relevant one is simply a person's age, as the 
	* relative importance of completing high school has increased markedly over
	* time -- meaning that even people who have fathers with little formal educ.
	* still face a lot of pressure to get at least that much education.
	
		* Note, of course, that regular regression of residuals on the X-variable
		* is pointless since we get the OLS estimates precisely by forcing the 
		* residuals to have zero correlation to the predictor -- but we could
		* add a non-linear regression line to look for non-linearities. To do
		* that, we need to do a lot more work, so I'll skip that here. 	
		
	* Brief addendum on outliers: although the Tukey rule is a very offhand
	* one that does *not* mean that the data-points in question "should" be
	* dropped, it may be useful to pull up the outliers in question. Here's
	* how we can do that using return values. 
	
	quietly sum educ, d
		* "Quietly" tells Stata that we don't need it to display the output
		* which is useful since we just want the results in Stata's memory.
	scalar LBed = r(p25) - ((r(p75)-r(p25))*1.5)
		* "Scalar" is a way to store a number in Stata with a more-helpful
		* name in English; here, I simply write "LBed" for "lower bound of ed."
		* "R(p25)" returns the 25th percentile; r(mean) returns the mean; 
		* r(sd) returns the SD, etc. For a full list of the values returned
		* by a command -- and, remember, to access these, you must invoke them
		* _right_ after the commmand which produces them -- see the help file. 
	scalar UBed = r(p75) + ((r(p75)-r(p25))*1.5)
	di LBed 
		// we can see what the LB is by telling Stata to display the contents
		// of the scalar we made. 
	di UBed
	
	* Now, if we did want to drop those outliers, we could do that -- although
	* note that dropping such individuals should be done very cautiously. One
	* alternative is to just make a new variable and make people missing on 
	* that new variable. 
	
	gen educ_no_OL = educ if (educ >LBed & educ <UBed)
		* "gen", short for generate, just makes a new variable. 
		* "if" tells Stata what conditions to watch out for -- here, that educ
		* is greater than the lower-bound and less than the upper-bound. 
	
	* We can verify that, on this new variable, all observations are between
	* the bounds of the outlier cutoffs. I don't think we should actually use
	* this variable here since I don't think we should drop the outliers, but
	* it is useful to see that what we've done works. 
	
	sum educ_no_OL, d
	
		* We can verify that those people outside the boundaries were replaced
		* as missings in the following way. 
		
		sort educ // we do this to get the data sorted by value of education
		list educ educ_no_OL in 1/25
			* Be careful with "list" as it lists out all observations, which is
			* often going to produce massive amounts of output. Here, I do it
			* cautiously by having Stata list only those observations between
			* 1 and 25, and I only do it after meaningfully sorting the data.
			
	* Finally, note one Stata "gotcha" about missing values. MVs are represented
	* internally as arbitrarily-large numbers. This can lead to some *very* 
	* unexpected behavior. Let's see an example. Suppose we want to find out
	* how many people's fathers had more than 16 years of education.
	
	count if paeduc>16
		* Woah -- that's pretty big. Have we maybe made a mistake? Check the 
		* frequency table...
	tab paeduc
		* We definitely have far fewer than 806 fathers with education>16.
	
	* So, what's happened here is that were insufficiently cautious about our
	* conditional statement: Stata included not only people whose fathers had 
	* more than 16 years of education here but also all missings as well. We
	* instead need to write, when we use greater-than comparisons...
	
	count if paeduc>16 & !missing(paeduc)
		* This produces the same result we would get by hand from our table. 
	
	* PRODUCING PUBLICATION-QUALITY GRAPHS. 
	
	* In this section, I will show you how to get graphics looking roughly like
	* those I produce for my lectures, which--if I say so myself--significantly
	* improve upon Stata's default. 
	
	* The first step is setting a better scheme. We'll follow the approach of
	* data-viz. guru Edward Tufte, which is conveniently embodied in a specific 
	* Stata scheme. Here's more on schemes: 
	view browse ///
			https://www.stata.com/meeting/germany18/slides/germany18_Jann.pdf
		
	* We'll need to install the scheme first. 
	
	ssc install scheme_tufte
	
	set scheme tufte
	
	* Now, let's first try getting the scatterplot looking a bit nicer. 
	* Personally, I think primary colors for the markers + B/W background is 
	* the easiest on the eyes. Here, I show simpler syntax for scatter so that
	* we have fewer parentheses to keep track of. We'll also add a title to both
	* axes as well as an overall title, modifying the size as needed. We'll also
	* print a note giving the source and sample size. You can simply write down
	* the n, but if you also want to, say, add the regression equation and 
	* correlation coefficient, a nice method is to store them in what is known
	* as a macro, or a container that we can give a text name to for convenience.
	
	* I'll let you decide how much of this automation you want to use vs. to
	* what extent you want to just paste things in. You can even use the 
	* point-and-click graph editor. But, here is what your graphs _must_ have...
	/* 
		1. Titles -- overall, X-ax, Y-ax
		2. Note with a source
		3. Some attention to detail -- jitter if needed, probably remove legend
		4. Mess around with the color a bit if you use a minimalist scheme.  
		
	What you *can* opt out of is using the macros -- you can just past in the
	sample size if you want. You also don't need to put the regression equation 
	on there. If you don't want to learn this optional material, just skip
	ahead to the part where I show how title options work and manually enter
	the text you want instead of using return values. */ 
	
	* Graphs without the convenience (but also learning curve) of macros.
	
	* If you don't want to use macros, you can just hand enter relevant values 
	* into the post-graph options, though it is more tedious. Below the 
	* code, I explain what each piece means. All of these values I obtain
	* from the regression output given just beforehand. The three forward slashes
	* extend commands beyond a given line for readability; they are necessary
	* because Stata otherwise regards the end of a line as the end of a command. 
	
	reg educ paeduc
	
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
	
	* Graphs using the convenience of macros.
	
	* First, let's run the regression to get the results in Stata's memory. 
	* We can add the prefix "qui" for quietly since we don't necessarily want
	* to see the output for this purpose. 
	
	qui reg educ paeduc
	
	* Now we need to run the next part all in one ago -- macros don't stick 
	* around in working memory beyond a single "run" of a .do-file or portion
	* thereof. So, the contents of the local will be deleted after we run this.
	
	* Macros are containers for numbers or text. The general syntax for making 
	* them for numbers is "local [macroname] = expression". Here, that lets us
	* just store the return values from our analysis without having to write 
	* down actual numbers, which can be annoying. 
	
	local n = e(N) 
		// the difference between r(N) and e(N) is a computer sciencey thing. 
	matrix coeffs = e(b) 
		// the coefficients are a vector so we store them like so. 
		// Now we'll pull out those elements and put them in macros while 
		// also rounding for readability. 
	local slope = round(coeffs[1, 1], .01)
		* This puts the first coefficient, which has matrix coordinates [1, 1]
		* in a local called "slope" while also rounding it to the hundredths'
		* place. We'll do the same for the intercept.
	local intercept = round(coeffs[1, 2], .01)
	local r2 = round(e(r2), 0.01)
		* R^2 is just a scalar still. 
		
	* We print the contents of macros like this: 
	
	di `n' 
	di `slope'
	
		* we use a left tick (next to the "!/1" key) and a right-tick to enclose
		* the macro. Otherwise, nothing will happen (at best; at worst, an error)
	
	* Finally, we'll put it all together. Note that we'll need line extenders,
	* which are the three forward slashes. I'll also turn the legend off since 
	* it is typically not that useful. Note a couple of other small things, e.g.
	* that to get italics or other special fonts, we write things like {it: text}.
	
	scatter educ paeduc, jitter(5) mcolor(red) xtitle("Father's education (years)") ///
		ytitle("Respondent's education (years)") legend(off) ///
		title("Relationship of father's and child's education", size(medium)) ///
		note("Source: GSS 2018. {it:n} = `n'") ///
		text(5 15 "Estimated regression equation" ///
		"{it:Y} = `slope'{it:X} + `intercept'" ///
		`"{it:R} {superscript:2} = `r2'"', box fcolor(white)) || lfit educ paeduc
	
	* Here's a sample advanced technique with density curves. 
		* This example is inspired by Germán Rodriguez: https://archive.ph/pC17P
	
	kdensity educ, gen(edX edY) nograph 
		* This suppresses the graph but stores the X and Y coords in a new 
		* variable that we'll use in a moment. 
	kdensity paeduc, gen(paedX paedY) nograph
	
	gen zero = 0 // we'll use this in a moment, don't worry. 
	
	twoway (rarea edY zero edX, color(red%30) ytitle("Density") ///
		title("Distribution of education in the US", size(medium)) xtitle("Years"))
		
		* So, what we did here was make Stata shade the region of a graph between
		* "edY" (the height of the density curve) and "zero" (just the X-axis)
		* over the values of a third variable edX. For the sake of not having
		* the graph too dark, we print the color red at only 30 percent. The 
		* rest is review. Finally, we can add father's education and some of 
		* the other key information. 
	
	qui sum paeduc, d
	local dadn = r(N)
	qui sum educ, d
	local rn = r(N)
	
	twoway (rarea edY zero edX, color(red%30) ytitle("Density") ///
		title("Distribution of education in the US", size(medium)) xtitle("Years")) ///
		(rarea paedY zero paedX, color(green%30) ///
		note("Source: GSS 2018." ///
		"{it:n}{subscript:father} = `dadn'; {it:n}{subscript: respondent} = `rn'") ///
		legend(pos(9) ring(0) region(lcolor(black)) order(1 "Respondent" 2 "Father")))
		
		* Note that "legend(order(1 "text1" 2 "text2" ...))" allows direct
		* manipulation of the items in the legend. "Pos(clockface)" puts the
		* legend at a certain "clocktime" orientation; "ring(0)" puts the 
		* legend in the plot; "region(lcolor(black))" puts a box around it.
		
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










