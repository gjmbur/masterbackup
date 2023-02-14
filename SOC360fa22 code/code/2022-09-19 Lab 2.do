********* Second lab .do-file *********

	/* This .do-file shows you basic data analysis in Stata. 
	
	This writing in green is commentary, meaning that it is here for humans to 
	read but Stata won't try to run it as "code" proper, meaning that we can 
	write whatever and however we want. 

	The stuff in blue and black is actual Stata commands. These have to be 
	written in a specific way and Stata will read these commands as code per se,
	meaning that it will carry out very specific operations on your data -- e.g. 
	taking the mean, running a regression, producing a pie chart, whatever you 
	like -- if the code is written correctly. Here is an example. */ 
	
	// Note that there are three loosely equivalent ways of writing comments.
	* This is the third way. 

	************ Coding etiquette / basic set-up ************ 

	// This section tells you why you should include this standard header in all
	// of your do-files.
	
	capture log close /* Tells Stata to "close a log if any is open and do nothing 
	if no log is open. (The word capture means that Stata should not complain if 
	there is no log open to close.)" */ 

	clear all /* We remove any data that we've been using. Note that this
	command does not stop to ask you whether you want to save your data. Usually,
	you should NOT save your data at this level. 
	
	Instead, you should try to make any changes that you want to make to 
	your data in code so that you have a record of how you transformed a data-set
	that other people have access to. Then, you don't need to save your data.
	
	Once you transform your data-set, it is YOUR data-set and you
	are responsible for justifying the transformations in it. By contrast,
	if you make all changes to the data in Stata, as long as you justify your
	coding (as I am doing here), you can blame the place whence the data came
	for data problems, haha. 
	
	By the way, this is one reason that Excel is *not* in heavy use at a higher
	level in data analysis: it does not force you (or easily allow you) to leave 
	behind this detailed record of what you've done to the data. Reinhart and 
	Rogoff's famous blunder, for example, was vastly harder to spot because they
	were using a point-and-click interface. All "serious" statistical software,
	by contrast, forces you to document such changes or makes it easy. */ 
	
	set more off /* "tells Stata to run the commands continuously without 
	worrying about the capacity of the Results window to display the results" */ 

	cd ~/desktop/SOC360fa22 /* This tells Stata where to save
	stuff and also where to pull data from, allowing us to use the "use" command
	to load data (if it's in the same folder). The problem is that this is a
	location on my hardware, not yours; let's fix that now Make a directory called
	the same thing and then try to change to it. */ 
	
	* You can check your current working directory by writing...
	pwd // this should correspond to what you wrote after "cd" above. 

	* Now, let's load the GSS data. Download these from the course website and
	* put them into the folder we just made so that you can simply write "use 
	* [name of data file]" without having to spell out the full path-name. 

	use GSS2018

	/* We now want to create a log of our do-file, i.e. basically a text file of 
	results that lives in our directory. This is how we do that -->. These log
	files are basically like do-files, but their benefit (and drawback) is that
	they are just text-files that can't be directly run by Stata (which is a good
	thing if you want to share your results with a user of another program). It
	also contains the numeric results of your code, which a do-file does not. */

	log using SOC60lab2.log, replace 
	
	* OK! Let's finally get started. 
	
	* First we can take summary statistics for education. 
	summarize educ 
	sum educ, detail
		* prints more detailed summary stats, including many percentiles,
		* variance, skewness (!= skew--ignore for now)
	
	* We can take a histogram (the DAP I .do-file will show the fancy
	* titles, color schemes stuff, etc. later -- focus on basics for now.
	histogram educ
	hist educ, percent discrete // Tells Stata not to bin data, makes Y-axis %
	hist educ, normal kdensity // Plots density estimate and adds Normal for ref
	
	* We can also make boxplots 
	
	graph box educ
	graph hbox educ
	
	* and compare the distribution between variables
	graph hbox educ paeduc
	
	* Going back to the material from Week 1, you can also do bar graphs for
	* qualitative variables
	tab natfarey
	graph bar, over(natfarey) 
	tab uscitzn
	graph bar, over(uscitzn)
	sum uscitzn, d // are these statistics meaningful here? 
	
	* You can solve z-table problems in Stata, whether you have data in or not.
	display normal(-1.96) 
		// gives cumulative proportion up to z = 1.96. This agrees with our 
		// sense that the area outside -1.96 and 1.96 is about five percent
		// since we should double the answer to get that. 
	display invnorm(0.005) 
		// gives area to left of curve containing 0.5 percent. So, if we wanted
		// middle 99 percent, we'd look for x-values corresponding to z ~= -2.58
		// and z~= 2.58 (rounding to the decimals used by our book).
	display 2*(normal(-2.58)) 
		// Plugging in the answer we got above, this is almost exactly 0.01.
	
	
