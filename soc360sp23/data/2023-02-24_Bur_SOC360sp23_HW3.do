* You can date the do-file and write intro comments however you want
* Here's my recommendation. 

// Date: 2023-02-24. 
// Author: Bur, GJM. 
// Purpose: master Stata regression mechanics

capture noisily log close 
	* This closes open logs. -capture- is a rarely-used prefix that tells Stata
	* to ignore error messages. USE THIS SPARINGLY. I am including the other
	* option "noisily" to show you what Stata is aware of but ignoring. 

set more off 
	* Undoes an annoying default in Stata that displays limited information. 

cd "~/desktop/code/SOC360sp23"
	
	* This stands for "change directory" and tells Stata to consider a folder
	* its "default" folder until otherwise specified. This lets us pull data
	* from this folder and save things (such as log files) to it without having
	* to write down long file-paths, which is convenient (and makes it easier
	* to share code between people, e.g. if everyone working on a project or 
	* class has a folder called "SOC360sp23"). 
	
	* THIS MUST GO TO A PATH ON YOUR COMPUTER!
	* Quotation marks are unnecessary unless your path has spaces in it.
	* Since people tend to make lots of mistakes about this early on, I am
	* including them. 
	
log using "2023-02-24_Bur_SOC360sp23_HW3", text replace
	
	* THIS MUST GO TO A PATH ON YOUR COMPUTER!
	* This makes a text file of your session, including commands run 
	* from the .do-file and those run from the command window. 
	
clear all 
	
	* This might seem scary -- it deletes all data in memory. Why should it
	* NOT scare you? You should have made all changes to the data in a do-file
	* making it pointless to actually save the data as a new data-set. 
	
* Question 5.4a
	* YOU MUST HAVE DOWNLOADED THE DATA AND PUT IT IN THE FOLDER TO WHICH 
	* YOU "CHANGED DIERCTORY"
	use coral
	
	* Your work should go here...
	* Skipping ahead ...

* "Regression Diagnostics 2"
	* ... there will be work for questions before this one ...

	sysuse auto, clear
	reg length weight, nohead
		// option "nohead" suppresses some output that we don't currently need
	gen yhat = 106.0965 + 0.0271028*weight // note: I get these from the output
	gen residual = length - yhat
	sum residual
	corr residual weight

* Some more questions follow...
	
log close 
