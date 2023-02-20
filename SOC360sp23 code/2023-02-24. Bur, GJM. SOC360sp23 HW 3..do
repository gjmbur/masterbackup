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

cd "~/Desktop/Code/SOC360sp23 code"
	
	* THIS MUST GO TO A PATH ON YOUR COMPUTER!
	* Quotation marks are unnecessary unless your path has spaces in it.
	* Since people tend to make lots of mistakes about this early on, I am
	* including them. 
	
log using "2023-02-24. Bur, GJM. SOC360sp23 HW 3", text replace


* ... there will be work for questions before this one ...

sysuse auto, clear
reg length weight, nohead
gen yhat = 106.0965 + 0.0271028*weight
gen residual = length - yhat
sum residu
corr residual weight
