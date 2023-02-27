* Date: 2023-02-27.
* Task: Clean PSID files for use. 
* Author: Griffin JM Bur

version 16
capture log close
set more off

* Must be careful with globals, but we want to be able to run the do-file
* in chunks and ensure future workability of code. This way, someone just needs
* to set up any old "projfolder" and then put the raw files into a folder; it
* would be nice to automate this, but the PSID insists on putting public-use
* files behind a JSON firewall. There's a way around this with Python, but it's 
* not really worth it; see https://github.com/tyler-abbot/psid_py. 

global projfolder "~/desktop/big_data_sets/PSID"
* I keep these folders separate because GitHub is picky about size
global dissfolder "~/desktop/code/dissertation/scripts"
cd $projfolder/PSID_raw
log using $dissfolder/PSID_cleaning, text replace

* 1. Just unzipping the files
	* Unzipping files: the file-names change pattern after 1993, so I obtain a
	* list of file.
	local list : dir . files "*.zip"
	di `list'
	foreach file of local list {
		unzipfile `file', replace
	}
	
	* Moving cleaned files. Let's back out our directory. 
	cd $projfolder

	* I couldn't figure out how to get the extended macro function to accept two
	* types of files, so I sloppily ran the same loop twice but looking for .do
	* and .txt files. 
	local list : dir "./PSID_raw" files "*.do"
	foreach file of local list {
		capture noisily copy ./PSID_raw/`file' ./PSID_cleaned/, replace
		}
	local list : dir "./PSID_raw" files "*.txt"
	foreach file of local list {
		capture noisily copy ./PSID_raw/`file' ./PSID_cleaned/, replace
		}
