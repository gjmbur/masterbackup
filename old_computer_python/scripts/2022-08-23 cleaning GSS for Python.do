* inspired by https://archive.ph/RwNAg

cd ~/desktop/soc365sp22

use ./original_data/gss2018, clear

*Find out which duplicated value labels there are
quietly labelbook, length(64)

return list, all

*r(nuniq) contains the not-unique-values

*on all variables in r(nuniq) use the numlabels command

numlabel `r(nuniq)', add

*Look at the not unique value labels again:
labelbook, length(12)

return list, all

save ~/desktop/Python/data/gss2018python, replace
