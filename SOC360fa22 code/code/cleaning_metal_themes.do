import excel using ~/downloads/metal_themes.xlsx, clear firstrow
drop F-Z
encode LyricsTheme, gen(theme)
encode Artist, gen(artist)
encode Song, gen(song)
drop Artist Song LyricsTheme
rename DecadeFormed DemoGroup, lower
format decadeformed %ty
forvalues i = 1/6 {
	gen demog`i' = demogroup == `i' & ~missing(demogroup)
}
rename demog1 CWm
rename demog2 CWf
rename demog3 BIPOCm
rename demog4 queer
rename demog5 BIPOCf
