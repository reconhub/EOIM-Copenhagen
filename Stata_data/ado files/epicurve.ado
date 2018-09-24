*! epicurve.do 
*! Display an epidemic curve
*! Gilles Desvé, 2007

capture program drop epicurve
program epicurve
syntax varlist [if] [in] [, title(string) subtitle(string) xtitle(string) ytitle(string) caption(string) color(string) starting(string)]

tempvar touse
mark `touse' `if' `in' 

tokenize `varlist'

local _xvar = "`1'"
local varcase = "`2'" 

// noisily : disp "`_xvar'"

preserve 

quietly {
if "`varcase'" != "" {
keep if `varcase' != 0
}
if "`if'" != "" {
keep `if'
}
}

// Drop temporary variables in case of 
capture drop dosoccur 
capture drop maxi
capture drop maxcase
capture drop _hline 
capture drop maxonset


if "`title'"=="" {
local title "`_xvar'" 
if "`varcase'" != "" {
local title =  "`varcase'" + " by " + "`title'" 
} 
}

if "`subtitle'"=="" {
// local subtitle:data label  
}

if "`xtitle'"=="" {
local xtitle:variable label `_xvar' 
}

if "`ytitle'"=="" {
local ytitle "Count" 
}

if "`color'"=="" {
local color "blue" 
}

if "`caption'"=="" {
local caption = c(current_date) 
}

// find the maximum number of case by day
bysort `_xvar': gen dosoccur = _N if `_xvar' !=.
quietly summarize dosoccur
// put maximum of case in maxcase macro
local maxcase = int(r(max)+ r(max)/10)

// Create a variable for horizontal line
gen _hline = _n 

egen maxonset = max(`_xvar')
local maxx = maxonset
egen minonset = min(`_xvar')
local minx = minonset
if "`starting'"!="" {
   local minx = d(`starting')  
}

// If X > Y
local yscale = max((`maxx' -`minx')/ 1.75, `maxcase')
// disp maxonset - minonset

// if Y > X we increase Y 
gen Maxi = max(`minx' + (`maxcase'*1.75) , maxonset +2)   
 
twoway (histogram `_xvar', sort frequency discrete color(`color') lcolor(white) yscale(range(0 `yscale')) xscale(range(`minx' `maxx'))) ///
 (dropline Maxi _hline in 1/`maxcase', lcolor(white) horizontal msymbol(none)), ///
  xtitle("`xtitle'", size(small))  xscale(lcolor(dknavy) outergap(2)) ///
   ytitle("`ytitle'", size(small)) ///  
   xlabel(#8, ticks labels labsize(vsmall)) ///
   subtitle(`subtitle') ///  
   title(`title') caption(`caption') legend(off)

 
 restore
 
 end
 
