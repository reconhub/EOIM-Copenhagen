* csinter.ado
* Gilles Desve, 2007-2008
* Thanks to Alain Moren and Aurelio Tobias
* Cohort analysis for outbreak investigation.
*
* Do a bivariate stratified analysis, presenting results (RR and 95%CI) for each stratum and adjusted result.
* In addition an additive interaction table is constructed
*
* syntax: csinter casevariable expvariable, by(stratification_variable) table plot
* options: 
*  by(stratification_variable)
*

capture program drop csinter
program csinter

syntax varlist(min=2 max=2) [if] [in], by(varname) [table plot]


tempvar touse
mark `touse' `if' `in'

tokenize `varlist'

quietly : count if `touse'
if r(N) == 0 {
   exit
}

local rowline = "-"
local rowname = ""
local iRow = 0

if "`touse'" != "" {
local touse = " & `touse'"
}


local _varsize = 12
local ilength =length("`2'")
if `ilength' > 12 {
local _varsize = `ilength'
}

quietly forvalues byvalue = 1(-1)0 {

// Get Exposed , Cases, AR
count if `2' == 1 & `by' == `byvalue' `touse'
local _Totexp = r(N)
count if `2' == 1 & `1' == 1 & `by' == `byvalue' `touse'
local _Caseexp = r(N)
local _Riskexp = `_Caseexp' / `_Totexp' * 100

// Get UnExposed , Cases, AR
count if `2' == 0 & `by' == `byvalue' `touse'
local _Totunexp = r(N)
count if `2' == 0 & `1' == 1 & `by' == `byvalue' `touse'
local _Caseunexp = r(N)
local _Riskunexp = `_Caseunexp' / `_Totunexp' * 100


// get RR and confidence interval
cs `1' `2' if `by' == `byvalue' `touse'
local _p = r(p)
local  _rr = r(rr)
local _lb_rr = r(lb_rr)
local _ub_rr = r(ub_rr)
local  _afe = r(afe)
local _lb_afe = r(lb_afe)
local _ub_afe = r(ub_afe)
local  _afp = r(afp)
local _lb_afp = r(lb_afp)
local _ub_afp = r(ub_afp)
local  _rd = r(rd)
local _lb_rd = r(lb_rd)
local _ub_rd = r(ub_rd)

if "`noabbreviate'"=="" {
  local _varsize = 12
}
local _thevar = substr("`2'",1,`_varsize')

noisily : {
display
// This number are calculated to allow copy and paste
if `byvalue'==1 display in text %15s "`by' = Exposed"
if `byvalue'==0 display in text %15s "`by' = Unexposed"

display in text "{hline 35}{c TRC}"
display in text %`_varsize's "`_thevar'" " Total   Cases  Risk % {c |}" "{col 40}Risk difference " _continue
display as result " " %5.2f `_rd'   _continue
local stemp : display %5.2f `_lb_rd'
display " [" ltrim("`stemp'") "-"   _continue
local stemp : display %5.2f `_ub_rd'
display ltrim("`stemp'") "]"

display in text "{hline 35}{c RT}" _continue
display in text "{col 40}     Risk Ratio "  _continue
display as result " " %5.2f `_rr'   _continue
local stemp : display %5.2f `_lb_rr'
display " [" ltrim("`stemp'") "-"   _continue
local stemp : display %5.2f `_ub_rr'
display ltrim("`stemp'") "]"

display in text %`_varsize's "Exposed"  _continue
display as result " " %5.0f `_Totexp'  _continue
display " " %6.0f `_Caseexp'    _continue
display " " %8.2f `_Riskexp'  in text " {c |}"  _continue
display in text "{col 40}Attrib.risk.exp "  _continue
display as result " " %5.2f `_afe'   _continue
local stemp : display %5.2f `_lb_afe'
display " [" ltrim("`stemp'") "-"   _continue
local stemp : display %5.2f `_ub_afe'
display ltrim("`stemp'") "]"

display in text %`_varsize's "UnExposed"  _continue
display as result " " %5.0f `_Totunexp'  _continue
display " " %6.0f `_Caseunexp'    _continue
display " " %8.2f `_Riskunexp'  in text " {c |}"  _continue
display in text "{col 40}Attrib.risk.pop " _continue
display as result " " %5.2f `_afp'   _continue
local stemp : display %5.2f `_lb_afp'
display " [" ltrim("`stemp'") "-"   _continue
local stemp : display %5.2f `_ub_afp'
display ltrim("`stemp'") "]"

display in text "{hline 35}{c BRC}"
display

}

}

quietly :  {

cs `1' `2' if 1==1 `touse'
local  _rr = r(rr)
local _lb_rr = r(lb_rr)
local _ub_rr = r(ub_rr)

cs `1' `2', by("`by'")
local _chi2_mh = r(chi2_mh)
local  _rrmh = r(rr)
local _lb_rrmh = r(lb_rr)
local _ub_rrmh = r(ub_rr)

}

local _p = chi2tail(1,`_chi2_mh')   // df
display in text %40s "Test of Homogeneity (M-H) : pvalue :" as result %11.7f `_p'

display

display in text %40s "Crude RR for `2' :"  _continue
display as result " " %5.2f `_rr'   _continue
local stemp : display %5.2f `_lb_rr'
display " [" ltrim("`stemp'") "-"   _continue
local stemp : display %5.2f `_ub_rr'
display ltrim("`stemp'") "]"

display in text %40s "MH RR for `2' adjusted for `by' :" as result _continue
display as result " " %5.2f `_rrmh'   _continue
local stemp : display %5.2f `_lb_rrmh'
display " [" ltrim("`stemp'") "-"   _continue
local stemp : display %5.2f `_ub_rrmh'
display ltrim("`stemp'") "]"
local d=(((`_rrmh'-`_rr')/`_rr')*100)
display in text %40s "Adjusted/crude relative change :" as result " " %5.2f `d' " %"

if "`table'" != "" {
display
display
display in text %12s "`by'" " /"
display in text %`_varsize's "`_thevar'"  "    Total     Cases     Risk %          RR  "
display in text "{hline 60}"

quietly {
capture drop _ref
gen _ref = cond(`by' == 0 & `2' == 0,0,2)
// Exp11 Get Exposed , Cases, AR

// replace Exp11 Get Exposed , Cases, AR
replace _ref = 1 if `by' == 1 & `2' == 1
count if _ref == 1 `touse'
local _Totexp = r(N)
count if _ref == 1 & `1' == 1 `touse'
local _Caseexp = r(N)
local _Riskexp = `_Caseexp' / `_Totexp' * 100

// get RR and confidence interval
cs `1' _ref if _ref != 2 `touse'
local _p = r(p)
local  _rr = r(rr)
local _rr11 = `_rr'
}

display in text %`_varsize's "++ " _continue
display as result " " %8.0f `_Totexp'  _continue
display " " %8.0f `_Caseexp'    _continue
display " " %10.2f `_Riskexp'   _continue
display in text "{col 48}"  _continue
display as result " " %5.2f `_rr'

quietly : {

// replace Exp10 Get Exposed , Cases, AR
replace _ref = 2 if _ref == 1
replace _ref = 1 if `by' == 1 & `2' == 0
count if _ref == 1 `touse'
local _Totexp = r(N)
count if _ref == 1 & `1' == 1 `touse'
local _Caseexp = r(N)
local _Riskexp = `_Caseexp' / `_Totexp' * 100

// get RR and confidence interval
cs `1' _ref if _ref != 2 `touse'
local _p = r(p)
local  _rr = r(rr)
local _rr10 = `_rr'
}
display in text %`_varsize's "+- " _continue
display as result " " %8.0f `_Totexp'  _continue
display " " %8.0f `_Caseexp'    _continue
display " " %10.2f `_Riskexp'   _continue
display in text "{col 48}"  _continue
display as result " " %5.2f `_rr'

quietly : {

// replace Exp01 Get Exposed , Cases, AR
replace _ref = 2 if _ref == 1
replace _ref = 1 if `by' == 0 & `2' == 1
count if _ref == 1 `touse'
local _Totexp = r(N)
count if _ref == 1 & `1' == 1 `touse'
local _Caseexp = r(N)
local _Riskexp = `_Caseexp' / `_Totexp' * 100

// get RR and confidence interval
cs `1' _ref if _ref != 2 `touse'
local _p = r(p)
local  _rr = r(rr)
local _rr01 = `_rr'

}
display in text %`_varsize's "-+ " _continue
display as result " " %8.0f `_Totexp'  _continue
display " " %8.0f `_Caseexp'    _continue
display " " %10.2f `_Riskexp'    _continue
display in text "{col 48}"  _continue
display as result " " %5.2f `_rr'


quietly : {

// Reference Get Exposed , Cases, AR
count if _ref == 0 `touse'
local _Totexp = r(N)
count if _ref == 0 & `1' == 1 `touse'
local _Caseexp = r(N)
local _Riskexp = `_Caseexp' / `_Totexp' * 100
}

display in text %`_varsize's "-- " _continue
display as result " " %8.0f `_Totexp'  _continue
display " " %8.0f `_Caseexp'    _continue
display " " %10.2f `_Riskexp'    _continue
display in text "{col 48}"  _continue
display as result " Reference "
display in text "{hline 60}"

display

display in text "Observed RR when exposed to both = " as result %8.2f `_rr11'

local _inter = (`_rr10' -1) + (`_rr01' - 1) + 1
display in text "Expected RR if exposed to both and no interaction = " as result %8.2f `_inter'

local _inter = (`_rr11' - 1 ) - (`_rr10' -1) - (`_rr01' - 1)
display in text "Interaction = " as result %8.2f `_inter'
}
display

if "`plot'" != "" {
quietly : {
capture drop _pcase0 _pcase1
xi: glm `1' i.`by'*`2', f(binomial) l(log)
predict _pcase0 if `by'==0
label var _pcase0 "`by'==0"
predict _pcase1 if `by'==1
label var _pcase1 "`by'==1"
}
twoway (line _pcase0 `2' ) (line _pcase1 `2', xlab(0 "No" 1 "Yes") l1(probability))
drop _pcase0 _pcase1
}

end

