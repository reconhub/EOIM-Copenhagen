* ccinter.ado
* Gilles Desve, 2006
* Thanks to Alain Moren and Aurelio Tobias
* Cohort analysis for outbreak investigation.
*
* Do a bivariate stratified analysis, presenting results (OR and 95%CI) for each stratum and adjusted result.
* In addition an additive interaction table is constructed
*
* syntax: csinter casevariable expvariable, by(stratification_variable) table plot
* options: 
*  by(stratification_variable)
*

capture program drop ccinter
program ccinter

syntax varlist(min=2 max=2) [if] [in],  by(varname) [table plot]

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

local varsize_ = 12
local ilength =length("`2'")
if `ilength' > 12 {
local varsize_ = `ilength'
}

quietly : {
// Get Total  case and controls
count if `1' != . & `2' != . & `by' != .  `touse'
local _Total = r(N)
count if `1' == . | `2' == . | `by' == . `touse'
local _Missing = r(N)
}
noisily : display as text " Number of obs = " %5.0f `_Total' " , Missing = " %5.0f `_Missing'

quietly forvalues byvalue = 1(-1)0 {
// Get Exposed , case and controls
count if `2' == 1 & `1' == 1 & `by' == `byvalue' `touse'
local _ExpCase = r(N)
count if `2' == 1 & `1' == 0 & `by' == `byvalue' `touse'
local _ExpControl = r(N)

// Get UnExposed , case and Controls
count if `2' == 0 & `1' == 1 & `by' == `byvalue' `touse'
local _UnExpCase = r(N)
count if `2' == 0 & `1' == 0 & `by' == `byvalue' `touse'
local _UnExpControl = r(N)

// get RR and confidence interval
cc `1' `2' if `by' == `byvalue' `touse'
local _p = r(p)
local  _rr = r(or)
local _lb_rr = r(lb_or)
local _ub_rr = r(ub_or)
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
  local varsize_ = 12
}
local _thevar = substr("`2'",1,`varsize_')

noisily : {
display
// This number are calculated to allow copy and paste
if `byvalue'==1 display in text %15s "`by' = Exposed"
if `byvalue'==0 display in text %15s "`by' = Unexposed"

display in text "{hline 30}{c TRC}"
display in text %`varsize_'s "`_thevar'" "  Cases   Controls{c |}"

display in text "{hline 30}{c RT}" _continue
display in text "{col 35}     Odds Ratio "  _continue
display as result " " %5.2f `_rr'   _continue
local stemp : display %5.2f `_lb_rr'
display " [" ltrim("`stemp'") "-"   _continue
local stemp : display %5.2f `_ub_rr'
display ltrim("`stemp'") "]"

display in text %`varsize_'s "Exposed"  _continue
display as result " " %5.0f `_ExpCase'  _continue
display "  " %6.0f `_ExpControl'    _continue
display "   " in text " {c |}"  _continue
display in text "{col 35}Attrib.risk.exp "  _continue
display as result " " %5.2f `_afe'   _continue
local stemp : display %5.2f `_lb_afe'
display " [" ltrim("`stemp'") "-"   _continue
local stemp : display %5.2f `_ub_afe'
display ltrim("`stemp'") "]"

display in text %`varsize_'s "UnExposed"  _continue
display as result " " %5.0f `_UnExpCase'  _continue
display "  " %6.0f `_UnExpControl'    _continue
display "   " in text " {c |}"  _continue
display in text "{col 35}Attrib.risk.pop " _continue
display as result " " %5.2f `_afp'

display in text "{hline 30}{c BRC}"
}

quietly : {
// Get Total  case and controls
count if `1' == 1 & `2' != . & `by' == `byvalue' `touse'
local _TotCase = r(N)
count if `1' == 0 & `2' != . & `by' == `byvalue' `touse'
local _TotControl = r(N)
local _PercCase = `_ExpCase' / `_TotCase' * 100
local _PercControl = `_ExpControl' / `_TotControl' * 100
}
noisily : {
display in text %`varsize_'s "Total"  _continue
display as result " " %5.0f `_TotCase'  _continue
display "  " %6.0f `_TotControl'

display in text %`varsize_'s "Exp %"  _continue
display as result " " %5.0f `_PercCase' "%"  _continue
display " " %6.0f `_PercControl' "%"

display

}

}

quietly :  {

cc `1' `2' if 1==1 `touse'
local  _rr = r(or)
local _lb_rr = r(lb_or)
local _ub_rr = r(ub_or)

cc `1' `2' if 1==1 `touse' , by("`by'") 
local _chi2_p = r(chi2_p)
local  _rrmh = r(or)
local _lb_rrmh = r(lb_or)
local _ub_rrmh = r(ub_or)

}

local _p = chi2tail(1,`_chi2_p')   // df
display in text %40s "Test of Homogeneity (M-H) : pvalue :" as result %11.7f `_p'

display

display in text %40s "Crude OR for `2' :"  _continue
display as result " " %5.2f `_rr'   _continue
local stemp : display %5.2f `_lb_rr'
display " [" ltrim("`stemp'") "-"   _continue
local stemp : display %5.2f `_ub_rr'
display ltrim("`stemp'") "]"

display in text %40s "MH OR for `2' adjusted for `by' :" as result _continue
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
display in text %`varsize_'s "`_thevar'"  "    Cases     Controls         OR  "
display in text "{hline 50}"

quietly {
capture drop _ref
gen _ref = cond(`by' == 0 & `2' == 0,0,2)
// Exp11 Get Exposed , Cases, AR

// replace Exp11 Get Exposed , Cases, AR
replace _ref = 1 if `by' == 1 & `2' == 1
count if _ref == 1 & `1' == 1 `touse'
local _ExpCase = r(N)
count if _ref == 1 & `1' == 0 `touse'
local _ExpControl = r(N)

// get RR and confidence interval
cc `1' _ref if _ref != 2 `touse'
local _p = r(p)
local  _rr = r(or)
local _rr11 = `_rr'
}
local stemp : display %6.2f `_rr'
DisplayOneLine `varsize_' "++" `_ExpCase' `_ExpControl' `stemp'

quietly : {

// replace Exp10 Get Exposed , Cases, AR
replace _ref = 2 if _ref == 1
replace _ref = 1 if `by' == 1 & `2' == 0
count if _ref == 1 & `1' == 1 `touse'
local _ExpCase = r(N)
count if _ref == 1 & `1' == 0 `touse'
local _ExpControl = r(N)

// get RR and confidence interval
cc `1' _ref if _ref != 2 `touse'
local _p = r(p)
local  _rr = r(or)
local _rr10 = `_rr'
}
local stemp : display %6.2f `_rr'
DisplayOneLine `varsize_' "+-" `_ExpCase' `_ExpControl' `stemp'

quietly : {

// replace Exp01 Get Exposed , Cases, AR
replace _ref = 2 if _ref == 1
replace _ref = 1 if `by' == 0 & `2' == 1
count if _ref == 1 & `1' == 1 `touse'
local _ExpCase = r(N)
count if _ref == 1 & `1' == 0 `touse'
local _ExpControl = r(N)

// get RR and confidence interval
cc `1' _ref if _ref != 2 `touse'
local _p = r(p)
local  _rr = r(or)
local _rr01 = `_rr'

}
local stemp : display %6.2f `_rr'
DisplayOneLine `varsize_' "-+" `_ExpCase' `_ExpControl' `stemp'

quietly : {

// Reference Get Exposed , Cases, AR
count if _ref == 0 & `1' == 1 `touse'
local _ExpCase = r(N)
count if _ref == 0 & `1' == 0 `touse'
local _ExpControl = r(N)

count if `1' == 1 & `2' != . & `by' != . `touse'
local _TotCase = r(N)
count if `1' == 0 & `2' != . & `by' != . `touse'
local _TotControl = r(N)
}

local _rr = " Reference "
DisplayOneLine `varsize_' "--" `_ExpCase' `_ExpControl' `_rr'

display in text "{hline 50}"
local _rr = " "
DisplayOneLine `varsize_' "Total" `_TotCase' `_TotControl' `_rr'

display

display in text "Observed OR when exposed to both = " as result %8.2f `_rr11'

local _inter = (`_rr10' -1) + (`_rr01' - 1) + 1
display in text "Expected OR if exposed to both and no interaction = " as result %8.2f `_inter'

local _inter = (`_rr11' - 1 ) - (`_rr10' -1) - (`_rr01' - 1)
display in text "Interaction = " as result %8.2f `_inter'
}
display

if "`plot'" != "" {
quietly : {
capture drop _pcase0 _ocase0 _pcase1 _ocase1
xi: glm `1' i.`by'*`2' if 1==1 `touse' , f(binomial) l(logit)
predict _pcase0 if `by'==0
generate _ocase0 = _pcase0/(1-_pcase0)
label var _ocase0 "`by'==0"
predict _pcase1 if `by'==1
generate _ocase1 = _pcase1/(1-_pcase1)
label var _ocase1 "`by'==1"
}
twoway (line _ocase0 `2') (line _ocase1 `2', xlab(0 "No" 1 "Yes") l1(log(odds)) yscale(log))
drop _pcase0 _ocase0 _pcase1 _ocase1
}

end

// =============================================
capture program drop DisplayOneLine
program define DisplayOneLine
args varsize_ LineHeader _ExpCase _ExpControl _rr

display in text %`varsize_'s "`LineHeader'" " " _continue
display as result " " %8.0f `_ExpCase'  _continue
display " " %8.0f `_ExpControl'    _continue
display in text "{col 36}"  _continue
display as result " " %12s "`_rr'"

end

