**** This code creates paper results for subprime stimulus variables
** All exclude capitals
*** 07/27
** 11/19
** 01/11

cd "/Users/aycankatitas/Dropbox/UVA stuff/usgr/"

set more off 

use "/Users/aycankatitas/Dropbox/UVA stuff/usgr/analysis_data/countylevel.dta",clear

global ylist newdummn
global xlist

*encode State, gen(State2)

** without capital
keep if capital==.


gen totalval_log = log(totalval_sum+0.1)
gen fdival1_log = log(fdival1+0.1)
gen fdival2_log = log(fdival2+0.1)
gen substimeduc_sc_mil = substimeduc_sc*1000000


*label variable RVoteshare "McCain Share 2008"
*label variable unemp08 "Unemployment Rate 2008 "
*label variable labforce "Labor Force 2008"
*label variable lagged_gfield "Foreign Greenfield Investment 2008"
*label variable cdR "Representative Partisanship"
*label variable logpatentcount "Patent Count (Logged) 2008"
*label variable logmacount "M\&A Count (Logged) 2008"
*label variable logtotaldom "Domestic Greenfield Investment (Logged) 2008"

*lab var tax_incentives_log "Tax Incentives (Logged)"
*lab var real_incentives_log "Real-Time Incentives (Logged)"
*lab var cash_incentives_log "Cash Incentives (Logged)"
*lab var land_incentives_log "Land Incentives (Logged)"
*label variable primestimdoe_log "Energy Stimulus (Logged)"
*label variable primestimdot_log "Transportation Stimulus (Logged)"
*label variable substimeduc_log "Education Stimulus (Logged)"
*label var substimeduccom_log "Education Stimulus to Community Colleges (Logged)"
*label var primestiminfra_log "Infrastructure Stimulus (Logged)"
*label var comp "Competitive County"
*label var comp2 "Competitive District"
*label var layoff "Mass Layoffs 2000-2006"
*label var elig "Eligible to Run"
*label var speced_sc "Share of Special Education Children"
*label var substimeduc_sc "Education Stimulus (Scaled)"
*label var substimeduc_sc_mil "Education Stimulus (Scaled)"
*lab var gdp08 "GDP Per Capita 2008"



label variable RVoteshare "John McCain Vote Share"
label variable unemp08 "Unemployment Rate"
label variable labforce "Labor Force"
label variable lagged_gfield "Foreign Greenfield Investment"
label variable cdR "Representative Partisanship"
label variable logpatentcount "Ln(Patent Count)"
label variable logmacount "Ln(M\&A Count)"
label variable logtotaldom "Ln(Domestic Greenfield Investment)"

lab var tax_incentives_log "Ln(Tax Incentives)"
lab var real_incentives_log "Ln(Real Incentives)"
lab var cash_incentives_log "Ln(Cash Incentives)"
lab var land_incentives_log "Ln(Land Incentives)"
label variable primestimdoe_log "Ln(Energy Stimulus)"
label variable primestimdot_log "Ln(Transportation Stimulus)"
label variable substimeduc_log "Ln(Education Stimulus)"
label var substimeduccom_log "Ln(Education Stimulus to Community Colleges)"
label var primestiminfra_log "Ln(Infrastructure Stimulus)"
label var comp "Competitive County"
label var comp2 "Competitive District"
label var layoff "2000-2006 Mass Layoffs"
label var elig "Eligible to Run"
label var speced_sc "Special Education Children"
label var substimeduc_sc "Education Stimulus Per Capita"
label var substimeduc_sc_mil "Education Stimulus Per Capita"
lab var gdp08 "GDP Per Capita"


label define newdummn 0 "Never Counties" 1 "Old Counties" 2 "New Counties"
label values newdummn newdummn


** TABLE A12 - Multinomial Logit Regression of Greenfield FDI on Stimulus and Real Incentives 
capture estimates drop *
qui {
mlogit $ylist substimeduc_log real_incentives_log RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store sr1, title("(1)")
estadd local state "yes", replace

mlogit $ylist substimeduc_log cash_incentives_log RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store sr2, title("(2)")
estadd local state "yes", replace

mlogit $ylist substimeduc_log land_incentives_log RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store sr3, title("(3)")
estadd local state "yes", replace
}

esttab sr1 sr2 sr3 using output/countylevel/mnlstimincen.tex,  se(2) b(2) label unstack noomitted noconstant nobaselevels replace  parentheses stats(N state, labels("Observations" "State-Fixed Effects")) ///
  drop(*.State _cons) ///
  nonumbers nonotes nogaps mlabels(, titles) ///
  title("Multinomial Logit Regression of Greenfield FDI on Stimulus and Real-Time Incentives") ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  order(real_incentives_log cash_incentives_log land_incentives_log substimeduc_log) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("Multinomial logit coefficients estimated via maximum likelihood. The outcome is a categorical variable that indicates  never, old and new county status. The baseline category is never counties. Model 1 includes all real-time incentives, Model 2 includes grants and training reimbursements, and Model 3 includes enterprise zones and infrastructure assistance. All models include state-fixed effects. Unless otherwise stated, variables are from 2008. Robust standard errors are in parentheses and clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
    
eststo clear 

** TABLE A13 - Multinomial Logit Regression of Greenfield FDI on Stimulus and Tax Incentives
capture estimates drop *
qui {
mlogit $ylist substimeduc_log tax_incentives_log RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store tx, title("(1)")
estadd local state "yes", replace
}

esttab tx using output/countylevel/mnltax.tex,  se(2) b(2) label unstack noomitted noconstant nobaselevels replace  parentheses stats(N state, labels("Observations" "State-Fixed Effects")) ///
  drop(*.State _cons) ///
  nonumbers nonotes nogaps mlabels(, titles) ///
  title("Multinomial Logit Regression of Greenfield FDI on Stimulus and Tax Incentives") ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  order(tax_incentives_log substimeduc_log) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("Multinomial logit coefficients estimated via maximum likelihood. The outcome is a categorical variable that indicates  never, old and new county status. The baseline category is never counties. The independent variable is tax incentives. The model include state-fixed effects. Robust standard errors are in parentheses and clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
  
eststo clear 


** TABLE A14 - Multinomial Logit Regression of Greenfield FDI on Competitive Counties and Mass Layoffs
capture estimates drop *
qui {
mlogit $ylist i.comp substimeduc_log real_incentives_log RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store sw1, title("(1)")
estadd local state "yes", replace

mlogit $ylist i.comp layoff substimeduc_log real_incentives_log RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store sw2, title("(2)")
estadd local state "yes", replace

mlogit $ylist i.comp##c.layoff substimeduc_log real_incentives_log RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store sw3, title("(3)")
estadd local state "yes", replace
}

esttab sw* using output/countylevel/mnlpols.tex,  se(2) b(2) label unstack noomitted noconstant nobaselevels replace  parentheses stats(N state, labels("Observations" "State-Fixed Effects")) ///
  drop(*.State _cons) ///
  nonumbers nonotes nogaps mlabels(, titles) ///
  title("Multinomial Logit Regression of Greenfield FDI on Competitive Counties and Mass Layoffs") ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  order(*.comp layoff *.comp#c.layoff) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("Multinomial logit coefficients estimated via maximum likelihood. The outcome is a categorical variable that indicates  never, old and new county status. The baseline category is never counties. An election is competitive if the difference in two-party vote share is less than 10 percentage points. All models include state fixed effects. Robust standard errors are in parentheses and clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
    
eststo clear 

** Margins output for education stimulus, real incentives and competitiveness effect 
*mlogit $ylist i.comp substimeduc_log real_incentives_log RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
*margins, at(real_incentives_log= (-7.3 -3.8)) predict(outcome(0)) vce(unconditional)
*margins, dydx(substimeduc_log real_incentives_log comp RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom) predict(outcome(2)) vce(unconditional) post

eststo mlogit 

foreach o in 0 1 2 {
  quietly margins, dydx(substimeduc_log real_incentives_log comp RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom) predict(outcome(`o')) vce(unconditional) post
  eststo, title(Outcome `o')
  estimates restore mlogit
}
eststo drop mlogit

esttab using output/figures/marginsoutput.csv, se(3) b(3) unstack noomitted noconstant nobaselevels noobs nostar wide replace

eststo clear 


* Results to create Figure 4 - figure created in figures.R
eststo clear 

mlogit $ylist i.comp##c.layoff substimeduc_log real_incentives_log RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)

eststo mlogit 

foreach o in 0 1 2 {
  quietly margins, at(comp=(0 1) layoff=(0 0.5 1 1.5 2 2.5)) vce(unconditional) predict(outcome(`o')) post
  eststo, title(Outcome `o')
  estimates restore mlogit
}
eststo drop mlogit

esttab using output/figures/complayoff.csv, se(3) b(3) unstack noomitted noconstant nobaselevels noobs nostar wide replace

** TABLE A15 - Multinomial Logit Regression of Greenfield FDI on Competitive Counties and Governor Term Limits 

capture estimates drop *
qui {
mlogit $ylist i.comp##i.elig substimeduc_log real_incentives_log RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store ce, title("(1)")
estadd local state "yes", replace
}

esttab ce using output/countylevel/mnlpols2.tex,  se(2) b(2) label unstack noomitted noconstant nobaselevels replace  parentheses stats(N state, labels("Observations" "State-Fixed Effects")) ///
  drop(*.State _cons) ///
  nonumbers nonotes nogaps mlabels(, titles) ///
  title("Multinomial Logit Regression of Greenfield FDI on Competitive Counties and Governor Term Limits") ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  order(comp elig comp#elig) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("Multinomial logit coefficients estimated via maximum likelihood. The outcome is a categorical variable that indicates  never, old and new county status. The baseline category is never counties. An election is competitive if the difference in two-party vote share is less than 10 percentage points. Eligibility to run is determined by whether a governor ran for the subsequent election. All models include state fixed effects. Robust standard errors are in parentheses and clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
    
* Results to create Figure A9 - figure created in figures.R
eststo clear 

mlogit $ylist i.comp##i.elig substimeduc_log real_incentives_log RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
eststo mlogit 

foreach o in 0 1 2 {
  quietly margins, over(comp elig) predict(outcome(`o')) post
  eststo, title(Outcome `o')
  estimates restore mlogit
}
eststo drop mlogit

esttab using output/figures/compelig.csv, se(3) b(3) unstack noomitted noconstant nobaselevels noobs nostar wide replace

** TABLE A16 - First Stage Regression Results for County-Level Local Government Incentives and Education Stimulus

eststo clear 

capture estimate drop *
qui{
	reg substimeduc_sc_mil speced_sc real_incentives_log i.comp RVoteshare unemp08 labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, cluster(State)
	est sto ivf, title("Education Stimulus Per Capita")
	estadd local state "yes", replace
}

esttab ivf using output/countylevel/ivcountyfirst.tex,  se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N state, labels("Observations" "State-Fixed Effects")) ///
  drop(*.State _cons) ///
  nonotes nogaps mlabels(, titles) ///
  title("First Stage Regression Results for County-Level Local Government Incentives and Education Stimulus") ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("The dependent variable is the amount of education stimulus scaled by a county's labor force. The independent variable is the number of special education students in a county divided by the county's school age population. Models include state fixed-effects. Unless otherwise stated, variables are from 2008. Robust standard errors are in parentheses and clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
  
eststo clear
  
** TABLE A17 - Two-Stage Least Squares Regressions of Greenfield FDI on Education Stimulus, County-Level
capture estimate drop *
qui{
	ivreg2 totalval_log real_incentives_log comp RVoteshare unemp08 labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State (substimeduc_sc_mil=speced_sc), cluster(State)
	est sto iv1, title("FDI All Counties")
	estadd local state "yes", replace
	
	ivreg2 fdival2_log real_incentives_log  comp RVoteshare unemp08 labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State (substimeduc_sc_mil=speced_sc), cluster(State)
	est sto iv2, title("FDI New Counties")
	estadd local state "yes", replace
	
	ivreg2 fdival1_log real_incentives_log comp RVoteshare unemp08 labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State (substimeduc_sc_mil=speced_sc), cluster(State)
	est sto iv3, title("FDI Old Counties")
	estadd local state "yes", replace
}

esttab iv1 iv2 iv3 using output/countylevel/ivcounty.tex,  se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N state, labels("Observations" "State-Fixed Effects")) ///
  drop(*.State _cons) ///
  nonotes nogaps mlabels(, titles) ///
  title("Two-Stage Least Squares Regressions of Greenfield FDI on Education Stimulus, County-Level") ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  order(substimeduc_sc_mil) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("The dependent variables are the log of total greenfield FDI in all counties, new counties and old counties respectively. The independent variables are the amount of education stimulus scaled by a county's labor population. Education stimulus is instrumented by the share of special education children in county's schools. Models include state fixed-effects. Unless otherwise stated, variables are from 2008. Robust standard errors are in parentheses and clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
  
* TABLE A18 - Multinomial Logit Regression of Greenfield FDI on Education Stimulus to Community Colleges and real Incentives

** Second-stage results 
capture estimates drop *
qui {
mlogit $ylist i.comp substimeduccom_log real_incentives_log RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store cc1, title("(1)")
estadd local state "yes", replace
}

esttab cc1 using output/countylevel/communitycollege.tex,  se(2) b(2) label unstack noomitted noconstant nobaselevels replace  parentheses stats(N state, labels("Observations" "State-Fixed Effects")) ///
  drop(*.State _cons) ///
  nonumbers nonotes nogaps mlabels(, titles) ///
  title("Multinomial Logit Regression of Greenfield FDI on Education Stimulus to Community Colleges and Real-Time Incentives") ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("Multinomial logit coefficients estimated via maximum likelihood. The outcome is a categorical variable that indicates  never, old and new county status. The baseline category is never counties. Education stimulus variable is measured by the subset of state stimulus to local governments allocated to community colleges. All models include state fixed effects. Robust standard errors are in parentheses and clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
    
eststo clear 

* TABLE A19 - Multinomial Logit Results - Different Types of Stimulus

capture estimates drop *
qui {
mlogit $ylist substimeduc_log primestimdoe_log real_incentives_log comp RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store a1, title("(1)")
estadd local state "yes", replace

mlogit $ylist substimeduc_log primestimdot_log real_incentives_log comp RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store a2, title("(2)")
estadd local state "yes", replace

mlogit $ylist substimeduc_log primestiminfra_log real_incentives_log comp RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store a3, title("(3)")
estadd local state "yes", replace

mlogit $ylist substimeduc_log primestimdoe_log primestimdot_log primestiminfra_log real_incentives_log comp RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store a4, title("(4)")
estadd local state "yes", replace
}


esttab a1 a2 a3 a4 using output/countylevel/mnldifferentstim.tex,  se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N state, labels("Observations" "State-Fixed Effects")) ///
  drop(*.State _cons) ///
  nonumbers nonotes nogaps mlabels(, titles) ///
  title("Multinomial Logit Results - Different Types of Stimulus") ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  order(primestimdoe_log primestimdot_log primestiminfra_log) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("Multinomial logit coefficients estimated via maximum likelihood. The dependent variables are categorical variables that indicate never, old and new county status. The baseline category is never counties. An election is competitive if the difference in two-party vote share is less than 10 percentage-points. All models include state-fixed effects. Unless otherwise stated, variables are from 2008. Robust standard errors are in parentheses and clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
  
*** TABLE A23 - Multinomial Logit Results - Including County Market Size

capture estimates drop *
qui {
mlogit $ylist substimeduc_log real_incentives_log comp RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom gdp08 i.State, vce(cluster State)
estimates store ms1, title("(1)")
estadd local state "yes", replace

}

esttab ms1 using output/countylevel/mnlmarketsize.tex,  se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N state, labels("Observations" "State-Fixed Effects")) ///
  drop(*.State _cons) ///
  nonumbers nonotes nogaps mlabels(, titles) ///
  title("Multinomial Logit Results - Including County Market Size") ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("Multinomial logit coefficients estimated via maximum likelihood. The dependent variable is a categorical variable that indicates a never, old and new county status. The baseline category is never counties. An election is competitive if the difference in two-party vote share is less than 10 percentage-points. Model 1 and 2 include state-fixed effects. GDP per capita measure is available only for metropolitan statistical areas. Unless otherwise stated, variables are from 2008. Robust standard errors are in parentheses and clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
 
  
** TABLE A20 - Multinomial Logit Results - Without Solar Energy
use "analysis_data/countylevelwosolar.dta",clear


global ylist newdummn
global xlist

*encode State, gen(State2)

** without capital
keep if capital==.


label variable RVoteshare "John McCain Vote Share"
label variable unemp08 "Unemployment Rate"
label variable labforce "Labor Force"
label variable lagged_gfield "Foreign Greenfield Investment"
label variable cdR "Representative Partisanship"
label variable logpatentcount "Ln(Patent Count)"
label variable logmacount "Ln(M\&A Count)"
label variable logtotaldom "Ln(Domestic Greenfield Investment)"

lab var real_incentives_log "Ln(Real Incentives)"
label variable substimeduc_log "Ln(Education Stimulus)"
label var comp "Competitive County"
label var comp2 "Competitive District"

label define newdummn 0 "Never Counties" 1 "Old Counties" 2 "New Counties"
label values newdummn newdummn

qui{
mlogit $ylist substimeduc_log real_incentives_log comp RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store a5, title("(1)")
estadd local state "yes", replace	
	
}

esttab a5 using output/countylevel/mnlsolar.tex,  se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N state, labels("Observations" "State-Fixed Effects")) ///
  drop(*.State _cons) ///
  nonumbers nonotes nogaps mlabels(, titles) ///
  title("Multinomial Logit Results - Without Solar Energy") ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("Multinomial logit coefficients estimated via maximum likelihood. The dependent variable is a categorical variable that indicates a never, old and new county status. The baseline category is never counties. An election is competitive if the difference in two-party vote share is less than 10 percentage-points. Model includes state-fixed effects. The sample excludes investment in renewable energy industry. Unless otherwise stated, variables are from 2008. Robust standard errors are in parentheses and clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
  

** TABLE A22 - Multinomial Logit Results - Without Tax Havens

use "analysis_data/countylevelwotax.dta",clear


global ylist newdummn
global xlist

*encode State, gen(State2)



** without capital
keep if capital==.


label variable RVoteshare "John McCain Vote Share"
label variable unemp08 "Unemployment Rate"
label variable labforce "Labor Force"
label variable lagged_gfield "Foreign Greenfield Investment"
label variable cdR "Representative Partisanship"
label variable logpatentcount "Ln(Patent Count)"
label variable logmacount "Ln(M\&A Count)"
label variable logtotaldom "Ln(Domestic Greenfield Investment)"

lab var real_incentives_log "Ln(Real Incentives)"
label variable substimeduc_log "Ln(Education Stimulus)"
label var comp "Competitive County"
label var comp2 "Competitive District"


label define newdummn 0 "Never Counties" 1 "Old Counties" 2 "New Counties"
label values newdummn newdummn

capture estimates drop *
qui {
mlogit $ylist substimeduc_log real_incentives_log comp RVoteshare unemp08  labforce cdR logpatentcount logmacount lagged_gfield logtotaldom i.State, vce(cluster State)
estimates store wo1, title("(1)")
estadd local state "yes", replace


}

esttab wo1  using output/countylevel/mnlwotax.tex,  se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N state, labels("Observations" "State-Fixed Effects")) ///
  drop(*.State _cons) ///
  nonumbers nonotes nogaps mlabels(, titles) ///
  title("Multinomial Logit Results - Without Tax Havens") ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("Multinomial logit coefficients estimated via maximum likelihood. The dependent variable is a categorical variable that indicates a never, old and new county status. The baseline category is never counties. An election is competitive if the difference in two-party vote share is less than 10 percentage-points. Model 1 and 2 include state-fixed effects. Sample excludes investment from the following countries: Netherlands, Switzerland, Luxembourg, Cayman Islands, Singapore, Panama, Mauritius, Cyprus, Ireland. Unless otherwise stated, variables are from 2008. Robust standard errors are in parentheses and clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
 
 
** TABLE A21 - Ordinary Least Squares Regression Results for Quid-Pro-Quo Greenfield FDI

use "analysis_data/quidproquo.dta",clear


lab var tradeop "Share of Protectionist Votes"
lab var tradeopb "Share of Protectionist Votes on Barriers"
lab var tradeops "Share of Protectionist Votes on Subsidies"
lab var unemptot "2008 Unemployment Rate"
lab var Party "Republican District"

capture estimates drop *

qui{
reg inv_value_log tradeop Party unemptot i.State, vce(cluster State)
estimates store q1, title("FDI")
estadd local state "yes", replace

reg inv_value_log tradeopb Party unemptot i.State, vce(cluster State)
estimates store q2, title("FDI")
estadd local state "yes", replace

reg inv_value_log tradeops Party unemptot i.State, vce(cluster State)
estimates store q3, title("FDI")
estadd local state "yes", replace
}

esttab q1 q2 q3 using output/countylevel/quidproquo.tex,  se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N state, labels("Observations" "State-Fixed Effects")) ///
order(tradeop tradeopb tradeops) ///
nonumbers nonotes nogaps mlabels(, titles) ///
  drop(*.State _cons)  ///
  title("Ordinary Least Squares Regression Results for Quid-Pro-Quo Greenfield FDI") ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  addnotes("The dependent variable is the logged cumulative value of new manufacturing FDI between 2009 and 2010. The independent variables are the share of all protectionist bills, protectionist bills on subsidies, and protectionist bills on barriers in the 109th and 110th Congresses. Unless otherwise stated, variables are from 2008. Robust standard errors are in parentheses and clustered by state. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
  
