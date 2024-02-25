***
* Investment Incentives Can Attract FDI 
* Aycan Katitas & Sonal Pandya 
* Do File to Generate State-level Regression Results 
*** 

cd "/Users/aycankatitas/Dropbox/UVA stuff/usgr/"

use "analysis_data/statelevel11.dta", clear

** Generate DVs as changes 2011-2009 - (2008-2006)
* Total investment change
gen totalinvsum_change = totalinvsum - totalinvsum06_lag
* New county change 
gen groupsum_gr2_change = groupsum_gr2 - groupsum06_gr2
* Real incentives change
gen real_statelev_change = real_statelev_mil - real_statelev_06_mil
* Tax incentives change
gen tax_statelev_change = tax_statelev_mil - tax_statelev_06_mil




** Label regression variables 

*lab var fmap_mil "Medicaid Stimulus \\ (millions)"
*lab var share_kerry "Kerry's 2004 vote share"
*lab var union_share "\% Union share"
*lab var gdp_pc "GDP per person 16+  \\ (millions)"
*lab var per_empl_manu "\% Manufacturing employment"
*lab var popestimate2008_mil " State population 16+ \\ (millions)"
*lab var sachange_totalemp_lag "Total employment change \\ lagged"
*lab var instrument_mil "Pre-recession Medicaid spending \\ (millions)"
*lab var totalinvsum_lag "Greenfield FDI \\ lagged"
*lab var totalinvsum06_lag "Greenfield FDI 2006 \\ lagged"
*lab var groupsum_lag_gr2 "Greenfield FDI \\ lagged"
*lab var groupsum_lag_gr1 "Greenfield FDI \\ lagged"
*lab var totaldominvsum_lag "Domestic investment \\ lagged"
*lab var gdp_pcmil "GDP per person 16+"
*lab var popestimate2008 " State population 16+"
*lab var tot_incent_lag_mil "Incentives \\ Lagged"
*lab var unemprate08 "\% Unemployment rate"
*lab var unemprate06 "\% Unemployment rate"
*lab var tot_statelev_lag_mil "State-sponsored incentives \\ lagged"
*lab var real_statelev_mil "State-sponsored real incentives \\ (millions)"
*lab var tax_statelev_mil "State-sponsored tax incentives \\ (millions)" 
*lab var cash_statelev_lag_mil "State-sponsored cash incentives \\ lagged"
*lab var land_statelev_lag_mil "State-sponsored land incentives \\ lagged"


lab var fmap_mil "Medicaid Stimulus"
lab var share_kerry "2004 John Kerry Vote Share"
lab var union_share "2007 Unionization Share"
lab var gdp_pc "GDP Per Capita"
lab var per_empl_manu "Manufacturing Employment"
lab var popestimate2008_mil "Working Age Population"
lab var sachange_totalemp_lag "Employment Growth"
lab var instrument_mil "2007 Medicaid Spending"
lab var totalinvsum_lag "2003-2008 Greenfield FDI"
lab var totalinvsum06_lag "2003-2005 Greenfield FDI"
lab var groupsum_lag_gr2 "2003-2008 Greenfield FDI"
lab var groupsum_lag_gr1 "2003-2008 Greenfield FDI"
lab var totaldominvsum_lag "2003-2008 Domestic Investment"
lab var gdp_pcmil "GDP Per Capita"
*lab var popestimate2008 "Working Age Population"
lab var tot_incent_lag_mil "Prior State Incentive Spending"
lab var unemprate08 "Unemployment Rate"
lab var unemprate05 "Unemployment Rate"
lab var union_share05 "Unionization Share"
lab var gdp_pcmil_05 "GDP Per Capita"
lab var per_empl_manu_05 "Manufacturing Employment"
lab var labforce_05_mil "Labor Force"
lab var tot_statelev_lag_mil "State Incentives Spending"
lab var real_statelev_mil "State Real Incentives Spending"
lab var tax_statelev_mil "State Tax Incentives Spending" 
lab var cash_statelev_lag_mil "State Cash Incentives Spending"
lab var land_statelev_lag_mil "State Land Incentives Spending"


*** TABLE 1: FIRST STAGE 
capture estimates drop *
qui {	
reg fmap_mil instrument_mil, robust 
est sto first0, title("Medicaid Stimulus")
estadd local region "no", replace
estadd local F "377.85", replace

* F-score obtained by 
*ivregress 2sls totalinvsum (fmap_mil=instrument_mil) , vce(robust)
*weakivtest
	
reg fmap_mil instrument_mil region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil totalinvsum_lag sachange_totalemp_lag unemprate08, robust
estimates store first1, title("Medicaid Stimulus")
estadd local region "yes", replace
estadd local F "1,105.91", replace

* F-score obtained by 
* ivregress 2sls totalinvsum region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil totalinvsum_lag sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil) , vce(robust)
* weakivtest
}

esttab first0 first1 using output/statelevel/firststage.tex,  se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N region F, labels("Observations" "Region-fixed Effects" "F-Statistic")) ///
  drop(reg* _cons) ///
  nonotes nogaps nonumbers mlabels(, titles) ///
  title("First Stage Regression Results")  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("The dependent variable is post-recession Medicaid stimulus. Unless otherwise states, variables are from 2008. Robust standard errors are in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
  
eststo clear

**** TABLE 2: FDI
capture estimates drop *
qui {
** Investment IV 
ivregress 2sls totalinvsum region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil totalinvsum_lag sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil) , vce(robust)
estimates store sw1, title("FDI")
estadd local region "yes", replace

** Investment going to Group 2 

ivregress 2sls groupsum_gr2 region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil groupsum_lag_gr2 sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil) , robust
estimates store sw2, title("FDI in New Counties")
estadd local region "yes", replace

** Investment going to Group 1
ivregress 2sls groupsum_gr1 region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil groupsum_lag_gr1 sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil) , robust
estimates store sw3, title("FDI in Old Counties")
estadd local region "yes", replace

** Investment going to Group 2 Change 
ivregress 2sls groupsum_gr2_change region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil groupsum_lag_gr2 sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil) , robust
estimates store sw4, title("\$\Delta\$ FDI in New Counties")
estadd local region "yes", replace
}

esttab sw1 sw2 sw3 sw4 using output/statelevel/ivreginvestmentJan09Dec11.tex,  rename(groupsum_lag_gr2 totalinvsum_lag groupsum_lag_gr1 totalinvsum_lag) se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N region , labels("Observations" "Region-Fixed Effects")) ///
nonotes nogaps mlabels(, titles) ///
  drop(reg* _cons) ///
  title("Two-Stage Least Squares Regression of Greenfield FDI on Medicaid Stimulus")  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("The dependent variable is post-recession FDI. The independent variable is post-recession Medicaid stimulus. Unless otherwise stated, variables are from 2008. Robust standard errors are in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
  
eststo clear

***** TABLE 3: INCENTIVES 
capture estimates drop *
qui {
** Real state-sponsored incentives

ivregress 2sls real_statelev_mil region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil real_statelev_lag_mil sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil) , robust
estimates store si1, title("State-Sponsored Real Incentives}")
estadd local region "yes", replace

** Real state-sponsored incentives change 

ivregress 2sls real_statelev_change region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil sachange_totalemp_lag real_statelev_lag_mil unemprate08 (fmap_mil=instrument_mil) , vce(robust)
estimates store si2, title("\$\Delta\$ State-Sponsored Real Incentives")
estadd local region "yes", replace

** Tax state-sponsored incentives
ivregress 2sls tax_statelev_mil region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil tax_statelev_lag_mil sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil) , robust
estimates store si3, title("State-Sponsored Tax Incentives")
estadd local region "yes", replace

** Tax state-sponsored incentives change 
ivregress 2sls tax_statelev_change region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil tax_statelev_lag_mil sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil) , robust
estimates store si4, title("\$\Delta\$ State-Sponsored Real Incentives")
estadd local region "yes", replace

}

esttab si1 si2 si3 si4 using output/statelevel/ivregincentivesJan09Dec11.tex, rename(real_statelev_lag_mil tot_statelev_lag_mil tax_statelev_lag_mil tot_statelev_lag_mil) se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N region , labels("Observations" "Region-Fixed Effects")) ///
  drop(reg* _cons) ///
  nonotes nogaps mlabels(, titles) ///
  title("Two-Stage Least Squares Regression Of Investment Incentives on Medicaid Stimulus")  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
   addnotes("The dependent variable is post-recession state sponsored incentives. The independent variable is post-recession Medicaid stimulus. Unless otherwise stated, variables are from 2008. Robust standard errors are in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
  

eststo clear

***********************
******* APPENDIX TABLES
***********************

** TABLE A4 - Placebo Test: Two-Stage Least Squares Regression of 2006-2008 Greenfield FDI on Medicaid Stimulus
capture estimates drop *
qui {
* Total Investment 2006-2008
ivregress 2sls totalinvsum06 region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share05 gdp_pcmil_05 per_empl_manu_05 labforce_05_mil totalinvsum06_lag unemprate05 (fmap_mil=instrument_mil) , vce(robust)
estadd local region "yes", replace 
estimates store i61, title("FDI")

* New Counties 2006-2008
ivregress 2sls groupsum06_gr2 region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share05 gdp_pcmil_05 per_empl_manu_05 labforce_05_mil groupsum06_lag_gr2 unemprate05 (fmap_mil=instrument_mil) , vce(robust)
estimates store i62, title("FDI in New Counties")
estadd local region "yes", replace 

ivregress 2sls groupsum06_gr1 region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share05 gdp_pcmil_05 per_empl_manu_05 labforce_05_mil  groupsum06_lag_gr1 unemprate05 (fmap_mil=instrument_mil) , vce(robust)
estimates store i63, title("FDI in Old Counties")
estadd local region "yes", replace 
}

esttab i6* using output/statelevel/ivregincent06.tex,  rename(groupsum06_lag_gr2 totalinvsum06_lag groupsum06_lag_gr1 totalinvsum06_lag) se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N region , labels("Observations" "Region-Fixed Effects")) ///
  drop(reg* _cons) ///
  nonotes nogaps mlabels(, titles) ///
  title("Placebo Test: Two-Stage Least Squares Regression of 2006-2008 Greenfield FDI on Medicaid Stimulus")  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("The dependent variable for the placebo test is the 2006-2008 total value of new manufacturing greenfield FDI in Model 1, total value of new manufacturing greenfield FDI in new counties in Model 2, and old counties in Model 3. 2003-2005 Greenfield FDI is calculated respective to these three groups. Unless otherwise stated, variables are from 2005. Unlike previous regressions, GDP and manufacturing employment is scaled by state labor force. We leave out the employment growth variable from the regressions due to data unavailability. Robust standard errors are in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.010")
  
eststo clear

** TABLE A5 - Two-Stage Least Squares Regression of Non-Federal Incentives on Medicaid Stimulus

capture estimates drop *
qui {
** Total real incentives
ivregress 2sls real_incent_mil region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil real_incent_lag_mil sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil) , robust
estimates store ii1, title("Real Incentives")
estadd local region "yes", replace

** Total tax incentives
ivregress 2sls tax_incent_mil region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil tax_incent_lag_mil sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil) , robust
estimates store ii2, title("Tax Incentives")
estadd local region "yes", replace
}

esttab ii1 ii2 using output/statelevel/diffincentives.tex, rename(real_incent_lag_mil tot_incent_lag_mil tax_incent_lag_mil tot_incent_lag_mil) se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N region , labels("Observations" "Region-fixed Effects")) ///
  drop(reg* _cons) ///
  nonotes nogaps mlabels(, titles) ///
  title("Two-Stage Least Squares Regression of Non-Federal Incentives on Medicaid Stimulus")  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
   substitute([htbp] [!htbp] {l} {p{0.75\linewidth}}) ///
  addnotes("The dependent variables consist of real state and local incentives in Model 1, and tax state and local incentives in Model 2. Prior state incentives spending measures respective incentive spending before 2009. Unless otherwise stated, variables are from 2008. Robust standard errors are in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.010")
  
 
eststo clear 

** TABLE A6 - Two-Stage Least Squares Regression of Cash and Land Incentives on Medicaid Stimulus
capture estimates drop *
qui {
ivregress 2sls cash_statelev_mil region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil cash_statelev_lag_mil sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil), robust
estimates store c1, title("Cash Incentives")
estadd local region "yes", replace

ivregress 2sls land_statelev_mil region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil land_statelev_lag_mil sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil), robust
estimates store c2, title("Land Incentives")
estadd local region "yes", replace
}

esttab c1 c2 using output/statelevel/cashlandincentives.tex, rename(cash_statelev_lag_mil tot_incent_lag_mil land_statelev_lag_mil tot_incent_lag_mil) se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N region , labels("Observations" "Region-fixed Effects")) ///
  drop(reg* _cons) ///
  nonotes nogaps mlabels(, titles) ///
  title("Two-Stage Least Squares Regression of Cash and Land Incentives on Medicaid Stimulus")  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  order(fmap_mil share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil sachange_totalemp_lag unemprate08 cash_statelev_lag_mil land_statelev_lag_mil) ///
   substitute([htbp] [!htbp] {l} {p{0.75\linewidth}}) ///
  addnotes("The dependent variable in Model 1 comprise 2009-2011 grants and training reimbursements. The dependent variable in Model 2 comprise 2009-2011 enterprise zones and grants. Prior state incentives spending measures respective incentive spending before 2009. Unless otherwise stated, variables are from 2008. Robust standard errors are in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.010")
  
** TABLE A7 - OLS Regression of State Greenfield FDI on Incentives in New and Old Counties
capture estimates drop *
qui {
reg groupsum_gr2 real_statelev_mil region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil groupsum_lag_gr2 sachange_totalemp_lag unemprate08 , robust
estimates store fi1, title("FDI in New Counties")
estadd local region "yes", replace

reg groupsum_gr2 tax_statelev_mil region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil groupsum_lag_gr2 sachange_totalemp_lag unemprate08, robust
estimates store fi2, title("FDI in New Counties")
estadd local region "yes", replace

reg groupsum_gr1 real_statelev_mil region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil groupsum_lag_gr1 sachange_totalemp_lag unemprate08, robust
estimates store fi3, title("FDI in Old Counties")
estadd local region "yes", replace

reg groupsum_gr1 tax_statelev_mil region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil groupsum_lag_gr1 sachange_totalemp_lag unemprate08, robust
estimates store fi4, title("FDI in Old Counties")
estadd local region "yes", replace
}


esttab fi1 fi2 fi3 fi4 using output/statelevel/fdiincentives.tex,  rename(groupsum_lag_gr1 groupsum_lag_gr2) se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N region , labels("Observations" "Region-Fixed Effects")) ///
  drop(reg* _cons) ///
  nonumbers nonotes nogaps mlabels(, titles) ///
  title("OLS Regression of Greenfield FDI on Incentives in New and Old Counties")  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  order(real_statelev_mil tax_statelev_mil) ///
  substitute([htbp] [!htbp] {l} {p{\linewidth}}) ///
  addnotes("The dependent variable in the first two models measure the 2009-2011 value of new manufacturing greenfield FDI in new counties. The dependent variable in the last two models measure the 2009-2011 value of new manufacturing greenfield FDI in old counties. The independent variables are state real and tax incentives 2009-2011. Unless otherwise stated, variables are from 2008. Robust standard errors are in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.010")

eststo clear  


** TABLE A8 - Two-Stage and Ordinary Least Squares Regression of Domestic Greenfield Investment on Medicaid Stimulus
capture estimates drop *
qui {
** Investment IV 
ivregress 2sls totaldominvsum region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil totaldominvsum_lag sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil) , vce(robust)
estimates store dr1, title("Domestic Investment")
estadd local region "yes", replace

** Real incentives 
reg totaldominvsum real_statelev_mil region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil totaldominvsum_lag sachange_totalemp_lag unemprate08 , robust
estimates store di1, title("Domestic Investment")
estadd local region "yes", replace

** Tax incentives 
reg totaldominvsum tax_statelev_mil region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil totaldominvsum_lag sachange_totalemp_lag unemprate08, robust
estimates store di2, title("Domestic Investment")
estadd local region "yes", replace
}

esttab dr1 di1 di2 using output/statelevel/ivregdominvincent.tex,  se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N region , labels("Observations" "Region-Fixed Effects")) ///
  drop(reg* _cons) ///
  nonotes nogaps mlabels(, titles) ///
  title("Two-Stage and Ordinary Least Squares Regression of Domestic Greenfield Investment on Medicaid Stimulus")  ///
  order(fmap_mil real_statelev_mil tax_statelev_mil) ///
  starlevels( * 0.10 ** 0.05 *** 0.010) /// 
   substitute([htbp] [!htbp] {l} {p{0.75\linewidth}}) ///
  addnotes("Model 1 is a 2SLS regression that regresses new domestic manufacturing greenfield investment on Medicaid stimulus. Model 2 and 3 are OLS regressions that regress new domestic manufacturing greenfield investment on state-sponsored real and tax incentives respectively. * p$<$0.10, ** p$<$0.05, *** p$<$0.010")
  
eststo clear


** TABLE A3 - Two-Stage Least Squares Regression of Greenfield FDI on Medicaid Stimulus 2009-2010

use "analysis_data/statelevel10.dta", clear
 
lab var fmap_mil "Medicaid Stimulus"
lab var share_kerry "2004 John Kerry Vote Share"
lab var union_share "2007 Unionization Share"
lab var per_empl_manu "Manufacturing Employment"
lab var popestimate2008_mil "Working Age Population"
lab var sachange_totalemp_lag "Employment Growth"
lab var instrument_mil "2007 Medicaid Spending"
lab var totalinvsum_lag "2003-2008 Greenfield FDI"
lab var groupsum_lag_gr2 "2003-2008 Greenfield FDI"
lab var groupsum_lag_gr1 "2003-2008 Greenfield FDI"
lab var gdp_pcmil "GDP Per Capita"
lab var unemprate08 "Unemployment Rate"

capture estimates drop *
qui {
** Investment IV 
ivregress 2sls totalinvsum region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil totalinvsum_lag sachange_totalemp_lag unemprate08 (fmap_mil=instrument_mil) , robust
estimates store swa1, title("FDI")
estadd local region "yes", replace

** Investment going to Group 2 

ivregress 2sls groupsum_gr2 region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil groupsum_lag_gr2 sachange_totalemp_lag unemprate08  (fmap_mil=instrument_mil) , robust
estimates store swa2, title("FDI in New Counties")
estadd local region "yes", replace

** Investment going to Group 1
ivregress 2sls groupsum_gr1 region_1 region_2 region_3 region_4 region_5 region_6 region_7 region_8 region_9 share_kerry union_share gdp_pcmil per_empl_manu popestimate2008_mil groupsum_lag_gr1 sachange_totalemp_lag  unemprate08 (fmap_mil=instrument_mil) , robust
estimates store swa3, title("FDI in Old Counties")
estadd local region "yes", replace
}

esttab swa1 swa2 swa3 using output/statelevel/ivreginvestmentincentiveJan09Dec10.tex, rename(groupsum_lag_gr2 totalinvsum_lag groupsum_lag_gr1 totalinvsum_lag) se(2) b(2) label unstack noomitted noconstant replace  parentheses stats(N region , labels("Observations" "Region-fixed Effects")) ///
  drop(reg* _cons) ///
  nonotes nogaps mlabels(, titles) modelwidth(20) ///
  title("Two-Stage Least Squares Regression of Greenfield FDI on Medicaid Stimulus 2009-2010")  ///
  starlevels( * 0.10 ** 0.05 *** 0.010) ///
  substitute([htbp] [!htbp] {l} {p{0.75\linewidth}}) ///
  addnotes("The dependent variables in Model 1, 2, and 3 are all greenfield FDI, greenfield FDI in new counties, and greenfield FDI in old counties 2009-2010, respectively. 2003-2008 Greenfield FDI is calculated respective to these three groups. Unless otherwise states, variables are from 2008. Robust standard errors are in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.010") 
  
