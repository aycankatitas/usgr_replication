##################
# Do Politicians Prioritize Investment Incentives? 
# Date: 09/07/2023 
# Author: Aycan Katitas 
# Descriptive Statistics
##################

library(dplyr)
library(readstata13)
library(foreign)
library(modelsummary)
library(estimatr)
library(tidyr)
library(tidyverse)
library(stargazer)
setwd("/Users/aycankatitas/Dropbox/UVA stuff/usgr")

## TABLE A1 - SUMMARY STATISTICS OF STATE-LEVEL REGRESSIONS 

statedf <- read.dta13("analysis_data/statelevel11.dta")

statedfsum <- statedf 

statedfsum <- statedfsum %>% 
  dplyr::select(state,state_abrev,
                totalinvsum,groupsum_gr1,groupsum_gr2,
                totalinvsum06,totaldominvsum,
                real_statelev_mil,tax_statelev_mil,land_statelev_mil,cash_statelev_mil,
                real_incent_mil,tax_incent_mil,
                fmap_mil,instrument_mil,
                share_kerry,union_share,gdp_pcmil,per_empl_manu,popestimate2008_mil,
                sachange_totalemp_lag,unemprate08)

summary(statedfsum)

# Change names
var.df <- tibble(var = c("totalinvsum",
                         "groupsum_gr1",
                         "groupsum_gr2",
                         "totalinvsum06",
                         "totaldominvsum",
                         "real_statelev_mil",
                         "tax_statelev_mil",
                         "land_statelev_mil",
                         "cash_statelev_mil",
                         "real_incent_mil",
                         "tax_incent_mil",
                         "fmap_mil",
                         "instrument_mil",
                         "share_kerry",
                         "union_share",
                         "gdp_pcmil",
                         "per_empl_manu",
                         "popestimate2008_mil",
                         "sachange_totalemp_lag",
                         "unemprate08"),
                 
                 var_name = c("New Manufacturing FDI (mil)\\ 2009-2011",
                              "New Manufacturing FDI (mil) \\ New Counties 2009-2011",
                              "New Manufacturing FDI (mil)\\  Old Counties 2009-2011",
                              "New Manufacturing FDI (mil)\\\ 2006-2008",
                              "New Manufacturing Domestic Investment (mil) \\\ 2009-2011",
                              "Real-Time Incentives (mil)",
                              "Tax Incentives (mil)",
                              "Land Incentives (mil)",
                              "Cash Incentives (mil)",
                              "State and Local \\ Real-Time Incentives (mil)",
                              "State and Local \\ Tax Incentives (mil)",
                              "Medicaid Stimulus 2009-2010 (mil)",
                              "Medicare Spending 2007 (mil)",
                              "\\% Kerry Vote Share 2004",
                              "\\% Union Share 2007",
                              "GDP per 16+ person 2008",
                              "\\% Manufacturing Employment 2008",
                              "Population 16+ 2008 (mil)",
                              "Employment Change 2008",
                              "\\% Unemployment Rate 2008"))


replaceVarName <- function(var.vec, var.df){
  # Prepare output vector
  out.vec <- rep(NA, length(var.vec))
  matches <- match(var.vec, var.df$var)
  out.vec <- var.df[matches,]$var_name
  
  if(any(is.na(out.vec))){
    warning(paste("Variable concordence missing: ", 
                  paste(var.vec[is.na(out.vec)], collapse = ", "), 
                  sep = ""))
  } else{
    #print("All variables successfully converted")
  }
  
  return(out.vec)
}

var.order <- c("^totalinvsum$",
               "^groupsum_gr1$",
               "^groupsum_gr2$",
               "^totalinvsum06$",
               "^totaldominvsum$",
               "^real_statelev_mil$",
               "^tax_statelev_mil$",
               "^land_statelev_mil$",
               "^cash_statelev_mil$",
               "^real_incent_mil$",
               "^tax_incent_mil$",
               "^fmap_mil$",
               "^instrument_mil$",
               "^share_kerry$",
               "^union_share$",
               "^gdp_pcmil$",
               "^per_empl_manu$",
               "^popestimate2008_mil$",
               "^sachange_totalemp_lag$",
               "^unemprate08$"
               )

var.label <- str_replace_all(var.order, "\\^", "")
var.label <- str_replace_all(var.label, "\\$", "")


sink("output/desc/stateivsummary.txt")
stargazer(statedfsum,
          digits=0,
          title="Summary Statistics and Data Sources for State-Level Regressions",
          nobs=FALSE,
          omit.summary.stat = c("p25", "p75"),
          covariate.labels = replaceVarName(var.vec = var.label,
                                            var.df = var.df))
sink()

## TABLE A - SUMMARY STATISTICS OF COUNTY LEVEL REGRESSION VARIABLES 


countysum <- read.dta("analysis_data/countylevel.dta")

countysum <- countysum %>% 
  filter(is.na(capital)) %>% 
  dplyr::select(substimeduc_log,
                primestimdot_log,primestimdoe_log,primestiminfra_log,
                real_incentives_log, cash_incentives_log, land_incentives_log,tax_incentives_log,
                comp,elig,layoff,
                RVoteshare,unemp08,labforce,logpatentcount,logmacount,logtotaldom,lagged_gfield)

summary(countysum)

sd(countysum$logtotaldom,na.rm=T)

# Change names
var.df <- tibble(var = c("substimeduc_log",
                         "primestimdot_log",
                         "primestimdoe_log",
                         "primestiminfra_log",
                         "real_incentives_log",
                         "cash_incentives_log",
                         "land_incentives_log",
                         "tax_incentives_log",
                         "comp",
                         "layoff",
                         "elig",
                         "RVoteshare",
                         "unemp08",
                         "labforce",
                         "logpatentcount",
                         "logmacount",
                         "logtotaldom"),
                 
                 var_name = c("Education Stimulus (Logged)",
                              "Transportation Stimulus (Logged)",
                              "Energy Stimulus (Logged)",
                              "Infrastructure Stimulus (Logged)",
                              "Real-Time Incentives (Logged)",
                              "Cash Incentives (Logged)",
                              "Land Incentives (Logged)",
                              "Tax Incentives (Logged)",
                              "Competitive County",
                              "Mass Layoffs (in 100,000s)",
                              "Governor Eligible to Run",
                              "McCain Vote Share 2008",
                              "\\% Unemployment Rate 2008",
                              "Labor Force (in millions)",
                              "Patent Count (Logged) 2008",
                              "M&A 2008",
                              "Domestic Greenfield Investment 2008"))


replaceVarName <- function(var.vec, var.df){
  # Prepare output vector
  out.vec <- rep(NA, length(var.vec))
  matches <- match(var.vec, var.df$var)
  out.vec <- var.df[matches,]$var_name
  
  if(any(is.na(out.vec))){
    warning(paste("Variable concordence missing: ", 
                  paste(var.vec[is.na(out.vec)], collapse = ", "), 
                  sep = ""))
  } else{
    #print("All variables successfully converted")
  }
  
  return(out.vec)
}

var.order <- c("^substimeduc_log$",
                         "^primestimdot_log$",
                         "^primestimdoe_log$",
                         "^primestiminfra_log$",
                         "^real_incentives_log$",
                         "^cash_incentives_log$",
                         "^land_incentives_log$",
                        "^tax_incentives_log$",
                         "^comp$",
                          "^elig$",
                         "^layoff$",
                         "^RVoteshare$",
                         "^unemp08$",
                         "^labforce$",
                         "^logpatentcount$",
                         "^logmacount$",
                         "^logtotaldom$"
               
)


var.label <- str_replace_all(var.order, "\\^", "")
var.label <- str_replace_all(var.label, "\\$", "")


sink("output/desc/countyivsummary.txt")
stargazer(countysum,
          digits=2,
          title="Summary Statistics and Data Sources for County-Level Regressions",
          nobs=FALSE,
          omit.summary.stat = c("p25", "p75"),
          covariate.labels = replaceVarName(var.vec = var.label,
                                            var.df = var.df))
sink()


## CALCULATE THE AVERAGE SIZE OF FDI IN THE SAMPLE PERIOD 

load("data/recessionfdi.Rdata")

## Calculate the average size of investment and jobs created for the period 
# 414 investments, 
desc  <- totalq %>% 
  summarise(across(c(value_2010,Jobs.Created), list(mean=~mean(.,na.rm=T),
                                                    median=~median(.,na.rm=T),
                                                    sd=~sd(.,na.rm=T))))

## INCENTIVES IN THE SAMPLE PERIOD 

incentives <- read.dta13("analysis_data/countylevel.dta")

# Average size of real and tax incentives in the county 

# Average real incentives is $506,338; tax incentives 2,849,303
# Median real and tax are 0 because few counties got incentives 
avg_incen <- incentives %>% 
  summarise(across(c(real_incentives,tax_incentives), list(mean=~mean(.,na.rm=T),
                                                           median=~median(.,na.rm=T),
                                                           sd=~sd(.,na.rm=T))))

# Job created per real incentive - 1.5 per each $100,000 real incentive 
jobs_percounty <- totalq %>% 
  group_by(FIPS) %>% 
  summarise(jobs=mean(Jobs.Created,na.rm=T))

incentives <- incentives %>% 
  left_join(.,jobs_percounty) %>% 
  mutate(jobs=ifelse(is.na(jobs),0,jobs),
         jobs.incen=jobs/(real_incentives/100000),
         jobs.incen=ifelse(is.na(jobs.incen)|jobs.incen==Inf,0,jobs.incen))

summary(incentives$jobs.incen)


## NEW/OLD COUNTY CHARACTERISTICS IN THE SAMPLE PERIOD

# Census Variable Comparison 

countysum <- read.dta("analysis_data/countylevel.dta")

countysum <- countysum %>% 
  filter(is.na(capital))

# breakdown into groups 
nevercounties <- countysum %>% 
  filter(newdummn==0) %>% 
  dplyr::select(FIPS,RVoteshare,unemp08)

oldcounties <- countysum %>% 
  filter(newdummn==1) %>% 
  dplyr::select(FIPS,RVoteshare,unemp08)

newcounties <- countysum %>% 
  filter(newdummn==2) %>% 
  dplyr::select(FIPS,RVoteshare,unemp08)

# 2010 census data 
bach <- read.csv("data/census10/bachedu.csv")
foreign <- read.csv("data/census10/foreignborn.csv")
median <- read.csv("data/census10/medianincome.csv")
totpop <- read.csv("data/census10/population.csv")
urban <- read.csv("data/census10/urbanrural.csv")
white <- read.csv("data/census10/white.csv")
perpov <- read.csv("data/census10/perpov.csv")


# get rid of revised counts in population 
totpop$totpop <- gsub("\\(.*","",totpop$totpop)
totpop$totpop <- as.numeric(totpop$totpop)

totpop2 <- left_join(totpop,foreign) %>% 
  left_join(.,urban) %>% 
  left_join(.,white) %>% 
  mutate(perfor = foreignborn/totpop,
         perurban = urban/totpop,
         perwhite=white/totpop) %>% 
  dplyr::select(fips,totpop,starts_with("per"))

totpop2 <- left_join(totpop2,bach) %>% 
  left_join(.,median) %>% 
  left_join(.,perpov) 

desc_stat <- function(dataset,totpop2){
  filename <- paste0("data/countysamp/",dataset,".csv")
  
  nevercounties <- read.csv(filename) %>% 
    rename(fips=FIPS) %>% 
    dplyr::select(-unemp08)
  
  nevercounties2 <- left_join(nevercounties,totpop2) %>% 
    mutate(perbach=bachedu/100,
           perpov = perpov/100) %>% 
    dplyr::select(-bachedu,-fips)
  
  neveravg <- nevercounties2 %>% 
    summarise_all(funs(mean,sd,min,max),na.rm=TRUE)
  
  neveravg_long <- reshape(neveravg, direction="long",
                           varying=list(c("RVoteshare_mean","totpop_mean","perfor_mean","perurban_mean","perwhite_mean","medianincome_mean","perpov_mean","perbach_mean"), c("RVoteshare_sd","totpop_sd","perfor_sd","perurban_sd","perwhite_sd","medianincome_sd","perpov_sd","perbach_sd"), c("RVoteshare_min","totpop_min","perfor_min","perurban_min","perwhite_min","medianincome_min","perpov_min","perbach_min"), c("RVoteshare_max","totpop_max","perfor_max","perurban_max","perwhite_max","medianincome_max","perpov_max","perbach_max")), 
                           v.names=c("mean","sd","min","max"),
                           times = c("RVoteshare","totpop","perfor","perurban","perwhite","medianincome","perpov","perbach")) %>% 
    dplyr::select(-id) %>% 
    rename(variable=time)
  
  neveravg_long
}

neversample <- desc_stat("nevercounties",totpop2)
oldsample <- desc_stat("oldcounties",totpop2)
newsample <- desc_stat("newcounties",totpop2)

nevercounties2 <- nevercounties %>% 
  rename(fips=FIPS) %>% 
  dplyr::select(-unemp08) %>% 
  left_join(.,totpop2) %>% 
  mutate(perfor=perfor*100,
         perurban=perurban*100,
         perwhite=perwhite*100) %>%
  rename(perbach=bachedu)%>% 
  mutate(group="never")

oldcounties2 <- oldcounties %>% 
  rename(fips=FIPS) %>% 
  dplyr::select(-unemp08) %>% 
  left_join(.,totpop2) %>% 
  mutate(perfor=perfor*100,
         perurban=perurban*100,
         perwhite=perwhite*100) %>%
  rename(perbach=bachedu)%>% 
  mutate(group="old")

newcounties2 <- newcounties %>% 
  rename(fips=FIPS) %>% 
  dplyr::select(-unemp08) %>% 
  left_join(.,totpop2) %>% 
  mutate(perfor=perfor*100,
         perurban=perurban*100,
         perwhite=perwhite*100) %>%
  rename(perbach=bachedu)%>% 
  mutate(group="new")

descsample <- rbind(nevercounties2,oldcounties2,newcounties2)

descsample2 <- descsample %>% 
  ungroup() %>% 
  dplyr::select(-fips) %>% 
  rename(`2008 McCain Share`=RVoteshare,
         `Population`=totpop,
         `% Foreign-Born`=perfor,
         `% Urban`=perurban,
         `% White`=perwhite,
         `Median Income`=medianincome,
         `% Poverty`=perpov,
         `% College-Educated`=perbach) %>%
  mutate(group=ifelse(group=="never"," Never Counties",
                      ifelse(group=="old","Old Counties","New Counties")))

# TABLE 9 - Difference in Means T-tests for None, Old, and New Counties in County-Level Regressions
datasummary_balance(~group, 
                    descsample2,
                    dinm=F,
                    output="output/desc/balancetable2.tex",
                    fmt="%.0f")

# TABLE A10 - Summary Statistics and Data Sources for County-Level Regressionsd

# t-test never new 
descnevernew  <- descsample2 %>% 
  filter(group!="Old Counties")

df <- lapply(descnevernew[,-9], function(i) t.test(i ~ descnevernew$group))


# % difference between new/old county industries 

load("data/newoldcountycharacteristics.Rdata")

newold_ind_ch <- newold_ind %>% 
  ungroup() %>% 
  dplyr::select(-total) %>% 
  pivot_wider(names_from=newdummn,
              values_from=count:per,
              names_prefix="group",
              values_fill=0) %>% 
  mutate(growth_rate = (count_group2-count_group1)/count_group1,
         percent_diff= per_group2-per_group1) %>% 
  arrange(desc(percent_diff))


# Calculate the average size of investment and jobs for old and new counties
newold_size <- newold %>% 
  filter(Investing.Company!="SASOL") %>% 
  group_by(newdummn) %>% 
  # summarize(across(c(Jobs.Created,value_2010),list(mean=~mean(.,na.rm=T),
  #                                                  median=~median(.,na.rm=T),
  #                                                  sd=~sd(.,na.rm=T)))) %>%
  mutate(newdummn=as.factor(ifelse(newdummn==1,"Old Counties","New Counties"))) %>% 
  rename(Jobs=Jobs.Created,
         `Value (2010$ million)`=value_2010)

# FOOTNOTE 

datasummary(Jobs+`Value (2010$ million)`~newdummn*(Mean+Median+SD),
            data=newold_size,
            output="output/desc/newoldjobs.tex",
            fmt = "%.2f",
            title="New Manufacturing FDI Value and Jobs Created in Old and New Counties"
)

## Checking new/old/never counties' jobs in foreign multinationals before the recession 

load("data/FDIjobscounty.Rdata")

# merge 1990 county population 
cty90 <- read.csv("data/geocorr1990_17OCT1400218.csv") %>%  
  mutate(FIPS=as.numeric(county)) %>% 
  rename(pop90=X1990.POP) %>% 
  dplyr::select(FIPS,pop90)

pregrjobs <- jobs.county %>% 
  filter(Year<2003) %>% 
  mutate(FIPS=as.numeric(county)) %>% 
  group_by(FIPS,cntyname,pop2k) %>% 
  summarise(across(Jobs.in.Foreign.Owned.Establishments:Number.of.FOEs.Originating.before.1991,sum,na.rm=T)) %>%
  left_join(.,cty90)

# 1331 counties missing - 1297 never, 34 new 
pregr2 <- fipsquarter %>% 
  left_join(.,pregrjobs) %>%
  #filter(is.na(cntyname)) %>%
  #count(newdummn) %>% 
  filter(!is.na(cntyname)) %>% 
  mutate(pop2k=as.numeric(pop2k)) %>%
  mutate_at(vars(Jobs.in.Foreign.Owned.Establishments:Number.of.FOEs.Originating.before.1991), list(pc = ~./pop90)) %>% 
  #dplyr::select(-pop2k)  
  mutate(countycat=ifelse(newdummn==0,"Never Counties",
                          ifelse(newdummn==1,"Old Counties","New Counties"))) %>% 
  dplyr::select(countycat,starts_with("Jobs.in.Foreign.Owned.Establishments"),starts_with("Jobs.in.FOEs.in.Goods.Industries"),starts_with("Jobs.in.FOEs.Originating.as.Openings.since.1991"),starts_with("Number.of.FOEs.Originating.before.1991"))
#ungroup() %>% 
#group_by(newdummn) %>%
#summarise(across(Jobs.in.Foreign.Owned.Establishments:Number.of.FOEs.Originating.before.1991_pc,median,na.rm=T)) 

names(pregr2) <- c("countycat",
                   "Jobs in Foreign Establishments",
                   "Jobs in Foreign Establishments Per Capita",
                   "Jobs in Foreign Establishments - Goods Industries",
                   "Jobs in Foreign Establishments - Goods Industries Per Capita",
                   "Jobs in Greenfield Foreign Establishments Since 1991",
                   "Jobs in Greenfield Foreign Establishments Since 1991 Per Capita",
                   "Number of Foreign Establishments Before 1991",
                   "Number of Foreign Establishments Before 1991 Per Capita")

pregr2$countycat <- factor(pregr2$countycat,
                           levels=c("Never Counties","Old Counties","New Counties"))

## Create summary table 
datasummary(All(pregr2)~Median*countycat,
            data=pregr2,
            output="output/pregrsumtable.tex",
            fmt = "%.2f"
)

## NEW/OLD COUNTY CHARACTERISTICS - COMPETITIVE COUNTIES 

analysisdf <- read.dta("analysis_data/countylevel.dta")

analysisdf <- analysisdf %>% 
  filter(is.na(capital))

prop.table(table(analysisdf$newdummn,analysisdf$comp),1)

## BEFORE/AFTER RECESSION TOTAL FDI VALUATION 

load("data/usfdisample.Rdata")

befaf <- fulldatam %>% 
  filter((Year>2004&Year<2008)|(Year>2008&Year<2012)) %>% 
  mutate(group=ifelse((Year>2004&Year<2008),"bef","af")) %>% 
  group_by(group) %>% 
  summarise(totalinv=sum(value_2010,na.rm=T))


## NEW/OLD COUNTY COMPANY EXPLORATION 

load("data/newoldcountycharacteristics.Rdata")

chinese <- newold %>% 
  filter(Source.Country=="China") %>% 
  count(newdummn,Industry.Sector)

newind <- newold %>% 
  filter(newdummn==2) %>%
  add_count() %>% 
  mutate(total=n) %>% 
  group_by(total,Industry.Sector) %>%
  summarise(indcount=n()) %>%
  mutate(per=indcount/total) %>%
  arrange(desc(per))
  
## EXPLORE HOW MANY COMPANIES IN NEW COUNTIES HAVE NOT INVESTED BEFORE 
load("data/newoldcountycharacteristics.Rdata")
load("data/fdi0319.Rdata")

# List of names of top 10 highest value companies in new and old counties 

complists <- newold %>% 
  dplyr::select(newdummn,Investing.Company,Industry.Sector,value_2010) %>%
  
  filter(Investing.Company!="SASOL") %>% 
  group_by(newdummn) %>% 
  slice_max(order_by = value_2010, n = 20) 

# Fuzzy matching to see how many old and new county companies have invested i) in US before ii) in the world before 
library(fuzzyjoin)

# Check if new county firms have invested in the US before 
newfirms <- newold 

usfdi <- fdi %>% 
  rename(Investing.Company=`Investing Company`,
         Parent.Company=`Parent Company`,
         Destination.Country = `Destination Country`,
         Industry.Sector = `Industry Sector`) %>% 
  separate(`Project Date`,c("Year","Month","Day"),sep="-") %>%
  filter(Destination.Country=="United States") %>% 
  filter(Year>2002&Year<2009) %>% 
  dplyr::select(Investing.Company,Parent.Company) %>% 
  distinct() %>%
  mutate(invest=1)

us <- newfirms %>%
  mutate(prev_invest=ifelse(Investing.Company %in% usfdi$Investing.Company,1,0)) %>%
  filter(newdummn==1)

prop.table(table(us$prev_invest))

# us_invest <- newfirms %>% 
#   dplyr::select(Investing.Company,Parent.Company) %>% 
#   stringdist_join(., usfdi, 
#                   by = "Investing.Company",
#                   mode = "left",
#                   ignore_case = FALSE, 
#                   method = "jw", 
#                   max_dist = 99, 
#                   distance_col = "dist") %>% 
#   group_by(Investing.Company.x) %>%
#   slice_min(order_by = dist, n = 1)


worldfdi <- fdi %>% 
  rename(Investing.Company=`Investing Company`) %>% 
  separate(`Project Date`,c("Year","Month","Day"),sep="-") %>%
  filter(Year>2002&Year<2009) %>% 
  dplyr::select(Investing.Company,`Parent Company`) %>% 
  distinct()

anywhere <- newfirms %>% 
  mutate(prev_invest=ifelse(Investing.Company %in% worldfdi$Investing.Company,1,0))  %>%
  filter(newdummn==1)


prop.table(table(anywhere$prev_invest))


us_p <- newfirms %>% 
  mutate(prev_invest=ifelse(Parent.Company %in% usfdi$Parent.Company,1,0)) %>%
  filter(newdummn==1)


prop.table(table(us_p$prev_invest))


anywhere_p <- newfirms %>% 
  mutate(prev_invest=ifelse(Parent.Company %in% worldfdi$`Parent Company`,1,0))  %>%
  filter(newdummn==1)

prop.table(table(anywhere_p$prev_invest))


