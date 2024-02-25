##################
# Investment Incentives Attract FDI
# Date: 08/10/2023 
# Author: Aycan Gamache 
# Data Cleaning and Merging
##################

##################
### setwd and load libraries 

setwd("/Users/aycankatitas/Dropbox/UVA stuff/usgr")
library(dplyr)
library(lubridate)
library(tidyverse)
library(lfe)
library(interplot)
library(foreign)
library(readstata13)
library(purrr)
library(stargazer)
library(modelsummary)
library(estimatr)
library(stringr)
library(ggpattern)
library(tidytext)
library(maps)

### Parameters 
# Change to exclude renewable energy firms + tax havens out of the sample

excludesolar=FALSE
excludetaxhaven=FALSE

### Options 

# modelsummary package options 
options("modelsummary_format_numeric_latex" = "plain")

### Functions 

## Correct county names in the fdi data
correct_adminregion <- function(data){
  
  orx <- c("New York City County","San Francisco City & County","City of Danville County","City of Suffolk County",
           "Lexington-Fayette County","Denver City and County","Louisville Jefferson County","City of Virginia Beach County",
           "Seminole County","Lafayette Consolidated Government Parish","Unified Govt. of Wyandotte County","City of Norfolk County",
           "La Grange County","City of Newport News County","City of Bristol County","City of Charlottesville County",
           "City of Hopewell County","Broomfield City and County","Columbus-Muscogee County","City of Chesapeake County",
           "City of Martinsville County","City of Covington County","Ste. Genevieve County","St John The Baptist Parish",
           "Camden  County","Athens-Clarke County","La Porte County","City of Salem County","City of Lynchburg County",
           "City of Fredericksburg","City of Galax County","City of Waynesboro","City of Franklin County","Carson City",
           "City of Colonial Heights County","Wayne  County","Lewis And Clark County","Orleans County",
           "City of Emporia County","NevadaCounty","La PazCounty","King And Queen County","Honolulu City and County County"
  )
  
  tarx <- c("New York County","San Francisco County","Danville City","Suffolk City",
            "Fayette County","Denver County","Jefferson County","Virginia Beach City",
            "Seminole County","Lafayette Parish","Wyandotte County","Norfolk City",
            "LaGrange County","Newport News City","Bristol City","Charlottesville City",
            "Hopewell City","Broomfield County","Muscogee County","Chesapeake City",
            "Martinsville City","Covington City","Ste Genevieve County","St John the Baptist Parish",
            "Camden County","Clarke County","LaPorte County","Salem City","	Lynchburg City",
            "Fredericksburg City","Galax City","Waynesboro City","Franklin City","Carson City City",
            "Colonial Heights City","Wayne County","Lewis and Clark County","Orleans Parish",
            "Emporia City","Nevada County","La Paz County","King and Queen County","Honolulu County")
  
  data$County <- mgsub::mgsub(data$County, orx,tarx)
  
  data
}

## Create data frame for MNL regressions
prep_mnl <- function(data,year){
  
  year <- as.numeric(year)
  logit2 <- data
  
  # Log control variables 
  logit2 <- logit2 %>% 
    mutate(logpatentcount=log(patentcount+0.0001),
           logmacount=log(macount+0.0001),
           logtotaldom = log(totaldomval+0.0001))
  
  # Create previous gfield variable (both expansion+new)
  
  expcounty <- fulldatafips %>%
    filter(Industry.Activity=="Manufacturing") %>%
  # filter(!Industry.Sector=="Coal, oil & gas") %>% 
    filter(Year<year) %>% 
    dplyr::select(FIPS)
  
  expcounty <- unique(expcounty)
  
  logit2 <- logit2 %>% 
    mutate(lagged_gfield = ifelse(FIPS %in% expcounty$FIPS,1,0))
  
  # Create CD variable - 2008 who got control of the district 
  countycd <- ctycd %>% 
    dplyr::select(county,state,cd108,afact) %>% 
    rename(FIPS=county,
           statefips=state,
           cdid=cd108)
  
  countycd$FIPS <- as.character(countycd$FIPS)
  
  # Change at-large district to 0 in elec08
  
  elec08 <- elec08 %>% 
    mutate(cdid=ifelse(statefips==10|statefips==30|statefips==38|statefips==46|statefips==50|statefips==56,0,cdid))
  
  countycd2 <- left_join(countycd,elec08)%>% 
    mutate(cdR=ifelse(rankR==1,1,0))
  
  # look at duplicate counties 
  
  dupl <- countycd2 %>% 
    group_by(FIPS) %>% 
    mutate(count=n()) %>% 
    filter(count>1) %>% 
    dplyr::select(FIPS,statefips,cdid,cdR,afact)
  
  dupl2 <- dupl %>% 
    dplyr::select(FIPS,cdR,afact) %>% 
    group_by(FIPS,cdR) %>% 
    summarise(sum=sum(afact))
  
  ### Including close duplicates
  
  dupleq08 <- dupl2 %>% 
    filter(sum<0.60&sum>0.5) %>% 
    dplyr::select(FIPS)
  
  countyR <- countycd2 %>% 
    dplyr::select(FIPS,cdR,afact) %>% 
    group_by(FIPS,cdR) %>% 
    summarise(sum=sum(afact)) %>% 
    filter(sum>0.5&!is.na(cdR)) %>% 
    dplyr::select(-sum) %>% 
    mutate(FIPS=as.numeric(FIPS))
  
  logit3 <- left_join(logit2,countyR)
  
  logit3
  
}

## Collect subprime stimulus data 

prep_analysis <- function(filename,stimname){
  name=paste0("data/stimulus_files/",filename,".Rdata")
  load(name)
  
  datavalm <- dataval %>% 
    dplyr::select(fips,stimval) %>% 
    ungroup() %>%
    rename(FIPS=fips) %>% 
    rename(!!stimname:=stimval)
  
  logit9qsub <- left_join(logit9qsub,datavalm)
}

## Take a log of variables that contain 0 
logplus <- function(x){
  d <- log(x+0.0001)
}

outlier=TRUE # get rid of outliers
startdate= "2008-12-31"
enddate="2012-01-01"

# Creating the DV for state-level regressions 2009-2011
make_ivregdata <- function(outlier,startdate,enddate,incstart,incend){
  
  # FDI part
  # fulldatam only includes new projects
  totals <- fulldatam
  
  totals$date <- as.Date(with(totals, paste(Year,Month,Day, sep="-")))
  
  #fulldatafips includes new coloc and extension
  totalsfull <- fulldatafips %>% 
    filter(Industry.Activity=="Manufacturing") 
   # %>% filter(!Industry.Sector=="Coal, oil & gas") 
  
  totalsfull$date <- as.Date(with(totalsfull, paste(Year,Month,Day, sep="-")))
  
  # Dom inv part
  totalsdom <- domm
  
  totalsdom$date <- as.Date(with(totalsdom, paste(Year,Month,Day, sep="-")))
  
  totalsdomfull <- domfips3 %>% 
    filter(IndustryActivity=="Manufacturing") 
  # %>% filter(!IndustrySector=="Coal, oil & gas")
  
  totalsdomfull$date <- as.Date(with(totalsdomfull, paste(Year,Month,Day, sep="-")))
  
  if(outlier==TRUE){
    #fdi
    fdimean <- mean(totals$value_2010)
    fdi2sd <- 2*sd(totals$value_2010)
    fdi2sdm <- fdimean+fdi2sd
    
    totals <- totals %>% 
      filter(value_2010 <fdi2sdm)
    
    #dom
    dommean <- mean(totalsdom$value_2010)
    dom2sd <- 2*sd(totalsdom$value_2010)
    dom2sdm <- dommean + dom2sd
    
    totals <- totals %>% 
      filter(value_2010 <fdi2sdm)
    
    totalsdom <- totalsdom %>% 
      filter(value_2010<dom2sdm)
    
  } else{
    totals <- totals
    
    totalsdom <- totalsdom 
  }
  
  totals1 <- totals %>% filter(date <enddate & date>startdate)
  
  totalsdom1 <- totalsdom %>% filter(date <enddate & date>startdate)
  
  # Aggregate at the county level - FDI
  totalscount <- totals1 %>% 
    add_count(FIPS) %>% 
    rename(totalcount="n") %>% 
    group_by(FIPS) %>% 
    mutate(totalval_sum = sum(value_2010)) %>%
    dplyr::select(State,FIPS,totalcount, totalval_sum) %>% 
    arrange(FIPS)
  
  totalscount <- unique(totalscount)
  
  # Aggregate at the county level - Domestic Investment
  totalsdomcount <- totalsdom1 %>% 
    add_count(FIPS) %>% 
    rename(totaldomcount="n") %>% 
    group_by(FIPS) %>% 
    mutate(totaldomval_sum = sum(value_2010)) %>%
    dplyr::select(State,FIPS,totaldomcount, totaldomval_sum) %>% 
    arrange(FIPS)
  
  totalsdomcount <- unique(totalsdomcount)
  
  # Create dataset for new/old county dummies - only for fdi
  # New counties can have had expansions previously, we are only categorizing them as new if they have not received new investment in the past - later check if results are robust if we change this 
  totalscount_lagno <- totals %>% filter(date <startdate) %>% 
    add_count(FIPS) %>% 
    rename(totalcount_lag="n") %>% 
    group_by(FIPS) %>% 
    mutate(totalval_sum_lag = sum(value_2010)) %>%
    dplyr::select(State,FIPS,totalcount_lag, totalval_sum_lag) %>% 
    arrange(FIPS)
  
  totalscount_lagno <- unique(totalscount_lagno)
  
  # Create new county dummy variable 
  
  totalscount <- totalscount %>% 
    mutate(newdummn=ifelse(FIPS %in% totalscount_lagno$FIPS,1,2))
  
  # Add lagged investment variable - we include both extensions and new investment for lagged
  # FDI 
  totals2 <- totalsfull %>% filter(date <startdate)
  
  totalscount_lag <- totals2 %>% 
    add_count(FIPS) %>% 
    rename(totalcount_lag="n") %>% 
    group_by(FIPS) %>% 
    mutate(totalval_sum_lag = sum(value_2010)) %>%
    dplyr::select(State,FIPS,totalcount_lag, totalval_sum_lag) %>% 
    arrange(FIPS)
  
  totalscount_lag <- unique(totalscount_lag)
  
  totalscountfin <- full_join(totalscount,totalscount_lag)
  
  # Add lagged investment variable - Domestic Investment 
  totalsdom2 <- totalsdomfull %>% filter(date <startdate)
  
  totalsdomcount_lag <- totalsdom2 %>% 
    add_count(FIPS) %>% 
    rename(totaldomcount_lag="n") %>% 
    group_by(FIPS) %>% 
    mutate(totaldomval_sum_lag = sum(value_2010)) %>%
    dplyr::select(State,FIPS,totaldomcount_lag, totaldomval_sum_lag) %>% 
    arrange(FIPS)
  
  totalsdomcount_lag <- unique(totalsdomcount_lag)
  
  totalsdomcount_lag2 <- full_join(totalsdomcount,totalsdomcount_lag)
  
  totalscountfin2 <- full_join(totalscountfin,totalsdomcount_lag2)
  
  
  vars <- c("totalcount","totalval_sum","totalcount_lag","totalval_sum_lag","newdummn",
            "totaldomcount","totaldomval_sum","totaldomcount_lag","totaldomval_sum_lag")
  
  totalscountfin2[vars][is.na(totalscountfin2[vars])] <- 0
  
  # ADDING CONTROLS 
  # County population
  
  countypop08 <- read.csv("data/countypop08.csv") %>% 
    dplyr::select(STATE,COUNTY,POPESTIMATE2008) %>% 
    filter(COUNTY!=0) %>% 
    mutate(county=sprintf("%03d",COUNTY)) %>% 
    mutate(FIPS=as.numeric(paste0(STATE,county,sep=""))) %>% 
    dplyr::select(FIPS,POPESTIMATE2008)
  
  totalscountfin2 <- left_join(totalscountfin2,countypop08)
  
  
  # Create total investment value + investment value by county categorization
  
  totalscount3 <- totalscountfin2 %>% 
    group_by(State) %>% 
    mutate(totalinvsum= sum(totalval_sum),
           statepop = sum(POPESTIMATE2008),
           totalinvsum_lag=sum(totalval_sum_lag),
           totaldominvsum=sum(totaldomval_sum),
           totaldominvsum_lag=sum(totaldomval_sum_lag)) %>%
    group_by(State,newdummn) %>% 
    mutate(groupsum= sum(totalval_sum),
           grouppop = sum(POPESTIMATE2008),
           groupsum_lag=sum(totalinvsum_lag)) %>%
    dplyr::select(State,newdummn,totalinvsum,totalinvsum_lag,totaldominvsum,totaldominvsum_lag, 
                  groupsum,groupsum_lag,statepop,grouppop)
  
  totalscount3 <- unique(totalscount3)
  
  
  # long to wide 
  
  totalcount_long <- totalscount3 %>%
    pivot_wider(names_from=newdummn,values_from=c("groupsum","groupsum_lag",
                                                  "grouppop"),names_prefix="gr")
  
  # Placebo test using 2006 investments 
  
  totals06 <- fulldatam 
  
  totals06$date <- as.Date(with(totals06, paste(Year,Month,Day, sep="-")))
  
  totals06full <- fulldatafips %>%
    filter(Industry.Activity=="Manufacturing") 
  # %>% filter(!Industry.Sector=="Coal, oil & gas")
  
  totals06full$date <- as.Date(with(totals06full, paste(Year,Month,Day, sep="-")))
  
  if(outlier==TRUE){
    fdimean <- mean(totals06$value_2010)
    fdi2sd <- 2*sd(totals06$value_2010)
    fdi2sdm <- fdimean+fdi2sd
    
    totals06 <- totals06 %>% 
      filter(value_2010 <fdi2sdm)
  } else{
    
    totals06 <- totals06
  }
  
  totals061 <- totals06 %>% filter(date <"2009-01-01"&date>"2005-12-31")
  
  # calculate county values 
  totals06count <- totals061 %>% 
    add_count(FIPS) %>% 
    rename(totalcount06="n") %>% 
    group_by(FIPS) %>% 
    mutate(totalval_sum06 = sum(value_2010)) %>% 
    dplyr::select(State,FIPS,totalcount06, totalval_sum06)
  
  totals06count <- unique(totals06count)
  
  # Estimate which counties are new and old for 2006 period - 
  
  totals062no <- totals06 %>% filter(date<"2005-12-31")
  
  totals06count_lagno <- totals062no %>% 
    add_count(FIPS) %>% 
    rename(totalcount06_lag="n") %>% 
    group_by(FIPS) %>% 
    mutate(totalval06_sum_lag=sum(value_2010)) %>% 
    dplyr::select(State,FIPS,totalcount06_lag,totalval06_sum_lag) %>% 
    arrange(FIPS)
  
  totals06count_lagno <- unique(totals06count_lagno)
  
  # create newdummn for 2006
  
  totals06count <- totals06count%>% 
    mutate(newdummn=ifelse(FIPS %in% totals06count_lagno$FIPS,1,2)) 
  
  # add lagged investment 
  
  totals06count_lag <- totals06full %>% 
    filter(date<"2005-12-31") %>% 
    add_count(FIPS) %>% 
    rename(total06count_lag="n") %>% 
    group_by(FIPS) %>% 
    mutate(total06val_sum_lag = sum(value_2010)) %>%
    dplyr::select(State,FIPS,total06count_lag, total06val_sum_lag) %>% 
    arrange(FIPS)
  
  totals06count_lag <- unique(totals06count_lag)
  
  totals06countfin <- full_join(totals06count,totals06count_lag)
  
  vars <- c("totalcount06","totalval_sum06","total06count_lag","total06val_sum_lag","newdummn")
  
  totals06countfin[vars][is.na(totals06countfin[vars])] <- 0
  
  # Add controls - county pop 05
  
  countypop05 <- read.csv("data/countypop05.csv") %>% 
    dplyr::select(STATE,COUNTY,POPESTIMATE2005) %>% 
    filter(COUNTY!=0) %>% 
    mutate(county=sprintf("%03d",COUNTY)) %>% 
    mutate(FIPS=as.numeric(paste0(STATE,county,sep=""))) %>% 
    dplyr::select(FIPS,POPESTIMATE2005)
  
  totals06countfin <- left_join(totals06countfin,countypop05)
  
  totals06count3 <- totals06countfin %>% 
    group_by(State) %>% 
    mutate(totalinvsum06= sum(totalval_sum06),
           statepop06 = sum(POPESTIMATE2005),
           totalinvsum06_lag=sum(total06val_sum_lag)) %>%
    group_by(State,newdummn) %>% 
    mutate(groupsum06= sum(totalval_sum06),
           grouppop06 = sum(POPESTIMATE2005),
           groupsum06_lag=sum(totalinvsum06_lag)
    ) %>%
    dplyr::select(State,newdummn,totalinvsum06,totalinvsum06_lag,statepop06,groupsum06,
                  grouppop06,groupsum06_lag)
  
  totals06count3 <- unique(totals06count3)
  
  # long to wide 
  totalcount06_long <- totals06count3 %>%
    pivot_wider(names_from=newdummn,
                values_from=c("groupsum06","groupsum06_lag",
                              "grouppop06"),names_prefix="gr")
  
  msdata <- logit9q%>% 
    dplyr::select(State,FIPS) %>% 
    mutate(dum09=ifelse(FIPS %in% totalscount$FIPS,1,0),
           dum06=ifelse(FIPS %in% totals06count$FIPS,1,0)) %>% 
    group_by(State) %>% 
    mutate(nocounty=n(),
           notreat09=sum(dum09),
           notreat06=sum(dum06))  %>%
    dplyr::select(State,nocounty,notreat06,notreat09)
  
  msdata <- unique(msdata)
  
  # add row for DC and AK in the data
  msdata <- msdata %>% 
    filter(!is.na(State)) %>% 
    ungroup() %>% 
    add_row(State="AK",
            nocounty=0,
            notreat06=0,
            notreat09=0) %>% 
    add_row(State="DC",
            nocounty=0,
            notreat06=0,
            notreat09=0)
  
  msdata2 <- left_join(msdata,totalcount_long) %>% 
    left_join(.,totalcount06_long)
  
  msdata2[is.na(msdata2)] <- 0
  msdata2 <- msdata2 %>% 
    arrange(desc(notreat09))
  
  # Import fmap outlays data from Chodorow-Reich et al. 
  fmap <- read.dta13("data/fmap.dta")
  
  #merge 
  
  msdatam <- msdata2 %>% 
    rename(state_abrev=State)
  
  fmap2 <- left_join(fmap,msdatam)
  
  # Add non-federal (state+local) incentives data - Good Jobs Subsidy Tracker
  
  load("data/state.incent.Rdata")
  
  ## Given incentives between 2009 and 2011
  
  incstart= 2008
  incend=2012 
  inc06start=2005
  inc06end=2009
  
  incen <- state.all %>% 
    filter(year>incstart&year<incend) %>% 
    dplyr::select(-year) %>% 
    group_by(state) %>% 
    summarise_all(sum,na.rm=TRUE) %>% 
    rename(state_abrev=state)
  
  incen_lag <- state.all %>% 
    filter(year<as.numeric(incstart+1)) %>% 
    dplyr::select(-year) %>% 
    group_by(state) %>% 
    summarise_all(sum,na.rm=TRUE) %>% 
    rename(state_abrev=state,
           tot.incent_lag=tot.incent,
           tax.incent_lag=tax.incent,
           real.incent_lag=real.incent)
  
  incen06 <- state.all %>% 
    filter(year>inc06start&year<inc06end) %>% 
    dplyr::select(-year) %>% 
    group_by(state) %>% 
    summarise_all(sum,na.rm=TRUE) %>% 
    rename(state_abrev=state) %>% 
    rename_at(vars(-state_abrev),function(x) paste0(x,"_06"))
  
  incen06_lag <- state.all %>% 
    filter(year<as.numeric(inc06start+1)) %>% 
    dplyr::select(-year) %>% 
    group_by(state) %>% 
    summarise_all(sum,na.rm=TRUE) %>% 
    rename(state_abrev=state) %>% 
    rename_at(vars(-state_abrev),function(x) paste0(x,"_06_lag"))
  
  fmap3 <- left_join(fmap2,incen) %>% 
    left_join(.,incen_lag) %>%
    left_join(.,incen06) %>% 
    left_join(.,incen06_lag)
  
  vars <- c("tot.incent","tax.incent","real.incent","tot.incent_lag","tax.incent_lag","real.incent_lag",
            "tot.incent_06","tax.incent_06","real.incent_06","tot.incent_06_lag","tax.incent_06_lag","real.incent_06_lag")
  
  fmap3[vars][is.na(fmap3[vars])] <- 0
  
  ## Add only state-sponsored incentives - Good Jobs Subsidy Tracker
  load("data/state.only.Rdata")
  
  # 2009-2011
  incen2 <- state.all %>% 
    filter(year>incstart&year<incend) %>% 
    dplyr::select(-year) %>% 
    group_by(state) %>% 
    summarise_all(sum,na.rm=TRUE) %>% 
    rename(state_abrev=state)
  
  incen2_lag <- state.all %>% 
    filter(year<as.numeric(incstart)+1) %>% 
    dplyr::select(-year) %>% 
    group_by(state) %>% 
    summarise_all(sum,na.rm=TRUE) %>% 
    rename(state_abrev=state,
           tot.statelev_lag=tot.statelev,
           tax.statelev_lag=tax.statelev,
           real.statelev_lag=real.statelev,
           cash.statelev_lag=cash.statelev,
           land.statelev_lag=land.statelev)
  
  # 2006-2008
  
  incen062 <- state.all %>% 
    filter(year>inc06start&year<inc06end) %>% 
    dplyr::select(-year) %>% 
    group_by(state) %>% 
    summarise_all(sum,na.rm=TRUE) %>% 
    rename(state_abrev=state) %>% 
    rename_at(vars(-state_abrev),function(x) paste0(x,"_06"))
  
  incen06_lag2 <- state.all %>% 
    filter(year<as.numeric(inc06start+1)) %>% 
    dplyr::select(-year) %>% 
    group_by(state) %>% 
    summarise_all(sum,na.rm=TRUE) %>% 
    rename(state_abrev=state) %>% 
    rename_at(vars(-state_abrev),function(x) paste0(x,"_06_lag"))
  
  
  fmap4 <- left_join(fmap3,incen2) %>% 
    left_join(.,incen2_lag) %>%
    left_join(.,incen062) %>% 
    left_join(.,incen06_lag2)
  
  vars <- c("tot.statelev","tax.statelev","real.statelev","tot.statelev_lag","tax.statelev_lag","real.statelev_lag",
            "cash.statelev","cash.statelev_lag","land.statelev","land.statelev_lag",
            "tot.statelev_06","tax.statelev_06","real.statelev_06","tot.statelev_06_lag","tax.statelev_06_lag",
            "real.statelev_06_lag","cash.statelev_06","cash.statelev_06_lag","land.statelev_06","land.statelev_06_lag")
  
  fmap4[vars][is.na(fmap4[vars])] <- 0
  
  fmap4
}

### LOAD AND CLEAN DATA 

## FDI 

load("data/fdi0319.Rdata")
fulldata <- fdi

# Clean up variables 
fulldata <- fulldata %>% 
  separate(`Project Date`,c("Year","Month","Day"),sep="-")
names(fulldata) <- c("Year","Month","Day","Investing.Company","Parent.Company","Source.Country","Source.State","Source.City","Destination.Country","Destination.State","Admin.Region","Destination.City","Industry.Sector","Sub.Sector","Cluster","Industry.Activity","Capital.Investment","Estimated","Jobs.Created","Estimated.1","Project.Type")

# Inflation adjust all FDI values to 2010 $ 
monthly_cpi <-
  read.csv("data/CPIAUCSL.csv", header = TRUE)

monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% dplyr::summarize(cpi = mean(CPIAUCSL))
yearly_cpi$adj_factor10 <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2010]

yearly_cpi <- yearly_cpi %>% 
  rename("Year"=cpi_year) %>% 
  dplyr::select(Year,adj_factor10)

yearly_cpi$Year <- as.factor(yearly_cpi$Year)

fulldata <- left_join(fulldata,yearly_cpi)

fulldata <- fulldata %>% 
  mutate(value_2010 = Capital.Investment*adj_factor10)

# Filter US companies with known state and county information 

fulldata <- fulldata %>% 
  filter(Destination.Country=="United States") %>% 
  filter(Destination.State!="Not Specified" & Admin.Region!="Not Specified") %>%
  filter(Destination.State!="Northern Mariana Islands" & Destination.State!="Not Specified") %>%
  rename(State="Destination.State")

# Join County names with FIPS codes
fulldata2 <- fulldata %>% 
  separate(Admin.Region, c("County", "nm"), "[()]")

fulldata2 <- correct_adminregion(fulldata2)

statecross <- read.csv("data/stateabb.csv",sep=";")

statecross <- statecross %>% 
  rename(stateab="State",
         State="state")

statecross <- statecross %>% 
  mutate(State=as.character(State)) %>% 
  mutate(State=ifelse(State=="District of Columbia","Washington, DC",State))

fulldata2$County <- as.character(fulldata2$County)

fulldata2 <- left_join(fulldata2, statecross)

fulldata2 <- fulldata2 %>% 
  dplyr::select(-State) %>% 
  rename(State=stateab) %>% 
  mutate(County=trimws(County),
         State=trimws(State))

# Import full county list in 2010
county <- read.csv("data/finalRvote.csv")
county$County <- gsub("[.]","",county$County)

countym <- county %>% 
  filter(Year==2010) %>% 
  dplyr::select(County,State,FIPS)

fulldatafips <- left_join(fulldata2,countym)

fulldatafips <- unique(fulldatafips)

# Fix County FIPS codes that did not merge
fulldatafips <- fulldatafips %>% 
  mutate(FIPS=ifelse(County=="Aleutians West Borough",2016,
                     ifelse(County=="Anchorage Borough",2020,
                            ifelse(County=="Fairbanks North Star Borough",2090,
                                   ifelse(County=="North Slope Borough",2185,
                                          ifelse(County=="Southeast Fairbanks Borough",2240,
                                                 ifelse(County=="Orleans Parish"&State=="NY",36073,
                                                        ifelse(County=="Orleans Parish"&State=="VT",50019,
                                                               ifelse(County=="Washington, DC",11001,FIPS)))))))))

# Save US FDI files with FIPS codes
save(fulldatafips,file="data/usdatawfips.Rdata")

## DOMESTIC INVESTMENT 

# NOTE: Domestic investment data lacks county information but cities are specified - I use both to figure out FIPS codes 

dominv <- read.dta13("data/interstate_2007_2019.dta")

# Join County names with FIPS codes
dominv <- dominv %>% 
  rename(Admin.Region=AdminRegion,
         State=DestinationState)

dominv2 <- dominv %>% 
  separate(Admin.Region, c("County", "nm"), "[()]")

dominv2 <- correct_adminregion(dominv2)
dominv2$County <- as.character(dominv2$County)
dominv2 <- left_join(dominv2, statecross)

dominv2 <- dominv2 %>% 
  dplyr::select(-State) %>% 
  rename(State=stateab) %>% 
  mutate(County=trimws(County),
         State=trimws(State))

countym <- unique(countym)

domfips <- left_join(dominv2,countym)

# Supplement FIPS codes with city information 
tocomp <- dominv %>% 
  rename(AdminRegion=Admin.Region) %>% 
  filter(DestinationCity!="Not Specified" & AdminRegion=="Not Specified") %>% 
  separate(DestinationCity,c("city","state"),sep="[()]") %>% 
  filter(State!="Puerto Rico"&State!="Guam") %>% 
  mutate(state=ifelse(State=="Texas","TX",
                      ifelse(State=="Utah","UT",state))) %>% 
  dplyr::select(state,city) %>% 
  mutate(city=trimws(city),
         state=trimws(state)) %>% 
  mutate(state=ifelse(city=="Shepherdstown"|city=="Wheeling","WV",state))

citycty <- read.csv("data/uscities.csv")

citycty <- citycty %>% 
  dplyr::select(city,state_id,county_fips) %>% 
  rename(state=state_id,
         FIPS=county_fips) %>% 
  mutate(city=trimws(city),
         state=trimws(state))

tocomp2 <- left_join(tocomp,citycty,by=c("city","state"))
tocomp2 <- tocomp2 %>% 
  mutate(citystate=paste0(city,state)) %>% 
  rename(FIPS2=FIPS)

domfips2 <- domfips %>%
  separate(DestinationCity,c("city","nnm"),sep="[()]") %>% 
  mutate(city=trimws(city)) %>%
  dplyr::select(-nm,-nnm) %>% 
  mutate(citystate=paste0(city,State))

tocomp2 <- unique(tocomp2)
domfips3 <- left_join(domfips2,tocomp2)

# Manually fix DC FIPS and other FIPS that have weird county names
domfips3 <- domfips3 %>% 
  mutate(FIPS=ifelse(citystate=="RedwoodCA"&County=="Gibson County",6081,
                     ifelse(citystate=="ShelbyNC"&County=="Gibson County",37045,
                            ifelse(citystate=="PeoriaAZ"&County=="Marengo County",4013,
                                   ifelse(citystate=="RadfordVA"&County=="Gibson County",51750,
                                          ifelse(citystate=="Ann ArborMI"&County=="Abbeville County",26161,
                                                 ifelse(citystate=="PearlandTX"&County=="Braxton County",48039,
                                                        ifelse(citystate=="TullahomaTN"&County=="Tullahoma County",47031,
                                                               ifelse(citystate=="KetchikanAK"&County=="Ketchikan Gateway Borough",2130,
                                                                      ifelse(citystate=="KodiakAK"&County=="Kodiak IslandBorough",2150,
                                                                             ifelse(citystate=="FairbanksAK"&County=="Fairbanks North Star Borough",2090,
                                                                                    ifelse(citystate=="Klamath FallsOR"&County=="Kalmath County",41035,
                                                                                           ifelse(citystate=="MaysvilleKY"&County=="Abbeville County",21161,
                                                                                                  ifelse(citystate=="JuneauAK"&County=="City and Borough of Juneau County",2110,
                                                                                                         ifelse(citystate=="AtlantaGA"&County=="Abbeville County",13121,
                                                                                                                ifelse(citystate=="HoonahAK"&County=="Skagway Hoonah Angoon Borough",2105,
                                                                                                                       ifelse(citystate=="Denali ParkAK"&County=="Denali Borough",2068,
                                                                                                                              ifelse(citystate=="NewportVT"&County=="Orleans Parish",50019,
                                                                                                                                     ifelse(citystate=="SidneyOH"&County=="Sidney County",39149,
                                                                                                                                            ifelse(State=="DC",11001,FIPS))))))))))))))))))))

domfips3 <- domfips3 %>% 
  mutate(FIPS=ifelse(is.na(FIPS),FIPS2,FIPS)) %>% 
  dplyr::select(-FIPS2) %>%
  filter(!is.na(FIPS)) %>%
  dplyr::select(-citystate,-state)

# Inflation adjust investment value to 2010 $
domfips3 <- domfips3 %>%
  separate(ProjectDate,c("Year","Month","Day"),sep="-") %>%
  left_join(yearly_cpi) %>% 
  mutate(value_2010 = CapitalInvestment*adj_factor10) %>% 
  filter(Year!=2003)

# Save US investment files with FIPS codes
save(domfips3,file="data/usdomdatawfips.Rdata")

### CREATE SAMPLE FOR FOREIGN AND DOMESTIC INVESTMENT 

# Foreign Investment - Filter only new and manufacturing projects
fulldatam <- fulldatafips %>% 
  filter(Industry.Activity=="Manufacturing" & Project.Type=="New") 
  # %>% filter(Industry.Sector!="Coal, oil & gas")

# Domestic Investment - Filter only new and manufacturing projects
domm <- domfips3 %>% 
  filter(IndustryActivity=="Manufacturing" & ProjectType=="New") 
  # %>%  filter(IndustrySector!="Coal, oil & gas")

# Filter out solar energy companies that might have come into the US because of a federal grant 
if(excludesolar==TRUE){
  fulldatam <- fulldatam %>% 
    filter(Industry.Sector!="Renewable energy")
  
  domm <- domm %>% 
    filter(IndustrySector!="Renewable energy")
} else{
  fulldatam <- fulldatam
  
  domm <- domm
}

# Filter out tax havens that might have other motives
# Potential tax havens: Netherlands, Switzerland, Luxembourg, Cayman Islands, British Virgin Islands, Singapore, Panama, Mauiritius, Cyprus, Ireland
if(excludetaxhaven==TRUE){
  fulldatam <- fulldatam %>% 
    filter(!Source.Country=="Netherlands"&!Source.Country=="Switzerland"&
             !Source.Country=="Luxembourg"&!Source.Country =="Cayman Islands"&
             !Source.Country=="Singapore"&!Source.Country=="Panama"&!Source.Country=="Mauritius"&
             !Source.Country=="Cyprus"&!Source.Country=="Ireland")
} else{
  fulldatam <- fulldatam
}

# Filter out FDI after the election of Donald Trump in 2016
fulldatam$date <- as.Date(with(fulldatam, paste(Year,Month,Day, sep="-")))
fulldatam <- fulldatam %>% filter(date <"2016-11-01") %>% 
  filter(!is.na(FIPS))
fulldatam$Year <- as.numeric(fulldatam$Year)

# Filter out domestic investment after the election of Donald Trump in 2016
domm$date <- as.Date(with(domm, paste(Year,Month,Day, sep="-")))
domm <- domm %>% filter(date <"2016-11-01") %>% 
  filter(!is.na(FIPS)) 
domm$Year <- as.numeric(domm$Year)

# Save US FDI file for data exploration 
save(fulldatam,file="data/usfdisample.Rdata")
# Save US investment file for data exploration
save(domm,file="data/usinvsample.Rdata")

### COUNTY LEVEL REGRESSION - DATA MERGING
### LIMIT SAMPLE TO POST-GR FDI 

# Create Post-Recession FDI 2009-2011 sample
uplimit = "2012-01-01"
lowlimit = "2008-12-31"

totalq <- fulldatam 
totalqleft <- fulldatam
totalq$date <- as.Date(with(totalq, paste(Year,Month,Day, sep="-")))
totalqleft$date <- as.Date(with(totalqleft, paste(Year,Month,Day, sep="-")))
totalq <- totalq %>% filter(date <uplimit&date>lowlimit)


totalqleft <- totalqleft %>% filter(date <lowlimit&date>"2008-12-31")
totalq$Year <- as.numeric(totalq$Year)
totalq <- totalq %>% dplyr::select(-date)

save(totalq, file="data/recessionfdi.Rdata")

## Aggregate at the county level
totalqcount <- totalq %>% 
  add_count(FIPS) %>% 
  rename(totaldum="n") %>% 
  group_by(FIPS) %>% 
  mutate(totalval_sum = sum(value_2010)) %>% 
  dplyr::select(FIPS,totaldum, totalval_sum)

totalqcount <- unique(totalqcount)

totalqcount$FIPS <- as.character(totalqcount$FIPS)

totalqcount <- totalqcount %>% 
  filter(!is.na(FIPS)) %>% 
  mutate(FIPS=as.numeric(FIPS))

# Merge other county characteristics

# Republican vote share
c09 <- county %>% 
  filter(Year==2008) %>% 
  dplyr::select(FIPS,RVoteshare) %>% 
  distinct() 

# County unemployment
countyunemp <- read.csv("data/countyunemp08.csv") %>% 
  dplyr::select(FIPS,unemp08)

c09 <- left_join(c09,countyunemp)

cregnew09q <- left_join(c09,totalqcount)

cregnew09q$totaldum[is.na(cregnew09q$totaldum)] <- 0
cregnew09q$totalval_sum[is.na(cregnew09q$totalval_sum)] <- 0

# Creating new-old-never county measure
# FIPS that got new GFDI in manu between 2003 and 2008 

c0308 <- fulldatam %>% 
  filter(Year<2009) %>% 
  dplyr::select(FIPS) %>% 
  distinct()

totalqleftfips <- totalqleft %>% 
  dplyr::select(FIPS) %>% 
  distinct()

c03082 <- rbind(c0308,totalqleftfips)

# Code counties that have received FDI 09-11 but not before = 2 - new counties
# Code counties that have received FDI 09-11 and before = 1 - old counties
# Code counties that have not received FDI 09-11 (even if they have received FDI before) = 0 - never counties
cregnew09q <- cregnew09q %>% 
  mutate(newdummn = ifelse((!FIPS %in% c03082$FIPS & totaldum>0),2,
                           ifelse(FIPS %in% c03082$FIPS & totaldum>0,1,0))) %>% 
  mutate(neverdum = ifelse(FIPS %in% c03082$FIPS & totaldum==0,1,0))

# Create list of new, old, never counties 
fipsquarter <- cregnew09q %>% 
  dplyr::select(FIPS,newdummn)

save(fipsquarter,file="data/countycategorizations.Rdata")

newold <- totalq %>% 
  left_join(.,fipsquarter)

save(newold,file="data/newoldcountycharacteristics.Rdata")

### ADDING CONTROLS 

## M&A 

mafiles = list.files("data/mafiles")

madatamerge = data.frame(fips=as.character(),
                         year=as.integer(),
                         macouny=as.integer())

for(i in 1:length(mafiles)){
  fname = mafiles[i]
  fname = paste0("data/mafiles/","",fname)
  madata <- read.csv(fname)
  madatamerge <- rbind(madatamerge,madata)
}


# Merge 2008 M&A deals with base df
madatamerge <- madatamerge %>% 
  filter(year==2008) %>% 
  rename(FIPS=fips) %>% 
  dplyr::select(-year)

cregnew09q <- left_join(cregnew09q,madatamerge)

cregnew09q$macount[is.na(cregnew09q$macount)] <- 0 

## Patents

patents <- read.csv("data/patentfile.csv") %>% 
  dplyr::select(FIPS.Code,starts_with("X")) %>% 
  rename(FIPS=FIPS.Code) %>% 
  mutate(FIPS=as.numeric(as.character(FIPS)))

patents_long <- patents %>% 
  gather(Year, patentcount, -c(FIPS))

patents_long$Year <- gsub("X","",patents_long$Year)

patents_long$Year <- as.numeric(patents_long$Year)

# Merge 2008 patents with base df
patent08 <- patents_long %>% 
  filter(Year==2008) %>% 
  dplyr::select(-Year) 

cregnew09q <- left_join(cregnew09q,patent08)

cregnew09q$patentcount[is.na(cregnew09q$patentcount)] <- 0 

## Domestic Investment 

domes08 <- domm %>% 
  group_by(Year,FIPS) %>% 
  summarise(totaldomval = sum(value_2010)) %>% 
  ungroup() %>% 
  mutate(Year=as.numeric(Year),
         FIPS=as.character(FIPS)) %>% 
  filter(Year==2008) %>% 
  dplyr::select(-Year) %>% 
  mutate(FIPS=as.numeric(FIPS))

# Merge 2008 domestic investment with base df
cregnew09q <- left_join(cregnew09q,domes08)
cregnew09q$totaldomval[is.na(cregnew09q$totaldomval)] <- 0

## Geographic identifiers 

# Merge states with counties 
countystate <- read.csv("data/allcountylist.csv") %>% 
  dplyr::select(FIPS,State)

# Fix Miami-Dade county because of code change 
# Fix Colorado 8014 because of code change 
countystate <- countystate %>% 
  mutate(FIPS=ifelse(FIPS==12025,12086,FIPS))

cregnew09q <- left_join(cregnew09q,countystate)

cregnew09q <- cregnew09q %>% 
  mutate(State=as.character(State)) %>% 
  mutate(State=ifelse(FIPS==8014,"CO",State))

# Add state capital counties to filter them out 

capitalzip <- read.csv("data/capitalzip.csv") %>% 
  rename(zipcode = capitalzip) %>% 
  mutate(capital=1) %>% 
  dplyr::select(zipcode,capital)

countyzip <- read.csv("data/zipcounty.csv") %>% 
  rename(FIPS=fips)

capitalzip <- left_join(capitalzip,countyzip)

cregnew09q <- left_join(cregnew09q,capitalzip)

### PREPARE DF FOR MNL REGRESSIONS 

# Congressional District 
elec08 <- read.csv("data/cdcontrol08.csv")

# County to CD crosswalk 
ctycd <- read.csv("data/ctycd0304.csv",header=T)

# County neighbor borders 
border <- read.csv("data/countyadj.csv")

# Create MNL data frame 

logit9q <- prep_mnl(cregnew09q,2009)

### ADDITIONAL CONTROLS 

## Add GDP

gdpmsa <- read.csv("data/msagdp.csv",stringsAsFactors = FALSE)

# Import crosswalk from metropolitan area - county

cbsa <- read.csv("data/cbsa2fipsxw.csv",stringsAsFactors = FALSE) 

cbsa <- cbsa %>% 
  filter(!is.na(cbsacode)) %>% 
  dplyr::select(cbsacode,cbsatitle,statename,fipsstatecode,fipscountycode) %>% 
  rename(GeoFIPS=cbsacode) %>% 
  mutate(GeoFIPS = as.character(GeoFIPS))

cbsa$GeoFIPS <- trimws(cbsa$GeoFIPS)

gdpmsa$GeoFIPS <- trimws(gdpmsa$GeoFIPS)

gdpbase <- full_join(gdpmsa,cbsa)

gdpbase2 <- gdpbase %>% 
  filter(!is.na(X2007)&GeoFIPS!="00998") %>% 
  dplyr::select(X2006,X2007,X2008,fipsstatecode,fipscountycode) %>% 
  mutate(fipscountycode=sprintf("%03d",fipscountycode),
         FIPS=as.numeric(paste0(fipsstatecode,fipscountycode))) %>% 
  dplyr::select(starts_with("X"),FIPS)

names(gdpbase2) <- c("gdp06","gdp07","gdp08","FIPS")

logit9qvall <- left_join(logit9q,gdpbase2)

## Add governor ideology 

governors <- read.csv("data/governors.csv",stringsAsFactors = FALSE)

governors <- governors %>% 
  dplyr::select(stateab,governor) %>% 
  rename(State=stateab)

logit9qvall <- left_join(logit9qvall,governors)

logit9qvall <- logit9qvall %>% 
  mutate(governor=ifelse(State=="HI","R",governor)) %>% 
  mutate(ideamatch=ifelse(governor=="R"&RVoteshare>0.50,1,
                          ifelse(governor=="D"&RVoteshare<0.50,1,0)))


## Add stimulus - Reading in Rdata files created by data_cleaning_stimulus.R

logit9qsub <- logit9qvall

# Education subprime stimulus that flow from states to sub-state entities 
logit9qsub <-  prep_analysis("educationsubprime","substimeduc")

# Education subprime stimulus that flow to community colleges - workforce training 
logit9qsub <-  prep_analysis("educsubprimecommunitycollege","substimeduccom")

# Department of Transportation prime stimulus - federal to states 
load("data/stimulus_files/dotprime.Rdata")

datavalm <- dataval %>% 
  dplyr::select(fips,stimval) %>% 
  ungroup() %>%
  rename(FIPS=fips) %>% 
  rename(primestimdot=stimval)

logit9qsub <- left_join(logit9qsub,datavalm)

# Department of Transportation prime highway stimulus 
load("data/stimulus_files/highwayprime.Rdata")

datavalm <- dataval %>% 
  dplyr::select(fips,stimval) %>% 
  ungroup() %>%
  rename(FIPS=fips) %>% 
  rename(primestimhighway=stimval)

logit9qsub <- left_join(logit9qsub,datavalm)

# Department of Energy prime stimulus

load("data/stimulus_files/doeprime.Rdata")

datavalm <- dataval %>% 
  dplyr::select(fips,stimval) %>% 
  ungroup() %>%
  rename(FIPS=fips) %>%
  rename(primestimdoe=stimval)

logit9qsub <- left_join(logit9qsub,datavalm)

# Infrastructure prime stimulus 

load("data/stimulus_files/infrastructure.Rdata")

datavalm <- dataval %>% 
  dplyr::select(fips,stimval) %>% 
  ungroup() %>%
  rename(FIPS=fips) %>%
  rename(primestiminfra=stimval)

logit9qsub <- left_join(logit9qsub,datavalm)

# Replace missing values with 0 in stimulus variables, covert to in millions, log+1

logit9qsub <- logit9qsub %>% 
  mutate(across(contains(c("substim","primestim")), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(across(contains(c("substim","primestim")), ~./1000000)) %>% 
  mutate(across(contains(c("substim","primestim")), list(log=~logplus(.))))

## Add county labor participation 

filename = paste0("data/laucnty8.txt")

unemp08 <- read.fwf(
  skip=6,
  file=filename,
  widths=c(17, 7, 7, 47, 12,11,14,9, 8), stringsAsFactors=FALSE)

unemp08 <- head(unemp08,-3)

unemp08 <- unemp08 %>%
  dplyr::select(V2,V3,V5,V6,V7) %>% 
  rename(statefips=V2,
         countyfips=V3,
         Year=V5,
         labforce=V6,
         employed=V7)

ctycross <- read.csv("data/allcountylist_stfips.csv") %>% 
  dplyr::select(statefips,countyfips,FIPS)

unemp08$statefips <- as.numeric(unemp08$statefips)
unemp08$countyfips <- as.numeric(unemp08$countyfips)

unemp08 <- left_join(unemp08,ctycross) %>% 
  filter(!is.na(FIPS)) %>% 
  dplyr::select(FIPS,Year,labforce,employed)

unemp08 <- as.data.frame(lapply(unemp08, trimws),stringsAsFactors = FALSE)

unemp08$Year <- as.numeric(unemp08$Year)
unemp08$FIPS <- as.numeric(unemp08$FIPS)
unemp08$employed <- as.numeric(gsub("," ,"", unemp08$employed))
unemp08$labforce <- as.numeric(gsub("," ,"", unemp08$labforce))

logit9qsub2 <- left_join(logit9qsub, unemp08)

logit9qsub2$employed <- as.numeric(logit9qsub2$employed)
logit9qsub2$labforce <- as.numeric(logit9qsub2$labforce)


# scale education stimulus by county population 

logit9qsub2 <- logit9qsub2 %>% 
  mutate_at(vars(setdiff(starts_with("substim"),ends_with("log"))), list(sc = ~./labforce)) %>% 
  mutate_at(vars(ends_with("sc")), list(log= ~logplus(.)))

## Add governor vote variables and term limits

# in 2009, which state belonged to which party's governor
govlist <- read.csv("data/govlist09.csv")

demgovrs <- govlist %>% 
  filter(govparty=="D") %>% 
  dplyr::select(stab)

repgovrs <- govlist %>% 
  filter(govparty=="R") %>% 
  dplyr::select(stab) %>% 
  add_row(stab="HI")

# Import governor vote share files 2005-2008 Dave Leip
gov05 <- read.csv("data/govelec05.csv",skip=1)
gov06 <- read.csv("data/govelec06.csv",skip=1)
gov07 <- read.csv("data/govelec07.csv",skip=1)
gov08 <- read.csv("data/govelec08.csv",skip=1)

# fix 2006 governor vote file
gov062 <- gov06 %>% 
  mutate(fips=as.character(sprintf("%05d",fips))) %>% 
  mutate(statefips = as.numeric(substr(fips,1,2))) %>% 
  filter(statefips!=33&statefips!=50) %>% 
  dplyr::select(-statefips) %>% 
  mutate(fips=as.numeric(fips))

goveleclist <- list(gov05,gov062,gov07,gov08)

# Calculate democrat and republican governor shares
goveleclist2 <- lapply(goveleclist, function(x) x %>% rename(demvote=vote1, repvote=vote2) %>%  mutate(govrepshare = repvote/totalvote, govdemshare=demvote/totalvote) %>% dplyr::select(fips,govrepshare,govdemshare))

govelect <- bind_rows(goveleclist2)

# Merge FIPS codes with state abbreviations

govelect <- govelect %>% 
  mutate(fips=as.character(sprintf("%05d",fips))) %>% 
  mutate(statefips = as.numeric(substr(fips,1,2)))

stabcross <- read.csv("data/statecross.csv")

stabcross <- stabcross %>% 
  dplyr::select(statefips,stateab)

govelect <- left_join(govelect,stabcross)

# Create competitive counties variables 

govelectm <- govelect %>% 
  mutate(incvote = ifelse(stateab %in% demgovrs$stab,govdemshare,govrepshare),
         diffnabs =govrepshare-govdemshare,
         diff= abs(govrepshare-govdemshare),
         comp=ifelse(diff<0.10,1,0),
         comp2=ifelse(diff<0.05,1,0)) %>% 
  rename(FIPS=fips) %>%
  mutate(FIPS=as.numeric(FIPS)) %>% 
  dplyr::select(FIPS,incvote,diffnabs,comp,comp2,govrepshare)

logit9qsub2 <- left_join(logit9qsub2,govelectm)

logit9qsub2 <- logit9qsub2 %>% 
  mutate(incwin = ifelse(incvote>=0.5,1,0))

# Adding governor term limits
govlist$run[is.na(govlist$run)] <- 0

govlistm <- govlist %>% 
  mutate(elig=ifelse(run==1,1,0)) %>%
  dplyr::select(stab,endterm,elig) %>%
  rename(State=stab)

logit9qsub2 <- left_join(logit9qsub2,govlistm)

## Add domestic investment to be a placebo variable 

domfdilhs <- domm %>% 
  filter(date>lowlimit&date<uplimit) %>% 
  group_by(FIPS) %>% 
  summarise(domtotalinvlhs=sum(value_2010))

logit9qsub3 <- left_join(logit9qsub2,domfdilhs)

logit9qsub3$domtotalinvlhs[is.na(logit9qsub3$domtotalinvlhs)] <- 0

logit9qsub3 <- logit9qsub3 %>% 
  mutate(State=as.factor(State),
         governor=as.factor(governor)) 

## Add county-level special education IV variable 


## Add county iv variables 

# population estimate in 2006/2008
pop <- read.dta13("data/popest0608.dta")

# disability education funds
load("data/ed.disable06.Rdata")

# calculate special education funds for each FIPS
disable <- data %>%
  mutate(FIPS=as.numeric(county_code)) %>% 
  mutate(speced=ifelse(spec_ed_students<0,0,spec_ed_students)) %>% 
  group_by(FIPS) %>% 
  mutate(speced=sum(speced)) %>% 
  dplyr::select(FIPS,speced) %>% 
  distinct() %>% 
  arrange(FIPS)

pop$FIPS <- as.numeric(pop$FIPS)

logit9qsub3 <- logit9qsub3 %>% 
  left_join(.,pop) %>%
  left_join(.,disable) %>% 
  mutate(speced_sc=speced/scagepop06) %>% 
  group_by(FIPS) %>% 
  mutate(fdival1=sum(totalval_sum[newdummn==1]),
         fdival2=sum(totalval_sum[newdummn==2]))

## COUNTY-LEVEL INCENTIVES 

# Import county fips and name data
data(county.fips)
force(county.fips)

data(state.fips)
force(state.fips)

state.abb <- state.fips %>% 
  dplyr::select(abb,polyname) %>% 
  rename(statename=polyname) %>% 
  separate(statename,c("statename"),sep=":") %>%
  distinct(abb,statename)

county.fips2 <- county.fips %>% 
  separate(polyname,c("polyname"),sep=":") %>% 
  distinct()

logitwincen <- logit9qsub3

# Import raw incentives data including only state-sponsored incentives 
load("data/all.stateincent.Rdata")

# 10 states have all county info missing - AK, AR, DC, ID, MA, MN, ND, NH, RI, WY
state2 <- state %>% 
  mutate(county=gsub("^$", NA, trimws(county)))

empty_states <- state2 %>%
  group_by(state) %>%
  filter(all(is.na(county))) %>%
  count(state)

# Fix county names to merge with FIPS codes
state.inc <- state2 %>% 
  filter(!state %in% empty_states$state) %>% 
  filter(!is.na(county)) %>% 
  mutate(county2=tolower(county)) %>% 
  rename(abb=state) %>% 
  left_join(.,state.abb) %>% 
  mutate(statename=ifelse(abb=="HI","hawaii",statename)) %>%
  separate_rows(county2, sep=",") %>%
  separate_rows(county2, sep="/") %>% 
  mutate(county2=gsub(" and others","",county2),
         county2=trimws(county2)) %>%
  mutate(polyname=paste0(statename,",",county2),
         polyname=gsub("\\.","",polyname),
         polyname=gsub("'","",polyname)) %>% 
  left_join(.,county.fips2)

# Manufally fix some counties' FIPS codes
state.inc <- state.inc %>% 
  mutate(fips=ifelse(polyname=="hawaii,kauai",15007,
                     ifelse(polyname=="hawaii,honolulu",15003,
                            ifelse(polyname=="hawaii,maui",15009,
                                   ifelse(polyname=="iowa,pocahantas",19151,
                                          ifelse(polyname=="illinois,lasalle",17099,
                                                 ifelse(polyname=="louisiana,desoto",22031,
                                                        ifelse(polyname=="ohio,lorrain",39093,
                                                               ifelse(polyname=="tennessee,dekalb",47041,
                                                                      ifelse(polyname=="texas,dewitt",48123,
                                                                             ifelse(polyname=="texas,camaron",48061,
                                                                                    ifelse(polyname=="	
texas,hayes",48209,
                                                                                           ifelse(polyname=="texas,wichita falls",48485,
                                                                                                  ifelse(polyname=="florida,dade",12086,
                                                                                                         ifelse(polyname=="florida,desoto",12027,
                                                                                                                ifelse(polyname=="maryland,prince george's",24033,
                                                                                                                       ifelse(polyname=="mississippi,desoto",28033,
                                                                                                                              ifelse(polyname=="mississippi,jeff davis",28065,
                                                                                                                                     ifelse(polyname=="ohio,vanwert",39161,
                                                                                                                                            ifelse(polyname=="virginia,wight",51197,
                                                                                                                                                   ifelse(polyname=="indiana,laporte",18091,
                                                                                                                                                          ifelse(polyname=="indiana,dekalb",18033,
                                                                                                                                                                 # fix VA cities 
                                                                                                                                                                 ifelse(polyname=="virginia,staunton",51790,
                                                                                                                                                                        ifelse(polyname=="virginia,lynchburg",51680,
                                                                                                                                                                               ifelse(polyname=="virginia,nortfolk",51710,
                                                                                                                                                                                      ifelse(polyname=="virginia,loundon",51107,
                                                                                                                                                                                             ifelse(polyname=="virginia,portsmouth",51740,
                                                                                                                                                                                                    ifelse(polyname=="virginia,bristol",51520,
                                                                                                                                                                                                           ifelse(polyname=="virginia,hopewell",51670,
                                                                                                                                                                                                                  ifelse(polyname=="virginia,petersburg",51730,
                                                                                                                                                                                                                         ifelse(polyname=="virginia,galax",51640,
                                                                                                                                                                                                                                ifelse(polyname=="virginia,danville",51590,
                                                                                                                                                                                                                                       ifelse(polyname=="virginia,charlottesville",51540,fips)))))))))))))))))))))))))))))))))


# Limit incentives to our sample period
state.inc <- state.inc %>% 
  filter(!is.na(fips)) %>% 
  filter(sub_year>2008&sub_year<2012) %>%  
  group_by(fips) %>% 
  mutate(all_incentives=sum(subsidy_adjusted),
         real_incentives=sum(subsidy_adjusted[now.pay==1]),
         tax_incentives=sum(subsidy_adjusted[tax==1]),
         cash_incentives=sum(subsidy_adjusted[cash==1]),
         land_incentives=sum(subsidy_adjusted[land==1])) %>% 
  dplyr::select(fips,ends_with("incentives")) %>% 
  distinct() %>%
  rename(FIPS=fips)

# Fill incentives with 0 if missing, convert to in millions, take log+1
logitwincen <- logitwincen %>% 
  left_join(.,state.inc) %>% 
  mutate(across(all_incentives:land_incentives,~ifelse(is.na(.), 0, .))) %>%
  mutate(across(all_incentives:land_incentives,~./1000000)) %>% 
  mutate(across(all_incentives:land_incentives,list(log=~logplus(.))))  

## ADD COUNTY MASS LAYOFF DATA 

# Import mass layoff data from BLS
mls <- read.csv("data/blsmls.csv",skip=3)

# calculate how many jobs lost between 2001-2006
mls_m <- mls %>% 
  dplyr::select(X,X.3,X.5,X.4) %>%
  rename("year"=X,
         "FIPS"=X.3,
         "layoff"=X.5) %>% 
  filter(year>1999&year<2007) %>% 
  group_by(FIPS) %>% 
  summarize(layoff=sum(layoff,na.rm=T)) %>%
  arrange(desc(layoff))

logitwincenlayoff <- logitwincen %>% 
  left_join(.,mls_m) %>% 
  mutate(layoff=ifelse(is.na(layoff),0,layoff),
         layoff=layoff/100000,
         layoff_log=log(layoff+0.1)) 

# Variable unit conversion - $ values, pop counts in millions, % are in 100s,layoff in 100,000

logitwincenlayoff <- logitwincenlayoff %>% 
  mutate(RVoteshare=RVoteshare*100,
         labforce=labforce/1000000)
# Write alternative versions of the data frame for analysis

if(excludetaxhaven==TRUE){ # excluding tax havens
  write.dta(logitwincenlayoff,"analysis_data/countylevelwotax.dta")
} else if(excludesolar==TRUE){
  write.dta(logitwincenlayoff,"analysis_data/countylevelwosolar.dta")
} else{
  write.dta(logitwincenlayoff,"analysis_data/countylevel.dta")
}

## STATE-LEVEL REGRESSION DATA MERGING 

# Create data frame without outliers, sample period: 2009-2011 
fmapjun11wo <- make_ivregdata(TRUE,"2008-12-31","2012-01-01",2008,2012)

# Add state unemployment 2008
stateunemp08 <- read.csv("data/stateunemp2008.csv",stringsAsFactors = FALSE,skip=7)

stateunemp082 <- stateunemp08 %>% 
  filter(X.3=="Total") %>% 
  dplyr::select(X.2,X.10) %>% 
  rename(state=X.2,
         unemprate08=X.10)

fmapjun11wo <- left_join(fmapjun11wo,stateunemp082)

# Add state unemployment 2005 
stateunemp05 <- read.csv("data/stateunemp2005.csv",stringsAsFactors = FALSE, skip=7)

stateunemp052 <- stateunemp05 %>% 
  filter(X.3=="Total") %>% 
  dplyr::select(X.2,X.10) %>% 
  rename(state=X.2,
         unemprate05=X.10) %>%
  mutate(unemprate05=as.numeric(unemprate05))

fmapjun11wo <- left_join(fmapjun11wo,stateunemp052)

# Add other 2005 controls 

## Union share 

union05 <- read.csv("data/unionrate05.csv",stringsAsFactors = FALSE)

fmapjun11wo <- left_join(fmapjun11wo,union05) 

write.csv(fmapjun11wo,"data/deneme.csv")

## % Manu Employment 

manu05 <- read.csv("data/manu05.csv",skip=3,
                   stringsAsFactors = FALSE)

manu05_m <- manu05 %>% 
  rename(state=GeoName,
         manuemp_05 = X2005) %>% 
  dplyr::select(state,manuemp_05)

fmapjun11wo <- left_join(fmapjun11wo,manu05_m) 

## GDP 

gdp05 <- read.csv("data/gdp05.csv", stringsAsFactors = FALSE)

gdp05_m <- gdp05 %>% 
  mutate(state=trimws(state),
         gdp_05=as.numeric(gsub(",","",gdp_05)))

fmapjun11wo <- left_join(fmapjun11wo,gdp05_m) 
# Investment variables are already in millions, convert incentives to millions + gdp to millions 
fmapjun11wo <- fmapjun11wo %>% 
  # create % manu emp + gdp per capita 
  mutate(per_empl_manu_05 = (manuemp_05/labforce_05)*100,
         gdp_pcmil_05 = (gdp_05*1000000/labforce_05*1000)/1000000) %>% 
  mutate_at(vars(fmap,popestimate2008,instrument,ends_with("incent"),ends_with("incent_lag"),ends_with("statelev"),ends_with("statelev_lag"),ends_with("incent_06"),ends_with("incent_06_lag"),ends_with("statelev_06"),ends_with("statelev_06_lag"),tot.incent_lag), list(mil = ~./1000000)) %>% 
  mutate(gdp_pcmil=gdp_pc*1000000,
         labforce_05_mil=labforce_05/1000) 


names(fmapjun11wo)[6] <- "region_dummies"

write.dta(fmapjun11wo,"analysis_data/statelevel11.dta")

# Create data frame without outliers, sample period: 2000-2010 - Appendix

fmapjun10wo <- make_ivregdata(TRUE,"2008-12-31","2011-01-01",2008,2011)

fmapjun10wo <- left_join(fmapjun10wo,stateunemp082)

fmapjun10wo <- fmapjun10wo %>% 
  mutate_at(vars(fmap,popestimate2008,instrument,ends_with("incent"),ends_with("statelev"),tot.incent_lag), list(mil = ~./1000000)) %>% 
  mutate(gdp_pcmil=gdp_pc*1000000) 

names(fmapjun10wo)[6] <- "region_dummies"

write.dta(fmapjun10wo,"analysis_data/statelevel10.dta")


## TESTING QUID-PRO-QUO WITH TRADE VOTING
#Looking at voting that happened before 2009 to see if 2009+2010 investments were as a result of them

# Import Trade voting 
tradep10 <- read.csv("data/110thforanalysis.csv")
tradep09 <- read.csv("data/109thforanalysis.csv")

tradep <- rbind(tradep10,tradep09)

tradepcd <- tradep %>% 
  dplyr::select(Votebarrier,Barriertotal,Votesubsidy,Subsidytotal,State,cdid) %>% 
  group_by(State,cdid) %>% 
  summarise(bvote = sum(Votebarrier, na.rm=TRUE),
            btotal = sum(Barriertotal, na.rm=TRUE),
            svote = sum(Votesubsidy, na.rm=TRUE),
            stotal = sum(Subsidytotal, na.rm= TRUE))


tradepcd$cdid <- gsub("At Large",0,tradepcd$cdid)

tradepcd$cdid <- as.numeric(tradepcd$cdid)

tradepcd2 <- tradepcd %>% 
  mutate(total=btotal+stotal,
         vote= bvote+svote, 
         protrade=vote/total,
         protradeb=bvote/btotal,
         protrades=svote/stotal) %>% 
  dplyr::select(State,cdid,protrade,protradeb,protrades) %>%
  rename("state"=State)

stateab <- read.csv2("data/stateabb.csv") 

tradepcd2 <- left_join(tradepcd2,stateab)

tradepcd2 <- tradepcd2 %>% 
  mutate("cdids"=paste0(State,cdid,sep="")) %>% 
  ungroup() %>% 
  dplyr::select(cdids,starts_with("protrade"))

tradepcd2 <- tradepcd2 %>% 
  mutate(tradeop = 1 - protrade,
         tradeopb = 1 - protradeb,
         tradeops = 1 - protrades)

# Limit data to 2009-2010 investments 

tradedf <- fulldatam %>% 
  filter(Year>2008&Year<2011) %>% 
  dplyr::select(State,FIPS,value_2010) %>% 
  group_by(State,FIPS) %>% 
  summarise_all(sum)

# Transition from county to cd 
ctycd <- read.csv("data/ctycd0304.csv",header=T)

ctycd2 <- ctycd %>% 
  rename("FIPS"=county) %>% 
  dplyr::select(FIPS,cd108,afact)

tradedf2 <- left_join(tradedf,ctycd2) 

tradedf3 <- tradedf2 %>% 
  mutate(inv_value=value_2010*afact,
         cdids=paste0(State,cd108)) %>% 
  ungroup() %>% 
  dplyr::select(-FIPS,-value_2010,-afact,-cd108,-State) %>% 
  group_by(cdids) %>%
  summarise_all(sum)

tradevbase <- left_join(tradepcd2,tradedf3)

tradevbase$inv_value[is.na(tradevbase$inv_value)] <- 0

tradevbase <- tradevbase %>% 
  mutate(State=substr(cdids,1,2))

## add unemployment at cd level

unemp08 <- read.csv('data/unemploycd08.csv') 

unemp08$state <- trimws(unemp08$state)

unemp08 <- left_join(unemp08,stateab)

unemp08 <- unemp08 %>% 
  mutate(cdids=paste0(State,cdid,sep="")) %>% 
  dplyr::select(cdids,unemptot,unempbach)

tradevbase <- left_join(tradevbase,unemp08)

## Add partisanship data to the main data 

party <- tradep09 %>% 
  rename(HR09 = House.Representative,
         Party09 = Party) %>% 
  dplyr::select(HR09,Party09,State,cdid)

cdidp <- full_join(tradep10,party)

cdidp <- cdidp %>% 
  dplyr::select(House.Representative,HR09,Party09,Party,State,cdid) 

cdidp$House.Representative = as.character(cdidp$House.Representative)
cdidp$HR09 = as.character(cdidp$HR09)
cdidp$Party09 = as.character(cdidp$Party09)
cdidp$Party = as.character(cdidp$Party)

cdidp$House.Representative = trimws(cdidp$House.Representative)
cdidp$HR09 = trimws(cdidp$HR09)

# 34 districts had partisan change from 109th congress to 110th congress
cdidp <- cdidp %>% 
  mutate(change = ifelse(((House.Representative==HR09) | (Party09==Party)),0,1)) 

cdidpuse <- cdidp %>% 
  filter(!HR09=="Brian P. Bilbray") %>% 
  filter(!House.Representative=="Niki Tsongas") %>% 
  filter(!HR09=="Robert Menendez") %>% 
  filter(!House.Representative=="Jo Ann Davis") %>% 
  dplyr::select(Party,State,cdid,change) %>% 
  rename(state=State) %>% 
  filter(change==0)


cdidpuse$state = trimws(cdidpuse$state)
cdidpuse <- left_join(cdidpuse,stateab)

cdidpuse <- cdidpuse %>% 
  mutate("cdids"=paste0(State,cdid,sep="")) %>% 
  dplyr::select(cdids,Party) %>% 
  distinct()

tradevbase <- left_join(tradevbase,cdidpuse)

tradevbase <- tradevbase %>% 
  mutate(inv_value_log=log(inv_value+0.1)) %>% 
  mutate(State=as.factor(State),
         Party=as.factor(Party))

write.dta(tradevbase,"analysis_data/quidproquo.dta")




