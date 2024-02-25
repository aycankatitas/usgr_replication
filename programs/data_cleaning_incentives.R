##################
# Do Politicians Prioritize Investment Incentives? 
# Date: 08/10/2023 
# Author: Aycan Katitas 
# Data Cleaning and Merging - Good Jobs First Investment Incentive Trackers
##################

##################
### setwd and load libraries 
setwd("/Users/aycankatitas/Dropbox/UVA stuff/usgr")
library(dplyr)

### IMPORT GJS Tracker

# "subsidy_adjusted" should be used to aggregate subsidies across projects (to avoid double counting)
# loans are not counted in subsidy amounts 
# All federal funding is omitted. 

data <- read.csv(file="data/subsidytrack.csv",header=T, as.is=T)

data$subsidy_type <- tolower(data$subsidy_type)

# omit federal subsidies 
data <- data[-which(data$subsidy_level=="federal"),]

data <- data %>%
  mutate(subsidy_type=trimws(subsidy_type))

# flag tax incentives 
tax <- rep(0,nrow(data))
tax.type <- c("property tax abatement","tax credit/rebate",
              "tax credit/rebate and grant","tax credit/rebate; property tax abatement",
              "tax increment financing")
tax[which(data$subsidy_type %in% tax.type)] <- 1
data <- as.data.frame(list(data,tax=tax),stringsAsFactors = F)

# flag real time incentives (that require real time payout of money)
real.time <- c("training reimbursement","cost reimbursement", "grant","grant/loan hybrid program","loan",
               "enterprise zone","loan or bond financing","multiple","infrastructure assistance")
# real.time <- c("training reimbursement","cost reimbursement", "grant","grant/loan hybrid program","loan",
#                "enterprise zone","loan or bond financing","multiple") # types of subsidies that involve real time payments
now.pay <- rep(0,nrow(data))
now.pay[which(data$subsidy_type %in% real.time)] <- 1
data <- as.data.frame(list(now.pay=now.pay,data),stringsAsFactors = F)

# flag cash incentives
data <- data %>% 
  mutate(cash=ifelse(subsidy_type %in% c("grant","training reimbursement"),1,0))

# flag land incentives 
data <- data %>% 
  mutate(land=ifelse(subsidy_type %in% c("enterprise zone","infrastructure assistance"),1,0))

# Create incentive measure with all non-federal incentives   
incent.state <- aggregate(data[,"subsidy_adjusted"],by=list(year=data$sub_year,state=data$state),sum,na.rm=T)
names(incent.state) <- c("year","state","tot.incent")

incent.tax <- aggregate(data[which(data$tax==1),"subsidy_adjusted"],by=list(year=data[which(data$tax==1),"sub_year"],state=data[which(data$tax==1),"state"]),sum,na.rm=T)
names(incent.tax) <- c("year","state","tax.incent")

incent.real <- aggregate(data[which(data$now.pay==1),"subsidy_adjusted"],by=list(year=data[which(data$now.pay==1),"sub_year"],state=data[which(data$now.pay==1),"state"]),sum,na.rm=T)
names(incent.real) <- c("year","state","real.incent")

state.all <- merge(incent.state,incent.tax,by=c("year","state"),all=T)
state.all <- merge(state.all,incent.real,by=c("year","state"),all=T)
save(state.all,file="data/state.incent.Rdata")

### CREATE DF FROM ONLY STATE-SPONSORED INCENTIVES
state <- data[which(data$subsidy_level=="state"),]
save(state,file="data/all.stateincent.Rdata")

state.lev <- aggregate(state[,"subsidy_adjusted"],by=list(year=state$sub_year,state=state$state),sum,na.rm=T)
names(state.lev) <- c("year","state","tot.statelev")

# aggregate state tax incentives 
state.tax <- aggregate(state[which(state$tax==1),"subsidy_adjusted"],by=list(year=state[which(state$tax==1),"sub_year"],state=state[which(state$tax==1),"state"]),sum,na.rm=T)
names(state.tax) <- c("year","state","tax.statelev")

# aggregate state real time incentives 
state.real <- aggregate(state[which(state$now.pay==1),"subsidy_adjusted"],by=list(year=state[which(state$now.pay==1),"sub_year"],state=state[which(state$now.pay==1),"state"]),sum,na.rm=T)
names(state.real) <- c("year","state","real.statelev")

# aggregate state cash incentives 
state.cash <- aggregate(state[which(state$cash==1),"subsidy_adjusted"],by=list(year=state[which(state$cash==1),"sub_year"],state=state[which(state$cash==1),"state"]),sum,na.rm=T)
names(state.cash) <- c("year","state","cash.statelev")

# aggregate state land incentives 
state.land <- aggregate(state[which(state$land==1),"subsidy_adjusted"],by=list(year=state[which(state$land==1),"sub_year"],state=state[which(state$land==1),"state"]),sum,na.rm=T)
names(state.land) <- c("year","state","land.statelev")

state.all <- merge(state.lev,state.tax,by=c("year","state"),all=T)
state.all <- merge(state.all,state.real,by=c("year","state"),all=T)
state.all <- merge(state.all,state.cash,by=c("year","state"),all=T)
state.all <- merge(state.all,state.land,by=c("year","state"),all=T)
save(state.all, file="data/state.only.Rdata")

