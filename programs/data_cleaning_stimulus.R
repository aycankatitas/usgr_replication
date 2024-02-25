##################
# Do Politicians Prioritize Investment Incentives? 
# Date: 08/10/2023 
# Author: Aycan Katitas 
# Data Cleaning and Merging for Stimulus Variables
##################

##################
### setwd and load libraries 

setwd("/Users/aycankatitas/Dropbox/UVA stuff/usgr")
library(dplyr)
library(lubridate)
library(data.table)
library(tidyr)

### FUNCTIONS 

prep_stim <- function(filename,stimname,local){
  filename= paste0("data/stimulus_files/",filename)
  data <- read.csv(filename,skip=1,stringsAsFactors = FALSE)
  
  # They are already all sub-recipient but just to make sure 
  datafull <- data %>% 
    filter(!is.na(Place.of.Performance.Postal.Code) & Recipient.Type=="Sub-recipient")
  
  if(local=="yes"){
    datafull <- datafull %>% 
      filter(grepl("COUNTY",Recipient.Name,ignore.case=TRUE)|grepl("CITY",Recipient.Name,ignore.case=TRUE))
  }
  
  data2 <- datafull %>% 
    dplyr::select(Recipient.Name,CFDA.Number,Award.Date,Award.Amount,Program.Source..TAS.,Activity.Code,Place.of.Performance.Postal.Code)
  
  names(data2) <- c("recname","cfdano","date","amount","tas","activitycode","pop_zip")
  
  data2$date <- mdy(data2$date)
  
  dat <- data2 %>% 
    separate(date,c("Year","Month","Day")) %>% 
    filter(Year==2009|Year==2010|Year==2011)
  
  dat <- left_join(dat,yearly_cpi,by="Year")
  dat$amount2 <- gsub(",","",dat$amount)
  dat$amount2 <- as.numeric(gsub("\\$","",dat$amount2))
  
  dat<- dat %>% 
    mutate(award_amt10 = amount2*adj_factor10)
  
  dat$zipcode<-as.character(dat$pop_zip)
  dat$zipcode <- substr(dat$zipcode, 1, nchar(dat$zipcode)-4)
  
  dat<-left_join(dat,zip)
  
  datzip <- dat %>% 
    filter(!is.na(fips))
  
  datazip <- left_join(datzip,capitalcounty)
  
  datazip$capital[is.na(datazip$capital)] <- 0
  
  datazip$award_amt10 <- as.numeric(datazip$award_amt10)
  
  dataval <- datazip %>% 
    group_by(fips,capital) %>% 
    summarise(stimval = sum(award_amt10))
  
  savename = paste0("data/stimulus_files/",stimname,"subprime.Rdata",sep="")
  
  save(dataval,file=savename)
  
}

### IMPORT INFLATION FILES AND CAPITAL CITIES

# Prepare inflation + zip-county data 
monthly_cpi <-  read.csv("data/CPIAUCSL.csv")
monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% dplyr::summarize(cpi = mean(CPIAUCSL))
yearly_cpi$adj_factor10 <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2010]
yearly_cpi <- yearly_cpi %>% rename("Year"=cpi_year) %>% dplyr::select(Year,adj_factor10)
yearly_cpi$Year <- as.character(yearly_cpi$Year)

# zipcode
zip<-read.csv("data/zipcounty.csv")
zip$zipcode <- as.character(zip$zipcode)

# Counties that include capital cities 
capitalzip <- read.csv("data/capitalzip.csv")

capitalzip <- capitalzip %>% 
  rename(zipcode=capitalzip)

capitalzip$zipcode <- as.character(capitalzip$zipcode)

capitalzip <- left_join(capitalzip,zip)

capitalcounty <- capitalzip %>% 
  dplyr::select(fips) %>% 
  mutate(capital=1)


# These are all subprime recipients
prep_stim("subdot.csv","dot","no")
prep_stim("subeducation.csv","education","no")
prep_stim("subdothighway.csv","highway","no")
prep_stim("suballstim.csv","allstim","no")

# These are subprime recipients that are local government 
prep_stim("subdot.csv","dotcounty","yes")
prep_stim("subdothighway.csv","highwaycounty","yes")
prep_stim("suballstim.csv","allstimcounty","yes")

## EDUCATION STIMULUS

filename="subeducation.csv"
filename= paste0("data/stimulus_files/",filename)
data <- read.csv(filename,skip=1,stringsAsFactors = FALSE)

datafull <- data %>% 
  filter(!is.na(Place.of.Performance.Postal.Code) & Recipient.Type=="Sub-recipient")

data2 <- datafull %>% 
  dplyr::select(Recipient.Name,CFDA.Number,Award.Date,Award.Amount,Program.Source..TAS.,Activity.Code,Place.of.Performance.Postal.Code)

names(data2) <- c("recname","cfdano","date","amount","tas","activitycode","pop_zip")

data2$date <- mdy(data2$date)

dat <- data2 %>% 
  separate(date,c("Year","Month","Day")) %>% 
  filter(Year==2009|Year==2010|Year==2011)

dat <- left_join(dat,yearly_cpi,by="Year")
dat$amount2 <- gsub(",","",dat$amount)
dat$amount2 <- as.numeric(gsub("\\$","",dat$amount2))

dat<- dat %>% 
  mutate(award_amt10 = amount2*adj_factor10)

dat$zipcode<-as.character(dat$pop_zip)
dat$zipcode <- substr(dat$zipcode, 1, nchar(dat$zipcode)-4)

dat<-left_join(dat,zip)

datzip <- dat %>% 
  filter(!is.na(fips))

datazip <- left_join(datzip,capitalcounty)

datazip$capital[is.na(datazip$capital)] <- 0

datazip$award_amt10 <- as.numeric(datazip$award_amt10)

# Looking at funds going to community colleges 

# search for community or college anywhere in the string without any specific order, but they both have to be in the string
datazip_com <- datazip %>% 
  filter(grepl("^(?=.*\\bCOMMUNITY\\b)(?=.*\\bCOLLEGE\\b)", recname, ignore.case = TRUE,perl=TRUE))

dataval <- datazip_com %>% 
  group_by(fips,capital) %>% 
  summarise(stimval = sum(award_amt10))


save(dataval,file="data/stimulus_files/educsubprimecommunitycollege.Rdata")

## DEPARTMENT OF TRANSPOTATION PRIME STIMULUS
# federal to states

data <- read.csv("data/stimulus_files/dotprime.csv",skip=1,stringsAsFactors = FALSE)

datafull <- data %>% 
  filter(!is.na(Place.of.Performance.Postal.Code))

data2 <- datafull %>% 
  dplyr::select(Recipient.Name,CFDA.Number,Award.Date,Award.Amount,Program.Source..TAS.,Activity.Code,Place.of.Performance.Postal.Code)

names(data2) <- c("recname","cfdano","date","amount","tas","activitycode","pop_zip")

data2$date <- mdy(data2$date)

dat <- data2 %>% 
  separate(date,c("Year","Month","Day")) %>% 
  filter(Year==2009|Year==2010|Year==2011)

dat <- left_join(dat,yearly_cpi,by="Year")
dat$amount2 <- gsub(",","",dat$amount)
dat$amount2 <- as.numeric(gsub("\\$","",dat$amount2))

dat<- dat %>% 
  mutate(award_amt10 = amount2*adj_factor10)

dat$zipcode<-as.character(dat$pop_zip)
dat$zipcode <- substr(dat$zipcode, 1, nchar(dat$zipcode)-4)

dat<-left_join(dat,zip)

datzip <- dat %>% 
  filter(!is.na(fips))

datazip <- left_join(datzip,capitalcounty)

datazip$capital[is.na(datazip$capital)] <- 0

datazip$award_amt10 <- as.numeric(datazip$award_amt10)

dataval <- datazip %>% 
  group_by(fips,capital) %>% 
  summarise(stimval = sum(award_amt10))

save(dataval,file="data/stimulus_files/dotprime.Rdata")
  
# Highway

datafull <- data %>% 
  filter(!is.na(Place.of.Performance.Postal.Code)) %>% 
  filter(Received.From=="Federal Highway Administration")

data2 <- datafull %>% 
  dplyr::select(Recipient.Name,CFDA.Number,Award.Date,Award.Amount,Program.Source..TAS.,Activity.Code,Place.of.Performance.Postal.Code)

names(data2) <- c("recname","cfdano","date","amount","tas","activitycode","pop_zip")

data2$date <- mdy(data2$date)

dat <- data2 %>% 
  separate(date,c("Year","Month","Day")) %>% 
  filter(Year==2009|Year==2010|Year==2011)

dat <- left_join(dat,yearly_cpi,by="Year")
dat$amount2 <- gsub(",","",dat$amount)
dat$amount2 <- as.numeric(gsub("\\$","",dat$amount2))

dat<- dat %>% 
  mutate(award_amt10 = amount2*adj_factor10)

dat$zipcode<-as.character(dat$pop_zip)
dat$zipcode <- substr(dat$zipcode, 1, nchar(dat$zipcode)-4)

dat<-left_join(dat,zip)

datzip <- dat %>% 
  filter(!is.na(fips))

datazip <- left_join(datzip,capitalcounty)

datazip$capital[is.na(datazip$capital)] <- 0

datazip$award_amt10 <- as.numeric(datazip$award_amt10)

dataval <- datazip %>% 
  group_by(fips,capital) %>% 
  summarise(stimval = sum(award_amt10))

save(dataval,file="data/stimulus_files/highwayprime.Rdata")

## DEPARTMENT OF ENERGY PRIME STIMULUS
# 89-4576: Department of Energy-Title 17 Innovative Technology Direct Loan Financing, Recovery
# 89-0331: Department of Energy-Energy Efficiency and Renewable Energy, Recovery

data <- read.csv("data/stimulus_files/primedoe.csv",skip=1,stringsAsFactors = FALSE)

# filter for specific TAs for renewable energy
datafull <- data %>% 
  filter(!is.na(Place.of.Performance.Postal.Code)) %>% 
  filter(grepl("89-4576",Program.Source..TAS.)|grepl("89-0331",Program.Source..TAS.))

data2 <- datafull %>% 
  dplyr::select(Recipient.Name,CFDA.Number,Award.Date,Award.Amount,Program.Source..TAS.,Activity.Code,Place.of.Performance.Postal.Code)

names(data2) <- c("recname","cfdano","date","amount","tas","activitycode","pop_zip")

data2$date <- mdy(data2$date)

dat <- data2 %>% 
  separate(date,c("Year","Month","Day")) %>% 
  filter(Year==2009|Year==2010|Year==2011)

dat <- left_join(dat,yearly_cpi,by="Year")
dat$amount2 <- gsub(",","",dat$amount)
dat$amount2 <- as.numeric(gsub("\\$","",dat$amount2))

dat<- dat %>% 
  mutate(award_amt10 = amount2*adj_factor10)

dat$zipcode<-as.character(dat$pop_zip)
dat$zipcode <- substr(dat$zipcode, 1, nchar(dat$zipcode)-4)

dat<-left_join(dat,zip)

datzip <- dat %>% 
  filter(!is.na(fips))

datazip <- left_join(datzip,capitalcounty)

datazip$capital[is.na(datazip$capital)] <- 0

datazip$award_amt10 <- as.numeric(datazip$award_amt10)

dataval <- datazip %>% 
  group_by(fips,capital) %>% 
  summarise(stimval = sum(award_amt10))

save(dataval,file="data/stimulus_files/doeprime.Rdata")

## INFRASTRUCTURE PRIME STIMULUS
#13-0554: Department of Commerce-National Telecommunication and Information Administration-Broadband Technology Opportunities Program, Recovery Act
# (This was only given to Uni of Hawaii, not included)
#27-0200: Federal Communications Commission-Broadband Technology Opportunities Program, Recovery Act
#(Prime recipients)
#10.886 - Rural Broadband Access Loans and Loan Guarantees (prime)
# Department of Labor-Employment and Training Administration-Training and Employment Services (subprime recipients)


labor <- read.csv("data/stimulus_files/subemptraining.csv",skip=1,stringsAsFactors = FALSE)
broad <- read.csv("data/stimulus_files/primebroadband.csv",skip=1,stringsAsFactors = FALSE)
rbroad <- read.csv("data/stimulus_files/primeruralbroadband.csv",skip=1,stringsAsFactors = FALSE)

datalist <- list(labor,broad,rbroad)

datalist2 <- lapply(datalist, function(x) x %>% filter(!is.na(Place.of.Performance.Postal.Code)) %>% 
                                                         dplyr::select(Recipient.Name,CFDA.Number,Award.Date,Award.Amount,Program.Source..TAS.,Activity.Code,Place.of.Performance.Postal.Code))

data2 <- bind_rows(datalist2)

names(data2) <- c("recname","cfdano","date","amount","tas","activitycode","pop_zip")


data2$date <- mdy(data2$date)

dat <- data2 %>% 
  separate(date,c("Year","Month","Day")) %>% 
  filter(Year==2009|Year==2010|Year==2011)

dat <- left_join(dat,yearly_cpi,by="Year")
dat$amount2 <- gsub(",","",dat$amount)
dat$amount2 <- as.numeric(gsub("\\$","",dat$amount2))

dat<- dat %>% 
  mutate(award_amt10 = amount2*adj_factor10)

dat$zipcode<-as.character(dat$pop_zip)
dat$zipcode <- substr(dat$zipcode, 1, nchar(dat$zipcode)-4)

dat<-left_join(dat,zip)

datzip <- dat %>% 
  filter(!is.na(fips))

datazip <- left_join(datzip,capitalcounty)

datazip$capital[is.na(datazip$capital)] <- 0

datazip$award_amt10 <- as.numeric(datazip$award_amt10)

dataval <- datazip %>% 
  group_by(fips,capital) %>% 
  summarise(stimval = sum(award_amt10))

save(dataval,file="data/stimulus_files/infrastructure.Rdata")













  








