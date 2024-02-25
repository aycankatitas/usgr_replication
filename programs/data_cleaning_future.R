##################
# Investment Incentives Attract FDI
# Date: 08/10/2023 
# Author: Aycan Katitas 
# What Happens After 2011? 
##################
setwd("/Users/aycankatitas/Dropbox/UVA stuff/usgr")

load("data/usdatawfips.Rdata")

man1316 <- fulldatafips %>% 
  filter(Industry.Activity=="Manufacturing"&Project.Type=="New") %>% 
  filter(Year>2011&Year<2017) %>% 
  group_by(FIPS) %>% 
  summarise(totalinv=sum(value_2010))

county_an <- read.dta("analysis_data/countylevel.dta")

fan <- county_an %>% 
  dplyr::select(FIPS,State,newdummn,capital) %>% 
  filter(!is.na(State))

fan <- left_join(fan,man1316)

fan$capital[is.na(fan$capital)] <- 0 
fan$totalinv[is.na(fan$totalinv)] <- 0 

fan <- fan %>% 
  mutate(newdummn=as.factor(newdummn))

# Add unemployment 
filename = paste0("data/laucnty11.txt")

unemp <- read.fwf(
  skip=6,
  file=filename,
  widths=c(17, 7, 7, 47, 12,11,14,9, 8))

unemp2 <- head(unemp,-3)

unemp2 <- unemp2 %>%
  dplyr::select(V2,V3,V5,V7,V9) %>% 
  rename(statefips=V2,
         countyfips=V3,
         Year=V5,
         Employed=V7,
         unemp = V9)


unemp2 <- data.frame(lapply(unemp2, trimws), stringsAsFactors = FALSE)


unemp2$statefips <- as.numeric(unemp2$statefips)
unemp2$countyfips <- as.numeric(unemp2$countyfips)

ctycross <- read.csv("data/allcountylist2.csv") %>% 
  dplyr::select(statefips,countyfips,FIPS)

unemp2 <- left_join(unemp2,ctycross) %>% 
  filter(!is.na(FIPS)) %>% 
  dplyr::select(FIPS,Year, Employed,unemp)

unemp2$Year <- as.numeric(unemp2$Year)
unemp2$Employed <- as.numeric(gsub("," ,"", unemp2$Employed))

fan <- left_join(fan,unemp2)

fan <- fan %>% 
  mutate(unemp=as.numeric(unemp))

# Patents 

patents <- read.csv("data/patentfile.csv",stringsAsFactors = FALSE) %>% 
  dplyr::select(FIPS.Code,starts_with("X")) %>% 
  rename(FIPS=FIPS.Code)

patents_long <- patents %>% 
  gather(Year, patentcount, -c(FIPS))

patents_long$Year <- gsub("X","",patents_long$Year)

patents_long <- patents_long %>% 
  filter(Year=="2011") %>%
  dplyr::select(-Year) %>% 
  mutate(FIPS=as.numeric(FIPS))


fan <- left_join(fan,patents_long)

fan$patentcount[is.na(fan$patentcount)] <- 0 

fan <- fan %>% 
  mutate(patentcount_log=log(patentcount+0.1))

## Add domestic investment 

monthly_cpi <-
  read.csv("data/CPIAUCSL.csv", header = TRUE)

monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% dplyr::summarize(cpi = mean(CPIAUCSL))
yearly_cpi$adj_factor10 <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2010]

yearly_cpi <- yearly_cpi %>% 
  rename("Year"=cpi_year) %>% 
  dplyr::select(Year,adj_factor10)

yearly_cpi$Year <- as.factor(yearly_cpi$Year)
load("data/domesticfdiready.Rdata")

domfdi6 <- domfdifips3 %>% 
  left_join(yearly_cpi) %>% 
  mutate(value_2010=CapitalInvestment*adj_factor10)


domfdimanu <- domfdi6 %>% 
  filter(IndustryActivity=="Manufacturing" & ProjectType=="New") %>% 
  filter(!is.na(FIPS)) %>% 
  filter(Year>2011&Year<2017) %>% 
  group_by(FIPS) %>% 
  summarise(domtotalinv=sum(value_2010))

fan <- left_join(fan,domfdimanu)

fan$domtotalinv[is.na(fan$domtotalinv)] <- 0 


# Add RVoteshare

county <- read.csv("data/finalRvote.csv")

county$County <- gsub("[.]","",county$County)


countym <- county %>%
  filter(!is.na(RVoteshare)) %>% 
  filter(State!="HI") %>% 
  filter(State!="AK") %>% 
  distinct() %>% 
  filter(Year==2011) %>% 
  dplyr::select(FIPS,RVoteshare,RVote)

fan <- left_join(fan,countym)

fan <- fan %>% 
  mutate(logemp=log(Employed))

fan$newdummn <- factor(fan$newdummn, levels = c(0,1, 2))

fan <- fan %>% 
  mutate(totalinv_log=log(totalinv),
         domtotalinv_log=log(domtotalinv),
         totalinv_log=ifelse(totalinv_log==-Inf,0,totalinv_log),
         domtotalinv_log=ifelse(domtotalinv_log==-Inf,0,domtotalinv_log))

write.dta(fan,"analysis_data/statelevel1216.dta")

library(lfe)
d <- felm(totalinv_log ~ newdummn + RVoteshare+unemp+patentcount_log+Employed|State|0|State, data=fan)
summary(d)

d <- felm(domtotalinv_log ~ newdummn + RVoteshare+unemp+patentcount_log+Employed|State|0|State, data=fan)
summary(d)



