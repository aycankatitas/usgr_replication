##################
# Do Politicians Prioritize Investment Incentives? 
# Date: 08/10/2023 
# Author: Aycan Katitas 
# Figures
##################

##################
### setwd and load libraries 
setwd("/Users/aycankatitas/Dropbox/UVA stuff/usgr")
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpattern)
library(upstartr)
library(tidyverse)

load("data/usfdisample.Rdata")


### FIGURE 1 - GRAPH OF COUNTY EXPANSION - FOREIGN INVESTMENT

# Indicate which quarter investment has taken place
fulldatam$quarter <- lubridate::quarter(fulldatam$date, with_year=TRUE)
total <- fulldatam 

# Aggregate the number and value of FDI projects by quarter
merged <- total %>%
  group_by(quarter,FIPS) %>% 
  summarise(totalgren=n(),
            totalval=sum(value_2010,na.rm=T)) %>% 
  ungroup()

years = 2003:2016
quarters = 1:4
graphunique = data.frame(quarter = as.numeric(),
                         nocounty = as.numeric())
cg = data.frame(FIPS=as.character())

for(year in years){
  for(quarter in quarters){
    qs = as.numeric(paste0(year,".",quarter))
    
    cgn <- merged %>% 
      filter(quarter==qs) %>% 
      filter(!FIPS %in% cg$FIPS) %>% 
      dplyr::select(FIPS)
    
    cgn <- unique(cgn)
    
    cgnn <- nrow(cgn)
    
    cg <- rbind(cg,cgn)
    
    cgdf <- data.frame(quarter = qs,
                       nocounty = cgnn)
    
    graphunique <- rbind(graphunique,cgdf)
  }}

# Number of all counties that received investment in the quarter
countyyear <- merged %>% 
  count(quarter) %>% 
  rename(nocounty=n)

graphcounty <- rbind(countyyear,graphunique)
graphcounty$grp <- rep(factor(1:2),times = c(nrow(countyyear),nrow(graphunique)))

# x1 <- graphcounty %>% 
#   filter(grp==2) %>% 
#   filter(quarter<2009.1) %>% 
#   summarise(sum=sum(nocounty)) 
# 
# x <- graphcounty %>% 
#   filter(grp==1) %>% 
#   filter(quarter>2008.4&quarter<2012.1) %>%
#   summarise(sum=sum(nocounty))
# 
# xx <- graphcounty %>% 
#   filter(grp==2) %>% 
#   filter(quarter>2008.4&quarter<2012.1) %>%
#   summarise(sum=sum(nocounty))

graphcounty %>% 
  filter(quarter!=2016.4) %>% 
  ggplot() + 
  geom_line(aes(x = quarter,y = nocounty, colour=grp),size=5)+ 
  #geom_line(aes(x=quarter, tot.statelev),size=5,colour="red") +
  xlab("") + ylab("# of FDI Recipient U.S. Counties") +
  labs(colour = "") + 
  theme_bw() +
  scale_color_manual(labels = c("All recipients", "First time recipients (since 2003)"), values=c("black", "gray"))+ 
  theme_classic(base_size=66) + 
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        legend.position="right",
        legend.text=element_text(size=66),
        legend.key.size = unit(5, 'cm')) +
  scale_x_continuous(breaks = round(seq(min(graphcounty$quarter), max(graphcounty$quarter), by = 1),1)) + 
  scale_y_continuous(breaks=round(seq(0, 60, by = 10),1)) +
  ylim(0,60) + 
  geom_vline(xintercept=c(decimal_date(as.Date(c("2007-12-01"))),decimal_date(as.Date(c("2009-06-30")))), linetype="dashed",linewidth=3) 


ggsave(plot=last_plot(),filename="output/figures/quarterlycountyexpansion.png",
       width = 45, height = 25, units = "in")

### FIGURE 2 - INCENTIVES

# Plot yearly incentives data
load("data/state.only.Rdata")

incen_by_year <- state.all %>%  
  mutate(year=as.character(year)) %>% 
  reframe(across(tot.statelev:real.statelev, \(x) sum(x,na.rm=T)), .by=year) %>% 
  mutate(across(tot.statelev:real.statelev, ~./1000000000)) %>% 
  filter(year<2017)

incengraph <- incen_by_year %>% 
  pivot_longer(cols=tot.statelev:real.statelev,
               names_to="group",
               values_to="value") %>% 
  mutate(year=as.numeric(year))

incengraph$group <- factor(incengraph$group,levels=c("real.statelev","tax.statelev","tot.statelev"))

incengraph %>% 
  filter(group!="tot.statelev") %>% 
  ggplot()+
  geom_line(aes(x=year,y=value,colour=group),size=5) +
  xlab("Year")+
  ylab("State Incentives (in $billion)") +
  labs(colour = "") + 
  theme_bw() +
  scale_color_manual(labels = c("Real State Incentives","Tax State Incentives"), values=c("black","gray"))+ 
  theme_classic(base_size=66) + 
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        legend.position="right",
        legend.key.size = unit(3, 'cm')) +
  scale_x_continuous(breaks = round(seq(min(incengraph$year), max(incengraph$year), by = 2),1)) +
  scale_y_continuous(breaks=round(seq(0, 25, by = 5),1)) +
  geom_vline(xintercept=c(decimal_date(as.Date(c("2007-12-01"))),decimal_date(as.Date(c("2009-06-30")))), linetype="dashed",linewidth=3) 


ggsave(plot=last_plot(),filename="output/figures/incentivesgraph.png",
       width = 45, height = 25, units = "in")


### Figure 3 - New/Old County Predictors 

mnlg <- read.csv("output/figures/marginsoutput.csv",skip=1,stringsAsFactors = F,na.strings="")

names(mnlg) <- c("variable","diffnever","senever","diffvaold","sevaold","diffvanew","sevanew")

mnlg2  <- mnlg %>% 
  mutate_all(~gsub("\\(|\\)|=|\\*", "", .)) %>% 
  filter(variable!="Standard errors in parentheses") %>% 
  pivot_longer(cols=diffnever:sevanew,
               names_to="group",
               values_to="value") %>% 
  separate(group,c("var","group"),sep=-5,convert=T) %>% 
  pivot_wider(names_from="var",
              values_from="value") %>%
  mutate(diff=as.numeric(diff),
         se=as.numeric(se),
         ul=diff+1.96*se,
         ll=diff-1.96*se) %>% 
  mutate_if(is.numeric,funs(.*100)) %>% 
  mutate(group=ifelse(group=="never","Never County",
                      ifelse(group=="vaold", "Old County","New County")))

mnlg2$group <- factor(mnlg2$group, levels=c("New County","Old County","Never County"))
mnlg2$variable <- factor(mnlg2$variable, levels=c("logtotaldom","lagged_gfield","logmacount",
                                                  "logpatentcount","cdR","labforce","unemp08",
                                                  "RVoteshare","substimeduc_log","1.comp","real_incentives_log"))


mnlg2 %>% 
  ggplot(aes(x=variable, y=diff, ymin=ll, ymax=ul))+
  geom_pointrange(size=1) + 
  geom_hline(yintercept=0, lty=2,size=1,color="gray") +  # add a dotted line at x=1 after flip
  coord_flip() +
  facet_wrap(~group) +
  theme_classic(base_size = 46)  +
  theme(legend.position = "none") +
  labs(x="",
       y="Marginal Effect on Probability",
       title="",
       substitle="" #,caption=""
  ) +
  scale_x_discrete(labels=c("Domestic Greenfield Investment 2008","Foreign Greenfield Investment 2008",
                            "M&A Count (Logged) 2008","Patent Count (Logged) 2008","Representative Partisanship",
                            "Labor Force 2008","Unemployment Rate 2008","McCain Share 2008",
                            "Education Stimulus (Logged)","Competitive County","Real-Time Incentives (Logged)"))

ggsave(plot=last_plot(),filename="output/figures/mnlres.png",
       width = 30, height = 20, units = "in")


### FIGURE A - GRAPH OF COUNTY EXPANSION - DOMESTIC INVESTMENT 

load("data/usinvsample.Rdata")

domm$quarter <- lubridate::quarter(domm$date, with_year=TRUE)

domtotal <- domm 

merged <- domtotal %>%
  group_by(quarter,FIPS) %>% 
  summarise(totalgren=n(),
            totalval=sum(value_2010,na.rm=T)) %>% 
  ungroup()

years = 2007:2016
quarters = 1:4

graphunique = data.frame(quarter = as.numeric(),
                         nocounty = as.numeric())

cg = data.frame(FIPS=as.character())


for(year in years){
  for(quarter in quarters){
    qs = as.numeric(paste0(year,".",quarter))
    
    cgn <- merged %>% 
      filter(quarter==qs) %>% 
      filter(!FIPS %in% cg$FIPS) %>% 
      dplyr::select(FIPS)
    
    cgn <- unique(cgn)
    
    cgnn <- nrow(cgn)
    
    cg <- rbind(cg,cgn)
    
    cgdf <- data.frame(quarter = qs,
                       nocounty = cgnn)
    
    graphunique <- rbind(graphunique,cgdf)
  }}

# All counties 

countyyear <- merged %>% 
  count(quarter) %>% 
  rename(nocounty=n)


#write.csv(countyyear,"graphlist.csv",quote=FALSE,row.names=FALSE)

graphcounty <- rbind(countyyear,graphunique)
graphcounty$grp <- rep(factor(1:2),times = c(nrow(countyyear),nrow(graphunique)))


graphcounty %>% 
  filter(quarter!=2016.4) %>% 
  ggplot() +
  geom_line(aes(x = quarter,y = nocounty, colour=grp),size=4)+ 
  xlab("") + ylab("# of Domestic Investment Recipient U.S. Counties") +
  labs(colour = "") + 
  theme_bw() +
  scale_color_manual(labels = c("All recipients", "First time recipients (since 2007)"), values=c("black", "gray"))+ 
  theme_classic(base_size=66) + 
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        legend.position="right") +
  scale_x_continuous(breaks = round(seq(min(graphcounty$quarter), max(graphcounty$quarter), by = 1),1)) + 
  scale_y_continuous(breaks=round(seq(0, 60, by = 20),1)) +
  geom_vline(xintercept=c(decimal_date(as.Date(c("2007-12-01"))),decimal_date(as.Date(c("2009-06-30")))), linetype="dashed") 


ggsave(plot=last_plot(),filename="output/figures/domesticuniquegraphquarterlybw.png",
       width = 45, height = 25, units = "in")


### FIGURE A1 - MEDIAN VALUE OF FDI OVER TIME

# SHOULD WE exclude SASOL, an obvious outlier with $10 billion investment
overyearsw <- fulldatam %>% 
  filter(Investing.Company!="SASOL") %>% 
  group_by(Year) %>% 
  summarise(across(c(value_2010,Jobs.Created), 
                   list(mean = ~ mean(., na.rm=T),
                        median = ~ median(.,na.rm=T),
                        sd= ~ sd(., na.rm=T))))

# Median 
overyearsw %>%
  ggplot(aes(x=Year,y=value_2010_median)) + 
  geom_errorbar(aes(ymin=value_2010_median, ymax=value_2010_median+value_2010_sd), width=0.2,size=2) +
  geom_line(linewidth=2) +
  geom_point(size=8) +
  ylab("Investment Value (in $ million)") + 
  xlab("Year") + 
  scale_x_continuous(breaks=round(seq(min(overyearsw$Year), max(overyearsw$Year), by = 1),1)) + 
  scale_y_continuous(breaks=round(seq(0, 1600, by = 200),1)) + 
  theme_classic(base_size=66) 
  #+ ggtitle("Median New Foreign Manufacturing Investment Value over Years")


ggsave(plot=last_plot(),filename="output/figures/fdioveryearsmdsd.png",
       width = 45, height = 25, units = "in")

### FIGURE X - BEFORE/AFTER INDUSTRY ACTIVITY 

load("data/usdatawfips.Rdata")

#2007-2004 2011-2008
brdf <- fulldatafips %>% 
  mutate(group=ifelse(Year<2008&Year>2004,"2005-2007",
                      ifelse(Year>2008&Year<2012,"2009-2011",NA))) %>% 
  filter(!is.na(group)) 

brdf$group <- factor(brdf$group,levels=c("2005-2007","2009-2011"))

# Plotting the top 20 Industry Activity before-after GR 
brdf %>% 
  count(group,Industry.Activity) %>%
  rename(count=n) %>% 
  group_by(group) %>% 
  slice_max(order_by = count, n = 20) %>% 
  ggplot(aes(reorder_within(Industry.Activity,-count,group),count)) +
  #theme_classic(base_size=46) + 
  geom_col_pattern(
    aes(
      pattern = group
    ), 
    colour                   = 'black', 
    pattern_density          = 0.5, 
    pattern_key_scale_factor = 1.11
  ) +
  theme_classic(base_size=17)+
  theme(axis.text.x=element_text(angle=75, hjust=1),
        legend.position = "none") +
  
  facet_wrap(~group,scales = "free_x", drop = TRUE) +
  
  
  #theme(text=element_text(size=25),
  #     axis.text.x=element_text(angle=90),
  #    legend.position = "none") +
  #theme(axis.text.x=element_text(angle=55,hjust=1,size=25),
  #   legend.position = "none") +
  scale_x_reordered() + 
  labs(x="",
       y="Project Counts"
  ) 

ggsave(plot=last_plot(),filename="output/figures/beforeafterGRindustryactivity.png",
       width = 15, height = 10, units = "in")

### FIGURE X - BEFORE/AFTER INDUSTRY SECTOR IN MANUFACTURING

# Plotting top 20 industry sector before-after GR 
brdf %>% 
  filter(Industry.Activity=="Manufacturing") %>% 
  count(group,Industry.Sector) %>%
  rename(count=n) %>% 
  group_by(group) %>% 
  slice_max(order_by = count, n = 20) %>% 
  ggplot(aes(reorder_within(Industry.Sector,-count,group),count)) +
  #theme_classic(base_size=46) + 
  geom_col_pattern(
    aes(
      pattern = group
    ), 
    colour                   = 'black', 
    pattern_density          = 0.5, 
    pattern_key_scale_factor = 1.11
  ) +
  theme_classic(base_size=17)+
  theme(axis.text.x=element_text(angle=75, hjust=1),
        legend.position = "none") +
  
  facet_wrap(~group,scales = "free_x", drop = TRUE) +
  
  
  #theme(text=element_text(size=25),
  #     axis.text.x=element_text(angle=90),
  #    legend.position = "none") +
  #theme(axis.text.x=element_text(angle=55,hjust=1,size=25),
  #   legend.position = "none") +
  scale_x_reordered() + 
  labs(x="",
       y="Project Counts"
  ) 

ggsave(plot=last_plot(),filename="output/figures/beforeafterGRindustrysector.png",
       width = 15, height = 10, units = "in")

### FIGURE X - BEFORE/AFTER COO

# Plotting top 20 industry sector before-after GR 
brdf %>% 
  filter(Industry.Activity=="Manufacturing") %>% 
  count(group,Source.Country) %>%
  rename(count=n) %>% 
  group_by(group) %>% 
  slice_max(order_by = count, n = 20) %>% 
  ggplot(aes(reorder_within(Source.Country,-count,group),count)) +
  #theme_classic(base_size=46) + 
  geom_col_pattern(
    aes(
      pattern = group
    ), 
    colour                   = 'black', 
    pattern_density          = 0.5, 
    pattern_key_scale_factor = 1.11
  ) +
  theme_classic(base_size=17)+
  theme(axis.text.x=element_text(angle=75, hjust=1),
        legend.position = "none") +
  
  facet_wrap(~group,scales = "free_x", drop = TRUE) +
  
  
  #theme(text=element_text(size=25),
  #     axis.text.x=element_text(angle=90),
  #    legend.position = "none") +
  #theme(axis.text.x=element_text(angle=55,hjust=1,size=25),
  #   legend.position = "none") +
  scale_x_reordered() + 
  labs(x="",
       y="Project Counts"
  ) 

ggsave(plot=last_plot(),filename="output/figures/beforeafterGRcoo.png",
       width = 15, height = 10, units = "in")

### FIGURE X - Domestic Investment 

load("data/usinvsample.Rdata")

domm$quarter <- lubridate::quarter(domm$date, with_year=TRUE)

domtotal <- domm 

merged <- domtotal %>%
  group_by(quarter,FIPS) %>% 
  summarise(totalgren=n(),
            totalval=sum(value_2010,na.rm=T)) %>% 
  ungroup()

years = 2007:2016
quarters = 1:4

graphunique = data.frame(quarter = as.numeric(),
                         nocounty = as.numeric())

cg = data.frame(FIPS=as.character())


for(year in years){
  for(quarter in quarters){
    qs = as.numeric(paste0(year,".",quarter))
    
    cgn <- merged %>% 
      filter(quarter==qs) %>% 
      filter(!FIPS %in% cg$FIPS) %>% 
      dplyr::select(FIPS)
    
    cgn <- unique(cgn)
    
    cgnn <- nrow(cgn)
    
    cg <- rbind(cg,cgn)
    
    cgdf <- data.frame(quarter = qs,
                       nocounty = cgnn)
    
    graphunique <- rbind(graphunique,cgdf)
  }}

# All counties 

countyyear <- merged %>% 
  count(quarter) %>% 
  rename(nocounty=n)


#write.csv(countyyear,"graphlist.csv",quote=FALSE,row.names=FALSE)

graphcounty <- rbind(countyyear,graphunique)
graphcounty$grp <- rep(factor(1:2),times = c(nrow(countyyear),nrow(graphunique)))


graphcounty %>% 
  filter(quarter!=2016.4) %>% 
  ggplot() +
  geom_line(aes(x = quarter,y = nocounty, colour=grp),size=4)+ 
  xlab("") + ylab("# of Domestic Investment Recipient U.S. Counties") +
  labs(colour = "") + 
  theme_bw() +
  scale_color_manual(labels = c("All recipients", "First time recipients (since 2007)"), values=c("black", "gray"))+ 
  theme_classic(base_size=66) + 
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        legend.position="right") +
  scale_x_continuous(breaks = round(seq(min(graphcounty$quarter), max(graphcounty$quarter), by = 1),1)) + 
  scale_y_continuous(breaks=round(seq(0, 60, by = 20),1)) +
  geom_vline(xintercept=c(decimal_date(as.Date(c("2007-12-01"))),decimal_date(as.Date(c("2009-06-30")))), linetype="dashed") 


ggsave(plot=last_plot(),filename="output/figures/quarterlycountydomexpansion.png",
       width = 45, height = 25, units = "in")


### FIGURE X - New/Old County Industry Sector Figure 

load("data/newoldcountycharacteristics.Rdata")

newold_ind <- newold %>%
  add_count(newdummn) %>%  
  rename(total=n) %>% 
  group_by(newdummn,total) %>% 
  count(Industry.Sector) %>%
  rename(count=n) %>%
  mutate(per=(count/total)*100) %>%
  mutate(newdummn=ifelse(newdummn==1,"Old Counties","New Counties"))


newold_ind %>% 
  ggplot(aes(reorder_within(Industry.Sector,-count,newdummn),count)) +
  theme_classic(base_size=17) + 
  geom_col_pattern(
    aes(pattern = newdummn), 
    colour                   = 'black', 
    pattern_density          = 0.5, 
    pattern_key_scale_factor = 1.11,
) +
  scale_pattern_manual(values=c('wave', 'weave')) +
  facet_wrap(~newdummn,scales = "free_x", drop = TRUE) + 
  theme(axis.text.x=element_text(angle=75, hjust=1),
        legend.position = "none") +
  scale_x_reordered() + 
  labs(x="Industry Sector",
       y="Project Counts") 

ggsave(plot=last_plot(),filename="output/figures/newoldindsecper.png",
       width = 15, height = 10, units = "in")

## FIGURE X - New/Old County Home Country Figure

newold_country <- newold %>% 
  add_count(newdummn) %>% 
  rename(total=n) %>% 
  group_by(newdummn,total) %>% 
  count(Source.Country) %>%
  rename(count=n) %>%
  mutate(per=(count/total)*100) %>% 
  mutate(newdummn=ifelse(newdummn==1,"Old Counties","New Counties"))
  

newold_country %>% 
  ggplot(aes(reorder_within(Source.Country,-per,newdummn),per)) +
  theme_classic(base_size=17) + 
  geom_col_pattern(
    aes(pattern = newdummn), 
    colour                   = 'black', 
    pattern_density          = 0.5, 
    pattern_key_scale_factor = 1.11
  ) +
  scale_pattern_manual(values=c('wave', 'weave')) +
  facet_wrap(~newdummn,scales = "free_x", drop = TRUE) + 
  theme(axis.text.x=element_text(angle=75, hjust=1),
        legend.position = "none") +
  scale_x_reordered() + 
  labs(x="Source.Country",
       y="%") 

ggsave(plot=last_plot(),filename="output/figures/newoldhomecountryper.png",
       width = 15, height = 10, units = "in")

# FIGURE A - New/Old/Never Counties - Competitiveness and Mass Layoffs

res <- read.csv("output/figures/complayoff.csv",fill=T)

names(res) <- c("variable","Never_pred","Never_se","Old_pred","Old_se","New_pred","New_se")

res2  <- res %>% 
  filter(variable!="=") %>% 
  mutate_all(~gsub("\\(|\\)|=|\\*", "", .)) %>%
  filter(variable!="Standard errors in parentheses") %>% 
  mutate(comp=ifelse(variable %in% c("1._at","2._at","3._at","4._at","5._at","6._at"),"Not Competitive","Competitive"),
         layoff=ifelse(variable %in% c("1._at","7._at"),0,
                       ifelse(variable %in% c("2._at","8._at"),0.5,
                              ifelse(variable %in% c("3._at","9._at"),1,
                                     ifelse(variable %in% c("4._at","10._at"),1.5,
                                            ifelse(variable %in% c("5._at","11._at"),2,2.5)))))) %>% 
  dplyr::select(-variable) %>%
  pivot_longer(cols=Never_pred:New_se,
               names_to="group",
               values_to="value") %>%
  separate(group,c("status","var")) %>% 
  pivot_wider(names_from="var",
              values_from="value") %>%
  mutate(pred=as.numeric(pred),
         se=as.numeric(se)) %>% 
  mutate(across(pred:se,~.*100))

res2$comp <- factor(res2$comp, levels=c("Not Competitive","Competitive"))
res2$status <- factor(res2$status,levels=c("Never","Old","New"))

res2 %>% 
  ggplot(aes(layoff,pred, shape=status,linetype=status)) +
  geom_point(size=2)+
  geom_line(size=2) +
  facet_wrap(~status+comp,nrow=3,ncol=2) +
  labs(x="Mass Layoff (in 100,000s)",
       y="Predicted Probability (%)") + 
  theme_classic(base_size = 46) +
  theme(legend.position="none")

ggsave(plot=last_plot(),filename="output/figures/complayoff.png",
       width = 20, height = 20, units = "in") 

# FIGURE A - New/Old/Never Counties - Competitiveness and Governor Eligibility 

elig <- read.csv("output/figures/compelig.csv",fill=T)

names(elig) <- c("variable","Never_pred","Never_se","Old_pred","Old_se","New_pred","New_se")

elig2  <- elig %>% 
  filter(variable!="=") %>% 
  mutate_all(~gsub("\\(|\\)|=|\\*", "", .)) %>%
  filter(variable!="Standard errors in parentheses") %>%
  separate(variable,c("comp","elig"),sep="#") %>% 
  mutate(comp=ifelse(grepl("0",comp),"Not Competitive","Competitive"),
         elig=ifelse(grepl("0",elig),"Not Eligible","Eligible")) %>%
  pivot_longer(cols=Never_pred:New_se,
               names_to="group",
               values_to="value") %>%
  separate(group,c("status","var")) %>%
  pivot_wider(names_from="var",
              values_from="value") %>%
  mutate(pred=as.numeric(pred),
         se=as.numeric(se)) %>% 
  mutate(across(pred:se,~.*100))

elig2$comp <- factor(elig2$comp, levels=c("Not Competitive","Competitive"))
elig2$elig <- factor(elig2$elig, levels=c("Not Eligible","Eligible"))
elig2$status <- factor(elig2$status,levels=c("Never","Old","New"))



elig2 %>% 
  ggplot(aes(elig,pred,shape=status))+
  geom_point(size=4)  +
  geom_line(aes(group = interaction(status, comp)),linewidth=2) +
  theme_classic(base_size = 46) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 25),
    limits = c(0, 100)
  ) +
  facet_grid(status~comp,scales="free_y") +
  labs(x="",
       y="Predicted Probability (%)") + 
  theme_classic(base_size = 46) +
  theme(legend.position="none")
  

ggsave(plot=last_plot(),filename="output/figures/compelig.png",
       width = 20, height = 20, units = "in")





