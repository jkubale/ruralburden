## To do:
# import Wisconsin data
# import RUCA codes
# merge Wisconsin data with RUCA codes
# calculate mean and variance of COVID events over time by county
# calculate mean and variance of COVID events over time by census tract
# calculate 

library(haven)
library(readxl)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)

source("script/rural_functions.r")

vars <- c("OBJECTID", "geoidf", "GEO", "Date", "POS_NEW_CONF", "POS_NEW_PROB", "POS_NEW_CP", "DTH_NEW_CONF", 
          "DTH_NEW_PROB", "DTH_NEW_CP")

# Import Wisconsin data
wisc <- read_dta("data/Wisconsin_covid_tract_date_subset_20231001.dta")%>%# use to get tract populations
  zap_label()

wisc_tract_pop <- wisc%>%
  select(tract_fips10, tract_pop)%>%
  mutate(geoidf = factor(tract_fips10, ordered = F))%>%
  select(geoidf, tract_pop)%>%
  filter(is.na(tract_pop)==F)


## will use raw data for now to have control over time periods
wisc_raw <- read.csv("data/COVID19-Historical-V2-TRCT.csv")%>%
  filter(GEOID != "TRACT N/A")%>%
  mutate(geoidf = factor(GEOID, ordered = F))

## import RUCA data for Wisconsin
wisc_ruca <- read_excel("data/ruca2010revised.xlsx", skip = 1)%>%
  filter(`Select State`=="WI")%>%
  mutate(geoidf = factor(`State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)`), ordered=F,
         countyf = factor(`Select County`, ordered=F),
         pri_ruca = `Primary RUCA Code 2010`,
         pri_rucaf = factor(`Primary RUCA Code 2010`, ordered=F))%>%
  select(geoidf,
         countyf,
         pri_ruca,
         pri_rucaf,
         sec_ruca = `Secondary RUCA Code, 2010 (see errata)`, 
         tract_pop2010 = `Tract Population, 2010`, 
         tract_area2010 = `Land Area (square miles), 2010`,
         pop_dens2010 = `Population Density (per square mile), 2010`)


## create tract dataset by week
wisc_tractwk <- raw_impshp(wisc_raw, vars)

## create county dataset by week
wisc_countywk <- wisc_tractwk%>%
  ungroup()%>%
  group_by(countyf, week)%>%
  summarise(across(contains("NEW"), sum, .names = "{.col}_ct"))
  

# get number/proportion of each ruca score by county
ruca_num <- wisc_ruca%>%
  ungroup%>%
  filter(pri_rucaf !="99")%>%
  group_by(countyf, pri_rucaf)%>%
  summarise(num_ruca = n())%>%
  ungroup()%>%
  group_by(countyf)%>%
  mutate(tot_tracts = max(cumsum(num_ruca)),
         ruca_prop = num_ruca/tot_tracts)%>%
  select(countyf, pri_rucaf, ruca_prop)

wisc_ruca2 <- wisc_ruca%>%
  filter(pri_ruca !=99)%>%
  left_join(., ruca_num, by = c("countyf", "pri_rucaf"))%>%
  mutate(wt_ruca = pri_ruca*ruca_prop)%>%
  group_by(countyf)%>%
  summarise(mn_wt_ruca = mean(wt_ruca))%>%
  mutate(log_mn_wt_ruca = log(mn_wt_ruca))%>%
  ungroup()

summary(wisc_ruca2$log_mn_wt_ruca)
summary(wisc_ruca2$mn_wt_ruca)


wisc_ruca2 <- wisc_ruca2%>%
  mutate(quart_ruca = case_when(
    log_mn_wt_ruca <= -0.0922 ~ 1,
    log_mn_wt_ruca > -0.0922 & log_mn_wt_ruca <= 0.8120 ~ 2,
    log_mn_wt_ruca > 0.8120 & log_mn_wt_ruca < 1.1781 ~ 3,
    log_mn_wt_ruca > 1.1781 ~ 4
  ))

# split counties into training/testing groups (75:25)
set.seed(894894)

train <- wisc_ruca2%>%
  ungroup()%>%
  group_by(quart_ruca)%>%
  slice_sample(prop=.75)

test <- anti_join(wisc_ruca2, train, by="countyf")

wisc_tractwk_train <- inner_join(wisc_tractwk, train, by="countyf")%>%
  ungroup()%>%
  group_by(geoidf)%>%
  arrange(week)%>%
  mutate(prior_POS_CP1 = lag(POS_NEW_CP_sum),
         prior_POS_CP = case_when(
           is.na(prior_POS_CP1) ~0,
           T~prior_POS_CP1
         ))%>%
  select(-prior_POS_CP1)

wisc_countywk_train <- inner_join(wisc_countywk, train, by="countyf")%>%
  ungroup()%>%
  group_by(countyf)%>%
  arrange(week)%>%
  mutate(prior_POS_CP1 = lag(POS_NEW_CP_sum_ct),
         prior_POS_CP = case_when(
           is.na(prior_POS_CP1) ~0,
           T~prior_POS_CP1
         ))%>%
  select(-prior_POS_CP1)

wisc_tractwk_test <- inner_join(wisc_tractwk, test, by="countyf")%>%
  ungroup()%>%
  group_by(geoidf)%>%
  arrange(week)%>%
  mutate(prior_POS_CP1 = lag(POS_NEW_CP_sum),
         prior_POS_CP = case_when(
           is.na(prior_POS_CP1) ~0,
           T~prior_POS_CP1
         ))%>%
  select(-prior_POS_CP1)


wisc_countywk_test <- inner_join(wisc_countywk, test, by="countyf")%>%
  ungroup()%>%
  group_by(countyf)%>%
  arrange(week)%>%
  mutate(prior_POS_CP1 = lag(POS_NEW_CP_sum_ct),
         prior_POS_CP = case_when(
           is.na(prior_POS_CP1) ~0,
           T~prior_POS_CP1
         ))%>%
  select(-prior_POS_CP1)

# save test/train datasets
save(wisc_tractwk_train, file = "data/wisc_tractwk_train05022024.rda")
save(wisc_tractwk_test, file = "data/wisc_tractwk_test05022024.rda")
save(wisc_countywk_train, file = "data/wisc_countywk_train05022024.rda")
save(wisc_countywk_test, file = "data/wisc_countywk_test05022024.rda")


