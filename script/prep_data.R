# Prep data

library(haven)
library(readxl)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

## Call functions 
source("script/rural_functions.r")

## Wisconsin----

### Load data----
#### raw wisconsin COVID data
wisc_raw <- read.csv("data/COVID19-Historical-V2-TRCT.csv")%>%
  filter(GEOID != "TRACT N/A")%>%
  mutate(geoidf = factor(GEOID, ordered = F))

#### import RUCA data for Wisconsin
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


### create tract dataset by month
wisc_tractm <- raw_impshp(wisc_raw, w_vars)

### create additional variables 
#### shift month so 0 isn't reference 
wisc_tractm$month_shift <- wisc_tractm$month - 10

#### create variable for prior month POS_NEW_CP_sum
wisc_tractm <- wisc_tractm%>%
  ungroup()%>%
  group_by(geoidf)%>%
  arrange(month_shift)%>%
  mutate(prior_POS_CP1 = lag(POS_NEW_CP_sum),
         prior_POS_CP = case_when(
           is.na(prior_POS_CP1) ~0,
           T~prior_POS_CP1
         ))%>%
  select(-prior_POS_CP1)

### save month dataset
save(wisc_tractm, file = "data/wisc_tractm_07112024.rda")

### create datasets stratified by RUCA levels
#### 1-3: metropolitan/urban
#### 4-6: micropolitan/suburban
#### 7-10: rural

w_tractm_metro <- filter(wisc_tractm, pri_rucaf %in% c("1","2","3"))

w_tractm_micro <- filter(wisc_tractm, pri_rucaf %in% c("4","5","6"))

w_tractm_rural <- filter(wisc_tractm, pri_rucaf %in% c("7","8","9", "10"))

### save stratified datasets
save(w_tractm_metro, file = "data/w_tractm_metro07112024.rda")
save(w_tractm_micro, file = "data/w_tractm_micro07112024.rda")
save(w_tractm_rural, file = "data/w_tractm_rural07112024.rda")


## Rhode Island----
### Load data----
ri <- read.csv("data/RhodeIslandMonthlyCasesByCensusTract2022(2010Census).csv")%>%
  select(-c("X.blank.", "Grand.Total"))%>%
  ## convert all * to blanks and then to NA by changing to numeric
  mutate(across(contains("X"), ~ as.numeric(ifelse(. %in% c("*",""), NA, .))))%>%
  ## drop "X" from date variable names as those will become values
  rename_with(., ~substr(.,2,8), starts_with("X"))%>%
  pivot_longer(!Census.Tract, names_to = "date", values_to = "cases")

names_chk <- colnames(ri[2:23])%>%
  substr(., 2,8)

## Delaware----

## Louisiana----

## New Mexico----
# Holding off until I can get longitudinal data with 2010 geography
### Load data----

# nm <- read_stata("data/NewMexico_covid_tract20_dates_20231012_subset.dta")%>%
#   zap_label()
# colnames(nm)
# head(nm)
