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
wisc_tractm <- raw_impshp(wisc_raw, w_vars, wisc_ruca)

### create additional variables 
#### shift month so 0 isn't reference 
wisc_tractm2 <- wisc_tractm%>%
  ungroup%>%
  distinct(mon_yr)%>%
  arrange(mon_yr)%>%
  mutate(month = row_number())%>%
  right_join(., wisc_tractm, by="mon_yr")%>%
  mutate(month_shift = month - 10)%>%
  #### create variable for prior month POS_NEW_CP_sum
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
save(wisc_tractm2, file = "data/wisc_tractm_08132024.rda")

### create datasets stratified by RUCA levels
#### 1-3: metropolitan/urban
#### 4-6: micropolitan/suburban
#### 7-10: rural

w_tractm_metro <- filter(wisc_tractm2, pri_rucaf %in% c("1","2","3"))

w_tractm_micro <- filter(wisc_tractm2, pri_rucaf %in% c("4","5","6"))

w_tractm_rural <- filter(wisc_tractm2, pri_rucaf %in% c("7","8","9", "10"))

### save stratified datasets
save(w_tractm_metro, file = "data/w_tractm_metro08132024.rda")
save(w_tractm_micro, file = "data/w_tractm_micro08132024.rda")
save(w_tractm_rural, file = "data/w_tractm_rural08132024.rda")


## Rhode Island----
### Load data----
ri <- read.csv("data/RhodeIslandMonthlyCasesByCensusTract2022(2010Census).csv")%>%
  ## drop unwanted and blank variables
  select(-c("X.blank.", "Grand.Total"))%>%
  rename(tract = Census.Tract)%>%
  ## drop summation rows from bottom
  filter(tract != "Grand Total", tract != "(blank)")%>%
  ## convert all * to blanks and then to NA by changing to numeric
  mutate(across(contains("X"), ~ as.numeric(ifelse(. %in% c("*",""), NA, .))))%>%
  ## drop "X" from date variable names as those will become values
  rename_with(., ~substr(.,2,8), starts_with("X"))%>%
  pivot_longer(!tract, names_to = "date", values_to = "cases")%>%
  mutate(geoidf = factor(tract, ordered=F))
  

ri_ruca <- read_excel("data/ruca2010revised.xlsx", skip = 1)%>%
  filter(`Select State`=="RI")%>%
  mutate(geoidf = factor(`State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)`), ordered=F,
         countyf = factor(`Select County`, ordered=F),
         pri_ruca = `Primary RUCA Code 2010`,
         pri_rucaf = factor(`Primary RUCA Code 2010`, ordered=F))%>%
  select(geoidf,
         countyf,
         pri_ruca,
         pri_rucaf,
         # sec_ruca = `Secondary RUCA Code, 2010 (see errata)`, 
         tract_pop2010 = `Tract Population, 2010`, 
         tract_area2010 = `Land Area (square miles), 2010`,
         pop_dens2010 = `Population Density (per square mile), 2010`)
  

ri_dat <- left_join(ri, ri_ruca, by="geoidf")%>%
  ## drop tracts with missing ruca and/or county
  filter(pri_rucaf != "99")

## save cleaned long data 
save(ri_dat, file = "data/ri_datlong080724.rda")

## Delaware----
### come back to -- extremely messy and some definite inconsistencies
# de_dat <- list()

test <- read.csv("data/DE2021/census-tract-2-covid-19-all-downloadable-data-2021-06-03.csv")%>%
  # filter(Statistic=="New Positive Cases"|Statistic=="Probable Positive Cases", Unit == "people")
  filter(Statistic=="Probable Positive Cases")

## tract FIPS seems to be state + county + tract so for delaware:
## 10 + 001, 003, or 005 + tract code *100 (with leading 0s added for 6 digits)
formatC(anim, width = 6, format = "d", flag = "0")
  
de_ruca <- read_excel("data/ruca2010revised.xlsx", skip = 1)%>%
  filter(`Select State`=="DE")
  mutate(geoidf = factor(`State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)`), ordered=F,
         countyf = factor(`Select County`, ordered=F),
         pri_ruca = `Primary RUCA Code 2010`,
         pri_rucaf = factor(`Primary RUCA Code 2010`, ordered=F))%>%
  select(geoidf,
         countyf,
         pri_ruca,
         pri_rucaf,
         # sec_ruca = `Secondary RUCA Code, 2010 (see errata)`, 
         tract_pop2010 = `Tract Population, 2010`, 
         tract_area2010 = `Land Area (square miles), 2010`,
         pop_dens2010 = `Population Density (per square mile), 2010`)

chk_de <- read_stata("data/Delaware_covid_tract10_dates_20231012_subset.dta")
## Louisiana----

## New Mexico----
# Holding off until I can get longitudinal data with 2010 geography
### Load data----
nm <- read_stata("data/NewMexicoMonthlyCasesByCensusTract2022_date_20231001_nhgis_tr2010_simple_share.dta")%>%
  zap_label()

## get RUCA codes
nm_ruca <- ruca_imp("NM")

nm2 <- nm%>%
  # select(all_of(...))%>%
  # ## add month variable to summarize
  # mutate(date = mdy(Date),
  #        year = year(date),
  #        month = month(date),
  #        mon_yr = make_date(year, month)
  #        # month = as.numeric(floor(interval(as.Date("2020-01-22"), date)/months(1)+1))
  # )%>%
  mutate(geoidf = factor(tract_fips10, ordered=F))%>%
  # ## group by census tract and month and summarize outcomes
  group_by(geoidf, date)%>%
  summarise(cases_sum = sum(cases))%>%
  # summarise(across(contains("NEW"), sum, .names = "{.col}_sum"))%>%
  left_join(., nm_ruca, by="geoidf")%>%
  # left_join(., wisc_tract_pop, by="geoidf")%>%
  filter(pri_rucaf != "99")%>%
  mutate(tract_pop10k = tract_pop2010/1e4)

nm_tractm <- nm2%>%
  ungroup%>%
  distinct(date)%>%
  arrange(date)%>%
  mutate(month = row_number())%>%
  right_join(., nm2, by="date")

nm_tractm$month_shift <- nm_tractm$month - 5

#### create variable for prior month POS_NEW_CP_sum
nm_tractm <- nm_tractm%>%
  ungroup()%>%
  group_by(geoidf)%>%
  arrange(month_shift)%>%
  mutate(cases_int = round(cases_sum,0),
    prior_cases1 = lag(cases_int),
         prior_cases = case_when(
           is.na(prior_cases1) ~0,
           T~prior_cases1
         ),
    cases10k = cases_int/tract_pop10k,
    ruca_cat = case_when(
      pri_rucaf %in% c("1", "2", "3") ~ "Metropolitan",
      pri_rucaf %in% c("4", "5", "6") ~ "Micropolitan",
      pri_rucaf %in% c("7", "8", "9", "10") ~ "Rural"
    ))%>%
  select(-prior_cases1)


nm_tractm_metro <- filter(nm_tractm, pri_rucaf %in% c("1","2","3"))

nm_tractm_micro <- filter(nm_tractm, pri_rucaf %in% c("4","5","6"))

nm_tractm_rural <- filter(nm_tractm, pri_rucaf %in% c("7","8","9", "10"))
