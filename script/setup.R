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

# Import Wisconsin data
wisc <- read_dta("data/Wisconsin_covid_tract_date_subset_20231001.dta")%>%# use to get tract populations
  zap_label()

wisc_tract_pop <- wisc%>%
  select(tract_fips10, tract_pop)%>%
  mutate(geoidf = factor(tract_fips10, ordered = F))%>%
  select(geoidf, tract_pop)%>%
  filter(is.na(tract_pop)==F)




## will use raw data for now to have control over time periods
wisc_raw <- read.csv("data/COVID19-Historical-V2-TRCT.csv")

## drop "TRACT N/A" for now
wisc_raw2 <- wisc_raw%>%
  filter(GEOID != "TRACT N/A")

## subset data to wanted variables to assess sub county variability (may add more later)
wisc_rawsub_plus <- wisc_raw2%>%
  select(OBJECTID,
         GEOID,
         GEO,
         Date,
         POS_NEW_CONF, 
         POS_NEW_PROB,
         POS_NEW_CP,
         DTH_NEW_CONF,
         DTH_NEW_PROB,
         DTH_NEW_CP
         )%>%
  ## add week variable to summarize
  mutate(date = mdy(Date),
    week = as.numeric(floor(interval(as.Date("2020-01-22"), date)/weeks(1)+1)),
    geoid = as.numeric(GEOID),
    geoidf = factor(geoid, ordered=F))%>%
  ## group by census tract and week and summarize outcomes -- try across to clean up
  group_by(geoidf, week)%>%
  summarise(across(contains("NEW"), sum, .names = "{.col}_sum"))

wisc_rawsub_plus_notime <- wisc_raw2%>%
  select(OBJECTID,
         GEOID,
         GEO,
         Date,
         POS_NEW_CONF, 
         POS_NEW_PROB,
         POS_NEW_CP,
         DTH_NEW_CONF,
         DTH_NEW_PROB,
         DTH_NEW_CP
  )%>%
  ## add week variable to summarize
  mutate(geoid = as.numeric(GEOID),
         geoidf = factor(GEOID, ordered=F))%>%
  ## group by census tract and week and summarize outcomes -- try across to clean up
  group_by(geoidf)%>%
  summarise(across(contains("NEW"), sum, .names = "{.col}_sum"))

## add tract pop to wisc_rawsub
wisc_rawsub_plus2 <- left_join(wisc_rawsub_plus, wisc_tract_pop, by="geoidf") # some NA

wisc_rawsub_plus_notime2 <- left_join(wisc_rawsub_plus_notime, wisc_tract_pop, by="geoidf") # some NA


# chk <- filter(wisc_rawsub2, is.na(tract_pop))
# table(chk$geoid, useNA = "always")

## missing tract pop (or missing from cleaned wisconsin dataset?)
# 55025991702 55025991703 55031990000 55059990000 55071990000 55075990000 55079980000 55083990000 55089990000 
# chk2 <- wisc%>%
#   filter(tract_fips10 %in% c("55025991702",
#                       "55025991703",
#                       "55031990000",
#                       "55059990000",
#                       "55071990000",
#                       "55075990000",
#                       "55079980000",
#                       "55083990000",
#                       "55089990000" ))
## will ask Robert (can ignore for now as not estimating pop rates)

# Import RUCA codes
ruca <- read_excel("data/ruca2010revised.xlsx", skip = 1)
wisc_ruca <- ruca%>%
  filter(`Select State`=="WI")%>%
  mutate(geoidf = factor(`State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)`), ordered=F)%>%
  select(geoidf,
         county = `Select County`,
         pri_ruca = `Primary RUCA Code 2010`,
         sec_ruca = `Secondary RUCA Code, 2010 (see errata)`, 
         tract_pop2010 = `Tract Population, 2010`, 
         tract_area2010 = `Land Area (square miles), 2010`,
         pop_dens2010 = `Population Density (per square mile), 2010`)

# Merge data and RUCA codes----
wisc_subruca <- left_join(wisc_rawsub_plus2, wisc_ruca, by="geoidf")%>%
  mutate(week_shift = week-104,
         countyf = factor(county, ordered=F),
         pri_rucaf = factor(pri_ruca, ordered=F),
         tract_pop10k = tract_pop/1e4)%>%
  select(geoidf, countyf, pri_rucaf, week_shift, POS_NEW_CP_sum, tract_pop, tract_pop10k)%>%
  filter(is.na(tract_pop10k)==F)


save(wisc_subruca, file = "data/wisc_subruca04202024.rda")

wisc_subruca_notime <- left_join(wisc_rawsub_plus_notime2, wisc_ruca, by="geoidf")%>%
  mutate(countyf = factor(county, ordered=F),
         pri_rucaf = factor(pri_ruca, ordered=F),
         tract_pop10k = tract_pop/1e4)%>%
  select(geoidf, countyf, pri_rucaf, POS_NEW_CP_sum, tract_pop, tract_pop10k)%>%
  filter(is.na(tract_pop10k)==F)
  

  # group_by(countyf)%>%
  # mutate(mn_pri_ruca = round(mean(as.numeric(pri_rucaf)), digits = 0))%>%
  # distinct(countyf, .keep_all = T)

save(wisc_subruca_notime, file = "data/wisc_subruca_notime04202024.rda")
load("data/wisc_subruca_notime04202024.rda")

## county only----
wisc_ctruca_notime <- wisc_subruca_notime%>%
  group_by(countyf)%>%
  mutate(mn_pri_ruca = round(mean(as.numeric(pri_rucaf)), digits = 0))


ct_pop_cases <- wisc_ctruca_notime%>%
  group_by(countyf)%>%
  summarise(county_pop10k = (sum(tract_pop)/1e4),
            county_cases = sum(POS_NEW_CP_sum))

ct_ruca <- wisc_ctruca_notime%>%
  group_by(countyf)%>%
  distinct(countyf, .keep_all = T)%>%
  select(countyf, mn_pri_ruca)

wisc_ctonly_notime <- left_join(ct_ruca, ct_pop_cases, by="countyf")

# misc code and data exploration----
# oo <- options(repos = "https://cran.r-project.org/")
# install.packages("Matrix")
# install.packages("lme4")
# options(oo)
# 
# 
# chk <- wisc_raw%>%
#   group_by(GEOID)%>%
#   summarise(n_obs = n())
# 
# chk2 <- wisc%>%
#   group_by(tract_fips10)%>%
#   summarise(n_obs = n())
# 
# chk <- wisc_raw%>%
#   filter(str_detect(str_to_lower(GEOID, "/")))
# 
# colnames(wisc_raw2)
# 
# # c(55025991702,
# #   55025991703,
# #   55031990000,
# #   55059990000,
# #   55071990000,
# #   55075990000,
# #   55079980000,
# #   55083990000,
# #   55089990000 )
