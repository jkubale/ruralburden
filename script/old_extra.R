# Old/extra code
# wisc <- read_dta("data/Wisconsin_covid_tract_date_subset_20231001.dta")%>%# use to get tract populations
#   zap_label()
# 
# wisc_tract_pop <- wisc%>%
#   select(tract_fips10, tract_pop)%>%
#   mutate(geoidf = factor(tract_fips10, ordered = F))%>%
#   select(geoidf, tract_pop)%>%
#   filter(is.na(tract_pop)==F)


## Simplified analysis (without time)
tract_genp1 <- glmmTMB(POS_NEW_CP_sum ~ pri_rucaf + 
                         bs(week) + 
                         bs(prior_POS_CP) + 
                         (1|geoidf) + 
                         (1|countyf), data=wisc_tractwk_train, family = genpois)

predict(tract_genp1, newdata = new_dat, re.form =  )
class(wisc_tractwk_train$)
library(lme4)
library(dplyr)
library(equatiomatic)

load(file = "data/wisc_subruca04022024.rda")

wisc_subruca2 <- wisc_subruca%>%
  filter(pri_ruca !=99)

colnames(wisc_subruca2)

wisc_ruca_notime <- wisc_subruca2%>%
  ungroup()%>%
  group_by(pri_ruca)%>%
  summarise()
  
# treat county and ruca combinations as strata and do stratified sampling
# look for small cells and combine by neighbor
# weighted avg of ruca in county





county_only <- glmer(POS_NEW_CP_sum ~ bs(week_shift) + as.factor(pri_ruca) + (1|county), offset = log(tract_pop2010), data=dat_sub2, family=poisson)

county_tract <- glmer(POS_NEW_CP_sum ~ bs(week_shift) + as.factor(pri_ruca) + (1|geoid) + (1|county), offset = log(tract_pop2010), data=dat_sub2, family=poisson)