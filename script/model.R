# library(lme4)
library(glmmTMB)
library(tictoc)
library(dplyr)
library(splines)
library(Metrics)

load(file = "data/wisc_subruca04022024.rda")

wisc_subruca2 <- wisc_subruca%>%
  filter(pri_ruca !=99)

  
# get number/proportion of each ruca score by county
ruca_num <- wisc_ruca%>%
  ungroup%>%
  filter(pri_ruca !=99)%>%
  group_by(county, pri_ruca)%>%
  summarise(num_ruca = n())%>%
  ungroup()%>%
  group_by(county)%>%
  mutate(tot_tracts = max(cumsum(num_ruca)),
         ruca_prop = num_ruca/tot_tracts)%>%
  select(county, pri_ruca, ruca_prop)

wisc_ruca2 <- wisc_ruca%>%
  filter(pri_ruca !=99)%>%
  left_join(., ruca_num, by = c("county", "pri_ruca"))%>%
  mutate(wt_ruca = pri_ruca*ruca_prop)%>%
  group_by(county)%>%
  summarise(mn_wt_ruca = mean(wt_ruca))%>%
  ungroup()


## explore ways to account for different amount of observations per county -- largely driven by Milwaukee 
county_obs <- wisc_subruca2%>%
  group_by(county)%>%
  summarise(num_obs = n())

summary(county_obs)
summary(wisc_ruca2$log_mn_wt_ruca)
summary(wisc_ruca2$mn_wt_ruca)


wisc_ruca2%>%
  mutate(log_mn_wt_ruca = log(mn_wt_ruca),
         quart_ruca = case_when(
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

test <- anti_join(wisc_ruca2, train, by="county")

# save lists of train/test counties
save(train, file = "data/training_counties.rda")
save(test, file = "data/test_counties.rda")

load("data/training_counties.rda")
load("data/test_counties.rda")

## may need to account for number of observations as well

wi_training_full <- inner_join(train, wisc_subruca2, by="county")%>%
  ungroup()%>%
  filter(tract_pop2010 != 0, is.na(tract_pop2010)==F)%>%
  mutate(pri_rucaf = factor(pri_ruca, ordered=F),
         geoidf = factor(geoid, ordered=F),
         countyf = factor(county, ordered=F),
         week_shift = week-104)%>%
  ungroup()

training_county_pop <- wi_training_full%>%
  select(countyf, geoidf, tract_pop2010)%>%
  distinct(geoidf, .keep_all = T)%>%
  group_by(countyf)%>%
  summarise(county_pop = sum(tract_pop2010))

wi_training_full <- left_join(wi_training_full, training_county_pop, by="countyf")
  

wi_testing_full <- anti_join(wisc_subruca2, train, by="county")%>%
  ungroup()%>%
  filter(tract_pop2010 != 0, is.na(tract_pop2010)==F)%>%
  mutate(pri_rucaf = factor(pri_ruca, ordered=F),
         geoidf = factor(geoid, ordered=F),
         countyf = factor(county, ordered=F),
         week_shift = week-104)%>%
  ungroup()

testing_county_pop <- wi_testing_full%>%
  select(countyf, geoidf, tract_pop2010)%>%
  distinct(geoidf, .keep_all = T)%>%
  group_by(countyf)%>%
  summarise(county_pop = sum(tract_pop2010))

wi_testing_full <- left_join(wi_testing_full, testing_county_pop, by="countyf")

save(wi_training_full, file = "data/wi_train_full.rda")
save(wi_testing_full, file = "data/wi_test_full.rda")


load("data/wi_train_full.rda")
load("data/wi_test_full.rda")

## fit models----
county_only_genp <- glmmTMB(POS_NEW_CP_sum ~ bs(week_shift) + 
                    pri_rucaf + 
                    (1|geoidf) + 
                    (1|countyf) +
                    offset(log(county_pop/1e4)), data=wi_training_full, family = genpois)

county_plus_genp <- glmmTMB(POS_NEW_CP_sum ~ bs(week_shift) + 
                    pri_rucaf + 
                    (1|geoidf) + 
                    (1|countyf) +
                    offset(log(tract_pop2010/1e4)), data=wi_training_full, family = genpois)

## make predictions----

### first make predictions using raw data
wi_testing_full <- wi_testing_full%>%
  ungroup()%>%
  group_by(countyf, week_shift)%>%
  mutate(rate_county = POS_NEW_CP_sum/county_pop)%>%
  ungroup()%>%
  group_by(geoidf, week_shift)%>%
  mutate(rate_tract = POS_NEW_CP_sum/tract_pop2010)%>%
  ungroup()

test_county <- wi_testing_full%>%
  select(week_shift, pri_rucaf, county_pop)
  

test_tract <- wi_testing_full%>%
  select(week_shift, pri_rucaf, tract_pop2010)

pred_county_onlygenp <- county_only_genp%>%
  predict(test_county, re.form = NA, type="response")

data.frame(RMSE = Metrics::rmse(wi_testing_full$rate_county, pred_county_onlygenp),
           R2 = (cor(wi_testing_full$rate_county, pred_county_onlygenp))^2,
           MAE = Metrics::mae(wi_testing_full$rate_county, pred_county_onlygenp))

pred_county_plusgenp <- county_plus_genp%>%
  predict(test_tract, re.form = NA, type="response")

data.frame(RMSE = Metrics::rmse(wi_testing_full$rate_tract, pred_county_plusgenp),
           R2 = (cor(wi_testing_full$rate_tract, pred_county_plusgenp))^2,
           MAE = Metrics::mae(wi_testing_full$rate_tract, pred_county_plusgenp))


### then make predictions by RUCA code



# next steps:
## split dataset
## fit training models

### 
tracts <- unique(wisc_subruca2$geoid)
tract_sample <- sample(tracts, 50)

dat_sub <- wisc_subruca2%>%
  filter(geoid %in% tract_sample)


max_wk <- wisc_subruca2%>%
  ungroup()%>%
  # group_by(week)%>%
  summarise(mx_wk = max(POS_NEW_CP_sum))


dat_sub2 <- dat_sub%>%
  ungroup()%>%
  mutate(week_shift = week-104,
         pri_rucaf = factor(pri_ruca, ordered=F),
         geoidf = factor(geoid, ordered=F),
         countyf = factor(county, ordered=F))

## run with spatial component and no time component 
## select ruca codes for training and testing sets
## model rate using just county
## model rate with county and tract 
## how do these compare to crude rates
## how different are ruca estimates using both or just county

model1a <- glmer.nb(POS_NEW_CP_sum ~ bs(week_shift) + pri_rucaf + (1|geoid) + (1|county), offset = log(tract_pop2010), data=dat_sub2, verbose = T)
summary(model1)
extract_eq(model1)

library(glmmTMB)
model1 <- glmmTMB(POS_NEW_CP_sum ~ bs(week_shift) + 
                     pri_rucaf + 
                     (1|geoid) + 
                     (1|county) +
                     offset(log(tract_pop2010)), data=dat_sub2, family = nbinom2)

model2 <- glmmTMB(POS_NEW_CP_sum ~ bs(week_shift) + 
                    pri_rucaf + 
                    (1|geoid) + 
                    (1|county) +
                    offset(log(tract_pop2010)), data=dat_sub2, family = nbinom1)


performance::check_overdispersion(model1)
performance::check_overdispersion(model2)
summary(model1)
summary(model2)

model2b <- glmer(POS_NEW_CP_sum ~ bs(week_shift)  + (1|pri_ruca2) +  (1|geoid) + (1|county), offset = log(tract_pop2010), data=dat_sub2, family=poisson)
# chk if census tracts in same county can have different ruca
# nesting ruca within county assumes that census tracts within counties that have the same ruca are more similar
summary(model2)
summary(model2a)
summary(model2b)



model1a <- glmer(POS_NEW_CP_sum ~ bs(week_shift) + pri_ruca2 +  (1|geoid) + (1|county) +(1|county:pri_ruca2), offset = log(tract_pop2010), data=dat_sub2, family=poisson)
summary(model1a)


model2 <- glmer(POS_NEW_CP_sum ~ bs(week) + as.factor(pri_ruca) + (1|county), offset = log(tract_pop2010), data=dat_sub, family=poisson)
summary(model2)

model3 <- glmer(POS_NEW_CP_sum ~ bs(week) + as.factor(pri_ruca) + (1|geoid), offset = log(tract_pop2010), data=dat_sub, family=poisson)
summary(model3)
