# library(lme4)
library(glmmTMB)
library(tictoc)
library(dplyr)
library(splines)
library(Metrics)

# load(file = "data/wisc_subruca04022024.rda")
load(file = "data/wisc_subruca04202024.rda")
# load("data/wisc_subruca_notime04202024.rda")
# wisc_subruca2 <- wisc_subruca%>%
#   filter(as.numeric(pri_rucaf) !=99)
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
  mutate(log_mn_wt_ruca = log(mn_wt_ruca))%>%
  ungroup()


## explore ways to account for different amount of observations per county -- largely driven by Milwaukee 
county_obs <- wisc_subruca%>%
  group_by(countyf)%>%
  summarise(num_obs = n())

summary(county_obs)
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
  slice_sample(prop=.75)%>%
  mutate(countyf= factor(county, ordered=F))

test <- anti_join(wisc_ruca2, train, by="county")%>%
  mutate(countyf= factor(county, ordered=F))

# save lists of train/test counties
save(train, file = "data/training_counties.rda")
save(test, file = "data/test_counties.rda")

load("data/training_counties.rda")
load("data/test_counties.rda")


## may need to account for number of observations as well
## no time
wi_training_tract_notime <- inner_join(train, wisc_subruca_notime, by="countyf")%>%
  ungroup()%>%
  filter(tract_pop10k != 0, is.na(tract_pop10k)==F, pri_rucaf !=99)%>%
  ungroup()%>%
  mutate(ln_tract_pop10k = log(tract_pop10k))

# training_county_pop <- wi_training_tract_notime%>%
#   select(countyf, geoidf, tract_pop)%>%
#   distinct(geoidf, .keep_all = T)%>%
#   group_by(countyf)%>%
#   summarise(county_pop = sum(tract_pop))%>%
#   ungroup()%>%
#   mutate(county_pop10k = county_pop/1e4)

wi_testing_tract_notime <- inner_join(test, wisc_subruca_notime, by="countyf")%>%
  ungroup()%>%
  filter(tract_pop10k != 0, is.na(tract_pop10k)==F, pri_rucaf !=99)%>%
  ungroup()%>%
  ungroup()%>%
  mutate(ln_tract_pop10k = log(tract_pop10k),
         tract_rate = POS_NEW_CP_sum/tract_pop10k)

# training_county_pop <- wi_training_tract_notime%>%
#   select(countyf, geoidf, tract_pop)%>%
#   distinct(geoidf, .keep_all = T)%>%
#   group_by(countyf)%>%
#   summarise(county_pop = sum(tract_pop))%>%
#   ungroup()%>%
#   mutate(county_pop10k = county_pop/1e4)

wi_training_ct_notime <- inner_join(train, ct_pop_cases, by="countyf")%>%
  ungroup()%>%
  filter(county_pop10k != 0, is.na(county_pop10k)==F)%>%
  left_join(., ct_ruca, by="countyf")

wi_testing_ct_notime <- inner_join(test, ct_pop_cases, by="countyf")%>%
  ungroup()%>%
  filter(county_pop10k != 0, is.na(county_pop10k)==F)%>%
  left_join(., ct_ruca, by="countyf")%>%
  ungroup()%>%
  mutate(county_rate = county_cases/county_pop10k)

## save notime train and test datasets
save(wi_training_ct_notime, file = "data/wi_training_ct_notime042024.rda")
save(wi_testing_ct_notime, file = "data/wi_testing_ct_notime042024.rda")

save(wi_training_tract_notime, file = "data/wi_training_tract_notime042024.rda")
save(wi_testing_tract_notime, file = "data/wi_testing_tract_notime042024.rda")

## fit no time models on training data----
tic()
county_notime <- glmmTMB(county_cases ~ as.factor(mn_pri_ruca) + 
                         (1|countyf), 
                         data=wi_training_ct_notime, family = genpois)
toc()
summary(county_notime)

tic()
tract_notime <- glmmTMB(POS_NEW_CP_sum ~ pri_rucaf + 
                          (1|geoidf)  + 
                          (1|countyf), 
                          data=wi_training_tract_notime, family = genpois)

toc()
summary(tract_notime)

### with subsets of RUCA codes
wi_training_ct_notime <- wi_training_ct_notime%>%
  ungroup()%>%
  mutate(ruca_cat = factor(case_when(
    mn_pri_ruca %in% c(1:3) ~ "Metro",
    mn_pri_ruca %in% c(4:6) ~ "Micro",
    mn_pri_ruca %in% c(7:9) ~ "Smalltown",
    mn_pri_ruca == 10 ~ "Rural"
  ), ordered=F))

wi_training_tract_notime <- wi_training_tract_notime%>%
  ungroup()%>%
  mutate(ruca_cat = factor(case_when(
    pri_rucaf %in% c(1:3) ~ "Metro",
    pri_rucaf %in% c(4:6) ~ "Micro",
    pri_rucaf %in% c(7:9) ~ "Smalltown",
    pri_rucaf == 10 ~ "Rural"
  ), ordered=F))

table(wi_training_tract_notime$ruca_cat, useNA = "always")

ct_metro <- filter(wi_training_ct_notime, ruca_cat=="Metro")
tract_metro <- filter(wi_training_tract_notime, ruca_cat=="Metro")

ct_smalltown <- filter(wi_training_ct_notime, ruca_cat=="Smalltown")
tract_smalltown <- filter(wi_training_tract_notime, ruca_cat=="Smalltown")

## metro metro
tic()
county_notime <- glmmTMB(county_cases ~ 
                           (1|countyf), 
                         data=ct_metro, family = genpois)
toc()
summary(county_notime)

tic()
tract_notime <- glmmTMB(POS_NEW_CP_sum ~  
                          (1|geoidf)  + 
                          (1|countyf), 
                        data=tract_metro, family = genpois)

toc()
summary(tract_notime)


## small town: small town
tic()
county_notime <- glmmTMB(county_cases ~ 
                           (1|countyf), 
                         data=ct_smalltown, family = genpois)
toc()
summary(county_notime)

tic()
tract_notime <- glmmTMB(POS_NEW_CP_sum ~  
                          (1|geoidf)  + 
                          (1|countyf), 
                        data=tract_smalltown, family = genpois)

toc()
summary(tract_notime)

# wi_training_full <- left_join(wi_training_full, training_county_pop, by="countyf")

county_testdat <- wi_testing_ct_notime%>%
  select(mn_pri_ruca, county_pop10k)

tract_testdat <- wi_testing_tract_notime%>%
  select(pri_rucaf, tract_pop10k)

pred_county_genp <- county_notime%>%
  predict(county_testdat, re.form = NA)
  bind_cols(., county_testdat)%>%
  rename(county_rate =`...1`,
         pri_rucaf = mn_pri_ruca)%>%
  group_by(pri_rucaf)%>%
  summarise(mn_rate = mean(county_rate))

data.frame(RMSE = Metrics::rmse(wi_testing_ct_notime$county_rate, pred_county_genp),
           R2 = (cor(wi_testing_ct_notime$county_rate, pred_county_genp))^2,
           MAE = Metrics::mae(wi_testing_ct_notime$county_rate, pred_county_genp))

tract_testdat <- expand.grid(pri_rucaf = levels(wi_testing_tract_notime$pri_rucaf),
                                        ln_tract_pop10k = wi_training_tract_notime$ln_tract_pop10k)
  filter(pri_rucaf !=99, is.na(pri_rucaf)==F)

ruca_lev <- data.frame(pri_rucaf = levels(droplevels(wi_testing_tract_notime$pri_rucaf)))
ruca_lev <- data.frame(pri_rucaf = wi_testing_tract_notime$pri_rucaf)

ruca_lev2 <-   data.frame(mn_pri_ruca = levels(droplevels(factor(wi_testing_ct_notime$mn_pri_ruca))))
ruca_lev2 <-  data.frame(mn_pri_ruca = wi_testing_ct_notime$mn_pri_ruca)


predictions_tract <- data.frame(prediction = predict(tract_notime, newdata=ruca_lev, re.form=NA, type="response"))%>%
  bind_cols(., wi_testing_tract_notime)%>%
  group_by(countyf)%>%
  summarise(ct_pred = sum(prediction))

predictions_county <- data.frame(prediction_county = predict(county_notime, newdata=ruca_lev2, re.form=NA, type="response"))%>%
  bind_cols(., wi_testing_ct_notime)%>%
  select(countyf, prediction_county)
  

both_preds <- left_join(predictions_tract, predictions_county, by="countyf")
  
obs_preds <- data.frame(countyf = wi_testing_ct_notime$countyf, wi_testing_ct_notime$county_rate)
  
all_preds <- left_join(both_preds, obs_preds, by="countyf")

  expand.grid(
    pri_rucaf = levels(droplevels(wi_training_tract_notime$pri_rucaf)),
    ln_tract_pop10k = wi_training_tract_notime$ln_tract_pop10k
  )%>%
  mutate(predictions = predict(tract_notime, newdata=., re.form=NA, type="response"))%>%
  group_by(pri_rucaf)%>%
  summarise(mn_pred = mean(predictions))
  
pred_tract_genp <- tract_notime%>%
  predict(tract_notime, newdata = tract_testdat, re.form = NA, type="response")
  group_by(pri_rucaf)%>%
  summarise(mn_pred = mean(prediction))
  

expand.grid(
  age        = levels(insurance$age), 
  ln_holders = insurance$ln_holders
) %>% 
  mutate(prediction = predict(nb_glm_offset, newdata = ., type = 'response')) %>% 
  group_by(age) %>% 
  summarise(avg_prediction = mean(prediction)) 
  
  predict(tract_testdat)%>%
  bind_cols(., tract_testdat)%>%
  rename(tract_rate =`...1`)%>%
  group_by(pri_rucaf)%>%
  summarise(mn_rate = mean(tract_rate))

data.frame(RMSE = Metrics::rmse(wi_testing_tract_notime$tract_rate, pred_tract_genp),
           R2 = (cor(wi_testing_tract_notime$tract_rate, pred_tract_genp))^2,
           MAE = Metrics::mae(wi_testing_tract_notime$tract_rate, pred_tract_genp))




#######################################################
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

wi_training_full <- wi_training_full%>%
  ungroup()%>%
  mutate(county_pop10 = county_pop/1e4,
         tract_pop2010_10 = tract_pop2010/1e4)


## fit models witout time----
wi_train_notime <- wi_testing_full%>%
  

## fit models with time----
tic()
county_only_genp <- glmmTMB(POS_NEW_CP_sum ~ bs(week_shift) + 
                    pri_rucaf + 
                    (1|geoidf) + 
                    (1|countyf) +
                    offset(log(county_pop10)), data=wi_training_full, family = genpois)
toc()

tic()
county_plus_genp <- glmmTMB(POS_NEW_CP_sum ~ bs(week_shift) + 
                    pri_rucaf + 
                    (1|geoidf) + 
                    (1|countyf) +
                    offset(log(tract_pop2010_10)), data=wi_training_full, family = genpois)
toc()

## make predictions----

### first make predictions using raw data
wi_testing_full <- wi_testing_full%>%
  ungroup()%>%
  group_by(countyf, week_shift)%>%
  mutate(county_pop10 = county_pop/1e4,
         tract_pop2010_10 = tract_pop2010/1e4,
    rate_county = POS_NEW_CP_sum/county_pop10)%>%
  ungroup()%>%
  group_by(geoidf, week_shift)%>%
  mutate(rate_tract = POS_NEW_CP_sum/tract_pop2010_10)%>%
  ungroup()

test_county <- wi_testing_full%>%
  select(week_shift, pri_rucaf, county_pop10)
  

test_tract <- wi_testing_full%>%
  select(week_shift, pri_rucaf, tract_pop2010_10)

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


ruca_pop <- wi_testing_full%>%
  ungroup()%>%
  distinct(geoidf, .keep_all = T)%>%
  group_by(pri_rucaf)%>%
  summarise(ruca_pop = sum(tract_pop2010_10))

test_county_ruca <- wi_testing_full%>%
  ungroup()%>%
  group_by(pri_rucaf)%>%
  summarise(posCP_ruca = sum(POS_NEW_CP_sum))%>%
  left_join(., ruca_pop, by="pri_rucaf")%>%
  mutate(ruca_rate = posCP_ruca/ruca_pop)

pred_county_onlygenp <- county_only_genp%>%
  predict(data.frame(week_shift = rep(c(-103:36),10), 
                     pri_rucaf = rep(c(1:10),)), re.form = NA, type="response")


test_county_plus_ruca <- 


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
