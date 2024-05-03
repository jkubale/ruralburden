# library(lme4)
library(glmmTMB)
# library(mgcv)
library(tictoc)
library(dplyr)
library(splines)
library(Metrics)

# load data
load("data/wisc_tractwk_train05022024.rda")
load("data/wisc_countywk_train05022024.rda")

# Poisson gam -- come back to, currently takes way too long to run
# tract_gamp1 <- gam(POS_NEW_CP_sum ~ pri_rucaf + s(prior_POS_CP) + s(week) + s(geoidf, bs="re"), method="REML", data = wisc_tractwk_train, family = "poisson")

# generalized poisson model
wisc_tractwk_train$week_shift <- wisc_tractwk_train$week - 104
wisc_countywk_train$week_shift <- wisc_countywk_train$week - 104
wisc_countywk_train$mn_wt_ruca_rnd <- factor(round(wisc_countywk_train$mn_wt_ruca,0),ordered=F)

tic()
tract_genp1 <- glmmTMB(POS_NEW_CP_sum ~ pri_rucaf + 
          bs(week) + 
          bs(prior_POS_CP) + 
          (1|geoidf) + 
          (1|countyf), data=wisc_tractwk_train, family = genpois)
toc()

## need to double check county sums of confirmed and probable cases
tic()
county_genp1 <- glmmTMB(POS_NEW_CP_sum_ct ~ mn_wt_ruca_rnd + 
                         ns(week) + 
                         ns(prior_POS_CP) + 
                         (1|countyf), data=wisc_countywk_train, family = genpois)
toc()

saveRDS(wisc_tractwk_train, file = "data/wisc_tractwk_train05032024.rds")
saveRDS(wisc_countywk_train, file = "data/wisc_countywk_train05032024.rds")

summary(tract_genp1)
summary(county_genp1)

## make predictions----
load("data/wisc_tractwk_test05022024.rda")
load("data/wisc_countywk_test05022024.rda")

wisc_tractwk_test$week_shift <- wisc_tractwk_test$week - 104
wisc_countywk_test$week_shift <- wisc_countywk_test$week - 104
wisc_countywk_test$mn_wt_ruca_rnd <- factor(round(wisc_countywk_test$mn_wt_ruca,0),ordered=F)

tract_test <- wisc_tractwk_test%>%
  ungroup()%>%
  select(pri_rucaf, week, prior_POS_CP)
  # select(pri_rucaf, week_shift, prior_POS_CP)

county_test <- wisc_countywk_test%>%
  ungroup()%>%
  select(mn_wt_ruca_rnd, week, prior_POS_CP)
  # select(mn_wt_ruca_rnd, week_shift, prior_POS_CP)

tract_pred <- data.frame(pred = predict(tract_genp1, newdata = tract_test, re.form = NA, type = "response"))%>%
  bind_cols(., wisc_tractwk_test)
summary(tract_pred)

data.frame(RMSE = Metrics::rmse(wisc_tractwk_test$POS_NEW_CP_sum, tract_pred$pred),
           R2 = (cor(wisc_tractwk_test$POS_NEW_CP_sum, tract_pred$pred))^2,
           MAE = Metrics::mae(wisc_tractwk_test$POS_NEW_CP_sum, tract_pred$pred))

county_pred <- data.frame(pred = predict(county_genp1, newdata = county_test, re.form = NA, type = "response"))%>%
  bind_cols(., wisc_countywk_test)%>%
  mutate(pred2 = case_when(
    pred <0 ~0,
    pred >3100 ~ 3100,
    T ~ pred
  )) # issues are in milwaukee county
summary(county_pred)



data.frame(RMSE = Metrics::rmse(wisc_countywk_test$POS_NEW_CP_sum_ct, county_pred$pred2),
           R2 = (cor(wisc_countywk_test$POS_NEW_CP_sum_ct, county_pred$pred2))^2,
           MAE = Metrics::mae(wisc_countywk_test$POS_NEW_CP_sum_ct, county_pred$pred2))
table(wisc_countywk_train$mn_wt_ruca_rnd, useNA = "always")
table(wisc_countywk_test$mn_wt_ruca_rnd, useNA = "always")




## Start looking at predictions vs. data (training) to tweak models if possible
# train_tract <- filter(wisc_tractwk_train, geoidf=="55009940001")
train_tract_fit <- data.frame(fit = predict(tract_genp1, re.form = NA, type="response"))%>%
  bind_cols(., wisc_tractwk_train)%>%
  filter(geoidf=="55009940001")
plot(x=train_tract_fit$week, y=train_tract_fit$POS_NEW_CP_sum)
lines(x=train_tract_fit$week, y=train_tract_fit$fit, type = "l")

# train_county <- filter(wisc_countywk_train, countyf=="Adams County")
train_county_fit <- data.frame(fit = predict(county_genp1, re.form = NA, type="response"))%>%
  bind_cols(., wisc_countywk_train)%>%
  filter(countyf=="Dane County")
plot(x=train_county_fit$week, y=train_county_fit$POS_NEW_CP_sum_ct)
lines(x=train_county_fit$week, y=train_county_fit$fit, type = "l")

attr(bs(wisc_countywk_train$POS_NEW_CP_sum_ct), "knots")
county_genp1$modelInfo$