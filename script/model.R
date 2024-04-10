library(lme4)
library(dplyr)


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
  ungroup()%>%
  mutate(log_mn_wt_ruca = log(mn_wt_ruca),
         quart_ruca = case_when(
           log_mn_wt_ruca <= -0.0922 ~ 1,
           log_mn_wt_ruca > -0.0922 & log_mn_wt_ruca <= 0.8120 ~ 2,
           log_mn_wt_ruca > 0.8120 & log_mn_wt_ruca < 1.1781 ~ 3,
           log_mn_wt_ruca > 1.1781 ~ 4
         ))
         # quart_logwtruca = gtools::quantcut(log_mn_wt_ruca))

summary(wisc_ruca2$log_mn_wt_ruca) #used to find quartiles -- split out above code so this can be put between so code flows correctly

train <- wisc_ruca2%>%
  ungroup()%>%
  # group_by(quart_ruca)%>%
  slice_sample(n=54, by=quart_ruca, replace = F) ## something not working right


### 
tracts <- unique(wisc_subruca2$geoid)
tract_sample <- sample(tracts, 150)

dat_sub <- wisc_subruca2%>%
  filter(geoid %in% tract_sample)
library(splines)

max_wk <- wisc_subruca2%>%
  ungroup()%>%
  # group_by(week)%>%
  summarise(mx_wk = max(POS_NEW_CP_sum))


dat_sub2 <- dat_sub%>%
  ungroup()%>%
  mutate(week_shift = week-104,
         pri_ruca2 = as.factor(pri_ruca))

## run with spatial component and no time component 
## select ruca codes for training and testing sets
## model rate using just county
## model rate with county and tract 
## how do these compare to crude rates
## how different are ruca estimates using both or just county

model1 <- glmer(POS_NEW_CP_sum ~ bs(week_shift) + as.factor(pri_ruca) + (1|geoid) + (1|county), offset = log(tract_pop2010), data=dat_sub2, family=poisson)
summary(model1)


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
