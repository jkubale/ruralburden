library(lme4)
library(dplyr)


load(file = "data/wisc_subruca04022024.rda")

wisc_subruca2 <- wisc_subruca%>%
  filter(pri_ruca !=99)

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
