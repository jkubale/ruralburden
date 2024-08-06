# Fit models

library(glmmTMB)
library(boot)
library(Hmisc)
library(tictoc)
library(dplyr)
library(splines)
# library(Metrics)

## Load datasets----

### Wisconsin
load("data/wisc_tractm_07112024.rda") # overall Wisconsin month data
load("data/w_tractm_metro07112024.rda") # Wisconsin month data metro tracts
load("data/w_tractm_micro07112024.rda") # Wisconsin month data micro tracts
load("data/w_tractm_rural07112024.rda") # Wisconsin month data rural tracts

## Fit models----

### Wisconsin----


#### Unstratified county only
tic()
w_county_genp1 <- glmmTMB(POS_NEW_CP_sum ~ pri_rucaf + 
                         bs(month_shift) + 
                         bs(prior_POS_CP) + 
                         (1|countyf), data=wisc_tractm, family = genpois, REML=T)
toc()
summary(w_county_genp1)
###### log likelihood -195154.3  

#### Unstratified county + tract 
tic()
w_tract_genp1 <- glmmTMB(POS_NEW_CP_sum ~ pri_rucaf + 
                         bs(month_shift) + 
                         bs(prior_POS_CP) + 
                         (1|countyf) + (1|geoidf), data=wisc_tractm, family = genpois, REML=T)
toc()

summary(w_tract_genp1)
###### log likelihood -195148.4  
dev_conv(w_tract_genp1)
dev_dif(w_county_genp1, w_tract_genp1)

##### Likelihood ratio test of models
((-195154.3)*(-2)) - ((-195148.4)*(-2))
(0.5*(1-pchisq(11.7,1))+0.5*(1-pchisq(11.7,2))) ## 0.0017

#### Stratified models county only

##### Metro
tic()
w_countymetro_genp1 <- glmmTMB(POS_NEW_CP_sum ~ pri_rucaf + 
                            bs(month_shift) + 
                            bs(prior_POS_CP) + 
                            (1|countyf), data=w_tractm_metro, family = genpois, REML=T)
toc()
summary(w_countymetro_genp1)

str(w_countymetro_genp1)


##### Micro
tic()
w_countymicro_genp1 <- glmmTMB(POS_NEW_CP_sum ~ pri_rucaf + 
                                 bs(month_shift) + 
                                 bs(prior_POS_CP) + 
                                 (1|countyf), data=w_tractm_micro, family = genpois, REML=T)
toc()
summary(w_countymicro_genp1)

##### Rural
tic()
w_countyrural_genp1 <- glmmTMB(POS_NEW_CP_sum ~ pri_rucaf + 
                                 bs(month_shift) + 
                                 bs(prior_POS_CP) + 
                                 (1|countyf), data=w_tractm_rural, family = genpois, REML=T)
toc()
summary(w_countyrural_genp1)
#### Stratified models county + tract