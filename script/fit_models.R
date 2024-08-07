# Fit models

library(glmmTMB)
# library(boot)
# library(Hmisc)
library(tictoc)
library(dplyr)
library(splines)
# library(Metrics)

## Call functions 
source("script/rural_functions.r")

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

##### Likelihood ratio test of models
mod_lrt(w_county_genp1, w_tract_genp1) ## 0.0018


#### Stratified models 

##### Metro

## County
tic()
w_countymetro_genp1 <- s_county_mod(w_tractm_metro)
toc()

summary(w_countymetro_genp2)

## Tract
tic()
w_tractmetro_genp1 <- s_tract_mod(w_tractm_metro)
toc()

summary(w_tractmetro_genp1)

## LRT comparing models
mod_lrt(w_countymetro_genp2, w_tractmetro_genp1) ## 0.000017

##### Micro
## County
tic()
w_countymicro_genp1 <- s_county_mod(w_tractm_micro)
toc()

summary(w_countymicro_genp1)

## Tract
tic()
w_tractmicro_genp1 <- s_tract_mod(w_tractm_micro)
toc()

summary(w_tractmicro_genp1)

## LRT comparing models
mod_lrt(w_countymicro_genp1, w_tractmicro_genp1) ## 1

##### Rural
## County
tic()
w_countyrural_genp1 <- s_county_mod(w_tractm_rural)
toc()

summary(w_countyrural_genp1)

## County
tic()
w_tractrural_genp1 <- s_tract_mod(w_tractm_rural)
toc()

summary(w_tractrural_genp1)


## LRT comparing models
mod_lrt(w_countyrural_genp1, w_tractrural_genp1) ## 1

