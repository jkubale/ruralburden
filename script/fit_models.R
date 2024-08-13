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
load("data/wisc_tractm_08122024.rda") # overall Wisconsin month data
load("data/w_tractm_metro08122024.rda") # Wisconsin month data metro tracts
load("data/w_tractm_micro08122024.rda") # Wisconsin month data micro tracts
load("data/w_tractm_rural08122024.rda") # Wisconsin month data rural tracts

## Fit models----

### Wisconsin----

#### Unstratified county only
tic()
w_county_genp1 <- glmmTMB(POS_NEW_CP_sum ~ pri_rucaf + 
                         bs(month_shift) + 
                         bs(prior_POS_CP) + 
                         (1|countyf), data=wisc_tractm2, family = genpois, REML=T)
toc()
summary(w_county_genp1)
###### log likelihood -195154.3  

#### Unstratified county + tract 
tic()
w_tract_genp1 <- glmmTMB(POS_NEW_CP_sum ~ pri_rucaf + 
                         bs(month_shift) + 
                         bs(prior_POS_CP) + 
                         (1|countyf) + (1|geoidf), data=wisc_tractm2, family = genpois, REML=T)
toc()

summary(w_tract_genp1)

##### Likelihood ratio test of models
dif = dev_conv(w_county_genp1) - dev_conv(w_tract_genp1)
mod_lrt(w_county_genp1, w_tract_genp1) ## <0.0001



#### Stratified models 

##### Metro

## County
tic()
w_countymetro_genp1 <- s_county_mod(w_tractm_metro, w_tractm_metro$POS_NEW_CP_sum, w_tractm_metro$prior_POS_CP)
toc()

summary(w_countymetro_genp1)

## Tract
tic()
w_tractmetro_genp1 <- s_tract_mod(w_tractm_metro$POS_NEW_CP_sum,w_tractm_metro$prior_POS_CP, w_tractm_metro)
toc()

summary(w_tractmetro_genp1)

## LRT comparing models
mod_lrt(w_countymetro_genp1, w_tractmetro_genp1) ## 0.000017

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
w_countyrural_genp1 <- s_county_mod(w_tractm_rural, w_tractm_rural$POS_NEW_CP_sum, w_tractm_rural$prior_POS_CP)
toc()

summary(w_countyrural_genp1)

## County
tic()
w_tractrural_genp1 <- s_tract_mod(POS_NEW_CP_sum, prior_POS_CP, w_tractm_rural)
toc()

summary(w_tractrural_genp1)


## LRT comparing models
mod_lrt(w_countyrural_genp1, w_tractrural_genp1) ## 1

### New Mexico----

#### Unstratified county only
tic()
nm_county_genp1 <- glmmTMB(cases_int ~ pri_rucaf + 
                            bs(month_shift) + 
                            bs(prior_cases) + 
                            (1|countyf), data=nm_tractm, family = genpois, REML=T)
toc()
summary(nm_county_genp1)
###### log likelihood -195154.3  

#### Unstratified county + tract 
tic()
nm_tract_genp1 <- glmmTMB(cases_int ~ pri_rucaf + 
                           bs(month_shift) + 
                           bs(prior_cases) + 
                           (1|countyf) + (1|geoidf), data=nm_tractm, family = genpois, REML=T)
toc()

summary(w_tract_genp1)

##### Likelihood ratio test of models
dif = dev_conv(nm_county_genp1) - dev_conv(nm_tract_genp1)
mod_lrt(nm_county_genp1, nm_tract_genp1) ## 0.0018

#### Stratified models 

##### Metro

## County
tic()
nm_countymetro_genp1 <- s_county_mod(nm_tractm_metro$cases_int, nm_tractm_metro$prior_cases,nm_tractm_metro)
toc()

summary(nm_countymetro_genp2)

## Tract
tic()
nm_tractmetro_genp1 <- s_tract_mod(nm_tractm_metro$cases_int, nm_tractm_metro$prior_cases,nm_tractm_metro)
toc()

summary(nm_tractmetro_genp1)

## LRT comparing models
mod_lrt(nm_countymetro_genp1, nm_tractmetro_genp1) ## 0.000017

##### Micro
## County
tic()
nm_countymicro_genp1 <- s_county_mod(nm_tractm_micro$cases_int, nm_tractm_micro$prior_cases,nm_tractm_micro)
toc()

summary(nm_countymicro_genp1)

## Tract
tic()
nm_tractmicro_genp1 <- s_tract_mod(nm_tractm_micro$cases_int, nm_tractm_micro$prior_cases,nm_tractm_micro)
toc()

summary(nm_tractmicro_genp1)

## LRT comparing models
mod_lrt(nm_countymicro_genp1, nm_tractmicro_genp1) ## 1

##### Rural
## County
tic()
nm_countyrural_genp1 <- s_county_mod(nm_tractm_rural$cases_int, nm_tractm_rural$prior_cases,nm_tractm_rural)
toc()

summary(nm_countyrural_genp1)

## County
tic()
nm_tractrural_genp1 <- s_tract_mod(nm_tractm_rural$cases_int, nm_tractm_rural$prior_cases,nm_tractm_rural)
toc()

summary(nm_tractrural_genp1)


## LRT comparing models
mod_lrt(nm_countyrural_genp1, nm_tractrural_genp1) ## 1
