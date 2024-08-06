# Functions for analysis comparing models using county vs. census tract data

w_vars <- c("OBJECTID", "geoidf", "GEO", "Date", "POS_NEW_CONF", "POS_NEW_PROB", "POS_NEW_CP", "DTH_NEW_CONF", 
            "DTH_NEW_PROB", "DTH_NEW_CP")


raw_impshp <- function(data, ...){
  # v <- c(...)
  data%>%
  select(all_of(...))%>%
   ## add month variable to summarize
  mutate(date = mdy(Date),
         month = as.numeric(floor(interval(as.Date("2020-01-22"), date)/months(1)+1)))%>%
  # ## group by census tract and month and summarize outcomes
  group_by(geoidf, month)%>%
  summarise(across(contains("NEW"), sum, .names = "{.col}_sum"))%>%
    left_join(., wisc_ruca, by="geoidf")%>%
    # left_join(., wisc_tract_pop, by="geoidf")%>%
  filter(pri_rucaf != "99")%>%
    mutate(tract_pop10k = tract_pop2010/1e4)
}


## Extract log-likelihood from model and convert to deviance
dev_conv <- function(model){
  ((logLik(model)[1])*(-2))
}

dev_dif <- function(model1, model2){
  dev_conv(model1) - dev_conv(model2)
}

## county only models

county_mod <- function(dat){
glmmTMB(POS_NEW_CP_sum ~ pri_rucaf + 
          bs(month_shift) + 
          bs(prior_POS_CP) + 
          (1|countyf), data=dat, family = genpois, REML=T)
}

## County + tract models
tract_mod <- function(dat){
  glmmTMB(POS_NEW_CP_sum ~ pri_rucaf + 
            bs(month_shift) + 
            bs(prior_POS_CP) + 
            (1|countyf) + (1|geoidf), data=dat, family = genpois, REML=T)
}
# 
# mean.func <- function(x, index){
#   d <- x[index]
#   return(mean(d))
# }
