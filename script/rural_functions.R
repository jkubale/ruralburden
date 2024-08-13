# Functions for analysis comparing models using county vs. census tract data

w_vars <- c("OBJECTID", "geoidf", "GEO", "Date", "POS_NEW_CONF", "POS_NEW_PROB", "POS_NEW_CP", "DTH_NEW_CONF", 
            "DTH_NEW_PROB", "DTH_NEW_CP")

# Import state RUCA codes
ruca_imp <- function(state){
  read_excel("data/ruca2010revised.xlsx", skip = 1)%>%
  filter(`Select State`==state)%>%
  mutate(geoidf = factor(`State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)`), ordered=F,
         countyf = factor(`Select County`, ordered=F),
         pri_ruca = `Primary RUCA Code 2010`,
         pri_rucaf = factor(`Primary RUCA Code 2010`, ordered=F))%>%
  select(geoidf,
         countyf,
         pri_ruca,
         pri_rucaf,
         sec_ruca = `Secondary RUCA Code, 2010 (see errata)`, 
         tract_pop2010 = `Tract Population, 2010`, 
         tract_area2010 = `Land Area (square miles), 2010`,
         pop_dens2010 = `Population Density (per square mile), 2010`)
}

raw_impshp <- function(data, vars, s_ruca){

  data%>%
  select(all_of(vars))%>%
   ## add month variable to summarize
  mutate(date = mdy(Date),
         year = year(date),
         month = month(date),
         mon_yr = make_date(year, month)

         )%>%
  # ## group by census tract and month and summarize outcomes
  group_by(geoidf, mon_yr)%>%
  summarise(across(contains("NEW"), sum, .names = "{.col}_sum"))%>%
  left_join(., s_ruca, by="geoidf")%>%
    filter(pri_rucaf != "99", tract_pop2010 >0)%>%
      mutate(tract_pop10k = tract_pop2010/1e4)

  
}

## Load and merge many DE csv files
# lc_csvs <- function(){
#   
#   
# }

## Extract log-likelihood from model and convert to deviance
dev_conv <- function(model){
  ((logLik(model)[1])*(-2))
}

## calculate difference and conduct modified likelihood ratio test
mod_lrt <- function(model1, model2){
  dif = dev_conv(model1) - dev_conv(model2)
  (0.5*(1-pchisq(dif,1))+0.5*(1-pchisq(dif,2)))
}

## county only models

county_mod <- function(outcome, prior_m, dat){
glmmTMB(outcome ~ pri_rucaf + 
          bs(month_shift) + 
          bs(prior_m) + 
          (1|countyf), data=dat, family = genpois, REML=T)
}

## County + tract models
tract_mod <- function(dat){
  glmmTMB(POS_NEW_CP_sum ~ pri_rucaf + 
            bs(month_shift) + 
            bs(prior_POS_CP) + 
            (1|countyf) + (1|geoidf), data=dat, family = genpois, REML=T)
}

## county only - no ruca
s_county_mod <- function(dat, outcome, prior_m){
  glmmTMB({{outcome}} ~  
            bs(month_shift) + 
            bs({{prior_m}}) + 
            (1|countyf), data=dat, family = genpois, REML=T)
}
## tract + county - no ruca
s_tract_mod <- function(outcome, prior_m, dat){
  glmmTMB({{outcome}} ~  
            bs(month_shift) + 
            bs({{prior_m}}) + 
            (1|countyf) + (1|geoidf), data=dat, family = genpois, REML=T)
}
# 
# mean.func <- function(x, index){
#   d <- x[index]
#   return(mean(d))
# }
