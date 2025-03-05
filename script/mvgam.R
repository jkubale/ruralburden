library(mvgam)
library(dplyr)

colnames(nm_tractm_rural)

nm_metro_sub <- select(nm_tractm_metro, 
                       cases_int, 
                       time = month,
                       countyf,
                       geoidf = geoidf)%>%
  mutate(countyf = droplevels(countyf),
         geoidf = droplevels(geoidf))
  
## training data
data_train <- filter(nm_metro_sub)%>%
  ungroup()%>%
  mutate(series = factor("Metro", ordered=F))

data_train2 <- data_train%>%
  select(-countyf)

# data_train2 <- data_train %>%
#   right_join(expand.grid(time = seq(min(data_train$time),
#                                     max(data_train$time)),
#                          countyf = factor(unique(data_train$countyf), levels = levels(data_train$countyf)),
#                          geoidf = factor(unique(data_train$geoidf), levels = levels(data_train$geoidf)),
#                          series = factor("Rural", ordered=F)))

## pull out county and tract and add back in after expand.grid and merge to avoid missingness
county_tract <- data_train%>%
  select(countyf, geoidf)%>%
  distinct()%>%
  mutate(num = row_number())


# data_test <- county_tract%>%
#   select(-num)
  


train_grid <- expand.grid(time = c(1:28),
                        geoidf = factor(unique(data_train$geoidf), levels = levels(data_train$geoidf)),
                        series = factor("Metro"))

data_train3 <- right_join(data_train2, train_grid, by = c("time", "geoidf", "series"="series"))%>%
  arrange( geoidf, time)%>%
  left_join(county_tract, by="geoidf")%>%
  mutate(geoidf = droplevels(geoidf),
         countyf = droplevels(countyf),
         series = factor(paste0("Metro", "_", num)))

nm_rural_mvgam <- data_train3
nm_micro_mvgam <- data_train3
nm_metro_mvgam <- data_train3

save(nm_rural_mvgam, file = "data/nm_rural_mvgam.rda")
save(nm_micro_mvgam, file = "data/nm_micro_mvgam.rda")
save(nm_metro_mvgam, file = "data/nm_metro_mvgam.rda")


load("data/nm_rural_mvgam.rda")

# Testing data
series <- data_train3%>%
  select(geoidf, series)%>%
  distinct()

county_tract_test <- expand.grid(time = c(10:14),
                                 geoidf = county_tract$geoidf)%>%
  left_join(county_tract, by="geoidf")%>%
  select(-num)%>%
  arrange(geoidf, time)

data_test <- filter(nm_rural_sub, time %in% c(10:14))%>%
  ungroup()%>%
  select(-countyf)%>%
  right_join(county_tract_test, by = c("geoidf", "time"))%>%
  left_join(series, by = "geoidf")%>%
  arrange(geoidf, time)

## prior/data chk below works finally!
get_mvgam_priors(cases_int ~ 1,
                 data=data_train3,
                 family = poisson(),
)

get_mvgam_priors(cases_int ~ 1,
                 data=data_test,
                 family = poisson(),
)
  
data_train <- data_train3[,-6]

save(data_train, file = "data/nm_rurpred10_14train.rda")
save(data_test, file = "data/nm_rurpred10_14test.rda")



mod1 <- mvgam(cases_int ~ s(time, bs = "cr", k=8) + s(countyf, bs = "re") + s(geoidf, bs = "re"),
              family = poisson(),
              data = data_train,
              newdata = data_test,
              trend_model = 'AR1',
              noncentred = T,
              silent = 2)


# 
# data_test <- filter(nm_rural_sub, time ==5)%>%mutate(series = factor("Rural", ordered=F),
#                                                      geoid = as.character(geoidf))
# 
# data_train2 <- select(data_train, 
#                       cases_int,
#                       time,
#                       series = as.factor(geoid))

# plot_mvgam_series(data = data_train2, series = 1, y = 'cases1')



mod1 <- mvgam(cases_int ~ s(geoid, bs = "re")-1,
                family = poisson(),
                data = data_train,
                newdata = data_test,
                trend_model = 'AR1')



get_mvgam_priors(as.numeric(cases_int) ~ 1,
                 data=data_train2,
                 family = poisson(),
                 )

glimpse(data_train2)

## issue appears to be missing time points for various census tracts

###############
# See if I can get it to work with package data

data("portal_data")

portal_data %>%
  
  # mvgam requires a 'time' variable be present in the data to index
  # the temporal observations. This is especially important when tracking 
  # multiple time series. In the Portal data, the 'moon' variable indexes the
  # lunar monthly timestep of the trapping sessions
  dplyr::mutate(time = moon - (min(moon)) + 1) %>%
  
  # We can also provide a more informative name for the outcome variable, which 
  # is counts of the 'PP' species (Chaetodipus penicillatus) across all control
  # plots
  dplyr::mutate(count = PP) %>%
  
  # The other requirement for mvgam is a 'series' variable, which needs to be a
  # factor variable to index which time series each row in the data belongs to.
  # Again, this is more useful when you have multiple time series in the data
  dplyr::mutate(series = as.factor('PP')) %>%
  
  # Select the variables of interest to keep in the model_data
  dplyr::select(series, year, time, count, mintemp, ndvi)%>%
  mutate(time = as.numeric(time)) -> model_data

plot_mvgam_series(data = model_data, series = 1, y = 'count')
get_mvgam_priors(count~ 1,
                 data=model_data,
                 family = poisson(),
)
glimpse(model_data)

model_data %>% 
  dplyr::filter(time <= 160) -> data_train 
model_data %>% 
  dplyr::filter(time > 160) -> data_test

model3 <- mvgam(count ~ s(time, bs = 'bs', k = 15) + 
                  ndvi,
                family = poisson(),
                data = data_train,
                newdata = data_test)
