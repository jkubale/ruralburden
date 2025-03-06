# Analysis pipeline rough steps:
#   • Function for subsetting train/test data to iterate by month
# • Function for fitting model (with 1 month forecast) and saving as .rds
# • Need notification to end session if finishes early
# 
# Could do in batches of 5-10 months on cluster?

load("data/nm_rural_mvgam.rda")
load("data/nm_micro_mvgam.rda")
load("data/nm_metro_mvgam.rda")


table(nm_rural_mvgam$time)
colnames(nm_rural_mvgam)

fit_mvgam <- function(dat, months){
  
df <- dat  
data_train = df[df$time <= months,]
data_test = df[df$time == (months+1),]
  
  mvgam(cases_int ~ s(time, bs = "cr", k=(months - 1)) + s(countyf, bs = "re") + s(geoidf, bs = "re"),
        family = poisson(),
        data = data_train,
        newdata = data_test,
        trend_model = 'AR1',
        noncentred = T,
        silent = 2)
}

fit_mods(nm_rural_mvgam, 5)

for (i in 5:10){
mod <- fit_mvgam(nm_rural_mvgam, i)

saveRDS(mod, )  

}

saveRDS()