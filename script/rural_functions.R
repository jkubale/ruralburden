# Functions for analysis comparing models using county vs. census tract data

raw_impshp <- function(data, ...){
  # v <- c(...)
  data%>%
  select(all_of(...))%>%
   ## add week variable to summarize
  mutate(date = mdy(Date),
         week = as.numeric(floor(interval(as.Date("2020-01-22"), date)/weeks(1)+1)))%>%
  # ## group by census tract and week and summarize outcomes -- try across to clean up
  group_by(geoidf, week)%>%
  summarise(across(contains("NEW"), sum, .names = "{.col}_sum"))%>%
    left_join(., wisc_ruca, by="geoidf")%>%
    left_join(., wisc_tract_pop, by="geoidf")%>%
  filter(pri_rucaf != "99")%>%
    mutate(tract_pop10k = tract_pop/1e4)
}

