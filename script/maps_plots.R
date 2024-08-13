## Making maps

library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
library(gganimate)
library(scico)

par(mfrow=c(1,1))
## get 2010 tract geographies for WI and NM

### WI
wi_tracts <- tracts(state = "WI", cb = T, year = 2010)%>%
  mutate(geoidf = factor(paste0(STATE,COUNTY,TRACT), ordered=F))%>%
  select(STATE, geoidf, geometry)

### NM 
nm_tracts <- tracts(state = "NM", cb = T, year = 2010)%>%
  mutate(geoidf = factor(paste0(STATE,COUNTY,TRACT), ordered=F))%>%
  select(STATE, geoidf, geometry)


plot(nm_tracts)

## incidence rate over time



## incidence rate over time by rurality

### NM
scico(3, palette = "vik")


nm_line <-  nm_tractm%>%
  group_by(date, ruca_cat, geoidf)%>%
  summarize(
    # pop_sum10k = (sum(tract_pop2010)/1e4),
    cases_sum = sum(cases_int),
    rate10k = cases_sum/tract_pop10k)%>%
ggplot(aes(x = date, y = rate10k, group = geoidf, color = ruca_cat))+
  geom_line(linewidth=1, , alpha=0.5)+
  labs(x="", y="Cases per 10,000")+
  scale_x_date(date_labels="%b-%Y", date_breaks="3 month", expand=c(0,0))+
  # scale_color_manual(values = c("#C07348", "#80E6FF", "#4C3D72" ))+
    scale_color_manual(values = c("#CC875F","#A6C9D9","#001260" ))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 24), 
        legend.position = "top",
        axis.text.x = element_text(size = 20, angle = 45,  hjust=1),
        axis.title.y = element_text(size=24),
        legend.text = element_text(size=20),
        legend.title = element_blank())
  
nm_line_t <- nm_line+transition_reveal(date)
anim_save("output/gifs/nm_tractline.gif",nm_line_t)
### WI
scico(3, palette = "vik")

wi_tractm3 <- wisc_tractm2%>%
  mutate(
    ruca_cat = case_when(
      pri_rucaf %in% c("1", "2", "3") ~ "Metropolitan",
      pri_rucaf %in% c("4", "5", "6") ~ "Micropolitan",
      pri_rucaf %in% c("7", "8", "9", "10") ~ "Rural"
    )
  )%>%
  group_by(mon_yr, ruca_cat, geoidf)%>%
  summarize(
    # pop_sum10k = (sum(tract_pop2010)/1e4),
    cases_sum = sum(POS_NEW_CP_sum),
    rate10k = cases_sum/tract_pop10k)


wi_line <-  wisc_tractm2%>%
  mutate(
    ruca_cat = case_when(
      pri_rucaf %in% c("1", "2", "3") ~ "Metropolitan",
      pri_rucaf %in% c("4", "5", "6") ~ "Micropolitan",
      pri_rucaf %in% c("7", "8", "9", "10") ~ "Rural"
    )
  )%>%
  group_by(mon_yr, ruca_cat, geoidf)%>%
  summarize(
    # pop_sum10k = (sum(tract_pop2010)/1e4),
    cases_sum = sum(POS_NEW_CP_sum),
    rate10k = cases_sum/tract_pop10k)%>%
  ggplot(aes(x = mon_yr, y = rate10k, group = geoidf, color = ruca_cat))+
  geom_line(linewidth=1, , alpha=0.5)+
  labs(x="", y="Cases per 10,000")+
  scale_x_date(date_labels="%b-%Y", date_breaks="3 month", expand=c(0,0))+
  # scale_color_manual(values = c("#C07348", "#80E6FF", "#4C3D72" ))+
  scale_color_manual(values = c("#CC875F","#A6C9D9","#001260" ))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 24), 
        legend.position = "top",
        axis.text.x = element_text(size = 20, angle = 45,  hjust=1),
        axis.title.y = element_text(size=24),
        legend.text = element_text(size=20),
        legend.title = element_blank())

wi_line_t <- wi_line+transition_reveal(mon_yr)
anim_save("output/gifs/wi_tractline.gif", wi_line_t)



