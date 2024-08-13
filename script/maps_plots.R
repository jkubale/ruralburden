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

nm_tractm_geo <- left_join(nm_tractm, nm_tracts, by="geoidf")%>%
  group_by(date, geoidf)%>%
  mutate(
    # pop_sum10k = (sum(tract_pop2010)/1e4),
    cases_sum = sum(cases_int),
    rate10k = cases_sum/tract_pop10k,
    rate10k2 = case_when(
      rate10k==0 ~ NA,
      T~rate10k
    ))

## maps of covid cases

### NM
chk <- filter(nm_tractm_geo, month==10)

nm_geo <-  ggplot(nm_tractm_geo)+
  geom_sf(aes(geometry=geometry, group=geoidf, fill=rate10k))+
  scale_fill_distiller(
    na.value = "#FFFFCC",
    palette = "YlOrRd",
    direction = 1)+
  labs(fill="COVID-19 Cases per 10,000")+
  theme_void()+
  theme(
    legend.position = "top"
  )


# "#FFFFCC" "#FEB24C" "#F03B20"
nm_geo_time <- nm_geo+transition_time(date)
anim_save("output/gifs/nm_geotime.gif", nm_geo_time)

### WI
load("data/wisc_tractm_08132024.rda")

wi_tractm_geo <- left_join(wisc_tractm2, wi_tracts, by="geoidf")%>%
  group_by(mon_yr, geoidf)%>%
  mutate(
    # pop_sum10k = (sum(tract_pop2010)/1e4),
    cases_sum = sum(POS_NEW_CP_sum),
    rate10k = cases_sum/tract_pop10k
    # rate10k2 = case_when(
    #   rate10k==0 ~ NA,
    #   T~rate10k
    )
chk <- filter(wi_tractm_geo, month==10)

wi_geo <- ggplot(wi_tractm_geo)+
  geom_sf(aes(geometry=geometry, group=geoidf, fill=rate10k))+
  scale_fill_distiller(
    na.value = "#FFFFCC",
    palette = "YlOrRd",
    direction = 1)+
  labs(fill="COVID-19 Cases per 10,000")+
  theme_void()+
  theme(
    legend.position = "top"
  )

wi_geo_time <- wi_geo+transition_time(mon_yr)
anim_save("output/gifs/wi_geotime.gif", wi_geo_time)

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

## save static plot
png(filename = "slides/figures/nm_tractline.png")
nm_line
dev.off()

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

## save static plot
png(filename = "slides/figures/wi_tractline.png")
wi_line
dev.off()

