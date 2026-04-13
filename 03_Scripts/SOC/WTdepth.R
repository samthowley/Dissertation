library(tidyverse)


# core_id: just a unique identifier I made for each core
# type: denotes who took the core which implies wetland versus upland.
# geom: core’s coordinates EPSG: 26917
# core_z: The elevation for the core. A few cores were beyond my DEM’s area, so they’re NaN.
# core_est_med_wtd: The estimated median water table depth at the core’s location
# cores_est_75p_wtd: The estimated 75th percentile water table depth at the core’s location
# dist_threshold: 1000m or 250m, used to spatially select wells that applied to core.
# n_wells: The number of wells within the distance threshold.
# avg_well_dist: Average distance between wells and the core
# mean_well_z: the average elevation at the wells
# wells_core_z_diff: The elevation difference between wells and the core
# wells_median_wtd: The median water table depth at the well’s location.
# wells_p75_wtd: The 75th percentile water table depth at the well's location.
WTe <- read_csv("01_Raw_data/est_wtd_depth_at_cores 1(in).csv")%>%
  pivot_wider(
    names_from = `dist_threshold`, values_from = `dist_threshold`
  )%>%
  select(core_id, type, core_z, core_est_med_wtd, `250`, `1000`)


WTe1000<-WTe%>%drop_na(`1000`)%>%select(core_est_med_wtd, core_id, type)%>%
  rename(core_est_med_wtd1000=core_est_med_wtd)
WTe250<-WTe%>%drop_na(`250`)%>%select(-`1000`)

wtd.edit<-left_join(WTe250, WTe1000)%>%
  mutate(core_est_med_wtd=if_else(is.na(core_est_med_wtd), core_est_med_wtd1000, core_est_med_wtd))%>%
  select(-core_est_med_wtd1000)%>%
  drop_na(core_est_med_wtd)


soil.cores <- read_csv("04_Output/carbon mapping/LOI.csv")


names(WTe)
unique(WTe$type)
WTe%>%
  filter(type=='Azade_soil_core')%>%
  ggplot(aes(x=core_id, y=core_est_med_wtd, color=dist_threshold))+
  geom_point()
