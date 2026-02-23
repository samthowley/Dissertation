library(tidyverse)
wetland_cover <- read_csv("01_Raw_data/wetland cover/wetland_cover.csv")%>%
  select(Basin_Name, AREA, PERCENTAGE)%>%
  rename(Basin='Basin_Name')

wetland_stage <- read_csv("01_Raw_data/wetland cover/wetland stage.csv")%>%
  separate(well_id, "_", into=c("Basin", "wetland"))%>%
  filter(flag==0)%>%
  group_by(Basin, date)%>%
  mutate(
    well.depth.m=mean(well_depth_m, na.rm=T),
  )%>%
  select(date, Basin, well.depth.m)%>%
  left_join(wetland_cover, by=c('Basin'))%>%
  mutate(watershed.inundation=(AREA*well.depth.m),
         day=mdy(date))%>% select(-date)

ggplot(
  wetland_stage %>% filter(Basin==13),
  aes(x = date, y = watershed.inundation)) +
  geom_point() +
  facet_wrap(~Basin, scales = "free")+theme_minimal()

write_csv(wetland_stage, "01_Raw_data/wetland cover/watershed.inundation.csv")

