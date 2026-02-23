source("03_Scripts/Streams/analysis/data for analysis.R")

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
  mutate(
    watershed.inundation=round((AREA*well.depth.m),2),
         day=mdy(date))%>%
  select(-date)

write_csv(wetland_stage, "01_Raw_data/wetland cover/watershed.inundation.csv")



inundation <- int.ext %>%
  mutate(
    day = as.Date(Date)              # make a Date column from Date
  ) %>%
  left_join(
    watershed_inundation %>%
      mutate(day = as.Date(day)),    # or whatever the date column is called
    by = c("day", "Basin"))              # adjust if your keys differ

ggplot(
  inundation%>% filter(NEP_corrected<0),
  aes(x = watershed.inundation/1e9, y = avg.external)) +
  geom_point(color='black') +
  facet_wrap(~ID, scales='free', nrow=1)+
  ylab(expression(CO[2]~'g'/m^2/'day')) +
  xlab(expression('Watershed Inundation'~km^3~('Area')('Well Depth')))+
  ggtitle(expression(CO[2]~'Pathway'~'Responses'~'to'~'Watershed'~'Inundation'))+
  lm.common

inundation%>%
  ggplot(
    aes(x = watershed.inundation/1e9, y = int.ext.ratio)) +
  geom_point() +
  scale_y_log10()+
  facet_wrap(~ID, ncol = 4, scales='free')+
  ylab("Avg Internal/ Avg External") +
  xlab(expression('Watershed Inundation'~km^3~('Area')('Well Depth')))+
  ggtitle(expression(CO[2]~'Pathway'~'Responses'~'to'~'Watershed'~'Inundation'))


id_order <- c("15", "5", "5a", "3", "6", "13", "7", "9")

chem%>%select(Date, ID, Temp_DO)%>%
  filter(Temp_DO>28, !ID %in% c('14', '6a'))%>%
  left_join(inundation)%>%
  mutate(ID = factor(ID, levels = id_order))%>%
  ggplot(
    aes(x=Temp_DO, y = watershed.inundation)) +
  geom_point() +
  facet_wrap(~ID, ncol = 4, scales='free')+
  xlab("Temperature") +
  ylab(expression('Watershed Inundation'~km^3~('Area')('Well Depth')))+
  lm.common


