source("03_Scripts/Streams/analysis/data for analysis.R")


wetland_stage <- read_csv("01_Raw_data/wetland cover/wetland stage.csv")%>%
  separate(well_id, "_", into=c("Basin", "wetland"))%>%
  filter(flag==0)%>%
  mutate(day=dmy(date))%>%
  group_by(Basin, day)%>%
  summarise(
    well.depth.m=mean(well_depth_m, na.rm=T))%>%
  left_join(int.ext, by=c('Basin', 'day'))%>%
  drop_na(CO2)

inundation<-wetland_stage%>%
  mutate(inundation=(contrib.wetland.area/basin.area)*well.depth.m)

write_csv(inundation, "01_Raw_data/wetland cover/watershed.inundation.csv")



inundation%>%
ggplot(
  aes(x = watershed.inundation/1e9, y = external)) +
  geom_point(color='black') +
  facet_wrap(~ID, scales='free', nrow=1)+
  ylab(expression(CO[2]~'g'/m^2/'day')) +
  xlab(expression('Watershed Inundation'~km^3~('Area')('Well Depth')))+
  ggtitle(expression(CO[2]~'Pathway'~'Responses'~'to'~'Watershed'~'Inundation'))



inundation%>%
  ggplot(
    aes(x =inundation/1e9, y = int.ext.ratio)) +
  geom_point() +
  scale_y_log10()+
  facet_wrap(~ID, ncol = 4, scales='free')+
  ylab("Avg Internal/ Avg External") +
  xlab(expression('Watershed Inundation'~km^3~('Area')('Well Depth')))+
  ggtitle(expression(CO[2]~'Pathway'~'Responses'~'to'~'Watershed'~'Inundation'))+
  geom_smooth(method='lm')


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


