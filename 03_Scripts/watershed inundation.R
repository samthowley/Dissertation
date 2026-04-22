source("03_Scripts/Streams/analysis/data for analysis.R")

basin_area <- read_csv("01_Raw_data/wetland cover/basin_area.csv")%>%
  select(Basin, Shape_Area)%>%
  rename(basin.area=Shape_Area)

totalbasinwetland <- read_csv("01_Raw_data/wetland cover/totalbasinwetland.csv")%>%
  select(Basin, SHAPE_Area)%>%
  mutate(total.wetland.area=SHAPE_Area)

watershed<-left_join(basin_area, totalbasinwetland)%>%
  group_by(Basin)%>%
  summarise(
    total.wetland.area=sum(total.wetland.area),
    basin.area=mean(basin.area, na.rm=T)
  )%>%
  mutate(basin.wetland.perc=total.wetland.area/basin.area)%>%
  rename(ID=Basin)

write_csv(watershed, "01_Raw_data/wetland cover/wetland.perc.csv")


watershed%>%
  ggplot(aes(x=Basin))+
  geom_point(aes(y=total.wetland.area))+
  geom_point(aes(y=basin.area), color='red')
  geom_point()




file.names <- list.files(path="01_Raw_data/contrib wetlands", pattern=".csv", full.names=TRUE)
DO_all<-data.frame()
for(i in file.names){
  DO<-read.csv(i)
  DO$ID<-strsplit(file_path_sans_ext(i), '/')[[1]][3]
  DO_all<-rbind(DO_all, DO)
  DO_all[order(as.Date(DO_all$date, format="%Y-%m-%d %H:%M:%S")),]
}
unique(DO_all$ID)
names(DO_all)

wetland.contrib<-DO_all%>%
  select(ID, Shape_Area_overlay, SHAPE_Area)%>%
  group_by(ID)%>%
  summarise(
    subbasin.area=mean(Shape_Area_overlay, na.rm=T),
    contrib.wetland.area=sum(SHAPE_Area, na.rm=T)
  )%>%
  mutate(contrib.wetland.perc=contrib.wetland.area/ subbasin.area)

wetland.contrib%>%
  ggplot(aes(x=ID))+
  geom_point(aes(y=contrib.wetland.area))+
  geom_point(aes(y=subbasin.area), color='red')



wetland.cover<-left_join(watershed, wetland.contrib)


wetland_stage <- read_csv("01_Raw_data/wetland cover/wetland stage.csv")%>%
  separate(well_id, into = c("ID", "wetland"), sep = "_")%>%
  mutate(Date=mdy(date))%>%
  filter(flag==0, !ID %in% c('14', 'dry', 'wet'))%>%
  group_by(ID, Date)%>%
  summarise(
    well_depth_m=mean(well_depth_m,na.rm=T)
  )%>%
  left_join(wetland.cover)%>%
  drop_na(well_depth_m)%>%
  mutate(
    contrib.wetland.inun=contrib.wetland.perc*well_depth_m,
    total.wetland.inun=basin.wetland.perc*well_depth_m
  )

wetland_stage%>%
  ggplot(aes(x=Date, y=total.wetland.inun))+
  geom_point()+
  facet_wrap(~ID, scales='free')
  
write_csv(wetland_stage, "04_Output/watershed.inundation.csv")

