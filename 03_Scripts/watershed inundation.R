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
  mutate(basin.wetland.perc=total.wetland.area/basin.area)




file.names <- list.files(path="01_Raw_data/contrib wetlands", pattern=".csv", full.names=TRUE)
DO_all<-data.frame()
for(i in file.names){
  DO<-read.csv(i)
  DO$ID<-strsplit(file_path_sans_ext(i), '/')[[1]][3]
  DO_all<-rbind(DO_all, DO)
  DO_all[order(as.Date(DO_all$date, format="%Y-%m-%d %H:%M:%S")),]
}

wetland.contrib<-DO_all%>%
group_by(ID)%>%
  summarise(
    contrib.wetland.area=sum(SHAPE_Area),
    subbasin.area=mean(Shape_Area_overlay, na.rm=T)
  )%>%
  mutate(
    Basin=case_when(ID=='5'~'5',ID=='5a'~'5',ID=='15'~'15',
                    ID=='3'~'6',ID=='7'~'7',ID=='6'~'6',ID=='6a'~'6',
                    ID=='9'~'9', ID=='13'~'13'),
    contrib.wetland.perc=contrib.wetland.area/subbasin.area
  ) 


wetland.cover<-left_join(wetland.contrib, watershed)

wetland_stage <- read_csv("01_Raw_data/wetland cover/wetland stage.csv")%>%
  separate(well_id, into = c("ID", "wetland"), sep = "_")%>%
  mutate(Date=mdy(date))%>%
  filter(flag==0)%>%
  group_by(ID, Date)%>%
  summarise(
    well_depth_m=mean(well_depth_m,na.rm=T)
  )%>%
  left_join(wetland.cover)%>%
  drop_na(well_depth_m)%>%
  mutate(
    contrib.wetland.perc=contrib.wetland.perc*well_depth_m,
    total.wetland.perc=basin.wetland.perc*well_depth_m
  )

  
write_csv(wetland_stage, "04_Output/watershed.inundation.csv")

