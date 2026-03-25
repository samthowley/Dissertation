library(tidyverse)
library(readxl)
library(cowplot)
library(measurements)
library(plotly)

#Faith#########
Bradford_coords_all <- read_excel("04_Output/RC/carbon mapping/Bradford_coords_all.xlsx")%>%
  select(-Type, -`Sensors present`)



Bradford_Data_Faith <- read_csv("04_Output/RC/carbon mapping/Bradford Data_Faith.csv")%>%
  rename(Site_ID="Wetland Name")%>%
  left_join(Bradford_coords_all, by='Site_ID')%>%
  select(Site_ID, Lat, Long, `Soil sample depth increment (cm)`, `Average LOI SOM`)%>%
  rename(Increment= `Soil sample depth increment (cm)`,
         LOI.SOM=`Average LOI SOM`)%>%
  mutate(Type='GIW',
         person='Faith')

#AJ#########
BradfordFebSOC_AJ <- read_csv("04_Output/RC/carbon mapping/BradfordFebSOC_AJ.csv")%>%
  rename(Site_ID=Site, LOI.SOM=Average)%>%
  mutate(
    Increment = case_when(
      Depth  == "U" ~ "0_30",
      Depth  == "L" ~ "30_60"
    ),
    person='AJ',
    Type='GIW')
  

WetlandElevation_AJ <- read_excel("04_Output/RC/carbon mapping/WetlandElevations_Feb2026_AJ.xls")%>%
  select(Site, Lat_u_D, Lon_U_D)%>%
  rename(Site_ID=Site, Lat=Lat_u_D, Long=Lon_U_D)

AJ<-left_join(BradfordFebSOC_AJ, WetlandElevation_AJ)%>%
  select(names(Bradford_Data_Faith))

#Azade###########
  Forest_plot_master_file <- read_excel("04_Output/RC/carbon mapping/Forest_plot_master_file.xlsx") %>%
    select(LAT, LONG, BASIN, OM_CONTENT_UPPER, OM_CONTENT_LOWER, TYPE) %>%
    pivot_longer(
      cols = c(OM_CONTENT_UPPER, OM_CONTENT_LOWER),
      names_to = "Inc.Cat",
      values_to = "LOI.SOM"
    ) %>%
    mutate(
      Increment = case_when(
        Inc.Cat == "OM_CONTENT_UPPER" ~ "0_20",
        Inc.Cat == "OM_CONTENT_LOWER" ~ "20_40"
      ),
      Type = case_when(
        TYPE %in% c("Dense isolated wetland", "Logged Cypress Swamp", "Cypress/Bay Swamp") ~ "GIW",
        TYPE == "Floodplain swamp" ~ "RW",
        TYPE %in% c("Pine Stand", "Hardwood Forest", "Clearcut", "Young Pine Stand", "Young pine stand") ~ "Upland"
      ),
      person='Azade'
      ) %>%
    rename(Lat=LAT, Long=LONG, Site_ID=BASIN)%>%
  select(names(Bradford_Data_Faith))
  
#combine##########
  
soil.samples<-rbind(Forest_plot_master_file, Bradford_Data_Faith, AJ)%>%
  separate(Increment, into = c("Upper", "Lower"), sep = "_", remove = FALSE)
  

soil.samples.XY<-soil.samples%>%select(Site_ID, person, Lat, Long)
write_csv(soil.samples.XY, "04_Output/RC/carbon mapping/soil.samples.XY.csv")


#my streams#####

stream_depths <- read_csv("04_Output/RC/carbon mapping/stream depths.csv")%>%
  rename(Site_ID=ID)%>%
  left_join(Bradford_coords_all)
write_csv(stream_depths, "04_Output/RC/carbon mapping/stream_depths.csv")

#figure##########
surfaceelevations <- read_csv("04_Output/RC/carbon mapping/surfaceelevations.csv")
soil.samples%>%
  ggplot(aes(x=Type, y=LOI.SOM))+
  geom_boxplot()+
  geom_jitter(aes(color=as.numeric(Lower)))+
  scale_color_gradient(high='red',low='blue')+
  facet_wrap(~person)

soil.samples%>%
  ggplot(aes(x=LOI.SOM, y=as.numeric(Lower)))+
  geom_point(aes(shape=person, color=Type))+
  facet_wrap(~person)

soil.samples%>%left_join(surfaceelevations)%>%
  ggplot(aes(x=RASTERVALU, y=LOI.SOM, color=as.numeric(Lower)))+
  geom_point(shape=1)+
  scale_color_gradient(high='red',low='blue')
