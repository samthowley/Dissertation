library(tidyverse)
library(readxl)
library(cowplot)
library(measurements)
library(plotly)

well.measurements<-read_excel("01_Raw_data/RW.log.xlsx",
                              sheet = "well dims")

scope.elevations.raw<-read_excel("01_Raw_data/RW.log.xlsx",
                             sheet = "scope elevations")%>%
  separate(Site, into = c("Stream", "GW"), sep = "GW", remove = FALSE)
  

scope.elevations.04172026<-read_excel("01_Raw_data/RW.log.xlsx",
                                 sheet = "scope elevations 04172026")%>%
  rename(scope.surface.elevation.04162026=scope.surface.elevation)%>%
  select(Stream, GW, scope.surface.elevation.04162026)%>%
  mutate(Stream=as.character(Stream), GW=as.character(GW))

scope.elevations<-left_join(scope.elevations.raw, scope.elevations.04172026)%>%
  mutate(scope.surface.elevation=if_else(Stream=='5',scope.surface.elevation.04162026,scope.surface.elevation))


bed.elevations<-scope.elevations%>%filter(Site %in% c('5GW0', '6GW0', '9GW0'))%>%
  rename(bed.elevation=elevation.ft)%>%
  select(-Site)

scope.elevations.m<-left_join(scope.elevations, bed.elevations, by='ID')%>%
  separate(Site, into = c("ID", "Well"), sep = "GW", remove = FALSE)%>%
  mutate(datum.elevation=(elevation.ft*-1)+bed.elevation,
         datum.elevation=conv_unit(datum.elevation, 'ft', 'm')
         )%>%
  select(ID, Well,elevation.ft,bed.elevation,datum.elevation)


RC.elevations<-well.measurements %>%
  separate(Site, into = c("ID", "Well"), sep = "GW", remove = FALSE)%>%
  left_join(scope.elevations.m, by=c('ID', "Well"))%>%
  
  mutate(
    distance.from.stream=if_else(is.na(distance.from.stream), 0, distance.from.stream))%>%
  group_by(ID)%>%
  mutate(
    well.bottom.loc=datum.elevation-below.ground,
    well.height=if_else(is.na(well.height), 0, well.height),
    well.height=well.height+datum.elevation,
    screen.extent=screen.extent+datum.elevation
  )


WTdepth<-read_excel("01_Raw_data/RW.log.xlsx", sheet = "log")%>%
  mutate(WT.distance.from.surface=as.numeric(WT.distance.from.surface))%>%
  select(Date, Site, top2WTE, WT.distance.from.surface, Sampled)%>%
  separate(Site, into = c("ID", "Well"), sep = "GW", remove = FALSE)


depth <- read_csv("02_Clean_data/depth.csv")%>%
  filter(ID %in% c('5', '6', '9'))%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(depth=mean(depth, na.rm=T))
  

head.gradient <- full_join(WTdepth, RC.elevations) %>%
  drop_na(Well, Sampled) %>%
  filter(Well!=7)%>%
  mutate(
    WT.elevation = datum.elevation + WT.distance.from.surface,
  ) %>%
  left_join(depth) %>%
  mutate(
    head.diff = depth-WT.elevation,
    gradient = head.diff/ distance.from.stream,
    gradient=if_else(ID=='5'& Well=='4'& head.diff<0, NA, gradient),
    gradient=if_else(ID=='5'& Well=='3'& head.diff<0, NA, gradient),
    gradient=if_else(ID=='5'& Well=='5'& head.diff<0, NA, gradient),
    gradient=if_else(ID=='6'& Well=='3'& head.diff<0, NA, gradient),
    gradient=if_else(ID=='6'& Well=='2'& head.diff< -0.5, NA, gradient),
    gradient=if_else(ID=='9'& Well=='2'& head.diff>0.5, NA, gradient),
    gradient=if_else(ID=='9'& Well=='1'& head.diff>0.5, NA, gradient),
    gradient=if_else(ID=='9'& Well=='4'& depth<0.1 & head.diff<0, NA, gradient),
    
  )%>%
  select(-Sampled, -total.well.length, -below.ground, -screen.extent, -well.bottom.loc)



head.gradient%>%
  group_by(ID)%>%
  ggplot(aes(x = Well, y = gradient)) +
  geom_boxplot() +
 # geom_line()+
  geom_hline(yintercept = 0, color='black')+
  geom_jitter()+
  ylab("head gradient")+
  facet_wrap(~ID, scales = "free")+theme_minimal()


head.gradient%>%
  filter(ID==9)%>%
  ggplot(aes(x = depth, y = gradient)) +
  geom_point(size=2) +
  geom_hline(yintercept = 0, color='black')+
  facet_wrap(~Well, scales='free')+
    theme_minimal()

ggplotly()

library(plotly)

write_csv(head.gradient, "test.csv")

library(ggpmisc)

RCcrosssections <- read_csv("04_Output/RC/RCcrosssections.csv")
names(RCcrosssections)

RCcrosssections%>%
  filter(Basin==9)%>%
  ggplot(aes(x = OID_, y = RASTERVALU)) +
  geom_point()+
  stat_poly_line(formula = y ~ x, se = FALSE, alpha=0.3)+
  stat_poly_eq(
    aes(label = paste(..rr.label.., sep = " ~~ ")),
    formula = y ~ x, parse = TRUE, 
    size = 3, label.x = "right", label.y = "bottom", vstep = 0.05
  )+
  geom_point()+
  facet_wrap(~ORIG_FID, scales='free')+
  theme_minimal()
