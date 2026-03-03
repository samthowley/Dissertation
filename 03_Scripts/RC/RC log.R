library(tidyverse)
library(readxl)
library(cowplot)
library(measurements)

WTdepth<-read_excel("01_Raw_data/RW.log.xlsx", sheet = "log")%>%
  mutate(WT.distance.from.surface=as.numeric(WT.distance.from.surface))

RC.dims<-read_excel("01_Raw_data/RW.log.xlsx", sheet = "well dims")

RC <- left_join(WTdepth, RC.dims, by='Site')%>%
  filter(!is.na(Date))%>%
  mutate( WTE=surface.elevation-WT.distance.from.surface)

well_types <- data.frame(
  Site = c('5GW0', '5GW1', '5GW2', '5GW3', '5GW4', '5GW5', '5GW6', '5GW7', '5GW8',
           '6GW0', '6GW1', '6GW2', '6GW3', '6GW4', '6GW5',
           '9GW0', '9GW1','9GW2', '9GW3', '9GW4', '9GW5'),
  well_types = c('stream', 'RW', 'RW', 'RW', 'RW', 'RW', 'RW', 'RW', "upland",
                 'stream', 'RW', 'RW', 'RW', 'RW', "upland",
                 'stream', 'RW', 'RW', 'RW', 'RW', "upland"))
RC<-left_join(RC, well_types)%>%mutate(ID=as.character(ID))

stream.flow <- read_csv("04_Output/flow_regime_daily.csv")%>%
  filter(ID %in% c('5', '6', '9'))%>%
  select(Date, ID, qL_m2.sec, width, depth)%>%
  rename(stream.w=width, stream.depth=depth)


RC.regime<-left_join(RC, stream.flow)%>%
  arrange(Site, Date)%>%
  filter(!is.na(Site))%>%
  select(
    Date, Site, WTE, WT.distance.from.surface, distance.from.stream, qL_m2.sec,
    stream.w, stream.depth, well_types, Temp, Sampled
  )

RC<-left_join(RC.regime, read_excel("01_Raw_data/RW.log.xlsx",
                                    sheet = "scope elevations"))%>%
  separate(Site, into = c("ID", "Well"), sep = "GW", remove = FALSE)

#write_csv(RC, "04_Output/RC/RC.flow.regime.csv")

#check WT###########


well.measurements<-read_excel("01_Raw_data/RW.log.xlsx",
           sheet = "well dims")%>%
  select(-surface.elevation, -well.bottom.elevation)

scope.elevations<-read_excel("01_Raw_data/RW.log.xlsx",
           sheet = "scope elevations")

WTdepth<-read_excel("01_Raw_data/RW.log.xlsx", sheet = "log")%>%
  mutate(WT.distance.from.surface=as.numeric(WT.distance.from.surface))%>%
  select(Date, Site, WT.distance.from.surface, Sampled)

RC.elevations<-full_join(scope.elevations, well.measurements)%>%
  separate(Site, into = c("ID", "Well"), sep = "GW", remove = FALSE)%>%
  mutate(
    distance.from.stream=if_else(is.na(distance.from.stream), 0, distance.from.stream))%>%
  group_by(ID)%>%
  mutate(
    stream.bed.elevation = elevation.ft[Well == "0"],
    
    datum.surface.elevation=(elevation.ft*-1)+stream.bed.elevation,
    datum.surface.elevation=conv_unit(datum.surface.elevation, 'ft', 'm'),

    
    well.bottom.elevation=datum.surface.elevation- below.ground,
    screen.extent=well.bottom.elevation+screen.extent)


RC.elevations%>%
  drop_na(Well)%>%
  ggplot(aes(x = distance.from.stream, y = elevation.ft, color=Well)) +
  geom_line(color='black')+
  geom_point(size=2)+
  ylab("Scope Elevations (ft)")+
  facet_wrap(~ID, scales = "free")+theme_minimal()


full_join(WTdepth, RC.elevations)%>%
  drop_na(Well)%>%
  mutate(
    WT.elevation=datum.surface.elevation+WT.distance.from.surface)%>%
  
  ggplot(aes(x = distance.from.stream, y = datum.surface.elevation, color=Well)) +
  geom_linerange(aes(ymin = well.bottom.elevation,
                     ymax = screen.extent),
                 color = "gray60",
                 size = 2)+
  geom_hline(yintercept = 0, color='blue')+
  geom_line(color='black')+
  geom_point(size=2)+
  ylab("Surface Elevations; Datum: Stream Bed (m)")+
  facet_wrap(~ID, scales = "free")+theme_minimal()



full_join(WTdepth, RC.elevations)%>%
  drop_na(Well, Sampled)%>%
  mutate(
    WT.elevation=datum.surface.elevation+WT.distance.from.surface,
         Sampled)%>%
  
  ggplot(aes(x = distance.from.stream, y = datum.surface.elevation)) +
  geom_point(size=3)+
  geom_linerange(aes(ymin = well.bottom.elevation,
                     ymax = screen.extent),
                 color = "black",
                 size = 2)+
  geom_point(aes(y=WT.elevation, color=Sampled)) +
  geom_hline(yintercept = 0, color='blue')+
  geom_line(color='black')+
  ylab("Surface Elevations; Datum: Stream Bed (m)")+
  facet_wrap(~ID, scales = "free")+theme_minimal()








