library(tidyverse)
library(readxl)
library(cowplot)

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

well.dims <- read_excel("01_Raw_data/RW.log.xlsx",
                     sheet = "well dims")%>%
  select(Site, below.ground)

well.bottom<-left_join(
  read_excel("01_Raw_data/RW.log.xlsx", sheet = "well dims"),
  read_excel("01_Raw_data/RW.log.xlsx",sheet = "scope elevations"),
  by=c('Site')
  )%>%select(Site, below.ground, surface.elevation)%>%
  mutate(bottom.elevation=surface.elevation-below.ground)

library(plotly)




ggplot(
  RC %>% filter(Site != "5GW7", !is.na(Sampled)),
  aes(x = distance.from.stream, y = scope.surface.elevation, color = Well)) +
  geom_point(size=2) +
  geom_line(color='black')+

  facet_wrap(~ID, scales = "free")+theme_minimal()









checking<-read_excel("01_Raw_data/RW.log.xlsx",sheet = "scope elevations")%>%

  full_join(RC.dims%>%select(Site, distance.from.stream))%>%
  left_join(RC.regime%>%select(Site, distance.from.stream, WT.distance.from.surface))%>%
  group_by(ID)%>%
  mutate(
    distance.from.stream=if_else(is.na(distance.from.stream), 0, distance.from.stream),
    stream.datum=(elevation.ft*-1)+max(elevation.ft, na.rm = T),
    WT.height=stream.datum+WT.distance.from.surface)%>%
  separate(Site, into = c("ID", "Well"), sep = "GW", remove = FALSE)%>%
  arrange(ID, distance.from.stream)%>%ungroup%>%
  full_join(
    RC.regime%>%select(Site, distance.from.stream, WT.distance.from.surface))%>%
  full_join(

    RC.regime%>%select(Site, distance.from.stream, stream.depth)%>%
      mutate(distance.from.stream=0)%>%
      separate(Site, into = c("ID", "Well"), sep = "GW", remove = FALSE)%>%
      select(ID, distance.from.stream, stream.depth)%>%
      distinct(ID, distance.from.stream, stream.depth)
  )


plot_grid(

  ggplot(checking%>% filter(!is.na(ID)), aes(x=distance.from.stream,  color=Well))+
  geom_point(aes(y=stream.datum), size=3)+
  geom_path(aes(y=stream.datum),color='gray')+
  geom_point(aes(y=stream.depth),color='lightblue')+
  geom_point(aes(y=WT.height),color='pink')+
  ylab('Stream Datum Elevations (ft)')+
  xlab("Distance from stream (ft)")+
  facet_wrap(~ID)
  ,
  ggplot(checking%>% filter(!is.na(ID)), aes(x=distance.from.stream, y=elevation.ft, color=Well))+
    geom_point(size=3)+
    geom_path(color='gray')+
    ylab('Raw Elevations from Scopeb (ft)')+
    xlab("Distance from stream (ft)")+
    facet_wrap(~ID)
  ,
  nrow=2)








ggplot(checking, aes(x=distance.from.stream, y=stream.datum, color=Well))+
  geom_point(size=3)+
  geom_path(color='gray')+
  ylab('Stream Datum Elevations (ft)')+
  xlab("Distance from stream (ft)")+
  facet_wrap(~ID)





  ggplot(checking, aes(x=distance.from.stream, y=stream.datum, color=Well))+
  geom_point(size=3)+
  geom_path(color='gray')+
  ylab('Stream Datum Elevations (ft)')+
  xlab("Distance from stream (ft)")+
  facet_wrap(~ID)




# geom_hline(
#   data = chk.elevations,
#   aes(yintercept = bottom.elevation, group = Site),
#   inherit.aes = FALSE,
#   color = "gray", size = 0.8) +
#   geom_hline(yintercept = 0,color='blue')+



