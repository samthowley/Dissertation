library(tidyverse)
library(readxl)
library(cowplot)
library(measurements)
library(plotly)

#well dimensions and distance from stream
well.measurements<-read_excel("01_Raw_data/RW.log.xlsx",
                              sheet = "well dims")

well.heights<-well.measurements%>%select(Site, well.height, distance.from.stream)%>%
  separate(Site, into = c("Stream", "GW"), sep = "GW")


scope.elevations.04172026<-read_excel("01_Raw_data/RW.log.xlsx", sheet = "scope elevations 04172026")%>%
  mutate(
    Stream = as.character(Stream), 
    GW = as.character(GW),
    elevation.m= conv_unit(elevation.ft, 'ft', 'm')
    ) %>%
  left_join(well.heights, by=c('Stream', 'GW'))%>%
  mutate(elevation.corrected=elevation.m-well.height)%>%
  select(distance.from.stream, Stream, GW, elevation.ft, elevation.m, well.height, elevation.corrected)


bed.elevations<-scope.elevations.04172026%>%
  filter(GW=='0')%>%
  rename(bed.elevation=elevation.m)%>%
  select(Stream, bed.elevation)


RC.elevations<-left_join(scope.elevations.04172026, bed.elevations)%>%
  mutate(
    datum.elevation=(elevation.corrected*-1)+bed.elevation, #compute data elevation
  )


RC.elevations%>%
  ggplot(aes(x = distance.from.stream, y = datum.elevation, color=GW)) +
  geom_point(size=2) +
  geom_hline(yintercept = 0, color='black')+
  theme_minimal()+
  facet_wrap(~Stream)

WT<-read_excel("01_Raw_data/RW.log.xlsx", sheet = "log")%>%
  mutate(WT.distance.from.surface=as.numeric(WT.distance.from.surface))%>%
  select(Date, Site, top2WTE, WT.distance.from.surface)%>%
  separate(Site, into = c("Stream", "GW"), sep = "GW")%>%
  filter(GW != '7')


WTdepth<-left_join(WT, RC.elevations)%>%
  mutate(WTE=datum.elevation+WT.distance.from.surface)


depth <- read_csv("02_Clean_data/depth.csv")%>%
  filter(ID %in% c('6', '9', '5'))%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(depth=mean(depth, na.rm=T))%>%
  rename(Stream=ID)


head.gradient <- WTdepth%>%
  left_join(depth) %>%
  mutate(
    head.diff = WTE-depth,
    gradient = head.diff/ distance.from.stream,
    WTE=if_else(Stream=='6' & GW=='4' & WTE<0 & depth>0.2, NA, WTE),
    WTE=if_else(Stream=='5' & GW=='5' & WTE<0, NA, WTE),
    WTE=if_else(Stream=='9' & GW=='1' & WTE<0.2, NA, WTE),
    WTE=if_else(Stream=='9' & GW=='2' & WTE< -0.25, NA, WTE),
    WTE=if_else(Stream=='9' & GW=='3' & WTE<0 & depth>0.1, NA, WTE)
)


head.gradient%>%
  ggplot(aes(x = distance.from.stream, y = head.diff, color=GW)) +
  geom_point(size=2) +
  geom_hline(yintercept = 0, color='black')+
  theme_minimal()+
  scale_x_log10()+
  facet_wrap(~Stream, scales='free')
  

head.gradient%>%
  filter(Stream==9)%>%
  ggplot(aes(x = depth, y = WTE, color=GW)) +
  geom_point(size=2) +
  geom_hline(yintercept = 0, color='black')+
  theme_minimal()+
  facet_wrap(~GW+Stream, scales='free')

library(plotly)
ggplotly()

write_csv(head.gradient, "test.csv")

#compare elevations##############
scope.elevations.og<-read_excel("01_Raw_data/RW.log.xlsx", sheet = "scope elevations")%>%
  separate(Site, into = c("Stream", "GW"), sep = "GW", remove = F)%>%
  mutate(Stream = as.character(Stream), 
         GW = as.character(GW),
         elevation.m=conv_unit(elevation.ft, 'ft', 'm')) %>%
  left_join(well.heights, by=c('Stream', 'GW'))%>%
  select(Stream, GW, distance.from.stream, elevation.m, well.height)


bed.elevations.og<-scope.elevations.og%>%filter(GW==0)%>%
  rename(bed.elevation=elevation.m)%>%
  select(Stream, bed.elevation)

scope.elevations.m.og<-left_join(scope.elevations.og, bed.elevations.og, by=c('Stream'))%>%
  mutate(
    datum.elevation=(elevation.m*-1)+bed.elevation, #compute data elevation
         )%>%
  select(Stream,GW,elevation.m,datum.elevation)%>%
  rename(elevation.m.og=elevation.m, datum.elevation.og=datum.elevation)
  
  
compare<-left_join(RC.elevations, scope.elevations.m.og)


WT<-read_excel("01_Raw_data/RW.log.xlsx", sheet = "log")%>%
  mutate(WT.distance.from.surface=as.numeric(WT.distance.from.surface))%>%
  select(Date, Site, top2WTE, WT.distance.from.surface)%>%
  separate(Site, into = c("Stream", "GW"), sep = "GW")%>%
  filter(GW != '7')


WTdepth<-left_join(WT, compare)%>%
  mutate(
    WTE=datum.elevation+WT.distance.from.surface,
    WTE.og=datum.elevation.og+WT.distance.from.surface
    )


depth <- read_csv("02_Clean_data/depth.csv")%>%
  filter(ID %in% c('6', '9', '5'))%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(depth=mean(depth, na.rm=T))%>%
  rename(Stream=ID)


head.gradient <- WTdepth%>%
  left_join(depth) %>%
  mutate(
    head.diff.og = WTE.og-depth,
    head.diff.new = WTE-depth
    )


(a<-head.gradient%>%
  ggplot(aes(x = distance.from.stream, color=GW)) +
  geom_point(aes(y=head.diff.og, shape="Jan 2026"), size=3, stroke=1.5) +
  geom_point(aes(y=head.diff.new, shape="April 2026"), size=3, stroke=1.5) +
  scale_shape_manual(values = c(1, 8))+
  geom_hline(yintercept = 0, color='black')+
  theme_minimal()+
  ylab('Head differential (m)')+
  facet_wrap(~Stream, scales='free'))


(b<-ggplot(compare, aes(x=distance.from.stream, color=GW))+
  geom_point(aes(y=datum.elevation.og, shape="Jan 2026"), size=3, stroke=1.5)+
  geom_point(aes(y=datum.elevation, shape='April 2026'), size=3, stroke=1.5)+
    scale_shape_manual(values = c(1, 8))+
  facet_wrap(~Stream, scales='free')+theme_classic())


plot_grid(a,b, ncol=1)


write_csv(compare, "test.csv")

#Cross sections###########
library(ggpmisc)

RCcrosssections <- read_csv("04_Output/RC/RCcrosssections.csv")%>%
  mutate(LineID=if_else(is.na(LineID), 9, LineID))
names(RCcrosssections)

prox10m <- read_csv("04_Output/RC/prox10m.csv")%>%
  select(LineID, OID_,Fill_DEM_Cul1, Stream, GW)%>%
  rename(Stream_2=Stream, GW_2=GW)%>%
  mutate(LineID=if_else(is.na(LineID), Stream_2, LineID))

prox5m <- read_csv("04_Output/RC/prox5m.csv")%>%
  select(LineID, OID_,Fill_DEM_Cul1, Stream, GW)%>%
  mutate(LineID=if_else(is.na(LineID), Stream, LineID))


test<-left_join(RCcrosssections, prox5m)%>%
  left_join(prox10m)%>%
  mutate(Stream=if_else(is.na(Stream), Stream_2, Stream),
         GW=if_else(is.na(GW), GW_2, GW),
         )
unique(test$Stream_2)


test%>%filter(LineID=='9')%>%
ggplot(aes(OID_, Fill_DEM_Cul1, color=as.factor(GW)))+
  geom_point()+
  facet_wrap(~ORIG_FID, scales='free')+
  theme_classic()

#well length############
names(head.gradient)

well.length<-well.measurements%>% select(Site, ID, below.ground)%>%
  separate(Site, into = c("Stream", "GW"), sep = "GW")%>%
  mutate(GW=as.factor(GW))

sampled<-read_excel("01_Raw_data/RW.log.xlsx", sheet = "log")%>%
  select(Date, Site, Sampled)%>%
  separate(Site, into = c("Stream", "GW"), sep = "GW")
  

RC.setup<-left_join(head.gradient, well.length)%>%
  mutate(bottom.well.elevation=datum.elevation-below.ground)%>%
  left_join(sampled)


RC.setup %>%
  ggplot(aes(x = distance.from.stream)) +
  geom_rect(
    aes(xmin = distance.from.stream - 0.4, xmax = distance.from.stream + 0.4,
        ymin = bottom.well.elevation, ymax = datum.elevation+well.height),
    fill = "gray70", color = NA) +
  geom_point(aes(y = WTE, color=Sampled)) +
  geom_line(aes(y=datum.elevation), color='darkgreen', size=1)+
  geom_hline(yintercept = 0, color='blue')+
  facet_wrap(~Stream, scales = 'free')+
  theme_classic()
