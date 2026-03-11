well.measurements<-read_excel("01_Raw_data/RW.log.xlsx",
                              sheet = "well dims")

scope.elevations<-read_excel("01_Raw_data/RW.log.xlsx",
                             sheet = "scope elevations")

RC.elevations<-full_join(scope.elevations, well.measurements)%>%
  separate(Site, into = c("ID", "Well"), sep = "GW", remove = FALSE)%>%
  mutate(
    distance.from.stream=if_else(is.na(distance.from.stream), 0, distance.from.stream))%>%
  group_by(ID)%>%
  mutate(
    
    stream.bed.elevation = elevation.ft[Well == "0"],
    datum.surface.elevation=(elevation.ft*-1)+stream.bed.elevation,
    datum.surface.elevation=conv_unit(datum.surface.elevation, 'ft', 'm'),
    
    
    well.bottom.elevation=datum.surface.elevation-below.ground,
    
    well.height=if_else(is.na(well.height), 0, well.height),
    well.height=well.height+datum.surface.elevation,
    
    screen.extent=screen.extent+datum.surface.elevation
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
  

head.gradient<-full_join(WTdepth, RC.elevations)%>%
  drop_na(Well, Sampled)%>%
  mutate(
    WT.elevation=datum.surface.elevation+WT.distance.from.surface,
    Sampled=if_else(WT.distance.from.surface>=below.ground & Sampled=='N', 'DID NOT PUMP', Sampled),
    month=month(Date)
  )%>%
  left_join(depth)%>%
  filter(Site %in% c('5GW4', '5GW5', '5GW6', '5GW8', '6GW1', '6GW2', 
                     '6GW3', '9GW1','9GW2', '9GW3', '9GW4', '9GW5'))%>%
  arrange(ID, distance.from.stream, Date)
  



head.gradient <- full_join(WTdepth, RC.elevations) %>%
  drop_na(Well, Sampled) %>%
  mutate(
    WT.elevation = datum.surface.elevation + WT.distance.from.surface,
    Sampled = if_else(WT.distance.from.surface >= below.ground & Sampled == 'N', 'DID NOT PUMP', Sampled),
    month = month(Date)
  ) %>%
  left_join(depth) %>%
  filter(Site %in% c('5GW5', '5GW6', '5GW8', '6GW1', '6GW2',
                     '6GW3', '9GW1', '9GW2', '9GW3', '9GW4', '9GW5')) %>%
  select(Date, ID, Site, Well, depth, datum.surface.elevation, WT.distance.from.surface,
         WT.elevation, distance.from.stream, scope.surface.elevation, month) %>%
  arrange(ID, distance.from.stream, Date) %>%
  mutate(
    stream.head =depth,
    head.diff = WT.elevation - stream.head,
    gradient = head.diff / distance.from.stream
  )



head.gradient%>%
  ggplot(aes(x = Well, y = gradient, color=as.factor(Date))) +
  geom_point() +
  geom_line()+
  geom_hline(yintercept = 0, color='blue')+
  geom_point(size=3)+
  facet_wrap(~ID, scales = "free")+theme_minimal()



write_csv(head.gradient, "test.csv")
