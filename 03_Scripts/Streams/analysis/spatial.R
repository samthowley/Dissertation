source("03_Scripts/Streams/analysis/data for analysis.R")



basin_area <- read_csv("01_Raw_data/wetland cover/basin_area.csv")%>%
  select(Basin, Shape_Area)%>%rename(ID=Basin)

pH.avg<-pH%>%
  group_by(ID)%>%summarise(pH.avg=round(mean(pH, na.rm=T), 2))

SpC.avg<-SpC%>%
  group_by(ID)%>%summarise(SpC.avg=round(mean(SpC, na.rm=T), 2))

Q.avg<-discharge%>%
  left_join(basin_area)%>%
  # mutate(
  #   Q=Q/10^3,
  #   q=Q/Shape_Area)%>%
  group_by(ID)%>%summarise(Q.avg=round(mean(Q, na.rm=T),2))

wetland_perc <- read_csv("01_Raw_data/wetland cover/wetland.perc.csv")

T.avg<-temperature%>%
  group_by(ID)%>%summarise(T.avg=round(mean(TempC, na.rm=T), 2))

int.ext.spat<-int.ext%>% left_join(pH.avg)%>%left_join(SpC.avg)%>%
  left_join(Q.avg)%>%left_join(wetland_perc)%>%left_join(T.avg)%>%
  mutate(
    int.contrib=(internal/CO2_flux)*100,
    ext.contrib=(external/CO2_flux)*100
  )

int.ext.avg<-int.ext.spat%>%
  group_by(ID)%>%
  summarise(
    int.avg=round(mean(internal, na.rm=T),2),
    ext.avg=round(mean(external, na.rm=T),2),
    int.contrib=round(mean((internal/CO2_flux)*100, na.rm=T), 2),
    ext.contrib=round(mean((external/CO2_flux)*100, na.rm=T), 2)
  )%>% left_join(pH.avg)%>%left_join(SpC.avg)%>%
  left_join(Q.avg)%>%left_join(wetland_perc)%>%left_join(T.avg)

summary(lm(pH.avg ~ int.contrib, data = int.ext.avg))
summary(lm(SpC.avg ~ int.contrib, data = int.ext.avg))
summary(lm(Q.avg ~ int.contrib, data = int.ext.avg))
summary(lm(basin.wetland.perc ~ int.contrib, data = int.ext.avg))


######################################################################

int.ext.spat%>%
  ggplot(aes(x=pH.avg, y=SpC.avg, color=ID))+
  geom_point()+theme_minimal()+scale_y_log10()+
  xlab('pH')+ylab('Specific Conductivity')

int.ext.spat%>%
  mutate(
    pH.avg=as.factor(pH.avg)
        )%>%
  ggplot(aes(x=pH.avg, y=int.contrib, color=ID))+
  geom_violin()+
  geom_jitter(alpha=0.3)+
  theme_minimal()+scale_y_log10()+
  xlab('pH')


######################################################################

int.ext.spat%>%
  ggplot(aes(x=T.avg, y=Q.avg, color=ID))+
  geom_point()+theme_minimal()+scale_y_log10()

int.ext.spat%>%
  mutate(
    Q.avg=as.factor(Q.avg)
  )%>%
  ggplot(aes(x=Q.avg, y=int.contrib, color=ID))+
  geom_violin()+
  geom_jitter(alpha=0.3)+
  theme_minimal()+scale_y_log10()+
  xlab('pH')
