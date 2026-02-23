library(anytime)
library(tidyverse)
library(readxl)
setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project")
#compile PT####
file.names <- list.files(path="Wetlands/Wetland_H20_level/For R", pattern=".csv", full.names=TRUE)
PT_all<-data.frame()
for(fil in file.names){
  PT <- read_csv(fil,col_types = cols(`#` = col_skip()),skip = 1)
  PT<-PT[,c(1,2)]
  colnames(PT)[1] <- "Date"
  colnames(PT)[2] <- "PT"
  PT$Date <- mdy_hms(PT$Date)
  PT$BasinID<-strsplit(basename(fil), '_')[[1]][1]
  PT$WetlandID<-strsplit(basename(fil), '_')[[1]][2]
  PT_all<-rbind(PT_all,PT)}

PT_all$BasinID[PT_all$BasinID=='14/9']<-'14.9'#change name so its more compatible in R

PT_all <-  PT_all %>%
  mutate(region= case_when(BasinID=="6"|BasinID=="6a"|BasinID=="3"|
                          BasinID=="7"~ 'N',
                           BasinID=="5"|BasinID=="5a"|BasinID=="15"|
                          BasinID=="9"|BasinID=="14"|BasinID=="13"|
                          BasinID=="14.9"~ 'S'))

#ID basins by their region for easy joining with baro

write_csv(PT_all, "Wetlands/Wetland_H20_level/Data_processed/compiled_PT.csv")

########################
#Form dataframe (df)####
########################
PT<-read_csv("Wetlands/Wetland_H20_level/Data_processed/compiled_PT.csv")

#PT<-PT_all %>% filter(Date>'2023-01-01')

PT <- PT %>%
  mutate(Site = paste(BasinID, WetlandID, sep = "_")) %>%
  arrange(Site, Date) %>% mutate(day=as.Date(Date))

setwd("C:/Howley_Bradford_Streams/Howley_Bradford_Streams")
baro<-read_csv('01_Raw_data/PT/compiled_baro.csv')


master<-left_join(PT, baro, by=c('region','Date'))
master <- master[!duplicated(master[c('Date','Site')]),]

# check<-PT%>%filter(Site=='13_271')
# ggplot(check, aes(Date, PT)) +
#   geom_line()


setwd("//ad.ufl.edu/ifas/SFRC/Groups/Hydrology/Bradford_Forest_Project")
Wetland_well_metadata <- read_excel("Masterfiles_latest/Wetland_well_metadata.xlsx",sheet = "Wetland_pivot_history")

#Format metadata to be compatable with master df
chk1<-Wetland_well_metadata %>% select(Site_ID,Basin_ID,`P_G/L_date_1`,
                                       P_G_cm_1,P_L_cm_1) %>% rename('PG'="P_G_cm_1", 'PL'='P_L_cm_1', 'Date'='P_G/L_date_1')

chk2<-Wetland_well_metadata %>%select(Site_ID,Basin_ID,`P_G/L_date_2`,
                                      P_G_cm_2,P_L_cm_2)%>% rename('PG'="P_G_cm_2", 'PL'='P_L_cm_2', 'Date'='P_G/L_date_2')

chk3<-Wetland_well_metadata %>%select(Site_ID,Basin_ID,`P_G/L_date_3`,
                                      P_G_cm_3,P_L_cm_3)%>% rename('PG'="P_G_cm_3", 'PL'='P_L_cm_3', 'Date'='P_G/L_date_3')

chk4<-Wetland_well_metadata %>%select(Site_ID,Basin_ID,`P_G/L_date_4`,
                                      P_G_cm_4,P_L_cm_4)%>% rename('PG'="P_G_cm_4", 'PL'='P_L_cm_4', 'Date'='P_G/L_date_4')
PG_PL<-rbind(chk1,chk2,chk3,chk4)

PG_PL <- PG_PL %>%
  rename('day'='Date',"Site"="Site_ID")%>%select(day, PG, PL,Site)

master<-left_join(master, PG_PL, by=c('Site','day'))

#Arrange df to be in chronological order by site. Then fill down PG and PL values
master <- master %>% arrange(Site, Date)%>% fill(PG, PL)
#######################
#Calculate Stage#####
######################

master<-master %>%
  mutate(Water_press=PT-PTbaro)%>%
  mutate(sensor_depth=1000*Water_press/2.2/(2.54^2)/100, PG=as.numeric(PG), PL=as.numeric(PL))%>%
  mutate(depth=sensor_depth-(PL-PG)/100)%>%
  select(Date, Site, BasinID, Water_press, sensor_depth,depth, PT, PTbaro, PG,PL) %>%
  filter(depth> -5) %>% filter(depth<4)

ggplot(master%>%filter(BasinID=='9'), aes(x=Date, color=Site)) + geom_line(aes(y=depth))+facet_wrap(~ BasinID, ncol=5)

wetland_h<-master %>% mutate(day=as.Date(Date))%>% group_by(Site, day)%>%
  mutate(depth=mean(depth, na.rm=T))%>%
  distinct(Site, day, .keep_all=T)

CO2<-chimney%>%select(Date, ID, Basin, day, passive, mean_CO2flux, ER,Q_quartile)%>%
  rename(BasinID=Basin)

wetland_h.CO2<-left_join(CO2, wetland_h, c('day','BasinID'))

ggplot(wetland_h.CO2, aes(depth, passive, color=Site)) +
  geom_point()+
  scale_y_log10()+
  xlab('Wetland Stage (m)')+
  ylab(expression(Passive~CO[2]~~'g'/m^2/'day'))+
  facet_wrap(~ID, scales='free')


