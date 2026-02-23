library(tidyverse)
library(ggpmisc)
library(cowplot)
library(ggplot2)


fatble <- read_csv("fatble.csv", col_select = 1:4)%>%
  rename('distance'='distance...4', 'Long'="Site...2")%>%
  group_by(ID)%>%
  arrange(ID, distance)%>%
  mutate(ID=as.character(ID),Long=as.character(Long))

bf <- read_csv("04_Output/baseflow.csv")%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  mutate(baseflow=mean(baseflow, na.rm=T))%>%
           distinct(ID, Date, .keep_all = T)%>%
  select(Date, ID, baseflow)%>%
  filter(ID %in% c('5','6', '9'))


long_log<-read_csv("01_Raw_data/Long. Log.csv")
long_log_edited <- long_log %>% rename(Date=Visited)%>%mutate(Date=mdy(Date))%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")%>%
  mutate(Long=ifelse(is.na(Long), '0',Long))


fa_long<-left_join(long_log_edited, fatble, by=c('ID', 'Long'))

qL<-left_join(fa_long, bf, by=c('ID','Date'))%>%
  select(Date, ID, Long, DO, pH, distance, RASTERVALU, baseflow)%>%
  arrange(ID,Date, distance)

library(openxlsx)
write.xlsx(qL, file = "qL.xlsx")


discharge<-discharge %>%
  filter(ID=='5', Date=='2025-04-02')


