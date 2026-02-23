library(ggpubr)
library(tidyverse)
library(writexl)
library(openxlsx)
library(readxl)
library(cowplot)

no_bins<- read_csv("04_Output/master_metabolism.csv")%>% rename(SM_K600=K600_daily_mean)%>% select(ID, Date, SM_K600)

fixed<- read_csv("04_Output/metabolism_04292025.csv")%>%
  rename(fixed_K600=K600_daily_mean, Date=date)%>%
  select(ID, Date, fixed_K600)%>%
  mutate(ID=as.character(ID))

depth <- read_csv("02_Clean_data/depth.csv")%>% select(Date, depth, ID)
discharge <- read_csv("02_Clean_data/discharge.csv")%>% select(Date, Q, ID)
h_q<-left_join(depth, discharge, by=c('Date', 'ID'))

u <- read_csv("02_Clean_data/velocity.csv")%>% select(Date, u, ID)
huq<-left_join(h_q, u, by=c('Date', 'ID'))

stream_widths <- read_excel("01_Raw_data/stream widths.xlsx")%>%select(ID, width_m)
hqwu<-left_join(huq,stream_widths, by=c('ID'))%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  mutate(
    u=mean(u, na.rm=T),
    Q=mean(Q, na.rm=T),
    depth=mean(depth, na.rm=T)
  )%>% distinct(Date, ID, .keep_all = T)

froude<-hqwu%>%
  mutate(
    Froude=u/sqrt(9.81*depth))

slopes <- read_excel("01_Raw_data/stream slopes.xlsx", sheet = "Sheet2")

variables<-left_join(froude, slopes, c('ID'))%>%mutate(day=as.Date(Date))%>%
  group_by(ID,day)%>%
  summarise(Q=mean(Q, na.rm=T),
            depth=mean(Q, na.rm=T),
            u=mean(u, na.rm=T),
            width_m=mean(width_m),
            froude=mean(Froude, na.rm=T),
            slope=mean(slope_RTK, na.rm=T))%>%
  rename(Date=day)

k600 <- read_csv("01_Raw_data/GD/GasDome_compiled.csv")%>%
  select(Date, ID, KCO2_dh, k600_dh, KCO2_1d)%>%
  mutate(Date=as.Date(Date))

k600_variables<-left_join(k600, variables, by=c('Date', 'ID'))

raymond<-k600_variables%>%
  mutate(Q=Q*0.3)%>%
  mutate(raymond1= ((u*slope)^0.089)*(depth^0.54)*5037,
         raymond2= 5937*(1-2.54*froude^2)*((u*slope)^0.089)*(depth^0.58),
         raymond3=1162*(slope^0.77)*(u^0.85),
         raymond4=((u*slope)^0.76)*951.2,
         raymond5=(u*slope)*2841+202,
         raymond5=929*((u*slope)^0.75)*(Q^0.011),
         raymond6=4725*((u*slope)^0.86)*(Q^-0.14)*(depth^0.66))

ggplot(raymond, aes(x=Q)) +
  geom_point(aes(y=k600_dh, color='Gas Dome'))+
  geom_point(aes(y=raymond1, color='raymond1'))+
  geom_point(aes(y=raymond2, color='raymond2'))+
  geom_point(aes(y=raymond3, color='raymond3'))+
  geom_point(aes(y=raymond4, color='raymond4'))+
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ ID, ncol=5, scales = 'free')

compare<-left_join(fixed, no_bins, by=c('Date','ID'))
compare<-left_join(h_q,compare, by=c('Date','ID'))

ggplot(compare, aes(x=Q)) +
  geom_point(aes(y=fixed_K600, color='fixed_K600'))+
  geom_point(aes(y=SM_K600, color='SM_K600'))+
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ ID, ncol=5, scales = 'free')
dev.new()

compare%>%
  group_by(ID)%>%
  summarize(fixed=mean(fixed_K600, na.rm=T),
            SM=mean(SM_K600, na.rm=T))
