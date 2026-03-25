#packages#####
rm(list=ls())

library(tidyverse)
library(readxl)
library(measurements)
library(mmand)
library(zoo)
library(broom)
library(weathermetrics)
library(streamMetabolizer)
library(openxlsx)

#GW Correction#####

compiled_baro <- read_csv("01_Raw_data/PT/compiled_baro.csv")%>%
  mutate(Date=as.Date(Date))%>%
  group_by(region, Date)%>%
  mutate(PT=mean(PTbaro, na.rm=T)*68.9476)%>%
  distinct(region, Date, .keep_all=T)%>%
  select(Date, region, PT)

DO <- read_csv("02_Clean_data/DO_cleaned.csv") %>%
  mutate(region=
           case_when(ID=="6"|ID=="6a"|ID=="3"|ID=="7"~ 'N',
                     ID=="5"|ID=="5a"|ID=="15"|ID=="9"|ID=="14"|ID=="13"~ 'S'))

DO_edit<-left_join(DO, compiled_baro)%>%
  arrange(ID, Date)%>%
  mutate(light=calc_light(Date,  29.8, -82.6))%>%
  mutate(time=case_when(
    light>1000~'day',
    light<=1000~'night'),
    Date=as.Date(Date)) %>%
  group_by(ID,Date, time)%>%
  mutate(DO_night=mean(DO, na.rm = T))%>%ungroup()%>%
  group_by(Date, ID)%>%
  mutate(DO=mean(DO, na.rm=T), DO_Saturation=calc_DO_sat(Temp_DO, PT))%>%
  distinct(Date, ID, .keep_all=T)%>%
  select(-light, -time, -PT)


metabolism<-read_csv('04_Output/stream/master_metabolism.csv')%>%
  mutate(NEP=(GPP+ER))%>%
  rename(Date=date)

met_DO<-left_join(metabolism, DO_edit, by=c('Date','ID'))

flow.regime <- read_csv("04_Output/flow_regime_daily.csv")%>%select(-K600)

met_DO.flow<-left_join(met_DO, flow.regime, by=c('Date','ID'))

units<-met_DO.flow %>%
  mutate(
    Q_m3.day=Q_m3.s*86400,
    u_m.day=u*86400,
  )

DO_GW<-1.3

gw_corrected<-units%>%
  mutate(
    GW_correction=(DO_GW-DO)*(qL_m2.sec/width)*86400,
    ER_GW_correction=(DO_GW-DO_night)*(qL_m2.sec/width)*86400
  )%>%
  mutate(NEP_corrected= NEP-GW_correction,
         ER_corrected= ER-ER_GW_correction)%>%
  select(Date, ID, NEP_corrected, GPP, ER_corrected, K600)


gw_corrected%>%
  ggplot(aes(x=Date, y=NEP_corrected))+
  geom_point()+
  facet_wrap(~ID)

write_csv(gw_corrected, "04_Output/gw_corrected_metabolism.csv")

