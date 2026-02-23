
library(tidyverse)
library(writexl)
library(readxl)

CO2mol <- function(CO2) {
  CO2$Temp_C<-fahrenheit.to.celsius(CO2$Temp_PT)
  CO2$Temp_K<-CO2$Temp_C+273.15
  CO2$exp<-2400*((1/CO2$Temp_K)-(1/298.15))
  CO2$KH<-0.034*2.178^(CO2$exp)#mol/L/atm

  CO2$CO2_atm<-CO2$CO2/1000000
  CO2$CO2_molL<-CO2$CO2_atm*CO2$KH

  CO2$CO2.water_umol.L<-CO2$CO2_molL*10^6

  return(CO2)}

RC_flow_regime <- read_csv("04_Output/RC/RC.flow.regime.csv")
gas <- read_csv("04_Output/gas.samples.csv")

RC.CO2 <- subset(gas, type == "CO2")%>%
  rename(CO2.water_umol.L=water_umol.L, CO2.water.ppm=water.ppm)%>%
  filter(chapter=='RC')%>%select(-type, -chapter)

RC.CH4 <- subset(gas, type == "CH4")%>%
  rename(CH4.water_umol.L=water_umol.L, CH4.water.ppm=water.ppm)%>%
  filter(chapter=='RC')%>%select(-type, -chapter)

RC.gas<-left_join(RC.CO2, RC.CH4, by=c('Date', 'Site'))

lateral.flux<-
  left_join(RC.gas, RC_flow_regime)%>%
  mutate(
    CO2_molL=CO2.water_umol.L/10^6,
    CO2_flux=qL_m2.sec*86400*CO2_molL*44*10^3*(1/stream.w),
    CH4_molL=CH4.water_umol.L/10^6,
    CH4_flux=qL_m2.sec*86400*CH4_molL*44*10^3*(1/stream.w)
    )%>%
  arrange(Site, Date)%>%
  distinct(Site, Date, .keep_all = T)%>%
  select( -stream.w)

#STREAM####

RC.CO2.stream<- subset(gas, type == "CO2")%>%
  filter(chapter=='stream', Site %in% c('5', '6', '9'))%>%
  rename(CO2.water_umol.L=water_umol.L,
         ID=Site)%>%
  select(-type, -chapter, -water.ppm)%>%
  mutate(
    CO2_molL=CO2.water_umol.L/10^6
  )

RC.CH4.stream <- subset(gas, type == "CH4")%>%
  filter(chapter=='stream', Site %in% c('5', '6', '9'))%>%
  rename(CH4.water_umol.L=water_umol.L,
         ID=Site)%>%
  select(-type, -chapter, -water.ppm)%>%
  mutate(
    CH4_molL=CH4.water_umol.L/10^6
  )

stream.gas<-left_join(RC.CO2.stream, RC.CH4.stream)


CO2 <- read_csv("02_Clean_data/CO2.csv")%>%
  filter(ID %in% c('5', '6', '9'))%>%  select(Date, ID, CO2)%>%
  left_join(read_csv("02_Clean_data/temperature.csv"))%>%
  CO2mol()%>%
  mutate(
    CH4.water_umol.L= NA,
    CH4_molL=NA
  )%>%
  select(names(stream.gas))


CO2.rng<-
  left_join(lateral.flux, rbind(stream.gas, CO2))%>%
  mutate(
    CO2_flux=NA,
    Distance_m=0.1,
    WT_elevations=0,
    Well=0,
    well_types='stream',
    qL_m2.sec=NA,
    DOC_g.m2.day=NA,
    DIC_g.m2.day=NA,
    Site=paste0(ID, 'GW', Well),
    Date=as.Date(Date))%>%
  select(names(lateral.flux))%>%
  distinct(Site, Date, .keep_all=T)

RC.gas<-rbind(lateral.flux, CO2.rng)


write_csv(RC.gas, "04_Output/RC/RC.gas.sample.flux.csv")




test<-RC.gas%>%filter(Site=='5GW5')
