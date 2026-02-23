library(tidyverse)
library(marelac)
library(tidyverse)
library(weathermetrics)
library(streamMetabolizer)
library(ggtern)
library(readxl)

R<-0.08205
V_wp<-0.07
V_hp<-0.03
conversion_bar2atm<-0.986923
conversion_PSI2atm<-14.696

gas.sample.conv <- function(chapter) {

  k<-chapter%>%mutate(
    k_CO2=(gas_solubility(S = 0, t = water.Temp.C, species = "CO2"))/(conversion_bar2atm*10^6),
    k_CH4=(gas_solubility(S = 0, t = water.Temp.C, species = "CH4"))/(conversion_bar2atm*10^6),
    k_N2O=(gas_solubility(S = 0, t = water.Temp.C, species = "N2O"))/(conversion_bar2atm*10^6))%>%
    mutate(Date=as.Date(Date))

  dfCO2<-k %>%
    select(-CH4_air_ppm, -CH4_x, -N2O_air_ppm, -N2O_x, -k_CH4, -k_N2O)%>%
    rename(
      conc.ppm=CO2_x, air.ppm=CO2_air_ppm, k=k_CO2
    )%>%
    mutate(type='CO2', air.ppm=as.numeric(air.ppm))%>%
    mutate(
      air.ppm=if_else(air.ppm<345, NA, air.ppm),
      air.ppm=if_else(is.na(air.ppm), mean(air.ppm, na.rm=T), air.ppm))

  dfCH4<-k %>%
    select(-CO2_air_ppm, -CO2_x, -k_CO2, -N2O_air_ppm, -N2O_x, -k_N2O)%>%
    rename(
      conc.ppm=CH4_x, air.ppm=CH4_air_ppm, k=k_CH4
    )%>%
    mutate(type='CH4', air.ppm=as.numeric(air.ppm))%>%
    group_by(ID)%>%
    mutate(
      #air.ppm=if_else(air.ppm<345, NA, air.ppm),
      air.ppm=if_else(is.na(air.ppm), mean(air.ppm, na.rm=T), air.ppm))

  dfN2O<-k %>%
    select(-CO2_air_ppm, -CO2_x, -k_CO2, -CH4_air_ppm, -CH4_x, -k_CH4)%>%
    rename(
      conc.ppm=N2O_x, air.ppm=N2O_air_ppm, k=k_N2O
    )%>%
    mutate(type='N2O', air.ppm=as.numeric(air.ppm))%>%
    group_by(ID)%>%
    mutate(
      #air.ppm=if_else(air.ppm<345, NA, air.ppm),
      air.ppm=if_else(is.na(air.ppm), mean(air.ppm, na.rm=T), air.ppm))

  gas_samples<-rbind(dfCO2, dfCH4, dfN2O)

  uatm<-gas_samples%>%
    mutate(
      water.Temp.K=water.Temp.C+273.15,
      air.Temp.K=air.Temp.C+273.15,
      conc.atm=conc.ppm*P.air.atm,
      air.atm=air.ppm*P.air.atm)

  hs_umol<-uatm %>%
    mutate(
      air.umol=(air.atm*V_hp)/(air.Temp.K*R)
    )

  water_umol.L<-hs_umol%>%
    mutate(
      water_umol.L=
        ((conc.atm*V_wp*k)+((conc.atm*V_hp)/(R*water.Temp.K))-air.umol)/V_wp
    )

  KH<-water_umol.L %>%
    mutate(KH=0.034*exp(2400*((1/water.Temp.K)-(1/298.15))))

  water.atm<-KH%>%
    mutate(
      water.ppm=(((water_umol.L/10^6)/KH))*10^6
    )


  return(water.atm)}

#Call in gas samples#####
file.names <- list.files(path="01_Raw_data/Picarro", pattern=".csv", full.names=TRUE)
gas_samples<-data.frame()
for(i in file.names){
  runs <- read_csv(i)
  runs<-runs%>%select(SampleName, N2O_dry_mean, CO2_dry_mean, CH4_dry_mean)
  gas_samples<-rbind(runs, gas_samples)}

format<-gas_samples%>%
  rename(N2O_x=N2O_dry_mean, CO2_x=CO2_dry_mean, CH4_x=CH4_dry_mean)%>%
  separate(SampleName, into = c("Site", "Date","Rep"), sep = "_")%>%
  mutate(Date=mdy(Date))

air<-format %>% filter(Rep=='air')%>%mutate(ID=Site)%>%
  rename(N2O_air_ppm=N2O_x, CH4_air_ppm= CH4_x, CO2_air_ppm=CO2_x)%>%select(-Rep, -Site)

water<-format %>% filter(Rep!='air')%>%mutate(across(3:6,as.numeric))

#split gas samples by chapter#######
samples<-water %>% mutate(chapter=case_when(Site=='3'~'stream',Site=='5'~'stream',Site=='5a'~'stream',
                                              Site=='6'~'stream',Site=='6a'~'stream',Site=='7'~'stream',
                                              Site=='9'~'stream',Site=='13'~'stream',Site=='15'~'stream',

                                              Site=='5GW1'~'RC',Site=='5GW2'~'RC',Site=='5GW3'~'RC',Site=='5GW4'~'RC',
                                              Site=='5GW5'~'RC',Site=='5GW6'~'RC',Site=='5GW7'~'RC',Site=='6GW1'~'RC',
                                              Site=='6GW2'~'RC',Site=='6GW3'~'RC',Site=='6GW4'~'RC',Site=='6GW5'~'RC',
                                              Site=='6GW6'~'RC',Site=='9GW1'~'RC',Site=='9GW2'~'RC',Site=='9GW3'~'RC',
                                              Site=='9GW4'~'RC',Site=='9GW5'~'RC',Site=='5GW8'~'RC',

                                              Site=='5.1'~'long',Site=='5.2'~'long',Site=='5.3'~'long',
                                              Site=='5.4'~'long',Site=='5.5'~'long',Site=='5.6'~'long',

                                              Site=='6.1'~'long',Site=='6.2'~'long',
                                              Site=='6.3'~'long',Site=='3.1'~'long',Site=='3.2'~'long',Site=='3.3'~'long',
                                              Site=='3.4'~'long',

                                              Site=='9.1'~'long',Site=='9.2'~'long',Site=='9.3'~'long',Site=='9.4'~'long',
                                              Site=='9.5'~'long',Site=='9.6'~'long',Site=='9.Sam'~'long'))

stream<-samples %>%filter(chapter=='stream')
RC<-filter(samples, chapter=='RC')
long<-filter(samples, chapter=='long')


#call in complimentary data####

compiled_PT <- read_csv("01_Raw_data/PT/compiled_PT.csv")%>%
  rename(
    water.pressure.PSI=PT,
    water.Temp.F=Temp_PT)

baro <- read_csv("01_Raw_data/PT/compiled_baro.csv")%>%
  rename(
    air.Temp.F=Temp.air,
    air.pressure.PSI=PTbaro)%>%select(-ID)

P_format<-left_join(compiled_PT, baro, by=c('Date', 'region'))%>%
  mutate(hours=hour(Date), Date=as.Date(Date))%>%
  filter(hours %in% c(10:15))%>%group_by(ID, Date)%>%
  mutate(
    water.pressure.PSI=mean(water.pressure.PSI, na.rm=T),
    air.pressure.PSI=mean(air.pressure.PSI, na.rm=T),
    water.Temp.F=mean(water.Temp.F, na.rm=T),
    air.Temp.F=mean(air.Temp.F, na.rm=T)
  )%>%distinct(ID, Date, .keep_all = T)

library(weathermetrics)
P<-P_format%>%arrange(ID, Date)%>%
  fill(air.pressure.PSI, .direction='downup')%>%
  mutate(
    P.water.atm=water.pressure.PSI/conversion_PSI2atm,
    P.air.atm=air.pressure.PSI/conversion_PSI2atm,
    water.Temp.C= fahrenheit.to.celsius(water.Temp.F),
    air.Temp.C= fahrenheit.to.celsius(air.Temp.F),
    )%>%
  select(-water.pressure.PSI, -air.pressure.PSI, -region, -air.Temp.F, -water.Temp.F)

#Calculate stream##########

stream<-stream%>%rename(ID=Site)
stream.P<-left_join(stream, P)
stream.air<-left_join(stream.P, air, by=c('Date', 'ID'))

library(marelac)
stream.samples<- gas.sample.conv(stream.air)

#Calculate RW##########
P<-P%>%select(-water.Temp.C)

RC.log <- read_excel("01_Raw_data/RW.log.xlsx")%>%
  separate(Site, into = c("ID", "Well"), sep = "GW", remove = FALSE)%>%
  filter(Sampled!='N')%>%
  mutate(month=month((Date)))%>%
  group_by(month, ID)%>%
  mutate(Temp=as.numeric(Temp),
    Temp = if_else(is.na(Temp), mean(Temp, na.rm = TRUE), Temp))%>%
  select(Date, Site, Temp)%>%
  rename(water.Temp.C=Temp)

RC.all<-left_join(RC, RC.log)

RC.P<-left_join(RC.all, P)
RC.air<-left_join(RC.P, air, by=c('Date', 'ID'))

RC.samples<-gas.sample.conv(RC.air)

#Calculate Long##########

Long.Log <- read_excel("01_Raw_data/Long. Log.xlsx")%>%select(Site, ID, Temp)%>%
  rename(water.Temp.C=Temp)
long.all<-left_join(long, Long.Log)%>%mutate(ID=as.character(ID), water.Temp.C=as.numeric(water.Temp.C))

long.P<-left_join(long.all, P)

#use this for now until I have enough data to mean for NAs by long site
air.for.long<-air%>%group_by(ID)%>%mutate(
  N2O_air_ppm=mean(as.numeric(N2O_air_ppm),na.rm=T),
  CO2_air_ppm=mean(as.numeric(CO2_air_ppm),na.rm=T),
  CH4_air_ppm=mean(as.numeric(CH4_air_ppm),na.rm=T)
)%>%distinct(ID, .keep_all = T)%>%select(-Date)

long.air<-left_join(long.P, air.for.long, by=('ID'))

long.samples<-gas.sample.conv(long.air)

#combine datasets###
stream.samples.df<-stream.samples%>%
    select(ID,Date,water_umol.L,water.ppm,type,chapter)%>%rename(Site=ID)
RC.samples.df<-RC.samples%>%select(names(stream.samples.df))
long.samples.df<-long.samples%>%select(names(stream.samples.df))

write_csv(rbind(stream.samples.df, RC.samples.df, long.samples.df), "04_Output/gas.samples.csv")
