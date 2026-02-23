library(weathermetrics)
library(tidyverse)
library(writexl)
library(readxl)

strm.CO2 <- subset(gas, type == "CO2")%>%rename(CO2.water_umol.L=water_umol.L, CO2.water.ppm=water.ppm)%>%
  filter(chapter=='stream')%>%select(-type, -chapter)

flow.regime <- read_csv("04_Output/flow_regime_daily.csv")%>%
  left_join(read_csv("02_Clean_data/temperature.csv"))%>%
  mutate(Date=as.Date(Date))%>%
  mutate(Temp_C=fahrenheit.to.celsius(mean(Temp_PT, na.rm=T)))%>%
  distinct(Date, ID, .keep_all = T)

lateral.flux<-left_join(strm.CO2, flow.regime)%>%
  mutate(
    K600_m.d=K600*depth,
    SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3,
    KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3)),
    KCO2_d=KCO2_m.d/depth,
    CO2_g.m2.day=KCO2_m.d*(CO2.water_umol.L)*44*10^-3)%>%
  select(Date, ID, CO2_g.m2.day)%>%distinct(ID, Date, .keep_all = T)

