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
library(outliers)

#Internal Pathway#####
data <- lapply(
  c("02_Clean_data/depth.csv",
    "02_Clean_data/discharge.csv"),
  function(x) read_csv(x, col_types = cols(ID = col_character()))
)

master <- reduce(data, full_join, by = c("ID", 'Date'))%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(
    TempC=fahrenheit.to.celsius(mean(Temp_PT, na.rm=T)),
    Q=mean(Q, na.rm=T),
    depth=mean(depth, na.rm=T)
  )


gw_corrected <- read_csv("04_Output/stream/gw_corrected_metabolism.csv")

gw_corrected<-left_join(master, gw_corrected)%>%
  filter(!is.na(NEP_corrected))

KH<-gw_corrected %>%
  mutate(
         Temp_K=TempC+273.15,
         KH=0.034*exp(2400*((1/Temp_K)-(1/298.15))))


KCO2<-KH %>%
  mutate(
    K600_m.d=K600*depth,
    SchmidtCO2hi=1742-91.24*TempC+2.208*TempC^2-0.0219*TempC^3,
    KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3)),
    KCO2_d=KCO2_m.d/depth
    )


CO2<-read_csv("02_Clean_data/CO2.csv")%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(CO2=mean(CO2, na.rm=T))


flux<-left_join(KCO2,CO2, by=c('Date','ID'))%>%
  mutate(
    CO2_flux=KCO2_m.d*(CO2-422)*KH*(1/10^6)*12*10^3,
    )%>%
  distinct(Date,ID, .keep_all = T)%>%
  filter(!is.na(CO2_flux))



pathways<-flux%>%
  mutate(
    internal=NEP_corrected*(-12*1.2)/32,

    external=abs(CO2_flux-internal),
    )%>%
  filter(!ID=='6a')%>%
  select(ID, Date, CO2, K600, depth, Q, TempC, CO2_flux,
         external, internal, NEP_corrected,
         ER_corrected)



internal.contrib<-pathways%>%
  mutate(
    int.contrib=100*internal/CO2_flux, na.rm=T,
    ext.contrib=100*external/CO2_flux, na.rm=T,
    int.contrib=if_else(int.contrib>100, 100, int.contrib),
    ext.contrib=if_else(ext.contrib>100, 100, ext.contrib),
    int.contrib=if_else(int.contrib<0, 0, int.contrib),
  )
range(internal.contrib$int.contrib, na.rm=T)



#ggplotly()

ggplot(
  internal.contrib,
  aes(x = Q)) +
  scale_y_log10()+scale_x_log10()+
  geom_point(aes(y = int.contrib), color='red') +
   geom_point(aes(y = ext.contrib), color='black') +
  # geom_point(aes(y = CO2_flux), color='purple') +
  facet_wrap(~ID, ncol = 4, scales = 'free')

mean(pathways$internal, na.rm=T)
mean(pathways$external, na.rm=T)

pathways%>%
  summarise(
    mean=mean(external/CO2_flux, na.rm=T)
  )

write_csv(internal.contrib, "04_Output/stream/external-internal.csv")

