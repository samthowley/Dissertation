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
file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(8, 7, 6)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))%>%
  mutate(date=as.Date(Date))

gw_corrected <- read_csv("04_Output/stream/gw_corrected_metabolism.csv")%>%
  rename(date=Date)%>%
  filter(NEP_corrected<0)

gw_corrected<-left_join(master, gw_corrected)

KH<-gw_corrected %>%
  mutate(Temp_C=fahrenheit.to.celsius(Temp_DO),
         Temp_K=Temp_C+273.15,
         KH=0.034*exp(2400*((1/Temp_K)-(1/298.15))))

KCO2<-KH %>%
  mutate(
    K600_m.d=K600*depth,
    SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3,
    KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3)),
    KCO2_d=KCO2_m.d/depth
    )%>%
  rename(day=Date)

CO2<-read_csv("02_Clean_data/CO2.csv")%>%
  mutate(date=as.Date(Date))

flux<-left_join(CO2,KCO2, by=c('date','ID'))%>%
  group_by(Date,ID)%>%
  mutate(
    CO2_day=mean(CO2, na.rm = T))%>%
  ungroup()%>%
  mutate(
    CO2_flux=KCO2_m.d*(CO2_day-422)*KH*(1/10^6)*12*10^3,
    )%>%
  distinct(date,ID, .keep_all = T)


pathways<-flux%>%
  mutate(
    internal=NEP_corrected*(-12*1.2)/32,
    internal=if_else(NEP_corrected>0, 0, internal),

    external=abs(CO2_flux-internal),
    int.ext.ratio=internal/external,

    Basin=case_when(ID=='5'~'5',ID=='5a'~'5',ID=='15'~'15',
                         ID=='3'~'6',ID=='7'~'7',ID=='6'~'6',ID=='6a'~'6',
                         ID=='9'~'9', ID=='13'~'13')
    )%>%
  filter(!ID=='6a', !is.na(ID))%>%
  select(ID, Date, CO2, day, K600, depth, Q, CO2_flux,
         external, internal, int.ext.ratio, NEP_corrected,
         ER_corrected)


#ggplotly()

ggplot(
  pathways,
  aes(x = Q)) +
  scale_y_log10()+scale_x_log10()+
  geom_point(aes(y = internal), color='red') +
   geom_point(aes(y = external), color='black') +
  # geom_point(aes(y = CO2_flux), color='purple') +
  facet_wrap(~ID, ncol = 4, scales = 'free')

mean(pathways$internal, na.rm=T)
mean(pathways$external, na.rm=T)
range(pathways$NEP_corrected, na.rm=T)
pathways%>%
  group_by(ID)%>%
  summarise(
    mean=mean(internal/CO2_flux, na.rm=T)
  )

write_csv(pathways, "04_Output/stream/external-internal.csv")

