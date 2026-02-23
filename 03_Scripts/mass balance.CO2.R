library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)
library(ggpmisc)
library('StreamMetabolism')
library(hydroTSM)

#fCO2####

external_internal <- read_csv("04_Output/stream/external-internal.csv")%>%
  select(Date, ID, CO2_flux)%>%
  rename(fCO2=CO2_flux)

stream_gas_sample_flux <- read_csv("04_Output/stream/stream.gas.sample.flux.csv")%>%
  rename(fCO2=CO2_g.m2.day)

fco2<-rbind(external_internal, stream_gas_sample_flux)%>%
  mutate(fCO2=fCO2*-1)

#metabolism########

NEP <- read_csv("04_Output/stream/gw_corrected_metabolism.csv")%>%
  select(Date, ID, GPP, ER)%>%
  mutate(
    ER=ER*-1,
    GPP=GPP*-1
  )

#latercal CO2#######

RC_gas_sample_flux <- read_csv("04_Output/RC/RC.gas.sample.flux.csv")%>%
  group_by(Site, Date)%>%
  summarise(
    CO2_flux=mean(CO2_flux, na.rm=T)
  )%>%
  mutate(
    location=
      case_when(
        Site=="5GW8" ~ 'TER',
        Site=="6GW5"~'TER',
        Site=='9GW5'~'TER',
        .default = "RIP"
      )
  )%>%
  separate(Site, into = c("ID", "Well"), sep = "GW")


TER<-RC_gas_sample_flux%>%
  filter(location=='TER')%>%
  rename(TER=CO2_flux)%>%
  select(Date, ID, TER)


RIP<-RC_gas_sample_flux%>%
  filter(location=='RIP')%>%
  rename(RIP=CO2_flux)%>%
  select(Date, ID, RIP)

gas.samples<-full_join(TER, RIP)

#combine#####

mass.balance.list <- list(RIP, TER, fco2, NEP)
mass.balance <- reduce(mass.balance.list, left_join, by=c('Date', 'ID'))%>%
  filter(complete.cases(fCO2))

mb.long <- pivot_longer(mass.balance,
                        cols = c(RIP, fCO2, GPP, ER, TER),
                        names_to = "Cat",
                        values_to = "Flux")%>%
  group_by(ID, Date)%>%
  mutate(total=sum(Flux, na.rm=T))

check<-mb.long%>%filter(ID=='5', Date=='2025-01-10')


ggplot(mb.long, aes(x = Date, y = Flux, color = Cat)) +
  geom_point()+
  geom_point(aes(y=total), color='red') +
  ylab(expression(CO[2]~g/m^2/day))+
  facet_wrap(~ID)

plot_grid(
  ggplot(mb.long, aes(x = ID, y = Flux, fill = Cat)) +
    geom_bar(stat = "identity") +
    ylab(expression(CO[2]~g/m^2/day)),

  ggplot(mb.long, aes(x = Date, y = Flux, fill = Cat)) +
            geom_bar(stat = "identity") +
            ylab(expression(CO[2]~g/m^2/day))+facet_wrap(~ID),

          ncol=1)




#write_csv(mass.balance, "mass.balance.csv")

