#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(weathermetrics)
library(ggtern)
library(ggpmisc)
library(plotly)
theme_set(theme(axis.text.x = element_text(size = 12),
                axis.text.y = element_text(size = 17),
                axis.title.y = element_text(size = 17, angle = 90),
                axis.title.x = element_text(size = 17),
                plot.title = element_text(size = 17),
                legend.key.size = unit(0.5, 'cm'),
                legend.text=element_text(size = 12),
                legend.title =element_blank(),
                legend.position ="bottom",
                panel.grid.major.x = element_line(color = "black"),  # Customize x-axis major gridlines
                panel.grid.minor.y = element_line(color = "black", linetype = "dashed"),
                panel.background = element_rect(fill = 'white'),
                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray")))

flow_regime_daily <- read_csv("04_Output/flow_regime_daily.csv")
#include gas samples#######

strm_gas <- read_csv("04_Output/gas.samples.csv")

CO2 <- subset(strm_gas, type == "CO2" & chapter=='stream')%>%rename(CO2.water_umol.L=water_umol.L, CO2.water.ppm=water.ppm)%>%select(-type, -chapter)
CH4 <- subset(strm_gas, type == "CH4" & chapter=='stream')%>%rename(CH4.water_umol.L=water_umol.L, CH4.water.ppm=water.ppm)%>%select(-type, -chapter)

gas<-full_join(CO2, CH4)

#sample C##########

shimadzu<-read_csv('04_Output/TDC_stream.csv')

alkalinity <- read_csv("02_Clean_data/alkalinity.csv")
alkalinity_edited<-alkalinity%>%select(Date, ID, DIC_mgL, CO2_mgL, HCO3_mgL, CO3_mgL)

combined<-left_join(shimadzu, alkalinity_edited, by=c('Date', 'ID'))

totC<-combined %>%distinct(Date, ID, .keep_all = T)%>%
  mutate(DIC = if_else(is.na(DIC), as.numeric(DIC_mgL), DIC))%>%
  select(Date, ID, DIC, DOC, POC, ID, depth, pH, Q, CO2, Temp_pH)%>%
  mutate(POC=abs(POC))

totC<-totC %>% mutate(TotalC=DIC+DOC+POC)%>%
  mutate(DIC_perc=DIC/TotalC, DOC_perc=DOC/TotalC, POC_perc=POC/TotalC)

totC_gas<-full_join(totC,gas)

#FDOM##############
eem_stream <- read_csv("04_Output/eem_stream.csv")%>%select(-Rep, -chapter, -Site)

all_sampled_C<-full_join(totC_gas,eem_stream)%>%select(-depth, -Q)

write_csv(all_sampled_C, "04_Output/stream_sampledC.csv")
