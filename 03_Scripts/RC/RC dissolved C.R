library(tidyverse)
library(writexl)
library(readxl)

RC_flow_regime <- read_csv("04_Output/RC/RC.flow.regime.csv")

sampled_solid_carbon <- read_csv("04_Output/sampled.solid.carbon.csv")%>%
  filter(chapter=='RC')%>%
  select(-TNPOC, -POC, -chapter)


RC.dissolved.c<-left_join(RC_flow_regime, sampled_solid_carbon)%>%
  mutate(
    DOC_g.m2.day=DOC*qL_m2.sec*(1/stream.w)*86400,
    DIC_g.m2.day=DIC*qL_m2.sec*(1/stream.w)*86400
  )%>%
  select(-stream.w, -Temp)

strm.carbon <- read_csv("04_Output/sampled.solid.carbon.csv")%>%
  filter(chapter=='stream', ID %in% c('5', '6', '9'))%>%
  mutate(
    distance.from.stream=0.01,
    GW=0,
    WTdepth=NA,
    WTE=0,
    Well=0,
    well_types='stream',
    qL_m2.sec=NA,
    DOC_g.m2.day=NA,
    DIC_g.m2.day=NA,
    Site=paste0(ID, 'GW', Well)
  )%>%select(names(RC.dissolved.c))

RC.DC<-rbind(RC.dissolved.c, strm.carbon)

write_csv(RC.DC, "04_Output/RC/RC.DC.sample.flux.csv")


