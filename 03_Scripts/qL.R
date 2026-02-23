library(tidyverse)

baseflow <- read_csv("04_Output/baseflow.csv")%>%
  mutate(baseflow_m3.sec=baseflow/10^3)%>%
  select(ID, Date, baseflow_m3.sec)

uca <- data.frame(
  ID = c('5', '6', '9'),
  UCA = c(2e-4, 1e-4, 1e-4))

qL<-left_join(baseflow, uca)%>%
  mutate(
    UCA=if_else(is.na(UCA), mean(UCA, na.rm=T), UCA),
    qL_m2.sec=UCA*baseflow_m3.sec,
    min=minute(Date)
    )%>%
  select(ID, Date,qL_m2.sec)


write_csv(qL, "02_Clean_data/qL.csv")


ggplot(qL, aes(x=Date, y=qL_m2.sec))+
  geom_point()+
  facet_wrap(~ID, scales="free")

