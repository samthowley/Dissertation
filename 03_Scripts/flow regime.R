library(tidyverse)

mean_daily <- function(file, value_col) {
  read_csv(file) %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Date, ID) %>%
    summarise("{value_col}" := mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
}

depth<-read_csv('02_Clean_data/depth.csv')%>%
  mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(depth=mean(depth, na.rm = T), Water_press=mean(Water_press, na.rm = T)) %>%
  select(Date, ID, depth, Water_press)%>%distinct(Date, ID, .keep_all = T)

Q<-read_csv('02_Clean_data/discharge.csv')%>%
  mutate(Date=as.Date(Date))%>% group_by(Date, ID) %>%
  mutate(Q=mean(Q, na.rm = T),
         Q_m3.s=Q/10^3) %>%
  select(Date, ID, Q, Q_m3.s)%>%
  distinct(Date, ID, .keep_all = T)

velocity <- read_csv("02_Clean_data/velocity.csv")%>%select(Date, ID, u)%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  mutate(u=mean(u, na.rm=T))%>%
  distinct(Date, ID, .keep_all = T)

baseflow <- read_csv("04_Output/baseflow.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date, ID) %>%
  mutate(baseflow = mean(baseflow, na.rm = TRUE)) %>%
  distinct(Date, ID, .keep_all = TRUE) %>% ungroup()

library(zoo)
bf<-baseflow%>%
  group_by(ID) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    bf = rollmean(baseflow, k = 25, fill = NA, align = "center", na.rm=T)) %>%
  ungroup()%>%
  select(Date, ID, bf)

ggplot(bf %>%filter(Date>"2023-11-10", ID %in% c('5', '6', '9')), aes(x=Date, y=bf, color=ID))+
  geom_line()+scale_y_log10()+
  ggtitle("Timeseries of BEF Baseflow")+
  ylab('L/s')




K600 <- read_csv("04_Output/master_metabolism.csv")%>%
  select(ID, date, K600)%>% rename(Date=date)

qL<- mean_daily("02_Clean_data/qL.csv", "qL_m2.sec")%>%mutate(ID=as.character(ID))

flow.list <- list(depth, Q, velocity, bf, K600, qL)
flow.regime <- reduce(flow.list, left_join, by=c('Date', 'ID'))%>%
  group_by(ID)%>%
  mutate(K600=if_else(is.na(K600), mean(K600, na.rm=T), K600))%>%
           ungroup()%>%
  mutate(
    width=Q_m3.s/(u*depth),
    reach=(0.7*(u*86400)/K600),
    A=reach*width)



write_csv(flow.regime, "04_Output/flow_regime_daily.csv")

ggplot(flow.regime, aes(x=Date, y=qL_m2.sec))+
  geom_point()+
  facet_wrap(~ID, scales="free")


