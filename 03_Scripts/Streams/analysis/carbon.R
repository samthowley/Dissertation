source("03_Scripts/Streams/analysis/data for analysis.R")

Q<-discharge%>%mutate(day=as.Date(Date))%>%
  group_by(ID, day)%>%
  summarise(
    Q=mean(Q, na.rm=T)
  )



left_join(fdom, Q)%>%
  filter(!is.na(Q))%>%
  ggplot(aes(x=Q, y=bix))+
  scale_y_log10()+scale_x_log10()+
  geom_point()+
  lm.common+
  facet_wrap(~ID, scales='free')

left_join(DOC, Q)%>%
  ggplot(aes(x=Q, y=DOC))+
  geom_point()+
  scale_y_log10()+scale_x_log10()+
  lm.common+
  facet_wrap(~ID, scales='free')
