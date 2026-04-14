source("03_Scripts/Streams/analysis/data for analysis.R")

Q<-discharge%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(
    Q=mean(Q, na.rm=T)
  )


DOC<-DOC%>%rename(Date=day)
DOC.Q<-left_join(DOC, Q, by=c('ID', 'Date'))%>%
  filter(Q>0.9)


fdom<-fdom%>%rename(Date=day)
fdom.Q<-left_join(fdom, Q, by=c('ID', 'Date'))%>%
  filter(!is.na(Q))



left_join(fdom, int.ext)%>%
  filter(!is.na(Q))%>%
  ggplot(aes(x=hix))+
  scale_y_log10()+scale_x_log10()+
  geom_point(aes(y=internal), color='red')+
  geom_point(aes(y=external), color='black')+
  facet_wrap(~ID, scales='free')

left_join(DOC, int.ext)%>%
  ggplot(aes(x=DOC))+
  geom_point(aes(y=internal), color='red')+
  geom_point(aes(y=external), color='black')+
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ID, scales='free')






DOC.Q%>%
  ggplot(aes(x=Q, y=DOC))+
  geom_point()+
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ID, scales='free')

rC <- lmList(log10(Q) ~ log10(DOC) | ID, data=DOC.Q)
summary(rC)
(cf <- coef(rC))


rC <- lmList(log10(Q) ~ bix | ID, data=fdom.Q)
summary(rC)
(cf <- coef(rC))
