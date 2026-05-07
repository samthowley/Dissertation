
library(tidyverse)
library(measurements)
library(readxl)

volume.cyl.cm<-conv_unit(1.5, 'in', 'cm')^2*conv_unit(4, 'in', 'cm')*3.14
volume.hammer.cm<-3.14* conv_unit(1, 'in', 'cm')^2 *45

SOC_log <- read_excel("01_Raw_data/SOC log.xlsx", sheet = "Carbon Content ")

#BD=g/cm^3
MC <- read_excel("01_Raw_data/SOC log.xlsx", sheet = "Moisture Content")%>%
  mutate(
    BD=if_else(Method=='cyl', `total mass`*(volume.cyl.cm/10^6), `total mass`*(volume.hammer.cm/10^6))
  )

MC %>%
  ggplot(aes(x = Depth, y = BD, color = Location)) +
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge()) +
  facet_wrap(~Method, scales = 'free')+
  ylab(expression(paste("Bulk Density"~~(g~m^-3))))


LOI<-SOC_log%>%
  mutate(
    fraction.LOI=`LOI OM`/`boat+dry soil`)%>%
  full_join(MC)%>%
  mutate(
    Depth=if_else(Depth=='10t20', '10-20', Depth),
    LOI.content=fraction.LOI*`total mass`
    )%>%
  separate(Depth, into = c("up", "low"), sep = "-", remove=F)%>%
  mutate(
    low=as.numeric(low),
    LOI.Stock=(fraction.LOI*BD*low),
    volume=case_when(
      Method=='cyl' ~ volume.cyl.cm,
      Method=='hammer' ~ volume.hammer.cm),
    volume.m3=volume/10^6,
    c.volume=LOI.content/volume.m3
    )


LOI%>%
  filter(`LOI OM`<5)%>%
  ggplot(aes(x=Depth, y=fraction.LOI, color=Location))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge()) +
  facet_wrap(~Method, scales='free')+
  labs(y='LOI OM (g)',
       title='LOI OM (g) by Depth and Method')

LOI%>%
  filter(`LOI OM`<5)%>%
  ggplot(aes(x=Depth, y=`LOI OM`, color=Location))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge()) +
  facet_wrap(~Method, scales='free')+
  labs(y='LOI OM (g)',
       title='LOI OM (g) by Depth and Method')


LOI%>%
  filter(`LOI OM`<5)%>%
  ggplot(aes(x=Depth, y=LOI.content,  color=Location))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge()) +
  facet_wrap(~Method, scales='free')+
  labs(y='LOI OM (g)/ Total Core Mass',
       title='LOI Fraction by Depth and Method')


LOI%>%
  filter(`LOI OM`<5)%>%
  ggplot(aes(x=Depth, y=c.volume, color=Location))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge()) +
  facet_wrap(~Method, scales='free')+
  labs(y=expression(g ~ m^-3),
       title='LOI OM by Volume')

LOI%>%
  filter(`LOI OM`<5)%>%
  ggplot(aes(x=Depth, y=LOI.Stock, color=Location))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge()) +
  facet_wrap(~Method, scales='free')+
  labs(y=expression(g^2 ~ m^2),
       title='LOI OM Stock')

 