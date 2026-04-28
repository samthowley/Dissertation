
library(tidyverse)
library(measurements)
library(readxl)

volume.cyl.cm<-conv_unit(3.14*1.5^2*4, 'in', 'cm')
volume.hammer.cm<-3.14*2.54^2*45

SOC_log <- read_excel("01_Raw_data/SOC log.xlsx", sheet = "Carbon Content ")

#BD=g/cm^3
MC <- read_excel("01_Raw_data/SOC log.xlsx", sheet = "Moisture Content")%>%
  mutate(
    BD=if_else(Method=='cyl', `total mass`*volume.cyl.cm, `total mass`*volume.hammer.cm)
  )

MC%>%
  ggplot(aes(x=Depth, y=BD, color=Location))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~Method, scales='free')


LOI<-SOC_log%>%
  mutate(
    fraction.LOI=`LOI OM`/`boat+dry soil`,
    C.fraction=fraction.LOI*0.45)%>%
  filter(fraction.LOI>0)%>%
  full_join(MC)%>%
  mutate(
    Depth=if_else(Depth=='10t20', '10-20', Depth),
    C.content=C.fraction*`total mass`
    )%>%
  separate(Depth, into = c("up", "low"), sep = "-", remove=F)%>%
  mutate(
    low=as.numeric(low),
    C.Stock=(C.fraction*BD*low)/10^4)


LOI%>%
  ggplot(aes(x=Depth, y=C.fraction, color=Location))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~Method, scales='free')


LOI%>%
  ggplot(aes(x=Depth, y=C.Stock, color=Location))+
  geom_boxplot()+
  geom_jitter()+
  facet_wrap(~Method, scales='free')
