###packages####
library(tidyverse)
library(writexl)
library(readxl)
library(seacarb)
library(weathermetrics)

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,4,8,11)]
data <- lapply(file.names,function(x) {read_csv(x)})
merged_data <- reduce(data, left_join, by = c("ID", 'Date'))

master<-merged_data %>% rename('Temp'='Temp_PT.x')%>% select(Date, ID, CO2, pH, Temp, Water_press, depth)%>%
  filter(CO2>0)%>%filter(Water_press>0)

master <- master[complete.cases(master[ , c('Temp','CO2','pH','Water_press')]), ]

temp<-master %>% mutate(Temp=fahrenheit.to.celsius(Temp)) %>%mutate(Temp_K=Temp+273.15)

KH<-temp %>% mutate(exp=2400*((1/Temp_K)-(1/298.15))) %>%mutate(KH=0.034*2.178^(exp))%>%
  mutate(CO2_atm=CO2/10^6, CO2_molL=CO2_atm*KH)

KH$K1<-K1(S=0.01, T=KH$Temp, P=KH$Water_press)
KH$K2<-K2(S=0.01, T=KH$Temp, P=KH$Water_press)

DIC<-KH %>%
  mutate(H=10^(-1*pH)) %>%
  mutate(HCO3_molL=(K1*CO2_molL)/H,
         CO3_molL=(K1*K2*CO2_molL)/H^2) %>%
  mutate(CO2_mgL=CO2_molL*44*1000,
         HCO3_mgL=HCO3_molL*61*1000,
         CO3_mgL=CO3_molL*60*1000)%>%
  mutate(DIC_mgL=CO2_mgL+CO3_mgL+HCO3_mgL)%>%
  mutate(DIC_mmol= (CO2_molL+CO3_molL+HCO3_molL)*1000)%>%
  select(Date,ID,pH, CO2, DIC_mgL,CO2_mgL,HCO3_mgL,CO3_mgL)%>%
  mutate(DIC_mgL=if_else(ID=='6a'& DIC_mgL>100, NA, DIC_mgL))

ggplot(DIC, aes(x=Date)) + geom_point(aes(y=DIC_mgL))+facet_wrap(~ ID, ncol=5, scale='free')

# s9<-DIC %>%filter(ID=='9')
# ggplot(s9, aes(x=Date)) + geom_point(aes(y=pH))

write_csv(DIC, "02_Clean_data/alkalinity.csv")

north<-DIC%>%filter(ID %in% c('5','5a','15','7'))
south<-DIC%>%filter(ID %in% c('3','6','6a','9','13'))

a<-ggplot(north, aes(x=Date)) + geom_point(aes(y=CO2))+facet_wrap(~ ID, ncol=5, scale='free')
b<-ggplot(north, aes(x=Date)) + geom_point(aes(y=pH))+facet_wrap(~ ID, ncol=5, scale='free')
c<-ggplot(north, aes(x=Date)) + geom_point(aes(y=DIC_mgL))+facet_wrap(~ ID, ncol=5, scale='free')
plot_grid(a,b,c, ncol=1)


a<-ggplot(south, aes(x=Date)) + geom_point(aes(y=CO2))+facet_wrap(~ ID, ncol=5, scale='free')
b<-ggplot(south, aes(x=Date)) + geom_point(aes(y=pH))+facet_wrap(~ ID, ncol=5, scale='free')
c<-ggplot(south, aes(x=Date)) + geom_point(aes(y=DIC_mgL))+facet_wrap(~ ID, ncol=5, scale='free')
plot_grid(a,b,c, ncol=1)
