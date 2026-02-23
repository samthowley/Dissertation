#packages#####
library(tidyverse)
library(writexl)
library(readxl)
library(weathermetrics)
library(cowplot)
library(plotly)
library(units)
library(tools)
library(plotly)


samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2024-05-06 00:00", tz="UTC"),
                                            to=as.POSIXct("2026-01-06 00:00", tz="UTC"),by="hour")))
CO2<-data.frame()

file.names <- list.files(path="01_Raw_data/Lily Box/csv", pattern=".csv", full.names=TRUE)
for(i in file.names){
  LB <- read_csv(i)
  LB<-LB[,c(1,5)]
  colnames(LB)[2] <- "CO2"

  LB$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][4]
  LB$source_file <- basename(i)
  LB$file_type <- tools::file_ext(i)

  fname <- basename(i)
  sampling.date <- mdy(str_extract(fname, "\\d{8}"))
  LB$sampling.date <- sampling.date

  CO2<-rbind(CO2, LB)}
CO2<-CO2%>%mutate(Date=mdy_hm(Date))

file.names <- list.files(path="01_Raw_data/Lily Box/dat", pattern=".dat", full.names=TRUE)
for(i in file.names){
  LB <- read_csv(i, skip= 1,col_types = cols(`31031` = col_number()))

  LB<-LB[-c(1:2),]

  columns_to_keep <- c("TIMESTAMP", "CO2High", "Eosense", 'CO2')
  cols_present <- intersect(columns_to_keep, names(LB))
  LB<-LB[, cols_present, drop = FALSE] #keep columns from columns_to_keep if present
  LB<-LB[,c(1:2)]


  colnames(LB)[1] <- "Date"
  colnames(LB)[2] <- "CO2"
  LB$ID<-strsplit(basename(i), '_')[[1]][1]
  LB$source_file <- basename(i)
  LB$file_type <- tools::file_ext(i)
  LB<-LB %>% mutate(Date=ymd_hms(Date))

  fname <- basename(i)
  sampling.date <- mdy(str_extract(fname, "\\d{8}"))
  LB$sampling.date <- sampling.date

  CO2<-rbind(CO2, LB)}
CO2<-CO2 %>% mutate(CO2=as.numeric(CO2))

file.names <- list.files(path="01_Raw_data/Lily Box/dat/3", pattern=".dat", full.names=TRUE)
for(i in file.names){
  LB <- read_csv(i, skip= 1)

  LB<-LB[-c(1:2),]

  columns_to_keep <- c("TIMESTAMP", "CO2High", "Eosense", 'CO2')
  cols_present <- intersect(columns_to_keep, names(LB))
  LB<-LB[, cols_present, drop = FALSE] #keep columns from columns_to_keep if present
  LB<-LB[,c(1:2)]


  colnames(LB)[1] <- "Date"
  colnames(LB)[2] <- "CO2"
  LB$ID<-strsplit(basename(i), '_')[[1]][1]
  LB$source_file <- basename(i)
  LB$file_type <- tools::file_ext(i)

  fname <- basename(i)
  sampling.date <- mdy(str_extract(fname, "\\d{8}"))
  LB$sampling.date <- sampling.date

  LB<-LB %>% mutate(Date=ymd_hms(Date),CO2=as.numeric(CO2))
  CO2<-rbind(CO2, LB)}

CO2<-CO2%>%
  drop_na()%>%
  filter(ID !='14')%>%
  mutate(
    sampling.year=year(sampling.date),
    year=year(Date)
  )%>%filter(year==sampling.year)

#clean######

sites<-split(CO2,CO2$ID)

s5<-sites[['5']]
s5<- #program has multiplier; every day after 0509 needs to be multiplied by 2
  s5%>%
  filter(!source_file %in% c("5_Bradford_5_08132025.dat","5_Bradford_5_09262025.dat",
                             "5_Bradford_5_12052025.dat", "5_Bradford_5_07012025.dat")
        )%>%
  mutate(
    day=as.Date(Date),
    CO2=if_else(day>="2025-07-01", CO2*6, CO2),
    CO2=if_else(day=='2024-09-25' & CO2>10500, NA, CO2),
    CO2=if_else(day=='2024-03-21' & CO2>10000, NA, CO2),
    CO2=if_else(day=='2024-04-09' & CO2>9500, NA, CO2),
    CO2=if_else(day=='2024-09-25' & CO2>2500, NA, CO2),
    CO2=if_else(day=='2024-09-27' & CO2>3000, NA, CO2),
    CO2=if_else(day=='2024-09-28' & CO2>3000, NA, CO2),
    CO2=if_else(day=='2024-10-23' & CO2>10060, NA, CO2),
    CO2=if_else(day=='2024-10-24' & CO2>2060, NA, CO2),
    CO2=if_else(day=='2024-10-25' & CO2>2060, NA, CO2),
    CO2=if_else(day=='2024-10-19' & CO2>2060, NA, CO2),
    CO2=if_else(day=='2024-10-04' & CO2>15000, NA, CO2),
    )%>%
  filter(CO2>2000&CO2<20000)%>%
  group_by(ID, Date)%>%
  summarise(CO2=mean(CO2, na.rm=T))%>%
  select(ID, Date, CO2)
  mean(s5$CO2, na.rm=T)


s15<-sites[['15']]
s15<- #program has multiplier offset leave as is
  s15%>%
  filter(CO2>1700, CO2<8800)%>%
  distinct(ID, Date, CO2)%>%
  mutate(CO2=CO2*2.2)%>%
  distinct(ID, Date, CO2)
mean(s15$CO2, na.rm=T)


s7<-sites[['7']]
s7<-s7%>% #messy but neccessary
  filter(Date<'2025-01-01', CO2>1700)%>%
  mutate(
    day=as.Date(Date),
    CO2=if_else(day=='2024-09-15' & CO2>17000, NA, CO2),
    CO2=CO2/2,
    CO2=if_else(day<='2023-12-18', CO2/2, CO2),
    CO2=if_else(day<='2023-09-09', CO2/1.2, CO2),
    CO2=if_else(day>='2024-04-18' & day<='2024-05-23', CO2/4.2, CO2),
    CO2=CO2*6.6
  )%>%
  distinct(ID, Date, CO2)
mean(s7$CO2, na.rm=T)


s5a<-sites[['5a']]
s5a<- #new program after 07/09/2024
  s5a%>%
  filter(CO2>11000)%>%
  mutate(
    day=as.Date(Date),
    #CO2=if_else(day>'2024-07-29', CO2/2.1, CO2),
    CO2=CO2
  )%>%
  distinct(ID, Date, CO2)
mean(s5a$CO2, na.rm=T)


s3<-sites[['3']]
s3<-s3%>%
filter(
  CO2>1400, CO2<15000,
  source_file != '3_Bradford_3_10042024.dat')%>%
  mutate(
    CO2=if_else(Date<'2023-09-28', CO2/2, CO2),
    CO2=CO2*3
  )%>%
  distinct(ID, Date, CO2)
mean(s3$CO2, na.rm=T)



s9<-sites[['9']]
s9<- s9%>%
  filter(
    CO2>1200, CO2<11000)%>%
  mutate(CO2=CO2*2)%>%
  distinct(ID, Date, CO2)
mean(s9$CO2, na.rm=T)


s13<-sites[['13']]
s13<-
s13%>%
  mutate(
    CO2=if_else(Date>'2025-07-01', CO2, CO2),
    CO2=if_else(Date>'2024-05-30' & Date< '2025-02-24', CO2/2, CO2),
    CO2=CO2,
  )%>%
  filter(
    CO2>2100, CO2< 15000)%>%
  mutate(CO2=CO2*4.2)%>%
  distinct(ID, Date, CO2)
mean(s13$CO2, na.rm=T)


s6<-sites[['6']]
s6<-
  s6%>%
  mutate(
    day=as.Date(Date),
    CO2=if_else(day<='2024-08-14', CO2/6, CO2),
    CO2=if_else(day=='2024-04-22' & CO2>2940, NA, CO2),
    CO2=if_else(day=='2024-04-23' & CO2>2940, NA, CO2)
  )%>%
  filter(CO2>900)%>%
  mutate(CO2=CO2*6)%>%
  distinct(ID, Date, CO2)
mean(s6$CO2, na.rm=T)

# ggplotly(
#   s6%>%
#     filter(CO2>1000)%>%
#     mutate(
#       day=as.Date(Date),
#       CO2=if_else(day<='2024-08-14', CO2/6, CO2),
#       CO2=if_else(day=='2024-04-22' & CO2>2940, NA, CO2),
#       CO2=if_else(day=='2024-04-23' & CO2>2940, NA, CO2),
#
#     )%>%
#     ggplot(aes(x=Date, y=CO2))+
#     geom_point()
# )



# ggplot(CO2.clean,  aes(x=Date, y=CO2))+
#   geom_point()+
#   facet_wrap(~ID, scales='free')


write_csv(rbind(s5,s5a,s15,s6,s7,s3,s13,s9), "02_Clean_data/CO2.csv")


#include gas samples####

read_csv("04_Output/gas.samples.csv")%>% filter(type=='CO2')%>%
 rename(CO2=water.ppm)%>%
  filter(chapter=='stream')%>%
  ggplot(aes(x=Date, y=CO2))+
  geom_point()+
  facet_wrap(~Site, scales='free')
