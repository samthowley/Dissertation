#BY sam
#packages#####
library(tidyverse)
library(writexl)
library(readxl)
library(weathermetrics)
library(measurements)
library(cowplot)
library(tools)


samplingperiod <- data.frame(Date = rep(seq(from=as.POSIXct("2023-10-06 00:00", tz="UTC"),
                                 to=as.POSIXct("2025-12-06 00:00", tz="UTC"),by="hour")))

clean_DO <- function(fil) {
  DO <- read_csv(fil,skip= 1) # read csv
  DO<-DO[,c(2,3,4)] #these columns
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp_DO"
  DO$Date <- mdy_hms(DO$Date)
  DO<-left_join(DO, samplingperiod, by='Date')
  DO$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][6]
  return(DO)}
MiniDot_DO<-function(fil){
  DO <- read_csv(i,skip= 8)
  DO<-DO[,c(2,6,5)]
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp_DO"
  DO<-left_join(DO, samplingperiod, by='Date')
  DO$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][6]

  # for(i in 1:nrow(DO)){
  # if(DO$DO[i]<=0 | DO$DO[i]>=7.4) { DO$DO[i]<- NA}
  # else {DO$DO[i]<- DO$DO[i]-0 }}
  return(DO)}
clean_SpC <- function(fil) {
  SpC <- read_csv(fil, skip= 1)
  SpC<-SpC[,c(2,3,4)]
  colnames(SpC)[1] <- "Date"
  colnames(SpC)[2] <- "SpC"
  colnames(SpC)[3] <- "Temp_SpC"
  SpC$Date <- mdy_hms(SpC$Date)
  SpC<-left_join(SpC, samplingperiod, by='Date')
  SpC$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][6]
  return(SpC)
}
clean_pH <- function(i) {
  pH <- read_xlsx(i)
  pH<-pH[,c(2,5,3)]
  colnames(pH)[1] <- "Date"
  colnames(pH)[2] <- "pH"
  colnames(pH)[3] <- "Temp_pH"
  pH$Temp_pH<-celsius.to.fahrenheit(pH$Temp_pH)
  #pH<-filter(pH, pH<6.2) #remove hours out of water
  pH$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][6]
  return(pH)}

theme_set(theme(axis.text.x = element_text(size = 12, angle=0),
                             axis.text.y = element_text(size = 17, angle=0),
                             axis.title =element_text(size = 17, angle=0),
                             plot.title = element_text(size = 17, angle=0),
                             legend.key.size = unit(0.8, 'cm'),
                             legend.text=element_text(size = 17),
                             legend.title =element_text(size = 17),
                             legend.position ="none",
                             panel.background = element_rect(fill = 'white'),
                             axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))

####DO######
file.names <- list.files(path="01_Raw_data/HOBO Excels/DO", pattern=".csv", full.names=TRUE)

DO_all<-data.frame()
for(i in file.names){
  DO<-clean_DO(i)
  DO_all<-rbind(DO_all, DO)
  DO_all[order(as.Date(DO_all$date, format="%Y-%m-%d %H:%M:%S")),]
}

file.names <- list.files(path="01_Raw_data/MiniDot", pattern=".TXT", full.names=TRUE)
for(i in file.names){
  DO<-MiniDot_DO(i)
  DO_all<-rbind(DO_all, DO)
  DO_all[order(as.Date(DO_all$date, format="%Y-%m-%d %H:%M:%S")),]
}

DO_all$DO[DO_all$DO<0]<-NA
DO_all$Temp_DO[DO_all$Temp_DO<0]<-NA
DO_all$DO[DO_all$DO>10]<-NA


DO_all<-left_join(samplingperiod, DO_all)
DO_all<- DO_all[!duplicated(DO_all[c('Date','ID')]),]
DO_all<-DO_all %>% mutate(ID=as.character(ID))

ggplot(DO_all, aes(Date, DO)) + geom_line() + facet_wrap(~ ID, ncol=4)

# discharge <- read_csv("02_Clean_data/discharge.csv")
# DO_check<-left_join(DO_all, discharge)
#
# a<-ggplot(DO_check %>% filter(ID=='5', Q>1, Date>'2025-04-01'), aes(Date, DO)) + geom_line()
# b<-ggplot(DO_check %>% filter(ID=='5',  Date>'2025-04-01'), aes(Date, Q)) + geom_line()
#
# plot_grid(a,b, ncol=1)

write_csv(DO_all%>%filter(!ID %in% c("6tstat2", "9stat2", NA_real_)),
          "02_Clean_data/DO_cleaned.csv")

write_csv(DO_all%>%filter(!ID %in% c("6tstat2", "9stat2")),
          "02_Clean_data/DO_stat2.csv")

####SpC####
file.names <- list.files(path="01_Raw_data/HOBO Excels/SpC", pattern=".csv", full.names=TRUE)
SpC_all<-data.frame()
for(i in file.names){
  SpC<-clean_SpC(i)
  SpC_all<-rbind(SpC_all, SpC)}

SpC_all$SpC[SpC_all$SpC>900]<-NA
SpC_all$SpC[SpC_all$SpC<25]<-NA

sites<-split(SpC_all,SpC_all$ID)
s13<-sites[['13']]
s15<-sites[['15']]# not working :()
s3<-sites[['3']] #npt working :()
s5<-sites[['5']]
s5a<-sites[['5a']]
s6<-sites[['6']]
s6a<-sites[['6a']]
s7<-sites[['7']]
s9<-sites[['9']]

s6<-s6 %>% filter(SpC<135 & SpC>50)
s6a<-s6a %>% filter(SpC<135)
s7<-s7 %>% filter(SpC<200 & SpC>80)
s9<-s9 %>% filter(SpC>35)
s5a<-s5a %>% filter(SpC<300)

SpC_all<-rbind(s5,s5a,s15,s3,s7,s6,s6a,s9,s13)

SpC_all<-left_join(samplingperiod,SpC_all)
SpC_all <- SpC_all[!duplicated(SpC_all[c('Date','ID')]),]
ggplot(SpC_all, aes(Date, SpC)) + geom_line() + facet_wrap(~ ID, ncol=4, scales='free')
range(SpC_all$Date,na.rm=T)

write_csv(SpC_all, "02_Clean_data/SpC_cleaned.csv")

###pH#####
file.names <- list.files(path="01_Raw_data/HOBO Excels/pH", pattern=".xlsx", full.names=TRUE)
pH_all<-data.frame()
for(fil in file.names){
  pH<-clean_pH(fil)
  pH_all <- rbind(pH_all, pH)
  }
pH_all <- pH_all[!duplicated(pH_all[c('Date','ID')]),]

# depth<-read_csv('02_Clean_data/depth.csv')
# pH_all<-left_join(pH_all, depth, by=c('Date','ID'))

sites<-split(pH_all,pH_all$ID)
s13<-sites[['13']]
s15<-sites[['15']]
s3<-sites[['3']]
s5<-sites[['5']]
s5a<-sites[['5a']]
s6<-sites[['6']]
s6a<-sites[['6a']]
s7<-sites[['7']]
s9<-sites[['9']]

s13<-s13 %>%filter(pH>4.5)
# a<-ggplot(s13, aes(Date, pH)) + geom_line()+geom_hline(yintercept = 4.5)
# b<-ggplot(s13, aes(Date, depth)) + geom_line()
# plot_grid(a,b, ncol=1)

s15<-s15 %>% filter(pH<6 & pH>2.87)
# a<-ggplot(s15, aes(Date, pH)) + geom_line()
# b<-ggplot(s15, aes(Date, depth)) + geom_line()
# plot_grid(a,b, ncol=1)

s3<-s3%>% filter(pH<4)
# a<-ggplot(s3, aes(Date, pH)) + geom_line()
# b<-ggplot(s3, aes(Date, depth)) + geom_line()
# plot_grid(a,b, ncol=1)

s5<-s5%>% filter(pH>3.3)
# a<-ggplot(s5, aes(Date, pH)) + geom_line()+geom_hline(yintercept = 3.3)
# b<-ggplot(s5, aes(Date, depth)) + geom_line()
# plot_grid(a,b, ncol=1)

# a<-ggplot(s5a, aes(Date, pH)) + geom_line()+geom_hline(yintercept = 3.3)
# b<-ggplot(s5a, aes(Date, depth)) + geom_line()
# plot_grid(a,b, ncol=1)

s6<-s6%>% filter(pH<5 & pH>3.5)
# a<-ggplot(s6, aes(Date, pH)) + geom_line()+geom_hline(yintercept = 3.5)
# b<-ggplot(s6, aes(Date, depth)) + geom_line()
# plot_grid(a,b, ncol=1)

s7<-s7%>%filter(pH<6 & pH>3.5)
# a<-ggplot(s7, aes(Date, pH)) + geom_line()+geom_hline(yintercept = 3.5)
# b<-ggplot(s7, aes(Date, depth)) + geom_line()
# plot_grid(a,b, ncol=1)

s9<-s9%>%filter(pH<8 & pH>3)%>%
  mutate(pH=if_else(Date>'2023-10-01'& Date<'2024-02-02' & pH>4, NA, pH))
# (a<-ggplot(s9, aes(Date, pH)) + geom_line()+geom_hline(yintercept = 4))
# b<-ggplot(s9, aes(Date, depth)) + geom_line()
# plot_grid(a,b, ncol=1)

pH_all<-rbind(s5,s5a,s15,s7,s3,s6,s6a,s9,s13)
pH_all<-left_join(samplingperiod,pH_all)
pH_all <- pH_all[!duplicated(pH_all[c('Date','ID')]),]

ggplot(pH_all %>% filter(ID=='15'), aes(Date, pH)) + geom_line()+facet_wrap(~ ID, ncol=4, scales='free')
range(pH_all$Date,na.rm=T)

write_csv(pH_all, "02_Clean_data/pH_cleaned.csv")

####Compile####

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5, 6, 7, 8, 10, 12)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))

master$Temp_PT[master$Temp_PT>87]<-NA
master$Temp_PT[master$Temp_PT<0]<-NA

master$Temp_PT <- ifelse(is.na(master$Temp_PT), master$Temp_pH, master$Temp_PT)
temperature<-master %>% select(Date, ID, Temp_PT)
write_csv(temperature, "02_Clean_data/temperature.csv")

master<-master[,c("Date","depth","ID","Q","CO2","DO","pH","SpC","Temp_PT","Water_press")]
master<-rename(master, 'Temp'="Temp_PT")%>%
  distinct(Date, ID, .keep_all = T)%>%
  filter(!is.na(ID))

master_long <- master %>%
  filter(Q>10)%>%
  pivot_longer(
    cols = c(depth, Q, CO2, DO, pH, SpC),
    names_to = "chem",
    values_to = "meas"
  )%>%
  mutate(
    chem= if_else(chem=='CO2', "CO2 ppm", chem),
    chem= if_else(chem=='DO', "DO mg/L", chem),
    chem= if_else(chem=='Q', "Q (L/s)", chem),
    chem= if_else(chem=='depth', "depth (m)", chem)

  )

distance_theme <- list(
  geom_violin(),
  facet_wrap(~chem, ncol = 3, scales='free'),
  theme(
    strip.text = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size=13),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12)
))

ggplot(
  master_long, aes(x = ID, y = meas)) +distance_theme+scale_y_log10()
