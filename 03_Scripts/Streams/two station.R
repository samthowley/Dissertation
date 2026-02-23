library(tidyverse)
library(writexl)
library(readxl)
library(weathermetrics)
library(measurements)
library(cowplot)
library(tools)

file.names <- list.files(path="01_Raw_data/HOBO Excels/DO/two station", pattern=".csv", full.names=TRUE)

DO_all<-data.frame()
for(i in file.names){

  DO <- read_csv(i,skip= 1) # read csv
  DO<-DO[,c(2,3,4)] #these columns
  colnames(DO)[1] <- "Date"
  colnames(DO)[2] <- "DO"
  colnames(DO)[3] <- "Temp_DO"
  DO$Date <- mdy_hms(DO$Date)
  DO$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][6]
  DO_all<-rbind(DO_all, DO)
}

DO_all<-DO_all%>%
  filter(DO > 0 & DO < 9) %>%
  distinct(DO, .keep_all = T)%>%
  separate(ID, into = c("ID", "station"), sep  = "stat",
           extra = "drop", # drop extra pieces
           fill  = "right" # if only one piece, put it in 'point' and fill stream with NA
  )%>%
  rename(DO.up=DO, Temp.up=Temp_DO)%>%
  mutate(ID=if_else(ID=='6t', '6', ID))

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(8, 7, 14, 15)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
master <- reduce(data, full_join, by = c("ID", 'Date'))%>%
  filter(ID %in% c('5', '6', '9'))

stat2<-left_join(DO_all, master)%>% filter(!is.na(DO))%>%select(-station)%>%
  mutate(DO.diff=DO-DO.up)
