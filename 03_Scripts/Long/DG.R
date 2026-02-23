library(anytime)
library(tidyverse)
library(readxl)
library(tools)
library(openxlsx)
library(lme4)
library(mmand)
library(grwat)
library(ggpmisc)
library(cowplot)
library(ggplot2)

###separating######
clean_DG <- function(DG) {
  DG<-DG[,c(2,3,4)]
  colnames(DG)[1] <- "Date"
  colnames(DG)[2] <- "LowSpC"
  colnames(DG)[3] <- "FullSpC"
  DG$Date<-mdy_hms(DG$Date)

  DG$time<-strftime(DG$Date, format="%H:%M:%S", tz = "UTC")
  DG<-DG %>%filter(time>start & time< end)
  return(DG)}

DG <- read_csv("01_Raw_data/DG/5 long/12082025_5.4.csv", skip = 1)

start<-'11:57:00'
end<-'12:15:00'

DG<-clean_DG(DG)

ggplot(DG, aes(Date,FullSpC)) + geom_line()

DG<-DG[,c(1,2,3)]

write_csv(DG, '01_Raw_data/DG/seperated/long.sampling/12082025_5.4.csv')

###### compile ####
DG_all<-data.frame()
file.names <- list.files(path="01_Raw_data/DG/seperated/long.sampling", pattern=".csv", full.names=TRUE)


for (i in file.names) {
  DG <- read_csv(i)

  if (!inherits(DG$Date, c("POSIXct", "POSIXt"))) {
    DG$Date <- mdy_hms(DG$Date)
  }

  DG$ID <- strsplit(file_path_sans_ext(i), "_")[[1]][4]
  DG_all <- rbind(DG_all, DG)
}

write_csv(DG_all, "01_Raw_data/DG/compiled.long_DG.csv")

#extract DG#####
notes<- read_csv("01_Raw_data/DG/Streams_dilution_gauging.csv",
                 col_types = cols(Date = col_date(format = "%m/%d/%Y")))

notes<-notes[,c(1,2,3,6)]
notes<-rename(notes, 'day'='Date', 'ID'='Site')

DG<- read_csv("01_Raw_data/DG/compiled.long_DG.csv")%>%mutate(day=as.Date(Date), ID=as.character(ID))

DG_notes<-left_join(DG, notes, by=c('day','ID'))

DG<-DG_notes %>% group_by(day,ID) %>%
  mutate(elapsed = as.numeric(Date-Date[1]))%>%ungroup()

DG_edit <- DG %>%
  mutate(time_group =  case_when(elapsed <= 5 ~ "prior",elapsed >= 5 ~ "after"))

DG_prior <- DG_edit %>%
  group_by(day, ID, time_group) %>%
  summarize(mean_prior = mean(LowSpC, na.rm = TRUE), .groups = "drop")%>%
  filter(time_group=='prior')

DG_calc<-left_join(DG_edit, DG_prior, by=c('day', 'ID'))%>%
  mutate(LowSpC = as.numeric(LowSpC))%>%
  mutate(SpC_cor=LowSpC-mean_prior)%>%
  mutate(NaCl=SpC_cor*0.51)%>%
  mutate(tC=elapsed*NaCl, single_mass=NaCl*5)%>%
  arrange(day,ID)%>%group_by(day,ID)%>%
  mutate(total_mass=cumsum(single_mass))%>% ungroup()

DG_Q <- DG_calc %>% group_by(day,ID)%>%
  mutate(m_0= sum(NaCl, na.rm=T)*5)%>%
  mutate(m_1= sum(tC, na.rm = T)*5)%>% ungroup %>%
  mutate(t_star=m_1/m_0,u_mean=Reach_m/t_star, Q=(NaCl_g*1000)/m_0, day=as.Date(Date))%>%ungroup()%>%
  distinct(ID, day, .keep_all=T)%>%
  separate(ID, into = c("ID", "point"), sep  = "\\.",   # literal dot
                         extra = "drop", # drop extra pieces
                         fill  = "right" # if only one piece, put it in 'point' and fill stream with NA
)%>% rename(Q.long=Q)

#Combine w Stream Q#######
discharge <- read_csv("02_Clean_data/discharge.csv")%>% filter(ID %in% c('5','6','9'))%>%
  mutate(day=as.Date(Date), Q=mean(Q, na.rm=T))%>%
  distinct(ID, day, .keep_all = TRUE)

compare<-left_join(DG_Q, discharge, by=c('ID', 'day'))%>%
  select(ID, day, Q.long, Q)%>% mutate(Q.fraction=Q.long/Q)

write_csv(DG_Q, "04_Output/compiled.long_DG.csv")





