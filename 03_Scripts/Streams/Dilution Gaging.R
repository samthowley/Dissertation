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

###seperating DG######
clean_DG <- function(DG) {
  DG<-DG[,c(2,3,4)]
  colnames(DG)[1] <- "Date"
  colnames(DG)[2] <- "LowSpC"
  colnames(DG)[3] <- "FullSpC"
  DG$Date<-mdy_hm(DG$Date)

  DG$time<-strftime(DG$Date, format="%H:%M:%S", tz = "UTC")
  DG1<-DG %>%filter(time>start & time< end)
  return(DG1)}

DG <- read_csv("01_Raw_data/DG/raw/DG_03112025.csv", skip = 1)

start<-'9:20:00'
end<-'10:40:00'

DG1<-clean_DG(DG)

ggplot(DG3, aes(Date,FullSpC)) + geom_line()

write_csv(DG, '01_Raw_data/DG/seperated/03112025_9.2.csv')

###### compile ####
DG_all<-data.frame()
file.names <- list.files(path="01_Raw_data/DG/seperated", pattern=".csv", full.names=TRUE)
for(i in file.names){
  DG<-read_csv(i, skip=1)%>%select(1:3)
  colnames(DG)[1] <- "Date"
  colnames(DG)[2] <- "LowSpC"
  colnames(DG)[3] <- "FullSpC"

  if (!inherits(DG$Date, c("POSIXct", "POSIXt"))) {
    DG$Date <- mdy_hms(DG$Date)
  }


  DG$ID<-strsplit(file_path_sans_ext(i), '_')[[1]][4]
  DG_all<-rbind(DG_all, DG)
}

write_csv(DG_all, "01_Raw_data/DG/compiled_DG.csv")
#extract DG#####
notes<- read_csv("01_Raw_data/DG/Streams_dilution_gauging.csv",
                 col_types = cols(Date = col_date(format = "%m/%d/%Y")))

notes<-notes[,c(1,2,3,6)]
notes<-rename(notes, 'day'='Date', 'ID'='Site')

DG<- read_csv("01_Raw_data/DG/compiled_DG.csv")%>%mutate(day=as.Date(Date))

DG_notes<-left_join(DG, notes, by=c('day','ID'))

DG<-DG_notes %>% group_by(day,ID) %>%
  mutate(elapsed = as.numeric(Date-Date[1]))%>%ungroup()

DG_edit <- DG %>%
  filter(Date>'2021-01-01', !ID %in% c('9.2', '6.3', '14'))%>%
  mutate(time_group =  case_when(elapsed <= 5 ~ "prior",elapsed >= 5 ~ "after"))

DG_prior <- DG_edit %>%
  group_by(day, ID, time_group) %>%
  mutate(LowSpC=as.numeric(LowSpC))%>%
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
  mutate(t_star=m_1/m_0,u_mean=Reach_m/t_star, Q=(NaCl_g*1000)/m_0)%>%ungroup()%>%
  distinct(ID, day, .keep_all=T)

write_csv(DG_Q, "04_Output/compiled_DG.csv")

#Calculate Q####
DG<-read_csv('04_Output/compiled_DG.csv')%>%mutate(day=as.Date(Date))

depth <- read_csv("02_Clean_data/depth.csv")%>%
  mutate(day=as.Date(Date))%>%
  group_by(day, ID)%>%summarize(depth=mean(depth, na.rm=T))%>%
  distinct(day, ID, .keep_all = T)

DG_rC<-left_join(DG, depth, by=c('ID', 'day'))%>%
  select(Date, ID, Q, u_mean, m_0, m_1, depth)

DG_rC<- DG_rC %>% mutate(logQ=log10(Q),logh=log10(depth)) %>%
  filter(!ID %in% c('14', '6.3', '9.2'))

split<-DG_rC %>% split(DG_rC$ID)
write.xlsx(split, file = '04_Output/rC_DG.xlsx')

rC <- lmList(logQ ~ logh | ID, data=DG_rC)
(cf <- coef(rC))

depth<-read_csv('02_Clean_data/depth.csv')
discharge <- depth %>% filter(!ID %in% c('6.3','9.2'))%>%
  mutate(Q= case_when(
    ID== '13'~ (10^cf[1,1])*depth^(cf[1,2]),
    ID== '15'~ (10^cf[2,1])*depth^(cf[2,2]),
    ID== '3'~ (10^cf[3,1])*depth^(cf[3,2]),
    ID== '5'~ (10^cf[4,1])*depth^(cf[4,2]),
    ID== '5a'~ (10^cf[5,1])*depth^(cf[5,2]),
    ID== '6'~ (10^cf[6,1])*depth^(cf[6,2]),
    ID== '6a'~ (10^cf[7,1])*depth^(cf[7,2]),
    ID== '7'~ (10^cf[8,1])*depth^(cf[8,2]),
    ID== '9'~ (10^cf[9,1])*depth^(cf[9,2])))%>% select(Date, ID, Q)


u_rC <- lmList(log10(depth) ~ log10(u_mean) | ID, data=DG_rC)
(u_cf <- coef(u_rC))

V <- depth %>%
  mutate(u= case_when(
    ID== '13'~ (10^u_cf[1,1])*depth^(u_cf[1,2]),
    ID== '15'~ (10^u_cf[2,1])*depth^(u_cf[2,2]),
    ID== '3'~ (10^u_cf[3,1])*depth^(u_cf[3,2]),
    ID== '5'~ (10^u_cf[4,1])*depth^(u_cf[4,2]),
    ID== '5a'~ (10^u_cf[5,1])*depth^(u_cf[5,2]),
    ID== '6'~ (10^u_cf[6,1])*depth^(u_cf[6,2]),
    ID== '6a'~ (10^u_cf[7,1])*depth^(u_cf[7,2]),
    ID== '7'~ (10^u_cf[8,1])*depth^(u_cf[8,2]),
    ID== '9'~ (10^u_cf[9,1])*depth^(u_cf[9,2])))

ggplot(DG_rC, aes(x=depth, y=u_mean)) +
  geom_point()+
  geom_smooth(method = lm)+
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ ID, ncol=5, scales = 'free')

write_csv(V, "02_Clean_data/velocity.csv")

write_csv(discharge, "02_Clean_data/discharge.csv")

#parameters for baseflow calculation##########
discharge<-read.csv('02_Clean_data/discharge.csv')

Q<-discharge %>% group_by(ID)%>%mutate(bf_parameter=lag(Q))

ggplot(Q, aes(x=Q, y=bf_parameter)) +
  geom_point()+geom_smooth(method='lm')+
  facet_wrap(~ID)

rC <- lmList(bf_parameter ~ Q | ID, data=Q)
(cf <- coef(rC))

mean(cf$Q, na.rm=T)

write_csv(cf, "01_Raw_data/bf_parameter.csv")

#Figures##########
discharge<-read.csv('02_Clean_data/discharge.csv')

ggplot(discharge %>% filter(ID %in% c('5','15','5a', '9', '13')), aes(x = as.Date(Date), y = Q, color = ID, group = ID)) +
  geom_line(size = 1) +  # Adjust line size for better visibility
  scale_y_log10() +scale_x_date(date_labels = "%Y") +
  ylab(expression('Discharge'~ft^3/sec)) + xlab("Date") +
  scale_color_brewer(palette = "Set1") +  # Use a color palette for better distinction
  ggtitle("South Basin")

ggplot(discharge %>% filter(ID %in% c('7','3','6a', '6')), aes(x = as.Date(Date), y = Q, color = ID, group = ID)) +
  geom_line(size = 1) +  # Adjust line size for better visibility
  scale_y_log10() +scale_x_date(date_labels = "%Y") +
  ylab(expression('Discharge'~ft^3/sec)) + xlab("Date") +
  scale_color_brewer(palette = "Set1") +  # Use a color palette for better distinction
  ggtitle("North Basin")

plot_grid(N,S, ncol=1)

N<-ggplot(discharge %>% filter(ID %in% c('7', '3', '6a', '6'), Date<'2024-12-31'),
       aes(x =ID, y =Q, fill=ID))+
         scale_fill_brewer(palette = "Set1") +
  geom_boxplot() +scale_y_log10()+facet_wrap(~ year(Date), scales = 'free',nrow=1) +
  labs(x = "Year", y = "Discharge (Q)") +theme_minimal()+ggtitle('North Basin')


S<-ggplot(discharge %>% filter(ID %in% c('5','15','5a', '9', '13'), Date<'2024-12-31'),
       aes(x =ID, y =Q, fill=ID))+
  scale_fill_brewer(palette = "Set2") +
  geom_boxplot() +scale_y_log10()+facet_wrap(~ year(Date), scales = 'free',nrow=1) +
  labs(x = "Year", y = "Discharge (Q)") +theme_minimal()+ggtitle('South Basin')

plot_grid(N,S, nrow=1)
