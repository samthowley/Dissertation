#packages#####
rm(list=ls())


library(tidyverse)
library(writexl)
library(readxl)

NPOC_all<-data.frame() #call in dat files
TOC_all<-data.frame() #call in dat files

file.names <- list.files(path="01_Raw_data/Shimadzu/dat files/detailed", pattern=".txt", full.names=TRUE)
for(fil in file.names){

  runs<-read_delim(fil,delim = "\t", escape_double = FALSE,
                   trim_ws = TRUE, skip = 10)

  result<-runs%>% filter(`Sample Name` != "Untitled"| `Sample Name`=='NPOC_Saline_100mgL', `Inj. No.` == 1)%>%
    select(`Sample Name`, `Analysis(Inj.)`, `Mean Area`, `Mean Conc.`)%>%
    rename("SampleID"="Sample Name", "Analyte"="Analysis(Inj.)", "Area"="Mean Area", "Conc"="Mean Conc.")%>%
    separate(SampleID, into = c("Site", "Date","Rep"), sep = "_")%>%
    mutate(Date=mdy(Date),  filename = basename(fil))

  TC<-result %>%filter(Analyte=='TC')%>%select(Site, Date, Rep,Conc,Area)%>%
    rename("TC.area"="Area", "TC.conc"="Conc")

  IC<-result %>%filter(Analyte=='IC')%>%rename("IC"="Analyte")%>%
    select(Site, Date, Rep,Conc,Area,filename)%>%
    rename("IC.area"="Area", "IC.conc"="Conc")

  NPOC<-result %>%filter(Analyte=='NPOC',Site!='NPOC')%>%rename("NPOC"="Analyte")%>%
    select(Site,Date,Rep,Conc,Area,filename)%>%
    rename("NPOC.area"="Area", "NPOC.conc"="Conc")

  TOC<-full_join(TC, IC, by=c("Site", "Date","Rep"))
  TOC<-TOC%>%select(-Rep)
  TOC_all<-rbind(TOC_all, TOC)

  NPOC_all<-rbind(NPOC_all, NPOC)
}

csv<-data.frame() #call in csv files
file.names <- list.files(path="01_Raw_data/Shimadzu/csvs/manipulated", pattern=".csv", full.names=TRUE)
for(fil in file.names){
  run <- read_csv(fil)
  run<-run %>% rename('Date'='Sample Date', 'DOC'='NPOC')%>% mutate(Date=mdy(Date), DIC= NA)%>%
    select(Date, Site, DIC, DOC)%>%
    mutate(filename = basename(fil))

  csv<-rbind(csv, run)
}


dissolved_NPOC<-NPOC_all %>% filter(!Rep %in% c("1", "2", "3"))%>%
  mutate(DOC = if_else(Date < '2025-12-20', NPOC.area*0.297-0.133, NPOC.area), DIC=NA)%>%
  select(Date, Site, DOC, filename)


dissolved_TOC<- TOC_all%>%filter(!is.na(Date))%>% #incorporating calibrations
  mutate(DIC = if_else(Date < '2025-12-20', IC.area*0.37+0.479, IC.area),
         DOC = if_else(Date < '2025-12-20', TC.area*0.2989-0.234, TC.area))

DOC<-dissolved_TOC%>%select(Date, Site, DOC, filename)
DOC_NPOC<-rbind(DOC,dissolved_NPOC)

DIC<-dissolved_TOC%>%select(Date, Site, DIC, filename)
DC<-full_join(DIC, DOC_NPOC, by = c("Date","Site", "filename"))

csv<-csv%>%select(names(DC))
dissolvedC_csv<-rbind(DC, csv)

particulate_NPOC<-NPOC_all %>% filter(Rep %in% c("1", "2", "3"))%>%
  mutate(TNPOC = if_else(Date < '2025-12-20', NPOC.area*0.297-0.133, NPOC.area))%>%
  group_by(Site, Date)%>%
    mutate(NPTOC=mean(TNPOC, na.rm=T))%>% distinct(Date,Site, .keep_all = T) %>%
  select(Site,Date,TNPOC)

sampledC<-full_join(dissolvedC_csv,particulate_NPOC, by=c('Date','Site'))
sampledC<-sampledC %>% mutate(POC=abs(TNPOC-DOC))%>%
  arrange(Date)%>%filter(DOC != is.na(DOC))%>%
  distinct(Site, Date, .keep_all = T)


##########
correct.site.names <- sampledC %>%
  mutate(
    Site = case_when(
      Site == '9GWup' ~ "9GW5",
      Site == '5GWup' ~ "5GW8",
      Site == '5GW7' & Date == "2025-03-12" ~ "5GW8",
      Site == '5GW6' & Date == "2024-08-20" ~ "5GW6_updated", # <-- Replace with correct?
      TRUE ~ Site
    ),
    Date = case_when(
      Date == '2004-05-08' ~ as.Date("2024-05-08"),
      Date == '2004-06-14' ~ as.Date("2024-06-14"),
      TRUE ~ Date
    )
  )
carbon<-correct.site.names %>% mutate(Site= if_else(Site=='5,5','5.5', Site))%>%
  mutate(ID=case_when(Site=='3'~'3',Site=='5'~'5',Site=='5a'~'5a',
                                    Site=='6'~'6',Site=='6a'~'6a',Site=='7'~'7',
                                    Site=='9'~'9',Site=='13'~'13',Site=='15'~'15',

                                    Site=='5GW1'~'5',Site=='5GW2'~'5',Site=='5GW3'~'5',Site=='5GW4'~'5',
                                    Site=='5GW5'~'5',Site=='5GW6'~'5',Site=='5GW7'~'5',Site=='6GW1'~'6',
                                    Site=='6GW2'~'6',Site=='6GW3'~'6',Site=='6GW4'~'6',Site=='6GW5'~'6',
                                    Site=='6GW6'~'6',Site=='9GW1'~'9',Site=='9GW2'~'9',Site=='9GW3'~'9',
                                    Site=='9GW4'~'9',Site=='9GW5'~'9',Site=='5GW8'~'5',

                                    Site=='5.1'~'5',Site=='5.2'~'5',Site=='5.3'~'5',
                                    Site=='5.4'~'5',Site=='5.5'~'5',Site=='6.1'~'6',Site=='6.2'~'6',
                                    Site=='6.3'~'6',Site=='6.4'~'6',Site=='6.5'~'6',Site=='6.6'~'6',
                                    Site=='9.1'~'9',Site=='9.2'~'9',Site=='9.3'~'9',Site=='9.4'~'9',
                                    Site=='9.5'~'9',Site=='9.6'~'9',Site=='9.Sam'~'9'))


carbon<-carbon %>% mutate(chapter=case_when(Site=='3'~'stream',Site=='5'~'stream',Site=='5a'~'stream',
                                            Site=='6'~'stream',Site=='6a'~'stream',Site=='7'~'stream',
                                            Site=='9'~'stream',Site=='13'~'stream',Site=='15'~'stream',

                                            Site=='5GW1'~'RC',Site=='5GW2'~'RC',Site=='5GW3'~'RC',Site=='5GW4'~'RC',
                                            Site=='5GW5'~'RC',Site=='5GW6'~'RC',Site=='5GW7'~'RC',Site=='5GW8'~'RC',

                                            Site=='6GW1'~'RC',
                                            Site=='6GW2'~'RC',Site=='6GW3'~'RC',Site=='6GW4'~'RC',Site=='6GW5'~'RC',
                                            Site=='6GW6'~'RC',

                                            Site=='9GW1'~'RC',Site=='9GW2'~'RC',Site=='9GW3'~'RC',
                                            Site=='9GW4'~'RC',Site=='9GW5'~'RC',

                                            Site=='5.1'~'long',Site=='5.2'~'long',Site=='5.3'~'long',
                                            Site=='5.4'~'long',Site=='5.5'~'long',Site=='5.6'~'long',

                                            Site=='6.1'~'long',Site=='6.2'~'long',
                                            Site=='6.3'~'long',Site=='3.1'~'long',Site=='3.2'~'long',Site=='3.3'~'long',
                                            Site=='3.4'~'long',

                                            Site=='9.1'~'long',Site=='9.2'~'long',Site=='9.3'~'long',Site=='9.4'~'long',
                                            Site=='9.5'~'long',Site=='9.6'~'long',Site=='9.Sam'~'long'))

carbon$chapter[is.na(carbon$chapter)]<-'wetland'
test<-carbon%>%filter(chapter=='wetland')
unique(test$Site)

write_csv(carbon, "04_Output/sampled.solid.carbon.csv")

