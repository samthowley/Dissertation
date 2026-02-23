library(tidyverse)
library(readxl)

read_csv("02_Clean_data/depth.csv")%>%
  group_by(ID)%>%
  summarize(
    n = n(),
    min=min(Date, na.rm = T)
  )%>% arrange(n)

read_csv("04_Output/gas.samples.csv")%>%
  filter(chapter=='stream', type=='CH4')%>%
  group_by(Site)%>%
  summarize(
    n = n(),
    min=min(Date, na.rm = T)
  )%>% arrange(n)

read_csv("04_Output/sampled.solid.carbon.csv")%>%
  filter(chapter=='stream')%>%
  filter(!is.na(POC))%>%
  group_by(Site)%>%
  summarize(
    n = n(),
    min=min(Date, na.rm = T)
  )%>% arrange(n)

read_csv("01_Raw_data/GD/GasDome_compiled.csv")%>%
  mutate(day=as.Date(Date))%>%
  distinct(day, ID, .keep_all = T)%>%
  group_by(ID)%>%
  summarize(
    n = n(),
    min=min(Date, na.rm = T)
  )%>% arrange(n)


read_csv("04_Output/RC/RC.water.sample.flux.csv")%>%
  group_by(Site)%>%
  filter(!is.na(DIC))%>%
  summarize(
    DOC.samples=n()
  )


read_csv("04_Output/RC/RC.gas.sample.flux.csv")%>%
  group_by(Site)%>%
  filter(!is.na(CO2.water.ppm))%>%
  summarize(
    Date=min(Date, na.rm = T)
  )



long.dis.c<-read_csv("04_Output/sampled.solid.carbon.csv")

long.DOC<-long.dis.c%>%
  filter(chapter=='long')%>%
  filter(!is.na(DOC))%>%
  distinct(Date, Site)%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.", remove = FALSE)%>%
  mutate(Long=if_else(ID=='3' & Long=='4','3', Long),
         Long=if_else(ID=='6' & Long=='1','4', Long),
         Long=if_else(ID=='6' & Long=='2','5', Long),
         Long=if_else(ID=='6' & Long=='3','6', Long),
         ID=if_else(ID=='3','6', ID))%>%
  group_by(Site, ID)%>%
  summarize(
    `DOC Samples` = n(),
    `DOC Sampling Dates`=min(Date, na.rm = T)
  )%>% arrange(ID,Site)


long.DIC<-long.dis.c%>%
  filter(chapter=='long')%>%
  filter(!is.na(DIC))%>%
  distinct(Date, Site)%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.", remove = FALSE)%>%
  mutate(Long=if_else(ID=='3' & Long=='4','3', Long),
         Long=if_else(ID=='6' & Long=='1','4', Long),
         Long=if_else(ID=='6' & Long=='2','5', Long),
         Long=if_else(ID=='6' & Long=='3','6', Long),
         ID=if_else(ID=='3','6', ID))%>%
  group_by(Site, ID)%>%
  summarize(
    `DIC Samples` = n(),
    `DIC Sampling Dates`=min(Date, na.rm = T)
  )%>% arrange(ID,Site)

long.gas<-read_csv("04_Output/gas.samples.csv")%>%
  filter(chapter=='long', type=='CO2')%>%
  distinct(Date, Site)%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.", remove = FALSE)%>%
  mutate(Long=if_else(ID=='3' & Long=='4','3', Long),
         Long=if_else(ID=='6' & Long=='1','4', Long),
         Long=if_else(ID=='6' & Long=='2','5', Long),
         Long=if_else(ID=='6' & Long=='3','6', Long),
         ID=if_else(ID=='3','6', ID))%>%
  group_by(ID,Site)%>%
  summarize(
    `CO2/ CH4 Samples` = n(),
    `CO2/ CH4 Sampling Dates`=min(Date, na.rm = T)
  )%>% arrange(ID, Site)



  df_list <- list(long.DOC, long.DIC, long.gas)

write.csv(reduce(df_list, full_join, by=c('Site')), "test.csv")

####################################################

theme_set(theme(
                legend.title =element_blank(),
                legend.position ="right",
                panel.background = element_rect(fill = 'white'),
                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray")))


RW.log <- read_excel("01_Raw_data/RW.log.xlsx", sheet = "log")%>%
  mutate(
    WT.distance.from.surface=as.numeric(WT.distance.from.surface)
  )%>%select(
    -Notes
  )


RW.dims <- read_excel("01_Raw_data/RW.log.xlsx", sheet = "well dims")%>%
  mutate(total.well.length=total.well.length*-1)


rw.wte<-left_join(RW.log, RW.dims, by='Site')%>%mutate(
  WTE=surface.elevation+WT.distance.from.surface,
  facet_label = paste0(Site, "\n", "distance:","\n", distance.from.stream,"\n", 'm'))%>%
  filter(
    Site!='5GW7',
    !Sampled %in% c(NA_real_, 'NA')
  )%>%
  arrange(Site, distance.from.stream)

for.labels<-rw.wte%>%select(facet_label, well.bottom.elevation, surface.elevation)%>%
  distinct(facet_label, .keep_all = T)


well.bottom <- for.labels[, c("facet_label", "well.bottom.elevation")]
surface.e <- for.labels[, c("facet_label", "surface.elevation")]




ggsave(filename = "RW.inventory.jpeg",
       ggplot(rw.wte, aes(x = Date, y = WTE, color = Sampled)) +
         geom_point(size = 1, stroke = 1.5) +
         facet_wrap(~facet_label, scales='free')+
         geom_hline(data = well.bottom, aes(yintercept = well.bottom.elevation), size=1)+
         geom_hline(data = surface.e, aes(yintercept = surface.elevation), size=1)+
         geom_hline(color='red', linetype='dashed', yintercept = 0, size=1),
       width = 18, height = 12, units = "in")

