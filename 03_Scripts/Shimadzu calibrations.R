rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(lme4)
library(openxlsx)
theme_set(theme(axis.text.x = element_text(size = 12, angle=0),
                axis.text.y = element_text(size = 17, angle=0),
                axis.title =element_text(size = 17, angle=0),
                plot.title = element_text(size = 17, angle=0),
                legend.key.size = unit(0.8, 'cm'),
                legend.text=element_text(size = 17),
                legend.title =element_text(size = 17),
                panel.background = element_rect(fill = 'white'),
                panel.grid.major = element_line(color = "gray", size = 0.5)))


calibrations<-data.frame()
file.names <- list.files(path="01_Raw_data/Shimadzu/dat files/detailed", pattern=".txt", full.names=TRUE)
for(fil in file.names){

  run<-read_delim(fil,delim = "\t", escape_double = FALSE,
                   trim_ws = TRUE, skip = 10)

  cals<-run %>% filter(`Sample Name` =='Untitled'| `Sample Name`=='NPOC_Saline_100mgL') %>%
    mutate(Date= mdy_hms(`Date / Time`)) %>%mutate(day=as.Date(Date))%>%
    select(`Anal.`, `Mean Area`, `Conc.`, day)%>%
    rename(Analyte=`Anal.`, Area=`Mean Area`, Conc=`Conc.`)%>%distinct(day, Conc, Analyte,.keep_all = TRUE)

  curve<- cals %>% group_by(day, Analyte) %>%
    summarise(model = list(lm(Conc ~Area , data = cur_data())),.groups = "drop") %>%
    mutate(coeffs = map(model, ~ broom::tidy(.x) %>%
                          select(term, estimate) %>%
                          pivot_wider(names_from = term, values_from = estimate)), r_squared = map_dbl(model, ~ summary(.x)$r.squared)  # Extract R-squared
    ) %>%
    unnest(cols = coeffs) %>%
    rename(intercept = `(Intercept)`, slope = Area) %>%
    select(day, Analyte, intercept, slope, r_squared)

  combine<-left_join(cals, curve, by=c('day', 'Analyte'))
  calibrations<-rbind(calibrations, combine)
}

calibrations<-calibrations%>% filter(r_squared>0.99)
IC_cals<-calibrations%>% filter(Analyte=='IC')
TC_cals<-calibrations%>% filter(Analyte=='TC')
NPOC_cals<-calibrations%>% filter(Analyte=='NPOC')

wb <- createWorkbook()
addWorksheet(wb, "IC")
addWorksheet(wb, "TC")
addWorksheet(wb, "NPOC")

writeData(wb, sheet = "IC", x = IC_cals)
writeData(wb, sheet = "TC", x = TC_cals)
writeData(wb, sheet = "NPOC", x = NPOC_cals)

saveWorkbook(wb, "01_Raw_data/Shimadzu/calcurves.xlsx", overwrite = TRUE)


ggplot(IC_cals, aes(x=Area, y=Conc, color = as.factor(day))) +
  geom_point(size=2)+geom_line()+ggtitle('IC Calibration Curves')


ggplot(TC_cals, aes(x=Area, y=Conc, color = as.factor(day))) +
  geom_point(size=2)+geom_line()+ggtitle('TC Calibration Curves')

ggplot(NPOC_cals, aes(x=Area, y=Conc, color = as.factor(day))) +
  geom_point(size=2)+geom_line()+ggtitle('NPOC Calibration Curves')

ggplot(TC_cals, aes(x=Area, y=Conc, color = as.factor(day))) +
  geom_point(size=2)+geom_line()+ggtitle('TC Calibration Curves')
