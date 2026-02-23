#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(cowplot)
library(seacarb)
library(ggtern)
library(tibble)

#need to include stream LB too
theme_set(theme(axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                axis.title.y = element_text(size = 20, angle = 90),
                axis.title.x = element_text(size = 20),
                plot.title = element_text(size = 20),
                legend.key.size = unit(0.5, 'cm'),
                legend.text=element_text(size = 8),
                legend.title =element_text(size = 8),
                legend.position ="bottom",
                panel.grid.major.x = element_blank(),  # Customize x-axis major gridlines
                panel.grid.minor.y = element_blank(),
                panel.background = element_rect(fill = 'white'),
                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                strip.text = element_text(size = 20)))

#Edit dims######

flow_regime_daily <- read_csv("04_Output/flow_regime_daily.csv")%>%
  filter(ID %in% c('6','9','5'))%>%
  select(ID, Date, Q)

fa <- read_excel("01_Raw_data/Long. Log.xlsx", sheet = "dims")%>%
  mutate(Site=as.factor(Site), ID=as.factor(ID))%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")%>%
  mutate(Long=if_else(is.na(Long), '0', Long))

Q.fa<-left_join(flow_regime_daily, fa, by='ID')%>%
  mutate(Q.prop=Q*Q_fraction_04.2025)%>%
  select(Date, ID, distance, Q, Q.prop)

#Water Carbon Samples############

shimadzu_samples<-read_csv("04_Output/TDC_long.csv")%>%
  mutate(ID=as.character(ID))%>%
  filter(!Site %in% c('3.3', '6.4', '5.6', '9.5', '5.5', '9.Sam'))%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")%>%
  select(-depth, -Q, -pH, -CO2, -Temp_pH, -chapter)%>%
  mutate(Long=as.factor(Long), ID=as.factor(ID))


shimadzu_stream<- read_csv("04_Output/TDC_stream.csv")%>%
  select(-depth, -Q, -pH, -CO2, -Temp_pH, -chapter)%>%
  mutate(Long='0')%>%
  filter(ID %in% c('5', '6', '9', '3'))%>%
  select(names(shimadzu_samples))


field_log <- read_excel("01_Raw_data/Long. Log.xlsx",
                       sheet = "Long. Log", col_types = c("numeric",
                                                          "numeric", "date", "skip", "numeric",
                                                          "numeric", "numeric", "numeric",
                                                          "numeric", "skip", "skip"))%>%
  filter(!is.na(Site))%>%
  filter(!Site %in% c('3.3', '6.4', '5.6', '9.5', '5.5', '9.Sam'))%>%
  rename(Date=Visited)%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")

shimadzu_samples_all<-rbind(shimadzu_stream,shimadzu_samples)

water_samples<-left_join(shimadzu_samples_all, field_log)%>%
  mutate(POC=abs(POC))


#include gas samples####
long_gas <- read_csv("04_Output/Picarro_gas.csv")%>%
  filter(chapter=='long')%>%
  filter(!ID %in% c('3.3', '6.4', '5.6', '9.5', '5.5', '9.Sam'))%>%
  separate(ID, into = c("ID", "Long"), sep = "\\.")%>%
  select(-chapter)%>%
  mutate(Long=as.factor(Long), ID=as.factor(ID))

stream_gas <- read_csv("04_Output/Picarro_gas.csv")%>%
  filter(chapter=='stream')%>%
  filter(ID %in% c('5','6','9','3'))%>%
  mutate(Long='0')%>%
  mutate(Long=as.factor(Long), ID=as.factor(ID))%>%select(names(long_gas))

gas_samples<-rbind(long_gas, stream_gas)

all_samples<-full_join(water_samples, gas_samples)%>%
  mutate(Long=as.factor(Long), ID=as.factor(ID))%>%
  distinct(Date, ID, Long, .keep_all = T)%>%
  mutate(Long=if_else(ID=='3' & Long=='4','3', Long),
         Long=if_else(ID=='6' & Long=='1','4', Long),
         Long=if_else(ID=='6' & Long=='2','5', Long),
         Long=if_else(ID=='6' & Long=='3','6', Long),
         ID=if_else(ID=='3','6', ID))%>%
  mutate(ID_Long = paste(ID, Long, sep = "_"))%>%
  mutate(ID_Long=if_else(ID_Long=='6_0','6_2', ID_Long))%>%
  select(-Temp_K)

c.q<-left_join(all_samples,Q.fa)

write_csv(c.q, "04_Output/master_long.csv")


#regression####
c.q <- read_csv("04_Output/master_long.csv")
cols <- c('DOC','DIC',"CO2_umol_L","CH4_umol_L",'ID_Long', "Q.prop" , "distance", 'ID')
unique_sites <- unique(c.q$ID_Long[!is.na(c.q$ID_Long)])

RC <- setNames(
  lapply(unique_sites, function(site_id) {
    df_subset <- c.q %>%
      filter(ID_Long == site_id) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites)

RC<-RC %>%mutate(lateral_CO2)

DOC_relationships <- lapply(RC, function(df) {
  # Remove rows with NA or zero for DOC or Q.prop
  df <- df %>% filter(!is.na(DOC), !is.na(Q.prop), Q.prop > 0, DOC > 0)
  valid_elev <- nrow(df) > 1
  DOC.C.Q.p <- DOC.C.Q.slope <- DOC.C.Q.r2 <- NA

  if (valid_elev) {
    fit <- tryCatch(
      lm(log10(DOC) ~ log10(Q.prop), data = df),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      DOC.C.Q.cf <- summary(fit)
      # Check if coefficient exists
      coef_names <- rownames(DOC.C.Q.cf$coefficients)
      if ("log10(Q.prop)" %in% coef_names) {
        DOC.C.Q.p <- DOC.C.Q.cf$coefficients["log10(Q.prop)", "Pr(>|t|)"]
        DOC.C.Q.slope <- DOC.C.Q.cf$coefficients["log10(Q.prop)", "Estimate"]
        DOC.C.Q.r2 <- DOC.C.Q.cf$r.squared
      }
    }
  }

  data.frame(
    DOC.C.Q.p = as.numeric(DOC.C.Q.p),
    DOC.C.Q.slope = as.numeric(DOC.C.Q.slope),
    DOC.C.Q.r2 = as.numeric(DOC.C.Q.r2)
  )
})
DOC_table <- bind_rows(DOC_relationships, .id = "ID")

DIC_relationships <- lapply(RC, function(df) {
  # Remove rows with NA or zero for DIC or Q.prop
  df <- df %>% filter(!is.na(DIC), !is.na(Q.prop), Q.prop > 0, DIC > 0)
  valid_elev <- nrow(df) > 1
  DIC.C.Q.p <- DIC.C.Q.slope <- DIC.C.Q.r2 <- NA

  if (valid_elev) {
    fit <- tryCatch(
      lm(log10(DIC) ~ log10(Q.prop), data = df),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      DIC.C.Q.cf <- summary(fit)
      # Check if coefficient exists
      coef_names <- rownames(DIC.C.Q.cf$coefficients)
      if ("log10(Q.prop)" %in% coef_names) {
        DIC.C.Q.p <- DIC.C.Q.cf$coefficients["log10(Q.prop)", "Pr(>|t|)"]
        DIC.C.Q.slope <- DIC.C.Q.cf$coefficients["log10(Q.prop)", "Estimate"]
        DIC.C.Q.r2 <- DIC.C.Q.cf$r.squared
      }
    }
  }

  data.frame(
    DIC.C.Q.p = as.numeric(DIC.C.Q.p),
    DIC.C.Q.slope = as.numeric(DIC.C.Q.slope),
    DIC.C.Q.r2 = as.numeric(DIC.C.Q.r2)
  )
})
DIC_table <- bind_rows(DIC_relationships, .id = "ID")

CO2_relationships <- lapply(RC, function(df) {
  # Remove rows with NA or zero for CO2 or Q.prop
  df <- df %>% filter(!is.na(CO2_umol_L), !is.na(Q.prop), Q.prop > 0, CO2_umol_L > 0)
  valid_elev <- nrow(df) > 1
  CO2.C.Q.p <- CO2.C.Q.slope <- CO2.C.Q.r2 <- NA

  if (valid_elev) {
    fit <- tryCatch(
      lm(log10(CO2_umol_L) ~ log10(Q.prop), data = df),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      CO2.C.Q.cf <- summary(fit)
      # Check if coefficient exists
      coef_names <- rownames(CO2.C.Q.cf$coefficients)
      if ("log10(Q.prop)" %in% coef_names) {
        CO2.C.Q.p <- CO2.C.Q.cf$coefficients["log10(Q.prop)", "Pr(>|t|)"]
        CO2.C.Q.slope <- CO2.C.Q.cf$coefficients["log10(Q.prop)", "Estimate"]
        CO2.C.Q.r2 <- CO2.C.Q.cf$r.squared
      }
    }
  }

  data.frame(
    CO2.C.Q.p = as.numeric(CO2.C.Q.p),
    CO2.C.Q.slope = as.numeric(CO2.C.Q.slope),
    CO2.C.Q.r2 = as.numeric(CO2.C.Q.r2)
  )
})
CO2_table <- bind_rows(CO2_relationships, .id = "ID")

CH4_relationships <- lapply(RC, function(df) {
  # Remove rows with NA or zero for CH4 or Q.prop
  df <- df %>% filter(!is.na(CH4_umol_L), !is.na(Q.prop), Q.prop > 0, CH4_umol_L > 0)
  valid_elev <- nrow(df) > 1
  CH4.C.Q.p <- CH4.C.Q.slope <- CH4.C.Q.r2 <- NA

  if (valid_elev) {
    fit <- tryCatch(
      lm(log10(CH4_umol_L) ~ log10(Q.prop), data = df),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      CH4.C.Q.cf <- summary(fit)
      # Check if coefficient exists
      coef_names <- rownames(CH4.C.Q.cf$coefficients)
      if ("log10(Q.prop)" %in% coef_names) {
        CH4.C.Q.p <- CH4.C.Q.cf$coefficients["log10(Q.prop)", "Pr(>|t|)"]
        CH4.C.Q.slope <- CH4.C.Q.cf$coefficients["log10(Q.prop)", "Estimate"]
        CH4.C.Q.r2 <- CH4.C.Q.cf$r.squared
      }
    }
  }

  data.frame(
    CH4.C.Q.p = as.numeric(CH4.C.Q.p),
    CH4.C.Q.slope = as.numeric(CH4.C.Q.slope),
    CH4.C.Q.r2 = as.numeric(CH4.C.Q.r2)
  )
})
CH4_table <- bind_rows(CH4_relationships, .id = "ID")


relationships<-left_join(DOC_table, DIC_table, by='ID')
relationships<-left_join(relationships,CO2_table, by='ID')
relationships<-left_join(relationships,CH4_table, by='ID')%>% separate(ID, into = c("ID", "Long"), sep = "_")

stream_dims <- read_excel("01_Raw_data/Long. Log.xlsx", sheet = "dims") %>%
  rename(UCA=RASTERVALU_04.2025)%>%
  select(Site, ID, distance, UCA)%>%
  separate(Site, into = c("ID", "Long"), sep = "\\.")%>%
  mutate(Long=if_else(ID=='3' & Long=='4','3', Long),
         Long=if_else(ID=='6' & Long=='1','4', Long),
         Long=if_else(ID=='6' & Long=='2','5', Long),
         Long=if_else(ID=='6' & Long=='3','6', Long),
         Long=if_else(is.na(Long), '0', Long),
         ID=if_else(ID=='3','6', ID))%>%
  mutate(distance=if_else(is.na(distance), 0, distance))%>%
  arrange(ID, distance)%>%
  mutate(UCA=if_else(is.na(UCA), 0, UCA))%>%
  group_by(ID)%>%
  mutate(UCA_diff=abs(lag(UCA)-UCA),
         dist_diff=abs(lag(distance)-distance),
         hotspots=UCA_diff/dist_diff)

relationships<-left_join(relationships, stream_dims, by=c('ID','Long'))

ggplot(relationships, aes(x = distance, y = hotspots)) +
  geom_point()+facet_wrap(~ID)

#wetland influence######

wetland_buffers <- read_csv("wetland proportion buffer.csv")%>% filter(Basin %in% c('5', '6', '9')) %>%
  select(Basin, buffer_radius, proportion)%>%rename(ID=Basin)

wetland_proxim <- read_csv("01_Raw_data/wetland_proxim.csv")%>% filter(Site %in% c('5', '6', '9')) %>%
  select(Site, NEAR_DIST)%>%rename(ID=Site, nearest_wetland=NEAR_DIST)

wetland_cover <- read_csv("01_Raw_data/wetland_cover.csv")%>% filter(Basin_Name %in% c('5', '6', '9')) %>%
  select(Basin_Name, PERCENTAGE) %>% rename(ID=Basin_Name, wetland_perc=PERCENTAGE)

wetland<-left_join(wetland_buffers, wetland_proxim, by='ID')
wetland<-left_join(wetland, wetland_cover, by='ID')

write_csv(wetland, "04_Output/wetland_influence.csv")
