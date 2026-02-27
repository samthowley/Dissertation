library(tidyverse)
library(writexl)
library(readxl)
library(lubridate)
library(weathermetrics)
library(tools)
library(cowplot)
library(ggpmisc)
library('StreamMetabolism')
library(hydroTSM)

DO <- read_csv("02_Clean_data/DO_cleaned.csv")
CO2<-read_csv("02_Clean_data/CO2_cleaned.csv")
depth<-read_csv("02_Clean_data/depth.csv")

df_list <- list(CO2, DO, depth)
combined_df <- reduce(df_list, full_join, by=c('Date', 'ID'))%>%
  mutate(date=as.Date(Date))%>%
  distinct(Date, ID, .keep_all = T)

master_metabolism <- read_csv("04_Output/master_metabolism.csv")

combined<-left_join(combined_df, master_metabolism, by=c('date', 'ID'))%>%
  arrange(ID, Date)%>%
  filter(complete.cases(DO, CO2))

KH<-combined%>%
  mutate(ssn=time2season(Date, out.fmt="seasons"),
                         Temp_C = fahrenheit.to.celsius(Temp_DO)) %>%
           mutate(Temp_K=Temp_C+273.15)%>%
           mutate(exp=2400*((1/Temp_K)-(1/298.15))) %>%
           mutate(KH=0.034*2.178^(exp))

mols<-KH %>%
  mutate(CO2_molL=(CO2/10^6)*KH,
         DO_molL=DO/32000)

ks<-mols %>%
  mutate(K600_m.d=K600*depth,
         SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3,
         SchmidtO2hi=1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3,
         KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3)),
         KO2_m.d=KCO2_m.d/((SchmidtCO2hi/SchmidtO2hi)^(-2/3)),
         DO.sat=Cs(Temp_C))#%>% select(day, ID, reactor, Q, Qbase, depth, KCO2_d, KH)

flux<-ks%>%
  mutate(
    CO2_flux=KCO2_m.d*(CO2-400)*KH*(1/10^6)*44.01*10^3,
    O2_flux=KO2_m.d*(DO-DO.sat),
    o2.co2=O2_flux/CO2_flux) %>%
  filter(ID != '14')%>%
  select(Date, ID, CO2_flux, O2_flux, o2.co2)

write_csv(flux, "04_Output/O2.CO2.fluxes.csv")


#Slope##########

flux_lm <- function(df) {
  if(nrow(df) < 2) return(NULL)  # skip if not enough data for a regression

  flux.lm <- lm(CO2_flux ~ O2_flux, data = df)
  cf <- coef(flux.lm)

  tibble(
    ID = df$ID[1],
    day = df$day[1],
    flux_slope = cf[2],
    flux_intercept = cf[1]
  )
}

flux.lm <- flux %>%
  group_by(ID, day) %>%
  group_split() %>%
  map_dfr(flux_lm)

flux_slope <- left_join(flux, flux.lm, by = c('day', 'ID')) %>%
  mutate(
    slope_type = case_when(
      flux_slope <= -0.7 & flux_slope >= -1.2 ~ '-1:1',
      flux_slope > -0.7 ~ 'CO2.dom',
      flux_slope < -1.2 ~ 'O2.dom',
      TRUE ~ NA_character_
    )
  )

median(flux_slope$flux_slope, na.rm=T)

ggplot(flux_slope %>%filter(ID %in% c('5','6')), aes(x = Q, y = flux_slope, color = slope_type)) +
  geom_point(shape = 1) +
  scale_x_log10()+
  geom_hline(yintercept = 1) +
  theme(legend.position = "bottom")

#################







