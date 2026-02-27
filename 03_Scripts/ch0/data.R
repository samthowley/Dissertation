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

#hourly
DO <- read_csv("02_Clean_data/DO_cleaned.csv")
CO2<-read_csv("02_Clean_data/CO2_cleaned.csv")
depth<-read_csv("02_Clean_data/depth.csv")
Q<-read_csv("02_Clean_data/discharge.csv")
#daily
area <- read_csv("04_Output/flow_regime_daily.csv") %>%
  select(Date, ID, A, reach, Q_m3.s)
nep<-read_csv("04_Output/gw_corrected_metabolism.csv")
ext.int <- read_csv("04_Output/stream/external-internal.csv")%>%
  select(Date, ID, external, internal)

df_list <- list(CO2, DO, depth, Q)
df <- reduce(df_list, full_join, by=c('Date', 'ID'))%>%
  mutate(Date=as.Date(Date))%>%
  drop_na(DO)%>%
  select(-method, -Water_press)%>%
  group_by(Date, ID) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )%>%  
  distinct(Date, ID, .keep_all = T)


df_list <- list(df, ext.int, nep, area)
combined_df <- reduce(df_list, full_join, by=c('Date', 'ID'))


KH<-combined_df%>%
  mutate(ssn=time2season(Date, out.fmt="seasons"),
         Temp_C = fahrenheit.to.celsius(Temp_PT)) %>%
  mutate(Temp_K=Temp_C+273.15)%>%
  mutate(exp=2400*((1/Temp_K)-(1/298.15))) %>%
  mutate(KH=0.034*2.178^(exp))%>%
  select(-Temp_K, -exp,-Temp_PT)


ks<-KH %>%
  mutate(K600_m.d=K600*depth,
         SchmidtCO2hi=1742-91.24*Temp_C+2.208*Temp_C^2-0.0219*Temp_C^3,
         SchmidtO2hi=1568-86.04*Temp_C+2.142*Temp_C^2-0.0216*Temp_C^3,
         KCO2_m.d=K600_m.d/((600/SchmidtCO2hi)^(-2/3)),
         KO2_m.d=KCO2_m.d/((SchmidtCO2hi/SchmidtO2hi)^(-2/3)),
         DO.sat=Cs(Temp_C))%>% 
  select(-SchmidtCO2hi, -SchmidtO2hi)




conversions<-ks%>%
  arrange(ID, Date)%>%
  group_by(ID)%>%
  mutate(
    
    O2mol=(DO-DO.sat)/32,
    CO2mol=(CO2-400)*(1/10^6)*KH,
    
    O2_flux=KO2_m.d*(DO-DO.sat)
    
    #O2
    # dt_days = as.numeric(difftime(lead(Date), lag(Date), units = "days")),
    # dCdt_obs = (lead(DO) - lag(DO)) / dt_days,
    # F_stor = depth * dCdt_obs,
    # 
    # O2.flux.Q=lead(DO)-lag(DO)*(Q_m3.s*86400/A),
    # 
    # K_flux=(DO-DO.sat)*KO2_m.d,
    # 
    # #cO2
    # CO2_flux=CO2mol*44.01*KCO2_m.d*10^3,
    
        ) %>%
  filter(ID != '14')



conversions%>%
  ggplot(aes(x=Q))+
  geom_point(aes(y=O2_flux, color="O2_flux"))+
  geom_point(aes(y=NEP_corrected, color="NEP"), shape=1)+
  #geom_point(aes(y=CO2_flux, color='total'), shape=1)+
  scale_x_log10()+
  #scale_y_log10()+
  facet_wrap(~ID, scales='free')

