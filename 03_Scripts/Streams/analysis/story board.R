
source("03_Scripts/Streams/analysis/data for analysis.R")

#Interpolating Hotchkiss Data###########

df <- tribble(
  ~discharge_m3_s, ~total, ~total_lo, ~total_hi,
  ~external, ~external_lo, ~external_hi,
  ~internal, ~internal_lo, ~internal_hi,
  # Extended low-discharge rows
  0.0001, 6.5, 5.5, 7.5,  5.5, 4.5, 6.5,  0.6, 0.4, 0.8,
  0.0003, 6.3, 5.3, 7.3,  5.3, 4.3, 6.3,  0.6, 0.4, 0.8,
  0.001,  6.0, 5.1, 7.0,  5.0, 4.1, 6.0,  0.7, 0.5, 0.9,
  0.003,  5.7, 4.9, 6.6,  4.7, 3.8, 5.6,  0.7, 0.5, 0.9,
  0.005,  5.4, 4.7, 6.2,  4.5, 3.6, 5.3,  0.8, 0.6, 1.0,
  # Original rows
  0.01, 5.2, 4.6, 6.0,  4.3, 3.5, 5.0,  0.8, 0.6, 1.0,
  0.02, 5.0, 4.5, 5.8,  4.0, 3.3, 4.7,  0.9, 0.7, 1.1,
  0.05, 4.8, 4.3, 5.5,  3.7, 3.0, 4.3,  1.1, 0.9, 1.3,
  0.1,  4.6, 4.2, 5.2,  3.4, 2.8, 4.0,  1.2, 1.0, 1.4,
  0.2,  4.5, 4.1, 5.0,  3.2, 2.7, 3.8,  1.3, 1.1, 1.5,
  0.5,  4.3, 3.9, 4.7,  3.0, 2.6, 3.4,  1.4, 1.2, 1.6,
  1,    4.1, 3.8, 4.5,  2.9, 2.6, 3.2,  1.2, 1.0, 1.4,
  2,    3.8, 3.4, 4.2,  2.8, 2.5, 3.1,  0.9, 0.7, 1.1,
  5,    3.5, 3.1, 3.9,  2.6, 2.3, 2.9,  0.7, 0.5, 0.9,
  10,   3.2, 2.8, 3.6,  2.4, 2.1, 2.7,  0.6, 0.4, 0.8,
  20,   2.9, 2.6, 3.2,  2.2, 2.0, 2.5,  0.6, 0.4, 0.8,
  50,   2.6, 2.3, 2.9,  1.9, 1.7, 2.2,  0.7, 0.5, 0.9,
  100,  2.3, 2.0, 2.6,  1.7, 1.5, 2.0,  0.7, 0.5, 0.9
) %>%
  mutate(
    total_se    = (total_hi - total_lo) / 2,
    external_se = (external_hi - external_lo) / 2,
    internal_se = (internal_hi - internal_lo) / 2
  )

interp_df <- df %>%
  mutate(logQ = log10(discharge_m3_s)) %>%
  complete(logQ = seq(min(logQ), max(logQ), length.out = 200)) %>%
  arrange(logQ) %>%
  mutate(
    discharge_m3_s = 10^logQ,
    
    total        = approx(log10(df$discharge_m3_s), df$total, xout = logQ)$y,
    total_se     = approx(log10(df$discharge_m3_s), df$total_se, xout = logQ)$y,
    
    external     = approx(log10(df$discharge_m3_s), df$external, xout = logQ)$y,
    external_se  = approx(log10(df$discharge_m3_s), df$external_se, xout = logQ)$y,
    
    internal     = approx(log10(df$discharge_m3_s), df$internal, xout = logQ)$y,
    internal_se  = approx(log10(df$discharge_m3_s), df$internal_se, xout = logQ)$y
  ) %>%
  select(-logQ)
  
cols_to_smooth <- c("total", "total_se", "external", "external_se", "internal", "internal_se", "discharge_m3_s" )

interp_df <- interp_df %>%
  mutate(logQ = log10(discharge_m3_s)) %>%
  mutate(across(
    .cols = all_of(cols_to_smooth),
    .fns  = ~ predict(loess(. ~ logQ, data = cur_data(), span = 0.3)),
    .names = "{.col}_smooth"
  )) %>%
  select(-logQ)


int.ext.summary<-int.ext%>%
  group_by(ID)%>%
  summarise(
    
    discharge_m3_s= mean(Q/10^3, na.rm=T),
    Q.min= min(Q/10^3, na.rm=T),
    Q.max= max(Q/10^3, na.rm=T),
        
    CO2flux.mn=mean(CO2_flux, na.rm=T),
    CO2flux.min=min(CO2_flux, na.rm=T),
    CO2flux.max=max(CO2_flux, na.rm=T),
       
    internal.mn=mean(internal, na.rm=T),
    internal.min=min(internal, na.rm=T),
    internal.max=max(internal, na.rm=T),
    
    external.mn=mean(external, na.rm=T),
    external.min=min(external, na.rm=T),
    external.max=max(external, na.rm=T)
         )%>%
  mutate(spatial=if_else(ID=='13', "Karst", "Tannic"))

comparisons<-full_join(interp_df, int.ext.summary)



ggplot(comparisons, aes(x = discharge_m3_s)) +
  
  geom_ribbon(aes(ymin = total_smooth - total_se_smooth, ymax = total_smooth + total_se_smooth,
                  fill = "Global CO2 Flux (Hotchkiss et al. 2015)"), alpha = 0.5, na.rm = T) +
  geom_ribbon(aes(ymin = external_smooth - external_se_smooth, ymax = external_smooth + external_se_smooth,
                  fill = "Global External Pathway"), alpha = 0.2, na.rm = T) +
  geom_ribbon(aes(ymin = internal_smooth - internal_se_smooth, ymax = internal_smooth + internal_se_smooth,
                  fill = "Global Internal Pathway"), alpha = 0.2, na.rm = T) +
  
  geom_point(aes(x = discharge_m3_s, y = internal.mn, color = "Internally Produced CO2"), size = 3) +
  geom_point(aes(x = discharge_m3_s, y = external.mn, color = "Externally Sourced CO2"), size = 3) +
  geom_point(aes(x = discharge_m3_s, y = CO2flux.mn, color = "Total CO2 Flux"), size = 3) +
  
  scale_fill_manual(name = NULL,
                    values = c("Global CO2 Flux (Hotchkiss et al. 2015)"       = "grey70",
                               "Global External Pathway" = "blue",
                               "Global Internal Pathway" = "red")) +
  scale_color_manual(name = NULL,
                     values = c("Total CO2 Flux"              = "black",
                                "Externally Sourced CO2" = "blue",
                                "Internally Produced CO2" = "darkred")) +
  
  guides(fill  = guide_legend(override.aes = list(alpha = 0.5)),
         color = guide_legend(override.aes = list(size = 3))) +
  
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = expression("Discharge (m"^3~s^-1*")"),
    y = expression(CO[2]~g/m^2/day)
  ) +
  theme_classic(base_size = 14)+
  ggtitle("BEF Comparisons to Global Estimate")



pubs<-read_csv("01_Raw_data/int ext comparison.csv")%>%
  mutate(discharge_m3_s=as.numeric(discharge_m3_s))%>%
  mutate(across(6:18, as.numeric))



full_join(int.ext.summary, pubs)%>%
  mutate(Source=if_else(is.na(Source),"This Paper", Source))%>%
  ggplot(aes(x=Source, y=(internal.mn/CO2flux.mn)*100, color=discharge_m3_s))+
  scale_color_viridis_c()+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 19,
           fill = "grey70", alpha = 0.5) +  
  geom_point(size=3)+
  theme_classic()+
  ggtitle("Internal Pathway Contribution to Low-Order Stream"~CO[2]~"Flux")
