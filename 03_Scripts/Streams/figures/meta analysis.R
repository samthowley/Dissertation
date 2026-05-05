
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




int.ext.summary<-left_join(int.ext, pH)%>%
  group_by(ID)%>%
  summarise(
    discharge_m3_s= mean(Q/10^3, na.rm=T),
    CO2flux.mn=mean(CO2_flux, na.rm=T),
    internal.mn=mean(internal, na.rm=T),
    external.mn=mean(external, na.rm=T),
    pH=mean(pH, na.rm=T)
         )%>%
  rename(Site=ID)%>%
  mutate(
    Citation="This Paper",
    Location="Florida, Coastal Plain",
    Biome="Subtropical",
    Source="Shallow Aquifer",
    Source=if_else(Site==13, "Deeper Groundwater Seepage", Source)
                    )


pubs<-read_csv("01_Raw_data/meta_analysis_extraction.csv")%>%
  select(Citation, Location, Biome, Source, Discharge_m3s, CO2_flux_gCm2day, Internal_Pathway_gCm2day, External_Pathway_gCm2day,
         pH)%>%
  rename(
    discharge_m3_s = Discharge_m3s,
    CO2flux.mn = CO2_flux_gCm2day,
    internal.mn = Internal_Pathway_gCm2day,
    external.mn = External_Pathway_gCm2day
  )%>%
  mutate(across(5:9, as.numeric))%>%
  filter(!is.na(internal.mn))%>%
  full_join(int.ext.summary)%>%
   mutate( pct_internal = (internal.mn / CO2flux.mn) * 100) %>%
  arrange(discharge_m3_s) %>%
  mutate(
    # Sub-label with mean discharge
    x_label = paste0(Source, "\n(", round(discharge_m3_s, 3), " m³ s⁻¹)"),
    x_label = factor(x_label, levels = unique(x_label))  # preserve Q order
  )%>%    filter(external.mn > 0, internal.mn>0.1)



unique(pubs$Source)

(b<-pubs%>%
    ggplot(aes(x = Citation, y = pct_internal, color = Source)) +
  
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 19,
                fill = "Global estimate (Hotchkiss et al. 2015)"),
            inherit.aes = FALSE) +
  geom_point(size = 3) +
  
  
  #BUILD LEGEND######
  scale_fill_manual(name = NULL, values = c("Global estimate (Hotchkiss et al. 2015)" = "lightgrey")) +

  scale_color_manual(
    name = "Source Water",
    values = c(
      "Riparian groundwater"            = "#E3C849",
      "Shallow Aquifer"                 = "#EDCA7F",
      "Wetland Seepage"                 = "#EAC34B",
      "Groundwater-fed"                 = "#378ADD",
      "Deeper Groundwater Seepage"      = "#008bcc",
      "Groundwater-dominated"           = "#3885B6",
      "Spring-fed"                      = "#5EA5C2",
      "Mixed"                           = "#8338AC",
      "Snowmelt + Groundwater Baseflow" = "#924CA1"
    ),
    breaks = c(
      "Riparian groundwater","Shallow Aquifer", "Wetland Seepage", "Groundwater-fed",
      "Deeper Groundwater Seepage", "Groundwater-dominated", "Spring-fed", "Mixed", "Snowmelt + Groundwater Baseflow"
    ),
    na.translate = FALSE
  ) +
  
  #FORMAT FIGURE###########
  labs(
    x = NULL,
    y = "Internal pathway contribution (%)",
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 345, hjust = 0, vjust = 1, size=10),
    axis.text.y = element_text(size=12),
    axis.title=element_text(size=12),
    legend.position = "right"
  ))


##################

comparisons<-full_join(interp_df, pubs)


a<-comparisons %>%
  pivot_longer(cols = c("internal.mn", "CO2flux.mn"),
               names_to = "Pathway",
               values_to = "CO2_flux") %>%
  ggplot(aes(x = discharge_m3_s)) +
  
  geom_ribbon(aes(ymin = internal_smooth - internal_se_smooth, ymax = internal_smooth + internal_se_smooth,
                  fill = "Global Internal Pathway"), alpha = 0.7, na.rm = T) +
  
  geom_ribbon(aes(ymin = total_smooth - total_se_smooth, ymax = total_smooth + total_se_smooth,
                  fill = "Total Flux (Hotchkiss et al. 2015)"), alpha = 0.7, na.rm = T) +
  
  geom_ribbon(aes(ymin = external_smooth - external_se_smooth, ymax = external_smooth + external_se_smooth,
                  fill = "Global External Pathway"), alpha = 0.5, na.rm = T) +
  geom_line(aes(y = CO2_flux, group = Site, color = Source),    # <-- replace Site with your ID column
            alpha = 0.4, linewidth = 0.5) +
  geom_point(aes(x = discharge_m3_s, y = CO2_flux, color = Source, shape = Pathway), size = 3) +
  #BUILD LEGEND#############
scale_fill_manual(name = NULL,
                  values = c(
                    "Total Flux (Hotchkiss et al. 2015)" = "black",
                    "Global Internal Pathway" = "grey",
                    "Global External Pathway" = "lightgrey"
                  )) +
  scale_color_manual(
    name = "Source Water",
    values = c(
      "Riparian groundwater"            = "#E3C849",
      "Shallow Aquifer"                 = "#EDCA7F",
      "Wetland Seepage"                 = "#EAC34B",
      "Groundwater-fed"                 = "#378ADD",
      "Deeper Groundwater Seepage"      = "#008bcc",
      "Groundwater-dominated"           = "#3885B6",
      "Spring-fed"                      = "#5EA5C2",
      "Mixed"                           = "#8338AC",
      "Snowmelt + Groundwater Baseflow" = "#924CA1"
    ),
    breaks = c(
      "Riparian groundwater", "Shallow Aquifer", "Wetland Seepage", "Groundwater-fed",
      "Deeper Groundwater Seepage", "Groundwater-dominated", "Spring-fed", "Mixed", "Snowmelt + Groundwater Baseflow"
    ),
    na.translate = FALSE
  ) +
  scale_shape_manual(
    name = expression(CO[2]~"Flux"),
    values = c(
      "internal.mn"  = 16,
      "CO2flux.mn"   = 1
    ),
    labels = c(
      "internal.mn" = "Internal Pathway",
      "CO2flux.mn"  = "Total Flux"
    )
  ) +
  #FORMAT FIGURE############
scale_x_log10() +
  scale_y_log10() +
  labs(
    x = expression("Discharge (m"^3~s^-1*")"),
    y = expression(CO[2]~g/m^2/day)
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "right"
  )
###########################

title   <- ggdraw() + draw_label("Internal Pathway Contribution in Low-Order Stream"~ CO[2] ~"Flux",
                                 size = 16)
legend  <- get_legend(a)


(panels  <- plot_grid(b + theme(legend.position = "none"), a + theme(legend.position = "none"),
                     ncol = 1,
                     rel_heights = c(0.7,1), align = "v"))
  
  
  
(body    <- plot_grid(panels, legend, ncol = 2, rel_widths = c(0.6, 0.2)))
plot_grid(title, body, ncol = 1, rel_heights = c(0.05, 1))



