source("03_Scripts/Streams/figures/meta analysis.R")
source("03_Scripts/Streams/figures/temporal.R")




Figure_Temp_Spat_Compare<-plot_grid(

plot_grid(
  temp_temperature+
    theme(legend.position = "none"), 
  p_flux_vs_temp+ 
    ggtitle(expression(CO[2]~'Pathway'~'Response'~'to'~'Spatial Differences in Stream Temperature'))
),


plot_grid(
  temp_Q+
    theme(legend.position = "none"), 
  p_flux_vs_Q+ 
    ggtitle(expression(CO[2]~'Pathway'~'Response'~'to'~'Spatial Differences in Discharge'))
),

nrow=2
)
  



ggsave(
  "05_Figures/Figure_Temp_Spat_Compare.jpg",
  plot = Figure_Temp_Spat_Compare,
  width = 16, height = 13, units = "in", dpi = 300
)

