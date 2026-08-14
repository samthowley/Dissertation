source("03_Scripts/Streams/analysis/data for analysis.R")

basin_area <- read_csv("01_Raw_data/wetland cover/basin_area.csv")%>%
  select(Basin, Shape_Area)%>%rename(ID=Basin)

pH.avg<-pH%>%
  group_by(ID)%>%summarise(pH.avg=round(mean(pH, na.rm=T), 2))

SpC.avg<-SpC%>%
  group_by(ID)%>%summarise(SpC.avg=round(mean(SpC, na.rm=T), 2))

Q.avg<-discharge%>%
  left_join(basin_area)%>%
  mutate(
    Q=Q/10^3,
    q=Q/Shape_Area)%>%
  group_by(ID)%>%
  summarise(
    Q.avg=round(mean(Q, na.rm=T),2),
    q.avg=mean(q, na.rm=T))

wetland_perc <- read_csv("01_Raw_data/wetland cover/wetland.perc.csv")

T.avg<-temperature%>%
  group_by(ID)%>%summarise(T.avg=round(mean(TempC, na.rm=T), 2))

int.ext.spat<-int.ext%>% left_join(pH.avg)%>%left_join(SpC.avg)%>%
  left_join(Q.avg)%>%left_join(wetland_perc)%>%left_join(T.avg)%>%
  left_join(basin_area, by='ID')%>%left_join(O2_CO2_fluxes, by='ID')%>%
  mutate(
    int.contrib=round(
      (internal/CO2_flux)*100,2),
    ext.contrib=round(
      (external/CO2_flux)*100,2),
    q=Q/10^3/Shape_Area)%>%
  filter(int.contrib<=100, ext.contrib<=100)


O2_CO2_fluxes <- read_csv("04_Output/O2.CO2.fluxes.csv")%>%
  group_by(ID)%>%
  summarise(
    RQ=round(mean(CO2_flux/O2_flux, na.rm=T), 2)
  )

int.ext.avg<-int.ext.spat%>%
  group_by(ID)%>%
  summarise(
    int.avg=round(mean(internal, na.rm=T),2),
    ext.avg=round(mean(external, na.rm=T),2),
    int.contrib=mean(int.contrib, na.rm=T),
    ext.contrib=mean(ext.contrib, na.rm=T),
  )%>% 
  left_join(pH.avg)%>%left_join(SpC.avg)%>%
  left_join(Q.avg)%>%left_join(wetland_perc)%>%left_join(T.avg)%>%
  left_join(O2_CO2_fluxes, by='ID')


temp_slopes <- read_csv("04_Output/stream/temp_slopes.csv")
Q_slopes <- read_csv("04_Output/stream/Q_slopes.csv")
q_slopes <- read_csv("04_Output/stream/q_slopes.csv")


T.slopes<- left_join(int.ext.avg, temp_slopes, by='ID')
q.slopes<- left_join(int.ext.avg,q_slopes, by='ID')


###pH and wetland boxplots###################################################################

common_list <-
  list(
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12)
    )
  )

# Violin Plot#############
(a <- int.ext.spat %>%
    ggplot(aes(x = ID, y = ext.contrib))+
    geom_violin(color = '#E69F00', fill = '#E69F00', alpha = 0.3) +
    geom_jitter( height = 0, alpha = 0.4,
                colour = "#E69F00", size = 1.2) +
    theme_classic() +
    labs(x = "Site", y = "External Contribution (%)")+
    # geom_rect(
    #   data = NULL,
    #   aes(
    #     xmin = 7.45,
    #     xmax = nlevels(pH.avg) + 0.55,
    #     ymin = min(int.ext.spat$ext.contrib, na.rm = TRUE) + 22,
    #     ymax = max(int.ext.spat$ext.contrib, na.rm = TRUE) + 5
    #   ),
    #   fill = NA,
    #   colour = "red",
    #   linetype = "dashed",
    #   linewidth = 0.7,
    #   inherit.aes = FALSE
    # ) +
    common_list)



(b <- int.ext.spat %>%
    ggplot(aes(x = ID, y = int.contrib))+
    geom_violin(color = '#0072B2', fill = '#0072B2', alpha = 0.3) +
    geom_jitter( height = 0, alpha = 0.4,
                 colour = "#0072B2", size = 1.2) +
    theme_classic() +
    labs(x = "Site", y = "Internal Contribution (%)")+
    # geom_rect(
    #   data = NULL,
    #   aes(
    #     xmin = 7.45,
    #     xmax = nlevels(pH.avg) + 0.55,
    #     ymin = min(int.ext.spat$ext.contrib, na.rm = TRUE) + 22,
    #     ymax = max(int.ext.spat$ext.contrib, na.rm = TRUE) + 5
    #   ),
    #   fill = NA,
    #   colour = "red",
    #   linetype = "dashed",
    #   linewidth = 0.7,
    #   inherit.aes = FALSE
    # ) +
    common_list)


ig_title <- ggdraw() +
  draw_label(
    expression("External and Internal Pathway Contribution to Total" ~ CO[2] ~ "Flux"),
    fontface  = "plain",   # bold handled inside expression
    size      = 13,
    x         = 0.5, hjust = 0.5
  )



panels <- plot_grid(a, b,
                    ncol = 1, 
                    align = "x")


(figA <- plot_grid(ig_title, panels,
                   ncol = 1,
                   rel_heights = c(0.07, 1)))





##comparing q, Q, and T slopes with wetland and pH############
common.layers<-list(
  geom_point(size=4, aes(shape=significance)),
    geom_hline(yintercept = 0),
    theme_classic(),
    stat_poly_line(
      formula = y ~ x, se = FALSE,  linetype='dashed'),
    stat_poly_eq(
      aes(label = paste(..p.value.label..,  sep = " ~~ ")),
      formula = y ~ x, parse = TRUE,
      size = 4, label.x = "right", label.y = "top", vstep = 0.05
    ),
  scale_color_manual(
    name="Pathway",
    values = c('External'='#E69F00', 'Internal'='#0072B2'),
    labels=c('External'='External Pathway', 'Internal'='Internal Pathway')
  ),
  scale_shape_manual(
    name="p-value",
    values=c(16,17),
    labels=c('insignificant'='p > 0.05', 'significant'='p ≤ 0.05')
  ),
  theme(
    axis.text = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 15),
    
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size=13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ),
  annotate("text", x = -Inf, y = Inf, label = "Positive Slope",
           hjust = -0.1, vjust = 1.5, size = 4, color = "gray40"),
  annotate("text", x = -Inf, y = -Inf, label = "Negative Slope",
           hjust = -0.1, vjust = -0.5, size = 4, color = "gray40")
  
  )


(a<-T.slopes%>%
  filter(pathway %in% c('External', "Internal"))%>%
  ggplot(aes(x=pH.avg, y=slope, color=pathway))+
  common.layers+
  xlab('Mean pH')+ylab(expression(beta[T]~"(Temperature Slope)")))


b<-T.slopes%>%
  filter(pathway %in% c('External', "Internal"))%>%
  ggplot(aes(x=basin.wetland.perc, y=slope, color=pathway))+
  common.layers+
  xlab("Wetland Cover (%)")+ylab(expression(beta[T]~"(Temperature Slope)"))

temp_panels<-plot_grid(a+ theme(legend.position = "none"),b+ theme(legend.position = "none"), ncol=1)

temp_title <- ggdraw() +
  draw_label(
    expression(bold("Spatial Patterns Effects on Temperature Dependent Slope")),
    fontface  = "plain",   # bold handled inside expression
    size      = 13,
    x         = 0.5, hjust = 0.5
  )

temp_col<-plot_grid(temp_title, temp_panels,
          ncol = 1,
          rel_heights = c(0.07, 1, 0.12))




(c<-q.slopes%>%
    filter(pathway %in% c('External', "Internal"))%>%
    ggplot(aes(x=pH.avg, y=slope, color=pathway))+
    common.layers+
    xlab('Mean pH')+ylab(expression(beta[Q]~"(Discharge Slope)")))


d<-q.slopes%>%
  filter(pathway %in% c('External', "Internal"))%>%
  ggplot(aes(x=basin.wetland.perc, y=slope, color=pathway))+
  common.layers+
  xlab("Wetland Cover (%)")+ylab(expression(beta[Q]~"(Discharge Slope)"))

q_panels<-plot_grid(c+ theme(legend.position = "none"),d+ theme(legend.position = "none"), ncol=1)

q_title <- ggdraw() +
  draw_label(
    expression(bold("Spatial Patterns Effects on Specific-Discharge Dependent Slope")),
    fontface  = "plain",   # bold handled inside expression
    size      = 13,
    x         = 0.5, hjust = 0.5
  )

q_col<-plot_grid(q_title, q_panels,
                 ncol = 1,
                 rel_heights = c(0.07, 1, 0.12))




legend <- get_legend(a)



plot_grid(temp_col, q_col, legend,
                   ncol = 3,
                   rel_widths = c(1, 1, 0.2))

