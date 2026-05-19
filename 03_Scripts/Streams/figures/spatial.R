source("03_Scripts/Streams/analysis/data for analysis.R")

basin_area <- read_csv("01_Raw_data/wetland cover/basin_area.csv")%>%
  select(Basin, Shape_Area)%>%rename(ID=Basin)

pH.avg<-pH%>%
  group_by(ID)%>%summarise(pH.avg=round(mean(pH, na.rm=T), 2))

SpC.avg<-SpC%>%
  group_by(ID)%>%summarise(SpC.avg=round(mean(SpC, na.rm=T), 2))

flashiness <- read_csv("04_Output/stream/flashiness.csv")%>%
  select(-n_days)

wetland_perc <- read_csv("01_Raw_data/wetland cover/wetland.perc.csv")

NEP<- read_csv("04_Output/stream/gw_corrected_metabolism.csv")%>%
  group_by(ID)%>%
  summarise(NEP=mean(NEP_corrected, na.rm=T))

T.avg<-temperature%>%
  group_by(ID)%>%summarise(T.avg=round(mean(TempC, na.rm=T), 2))


DOC <- read_csv("02_Clean_data/allC_stream.csv")%>%
  group_by(ID)%>%
  summarise(DOC=mean(DOC, na.rm=T))

int.ext.avg<-int.ext.spat%>%
  group_by(ID)%>%
  summarise(
    int.avg=round(mean(internal, na.rm=T),2),
    ext.avg=round(mean(external, na.rm=T),2),
    int.contrib=mean(int.contrib, na.rm=T),
    ext.contrib=mean(ext.contrib, na.rm=T),
  )%>% 
  left_join(pH.avg)%>%
  left_join(SpC.avg)%>%
  left_join(flashiness)%>%
  left_join(NEP)%>%
  left_join(wetland_perc)%>%
  left_join(T.avg)%>%
  left_join(basin_area, by='ID')%>%
  left_join(flashiness)%>%
  left_join(DOC)

power_slopes <- read_csv("04_Output/stream/power_slopes.csv")%>%left_join(int.ext.avg)

common.layers<-list(
  geom_point(size=4, aes(shape=significance)),
    geom_hline(yintercept = 0),
    theme_classic(),
    stat_poly_line(
      formula = y ~ x, se = FALSE,  linetype='dashed'),
    stat_poly_eq(
      aes(label = paste(..p.value.label..,  sep = " ~~ ")),
      formula = y ~ x, parse = TRUE,
      size = 4, label.x = "right", label.y = "top", vstep = 0.09
    ),
  scale_color_manual(
    name="Pathway",
    values = c('red', 'black'),
    labels=c('external'='External Pathway', 'internal'='Internal Pathway')
  ),
  scale_shape_manual(
    name="p-value",
    values=c(16,17),
    labels=c('insignificant'='>0.005', 'significant'='<=0.005')
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



power_slopes<-power_slopes%>%
  filter(pathway %in% c('External', "Internal"))%>%
  rename(`Mean pH`=pH.avg, 
         `Wetland Percent Cover`=basin.wetland.perc,
         `Specific Conductance (µS/cm)`=SpC.avg,
         `R-B Index`=RB_index,
         `CV of Daily Flows`=CV
         )%>%
  pivot_longer(
    cols = c('Mean pH', 
             'Wetland Percent Cover', 
             "Specific Conductance (µS/cm)",
             "R-B Index", 
             "NEP",
             "CV of Daily Flows", 'DOC'),
    values_to = "metric",
    names_to='indep'
  ) 


power_slopes %>%
  filter(indep=="DOC")%>%
  ggplot(aes(x = metric, y = b, color = pathway)) +
  common.layers +
  ylab(expression(beta)) +
  facet_wrap(~ driver + indep, scales = 'free', ncol=2)+
  theme(axis.title.x = element_blank())

names(power_slopes)

