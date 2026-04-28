source("03_Scripts/Streams/analysis/data for analysis.R")



basin_area <- read_csv("01_Raw_data/wetland cover/basin_area.csv")%>%
  select(Basin, Shape_Area)%>%rename(ID=Basin)

pH.avg<-pH%>%
  group_by(ID)%>%summarise(pH.avg=round(mean(pH, na.rm=T), 2))

SpC.avg<-SpC%>%
  group_by(ID)%>%summarise(SpC.avg=round(mean(SpC, na.rm=T), 2))

Q.avg<-discharge%>%
  left_join(basin_area)%>%
  # mutate(
  #   Q=Q/10^3,
  #   q=Q/Shape_Area)%>%
  group_by(ID)%>%summarise(Q.avg=round(mean(Q, na.rm=T),2))

wetland_perc <- read_csv("01_Raw_data/wetland cover/wetland.perc.csv")

T.avg<-temperature%>%
  group_by(ID)%>%summarise(T.avg=round(mean(TempC, na.rm=T), 2))

int.ext.spat<-int.ext%>% left_join(pH.avg)%>%left_join(SpC.avg)%>%
  left_join(Q.avg)%>%left_join(wetland_perc)%>%left_join(T.avg)%>%
  mutate(
    int.contrib=round(
      (internal/CO2_flux)*100,2),
    ext.contrib=round(
      (external/CO2_flux)*100,2))%>%
  filter(int.contrib<=100, ext.contrib<=100)

int.ext.avg<-int.ext.spat%>%
  group_by(ID)%>%
  summarise(
    int.avg=round(mean(internal, na.rm=T),2),
    ext.avg=round(mean(external, na.rm=T),2),
    int.contrib=mean(int.contrib, na.rm=T),
    ext.contrib=mean(ext.contrib, na.rm=T),
  )%>% 
  left_join(pH.avg)%>%left_join(SpC.avg)%>%
  left_join(Q.avg)%>%left_join(wetland_perc)%>%left_join(T.avg)

summary(lm(pH.avg ~ int.contrib, data = int.ext.avg))
summary(lm(SpC.avg ~ int.contrib, data = int.ext.avg))
summary(lm(Q.avg ~ int.contrib, data = int.ext.avg))
summary(lm(basin.wetland.perc ~ int.contrib, data = int.ext.avg))


temp_slopes <- read_csv("04_Output/stream/temp_slopes.csv")
Q_slopes <- read_csv("04_Output/stream/Q_slopes.csv")



T.slopes<- left_join(int.ext.avg, temp_slopes, by='ID')
summary(lm(pH.avg ~ slope, data =T.slopes%>%filter(pathway=="Internal")))
summary(lm(pH.avg ~ slope, data =T.slopes%>%filter(pathway=="External"))) #sig


Q.slopes<- left_join(int.ext.avg, Q_slopes, by='ID')
summary(lm(pH.avg ~ slope, data =Q.slopes%>%filter(pathway=="Internal")))
summary(lm(pH.avg ~ slope, data =Q.slopes%>%filter(pathway=="External"))) #sig


######################################################################
common_list<-
  list(
    theme(
      plot.title = element_text(hjust = 0.5, size=16),
      axis.title.y =  element_text(size=14),
      axis.text =  element_text(size=11))
  )



model <- lm(ext.contrib ~ pH.avg, data = int.ext.avg)
p_val <- summary(model)$coefficients["pH.avg", "Pr(>|t|)"]
p_label <- paste0("p = ", signif(p_val, 3))


(b<-int.ext.spat %>%
  mutate(pH.avg = as.factor(pH.avg)) %>%                 # make pH a factor
  ggplot(aes(x = pH.avg, y = ext.contrib)) +             # core mapping
  geom_violin() +   # violin shape
  geom_jitter(width = 0.15, height = 0, alpha = 0.3,      # scatter detail
              colour = "black", size = 1.2) +
  theme_minimal() +
  labs(x = "pH", y = "%") +
  ggtitle("External Contribution to Total"~CO[2]~"flux")+
  
  geom_rect(
    data = NULL,
    aes(
      xmin = 7.45,              # left edge of the first factor level
      xmax = nlevels(pH.avg) + 0.55,  # right edge beyond last factor level
      ymin = min(int.ext.spat$ext.contrib, na.rm = TRUE)+22,   # bottom edge
      ymax = max(int.ext.spat$ext.contrib, na.rm = TRUE)+5    # top edge
    ),
    fill = NA,
    colour = "red",
    linetype = "dashed",
    linewidth = 0.7,
    inherit.aes = FALSE
  )+
    
    annotate("text", x = Inf, y = Inf, label = p_label,
             hjust = 1.1, vjust = 38, size = 5)+
    
    common_list)



model <- lm(ext.contrib ~ basin.wetland.perc, data = int.ext.avg)
p_val <- summary(model)$coefficients["basin.wetland.perc", "Pr(>|t|)"]
p_label <- paste0("p = ", signif(p_val, 3))


(a<-int.ext.spat%>%
  mutate(
    basin.wetland.perc=round(basin.wetland.perc, 4)*100,
    basin.wetland.perc=paste(basin.wetland.perc, "%")
  )%>%
  ggplot(aes(x=as.factor(basin.wetland.perc), y=ext.contrib))+
  geom_violin()+
  geom_jitter(alpha=0.3)+
  theme_minimal()+
  #xlab('pH')+
  labs(x = "Wetland Area/Basin Area", y = "%") +
    annotate("text", x = Inf, y = Inf, label = p_label,
             hjust = 9.1, vjust = 42, size = 5)+
    
  common_list
)

plot_grid(b,a, ncol=1)


##############
common.layers<-list(
  geom_point(size=4, aes(shape=significance, color=r2)),
    geom_hline(yintercept = 0),
    theme_classic(),
    stat_poly_line(formula = y ~ x, se = FALSE),
    stat_poly_eq(
      aes(label = paste(..p.value.label..,  sep = " ~~ ")),
      formula = y ~ x, parse = TRUE,
      size = 5, label.x = "right", label.y = "top", vstep = 0.1
    ))



a<-plot_grid(
T.slopes%>%
  filter(pathway=='External')%>%
  ggplot(aes(x=pH.avg, y=slope))+
  ggtitle(expression(External~CO[2]~'~Temperature'~Slopes))+
  common.layers
,
T.slopes%>%
  filter(pathway=='Internal')%>%
  ggplot(aes(x=pH.avg, y=slope))+
  ggtitle(expression(Internal~CO[2]~'~Temperature'~Slopes))+
  common.layers,

ncol=2

)


b<-plot_grid(
  Q.slopes%>%
    filter(pathway=='External')%>%
    ggplot(aes(x=pH.avg, y=slope))+
    ggtitle(expression(External~CO[2]~'~Q'~Slopes))+
    common.layers
  ,
  Q.slopes%>%
    filter(pathway=='Internal')%>%
    ggplot(aes(x=pH.avg, y=slope))+
    ggtitle(expression(Internal~CO[2]~'~Q'~Slopes))+
    common.layers,
  
  ncol=2
)
  

plot_grid(a,b, ncol=1)
