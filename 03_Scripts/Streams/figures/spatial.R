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
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  )

# Plot pH#############
model <- lm(ext.contrib ~ pH.avg, data = int.ext.avg)
p_val <- summary(model)$coefficients["pH.avg", "Pr(>|t|)"]
p_label_b <- paste0("p = ", signif(p_val, 3))

(b <- int.ext.spat %>%
    mutate(pH.avg = as.factor(pH.avg)) %>%
    ggplot(aes(x = pH.avg, y = ext.contrib)) +
    geom_violin() +
    geom_jitter(width = 0.15, height = 0, alpha = 0.3,
                colour = "black", size = 1.2) +
    theme_minimal() +
    labs(x = "pH", y = "%") +
    ggtitle("External Contribution to Total" ~ CO[2] ~ "flux") +
    geom_rect(
      data = NULL,
      aes(
        xmin = 7.45,
        xmax = nlevels(pH.avg) + 0.55,
        ymin = min(int.ext.spat$ext.contrib, na.rm = TRUE) + 22,
        ymax = max(int.ext.spat$ext.contrib, na.rm = TRUE) + 5
      ),
      fill = NA,
      colour = "red",
      linetype = "dashed",
      linewidth = 0.7,
      inherit.aes = FALSE
    ) +
    annotate("text",
             x = nlevels(as.factor(int.ext.spat$pH.avg)),
             y = max(int.ext.spat$ext.contrib, na.rm = TRUE) + 5,
             label = p_label_b,
             hjust =3, vjust = 1, size = 5) +
    coord_cartesian(clip = "off") +
    common_list)

# Plot wetland perc#############
model <- lm(ext.contrib ~ basin.wetland.perc, data = int.ext.avg)
p_val <- summary(model)$coefficients["basin.wetland.perc", "Pr(>|t|)"]
p_label_a <- paste0("p = ", signif(p_val, 3))

(a <- int.ext.spat %>%
    mutate(
      basin.wetland.perc = round(basin.wetland.perc, 4) * 100,
      basin.wetland.perc = paste(basin.wetland.perc, "%")
    ) %>%
    ggplot(aes(x = as.factor(basin.wetland.perc), y = ext.contrib)) +
    geom_violin() +
    geom_jitter(alpha = 0.3) +
    theme_minimal() +
    labs(x = "Wetland Area/Basin Area", y = "%") +
    annotate("text",
             x = n_distinct(round(int.ext.spat$basin.wetland.perc, 4)),
             y = max(int.ext.spat$ext.contrib, na.rm = TRUE),
             label = p_label_a,
             hjust = 3, vjust = 1, size = 5) +
    coord_cartesian(clip = "off") +
    common_list
)

plot_grid(b, a, ncol = 1)

#plot q###############
model <- lm(ext.contrib ~ q.avg, data = int.ext.avg)
p_val <- summary(model)$coefficients["q.avg", "Pr(>|t|)"]
p_label_c <- paste0("p = ", signif(p_val, 3))

test<-int.ext.spat %>%
  left_join(Q.avg, by = "ID") 
(c <- int.ext.spat %>%
    ggplot(aes(x = as.factor(q.avg), y = ext.contrib)) +
    geom_violin() +
    geom_jitter(alpha = 0.3) +
    theme_minimal() +
    labs(x = expression("Specific Discharge" ~m^2~s^-1), y = "%") +
    # annotate("text",
    #          x = n_distinct(round(int.ext.spat$q, 10)),
    #          y = max(int.ext.spat$ext.contrib, na.rm = TRUE),
    #          label = p_label_c,
    #          hjust = 12, vjust = 1, size = 5) +
    coord_cartesian(clip = "off") +
    common_list
)

#plot RQ###############
model <- lm(ext.contrib ~ RQ, data = int.ext.avg)
p_val <- summary(model)$coefficients["RQ", "Pr(>|t|)"]
p_label_d <- paste0("p = ", signif(p_val, 3))

(c <- int.ext.spat %>%
    ggplot(aes(x = as.factor(RQ), y = int.contrib)) +
    geom_violin() +
    geom_jitter(alpha = 0.3) +
    theme_minimal() +
    labs(x = 'Respiratory Quotient', y = "%") +
    annotate("text",
             x = n_distinct(round(int.ext.spat$RQ, 10)),
             y = max(int.ext.spat$ext.contrib, na.rm = TRUE),
             label = p_label_d,
             hjust =1, vjust = 1, size = 5) +
    coord_cartesian(clip = "off") +
    common_list
)

##comparing q, Q, and T slopes with wetland and pH############
common.layers<-list(
  geom_point(size=4, aes(color=significance)),
    geom_hline(yintercept = 0),
    theme_classic(),
    stat_poly_line(formula = y ~ x, se = FALSE, color='black', linetype='dashed'),
    stat_poly_eq(
      aes(label = paste(..p.value.label..,  sep = " ~~ ")),
      formula = y ~ x, parse = TRUE,
      size = 5, label.x = "right", label.y = "top", vstep = 0.1
    ))


names(T.slopes)
(a<-plot_grid(
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

ncol=1

))


(b<-plot_grid(
  q.slopes%>%
    filter(pathway=='External')%>%
    ggplot(aes(x=pH.avg, y=slope))+
    ggtitle(expression(External~CO[2]~'~q'~Slopes))+
    common.layers
  ,
  q.slopes%>%
    filter(pathway=='Internal')%>%
    ggplot(aes(x=pH.avg, y=slope))+
    ggtitle(expression(Internal~CO[2]~'~q'~Slopes))+
    common.layers,
  
  ncol=1
))
  



(c<-plot_grid(
  q.slopes%>%
    filter(pathway=='External')%>%
    ggplot(aes(x=basin.wetland.perc, y=slope))+
    ggtitle(expression(External~CO[2]~'~q'~Slopes))+
    common.layers
  ,
  q.slopes%>%
    filter(pathway=='Internal')%>%
    ggplot(aes(x=basin.wetland.perc, y=slope))+
    ggtitle(expression(Internal~CO[2]~'~q'~Slopes))+
    common.layers,
  
  ncol=1
))


plot_grid(a,b,c, ncol=3)





(d<-plot_grid(
  q.slopes%>%
    filter(pathway=='External')%>%
    ggplot(aes(x=RQ, y=slope))+
    ggtitle(expression(External~CO[2]~'~RQ'~Slopes))+
    common.layers
  ,
  q.slopes%>%
    filter(pathway=='Internal')%>%
    ggplot(aes(x=RQ, y=slope))+
    ggtitle(expression(Internal~CO[2]~'~RQ'~Slopes))+
    common.layers,
  
  ncol=1
))
