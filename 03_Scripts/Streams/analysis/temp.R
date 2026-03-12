source("03_Scripts/Streams/analysis/data for analysis.R")

temp.impacts<-left_join(int.ext, temperature)%>%
  mutate(
    ID = factor(as.character(ID), levels = facet_order),
    facet_label = paste0(ID, "\n", round(wetland.perc, 1), "% wetland"),
    facet_label = factor(
      facet_label,
      levels = paste0(facet_order, "\n",
                      round(wetland.perc[match(facet_order, ID)], 1),
                      "% wetland")))


temp.impacts%>%
  group_by(ID)%>%
  summarise(
    TempC=median(TempC, na.rm=T)
  )%>%
  summarise(
    TempC=median(TempC, na.rm=T))


#scatter plots############

common.layers<-list(
  facet_wrap(~ID, scales='free', nrow=1),
    xlab('Temperature (\u00B0C)'),
    ylab(expression(CO[2]~'g'/m^2/'day')),
    scale_y_log10(),
    stat_poly_eq(
      aes(label = paste(..p.value.label.., sep = " ~~ ")),
      formula = y ~ x, parse = TRUE,
      size = 5, label.x = "left", label.y = "bottom")
  )


plot_grid(

temp.impacts%>%
  filter(ID %in% c('6','7'))%>%
  ggplot(
    aes(x= TempC,
        y= CO2_flux)) +
  geom_point(color='gray')+
  stat_poly_line(formula = y ~ x, se = FALSE, color='black')+
  ylab(expression(CO[2]~'g'/m^2/'day')) +
  ggtitle(expression('Total'~CO[2]~'Response'~'to'~'Temperature'))+
  common.layers
,
temp.impacts%>%
  filter(ID %in% c('6','7'))%>%
ggplot(
       aes(x= TempC,
           y= external)) +
  geom_point(color='black')+
  stat_poly_line(formula = y ~ x, se = FALSE, color='black')+
  ylab(expression(CO[2]~'g'/m^2/'day')) +
  ggtitle(expression('External Pathway'~'Response'~'to'~'Temperature'))+
  common.layers
,
temp.impacts%>%
  filter(ID %in% c('6','7'))%>%
  ggplot(
    aes(x= TempC,
        y= internal)) +
  geom_point(color='red')+
  stat_poly_line(formula = y ~ x, se = FALSE, color='red')+
  ylab(expression(CO[2]~'g'/m^2/'day')) +
  ggtitle(expression('Internal Pathway'~'Response'~'to'~'Temperature'))+
  common.layers,

ncol=1
)

temp.impacts%>%
  filter(ID %in% c('5','7'))%>%
  ggplot(
    aes(x= TempC,
        y= int.ext.ratio)) +
  geom_point() +
  geom_hline(yintercept = 1, color= 'red')+
  scale_y_log10()+
  facet_wrap(~ID, ncol = 4, scales = "free") +
  ggtitle(expression('Internal:External'~'Response'~'to'~'Temperature'))+
  xlab('Temperature (\u00B0C)')+
  ylab("Internal/External") +
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(
    aes(label = paste(..p.value.label..,  sep = " ~~ ")),
    formula = y ~ x, parse = TRUE,
    size = 5, label.x = "left", label.y = "top", vstep = 0.1
  )

temp.impacts%>%
  filter(ID==3)%>%
  pivot_longer(
    cols=c(internal, external, CO2_flux),
    names_to = 'Pathway',
    values_to = 'Flux'
  )%>%  ggplot(
    aes(x= TempC,
        y= Flux,
        group=Pathway, 
        color=Pathway)) +
  geom_point() +
  scale_color_manual(values=c('darkgray','black', 'darkred'))+
  scale_y_log10()+
  ggtitle(expression(CO[2]~"Pathway"~'Response'~'to'~'Temperature'))+
  xlab('Temperature (\u00B0C)')+
  ylab(expression(CO[2]~'g'/m^2/'day')) +
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(
    aes(label = paste(..p.value.label..,  sep = " ~~ ")),
    formula = y ~ x, parse = TRUE,
    size = 5, label.x = "right", label.y = "bottom", vstep = 0.07
  )+facet_wrap(~ID, scales='free')

#boxplots#############

temp.impacts%>%
  mutate(
    ID = factor(as.character(ID), levels = facet_order)
  )%>%
  drop_na(Q, internal, external)%>%
  group_by(ID)%>%
  mutate(
    class=
      case_when(
        TempC>mean(TempC, na.rm=T)~'Temperature (\u00B0C) > mean',
        TempC<mean(TempC, na.rm=T)~'Temperature (\u00B0C) < mean')
  )%>%ungroup()%>%
  filter(!is.na(class))%>%
  group_by(ID, class)%>%
  summarise(
    Internal=mean(internal, na.rm=T),
    External=mean(external, na.rm=T),
    Total=mean(CO2_flux, na.rm=T)
  )%>%
  pivot_longer(
    cols      = c(Internal, External, Total),
    names_to  = "Pathway",
    values_to = "flux"
  )%>%
  ggplot(aes(
    x=class,
    y = flux,
    fill=Pathway)) +
  scale_fill_manual(values=c('white', 'darkred', 'darkgray'))+
  geom_boxplot()+
  scale_y_log10()+
  ggtitle("Mean Pathway Response to Temperature")

#means########

all.means<-temp.impacts%>%
  group_by(ID)%>%
  summarise(
    mean.internal=mean(internal, na.rm=T),
    mean.external=mean(external, na.rm=T)
  )%>%
  mutate(temp.state="global")%>%
  drop_na()%>%
  rbind(
    temp.state.mean<-temp.impacts%>%
          group_by(ID, temp.state)%>%
          summarise(
            mean.internal=mean(internal, na.rm=T),
            mean.external=mean(external, na.rm=T)
          )%>%drop_na())


div.means<-all.means%>% filter(temp.state=='cool')%>%
  rename(
    mean.int.cool=mean.internal,
    mean.ext.cool=mean.external
  )%>%
  left_join(all.means%>% filter(temp.state=='warm')%>%
              rename(
                mean.int.warm=mean.internal,
                mean.ext.warm=mean.external
              ), by=c("ID"))%>%
  mutate(
    int.diff=mean.int.warm-mean.int.cool,
    ext.diff=mean.ext.warm-mean.ext.cool
  )


ggplot(div.means,
       aes(x= ID))+
  ggtitle(
    "Mean Difference Between Cool and Warm Days",
    subtitle = expression(
      atop(
        mu[cool] == "mean during cool days",
        mu[warm] == "mean during warm days"
      ))) +
  geom_point(size=3, aes(y=int.diff), color='red')+
  geom_point(size=3, aes(y=ext.diff), color='black')+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth=1)+
  ylab(expression(mu[warm]-mu[cool]))+
  theme(
    plot.subtitle = element_text(size = 16)
  )


#Q+temp############

temp.impacts%>%
  ggplot(aes(x= Q,
             y= TempC))+

  ggtitle("Correlation Between Discharge and Temperature")+
  xlab('Temperature (\u00B0C)')+
  ylab("Discharge L/s")+

  geom_point()+
  scale_x_log10()+
  facet_wrap(~facet_label, scales='free', nrow=2)+
  stat_poly_line(formula = y ~ x, se = FALSE, color='red')+
  stat_poly_eq(
    aes(label = paste(..p.value.label.., ..rr.label..,sep = " ~~ ")),
    size = 4, label.x = "right", label.y = "bottom", color='red')




temp.impacts%>%
  ggplot(aes(x= Q,
             y= external,
             color=TempC))+
  scale_color_gradient(high='orange', low = 'blue')+
  ylab("Internal/External")+

    ggtitle(expression('Internal: External'~'Response'~'to'~"Temperature"))+

    geom_point()+
    scale_y_log10()+scale_x_log10()+
    facet_wrap(~facet_label, scales='free')




ggplot(temp.impacts, aes(x= Q,y=internal,
           color= TempC))+
  scale_color_gradient(high='orange', low = 'blue')+

  ggtitle(expression('Internal'~CO[2]~'Response'~'to'~"Temperature"))+

  geom_point()+
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ID, scales='free')+
  stat_poly_line(formula = y ~ x, se = FALSE)+
stat_poly_eq(
  aes(label = paste(..p.value.label.., sep = " ~~ ")),
  formula = y ~ x, parse = TRUE,
  size = 4, label.x = "left", label.y = "top", vstep = 0.1
)

ggplot(temp.impacts, aes(x= Q,y=external,
                         color= TempC))+
  scale_color_gradient(high='orange', low = 'blue')+

  ggtitle(expression('External'~CO[2]~'Response'~'to'~"Temperature"))+

  geom_point()+
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ID, scales='free')+
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = " ~~ ")),
    formula = y ~ x, parse = TRUE,
    size = 4, label.x = "left", label.y = "top", vstep = 0.1
  )


#extract linear model#############

common.layers<-list(
  geom_boxplot(),
    geom_jitter(aes(shape=significance), size=5, color='blue'),
    labs(fill='Pathway', shape="p-value", color="Sites"),
    geom_hline(yintercept = 0, linetype="dashed", color='green', size=2),
    ggtitle("Mean Pathway Response to Temperature Variability"),
    ylab(expression('Pathway~Temperature'~~'Slope')),
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(size=17, color='black')
    )

)

rbind(
  site_lm_table_fun(temp.impacts, internal, ID, TempC) %>%
    mutate(pathway = "Internal") %>%
    rename(slope = slope, p = p_slope)
  ,
  site_lm_table_fun(temp.impacts, external, ID, TempC) %>%
    mutate(pathway = "External") %>%
    rename(slope = slope, p = p_slope)
  ,
  site_lm_table_fun(temp.impacts, CO2_flux, ID, TempC) %>%
    mutate(pathway = "Total") %>%
    rename(slope = slope, p = p_slope)
)%>%
  mutate(
    setting="Bradford Experimental Forest",
    significance=if_else(p<=0.005, "significant", "insignificant")
  )%>%
  ggplot(aes(
    x=pathway,
    y = slope,
    fill=pathway)) +
  common.layers+
  scale_fill_manual(values=c('black', 'darkred', 'gray'))


site_lm_table_fun(temp.impacts, int.ext.ratio, ID, TempC) %>%
  rename(slope = slope, p = p_slope)%>%
  mutate(
    setting="Bradford Experimental Forest",
    significance=if_else(p<=0.005, "significant", "insignificant")
  )%>%
  ggplot(aes(
    x=setting,
    y = slope
    )) +
  common.layers+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),

  )

