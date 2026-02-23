source("03_Scripts/Streams/analysis/data for analysis.R")

Q.impacts<-int.ext%>%
  filter(external>0.1)%>%
  mutate(
    ID = factor(as.character(ID), levels = facet_order),
    facet_label = paste0(ID, "\n", round(wetland.perc, 1), "% wetland")
  ) %>%
  mutate(
    facet_label = factor(
      facet_label,
      levels = paste0(facet_order, "\n",
                      round(wetland.perc[match(facet_order, ID)], 1),
                      "% wetland")
    ))

 Q.impacts.long<-int.ext%>%
   filter(external>0.1)%>%
  pivot_longer(
    cols      = c(internal, external, CO2_flux),
    names_to  = "pathway",
    values_to = "flux"
  )%>%
   mutate(
     ID = factor(as.character(ID), levels = facet_order),
     facet_label = paste0(ID, "\n", round(wetland.perc, 1), "% wetland")
   ) %>%
   mutate(
     facet_label = factor(
       facet_label,
       levels = paste0(facet_order, "\n",
                       round(wetland.perc[match(facet_order, ID)], 1),
                       "% wetland")
     ))

#scatter plots#############
 Q.impacts.long%>%filter(ID=='9')%>%
ggplot(
       aes(x = Q, y = flux,
           group = pathway, color=pathway)) +
  geom_point() +
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ID, ncol = 4, scales = "free") +
  ggtitle(expression(CO[2]~'Pathway'~'Responses'~'to'~'Discharge'))+
  ylab(expression(CO[2]~'g'/m^2/'day')) +
  xlab(expression('Discharge'~'L'~s^-1))+
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = " ~~ "), color=pathway,
        group=pathway),
    formula = y ~ x, parse = TRUE,
    size = 4, label.x = "right", label.y = "bottom", vstep = 0.05
  )+
  scale_colour_manual(
    name = "Pathway",
    values = col,
    labels = c( "Total","External", "Internal"))



plot_grid(
  Q.impacts%>%
    filter(ID %in% c('5','9'))%>%
    ggplot(
      aes(x = Q, y = external)) +
    geom_point(aes(color=CO2_flux), color='gray') +
    scale_y_log10()+scale_x_log10()+
    facet_wrap(~ID, ncol = 4, scales = "free") +
    ggtitle(expression('Total'~CO[2]~'Flux'~'Response'~'to'~'Discharge'))+
    xlab(expression('Discharge'~'L'~s^-1))+
    ylab(expression(C~'g'/m^2/'day')) +

    stat_poly_line(formula = y ~ x, se = FALSE, color='black')+
    stat_poly_eq(
      aes(label = paste(..p.value.label.., sep = " ~~ ")),
      formula = y ~ x, parse = TRUE,
      size = 5, label.x = "right", label.y = "bottom", color='black'
    ),

  Q.impacts%>%
    filter(ID %in% c('5','9'))%>%
    ggplot(
      aes(x = Q, y = external)) +
    geom_point(aes(color=internal), color='black',shape=1) +
    scale_y_log10()+scale_x_log10()+
    facet_wrap(~ID, ncol = 4, scales = "free") +
    ggtitle(expression('External Pathway'~'Response'~'to'~'Discharge'))+
    xlab(expression('Discharge'~'L'~s^-1))+
    ylab(expression(C~'g'/m^2/'day')) +

    stat_poly_line(formula = y ~ x, se = FALSE, color='black')+
    stat_poly_eq(
      aes(label = paste(..p.value.label.., sep = " ~~ ")),
      formula = y ~ x, parse = TRUE,
      size = 5, label.x = "right", label.y = "bottom", color='black'
    ),

  Q.impacts%>%
    filter(ID %in% c('5','9'))%>%
ggplot(
       aes(x = Q, y = internal)) +
  geom_point(aes(color=internal), color='red', shape=1) +
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ID, ncol = 4, scales = "free") +
  ggtitle(expression('Internal Pathway'~'Response'~'to'~'Discharge'))+
  xlab(expression('Discharge'~'L'~s^-1))+
  ylab(expression(C~'g'/m^2/'day')) +

  stat_poly_line(formula = y ~ x, se = FALSE, color='red')+
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = " ~~ ")),
    formula = y ~ x, parse = TRUE,
    size = 5, label.x = "right", label.y = "bottom", color='red'
  )
,

nrow=3
)





Q.impacts%>%
  filter(ID %in% c('5','9'))%>%
ggplot(
       aes(x = Q, y = int.ext.ratio)) +
  geom_point() +
  geom_hline(yintercept = 1, color='red')+
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ID, ncol = 4, scales = "free") +
  ggtitle(expression('Internal:External'~'Response'~'to'~'Discharge'))+
  xlab(expression('Discharge'~'L'~s^-1))+
  ylab("Internal / External") +

  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(
    aes(label = paste(..p.value.label..,  sep = " ~~ ")),
    formula = y ~ x, parse = TRUE,
    size = 5, label.x = "left", label.y = "top", vstep = 0.1
  )

#boxplots############

Q.impacts%>%
  mutate(
    ID = factor(as.character(ID), levels = facet_order)
  )%>%
  drop_na(Q, internal, external)%>%
  group_by(ID)%>%
  mutate(
    class=
      case_when(
        Q>mean(Q, na.rm=T)~'Discharge > mean',
        Q<mean(Q, na.rm=T)~'Discharge < mean')
  )%>%ungroup()%>%
  group_by(class, ID)%>%
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
  ggtitle("Mean Pathway Response to Flow")+
  common.list




#just CO2 flux and discharge###########
mean(Q.impacts$CO2, na.rm=T)
mean(Q.impacts$CO2_flux, na.rm=T)* (12/44)
mean(Q.impacts$Q, na.rm=T)

plot_grid(

  Q.impacts%>%
    group_by(ID)%>%
    summarise(
      CO2=mean(CO2, na.rm=T)
    )%>%
    mutate(setting="Bradford Experimental Forest")%>%
    ggplot(aes(
      x=setting,
      y = CO2)) +
    geom_boxplot(size=1)+
    geom_jitter(aes(color=ID), size=3)+
    ylab(expression(CO[2]~'ppm'))+
    ggtitle(expression('Mean:'~11746~"ppm"))+
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(size=17, color='black')
    )
,
Q.impacts%>%
  group_by(ID)%>%
  summarise(
    CO2_flux=mean(CO2_flux, na.rm=T)*(12/44)
  )%>%
  mutate(setting="Bradford Experimental Forest")%>%
ggplot(aes(
  x=setting,
    y = CO2_flux)) +
  geom_boxplot(size=1)+
  geom_jitter(aes(color=ID), size=3)+
  ylab(expression(C~'g'/m^2/'day'))+
  ggtitle(expression('Mean:'~6.48~ g/m^2/day))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=17, color='black')
  )

,

Q.impacts%>%
  group_by(ID)%>%
  summarise(
    Q=median(Q, na.rm=T)/ 10^3
  )%>%
  mutate(setting="Bradford Experimental Forest")%>%
  ggplot(aes(
    x=setting,
    y = Q)) +
  geom_boxplot(size=1)+
  geom_jitter(aes(color=ID), size=3)+
  ylab(expression("Discharge"~m^3/s))+
  ggtitle(expression('Mean:'~0.106~m^3/sec))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=17, color='black')
  )
,
nrow=1
)

#slopes##########


rbind(
site_lm_table_fun(Q.impacts, log10(internal), ID, log10(Q)) %>%
  mutate(pathway = "Internal") %>%
  rename(slope = slope, p = p_slope)
,
site_lm_table_fun(Q.impacts, log10(external), ID, log10(Q)) %>%
  mutate(pathway = "External") %>%
  rename(slope = slope, p = p_slope)
,
site_lm_table_fun(Q.impacts, log10(CO2_flux), ID, log10(Q)) %>%
  mutate(pathway = "Total") %>%
  rename(slope = slope, p = p_slope)
)%>%
  mutate(
    significance=if_else(p<=0.005, "significant", "insignificant")
  )%>%
  ggplot(aes(
    x=pathway,
    y = slope,
    fill=pathway)) +
  scale_fill_manual(values=c('black', 'darkred', 'gray'))+
  geom_boxplot()+
  geom_jitter(aes(shape=significance), size=5, color='blue')+
  labs(fill='Pathway', shape="p-value", color="Sites")+
  geom_hline(yintercept = 0, linetype="dashed", color='green', size=3)+
  ggtitle("Mean Pathway Response to Flow Variability")+
  ylab(expression('Pathway~Discharge'~'Slope'~(g/m^5/day)))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=17, color='black')
  )


site_lm_table_fun(Q.impacts, log10(int.ext.ratio), ID, log10(Q)) %>%
  mutate(Ratio = "Internal",
         significance=if_else(p_slope<=0.005, "significant", "insignificant")) %>%
  mutate(setting="Bradford Experimental Forest")%>%
  ggplot(aes(
    x=setting,
    y = slope)) +
  geom_boxplot(size=1)+
  geom_jitter(aes(shape=significance), size=5, color='blue')+
  labs(color='Sites')+
  geom_hline(yintercept = 0, linetype="dashed", color='green', size=3)+
  ylab("Internal/External Ratio~ Discharge Slope")+
  ggtitle("Pathway Dominance in Response to Discharge")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),

  )

