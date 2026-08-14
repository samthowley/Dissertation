source("03_Scripts/Streams/analysis/data for analysis.R")


ggplot(int.ext, aes(x=TempC, y=internal))+
  theme_minimal()+
  scale_x_log10()+  scale_y_log10()+
  geom_point()+
  facet_wrap(~ID, scales='free')


theme <- list(
  theme_minimal(),
  scale_y_log10(),
  geom_point(),
  stat_poly_line(formula = y ~ x, se = FALSE),
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..p.value.label.., ..rr.label.., sep = " ~~ ")),
    formula = y ~ x, parse = TRUE,
    size = 4, label.x = "right", label.y = "bottom"
  ),
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size=13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ),
  scale_color_manual(
    name="Pathway",
    values = c('external'='#E69F00', 'internal'='#0072B2'),
    labels=c('external'='External Pathway', 'internal'='Internal Pathway')
  ),
  labs(
    y = expression(CO[2]~Flux~(g~C~m^{-2}~day^{-1}))
  )
)

  
a<-int.ext%>%filter(ID %in% c('9'))%>%
  pivot_longer(
    cols = c('external', 'internal'),
    names_to = "pathway",
    values_to = "flux"
  )%>%
  ggplot(aes(x=Q/10^3, y=flux, color=pathway))+
 theme+scale_x_log10()+
  ggtitle(expression(Discharge-CO[2]~Flux~Relationship))+
  xlab(expression(Discharge~m^3~s^-1))
  

temperature<-temperature%>% mutate(day=as.Date(Date))%>%
  group_by(ID, day)%>%
  summarise(
    TempC=mean(TempC, na.rm=T)
  )

b<-int.ext %>% left_join(temperature) %>% filter(ID %in% c('9')) %>%
  pivot_longer(
    cols = c('external', 'internal'),
    names_to = "pathway",
    values_to = "flux"
  ) %>%
  ggplot(aes(x = TempC, y = flux, color = pathway)) +
  theme +
  ggtitle(expression(Temperature-CO[2]~Flux~Relationship)) +
  xlab(expression(Temperature~degree*C))



legend_left  <- get_legend(a)
legend_right <- get_legend(b)


pA_left_noleg  <- a  + theme(legend.position = "none")
pA_right_noleg <- b + theme(legend.position = "none")



panels<-plot_grid(pA_left_noleg,pA_right_noleg, ncol=1, align = 'v')



(figA <- plot_grid(panels, legend_left,
                   ncol = 2,
                   rel_widths = c(0.6, 0.1)))
