library(ggbreak)
library(scales)
library(ggpmisc)
library(cowplot)

common_layers<-list(
  geom_point(shape = 1, size = 2, stroke = 1.4),
    facet_wrap(~ID, scales = 'free'),
    scale_x_log10(),scale_y_log10(),
    scale_color_gradient(high = 'red', low = 'blue', name="Sampling Point"),
    stat_poly_line(formula = y ~ x, se = FALSE),
    stat_poly_eq(
      formula = y ~ x,
      aes(
        label = paste(after_stat(p.value.label), sep = "~~~")),
      parse = TRUE,size = 6,
      label.x.npc = "left",label.y.npc = "bottom",
      vstep = 0.07),
    labs(names = 'Sampling point', x = expression('Discharge'~m^3~s^-1), y = "DOC mg/L"),
    theme(
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 16),
      legend.key.size = unit(1, 'cm'),
      legend.position = 'none',
      axis.title.x = element_blank())
  )

title <- ggdraw() +
  draw_label("Flatwood Stream Longitudinal C-Q Dynamics", fontface = "bold",size=40)

ggsave(filename = paste0("05_Figures/C-Q dynamics.jpeg"),
       plot =
    plot_grid(
         ggplot(c.q, aes(x = Q.prop, y = DOC, color=Long, group = Long)) +
           common_layers+ylab('DOC mg/L'),

         ggplot(c.q, aes(x = Q.prop, y = DIC, color=Long, group = Long)) +
           common_layers+ylab('DIC mg/L'),

         ggplot(c.q, aes(x = Q.prop, y = CO2_umol_L, color=Long, group = Long)) +
           common_layers+ylab(expression(CO[2]~umol/L)),

         ggplot(c.q, aes(x = Q.prop, y = CH4_umol_L, color=Long, group = Long)) +
           common_layers+ylab(expression(CH[4]~umol/L))+
           theme(legend.position = 'bottom', axis.title.x = element_text()),

         ncol=1, rel_heights = c(1,1,1,1.3)
         ) ,
       width = 18, height = 17, units = "in")





CO2.slope<-relationships %>% select(ID, UCA, CO2.C.Q.slope)%>%
  rename(slope=CO2.C.Q.slope)%>%mutate(type='CO2')
CH4.slope<-relationships %>% select(ID, UCA, CH4.C.Q.slope)%>%
  rename(slope=CH4.C.Q.slope)%>%mutate(type='CH4')
DIC.slope<-relationships %>% select(ID, UCA, DIC.C.Q.slope)%>%
  rename(slope=DIC.C.Q.slope)%>%mutate(type='DIC')

slope<-rbind(CO2.slope, CH4.slope, DIC.slope)

ggplot(slope %>% filter(ID == '6'),
       aes(x = UCA/10^6, y = slope, color = type)) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "UCA (km)", y = "Slope", color= " ")+ggtitle("Stream 6 log-log C-Q Slopes")+
  geom_hline(yintercept = 0, linewidth=3)+
geom_point(size=4)+
stat_poly_line(formula = y ~ x, se = FALSE)+
stat_poly_eq(
  formula = y ~ x,
  aes(
    label = paste(after_stat(p.value.label), sep = "~~~")),
  parse = TRUE,size = 6,
  label.x.npc = "left",label.y.npc = "bottom",
  vstep = 0.05)+
theme(
  legend.title = element_text(size = 24),
  legend.text = element_text(size = 20),
  legend.key.size = unit(0.53, 'cm'),
  legend.position = "bottom")


library(ggplotly)
(plot_grid(

  ncol=1
  ))

names(relationships)


ggsave(filename = paste0("05_Figures/Long.H1.jpeg"),
       plot = plot_grid(Long_DOC), width = 31, height = 20, units = "in")

