library(tidyverse)
library(ggplot2)

theme_set(theme(    strip.text = element_text(size = 12),
                    axis.title.y = element_text(size=13, angle=90),
                    axis.title.x = element_text(size=13),
                    axis.text.x = element_text(size=12),
                    axis.text.y = element_text(size=12),
                    panel.grid.major.x = element_blank(),  # Customize x-axis major gridlines
                    panel.grid.minor.y = element_blank(),
                    panel.background = element_rect(fill = 'white'),
                    axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                    axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray")))


legend_size<-theme(
  legend.key.size = unit(1.5, "cm"),
  legend.key.height = unit(1, "cm"),
  legend.key.width = unit(1, "cm"),
  legend.text = element_text(size = 14),
  legend.title = element_text(size = 16)
)





test<-mol.ellipse.lm%>%select(ID,Date,flux_slope, flux_intercept)%>%rename(daily=Date)

test2<-O2.CO2%>%mutate(daily=as.Date(Date))%>%left_join(test)%>%
  filter(!is.na(flux_slope))%>%
  mutate(
    slope.ID = case_when(
      flux_slope > -0.8 ~ "> -0.8",
      flux_slope <= -1.2 ~ "<= -1.2",
      flux_slope > -1.2 & flux_slope <= -0.8 ~ "Between -1.2 and -0.8"
    ),
    anoxia = case_when(
      DO <= 0.5 ~ "< 0.5 mg/L",
      DO <= 1 ~ "< 1 mg/L",
      DO <= 3 ~ "< 3 mg/L",
      TRUE ~ "> 3 mg/L"
    ))

check<-test2%>%filter(ID=='6', flux_slope>0.3)

ggplot(test2, aes(x=CO2_flux, y=O2_flux)) +
  geom_point(aes(color = ssn))+
  scale_color_grey(start = 0.8, end = 0.2)+
  ggnewscale::new_scale_color()+
  geom_smooth(aes(group = daily, color=slope.ID), method = 'lm', se=F)+
  geom_abline(slope = -1, intercept = 0, color = "black", linetype = "dashed")+
  facet_wrap(~ID, scales='free')+facet_wrap(~ID, scales='free')+
  xlab(expression(CO[2]~g/m^2/day))+ylab(expression(O[2]~g/m^2/day))


ggplot(test2, aes(x=CO2_flux, y=O2_flux)) +
  geom_point(aes(color = anoxia), alpha=0.5)+
  scale_color_manual(values=c("red", "darkred","darkorange", "lightgray"))+
  ggnewscale::new_scale_color()+

  geom_smooth(aes(group=daily), method = 'lm', se = FALSE, color='black', size=0.5) +
  geom_abline(slope = -1, intercept = 0, color = "black", linetype = "dashed")+
  xlab(expression(CO[2]~g/m^2/day))+
  ylab(expression(O[2]~g/m^2/day))+facet_wrap(~ID, scales='free')



ggplot(O2.CO2, aes(x=CO2_flux, y=O2_flux, color=ssn, group = daily)) +
  geom_point()+
  geom_abline(slope = -1, intercept = 0, color = "black", linetype = "dashed")+
  stat_poly_line(formula = y ~ x, se = FALSE)+
  facet_wrap(~ID, scales='free')+facet_wrap(~ID, scales='free')


ggplot(check%>% filter(Date<'2023-11-02'), aes(x=Date, y=DO)) +
  geom_point()+ylab('DO mg/L')+
  ggplot(check%>% filter(Date<'2023-11-02'), aes(x=Date, y=CO2)) +
  geom_point()+ylab('CO2 ppm')

#T relationship#########
#CO2
ggplot(data = daily_df, aes(x = Temp_PT, y=CO2.umol.L)) +
  geom_point()+
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(aes(x = Temp_PT, y = CO2.mol.L,
                   label = paste(expression(CO[2]~"Actual:"),..p.value.label.., ..eq.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               size = 4.5, label.y=0.27)+

  ggnewscale::new_scale_color() +
  geom_point(data = daily_df, aes(x = Temp_PT, y=CO2.Sat_umol.L), color='red')+
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(aes(x = Temp_PT, y = CO2.Sat_umol.L,
                   label = paste(expression(CO[2]~"Saturation:"),..p.value.label.., ..eq.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               size = 4.5, label.y=0.2, color='red')+
  ylab(expression(μmol/L))+ xlab("Temperature (F)")+
  facet_wrap(~ID, scales='free')+scale_y_log10()

#O2
ggplot(data = daily_df, aes(x = Temp_PT, y=O2.umol.L)) +
  geom_point()+
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(aes(x = Temp_PT, y = O2.mol.L,
                   label = paste(expression(O[2]~"Actual:"),..p.value.label.., ..eq.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               size = 4.5, label.y=0.27)+

  ggnewscale::new_scale_color() +
  geom_point(data = daily_df, aes(x = Temp_PT, y=O2.Sat_umol.L), color='red')+
  stat_poly_line(formula = y ~ x, se = FALSE, color='black')+
  stat_poly_eq(aes(x = Temp_PT, y = O2.Sat_umol.L,
                   label = paste(expression(O[2]~"Saturation:"),..p.value.label.., ..eq.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               size = 4.5, label.y=0.2, color='red')+
  ylab(expression(μmol/L))+ xlab("Temperature (F)")+
  facet_wrap(~ID, scales='free')+scale_y_log10()


#O2-CO2 stoich figure#######################

ggplot(data = O2.CO2.mol, aes(x = Temp_PT, y=mol.L, color=gas)) +
  geom_point() +
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(aes(x = Temp_PT, y = mol.L,group=gas,
                   label = paste(..p.value.label.., ..eq.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               size = 4.5,vstep=0.09, label.y='top')+
  scale_color_manual(values=c('blue', 'darkorange'),
                     labels = c(expression(CO[2]), expression(O[2])),
                     name=" ")+
  geom_abline(slope = -1, intercept = 0, color = "black", linetype = "dashed")+
  ylab(expression(μmol/L))+xlab("Temperature (F)")+
  facet_wrap(~ID, scales='free', ncol=2)+legend_size

