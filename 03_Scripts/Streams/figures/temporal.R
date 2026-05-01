source("03_Scripts/Streams/analysis/data for analysis.R")


ggplot(int.ext, aes(x=Q, y=internal))+
  theme_minimal()+
  scale_x_log10()+  scale_y_log10()+
  geom_point()+
  facet_wrap(~ID, scales='free')


theme<-list(
  
  theme_minimal(),
    scale_x_log10(),  scale_y_log10(),
    geom_point(),
    stat_poly_line(formula = y ~ x, se = FALSE),
    stat_poly_eq(
      aes(label = paste(..p.value.label.., sep = " ~~ ")),
      formula = y ~ x, parse = TRUE,
      size = 5, label.x = "right", label.y = "bottom"
    ),
    theme(
      axis.text = element_text(size=12),
      legend.position = "right")
  )

  
int.ext%>%filter(ID %in% c('6', '5', '3'))%>%
  ggplot(aes(x=Q, y=internal, color=ID))+
  labs(
    x = expression(Discharge~L~s^-1),
    y = expression("Internal"~CO[2]~(C~g/m^2/day))
  ) +theme
  



int.ext%>%filter(ID %in% c('6', '5', '3'))%>%
  ggplot(aes(x=Q, y=external, color=ID))+
  labs(
    x = expression(Discharge~L~s^-1),
    y = expression("Internal"~CO[2]~(C~g/m^2/day))
  ) +theme


