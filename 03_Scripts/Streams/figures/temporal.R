int.ext <- read_csv("04_Output/stream/external-internal.csv")

temp_Q<-int.ext%>%
  pivot_longer(
    cols      = c(internal, external),
    names_to  = "pathway",
    values_to = "flux"
  )%>%
  filter(!ID %in% c('13', '5a'))%>%
  ggplot(
    aes(x = Q, y = flux,
        group = pathway, color=pathway)) +
  geom_point(shape = 1) +
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ID,scales = "free") +
  ggtitle(expression(CO[2]~'Pathway'~'Responses'~'to'~bold('Temporal')~'Fluctuations in Discharge'))+
  ylab(expression(C~'g'/m^2/'day')) +
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
    labels = c( "External", "Internal"))+
  theme_classic(base_size = 13)+
  theme(plot.title = element_text(size = 15, hjust = 0.5))
  





temp_temperature<-int.ext%>%
  pivot_longer(
    cols      = c(internal, external),
    names_to  = "pathway",
    values_to = "flux"
  )%>%  filter(!ID %in% c('13', '5a'))%>%
  ggplot(
    aes(x = TempC, y = flux,
        group = pathway, color=pathway)) +
  geom_point(shape = 1) +
  scale_y_log10()+
  facet_wrap(~ID, scales = "free") +
  ggtitle(expression(CO[2]~'Pathway'~'Responses'~'to'~bold('Temporal')~'Fluctuations in Temperature'))+
  ylab(expression(C~'g'/m^2/'day')) +
  xlab(expression("Temperature ("*degree*C*")"))+
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
    labels = c("External", "Internal"))+
  theme_classic(base_size = 13)+
  theme(plot.title = element_text(size = 15, hjust = 0.5))


