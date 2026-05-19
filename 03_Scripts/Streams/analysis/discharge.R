source("03_Scripts/Streams/analysis/data for analysis.R")

Q.impacts<-int.ext%>%
  filter(external>0.1)

 Q.impacts.long<-int.ext%>%
   filter(external>0.1)%>%
  pivot_longer(
    cols      = c(internal, external, CO2_flux),
    names_to  = "pathway",
    values_to = "flux"
  )

#scatter plots#############
 Q.impacts.long%>%
   #filter(ID=='9')%>%
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

