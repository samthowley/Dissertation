source("03_Scripts/ch0/data.R")

common.layers<-list(
   geom_point(),
    scale_y_log10(),
   scale_x_log10(),
    #geom_point(aes(y=NEP_corrected), shape=1, color='red')
    stat_poly_line(formula = y ~ x, se = FALSE, color='red'),
    stat_poly_eq(
      aes(label = paste(..p.value.label.., sep = " ~~ ")),
      formula = y ~ x, parse = TRUE,
      size = 5, label.x = "left", label.y = "bottom"),
    facet_wrap(~ID, scales='free')
)

flux%>%
  ggplot(aes(x=Q, y=DO))+
  common.layers
