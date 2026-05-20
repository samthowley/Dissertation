source("03_Scripts/Streams/analysis/data for analysis.R")

int.ext <- read_csv("04_Output/stream/external-internal.csv")

Q.lm<-rbind(
  site_lm_table_fun(int.ext, log10(internal), ID, log10(Q))%>%
    mutate(pathway='internal',
           significant=if_else(p_slope<0.01, 'Y', 'N')),
  site_lm_table_fun(int.ext, log10(external), ID, log10(Q))%>%
    mutate(pathway='external',
           significant=if_else(p_slope<0.01, 'Y', 'N'))
  )

Q.lm%>%filter(pathway=='internal')
Q.lm%>%filter(pathway=='external')


T.lm<-rbind(
  site_lm_table_fun(int.ext, log10(internal), ID, log10(TempC))%>%
    mutate(pathway='internal',
           significant=if_else(p_slope<0.01, 'Y', 'N')),
  site_lm_table_fun(int.ext, log10(external), ID, log10(TempC))%>%
    mutate(pathway='external',
           significant=if_else(p_slope<0.01, 'Y', 'N'))
  )


T.lm%>%filter(pathway=='internal')
T.lm%>%filter(pathway=='external')


test<-int.ext%>%filter(ID==6, Q<10)
range(test$Q, na.rm=T)

int.ext%>%
  pivot_longer(
    cols      = c(internal, external, CO2_flux),
    names_to  = "pathway",
    values_to = "flux"
  )%>%
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
    labels = c( "Total","External", "Internal"))+
  theme_minimal()



int.ext%>%
  pivot_longer(
    cols      = c(internal, external, CO2_flux),
    names_to  = "pathway",
    values_to = "flux"
  )%>%
  ggplot(
    aes(x = TempC, y = flux,
        group = pathway, color=pathway)) +
  geom_point() +
  scale_y_log10()+
  facet_wrap(~ID, ncol = 4, scales = "free") +
  ggtitle(expression(CO[2]~'Pathway'~'Responses'~'to'~'Temperature'))+
  ylab(expression(CO[2]~'g'/m^2/'day')) +
  xlab(expression("Temperature"))+
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
    labels = c( "Total","External", "Internal"))+
  theme_minimal()
