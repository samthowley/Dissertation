source("03_Scripts/Streams/analysis/data for analysis.R")


a<-int.ext %>%
  mutate(ratio = avg.internal / avg.external) %>%
  filter(ratio > 0.001, ratio < 10^4) %>%
  ggplot(aes(x = ID, y = ratio)) +
  common_layers_violin +
  xlab("Stream Sites") +
  ylab("Average Internal / External")


b<-int.ext%>%
  pivot_longer(
  cols = c("avg.internal", "avg.external"), # Select columns starting with "Year_"
  names_to = "pathway",           # Name the new column for variable names "Year"
  values_to = "flux")%>%
  filter(flux > 0.001, flux <45) %>%
  ggplot(aes(
    x = reorder(ID_wetland.perc, as.numeric(wetland.perc)),
               y = flux, color=pathway)) +
  geom_boxplot(size=1)+
  #geom_jitter(shape=1)+
    ylab(expression(CO[2]~'g'/m^2/'day'))+
  scale_color_manual(
    name   = "Pathways",
    labels = c("External", "Internal"),
    values = c("avg.external" = "black",
               "avg.internal" = "red")
  )+xlab("Wetland Cover %")+
  scale_x_discrete(labels = labels_vec_wetperc)

plot_grid(a,b, ncol=1)

summary(lm(avg.internal ~ as.numeric(wetland.perc), data = int.ext))
summary(lm(avg.external ~ as.numeric(wetland.perc), data = int.ext))
summary(lm(int.ext.ratio ~ as.numeric(wetland.perc), data = int.ext%>%filter(int.ext.ratio > 0.001, int.ext.ratio < 10^4)))

