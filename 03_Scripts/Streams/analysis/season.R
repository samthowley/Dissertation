source("03_Scripts/Streams/analysis/data for analysis.R")



season.impacts<-int.ext%>%
  mutate(
    season = time2season(Date, out.fmt = "seasons"),
    season = factor(season, levels = c("summer", "spring", "autumn", "winter"))
  )

season.impacts.long<-int.ext%>%
  mutate(
    season = time2season(Date, out.fmt = "seasons"),
    season = factor(season, levels = c("summer", "spring", "autumn", "winter"))
    )%>%
  pivot_longer(
    cols      = c(internal, external),
    names_to  = "pathway",
    values_to = "flux"
  )%>%
  filter(NEP_corrected<0, flux>1e-4)


season.impacts%>%
  ggplot(aes(x= Q,
             y= internal/external,
             color=season))+

  ggtitle(expression('External'~CO[2]~'Response'~'to'~"Temperature"))+

  geom_point()+
  scale_y_log10()+scale_x_log10()+
  facet_wrap(~ID, scales='free')



season.impacts %>%filter(internal<60)%>%
  ggplot(aes(x = season,
            y = internal
            )) +
  geom_boxplot(aes(group=season), outlier.shape = NA, fill='red')+
  facet_wrap(~ID, scales='free')


season.impacts %>%
  ggplot(aes(x = season,
             y = external
  )) +
  geom_boxplot(aes(group=season), outlier.shape = NA, fill='black')+
  facet_wrap(~ID, scales='free')

season.impacts %>%
  ggplot(aes(x = season,
             y = depth
  )) +
  scale_y_log10()+
  geom_boxplot(aes(group=season), outlier.shape = NA, fill='blue')+
  facet_wrap(~ID, scales='free')



ggplot(season.impacts.long %>%
       aes(x = season,
           y = flux)) +
  geom_boxplot(aes(color = pathway, group=interaction(season,pathway)),
    position = position_dodge2(width = 0.9),
               size = 1) +
  geom_jitter(aes(color = pathway),      # jitter by pathway color
              position = position_jitterdodge(jitter.width = 0.2),
              shape = 1, alpha = 0.7) +
  facet_wrap(~ ID, scales = "free_y") +
  ggtitle(expression('Seasonal'~'Variation'~'in'~CO[2]~'Sources'))+
  ylab(expression(CO[2]~'g'/m^2/'day'))+
  theme(axis.title.x = element_blank())




B<-ggplot(season.impacts %>%
            filter(flux > 0.001, flux < 45, pathway=='avg.internal'),
          aes(x = season,
              y = flux)) +

  geom_boxplot(aes(color = pathway, group=interaction(season,pathway)),
               position = position_dodge2(width = 0.9),
               size = 1) +

  geom_jitter(aes(color = pathway),      # jitter by pathway color
              position = position_jitterdodge(jitter.width = 0.2),
              shape = 1, alpha = 0.7) +

  facet_wrap(~ ID, scales = "free_y") +

  ggtitle(expression('Seasonal'~'Variation'~'in'~CO[2]~'Sources'))+
  ylab(expression(CO[2]~'g'/m^2/'day'))+

  theme(axis.title.x = element_blank())+
  scale_color_manual(
    name   = "Pathways",
    labels = c("External", "Internal"),
    values = c("avg.external" = "black",
               "avg.internal" = "red")
  )

plot_grid(A,B, nrow = 1)
