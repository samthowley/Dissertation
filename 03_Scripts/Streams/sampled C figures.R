
theme_set(theme(axis.text.x = element_text(size = 12, angle=0),
                axis.text.y = element_text(size = 17, angle=0),
                axis.title =element_text(size = 17, angle=0),
                plot.title = element_text(size = 17, angle=0),
                legend.key.size = unit(0.8, 'cm'),
                legend.text=element_text(size = 17),
                legend.title =element_text(size = 17),
                legend.position ="none",
                panel.background = element_rect(fill = 'white'),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")))


library(psycho)
all_sampled_C <- read_csv("04_Output/sampled.solid.carbon.csv")%>%
  filter(chapter=='stream')%>%
  filter(!is.na(ID))%>%
  left_join(read_csv("04_Output/flow_regime_daily.csv"))%>%
  mutate(DOC.export=Q*DOC,
         DIC.export=Q*DIC,
         POC.export=Q*POC)

names(all_sampled_C)

tern_data<-all_sampled_C%>%filter(!is.na(POC))

POC<-all_sampled_C %>% select(Date,ID,Q,depth, POC)%>% rename(Conc=POC)%>%mutate(Species= 'POC')
DIC<-all_sampled_C %>% select(Date,ID,Q,depth, DIC)%>% rename(Conc=DIC)%>%mutate(Species= 'DIC')
DOC<-all_sampled_C %>% select(Date,ID,Q,depth, DOC)%>% rename(Conc=DOC)%>%mutate(Species= 'DOC')

long_C<- rbind(POC, DIC, DOC)%>%
  mutate(export=Q*Conc)

order <- c("5", "5a", "15", "9", '13', '6', '6a', '3', '7')

library(ggtern)

discharge <- expression("Discharge"~L~s^-1)

common_layers <- list(  scale_color_gradient(
  low = "blue", high = "red", name = discharge),
    geom_point(size =01, shape=1, stroke=2),
    theme_minimal_grid(),
    theme(
      # Axis tick labels
      tern.axis.text.T = element_text(size = 14),
      tern.axis.text.L = element_text(size = 14),
      tern.axis.text.R = element_text(size = 14),
      # Facet labels
      strip.text = element_text(size = 18, face = "bold"),
      # Legend
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      legend.key.height = unit(0.7, "cm"),
      legend.key.width = unit(2, "cm"),
      # General
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16),
      legend.position = "bottom"),
  coord_tern(expand = TRUE)
)

ggtern(
  data = all_sampled_C ,aes(DOC.export, DIC.export, POC.export, colour = Q)) +
  common_layers+
  labs(
    x = "DOC
    (mg/s)",
    y = "DIC
(mg/s)",
    z = "POC
(mg/s)")+
  theme(
    tern.axis.title.T = element_text(size = 14),
    tern.axis.title.L = element_text(size = 14),
    tern.axis.title.R = element_text(size = 14))+
  ggtitle("Flatwood Stream Ternary Plot")


tern<-ggtern(
  data = all_sampled_C %>% filter(!ID=='6'),aes(DOC, DIC, POC, colour = Q)) +
  common_layers+
  theme(
    tern.axis.title.T = element_blank(),
    tern.axis.title.L = element_blank(),
    tern.axis.title.R = element_blank(),
    legend.position = "none")

ggsave(filename = "05_Figures/tern6.jpeg",
       plot = tern6,
       width = 8, height = 6, units = "in")

ggsave(filename = "05_Figures/tern.jpeg",
       plot = tern,
       width = 12, height = 6, units = "in")


#DOC Violin plots#######


DOC<-all_sampled_C %>%mutate(
  Basin=case_when(ID=='5'~'5',ID=='5a'~'5',ID=='15'~'15',
                  ID=='3'~'6',ID=='7'~'7',ID=='6'~'6',ID=='6a'~'6',
                  ID=='9'~'9', ID=='13'~'13'))

wetland_cover <- read_csv("01_Raw_data/wetland cover/wetland_cover.csv")%>%
  select(Basin_Name, PERCENTAGE) %>% rename(Basin=Basin_Name, wetland_perc=PERCENTAGE)%>%
  mutate(wetland_perc=round(wetland_perc, 2))

TOC<-full_join(DOC, POC)
TOC<-full_join(TOC, DIC)

DOC_wet<-full_join(TOC, wetland_cover)%>%
  mutate(ID_wet=paste(ID, wetland_perc, sep="_"),
         TOC=POC+DOC)%>%
  filter(!ID %in% c('6a', '14'))%>%
  filter(!is.na(ID))

DOC_wet$wetland_perc <- factor(
  DOC_wet$wetland_perc,
  levels = sort(unique(DOC_wet$wetland_perc), decreasing = TRUE))

labels_vec <- setNames(
  paste0(DOC_wet$ID, "\n", DOC_wet$wetland_perc),
  DOC_wet$ID_wet)


ggplot(DOC_wet %>% filter(!is.na(wetland_perc)),
          aes(x = reorder(ID_wet, -as.numeric(wetland_perc)),
              y = DIC)) +
  scale_y_log10()+
  geom_boxplot(size=1)+
  geom_jitter(color='blue')+
  theme(axis.title.x = element_text(size=21, angle=0),
        axis.title.y= element_text(size=21, angle=90),
        plot.title = element_text(size = 21))+
  scale_x_discrete(labels = labels_vec)+
  ylab("DIC mg/L")+xlab('Wetland Areal Cover %')+
  ggtitle("DIC Concentrations Across Sites")

range(DOC_wet$DOC, na.rm = T)
ggsave(filename = "05_Figures/DOC.across.sites.jpeg",
       plot = a,
       width = 8, height = 5, units = "in")
#C-Q relationship##########
library(lme4)
spec_DOC<-all_sampled_C%>%select(ID, Date, Q, DOC)%>%
  rename(conc=DOC)%>%mutate(type='DOC')

summary(lmList(log10(conc) ~ log10(Q) | ID, data=spec_DOC))


spec_DIC<-all_sampled_C%>%select(ID, Date, Q, DIC)%>%
  rename(conc=DIC)%>%mutate(type='DIC')

summary(lmList(log10(conc) ~ log10(Q) | ID, data=spec_DIC))


spec_POC<-all_sampled_C%>%select(ID, Date, Q, POC)%>%
  rename(conc=POC)%>%mutate(type='POC')

summary(lmList(log10(conc) ~ log10(Q) | ID, data=spec_POC))


Cspecs<-rbind(spec_DOC, spec_DIC, spec_POC)

ggsave(filename = "05_Figures/C_scatter_plots.jpeg",
       plot =
         ggplot(Cspecs%>% filter(Q>2),aes(x=Q, y=conc, color=type)) +
         geom_point()+
         geom_smooth(method = 'lm', se=F)+
         scale_y_log10()+scale_x_log10()+
         ylab('mg/L')+xlab("Discharge (L/s)")+
         facet_wrap(~ID, scales='free')+
         theme(legend.position = "bottom"),
       width = 8, height = 6, units = "in")


ggplot(spec_DOC%>% filter(Q>2),aes(x=Date, y=conc*Q)) +
  geom_point()+
  geom_smooth(method = 'lm', se=F)+
  scale_y_log10()+
  ylab('DOC Flux /L')+xlab("Discharge (L/s)")+
  facet_wrap(~ID, scales='free')+
  stat_poly_line(formula = y ~ x, se = FALSE) +
  stat_poly_eq(
    aes(
        label = paste(..p.value.label.., ..eq.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = 1,  # Forces placement at min of log-scaled x
    label.y = -1)


#C gas boxplots

gas.samples <- read_csv("04_Output/gas.samples.csv")

distance_theme <- list(
  geom_boxplot(),
  facet_wrap(~chapter, scales='free', ncol=1),
  theme(
    strip.text = element_text(size = 12),
    axis.title.y = element_text(size=13),
    axis.title.x = element_text(size=13),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12)
  ))

ggplot(
  gas.samples%>%filter(type=='CH4', chapter!='long'), aes(x = Site, y = water_umol.L)) +
  distance_theme+
  ylab(expression(CH[4]~umol/L))


atmospheric.equil<-gas.samples %>%
  mutate(
    CH4.atm.eq=if_else(type=='CH4', water_umol.L/0.003, water_umol.L)
  )

ggplot(
  atmospheric.equil%>%filter(type=='CH4', chapter!='long'), aes(x = Site, y = CH4.atm.eq)) +
  distance_theme+
  ylab(expression(CH[4]/atmospheric~concentrations))

