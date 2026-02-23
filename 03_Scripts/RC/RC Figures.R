#packages#####
rm(list=ls())

library(tidyverse)
library(writexl)
library(readxl)
library(cowplot)
library(lme4)
library(ggpmisc)

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

DIC.flux<-expression('DIC'~~('mg'~m^-2~day^-1))
DOC.flux<-expression('DOC'~~('mg'~m^-2~day^-1))

#Call in data############
file_path <- "04_Output/RC/RC_by_well.xlsx"
sheet_names <- excel_sheets(file_path)
RC_df <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
}) %>%
  bind_rows()


RC_df<-read_csv('04_Output/RC/master.RC.csv')%>%
  separate(Site, into = c("ID_GW", "Well"), sep = "GW", remove = FALSE)%>%
  filter(Site!='5GW7')


#Hypothesis 1: RC carbon is the primary source of stream carbon ##############

distance_theme <- list(
  geom_boxplot(width = 1),
  geom_jitter(shape=1),
  xlab("Distance (m)"),
  facet_wrap(~ID, ncol = 3, scales='free'),
  scale_fill_manual(values=c('lightblue',"lightgreen", 'red')),
  labs(fill = "Wells"),
  theme(
    legend.position = 'bottom',
    strip.text = element_text(size = 12),
    axis.title.y = element_text(size=13),
    axis.title.x = element_text(size=13),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.53, "cm"))
  )

distance_theme_top <- list(
  theme(axis.title.x= element_blank(),
        axis.text.x=element_blank(),
        legend.position = 'none'))

RC.DC<- read_csv("04_Output/RC/RC.DC.sample.flux.csv")%>%
  separate(Site, into = c("ID_GW", "Well"), sep = "GW", remove = FALSE)%>%
  filter(!is.na(ID))
b<-ggplot(
  RC.DC, aes(x = distance.from.stream, y = DOC, group = Well, fill=well_types)) +
    ylab("DOC mg/L") + distance_theme+distance_theme_top

a<-ggplot(
  RC.DC, aes(x = distance.from.stream, y = DIC, group = Well, fill=well_types)) +
  ylab("DIC mg/L") + distance_theme+distance_theme

RC.gas <- read_csv("04_Output/RC/RC.gas.sample.flux.csv")%>%
  separate(Site, into = c("ID", "Well"), sep = "GW", remove = FALSE)

c<-ggplot(
  RC.gas, aes(x = distance.from.stream, y = CO2_molL, group = Well, fill=well_types)) +
    ylab(expression(CO[2]~'mol'~L[-1])) + distance_theme

plot_grid(b,a, ncol=1, rel_heights = c(1,1.3))



RC.DC.factor<-RC.DC%>%mutate(distance.from.stream=as.factor(round(distance.from.stream,1)))%>%
  filter(!is.na(DOC))
RC.gas.factor <-RC.gas%>%mutate(distance.from.stream=as.factor(round(distance.from.stream,1)))


plot_grid(ggplot(
  RC.DC.factor, aes(x = distance.from.stream, y = DOC, group = Well, fill=well_types)) +
  ylab("DOC mg/L")+ distance_theme+distance_theme_top,

ggplot(
  RC.DC.factor, aes(x = distance.from.stream, y = DIC, group = Well, fill=well_types)) +
  ylab("DIC mg/L") + distance_theme,

ncol=1)



plot_grid(ggplot(
  RC.gas.factor, aes(x = distance.from.stream, y = CH4.water_umol.L, group = Well, fill=well_types)) +
    ylab(expression(CH[4]~"umol/L"))+ distance_theme+distance_theme_top,

  ggplot(
    RC.gas.factor, aes(x = distance.from.stream, y = CO2.water_umol.L, group = Well, fill=well_types)) +
    ylab(expression(CO[2]~"umol/L"))+ distance_theme,

  ncol=1)



ggsave(filename="05_Figures/RC_elevation_conc.jpeg",
       plot = plot_grid(a,b, ncol=1, rel_heights = c(1,1,1,1.3)),
       width =13,height = 12, units = "in")


#Hypothesis 2: RC fluxes will be greatest during periods of increased watershed inundation###########

s.e <- read_excel("01_Raw_data/RW.log.xlsx", sheet = "well dims")%>%
  select(Site, surface.elevation, well.bottom.elevation)

RC_s.e<-left_join(RC_df, s.e, by='Site')%>%
  arrange(ID, distance.from.stream)

for.labels<-RC_s.e%>%select(Site,surface.elevation, well.bottom.elevation)%>%
  distinct(Site, .keep_all = T)

surface.e <- for.labels[, c("Site", "surface.elevation")]
well.bottom <- for.labels[, c("Site", "well.bottom.elevation")]


common_layers <- list(
  geom_point(size = 2),
  stat_poly_line(formula = y ~ x, se = FALSE, alpha = 0.1),
  facet_wrap(~ Site, scales = 'free'),
  ylab("WTE (datum: Stream Benthos)"),
  theme(
    legend.position = 'bottom',
    strip.text = element_text(size = 12),
    axis.title.y = element_text(size = 15, angle = 90),
    axis.title.x = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 12)),
  #geom_hline(data = surface.e, aes(yintercept = surface.elevation), size=1, color='gray', linetype='dashed'),
  labs(color = " ")
)


ggplot(RC_s.e,
       aes(x = qL_m2.sec, y = DOC )) +

  stat_poly_eq(
    aes(x = CO2.water.ppm, y = WTE, label = paste(..p.value.label.., sep = "~~~")),
    formula = y ~ x, parse = TRUE,
    size = 5,
    label.x ='right',label.y = 'top',
  )+common_layers+ylab("DOC mg/L")+xlab(expression(qL~m^2/sec))



ggplot(RC_s.e,
       aes(x = DIC_g.m2.day, y = WTE)) +
  stat_poly_eq(
    aes(x = DIC_g.m2.day, y = WTE, label = paste(..p.value.label.., sep = "~~~")),
    formula = y ~ x, parse = TRUE,
    size = 5,
    label.x ='left',label.y = 'bottom',
  )+common_layers+xlab(DIC.flux)




ggplot(RC_s.e, aes(x = Date)) +
  geom_point(aes(y=CH4.water.ppm, color='CH4'),size = 1, stroke = 1.5) +
  geom_point(aes(y=CO2.water.ppm, color='CO2'),size = 1, stroke = 1.5) +
  facet_wrap(~Site, scales='free')+
  ylab("ppm")+
  scale_y_log10()+
  geom_hline(yintercept = 400, color='cyan', size=1)+
  geom_hline(yintercept = 3, color='hotpink', size=1)



um2.s<-expression(q[L]~um^2/s)
library(ggnewscale)
common_layer<-list(
  geom_point(aes(color = qL_m2.sec*(10^6))),
    scale_color_gradient(low = "blue", high = "red",
                         name = expression(q[L] ~ um^2/s)),
    new_scale_color(),
    geom_smooth(aes(color = Well, group = Well), method = "lm", se = FALSE, size = 1),
    facet_wrap(~ID, scales='free'),
    theme(legend.position = 'none'),
    xlab('Watertable Elevation (m)'))

RC_slopes <- read_csv("04_Output/RC_slopes.csv")%>%
  mutate(ID=as.factor(ID), Well=as.factor(Well))

rgression_dims<-RC_dims %>% separate(Site, into = c("ID", "Well"), sep = "GW")

regression_edited<-left_join(RC_slopes, rgression_dims, by=c("ID", "Well"))%>%
  mutate(Well=as.character(Well))%>%
  mutate(Well=if_else(Well=='0','Stream', Well),
         Well=if_else(Well=='8' & ID=='5', 'Upland', Well),
         Well=if_else(Well=='5' & ID=='9', 'Upland', Well),
         Well=if_else(Well=='5' & ID=='6', 'Upland', Well))%>%
  distinct(ID, Well,type, .keep_all = T)

ytitle_DOC<-expression('DOC flux/ Water Table Elevation'~mg~m^-3~s^-1)
ytitle_DIC<-expression('DIC flux/ Water Table Elevation'~mg~m^-3~s^-1)

ytitle_DOC<-expression('DOC'~('mg'~m^-2~s^-1))
ytitle_DIC<-expression('DIC'~('mg'~m^-2~s^-1))
r2title<-expression(r^2 > 0.4)
xtitle<-'Distance (m)'

common_layers <- list(
  geom_point(size = 5),
  xlab(xtitle),
  ylab("Slope (C flux~ WTE)"),
  facet_wrap(~ID, scales = 'free'),
  labs(color = " "),
  theme( legend.position = 'bottom',
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 12),
         legend.key.size = unit(1, "cm"),
         strip.text = element_text(size = 12),
         axis.title.y = element_text(size=13, angle=90),
         axis.title.x = element_text(size=13),
         axis.text.x = element_text(size=12),
         axis.text.y = element_text(size=12)),
  stat_poly_line(se = FALSE),
  stat_poly_eq(
    aes(label = paste(..p.value.label.., sep = "~~~~~"), color=type),
    formula = y ~ x,
    parse = TRUE,
    label.x.npc = "right", label.y.npc = "top", size =5),
  geom_hline(yintercept = 0, color='darkred', linetype='dashed'))



ggsave(filename="05_Figures/Ch2_H2_regression.jpeg",
       plot =
        plot_grid(

          ggplot(regression_edited %>%filter(Well!="stream",  type %in% c("DOC", "DIC")),
                 aes(x = distance.from.stream, y = slope, color=type))+
            common_layers,

          ggplot(regression_edited %>%filter(Well!="stream",  type %in% c("CO2", "CH4")),
                 aes(x = distance.from.stream, y = slope, color=type))+
            common_layers,
          ncol=1

        ),
       width =11,height = 8, units = "in")


