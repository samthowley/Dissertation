source("03_Scripts/Streams/analysis/data for analysis.R")

<<<<<<< HEAD
wetland_cover <- watershed.inundation%>%
  select(ID, total.wetland.area, basin.wetland.perc,
         contrib.wetland.area, contrib.wetland.perc)%>%
  distinct(ID, total.wetland.area, .keep_all = T)%>%
  mutate(basin.wetland.perc=round(basin.wetland.perc, 3),
         contrib.wetland.perc=round(contrib.wetland.perc, 3))
=======
<<<<<<< HEAD
#calculate inundation#############
contrib_wetlands <- read_csv("01_Raw_data/wetland cover/contrib_wetlands.csv")%>%
  rename(contrib.wetland.area=Wetland.area, contrib.wetland.perc=PERCENTAGE)

wetland_cover <- read_csv("01_Raw_data/wetland cover/wetland_cover.csv")%>%
  select(Basin_Name, AREA, PERCENTAGE)%>%
  rename(Basin='Basin_Name', total.wetland.area=AREA, total.wetland.perc=PERCENTAGE)
=======

wetland_cover <- watershed.inundation%>%
  select(ID, total.wetland.area, basin.wetland.perc,
         contrib.wetland.area, contrib.wetland.perc)%>%
  mutate(basin.wetland.perc=round(basin.wetland.perc, 2),
         contrib.wetland.perc=round(contrib.wetland.perc, 2)
  )
>>>>>>> a8500a1ec5520abcdef8a9d032c8701d8b6d3db7
>>>>>>> 14f919177a3d39685107c32796c1b1f85accbbe6

wetland.impact<-int.ext%>%
  select(ID, Date, int.ext.ratio, internal, external)%>%
  left_join(wetland_cover)%>%
  distinct(ID, Date,.keep_all = T)

#total wetland cover####################
names(wetland.impact)
total.wetland.impact<-wetland.impact%>%select(-contrib.wetland.area, -contrib.wetland.perc)

labs_df <- total.wetland.impact %>%
  distinct(ID, basin.wetland.perc) %>%
  arrange(basin.wetland.perc)

id_levels <- labs_df$ID
x_labs <- setNames(paste0(labs_df$ID, "\n", labs_df$basin.wetland.perc), labs_df$ID)

# 2) Means (for stars + trend line)
means_df <- total.wetland.impact %>%
  mutate(
    basin.wetland.perc = as.numeric(basin.wetland.perc),
    ID = factor(ID, levels = id_levels)
  ) %>%
  group_by(ID) %>%
  summarise(
    mean_ratio = mean(int.ext.ratio, na.rm = TRUE),
    basin.wetland.perc = first(basin.wetland.perc),
    mean_internal=mean(internal, na.rm=T),
    mean_external=mean(external, na.rm=T),
    .groups = "drop"
  )



for.lm<-total.wetland.impact%>%
  group_by(ID, basin.wetland.perc)%>%
  summarise(mean.ratio=mean(int.ext.ratio, na.rm=T))

summary(lm(mean.ratio ~ basin.wetland.perc, data = for.lm))

model <- lm(mean.ratio ~ basin.wetland.perc, data = for.lm)
p_val <- summary(model)$coefficients["basin.wetland.perc", "Pr(>|t|)"]
p_label <- paste0("p = ", signif(p_val, 3))

total.wetland.impact %>%
  mutate(
    ratio = int.ext.ratio,
    ID = factor(ID, levels = id_levels)
  ) %>%
  ggplot(aes(x = ID, y = ratio)) +
  geom_violin(size = 1) +
  geom_jitter(shape = 1, color = "gray", width = 0.15, alpha = 0.6) +
  geom_point(
    data = means_df,
    aes(y = mean_ratio),
    color = "red",
    shape = 8,
    size = 3
  ) +
  geom_hline(yintercept = 1, color = 'black') +
  annotate("text", x = Inf, y = Inf, label = p_label,
           hjust = 1.1, vjust = 1.5, size = 4) +
  scale_x_discrete(labels = x_labs) +
  scale_y_log10() +
  ylab("Average Internal / External") +
  xlab("Stream Site\nWetland cover (%)") +
  ggtitle("Internal:External Among Basins with Differing Wetland Cover")



for.lm<-total.wetland.impact%>%
  group_by(ID, basin.wetland.perc)%>%
  summarise(mean.int=mean(internal, na.rm=T))

summary(lm(mean.int ~ basin.wetland.perc, data = for.lm))

model <- lm(mean.int ~ basin.wetland.perc, data = for.lm)
p_val <- summary(model)$coefficients["basin.wetland.perc", "Pr(>|t|)"]
p_label <- paste0("p = ", signif(p_val, 3))


total.wetland.impact %>%
  mutate(
    ID = factor(ID, levels = id_levels)
  ) %>%
  ggplot(aes(x = ID, y = internal)) +
  geom_violin(size = 1) +
  geom_jitter(shape = 1, color = "gray", width = 0.15, alpha = 0.6) +
  geom_point(
    data = means_df,
    aes(y = mean_internal),
    color = "red",
    shape = 8,
    size = 3
  ) +  annotate("text", x = Inf, y = Inf, label = p_label,
                hjust = 1.1, vjust = 1.5, size = 4) +
  scale_x_discrete(labels = x_labs) +
  scale_y_log10() +
  xlab("Stream Site\nWetland cover (%)")


for.lm<-total.wetland.impact%>%
  group_by(ID, basin.wetland.perc)%>%
  summarise(mean.int=mean(external, na.rm=T))

summary(lm(mean.int ~ basin.wetland.perc, data = for.lm))

model <- lm(mean.int ~ basin.wetland.perc, data = for.lm)
p_val <- summary(model)$coefficients["basin.wetland.perc", "Pr(>|t|)"]
p_label <- paste0("p = ", signif(p_val, 3))

total.wetland.impact %>%
  mutate(
    ID = factor(ID, levels = id_levels)
  ) %>%
  ggplot(aes(x = ID, y = external)) +
  geom_violin(size = 1) +
  geom_jitter(shape = 1, color = "gray", width = 0.15, alpha = 0.6) +
  geom_point(
    data = means_df,
    aes(y = mean_external),
    color = "red",
    shape = 8,
    size = 3
  ) +  annotate("text", x = Inf, y = Inf, label = p_label,
                hjust = 1.1, vjust = 1.5, size = 4) +
  scale_x_discrete(labels = x_labs) +
  scale_y_log10() +
  xlab("Stream Site\nWetland cover (%)")
  

#contributing wetlands##########

contrib.wetland.impact<-int.ext%>%
  select(ID, Date, int.ext.ratio, Basin)%>%
  left_join(contrib_wetlands)%>%
  mutate(contrib.wetland.perc=round(contrib.wetland.perc, 2))


labs_df <- contrib.wetland.impact %>%
  distinct(ID, contrib.wetland.perc) %>%
  arrange(contrib.wetland.perc)

id_levels <- labs_df$ID
x_labs <- setNames(paste0(labs_df$ID, "\n", labs_df$contrib.wetland.perc), labs_df$ID)

# 2) Means (for stars + trend line)
means_df <- contrib.wetland.impact %>%
  mutate(
    ratio = int.ext.ratio,
    contrib.wetland.perc = as.numeric(contrib.wetland.perc),
    ID = factor(ID, levels = id_levels)
  ) %>%
  group_by(ID) %>%
  summarise(
    mean_ratio = mean(ratio, na.rm = TRUE),
    contrib.wetland.perc = first(contrib.wetland.perc),
    .groups = "drop"
  )



for.lm<-inundation%>%
  group_by(ID, Basin)%>%
  summarise(mean.ratio=mean(int.ext.ratio, na.rm=T))%>%
  left_join(contrib_wetlands)%>%
  fill(contrib.wetland.perc, .direction = 'down')

summary(lm(mean.ratio ~ contrib.wetland.perc, data = for.lm))


model <- lm(log10(mean.ratio) ~ contrib.wetland.perc, data = for.lm)
p_val <- summary(model)$coefficients["contrib.wetland.perc", "Pr(>|t|)"]
p_label <- paste0("p = ", signif(p_val, 3))






# 3) Plot
contrib.wetland.impact %>%
  mutate(
    ratio = int.ext.ratio,
    ID = factor(ID, levels = id_levels)
  ) %>%
  ggplot(aes(x = ID, y = ratio)) +
  geom_violin(size = 1) +
  geom_jitter(shape = 1, color = "gray", width = 0.15, alpha = 0.6) +
  geom_point(
    data = means_df,
    aes(y = mean_ratio),
    color = "red",
    shape = 8,
    size = 3
  ) +
  geom_hline(yintercept = 1, color = 'black') +
  annotate("text", x = Inf, y = Inf, label = p_label,
           hjust = 1.1, vjust = 1.5, size = 4) +
  scale_x_discrete(labels = x_labs) +
  scale_y_log10() +
  ylab("Average Internal / External") +
  xlab("Stream Site\nContributing Wetland cover (%)") +
  ggtitle("Internal:External Among Basins with Differing Wetland Cover")


#scatter plots##########

watershed_inundation.df<-left_join(int.ext, watershed.inundation)

common.layers<-list(  
  geom_point(color='black'),
                  ylab(expression(CO[2]~'g'/m^2/'day')),
                  scale_y_log10(),
                stat_poly_line(formula = y ~ x, se = FALSE),
                  stat_poly_eq(
                    aes(label = paste(..p.value.label..,  sep = " ~~ ")),
                    formula = log10(y) ~ x, parse = TRUE,
                    size = 5, label.x = "left", label.y = "top", vstep = 0.1
                  ),
                facet_wrap(~ID, scales='free')
                
)

#Basin Wetland Percent#
names(watershed_inundation.df)

plot_grid(
  
  watershed_inundation.df%>%
    filter(ID%in% c('3','9'))%>%
ggplot(aes(x = total.wetland.inun, y = external)) +
  xlab(expression('Watershed Inundation'~'(Basin Wetland Percent*Mean Watertable Depth)'))+
  ggtitle(expression('External'~'Pathway'~'Responses'~'to'~'Watershed'~'Inundation'))+
  common.layers
 
,
watershed_inundation.df%>%
  filter(ID%in% c('3','9'))%>%
ggplot(aes(x = total.wetland.inun, y = internal)) +
  xlab(expression('Watershed Inundation'~'(Basin Wetland Percent*Mean Watertable Depth)'))+
  ggtitle(expression('Internal'~'Pathway'~'Responses'~'to'~'Watershed'~'Inundation'))+
  common.layers,

watershed_inundation.df%>%
  filter(ID%in% c('3','9'))%>%
  ggplot(aes(x = total.wetland.inun, y = int.ext.ratio)) +
  xlab(expression('Watershed Inundation'~'(Basin Wetland Percent*Mean Watertable Depth)'))+
  ggtitle(expression('Pathway'~'Ratio'~'Responses'~'to'~'Watershed'~'Inundation'))+
  common.layers,
ncol = 2)

#Contributing Area


plot_grid(
  inundation%>%filter(ID%in% c('3','9'))%>%
    ggplot(aes(x = contrib.basin.inundation, y = external)) +
    xlab(expression('Watershed Inundation'~'(Contributing Area Wetland Percent*Mean Watertable Depth)'))+
    ggtitle(expression('External'~'Pathway'~'Responses'~'to'~'Watershed'~'Inundation'))+
    common.layers
  
  ,
  inundation%>%filter(ID%in% c('3','9'))%>%
    ggplot(aes(x = contrib.basin.inundation, y = internal)) +
    xlab(expression('Watershed Inundation'~'(Contributing Area Wetland Percent*Mean Watertable Depth)'))+
    ggtitle(expression('Internal'~'Pathway'~'Responses'~'to'~'Watershed'~'Inundation'))+
    common.layers,
  
  inundation%>%filter(ID%in% c('3','9'))%>%
    ggplot(aes(x = contrib.basin.inundation, y = int.ext.ratio)) +
    xlab(expression('Watershed Inundation'~'(Contributing Area Wetland Percent*Mean Watertable Depth)'))+
    ggtitle(expression('Pathway'~'Ratio'~'Responses'~'to'~'Watershed'~'Inundation'))+
    common.layers,
  nrow = 2)


names(inundation)

#extract slopes###########
names(wetland_cover)
slopes<-rbind(
site_lm_table_fun(watershed_inundation.df, log10(internal), ID, total.wetland.inun) %>%
  mutate(pathway = "Internal") %>%
  rename(slope = slope, p = p_slope)
,
site_lm_table_fun(watershed_inundation.df, log10(external), ID, total.wetland.inun) %>%
  mutate(pathway = "External") %>%
  rename(slope = slope, p = p_slope)
,
site_lm_table_fun(watershed_inundation.df, log10(int.ext.ratio), ID, total.wetland.inun) %>%
  mutate(pathway = "Ratio") %>%
  rename(slope = slope, p = p_slope)
)%>%
  left_join(wetland_cover)%>%
  mutate(
    significance=if_else(p<=0.005, "significant", "insignificant")
  )

names(slopes)

common.layers<-list(
  geom_point(aes(shape=significance, color=ID), size=3),
    labs(fill='Pathway', shape="p-value", color="Sites"),
    geom_hline(yintercept = 0, linetype="dashed"),
  xlab("Contributing Area Wetland Percent"),
    #geom_smooth(method=lm, se=F)+
    theme(
      axis.title.x = element_text(size=17, color='black'),
      axis.text.x = element_text(size=17, color='black')
    )+theme_minimal()
)

names(slopes)
plot_grid(
  
slopes%>%
  filter(pathway=="Internal")%>%
  ggplot(aes(
    x=basin.wetland.perc,
    y = slope)) +
  ggtitle("Internal Pathway Response to Watershed Inundation")+
  common.layers
,
slopes%>%
  filter(pathway=="External")%>%
  ggplot(aes(
    x=basin.wetland.perc,
    y = slope)) +
  ggtitle("External Pathway Response to Watershed Inundation")+
  common.layers
,
slopes%>%
  filter(pathway=="Ratio")%>%
  ggplot(aes(
    x=basin.wetland.perc,
    y = slope)) +
  ggtitle("Pathway Ratio Response to Watershed Inundation")+
  common.layers,

int.ext%>%
  group_by(ID)%>%
  summarise(Q=mean(Q, na.rm=T))%>%
  arrange(Q)%>%
  ggplot(aes(
    x=ID,
    y = Q)) +
  ggtitle("Mean Q")+
  geom_point(aes(color=ID), size=3)+
  theme_minimal()


)




