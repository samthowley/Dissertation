source("03_Scripts/Streams/analysis/data for analysis.R")

#calculate inundation#############
contrib_wetlands <- read_csv("01_Raw_data/wetland cover/contrib_wetlands.csv")%>%
  rename(contrib.wetland.area=Wetland.area, contrib.wetland.perc=PERCENTAGE, Basin=ID)

wetland_cover <- read_csv("01_Raw_data/wetland cover/wetland_cover.csv")%>%
  select(Basin_Name, AREA, PERCENTAGE)%>%
  rename(Basin='Basin_Name', total.wetland.area=AREA, total.wetland.perc=PERCENTAGE)%>%
  left_join(contrib_wetlands)

wetland_stage <- read_csv("01_Raw_data/wetland cover/wetland stage.csv")%>%
  separate(well_id, "_", into=c("Basin", "wetland"))%>%
  filter(flag==0)%>%
  group_by(Basin, date)%>%
  mutate(
    well.depth.m=mean(well_depth_m, na.rm=T),
  )%>%
  select(date, Basin, well.depth.m)


watershed.innundation<-left_join(wetland_stage, wetland_cover)%>%
  mutate(
    basin.area=total.wetland.area/(total.wetland.perc/100),
    total.basin.inundation=total.wetland.perc*well.depth.m,
    contrib.basin.inundation=(contrib.wetland.area/basin.area)*well.depth.m,
    Date=mdy(date),
    year=year(Date)
    )%>%
  filter(!Basin %in% c('14', '14.9', 'dry', 'wet'), 
         year %in% c('2023', '2024', '2025', '2026'))%>%
  select(-year, -date)%>%
  distinct(Date, Basin, .keep_all = T)

inundation<-left_join(int.ext%>%mutate(Date=as.Date(Date)), 
                      watershed.innundation%>%select(Date, Basin, total.basin.inundation, contrib.basin.inundation), 
                      by=c('Date', 'Basin'))#%>%

write_csv(watershed.innundation, "01_Raw_data/wetland cover/watershed.inundation.csv")
#total wetland cover####################
wetland_cover <- read_csv("01_Raw_data/wetland cover/wetland_cover.csv")%>%
  select(Basin_Name, AREA, PERCENTAGE)%>%
  rename(Basin='Basin_Name', total.wetland.area=AREA, total.wetland.perc=PERCENTAGE)%>%
  mutate(total.wetland.perc=round(total.wetland.perc, 2))

total.wetland.impact<-int.ext%>%
  select(ID, Date, int.ext.ratio, Basin)%>%
  left_join(wetland_cover)


labs_df <- total.wetland.impact %>%
  distinct(ID, total.wetland.perc) %>%
  arrange(total.wetland.perc)

id_levels <- labs_df$ID
x_labs <- setNames(paste0(labs_df$ID, "\n", labs_df$total.wetland.perc), labs_df$ID)

# 2) Means (for stars + trend line)
means_df <- total.wetland.impact %>%
  mutate(
    total.wetland.perc = as.numeric(total.wetland.perc),
    ID = factor(ID, levels = id_levels)
  ) %>%
  group_by(ID) %>%
  summarise(
    mean_ratio = mean(int.ext.ratio, na.rm = TRUE),
    total.wetland.perc = first(total.wetland.perc),
    .groups = "drop"
  )

for.lm<-inundation%>%
  group_by(ID, Basin)%>%
  summarise(mean.ratio=mean(int.ext.ratio, na.rm=T))%>%
  left_join(wetland_cover)
summary(lm(mean.ratio ~ total.wetland.perc, data = for.lm))

model <- lm(mean.ratio ~ total.wetland.perc, data = for.lm)
p_val <- summary(model)$coefficients["total.wetland.perc", "Pr(>|t|)"]
p_label <- paste0("p = ", signif(p_val, 3))

# 3) Plot
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
plot_grid(
  inundation%>%filter(ID%in% c('3','9'))%>%
ggplot(aes(x = total.basin.inundation, y = external)) +
  xlab(expression('Watershed Inundation'~'(Basin Wetland Percent*Mean Watertable Depth)'))+
  ggtitle(expression('External'~'Pathway'~'Responses'~'to'~'Watershed'~'Inundation'))+
  common.layers
 
,
inundation%>%filter(ID%in% c('3','9'))%>%
ggplot(aes(x = total.basin.inundation, y = internal)) +
  xlab(expression('Watershed Inundation'~'(Basin Wetland Percent*Mean Watertable Depth)'))+
  ggtitle(expression('Internal'~'Pathway'~'Responses'~'to'~'Watershed'~'Inundation'))+
  common.layers,

inundation%>%filter(ID%in% c('3','9'))%>%
  ggplot(aes(x = total.basin.inundation, y = int.ext.ratio)) +
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

slopes<-rbind(
site_lm_table_fun(inundation, log10(internal), ID, contrib.basin.inundation) %>%
  mutate(pathway = "Internal") %>%
  rename(slope = slope, p = p_slope)
,
site_lm_table_fun(inundation, log10(external), ID, contrib.basin.inundation) %>%
  mutate(pathway = "External") %>%
  rename(slope = slope, p = p_slope)
,
site_lm_table_fun(inundation, log10(int.ext.ratio), ID, contrib.basin.inundation) %>%
  mutate(pathway = "Ratio") %>%
  rename(slope = slope, p = p_slope)
)%>%
  mutate(
    Basin=case_when(ID=='5'~'5',ID=='5a'~'5',ID=='15'~'15',
                    ID=='3'~'6',ID=='7'~'7',ID=='6'~'6',ID=='6a'~'6',
                    ID=='9'~'9', ID=='13'~'13')
  )%>%
  left_join(wetland_cover)%>%
  mutate(
    significance=if_else(p<=0.005, "significant", "insignificant")
  )



slopes%>%
  ggplot(aes(
    x=contrib.wetland.perc,
    y = slope,
    color=pathway)) +
  geom_point(aes(shape=significance), size=5)+
  labs(fill='Pathway', shape="p-value", color="Sites")+
  geom_hline(yintercept = 0, linetype="dashed", color='green', size=3)+
  ggtitle("Pathway Response to Watershed Inundation")+
  xlab("Contributing Area Wetland Percent")+
  #geom_smooth(method=lm, se=F)+
  theme(
    axis.title.x = element_text(size=17, color='black'),
    axis.text.x = element_text(size=17, color='black')
  )+theme_minimal()


slopes%>%
  ggplot(aes(
    x=total.wetland.perc,
    y = slope,
    color=pathway)) +
  geom_point(aes(shape=significance), size=5)+
  labs(fill='Pathway', shape="p-value", color="Sites")+
  geom_hline(yintercept = 0, linetype="dashed", color='black', size=3)+
  ggtitle("Pathway Response to Watershed Inundation")+
  xlab("Basin Wetland Percent")+
  theme(
    axis.title.x = element_text(size=17, color='black'),
    axis.text.x = element_text(size=17, color='black'))+
  theme_minimal()


