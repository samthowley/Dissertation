#metabolism#########
source("03_Scripts/Streams/analysis/data for analysis.R")

mean(int.ext$ER_corrected, na.rm=T)


int.ext%>%
  group_by(ID)%>%
  summarise(
    ER=mean(ER_corrected, na.rm=T)*-1,
    GPP=mean(GPP, na.rm=T),
    `-NEP`=mean(NEP_corrected, na.rm=T)*-1
  )%>%
  pivot_longer(
    cols = c(`ER`, `GPP`, `-NEP`),
    names_to = "met",
    values_to = "flux"
  )%>%
  mutate(setting="Bradford Experimental Forest")%>%
  ggplot(aes(
    x=met,
    y = flux)) +
  geom_boxplot(size=1)+
  geom_jitter(aes(color=ID), size=3)+
  ylab(expression(O[2]~g/m^2/day))+
  labs(color = "Sites")+
  ggtitle(expression('Mean -NEP:'~ 6.16~O[2]~g/m^2/day))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=17, color='black')
  )




plot_grid(

  int.ext%>%
    group_by(ID)%>%
    summarise(
      CO2_flux=mean(CO2_flux, na.rm=T)*(12/44)
    )%>%
    mutate(setting="Bradford Experimental Forest")%>%
    ggplot(aes(
      x=setting,
      y = CO2_flux)) +
    geom_boxplot(size=1)+
    geom_jitter(aes(color=ID), size=3)+
    ylab(expression(C~'g'/m^2/'day'))+
    ggtitle(expression(CO[2]~'Flux Mean:'~6.48~ g/m^2/day))+
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(size=17, color='black')
    )

  ,

  int.ext%>%
    group_by(ID)%>%
    summarise(
      Q=median(Q, na.rm=T)/ 10^3
    )%>%
    mutate(setting="Bradford Experimental Forest")%>%
    ggplot(aes(
      x=setting,
      y = Q)) +
    geom_boxplot(size=1)+
    geom_jitter(aes(color=ID), size=3)+
    ylab(expression("Discharge"~m^3/s))+
    ggtitle(expression("Discharge Mean:"~0.106~m^3/sec))+
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(size=17, color='black')
    )
  ,
  nrow=1
)




int.ext%>%
  ggplot(aes(
    x=Q,
    y = K600)) +
  geom_point(size=1)+
  ylab(expression("Discharge"~m^3/s))+
  ggtitle(expression("Discharge Mean:"~0.106~m^3/sec))+
  scale_y_log10()+scale_x_log10()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=17, color='black')
  )+facet_wrap(~ID, scales='free')


int.ext%>%
  group_by(ID)%>%
  summarise(
    external=median(external, na.rm=T)
  )

int.ext%>%
  group_by(ID)%>%
  summarise(
    internal=median(internal/CO2_flux, na.rm=T)
  )

#External v Internal Violin Plots###########
mean(int.ext$internal, na.rm=T)

int.ext%>%
  group_by(ID)%>%
  summarise(
    Internal=mean(internal, na.rm=T),
    External=mean(external, na.rm=T)
  )%>%
  pivot_longer(
    cols=c(Internal, External),
    names_to = 'pathway',
    values_to='flux'
  )%>%
  mutate(setting="Bradford Experimental Forest")%>%

  ggplot(aes(
    x=setting,
    y = flux,
    fill=pathway)) +
  scale_fill_manual(values=c('black', 'darkred'))+
  labs(fill="Pathway", color="Sites")+
  geom_boxplot(size=1)+
  geom_jitter(aes(color=ID), size=3)+
  ylab(expression(C~'g'/m^2/'day'))+
  ggtitle("Internal and External Pathway Forest Means")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=17, color='black')
  )


# 1) Build an ID ordering + nice axis labels (ID on top, wetland% under)
labs_df <- int.ext %>%
  distinct(ID, wetland.perc) %>%
  mutate(wetland.perc_num = as.numeric(wetland.perc)) %>%
  arrange(wetland.perc_num)

id_levels <- labs_df$ID
x_labs <- setNames(paste0(labs_df$ID, "\n", labs_df$wetland.perc), labs_df$ID)

# 2) Means (for stars + trend line)
means_df <- int.ext %>%
  mutate(
    ratio = internal / external,
    wetland.perc_num = as.numeric(wetland.perc),
    ID = factor(ID, levels = id_levels)
  ) %>%
  group_by(ID) %>%
  summarise(
    mean_ratio = mean(ratio, na.rm = TRUE),
    wetland.perc_num = first(wetland.perc_num),
    .groups = "drop"
  )


# 3) Plot
int.ext %>%
  mutate(
    ratio = internal / external,
    ID = factor(ID, levels = id_levels)
  ) %>%
  ggplot(aes(x = ID, y = ratio)) +
  geom_violin(size=1) +
  geom_jitter(shape = 1, color = "gray", width = 0.15, alpha = 0.6) +
  # red star = mean
  geom_point(
    data = means_df,
    aes(y = mean_ratio),
    color = "red",
    shape = 8,   # star
    size = 3
  ) +
  geom_hline(yintercept = 1, color='black')+

  scale_x_discrete(labels = x_labs) +
  scale_y_log10() +
  ylab("Average Internal / External") +
  xlab("Stream Site\nWetland cover (%)")+
  ggtitle("Internal:External Among Basins with Differing Wetland Cover")

for.lm<-int.ext%>%
group_by(ID, wetland.perc)%>%
  summarise(
    mean.ratio=mean(internal / CO2_flux, na.rm=T)
  )
lm(mean.ratio ~ wetland.perc, data = for.lm)

active%>%
  group_by(ID)%>%
  summarize(active.passive.mean=mean(active.passive, na.rm=T),
            active.passive.min=min(active.passive, na.rm=T),
            active.passive.max=max(active.passive, na.rm=T),

            act_dom=sum(active.passive >1 , na.rm = TRUE),
            pass_dom=sum(active.passive <1 , na.rm = TRUE),
            tot=sum(active.passive >0 , na.rm = TRUE), #needed for the denominator

            act_days=act_dom/tot*100,
            pass__days=pass_dom/tot*100,

            act_perc.min=min(active/CO2_flux, na.rm=T),
            act_perc.max=max(active/CO2_flux, na.rm=T)
            )%>%
  select(ID, active.passive.mean, active.passive.max, active.passive.min, act_days,
         act_perc.min, act_perc.max)




#slopes##########

active<-active%>% group_by(ID) %>%
  mutate(Q_med=  case_when(Q>= median(Q, na.rm = T)~ "sup",
                           Q<=median(Q, na.rm = T)~"inf"))%>%
  mutate(Q_ID= paste0(ID, sep="_", Q_med))%>%
  filter(!is.na(Q))


active$ID <- with(active, reorder(ID, -as.numeric(as.character(wetland_perc))))

int<-active%>%
  select(ID, Q, Date, Q_med, internal)%>%
  rename(rate=internal)%>%mutate(type='internal')

ext<-active%>%
  select(ID, Q, Date, Q_med, external)%>%
  rename(rate=external)%>%mutate(type='external')
active.hist<-rbind(int, ext)

active.hist$ID <- factor(active.hist$ID, levels = c('13','15','3','5','5a','6','7','9'))


(a<-ggplot(
  active.hist,
  aes(x = Q, y = rate, color = type, group = type)) +
  geom_point(shape = 21) +
  stat_poly_line(formula = y ~ x, se = FALSE) +

    stat_poly_eq(aes(x = log10(Q), y = log10(rate), group = type, color=type,
                     label = paste(..p.value.label..,..rr.label.., sep = "~~~")),
                 formula = y ~ x, parse = TRUE,
                 size = 4,
                 label.x ='right',
                 label.y = 'bottom',
                 vstep=0.06)+


  ylab(expression('g'/m^2/'day')) +
  facet_wrap(~ID, ncol = 4, scales = 'free') +
  scale_colour_manual(name="", values = col,labels=c("External", "Internal"))+
  ggtitle(expression(CO[2]~Flux-Q~Relationship)) +
  scale_x_log10() + scale_y_log10() +
  xlab("Discharge L/s") +
  theme(
    axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    strip.text = element_text(size = 15),
    axis.text.x = element_text(size = 11)
    )+
    legend_size)

ggsave(filename = "05_Figures/external-internal.scatter.jpeg",
       a,
       width = 15, height = 7, units = "in")


 slopes <- read_csv("04_Output/external-internal_slopes.csv")

slopes$ID <- factor(slopes$ID, levels = c('15','5','5a','3','6','13','7','9'))

rel_internal_split<-slopes%>% select(ID, Q_med, active_slope, active_pvalue)%>%
  mutate(type='internal')%>% rename(slope=active_slope, pvalue=active_pvalue)

rel_external_split<-slopes%>% select(ID, Q_med, passive_slope, passive_pvalue)%>%
  mutate(type='external')%>% rename(slope=passive_slope, pvalue=passive_pvalue)

slopes.hist_split<-rbind(rel_internal_split, rel_external_split)

library(ggbreak)
library(plotly)

ggplot(slopes.hist_split %>%filter(pvalue<0.005), aes(x = ID, y=slope,color= type, shape=Q_med)) +
  geom_point(size=4, stroke=1.5) +
  scale_color_manual(values = c('black','red'),
                     labels=c("External Pathway", "Internal Pathway")) +
  scale_shape_manual(values = c(16,1),
                     name = expression(Q[50]),              # <-- Set legend title
                     labels = c("< Median Q", "> Median Q"))+  # Pick as many shapes as needed
  ylab("Rate of Change (Flux/Q)") +
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.2, linetype='dashed')+
  geom_hline(yintercept = -0.2, linetype='dashed')+
  common_theme +
  ggtitle("Log-Log Relationships of Active vs Passive")+scale_y_break(c(2, 8))

