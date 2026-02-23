library(tidyverse)
library(writexl)
library(readxl)
library(cowplot)
library(plotly)
library(ggpmisc)
library(lme4)

file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,4,6,7,8,11,9)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})
chem <- reduce(data, full_join, by = c("ID", 'Date'))

chem_daily<-chem%>% mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  mutate(
    DO=mean(DO, na.rm=T),
    CO2=mean(CO2, na.rm=T),
    Q=mean(Q, na.rm=T),
    SpC=mean(SpC, na.rm=T),
    depth=mean(depth, na.rm=T))%>% select(-Temp_PT.x, -Temp_PT.y)%>%
  distinct(Date, ID, .keep_all=T)


ggplot(chem_daily %>% filter(!ID %in% c('14', NA), Q>0.1, DO> 0.2),
       aes(x=Q, y=DO,color=ID)) +
  scale_y_log10()+scale_x_log10()+
  geom_point()+theme(axis.title.x=element_blank())+ facet_wrap(~ID, scales='free')+
  stat_poly_line(formula = y ~ x, se = FALSE) +
  stat_poly_eq(
    aes(x = Q, y = DO,
        label = paste(..p.value.label..,..eq.label.. , sep = "~~~")),
    formula = y ~ x, parse = TRUE,
    size = 4,
    label.x.npc = "right",label.y.npc = 0.017, vstep=0.07)

chem_daily%>% group_by(ID)%>%
  summarise(
    meanCO2=mean(CO2, na.rm = T),
    meanDO=mean(DO, na.rm=T),
    medianCO2=median(CO2, na.rm = T),
    medianDO=median(DO, na.rm = T)
  )

summary(lmList(CO2 ~ Q | ID, data=chem_daily))
summary(lmList(DO ~ Q | ID, data=chem_daily))

#metabolism#####

metabolism <- read_csv("04_Output/master_metabolism.csv")%>%rename(Date=date)
discharge <- read_csv("02_Clean_data/discharge.csv")
metabolism<-left_join(metabolism, discharge)

met_hist.GPP<-metabolism%>%select(Date, ID, GPP, Q)%>%
  rename(met=GPP)%>%mutate(type='GPP')

met_hist.ER<-metabolism%>%select(Date, ID, ER, Q)%>%
  rename(met=ER)%>%mutate(type='ER', met=met*-1)

met_hist.NEP<-metabolism%>%mutate(NEP=GPP-ER) %>%
  select(Date, ID, NEP, Q)%>%
  rename(met=NEP)%>%mutate(type='NEP')

met_hist<-rbind(met_hist.GPP, met_hist.ER, met_hist.NEP)%>% filter(!ID %in% c('14', '6a'))


ggplot(met_hist %>% filter(!ID %in% c('14', NA), Q>0.1, type!='GPP'),
       aes(x=Q, y=met,color=type)) +
  scale_y_log10()+scale_x_log10()+
  geom_point()+theme(axis.title.x=element_blank())+ facet_wrap(~ID, scales='free')+
  stat_poly_line(formula = y ~ x, se = FALSE) +
  stat_poly_eq(
    aes(x = Q, y = met, group=type, color=type,
        label = paste(..p.value.label.., sep = "~~~")),
    formula = y ~ x, parse = TRUE,
    size = 4,
    label.x.npc = "right",label.y.npc = 0.017, hstep=0.15)

ggplot(met_hist%>% filter(!is.na(ID)),
           aes(x = reorder(ID_q, -as.numeric(wetland_perc)),  y = met, fill = type)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c('brown','darkgreen'), name= "") +
    ggtitle("Metabolic Regime") +
    ylab(expression(O[2]~'g' / m^2 / 'day')) +
    common_theme+
    scale_x_discrete(labels = labels_vec)

#Figures##########

ordered_ids <- chem_daily %>%
  filter(!ID %in% c('14', NA)) %>%
  arrange(Q) %>%
  pull(ID) %>%
  unique()

chem_daily <- chem_daily %>%
  mutate(ID = factor(ID, levels = ordered_ids))

master %>%
  group_by(ID)%>%
  summarize(pH=mean(pH, na.rm=T),
            SpC=mean(SpC, na.rm=T),
            DO=mean(DO, na.rm=T),
            CO2=mean(CO2, na.rm=T))

pQ<-ggplot(chem_daily %>% filter(!ID %in% c('14', NA), Q>1),aes(x=as.factor(ID), y=Q)) +
  xlab('Stream ID')+ylab(expression('Discharge'~m^3~s^-1))+
  scale_y_log10()+ggtitle('Flatwood Stream Chemical Signature')+
  geom_boxplot()+theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
                       axis.title.y=element_text(size=15), plot.title=element_text(size=20))

pDO<-ggplot(chem_daily %>% filter(!ID %in% c('14', NA)),aes(x=as.factor(ID), y=DO)) +
  xlab('Stream ID')+ylab('DO mg/L')+
  geom_boxplot()+theme(axis.title.x =element_blank(),axis.text.x =element_blank())

ppH<-ggplot(chem_daily %>% filter(!ID %in% c('14', NA)),aes(x=as.factor(ID), y=pH)) +
  xlab('Stream ID')+ylab('pH')+
  geom_boxplot()+theme(axis.title.x=element_blank(),axis.text.x =element_blank())

pCO2<-ggplot(chem_daily %>% filter(!ID %in% c('14', NA), CO2>800),aes(x=as.factor(ID), y=CO2)) +
  xlab('Stream ID')+ylab(expression(CO[2]~ppm))+scale_y_log10()+
  geom_boxplot()+theme(axis.title.x=element_blank(),axis.text.x =element_blank())

pSpC<-ggplot(chem_daily %>% filter(!ID %in% c('14', NA)),aes(x=as.factor(ID), y=SpC)) +
  xlab('Stream ID')+
  geom_boxplot()+theme(axis.title.x =element_blank())

plot_grid(pQ,pDO,ppH,pCO2,pSpC, ncol=1, align = 'v')

ggsave(filename = "test.jpeg",
       plot = plot_grid(pQ,pDO,ppH,pCO2,pSpC, ncol=1, align = 'v'),
       width = 8, height = 10, units = "in")





