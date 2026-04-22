source("03_Scripts/Streams/analysis/data for analysis.R")

site_specific_results <- read_csv("04_Output/stream/models/site_specific_results.csv")%>%
  rename(ID=site)

site_specific_results%>%
  ggplot(aes(x=ID, y=Estimate, color=indep.var, shape=pathway))+
  geom_point(size=4)

basin_area <- read_csv("01_Raw_data/wetland cover/basin_area.csv")%>%
  select(Basin, Shape_Area)%>%rename(ID=Basin)

pH.avg<-pH%>%
  group_by(ID)%>%summarise(pH=mean(pH, na.rm=T))

left_join(site_specific_results, pH.avg)%>%
  filter(indep.var=='TempC')%>%
  ggplot(aes(x=pH, y=Estimate, color=pathway, shape = ID))+
  geom_point(size=4)



SpC.avg<-SpC%>%
  group_by(ID)%>%summarise(SpC=mean(SpC, na.rm=T))

Q.avg<-discharge%>%
  group_by(ID)%>%summarise(Q=mean(Q, na.rm=T))%>%
  left_join(basin_area)%>%
  mutate(q=Q/Shape_Area)

left_join(site_specific_results, Q.avg)%>%
  filter(indep.var=='lQ')%>%
  ggplot(aes(x=q, y=Estimate, color=pathway, shape = ID))+
  geom_point(size=4)


left_join(site_specific_results, read_csv("01_Raw_data/wetland cover/wetland.perc.csv"))%>%
  filter(indep.var=='lQ')%>%
  ggplot(aes(x=basin.wetland.perc, y=Estimate, color=pathway, shape = ID))+
  geom_point(size=4)

wetland_perc <- read_csv("01_Raw_data/wetland cover/wetland.perc.csv")

#drop############
dropT <- read_csv("04_Output/stream/models/dropT.csv")%>%
  mutate(
    dropped_from=if_else(is.na(dropped_from), 'full', dropped_from)
  )
dropQ <- read_csv("04_Output/stream/models/dropQ.csv")%>%
  mutate(
    dropped_from=if_else(is.na(dropped_from), 'full', dropped_from)
  )

dropQ %>%
  ggplot(aes(x = as.factor(site), y = Estimate, 
             ymin = `l-95% CI`, ymax = `u-95% CI`)) +
  geom_point(data = ~ filter(., test == "full"), 
             aes(shape = indep), size = 6, alpha=0.7, color='black') +
  geom_point(data = ~ filter(., test != "full"), 
             aes(shape = indep, color = dropped_from), size = 3) +
  geom_errorbar(width = 0.2) +
  theme_minimal()

test<-dropQ%>%
  filter(dropped_from %in% c('lint', 'full', 'both'),
         pathway=='lint')%>%
  arrange(site)


dropQ %>%
  filter(dropped_from %in% c('lint', 'full', 'both'),
         pathway == 'lint',
         indep=='TempC') %>%
  ggplot(aes(x = as.factor(site), y = Estimate, color=sigma, shape=dropped_from)) +
  geom_point(size=4) +
  scale_color_viridis_c()+
  theme_minimal()+
  ggtitle("Discharge Dropped: Internal")
           

dropQ %>%
  filter(dropped_from %in% c('lext', 'full', 'both'),
         pathway == 'lext',
         indep=='TempC') %>%
  ggplot(aes(x = as.factor(site), y = Estimate, color=sigma, shape=dropped_from)) +
  geom_point(size=4) +
  scale_color_viridis_c()+
  theme_minimal()+
  ggtitle("Discharge Dropped: External")



dropT %>%
  filter(dropped_from %in% c('lint', 'full', 'both'),
         pathway == 'lint',
         indep=='lQ') %>%
  ggplot(aes(x = as.factor(site), y = Estimate, color=sigma, shape=dropped_from)) +
  geom_point(size=4) +
  scale_color_viridis_c()+
  theme_minimal()+
  ggtitle("Temp Dropped: Internal")


dropT %>%
  filter(dropped_from %in% c('lext', 'full', 'both'),
         pathway == 'lext',
         indep=='lQ') %>%
  ggplot(aes(x = as.factor(site), y = Estimate, color=sigma, shape=dropped_from)) +
  geom_point(size=4) +
  scale_color_viridis_c()+
  theme_minimal()+
  ggtitle("Temp Dropped: External")
