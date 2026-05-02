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


#dropped Q                ################
shape_key <- c('full' = 16, 'lint' = 17, 'lext' = 15, 'both' = 18)

common_layers <- list(
  geom_point(size = 4),
  scale_color_viridis_c(),
  scale_shape_manual(values = c('full' = 16, 'lint' = 17, 'lext' = 15, 'both' = 18)),
  theme_minimal()
)

a<-dropQ %>%
  filter(dropped_from %in% c('lint', 'full'),
         pathway == 'lint',
         indep=='TempC') %>%
  ggplot(aes(x = as.factor(site), y = Estimate, color=sigma, shape=dropped_from)) +
  common_layers +
  ggtitle("Discharge Dropped: Internal")

b<-dropQ %>%
  filter(dropped_from %in% c('lext', 'full'),
         pathway == 'lext',
         indep=='TempC') %>%
  ggplot(aes(x = as.factor(site), y = Estimate, color=sigma, shape=dropped_from)) +
  common_layers +
  ggtitle("Discharge Dropped: External")

c<-dropQ %>%
  filter(dropped_from %in% c('full', 'both'),
         pathway == 'lext',
         indep=='TempC') %>%
  ggplot(aes(x = as.factor(site), y = Estimate, color=sigma, shape=dropped_from)) +
  common_layers +
  ggtitle("Discharge Dropped: Both Pathways")

(g<-plot_grid(a,b,c, ncol=3))

full.for.q<-site_specific_results%>%
  filter(indep.var=='TempC')%>%
  rename(indep=indep.var, site=ID)%>%
  select(site, pathway, Estimate, sigma)

dropQ_wide <- dropQ %>%
  pivot_wider(
    id_cols = c(pathway, indep, site, test),
    names_from = dropped_from,
    values_from = c(Estimate, sigma)
  ) %>%
  drop_na(Estimate_lint, Estimate_lext, Estimate_both) %>%
  select(-Estimate_full, -sigma_full) %>%
  left_join(full.for.q) %>%
  mutate(
    lint.diff = Estimate_lint - Estimate,
    lint.sigma.diff = sigma_lint - sigma,
    lext.diff = Estimate_lext - Estimate,
    lext.sigma.diff = sigma_lext - sigma,
    both.diff = Estimate_both - Estimate,
    both.sigma.diff = sigma_both - sigma
  )
  


d<-dropQ_wide %>%
  pivot_longer(cols = c(lint.diff, lext.diff, both.diff),
               names_to = "dropped_from",
               values_to = "diff") %>%
  ggplot(aes(x = as.factor(site), y = diff, color = dropped_from, shape = dropped_from)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_viridis_d() +
  theme_minimal() +
  facet_wrap(~dropped_from + indep, scales = "free") +
  labs(y = "Difference in Estimate from Full Model", x = "Site")

e<-dropQ_wide %>%
  pivot_longer(cols = c(lint.sigma.diff, lext.sigma.diff, both.sigma.diff),
               names_to = "dropped_from",
               values_to = "sigma.diff") %>%
  ggplot(aes(x = as.factor(site), y = sigma.diff, color = dropped_from, shape = dropped_from)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_viridis_d() +
  theme_minimal() +
  facet_wrap(~dropped_from + indep, scales = "free") +
  labs(y = "Difference in Sigma from Full Model", x = "Site")

plot_grid(g, d, e,ncol=1)

  