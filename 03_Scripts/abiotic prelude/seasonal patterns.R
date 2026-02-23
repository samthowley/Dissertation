library(tidyverse)
library(hydroTSM)
library(patchwork)
library(gganimate)
library(gifski)
library(png)
library(lubridate)
library(corrplot)
library('StreamMetabolism')
library(weathermetrics)
library(ggpmisc)

#Call in data###########
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

CH4<- read_csv("04_Output/gas.samples.csv")%>%
  filter(type=='CH4', chapter=='stream')%>%
  rename(CH4.umol.L=water_umol.L, ID=Site)%>%
  select(Date, ID, CH4.umol.L)


mean_daily <- function(file, value_col) {
  read_csv(file) %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Date, ID) %>%
    summarise("{value_col}" := mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
}

CO2 <- mean_daily("02_Clean_data/CO2_cleaned.csv", "CO2")
DO <- mean_daily("02_Clean_data/DO_cleaned.csv", "DO")
temp <- mean_daily("02_Clean_data/temperature.csv", "Temp_PT")
Q<- mean_daily("04_Output/flow_regime_daily.csv", "Q")%>%filter(Q>1)
depth<- mean_daily("04_Output/flow_regime_daily.csv", "depth")


df_list <- list(CO2, DO, Q, depth, temp,CH4)
daily_df <- reduce(df_list, full_join, by=c('Date', 'ID'))%>%cc
  mutate(ssn=time2season(Date, out.fmt="seasons"))%>%
  filter(!ID %in% c('14', NA_real_))%>%
  mutate(
    DO.sat=Cs(fahrenheit.to.celsius(Temp_PT)),
    Temp_K=fahrenheit.to.celsius(Temp_PT)+273.15,
    exp=2400*((1/Temp_K)-(1/298.15)),
    KH=0.034*2.178^(exp),
    CO2_atm=CO2/10^6,
    CO2.mol.L=(CO2_atm*KH),
    DO.mol.L=DO/32000
    )%>%select(-Temp_K,-exp,-KH,-CO2_atm)


Q<- read_csv("02_Clean_data/discharge.csv")%>%filter(Q>1)
depth<- read_csv("02_Clean_data/depth.csv")
temperature <- read_csv("02_Clean_data/temperature.csv")
DO <- read_csv("02_Clean_data/DO_cleaned.csv")
CO2<-read_csv("02_Clean_data/CO2_cleaned.csv")

df_list <- list(CO2, DO, Q, depth,CH4)
hourly_df <- reduce(df_list, full_join, by=c('Date', 'ID'))%>%
  mutate(hourly="hourly cloud")%>%filter(!ID %in% c('14', NA_real_))%>%
  mutate(
    DO.sat=Cs(fahrenheit.to.celsius(Temp_PT)),
    Temp_K=fahrenheit.to.celsius(Temp_PT)+273.15,
    exp=2400*((1/Temp_K)-(1/298.15)),
    KH=0.034*2.178^(exp),
    CO2_atm=CO2/10^6,
    CO2.mol.L=(CO2_atm*KH),
    DO.mol.L=DO/32000,
    ssn=time2season(Date, out.fmt="seasons")
  )%>%select(-Temp_K,-exp,-KH,-CO2_atm)


Q.label<-("Discharge (L/sec)")
CO2ppm.label<-expression(CO[2]~ppm)

CH4umol.label<-expression(CH[4]~μmol/L)
CO2umol.label<-expression(CO[2]~μmol/L)
CO2mol.label<-expression(CO[2]~mol/L)

DO.label<-'DO mg/L'
O2mol.label<-expression(O[2]~mol/L)
O2umol.label<-expression(O[2]~μmol/L)

#correlation plots############

independent.var<-daily_df%>% select(Q, depth, Temp_PT, month, CO2, DO)
M <- cor(independent.var, use = "pairwise.complete.obs")
corrplot(M, method = "circle", type = "lower", order = "AOE", diag = FALSE,
         addCoef.col = "black", number.cex = 0.8)

#Temperature#######
common.layers.temp.trends<-list(
  geom_point(),
  stat_poly_line(formula = y ~ x, se = FALSE),
  stat_poly_eq(aes(x = Temp_PT, y = CO2.mol.L,
                   label = paste(..p.value.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE,
                 size = 4),
  facet_wrap(~ID, scales='free'),
  theme(
    axis.title.y = element_text(size = 17, angle = 90),
    axis.title.x = element_text(size = 17)
  )
)

ggplot(data = daily_df, aes(x = Temp_PT, y = CO2.mol.L*10^6)) +
  geom_point(data = hourly_df, aes(x = Temp_PT, y = CO2.mol.L*10^6), color = 'gray') +
  common.layers.temp.trends+
  ylab(CO2umol.label)+xlab("Temperature (F)")

ggplot(data = daily_df, aes(x = Temp_PT, y = DO.mol.L*10^6)) +
  geom_point(data = hourly_df, aes(x = Temp_PT, y = DO.mol.L), color = 'gray') +
  common.layers.temp.trends+
  ylab(O2umol.label)+xlab("Temperature (F)")

ggplot(data = daily_df%>%filter(CH4.umol.L>0.1), aes(x = Temp_PT, y = CH4.umol.L)) +
  common.layers.temp.trends+
  ylab(CH4umol.label)+xlab("Temperature (F)")

#discharge#############

daily_df<-daily_df%>% group_by(ID) %>%
  filter(!is.na(Q))%>%
  mutate(Q_med=  case_when(Q>= median(Q, na.rm = T)~ "sup",
                           Q<=median(Q, na.rm = T)~"inf"))%>%
  mutate(Q_ID= paste0(ID, sep="_", Q_med))


cols <- c('DO.mol.L', 'CO2.mol.L', 'Q', 'ID','Q_ID')
unique_sites <- unique(daily_df$Q_ID[!is.na(daily_df$Q_ID)])

streams <- setNames(
  lapply(unique_sites, function(site_id) {
    df_subset <- daily_df %>%
      filter(Q_ID == site_id) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites)

streams_edited <- lapply(streams, function(df) {

  df <- df %>%
    filter(Q > 0, DO.mol.L > 0, CO2.mol.L>0) %>%
    filter(!is.na(Q_ID),!is.na(Q), !is.na(DO.mol.L), !is.na(CO2.mol.L))


  (DO.Q<-summary(lm(log10(DO.mol.L) ~ log10(Q), data = df)))
  Slope.DO <- DO.Q$coefficients[2,1]
  pvalue.DO <- DO.Q$coefficients[2, 4]

  (CO2.Q<-summary(lm(log10(CO2.mol.L) ~ log10(Q), data = df)))
  Slope.CO2 <- CO2.Q$coefficients[2,1]
  pvalue.CO2 <- CO2.Q$coefficients[2, 4]

  df<-df%>%
    mutate(
      Slope.DO=as.numeric(c(Slope.DO)),
      Slope.CO2=as.numeric(c(Slope.CO2)),

      pvalue.DO=as.numeric(c(pvalue.DO)),
      pvalue.CO2=as.numeric(c(pvalue.CO2))
    )%>%
    summarize(

      Slope.DO=mean(Slope.DO, na.rm=T),
      Slope.CO2=mean(Slope.CO2, na.rm=T),

      pvalue.DO=mean(pvalue.DO, na.rm=T),
      pvalue.CO2=mean(pvalue.CO2, na.rm=T)
    )
})

slopes <- bind_rows(streams_edited, .id = "ID")%>%
  separate(ID, into = c("Q_ID", "Q_med"), sep = "_")


common.layers.Q.trends<-list(
  geom_point(),
  stat_poly_line(formula = y ~ x, se = FALSE),
  stat_poly_eq(aes(group=Q_med,
    label = paste(..p.value.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               size = 4),
  facet_wrap(~ID, scales='free'),
  theme(
    axis.title.y = element_text(size = 17, angle = 90),
    axis.title.x = element_text(size = 17) ),
  scale_x_log10(), scale_y_log10()
)

ggplot(data = daily_df, aes(x = Q, y = CO2.mol.L*10^6, group=Q_med)) +
  common.layers.Q.trends+
  ylab(CO2umol.label)+xlab(Q.label)

ggplot(data = daily_df, aes(x = Q, y = DO.mol.L*10^6, group=Q_med)) +
  common.layers.Q.trends+
  ylab(O2umol.label)+xlab(Q.label)

ggplot(data = daily_df%>%filter(CH4.umol.L>0.1), aes(x = Q, y = CH4.umol.L)) +
  geom_point()+
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")),
             formula = y ~ x, parse = TRUE,
             size = 4)+
  facet_wrap(~ID, scales='free')+
  theme(
    axis.title.y = element_text(size = 17, angle = 90),
    axis.title.x = element_text(size = 17) )+
  scale_x_log10()+ scale_y_log10()



#O2:CO2 relationship###########
vachon<-hourly_df%>%mutate(day=as.Date(Date))%>%filter(!is.na(DO.mol.L), !is.na(CO2.mol.L))

ggplot(vachon, aes(x=CO2.mol.L*10^6, y=DO.mol.L*10^6, color=ssn, group=day)) +
  geom_point(color='gray')+
  geom_abline(slope = -1, intercept = 0, color = "black", linetype = "dashed")+
  geom_smooth(method='lm', se=F)+
  ylab(O2umol.label)+xlab(CO2umol.label)+
  xlim(-50, 1000)+
  facet_wrap(~ID)

mol.flux_lm <- function(df) {
  if(nrow(df) < 10) return(NULL)  # skip if not enough data for a regression

  flux.lm <- lm(CO2.mol.L ~ DO.mol.L, data = df)
  cf <- coef(flux.lm)

  tibble(
    ID = df$ID[1],
    day = df$day[1],
    flux_slope = cf[2],
    flux_intercept = cf[1]
  )
}

mol.ellipse.lm <- vachon %>%
  group_by(ID, day) %>%
  group_split() %>%
  map_dfr(flux_lm)%>%
  mutate(ssn=time2season(day, out.fmt="seasons"))

ggplot(mol.ellipse.lm, aes(x=ssn, y=flux_slope, fill=ssn)) +
  geom_boxplot(outliers = F)+
  #ylab(expression(O[2]:CO[2]))+xlab(" ")+
  facet_wrap(~ID, scales='free')

fluxes <- read_csv("04_Output/fluxes.csv")

ggplot(fluxes, aes(x=CO2_flux, y=O2_flux, color=ssn, group=date)) +
  geom_point(color='gray')+
  geom_abline(slope = -1, intercept = 0, color = "black", linetype = "dashed")+
  geom_smooth(method='lm', se=F)+
  #ylab(O2umol.label)+xlab(CO2umol.label)+
  #xlim(-50, 1000)+
  facet_wrap(~ID)

#Bank######
o2.temp<-daily_df%>%select(ID, Temp_PT, DO.mol.L)%>%rename(conc=DO.mol.L)%>%mutate(cat='o2')
co2.temp<-daily_df%>%select(ID, Temp_PT, CO2.mol.L)%>%rename(conc=CO2.mol.L)%>%mutate(cat='co2')%>%
  mutate(conc=conc*10^3) #mmol
ch4.temp<-daily_df%>%select(ID, Temp_PT, CH4.umol.L)%>%rename(conc=CH4.umol.L)%>%mutate(cat='ch4')
temp.trends<-rbind(co2.temp, o2.temp, ch4.temp)%>%filter(conc>0)


ggplot(data = temp.trends, aes(x = Temp_PT, y = conc, color=cat, group=cat)) +
  geom_point()+
  stat_poly_line(formula = y ~ x, se = FALSE) +
  stat_poly_eq(aes(x = Temp_PT, y = conc, group = cat,
                   label = paste(..p.value.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               size = 4,
               vstep=0.1, hjust = 0
  )+
  ylab("Metabolic Gas Concentrations")+xlab("Temperature (F)")+
  scale_color_discrete(labels = c(CH4umol.label, CO2mmol.label, O2mol.label))+
  theme(legend.position = "bottom")+
  facet_wrap(~ID, scales='free')

