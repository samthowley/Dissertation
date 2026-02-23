library(tidyverse)
library(hydroTSM)
library(corrplot)
library('StreamMetabolism')
library(weathermetrics)
library(ggpmisc)
library(marelac)

#Call in data###########

calc.gas.sat <- function(df) {
  df%>%mutate(
    CO2.Sat_umol.L = gas_satconc(
      S = 0,
      t = fahrenheit.to.celsius(Temp_PT),
      P = PT / 14.504,
      species = "CO2",
      atm = atmComp("CO2")
    ),
    O2.Sat_mg.L = gas_O2sat(
      S = 0,
      t = fahrenheit.to.celsius(Temp_PT),
      masl = 0,
      method = c("Weiss")),
    O2.Sat_umol.L=O2.Sat_mg.L*(10^3/32)
  )

}
mean_daily <- function(file, value_col) {
  read_csv(file) %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Date, ID) %>%
    summarise("{value_col}" := mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
}
find_lm_relationship <- function(df, xvar, yvar, filter_expr = NULL) {
  # Optional filter expression as a string
  if (!is.null(filter_expr)) {
    df <- dplyr::filter(df, !!rlang::parse_expr(filter_expr))
  }
  # Remove rows with NA in the two variables
  df <- dplyr::filter(df, !is.na(.data[[xvar]]), !is.na(.data[[yvar]]))
  # No log transformation, just use original variables
  df <- dplyr::mutate(df, x = .data[[xvar]], y = .data[[yvar]])
  mod <- lm(y ~ x, data = df)
  s <- summary(mod)
  out <- tibble::tibble(
    xvar = xvar,
    yvar = yvar,
    intercept = s$coefficients[1, 1],
    slope = s$coefficients[2,1],
    pvalue = s$coefficients[2,4]
  )
  return(out)
}


CO2 <- mean_daily("02_Clean_data/CO2_cleaned.csv", "CO2")
DO <- mean_daily("02_Clean_data/DO_cleaned.csv", "DO")
temp <- mean_daily("02_Clean_data/temperature.csv", "Temp_PT")
Q<- mean_daily("04_Output/flow_regime_daily.csv", "Q")%>%filter(Q>1)
depth<- mean_daily("04_Output/flow_regime_daily.csv", "depth")
PSI<- mean_daily("01_Raw_data/PT/compiled_PT.csv", "PT")


df_list <- list(CO2, DO, Q, depth, temp, PSI)
daily_df <- reduce(df_list, full_join, by=c('Date', 'ID'))%>%
  mutate(ssn=time2season(Date, out.fmt="seasons"))%>%
  filter(!ID %in% c('14', NA_real_))%>%
  filter(!is.na(Q), Q>2)%>%
  mutate(
    Temp_K=fahrenheit.to.celsius(Temp_PT)+273.15,
    exp=2400*((1/Temp_K)-(1/298.15)),
    KH=0.034*2.178^(exp),
    CO2_atm=CO2/10^6,
    CO2.mol.L=(CO2_atm*KH),
    O2.mol.L=DO/32000,
    O2.umol.L= (DO/32)*10^3,
    CO2.umol.L=10^6*CO2.mol.L,
    )%>%
  calc.gas.sat()%>%
  select(-Temp_K,-exp,-KH,-CO2_atm, -PT)




Q.hr<- read_csv("02_Clean_data/discharge.csv")%>%filter(Q>1)
depth<- read_csv("02_Clean_data/depth.csv")
temperature <- read_csv("02_Clean_data/temperature.csv")
DO <- read_csv("02_Clean_data/DO_cleaned.csv")
CO2<-read_csv("02_Clean_data/CO2_cleaned.csv")

df_list <- list(CO2, DO, Q.hr, depth)
hourly_df <- reduce(df_list, full_join, by=c('Date', 'ID'))%>%
  mutate(hourly="hourly cloud")%>%
  filter(!ID %in% c('14', NA_real_))%>%
  filter(!is.na(Q), Q>2)%>%
  mutate(
    Temp_K=fahrenheit.to.celsius(Temp_PT)+273.15,
    exp=2400*((1/Temp_K)-(1/298.15)),
    KH=0.034*2.178^(exp),

    CO2_atm=CO2/10^6,
    CO2.mol.L=(CO2_atm*KH),
    O2.mol.L=DO/32000,
    O2.umol.L= (DO/32)*10^3,

    CO2.umol.L=10^6*CO2.mol.L,

    ssn=time2season(Date, out.fmt="seasons"))%>%
  calc.gas.sat()%>%
  select(names(daily_df))


Q.label<-("Discharge (L/sec)")
CO2ppm.label<-expression(CO[2]~ppm)

CO2umol.label<-expression(CO[2]~μmol/L)
CO2mol.label<-expression(CO[2]~mol/L)

DO.label<-'DO mg/L'
O2mol.label<-expression(O[2]~mol/L)
O2umol.label<-expression(O[2]~μmol/L)

#Temperature#######
split_list <- daily_df %>%
  group_by(ID) %>%
  group_split()

ids <- daily_df %>% group_by(ID) %>% group_keys() %>% pull(ID)

temp.slopes <- map2(split_list, ids, ~{
  res_DO    <- find_lm_relationship(.x, "Temp_PT", "O2.umol.L", filter_expr = NULL)
  res_CO2   <- find_lm_relationship(.x, "Temp_PT", "CO2.umol.L", filter_expr = NULL)
  res_CO2S  <- find_lm_relationship(.x, "Temp_PT", "CO2.Sat_umol.L", filter_expr = NULL)
  res_O2S   <- find_lm_relationship(.x, "Temp_PT", "O2.Sat_umol.L", filter_expr = NULL)

  # Add group ID to each result
  res_DO$ID   <- .y
  res_CO2$ID  <- .y
  res_CO2S$ID <- .y
  res_O2S$ID  <- .y

  bind_rows(res_DO, res_CO2, res_CO2S, res_O2S)
}) %>% bind_rows()%>%
  pivot_wider(
    id_cols = ID,
    names_from = yvar,
    values_from = c(slope, pvalue, intercept),
    names_glue = "{.value}_{yvar}"
  )



#discharge#############

daily_df<-daily_df%>% group_by(ID) %>%
  mutate(Q_med=  case_when(Q>= median(Q, na.rm = T)~ "sup",
                           Q<=median(Q, na.rm = T)~"inf"))%>%
  mutate(Q_ID= paste0(ID, sep="_", Q_med))


cols <- c('O2.mol.L', 'CO2.mol.L', 'Q', 'ID','Q_ID')
unique_sites <- unique(daily_df$Q_ID[!is.na(daily_df$Q_ID)])

streams <- setNames(
  lapply(unique_sites, function(site_id) {
    df_subset <- daily_df %>%
      filter(Q_ID == site_id) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites)

#Turn this into a function where I can find the linear relationship between any two variables
streams_edited <- lapply(streams, function(df) {

  df <- df %>%
    filter(Q > 0, O2.mol.L > 0, CO2.mol.L>0) %>%
    filter(!is.na(Q_ID),!is.na(Q), !is.na(O2.mol.L), !is.na(CO2.mol.L))


  (DO.Q<-summary(lm(log10(O2.mol.L) ~ log10(Q), data = df)))
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

Q.slopes <- bind_rows(streams_edited, .id = "ID")%>%
  separate(ID, into = c("Q_ID", "Q_med"), sep = "_")

Q.inf<-Q.slopes %>% filter(Q_med=='inf')%>%
  rename(
    DO.inf.x=Slope.DO,
    DO.inf.p= pvalue.DO,
    CO2.inf.x=Slope.CO2,
    CO2.inf.p= pvalue.CO2,
    )
Q.sup<-Q.slopes %>% filter(Q_med=='sup')%>%
  rename(
    DO.sup.x=Slope.DO,
    DO.sup.p= pvalue.DO,
    CO2.sup.x=Slope.CO2,
    CO2.sup.p= pvalue.CO2,
  )
Q.slope.tbl<-left_join(Q.inf, Q.sup, by=c('Q_ID'))

common.layers.Q.trends<-list(
  geom_point(),
  stat_poly_line(formula = y ~ x, se = FALSE),
  stat_poly_eq(aes(group=Q_med,
    label = paste(..p.value.label.., ..eq.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE,
               size = 4.5, vjust=14.5, label.x = 'right'),
  facet_wrap(~ID, scales='free'),
  theme(
    axis.title.y = element_text(size = 17, angle = 90),
    axis.title.x = element_text(size = 17) ),
  scale_x_log10(), scale_y_log10()
)

Q.slope.tbl%>%select(Q_ID, DO.inf.x, DO.sup.x, DO.inf.p, DO.sup.p)

ggplot(data = daily_df, aes(x = Q, y = O2.mol.L*10^6, group=Q_med, color=Q_med)) +
  scale_color_manual(values=c('blue','darkblue'),
                     labels = c(expression('<'~Q[median]), expression('>'~Q[median])),
                     name=" ")+
  common.layers.Q.trends+legend_size+
  ylab(O2umol.label)+xlab(Q.label)





Q.slope.tbl%>%select(Q_ID, CO2.inf.x, CO2.inf.p, CO2.sup.x,  CO2.sup.p)

ggplot(data = daily_df%>%filter(ID %in% c('5','6', '9')), aes(x = Q, y = CO2.mol.L*10^6, group=Q_med, color=Q_med)) +
  scale_color_manual(values=c('darkorange','darkred'),
                     labels = c(expression('<'~Q[median]), expression('>'~Q[median])),
                     name=" ")+
  common.layers.Q.trends+legend_size+
  ylab(CO2umol.label)+xlab(Q.label)


ggplot(data = daily_df, aes(x = Q, y = CO2.mol.L*10^6)) + geom_point()+
  stat_poly_line(formula = y ~ x, se = FALSE)+
  stat_poly_eq(aes(
                 label = paste(..p.value.label.., sep = "~~~")),
             formula = y ~ x, parse = TRUE,
             size = 4.5, vjust=15, label.x = 'right')+
  scale_y_log10()+scale_x_log10()+
  ylab(CO2umol.label)+xlab(Q.label)+facet_wrap(~ID, scales='free')





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


O2.CO2 <- read_csv("04_Output/O2.CO2.fluxes.csv")%>%
  filter(complete.cases(CO2_flux, O2_flux))%>%
  left_join(hourly_df)%>%
  filter(Q>2)%>%
  distinct(ID, Date, .keep_all = T)

mol.flux_lm <- function(df) {
  if(nrow(df) < 10) return(NULL)  # skip if not enough data for a regression

  flux.lm <- lm(CO2_flux ~ O2_flux, data = df)
  cf <- coef(flux.lm)

  tibble(
    ID = df$ID[1],
    Date = df$Date[1],
    flux_slope = cf[2],
    flux_intercept = cf[1]
  )
}

mol.ellipse.lm <- O2.CO2 %>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date) %>%
  group_split() %>%
  map_dfr(mol.flux_lm)%>%
  left_join(daily_df, by=c('Date', 'ID'))

#how many days are below slope=-1
mol.ellipse.lm %>%
  mutate(
    slope.id = case_when(
      flux_slope < -1 ~ "inf",
      TRUE ~ "sup"
    )) %>%
  group_by(ID) %>%
  summarise(
    day.inf = sum(slope.id == "inf"),
    day.sup = sum(slope.id == "sup"),
    prop.inf = (day.inf / n())*100,
    prop.sup= (day.sup / n())*100
  )


mol.ellipse.lm$ssn <- factor(mol.ellipse.lm$ssn, levels = c("summer", "spring","autumn", "winter"))

mean_df <- mol.ellipse.lm %>%
  group_by(ID, ssn) %>%
  summarise(mean.slope = mean(flux_slope, na.rm = TRUE))

ggplot(mol.ellipse.lm, aes(x=ssn, y=flux_slope, fill=ssn)) +
  geom_boxplot(outliers = F)+
  geom_point(data = mean_df, aes(x = ssn, y = mean.slope), color = "red", size = 3)+
  geom_hline(yintercept = -1, color='red')+
  ylab("Daily Ellipse Slope")+facet_wrap(~ID, scales='free')

#means by season
mol.ellipse.lm %>%
  group_by(ID, ssn) %>%
  summarise(mean.slope = mean(flux_slope, na.rm = TRUE)) %>%
  pivot_wider(
    names_from = ssn,
    values_from = mean.slope
  )%>%
  select(ID, summer, spring, autumn, winter)



ggplot(mol.ellipse.lm, aes(x=ssn, y=flux_intercept, fill=ssn)) +
  geom_boxplot(outliers = F)+
  ylab("Daily Ellipse Offset")+facet_wrap(~ID, scales='free')


#Bank######
O2.real<-daily_df%>%
  select(-O2.Sat, -CO2.mol.L, -CO2.Sat)%>%
  rename(mol.L=O2.mol.L)%>%
  mutate(cat="real")
O2.sat<-daily_df%>%
  select(-O2.mol.L, -CO2.mol.L, -CO2.Sat)%>%
  rename(mol.L=O2.Sat)%>%
  mutate(cat="sat")
O2.temp<-rbind(O2.real,O2.sat)


CO2.real<-daily_df%>%
  select(-O2.Sat, -O2.mol.L, -CO2.Sat)%>%
  rename(mol.L=CO2.mol.L)%>%
  mutate(cat="real")
CO2.sat<-daily_df%>%
  select(-O2.mol.L, -CO2.mol.L, -O2.Sat)%>%
  rename(mol.L=CO2.Sat)%>%
  mutate(cat="sat")
CO2.temp<-rbind(CO2.real,CO2.sat)
