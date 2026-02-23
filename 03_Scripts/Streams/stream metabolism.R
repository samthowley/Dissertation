rm(list=ls())

#packages#####
library(ggpubr)
library(tidyverse)
library(writexl)
library(openxlsx)
library(readxl)
library(cowplot)
library(lubridate)
library(streamMetabolizer)
library(weathermetrics)
library('StreamMetabolism')
library(lme4)

#constants######
samplingperiod <- data.frame(solar.time = rep(seq(from=as.POSIXct("2023-10-06 00:00", tz="UTC"),
                                            to=as.POSIXct("2025-08-30 00:00", tz="UTC"),by="hour")))

#data####
file.names <- list.files(path="02_Clean_data", pattern=".csv", full.names=TRUE)
file.names<-file.names[c(5,7,11,6)]
data <- lapply(file.names,function(x) {read_csv(x, col_types = cols(ID = col_character()))})

merged_data <- reduce(data, left_join, by = c("ID", 'Date'))%>%
  filter(complete.cases(DO, depth))%>%
  mutate(ln.Q=log(Q))%>%
  group_by(ID)%>%
  mutate(split_q=case_when(depth>mean(depth, na.rm=T)~'hi',
                           depth<=mean(depth, na.rm=T)~'lo'))%>%
  mutate(ID_q = paste(ID, split_q, sep = "_"))


input <- merged_data %>%
  filter(depth > 0,ID != '14')%>%
  rename('DO.obs'='DO')%>%
  mutate(
    temp.water=fahrenheit.to.celsius(Temp_PT.x))%>%
  mutate(
    DO.sat=Cs(temp.water),
    solar.time=as.POSIXct(Date, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
  )%>%
  mutate(
    light=calc_light(solar.time,  29.8, -82.6))

split_list <- input %>%
  group_by(ID_q) %>%
  group_split()

names(split_list) <- input %>%
  group_by(ID_q) %>%
  group_keys() %>%
  pull(ID_q)

rdy_for_sm<- lapply(split_list, function(df) {
  df<-df %>%
    arrange(solar.time) %>%
    filter(c(TRUE, diff(solar.time) > 0))%>%
    select(solar.time, light, depth, DO.sat, DO.obs, temp.water)

  df<-left_join(samplingperiod, df)
})


#K600#############
sheet_names <- excel_sheets("04_Output/rC_K600.xlsx")
ks <- sheet_names[!sheet_names %in% c("6a")]

list_of_ks <- list()
for (sheet in ks) {
  df <- read_excel("04_Output/rC_K600.xlsx", sheet = sheet)
  list_of_ks[[sheet]] <- df
}

#specs######
k600_mean_list <- lapply(list_of_ks, function(k600_df) {
  k600 <- k600_df %>%
    group_by(ID_q) %>%
    summarise(
      K600=mean(k600_m.day,na.rm=T),
      sd_vals=sd(k600_m.day,na.rm=T))

  return(k600)
})

specs <- lapply(k600_mean_list, function(K_means) {
  site_id <- K_means$ID[1]
  K_vals <- K_means$K600[[1]]
  sd_vals <- K_means$sd_vals[[1]]


  # Handle missing or NA values in K_vals
  if (all(is.na(K_vals))) {
    warning(paste("Skipping site", site_id, "- K_vals all NA"))
    return(NULL)
  }

  # Build specs
  bayes_name <- mm_name(type='bayes',
                        pool_K600='normal',
                        err_obs_iid=TRUE, err_proc_iid=TRUE)

  bayes_specs <- specs(bayes_name,
                       K600_daily_meanlog_meanlog= log(K_vals),
                       K600_daily_meanlog_sdlog=log(2),
                       GPP_daily_lower=0,
                       burnin_steps=1000,
                       saved_steps=1000)
  })


valid_ids <- names(specs)[!sapply(specs, is.null)]
valid_streams <- rdy_for_sm[valid_ids]
valid_specs <- specs[valid_ids]

# Run streamMetabolizer on each valid site##############
metab_results_base <- mapply(function(site_data, site_spec) {
  metab(site_spec, data = site_data)
}, site_data = valid_streams, site_spec = valid_specs, SIMPLIFY = FALSE)

met_list_base <- lapply(metab_results_base, function(metab_results) {
   prediction2 <- metab_results@fit$daily #%>%
  return(prediction2)
})

met_results <- bind_rows(met_list_base, .id = "ID")%>%
  filter(
    GPP_daily_mean>0, ER_daily_mean<0, ER_Rhat > 0.9 & ER_Rhat < 1.2,K600_daily_Rhat > 0.9 & K600_daily_Rhat < 1.2)%>%
  separate(
    ID, into = c("ID", "q_sep"), sep = "_")%>%
  select(
    date, GPP_daily_mean, ER_daily_mean, K600_daily_mean, ID, -q_sep)%>%
  arrange(ID, date)%>%
  rename(
    GPP=GPP_daily_mean, ER=ER_daily_mean, K600=K600_daily_mean)



ggplot(met_results%>%filter(ID=='7'), aes(date)) +
  geom_point(aes(y = ER, color = 'ER')) +
  geom_point(aes(y = GPP, color = 'GPP')) +
  facet_wrap(~ ID, ncol = 3, scale = 'free') +
  #ylab(expression(O[2]~'g'/m^2/'day')) +
  xlab("Date")

write_csv(met_results, "04_Output/master_metabolism.csv")


####Testing specs with 5#####################################

bayes_name <- mm_name(type='bayes',
                      pool_K600='normal',
                      err_obs_iid=TRUE, err_proc_iid=TRUE)

bayes_specs <- specs(bayes_name,
                     K600_daily_meanlog_meanlog= log(5.6),
                     K600_daily_meanlog_sdlog=log(6),
                     GPP_daily_lower=0,
                     burnin_steps=1000,
                     saved_steps=1000)


s5<- input%>% filter(ID=='5')%>%
  select(solar.time, light, depth, DO.sat, DO.obs, temp.water)
s5_lo<-left_join(samplingperiod, s5)%>% filter(depth<mean(s5$depth, na.rm=T))%>%select(-ID)

ggplot(s5,aes(x=solar.time, y=depth)) + geom_point()




mm <- metab(bayes_specs, data=s5_lo)
s5_lo_results <- mm@fit$daily %>% select(date,GPP_daily_mean,ER_daily_mean,K600_daily_mean,
                                       GPP_Rhat,ER_Rhat,K600_daily_Rhat)

s5<-rbind(s5_hi_results, s5_lo_results)%>%arrange(date)

ggplot(s5,aes(x=date, y=GPP_daily_mean)) + geom_point()
ggplot(s5%>%filter(ER_Rhat>1.2),aes(x=date, y=ER_daily_mean)) + geom_point()

write_csv(s5, "04_Output/metabolism/s5.csv")
