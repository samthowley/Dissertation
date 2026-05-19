library(tidyverse)
library(readxl)
library(cowplot)
library(plotly)
library(ggpmisc)
library(openxlsx)
library(plotly)
library(hydroTSM)
library(dplyr)
library(randomForest)
library(corrplot)
library(Matrix)
library(lme4)
library(car)
library(partR2)
library(weathermetrics)
library(coin)        # permutation-based Spearman tests
library(car)         # VIF
library(ggrepel)     # non-overlapping site labels on plots
library(broom)    



facet_order <- c("15","5","5a","6", "3", "13", "7","9")  # EDIT THIS

col<-c("internal" ='red', "external"='black', 'CO2_flux'='darkgray')

temperature <- read_csv("02_Clean_data/temperature.csv")%>%
  mutate(
    TempC=fahrenheit.to.celsius(Temp_PT), Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(TempC=mean(TempC, na.rm=T))

int.ext <- read_csv("04_Output/stream/external-internal.csv")%>%
  mutate(
    day=as.Date(Date))%>%left_join(temperature)

discharge <- read_csv("02_Clean_data/discharge.csv")

fdom <- read_csv("04_Output/stream/eem_stream.csv")%>%
  rename(day=Date)

DOC <- read_csv("04_Output/sampled.solid.carbon.csv")%>%
  filter(chapter=='stream')%>%
  rename(day=Date)

DO <- read_csv("02_Clean_data/DO_cleaned.csv")

SpC <- read_csv("02_Clean_data/SpC_cleaned.csv")%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(SpC=mean(SpC, na.rm=T))

pH <- read_csv("02_Clean_data/pH_cleaned.csv")%>%
  mutate(Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(pH=mean(pH, na.rm=T))


master_metabolism <- read_csv("04_Output/stream/master_metabolism.csv")%>%
  select(date, K600, ID)%>%
  rename(day=date)

gw_corrected_metabolism <- read_csv("04_Output/stream/gw_corrected_metabolism.csv")%>%
  mutate(day=as.Date(Date))%>%
  select(ID, day, NEP_corrected)

#spatial df##########
watershed.inundation <- read_csv("04_Output/watershed.inundation.csv")%>%
  mutate(total.wetland.cover=total.wetland.area/basin.area)%>%
  group_by(ID)%>%
  summarise(total.wetland.cover=mean(total.wetland.cover, na.rm=T))
    
flashiness <- discharge %>%
  group_by(ID) %>%
  summarise(
    n_days   = n(),
    mean_Q   = mean(Q, na.rm = TRUE),
    sd_Q     = sd(Q, na.rm = TRUE),
    CV       = sd_Q / mean_Q,
    RB_index = sum(abs(diff(Q)), na.rm = TRUE) / sum(Q, na.rm = TRUE),
    .groups  = "drop"
  )

mean.pH<-pH%>% group_by(ID)%>%
  summarise(pH=mean(pH, na.rm=T))

mean.SpC<-SpC%>% group_by(ID)%>%
  summarise(SpC=mean(SpC, na.rm=T))

spatial_df<-left_join(watershed.inundation, flashiness)%>%
  left_join(mean.pH)%>%left_join(mean.SpC)%>%
  filter(ID!=14.9)
#site function############

site_lm_table_fun <- function(data, response, id_col = ID, x_col = Q) {
  response <- rlang::enquo(response)
  id_col   <- rlang::enquo(id_col)
  x_col    <- rlang::enquo(x_col)

  x_name <- rlang::quo_name(x_col)
  y_name <- rlang::quo_name(response)

  data %>%
    group_by(!!id_col) %>%
    tidyr::nest() %>%
    mutate(
      mod = purrr::map(data, ~ lm(reformulate(x_name, y_name), data = .x)),
      intercept = purrr::map_dbl(mod, ~ unname(coef(.x)["(Intercept)"])),
      slope     = purrr::map_dbl(mod, ~ unname(coef(.x)[x_name])),
      r2        = purrr::map_dbl(mod, ~ broom::glance(.x)$r.squared),
      p_slope   = purrr::map_dbl(mod, ~ broom::tidy(.x) %>% dplyr::filter(term == x_name) %>% dplyr::pull(p.value))
    ) %>%
    select(!!id_col, intercept, slope, r2, p_slope) %>%
    ungroup()
}

# Fits F = a * x^b via log10(F) ~ log10(x); returns a, b, r2, se (of b), and p (of b)
site_power_fun <- function(data, response, id_col = ID, x_col = Q) {
  response <- rlang::enquo(response)
  id_col   <- rlang::enquo(id_col)
  x_col    <- rlang::enquo(x_col)

  x_name     <- rlang::quo_name(x_col)
  y_name     <- rlang::quo_name(response)
  log_x_term <- paste0("log10(", x_name, ")")
  formula_str <- paste0("log10(", y_name, ") ~ ", log_x_term)

  data %>%
    group_by(!!id_col) %>%
    tidyr::nest() %>%
    mutate(
      mod = purrr::map(data, ~ lm(as.formula(formula_str), data = .x)),
      a   = purrr::map_dbl(mod, ~ 10^unname(coef(.x)["(Intercept)"])),
      b   = purrr::map_dbl(mod, ~ unname(coef(.x)[log_x_term])),
      r2  = purrr::map_dbl(mod, ~ broom::glance(.x)$r.squared),
      se  = purrr::map_dbl(mod, ~ broom::tidy(.x) %>% dplyr::filter(term == log_x_term) %>% dplyr::pull(std.error)),
      p   = purrr::map_dbl(mod, ~ broom::tidy(.x) %>% dplyr::filter(term == log_x_term) %>% dplyr::pull(p.value))
    ) %>%
    select(!!id_col, a, b, r2, se, p) %>%
    ungroup()
}

