library(tidyverse)
library(readxl)
library(cowplot)
library(plotly)
library(ggpmisc)
library(openxlsx)
library(hydroTSM)
library(dplyr)
library(randomForest)
library(corrplot)
library(Matrix)
library(lme4)
library(car)
library(partR2)
library(weathermetrics)
library(ggrepel)     # non-overlapping site labels on plots
library(broom)
library(janitor)
library(flextable)
library(officer)



facet_order <- c("15","5","5a","6", "3", "13", "7","9")  # EDIT THIS

col<-c("internal" ='red', "external"='black', 'CO2_flux'='darkgray')

#water quality#########
int.ext <- read_csv("04_Output/stream/external-internal.csv")

temperature <- read_csv("02_Clean_data/temperature.csv")%>%
  mutate(
    TempC=fahrenheit.to.celsius(Temp_PT), Date=as.Date(Date))%>%
  group_by(ID, Date)%>%
  summarise(TempC=mean(TempC, na.rm=T))


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
  )%>%filter(ID != '14')


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
    # lm() drops NAs but not the -Inf/NaN that log10() yields on zero/negative
    # values (e.g. Q <= 0 at sites 5/5a), so drop non-finite rows per fit here
    filter(is.finite(!!response), is.finite(!!x_col)) %>%
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


#meta analysis#########
int.ext.summary<-left_join(int.ext, pH)%>%
  group_by(ID)%>%
  summarise(
    discharge_m3_s= mean(Q/10^3, na.rm=T),
    CO2flux.mn=mean(CO2_flux, na.rm=T),
    internal.mn=mean(internal, na.rm=T),
    external.mn=mean(external, na.rm=T),
    pH=mean(pH, na.rm=T),
    TempC=mean(TempC, na.rm=T),
  )%>%
  rename(Site=ID)%>%
  mutate(
    DOI="This Paper",
    Citation="This Paper",
    Site_ID="This Paper",
    Location="Florida, Coastal Plain",
    Biome="Subtropical",
    Source_Water_Brief="Wetland seepage",
    
    Source_Water_Brief=if_else(Site==13, "Mixed", Source_Water_Brief),
    Source_Water_Brief=if_else(Site==5, "Mixed", Source_Water_Brief),
    precip_cm_yr=120
  )

# pubs block disabled: read_csv("01_Raw_data/meta_analysis_extraction.csv") -- this raw
# file no longer exists (superseded by meta_analysis_extraction_GENERATED_v2.csv, used
# directly by metaanalysis_spatiotempo_analysis.R). `pubs` was never referenced anywhere
# downstream of this file, so it's dead code; disabled rather than deleted in case the
# old file is restored. Left commented out 2026-08-09 to unblock sourcing.
# pubs<-read_csv("01_Raw_data/meta_analysis_extraction.csv")%>%
#   select(Citation, Location, Biome, Source, Discharge_m3s, CO2_flux_gCm2day, Internal_Pathway_gCm2day, External_Pathway_gCm2day,
#          pH)%>%
#   rename(
#     discharge_m3_s = Discharge_m3s,
#     CO2flux.mn = CO2_flux_gCm2day,
#     internal.mn = Internal_Pathway_gCm2day,
#     external.mn = External_Pathway_gCm2day
#   )%>%
#   mutate(across(5:9, as.numeric))%>%
#   filter(!is.na(internal.mn))%>%
#   full_join(int.ext.summary)%>%
#   mutate( pct_internal = (internal.mn / CO2flux.mn) * 100) %>%
#   arrange(discharge_m3_s) %>%
#   mutate(
#     # Sub-label with mean discharge
#     x_label = paste0(Source, "\n(", round(discharge_m3_s, 3), " m³ s⁻¹)"),
#     x_label = factor(x_label, levels = unique(x_label))  # preserve Q order
#   )%>%    filter(external.mn > 0, internal.mn>0.1)
