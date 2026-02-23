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


r2_nakagawa <- function(model) {
  var_fixed <- var(fitted(model))
  var_random <- VarCorr(model)$ID[1]
  var_resid <- sigma(model)^2
  r2_marg <- var_fixed / (var_fixed + var_random + var_resid)
  r2_cond <- (var_fixed + var_random) / (var_fixed + var_random + var_resid)
  return(c(marginal = r2_marg, conditional = r2_cond))
}

facet_order <- c("15","5","5a","6", "3", "13", "7","9")  # EDIT THIS

col<-c("internal" ='red', "external"='black', 'CO2_flux'='darkgray')

theme_set(theme(axis.text.x = element_text(12),
                axis.text.y = element_text(size = 12),

                axis.title.x = element_text(size=14, angle=360),
                axis.title.y = element_text(size=14, angle=90),
                plot.title = element_text(size = 14),

                legend.key.size = unit(0.5, "cm"),
                legend.key.height = unit(1, "cm"),
                legend.key.width = unit(1, "cm"),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 12),
                legend.position ="bottom",

                panel.background = element_rect(fill = 'white'),
                strip.text = element_text(size = 10),

                axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "gray"),
                axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "gray")))




wetland_cover <- read_csv("01_Raw_data/wetland cover/wetland_cover.csv")%>%
  rename(Basin=Basin_Name)%>%
  select(Basin, PERCENTAGE)%>%
  rename(wetland.perc=PERCENTAGE)

int.ext <- read_csv("04_Output/stream/external-internal.csv")%>%
  mutate(
    Basin=as.factor(Basin),
    day=as.Date(Date))%>%
  left_join(wetland_cover)%>%
  mutate(
    wetland.perc=round(wetland.perc, 2),
    ID_wetland.perc=paste0(ID, "_", wetland.perc)
         )

labels_vec_wetperc <- setNames(
  paste0(int.ext$ID, "\n", int.ext$wetland.perc),
  int.ext$ID_wetland.perc)

watershed_inundation <- read_csv("01_Raw_data/wetland cover/watershed.inundation.csv")%>%
  select(day, Basin, AREA, watershed.inundation)

temperature <- read_csv("02_Clean_data/temperature.csv")

discharge <- read_csv("02_Clean_data/discharge.csv")

fdom <- read_csv("04_Output/stream/eem_stream.csv")%>%
  rename(day=Date)

DOC <- read_csv("04_Output/sampled.solid.carbon.csv")%>%
  filter(chapter=='stream')%>%
  rename(day=Date)

DO <- read_csv("02_Clean_data/DO_cleaned.csv")

master_metabolism <- read_csv("04_Output/stream/master_metabolism.csv")%>%
  select(date, K600, ID)%>%
  rename(day=date)

gw_corrected_metabolism <- read_csv("04_Output/stream/gw_corrected_metabolism.csv")%>%
  mutate(day=as.Date(Date))%>%
  select(ID, day, NEP_corrected)

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
