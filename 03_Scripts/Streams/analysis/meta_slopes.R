library(tidyverse)
library(broom)

# --- Slopes from power function fits ---
slopes <- read_csv("04_Output/stream/power_slopes.csv") %>%
  select(ID, pathway, driver, b, r2, se)

# --- Site-level spatial variable means ---
pH_means <- read_csv("02_Clean_data/pH_cleaned.csv") %>%
  group_by(ID) %>%
  summarise(mean_pH = mean(pH, na.rm = TRUE), .groups = "drop")

SpC_means <- read_csv("02_Clean_data/SpC_cleaned.csv") %>%
  group_by(ID) %>%
  summarise(mean_SpC = mean(SpC, na.rm = TRUE), .groups = "drop")

wetland <- read_csv("01_Raw_data/wetland cover/wetland.perc.csv") %>%
  select(ID, basin.wetland.perc)

flashiness <- read_csv("04_Output/stream/flashiness.csv") %>%
  select(ID, mean_Q, CV, RB_index)

spatial <- list(pH_means, SpC_means, wetland, flashiness) %>%
  reduce(left_join, by = "ID")

# --- Join slopes with spatial variables ---
meta_data <- left_join(slopes, spatial, by = "ID")

# --- Model runner ---
# For mean_Q: power function (log10-log10); all others: linear lm
# Finite-value filter handles negative b and zero r2 after log10 transform
run_model <- function(data, response, predictor, use_power) {
  df <- data %>%
    select(all_of(c(response, predictor))) %>%
    drop_na()

  if (use_power) {
    df <- df %>%
      mutate(across(everything(), log10)) %>%
      filter(if_all(everything(), is.finite))
  }

  if (nrow(df) < 3) return(tibble(p_value = NA_real_, n = nrow(df)))

  mod <- lm(as.formula(paste(response, "~", predictor)), data = df)

  broom::tidy(mod) %>%
    filter(term == predictor) %>%
    transmute(p_value = p.value, n = nrow(df))
}

# --- Run all combinations ---
response_vars <- c("b", "r2", "se")
predictors    <- c("mean_pH", "mean_SpC", "basin.wetland.perc", "RB_index", "CV", "mean_Q")

results <- meta_data %>%
  group_by(pathway, driver) %>%
  group_split() %>%
  map_dfr(function(grp) {
    expand_grid(response = response_vars, predictor = predictors) %>%
      mutate(
        model_type = if_else(predictor == "mean_Q", "power", "linear"),
        out        = map2(response, predictor,
                         ~ run_model(grp, .x, .y, use_power = (.y == "mean_Q")))
      ) %>%
      unnest(out) %>%
      mutate(
        pathway = unique(grp$pathway),
        driver  = unique(grp$driver)
      )
  }) %>%
  select(pathway, driver, response, predictor, model_type, n, p_value) %>%
  mutate(significant = p_value <= 0.05)

write_csv(results, "04_Output/stream/meta_slopes.csv")

results%>% filter(significant==TRUE)
