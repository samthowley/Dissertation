
source("03_Scripts/Streams/analysis/data for analysis.R")
library(tidyverse)
library(broom)
library(car)       # vif()
library(ggplot2)
library(ggrepel)

df <- int.ext %>%
  mutate(ID = factor(ID))

spatial_df <- spatial_df %>%
  mutate(
    ID                  = factor(ID),
    total.wetland.cover = total.wetland.cover * 100
  )

# Pathway fill colors for density areas
pathway_palette <- c(Internal = "#E07B54", External = "#4A90D9")

# Site colors for rug / bar marks — one distinct color per site
site_palette <- c(
  "3"  = "#E41A1C",
  "5"  = "#377EB8",
  "5a" = "#4DAF4A",
  "6"  = "#984EA3",
  "7"  = "#FF7F00",
  "9"  = "#A65628",
  "13" = "#F781BF",
  "15" = "#999999"
)

predictor_lookup <- tibble(
  predictor       = c("total.wetland.cover", "RB_index", "pH", "SpC"),
  predictor_label = c("Wetland cover (%)", "RB flashiness index",
                      "pH", "Specific conductivity (μS cm⁻¹)")
)


# =============================================================================
# SLOPE FITTING — mirrors spatial_analysis.R exactly
# =============================================================================

fit_loglog <- function(ID_data, flux_col, min_n = 10) {
  d <- ID_data %>%
    filter(.data[[flux_col]] > 0, Q > 0) %>%
    mutate(log_flux = log(.data[[flux_col]]), log_Q = log(Q)) %>%
    filter(is.finite(log_flux), is.finite(log_Q))
  n_used <- nrow(d)
  if (n_used < min_n) {
    return(data.frame(slope = NA_real_, slope_se = NA_real_, p_value = NA_real_,
                      r2 = NA_real_, n_used = n_used,
                      flag = paste0("n=", n_used, " < ", min_n, " (insufficient)")))
  }
  mod  <- lm(log_flux ~ log_Q, data = d)
  tidm <- tidy(mod)
  glam <- glance(mod)
  data.frame(
    slope    = tidm$estimate[tidm$term == "log_Q"],
    slope_se = tidm$std.error[tidm$term == "log_Q"],
    p_value  = tidm$p.value[tidm$term == "log_Q"],
    r2       = glam$r.squared,
    n_used   = n_used,
    flag     = ifelse(glam$r.squared < 0.10, "r2 < 0.10", "OK")
  )
}

fit_temp_sensitivity <- function(ID_data, flux_col, vif_threshold = 5, min_n = 10) {
  d <- ID_data %>%
    filter(.data[[flux_col]] > 0, !is.na(TempC), !is.na(Q)) %>%
    mutate(log_flux = log(.data[[flux_col]])) %>%
    filter(is.finite(log_flux))
  n_used <- nrow(d)
  if (n_used < min_n) {
    return(data.frame(m = NA_real_, m_se = NA_real_, r2 = NA_real_,
                      n_used = n_used, max_vif = NA_real_,
                      model_used = "insufficient data",
                      flag = paste0("n=", n_used, " < ", min_n)))
  }
  mod_biv  <- lm(log_flux ~ TempC + Q, data = d)
  vif_vals <- tryCatch(vif(mod_biv), error = function(e) c(TempC = NA, Q = NA))
  max_vif  <- max(vif_vals, na.rm = TRUE)
  if (!is.na(max_vif) && max_vif > vif_threshold) {
    mod_use    <- lm(log_flux ~ TempC, data = d)
    model_used <- "simple (log_flux ~ TempC)"
    flag_str   <- paste0("VIF=", round(max_vif, 1), " > ", vif_threshold, " — fallback")
  } else {
    mod_use    <- mod_biv
    model_used <- "partial (log_flux ~ TempC + Q)"
    flag_str   <- if (!is.na(max_vif)) "OK" else "VIF unavailable"
  }
  tidm  <- tidy(mod_use)
  glam  <- glance(mod_use)
  m_row <- tidm[tidm$term == "TempC", ]
  data.frame(
    m = m_row$estimate, m_se = m_row$std.error,
    r2 = glam$r.squared, n_used = n_used,
    max_vif    = round(max_vif, 2),
    model_used = model_used,
    flag       = if (glam$r.squared < 0.10) paste0(flag_str, "; r2 < 0.10") else flag_str
  )
}

slopes_2a <- df %>%
  group_by(ID) %>%
  group_modify(~ map(c("internal", "external"), function(p) {
    fit_loglog(.x, p) %>% mutate(pathway = p)
  }) |> list_rbind()) %>%
  ungroup()

slopes_2b <- df %>%
  group_by(ID) %>%
  group_modify(~ map(c("internal", "external"), function(p) {
    fit_temp_sensitivity(.x, p) %>% mutate(pathway = p)
  }) |> list_rbind()) %>%
  ungroup()

slopes_2a_wide <- slopes_2a %>%
  select(ID, pathway, slope) %>%
  pivot_wider(names_from = pathway, values_from = slope) %>%
  rename(c_int = internal, c_ext = external) %>%
  left_join(spatial_df, by = "ID")

slopes_2b_wide <- slopes_2b %>%
  select(ID, pathway, m) %>%
  pivot_wider(names_from = pathway, values_from = m) %>%
  rename(m_int = internal, m_ext = external) %>%
  left_join(spatial_df, by = "ID")


# =============================================================================
# 1. KERNEL DENSITY PLOTS WITH SITE-COLORED RUG BARS
# =============================================================================
# fill  = Pathway  (density area color — no conflict with color scale)
# color = site ID  (rug bar color)
# geom_rug with linewidth = 4 renders as visible colored bars at the bottom.

# --- 1a  Discharge sensitivity (c) -------------------------------------------

discharge_dens <- slopes_2a %>%
  filter(!is.na(slope)) %>%
  mutate(
    Pathway  = case_match(pathway, "internal" ~ "Internal", "external" ~ "External"),
    site_lab = as.character(ID)
  )

p_discharge_density <- ggplot(discharge_dens, aes(x = slope)) +
  geom_density(aes(fill = Pathway), alpha = 0.4, color = NA) +
  geom_rug(aes(color = site_lab), sides = "b",
           linewidth = 4, length = unit(0.06, "npc"), alpha = 0.85) +
  scale_fill_manual(values  = pathway_palette, name = "Pathway") +
  scale_color_manual(values = site_palette,    name = "Site") +
  labs(
    x     = "Discharge sensitivity slope (c)",
    y     = "Density",
    title = "Distribution of discharge sensitivity slopes"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "right")


# --- 1b  Temperature sensitivity (m) -----------------------------------------

temp_dens <- slopes_2b %>%
  filter(!is.na(m)) %>%
  mutate(
    Pathway  = case_match(pathway, "internal" ~ "Internal", "external" ~ "External"),
    site_lab = as.character(ID)
  )

p_temp_density <- ggplot(temp_dens, aes(x = m)) +
  geom_density(aes(fill = Pathway), alpha = 0.4, color = NA) +
  geom_rug(aes(color = site_lab), sides = "b",
           linewidth = 4, length = unit(0.06, "npc"), alpha = 0.85) +
  scale_fill_manual(values  = pathway_palette, name = "Pathway") +
  scale_color_manual(values = site_palette,    name = "Site") +
  labs(
    x     = "Temperature sensitivity slope (m)",
    y     = "Density",
    title = "Distribution of temperature sensitivity slopes"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "right")




# =============================================================================
# 2. BOX AND WHISKER PLOTS — internal vs. external, per pathway
# =============================================================================
# fill  = Pathway  (box color)
# color = site ID  (jittered point color) — consistent with density plots above

# --- 2a  Discharge sensitivity (c) -------------------------------------------

p_discharge_box <- ggplot(discharge_dens, aes(x = Pathway, y = slope, fill = Pathway)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = site_lab), width = 0.12, size = 2, alpha = 0.85) +
  scale_fill_manual(values  = pathway_palette, name = "Pathway") +
  scale_color_manual(values = site_palette,    name = "Site") +
  labs(
    x     = "Pathway",
    y     = "Discharge sensitivity slope (c)",
    title = "Internal vs. external discharge sensitivity slopes"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "right")


# --- 2b  Temperature sensitivity (m) -----------------------------------------

p_temp_box <- ggplot(temp_dens, aes(x = Pathway, y = m, fill = Pathway)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = site_lab), width = 0.12, size = 2, alpha = 0.85) +
  scale_fill_manual(values  = pathway_palette, name = "Pathway") +
  scale_color_manual(values = site_palette,    name = "Site") +
  labs(
    x     = "Pathway",
    y     = "Temperature sensitivity slope (m)",
    title = "Internal vs. external temperature sensitivity slopes"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "right")




# =============================================================================
# 3. EXPLORATORY SCATTER PLOTS — slopes vs. spatial predictors
# =============================================================================

# --- 2a  Discharge slopes (c_int, c_ext) -------------------------------------

discharge_scatter <- slopes_2a_wide %>%
  pivot_longer(cols = c(c_int, c_ext),
               names_to = "slope_type", values_to = "slope") %>%
  pivot_longer(cols = all_of(predictor_lookup$predictor),
               names_to = "predictor", values_to = "pred_value") %>%
  left_join(predictor_lookup, by = "predictor") %>%
  filter(!is.na(slope), !is.na(pred_value)) %>%
  mutate(Pathway = case_match(slope_type, "c_int" ~ "Internal", "c_ext" ~ "External"))

p_discharge_scatter <- ggplot(discharge_scatter,
                               aes(x = pred_value, y = slope,
                                   color = Pathway, label = ID)) +
  geom_point(size = 2.5) +
  geom_text_repel(size = 3, show.legend = FALSE) +
  facet_wrap(~ predictor_label, scales = "free_x", ncol = 2) +
  scale_color_manual(values = pathway_palette) +
  labs(
    x     = "Predictor value",
    y     = "Discharge sensitivity slope (c)",
    color = "Pathway",
    title = "Exploratory: discharge sensitivity slopes (c_int, c_ext) vs. spatial predictors"
  ) +
  theme_bw(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))



# --- 2b  Temperature slopes (m_int, m_ext) -----------------------------------

temp_scatter <- slopes_2b_wide %>%
  pivot_longer(cols = c(m_int, m_ext),
               names_to = "slope_type", values_to = "slope") %>%
  pivot_longer(cols = all_of(predictor_lookup$predictor),
               names_to = "predictor", values_to = "pred_value") %>%
  left_join(predictor_lookup, by = "predictor") %>%
  filter(!is.na(slope), !is.na(pred_value)) %>%
  mutate(Pathway = case_match(slope_type, "m_int" ~ "Internal", "m_ext" ~ "External"))

p_temp_scatter <- ggplot(temp_scatter,
                          aes(x = pred_value, y = slope,
                              color = Pathway, label = ID)) +
  geom_point(size = 2.5) +
  geom_text_repel(size = 3, show.legend = FALSE) +
  facet_wrap(~ predictor_label, scales = "free_x", ncol = 2) +
  scale_color_manual(values = pathway_palette) +
  labs(
    x     = "Predictor value",
    y     = "Temperature sensitivity slope (m)",
    color = "Pathway",
    title = "Exploratory: temperature sensitivity slopes (m_int, m_ext) vs. spatial predictors"
  ) +
  theme_bw(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))


