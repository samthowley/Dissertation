
source("03_Scripts/Streams/analysis/data for analysis.R")


df <- int.ext %>%
  mutate(ID = factor(ID))

spatial_df <- spatial_df %>%
  mutate(
    ID                  = factor(ID),
    total.wetland.cover = total.wetland.cover * 100
  )

stopifnot("All site IDs in df must appear in spatial_df" =
  all(as.character(unique(df$ID)) %in% as.character(spatial_df$ID)))


# =============================================================================
# SECTION 1 — PRE-ANALYSIS DIAGNOSTICS
# =============================================================================


# --- 1b  Pairwise Spearman correlations among all spatial predictors ----------
# Predictors: total.wetland.cover, CV, pH, SpC
# Flag |rho| > 0.7 as potentially redundant.

spatial_pred_cols <- c("total.wetland.cover", "CV", "pH", "SpC")

corr_pred_pairs <- combn(spatial_pred_cols, 2, simplify = FALSE) %>%
  map_dfr(function(pair) {
    ct <- cor.test(spatial_df[[pair[1]]], spatial_df[[pair[2]]],
                   method = "spearman", exact = FALSE)
    data.frame(predictor_1 = pair[1], predictor_2 = pair[2],
               rho = round(ct$estimate, 3), p = round(ct$p.value, 4))
  })

print(corr_pred_pairs, row.names = FALSE)

high_corr_pairs <- corr_pred_pairs %>% filter(abs(rho) > 0.7)
if (nrow(high_corr_pairs) > 0) {
  cat("\nFLAG: |rho| > 0.7 for the following pairs — interpret Stage 2 results cautiously:\n")
  print(high_corr_pairs, row.names = FALSE)
} else {
  cat("\nAll predictor pairs: |rho| <= 0.7. Collinearity OK.\n")
}

# --- 1c  Spearman correlation: TempC vs Q within each ID -------------------

corr_TQ_ID <- df %>%
  group_by(ID) %>%
  summarise(
    n_valid = sum(!is.na(TempC) & !is.na(Q)),
    rho_TQ  = if (n_valid >= 3)
                cor(TempC, Q, use = "complete.obs", method = "spearman")
              else NA_real_,
    flag_TQ = ifelse(!is.na(rho_TQ) & abs(rho_TQ) > 0.7,
                     "HIGH collinearity — VIF likely elevated", "OK"),
    .groups = "drop"
  )

print(corr_TQ_ID)
cat("\nIDs with |rho| > 0.7 indicate seasonal T-Q confounding.\n")
cat("For those IDs, VIF > 5 in the bivariate model is expected;\n")
cat("the script will fall back to log(flux) ~ TempC only and flag them.\n\n")

# --- 1d  Diagnostic summary flags --------------------------------------------

diag_counts <- df %>%
  group_by(ID) %>%
  summarise(
    n_total     = n(),
    n_ext_le0   = sum(external <= 0, na.rm = TRUE),
    pct_ext_le0 = 100 * n_ext_le0 / n_total,
    .groups     = "drop"
  )

flag_ext <- diag_counts %>%
  filter(pct_ext_le0 > 20) %>%
  pull(ID) %>%
  as.character()

if (length(flag_ext) > 0) {
  cat("  IDS with >20% non-positive external flux:", paste(flag_ext, collapse = ", "), "\n")
  cat("  -> These IDs lose substantial data in log-ratio and log-log analyses.\n")
} else {
  cat("  All IDs: <= 20% non-positive external flux. OK.\n")
}

if (nrow(high_corr_pairs) > 0) {
  cat("  Spatial predictors: HIGH collinearity detected (see 1b above).\n")
} else {
  cat("  Spatial predictors: collinearity OK.\n")
}

high_TQ <- corr_TQ_ID %>% filter(flag_TQ != "OK") %>% pull(ID) %>% as.character()
if (length(high_TQ) > 0) {
  cat("  T-Q collinearity IDs:", paste(high_TQ, collapse = ", "), "\n")
} else {
  cat("  T-Q collinearity: all IDs OK.\n")
}


# =============================================================================
# SECTION 2 — GOAL 1: MEAN LOG(internal / external) RATIO PER ID
# =============================================================================


log_ratio_ID <- df %>%
  filter(internal > 0, external > 0) %>%
  mutate(log_ratio = log(internal / external)) %>%
  group_by(ID) %>%
  summarise(
    n_valid_logratio = n(),
    mean_log_ratio   = mean(log_ratio, na.rm = TRUE),
    sd_log_ratio     = sd(log_ratio,   na.rm = TRUE),
    .groups = "drop"
  )

cat("  Positive = internal dominance; negative = external dominance; 0 = parity\n")
print(log_ratio_ID)


goal1_df <- log_ratio_ID %>%
  left_join(spatial_df, by = "ID")

run_perm_spearman <- function(response_vec, predictor_vec, ID_labels,
                               resp_name, pred_name,
                               nresample = 99999) {
  d <- data.frame(
    resp = response_vec,
    pred = predictor_vec,
    ID = ID_labels
  ) %>% filter(!is.na(resp) & !is.na(pred))

  if (nrow(d) < 4) {
    warning("Too few observations for permutation test: ", resp_name, " ~ ", pred_name)
    return(data.frame(response = resp_name, predictor = pred_name,
                      rho = NA, p_raw = NA, n = nrow(d)))
  }

  rho <- cor(d$resp, d$pred, method = "spearman")

  pt <- spearman_test(
    resp ~ pred,
    data         = d,
    distribution = approximate(nresample = nresample)
  )

  data.frame(
    response  = resp_name,
    predictor = pred_name,
    rho       = round(rho, 3),
    p_raw     = round(pvalue(pt)[[1]], 5),
    n         = nrow(d)
  )
}

NRESAMPLE <- 999999 # set to 9999 for quick dev runs; restore to 99999 for final results

predictors_goal1 <- c("total.wetland.cover", "CV", "pH", "SpC")

perm_goal1 <- map(predictors_goal1, function(pred) {
  run_perm_spearman(goal1_df$mean_log_ratio, goal1_df[[pred]],
                    goal1_df$ID, "mean_log_ratio", pred, NRESAMPLE)
}) |> list_rbind()


perm_goal1$p_BH <- round(p.adjust(perm_goal1$p_raw, method = "BH"), 5)
perm_goal1$sig  <- ifelse(perm_goal1$p_BH < 0.05, "*", "")


print(perm_goal1, row.names = FALSE)
cat("\nNote: power at n=9 requires |rho| >= ~0.68 for p < 0.05 (two-tailed).\n")
cat("Do not over-interpret non-significant results with moderate rho.\n\n")



# =============================================================================
# SECTION 3 — GOAL 2a: DISCHARGE SENSITIVITY (c SLOPES)
# =============================================================================


fit_loglog <- function(ID_data, flux_col, min_n = 10) {
  d <- ID_data %>%
    filter(.data[[flux_col]] > 0, Q > 0) %>%
    mutate(
      log_flux = log(.data[[flux_col]]),
      log_Q    = log(Q)
    ) %>%
    filter(is.finite(log_flux), is.finite(log_Q))

  n_used <- nrow(d)

  if (n_used < min_n) {
    return(data.frame(slope = NA_real_, slope_se = NA_real_, p_value = NA_real_,
                      r2    = NA_real_, n_used   = n_used,
                      flag  = paste0("n=", n_used, " < ", min_n, " (insufficient)")))
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
    flag     = ifelse(glam$r.squared < 0.10,
                      "r2 < 0.10 — slope estimate unreliable", "OK")
  )
}


pathways_2a <- c("internal", "external", "CO2_flux")

slopes_2a <- df %>%
  group_by(ID) %>%
  group_modify(~ {
    map(pathways_2a, function(p) {
      fit_loglog(.x, p) %>%
        mutate(pathway = p)
    }) |> list_rbind()
  }) %>%
  ungroup()

print(slopes_2a %>% select(ID, pathway, slope, slope_se, r2, n_used, flag),
      row.names = FALSE)


low_r2_2a <- slopes_2a %>% filter(flag != "OK", !is.na(slope))

if (nrow(low_r2_2a) > 0) {
  cat("--- 3c: WARNING — IDs with r² < 0.10 (unreliable slopes) ---\n")
  print(low_r2_2a %>% select(ID, pathway, r2, flag), row.names = FALSE)
  cat("These ID-pathway combinations are retained in Stage 2 but flagged.\n")
  cat("Interpret Stage 2 Spearman results cautiously when flagged slopes are included.\n\n")
} else {
  cat("--- 3c: All ID-pathway r² >= 0.10. OK.\n\n")
}


slopes_2a_wide <- slopes_2a %>%
  select(ID, pathway, slope) %>%
  pivot_wider(names_from = pathway, values_from = slope) %>%
  rename(c_int   = internal,
         c_ext   = external,
         c_total = CO2_flux) %>%
  left_join(spatial_df, by = "ID")


responses_2a  <- c("c_int", "c_ext", "c_total")
predictors_2a <- c("total.wetland.cover", "CV", "pH", "SpC")

perm_goal2a <- map2(
  rep(responses_2a, each = length(predictors_2a)),
  rep(predictors_2a, times = length(responses_2a)),
  ~ run_perm_spearman(
      slopes_2a_wide[[.x]], slopes_2a_wide[[.y]],
      slopes_2a_wide$ID,  .x, .y, NRESAMPLE
    )
) |> list_rbind()


perm_goal2a$p_BH <- round(p.adjust(perm_goal2a$p_raw, method = "BH"), 5)
perm_goal2a$sig  <- ifelse(perm_goal2a$p_BH < 0.05, "*", "")

# Attach median r² per pathway across IDs (for reference in results table)
r2_summary_2a <- slopes_2a %>%
  group_by(pathway) %>%
  summarise(median_r2 = round(median(r2, na.rm = TRUE), 3), .groups = "drop") %>%
  mutate(response = case_match(pathway,
                               "internal" ~ "c_int",
                               "external" ~ "c_ext",
                               "CO2_flux" ~ "c_total"))

perm_goal2a <- perm_goal2a %>%
  left_join(r2_summary_2a %>% select(response, median_r2), by = "response")


print(perm_goal2a, row.names = FALSE)


# --- r² ~ spatial predictors (Goal 2a) --------------------------------------

r2_2a_wide <- slopes_2a %>%
  select(ID, pathway, r2) %>%
  pivot_wider(names_from = pathway, values_from = r2) %>%
  rename(r2_int   = internal,
         r2_ext   = external,
         r2_total = CO2_flux) %>%
  left_join(spatial_df, by = "ID")

responses_r2_2a <- c("r2_int", "r2_ext", "r2_total")

perm_r2_2a <- map2(
  rep(responses_r2_2a, each = length(predictors_2a)),
  rep(predictors_2a,   times = length(responses_r2_2a)),
  ~ run_perm_spearman(
      r2_2a_wide[[.x]], r2_2a_wide[[.y]],
      r2_2a_wide$ID, .x, .y, NRESAMPLE
    )
) |> list_rbind()

perm_r2_2a$p_BH <- round(p.adjust(perm_r2_2a$p_raw, method = "BH"), 5)
perm_r2_2a$sig  <- ifelse(perm_r2_2a$p_BH < 0.05, "*", "")

print(perm_r2_2a, row.names = FALSE)



# =============================================================================
# SECTION 4 — GOAL 2b: TEMPERATURE SENSITIVITY (m SLOPES)
# =============================================================================
#
# Response: log(flux) for each pathway (internal, external, CO2_flux).
# Preferred model: log(flux) ~ TempC + Q  — partial slope on TempC = m.
# Rationale for log: right-skewed flux distributions; m is then a proportional
# sensitivity (% change in flux per 1°C, controlling for discharge).
# Fallback: log(flux) ~ TempC if VIF > 5.
# Filter: flux > 0 required before log-transform.

# --- 4a  Safe fitting function with VIF check ---------------------------------

fit_temp_sensitivity <- function(ID_data, flux_col, vif_threshold = 5, min_n = 10) {
  d <- ID_data %>%
    filter(.data[[flux_col]] > 0, !is.na(TempC), !is.na(Q)) %>%
    mutate(log_flux = log(.data[[flux_col]])) %>%
    filter(is.finite(log_flux))

  n_used <- nrow(d)

  if (n_used < min_n) {
    return(data.frame(
      m = NA_real_, m_se = NA_real_, r2 = NA_real_,
      n_used = n_used, max_vif = NA_real_,
      model_used = "insufficient data",
      flag = paste0("n=", n_used, " < ", min_n)
    ))
  }

  # Preferred bivariate model
  mod_biv  <- lm(log_flux ~ TempC + Q, data = d)
  vif_vals <- tryCatch(vif(mod_biv), error = function(e) c(TempC = NA, Q = NA))
  max_vif  <- max(vif_vals, na.rm = TRUE)

  if (!is.na(max_vif) && max_vif > vif_threshold) {
    mod_use    <- lm(log_flux ~ TempC, data = d)
    model_used <- "simple (log_flux ~ TempC)"
    flag_str   <- paste0("VIF=", round(max_vif, 1), " > ", vif_threshold,
                         " — fell back to simple model")
  } else {
    mod_use    <- mod_biv
    model_used <- "partial (log_flux ~ TempC + Q)"
    flag_str   <- if (!is.na(max_vif)) "OK" else "VIF unavailable"
  }

  tidm  <- tidy(mod_use)
  glam  <- glance(mod_use)
  m_row <- tidm[tidm$term == "TempC", ]

  r2_flag <- if (glam$r.squared < 0.10) {
    paste0(flag_str, "; r2 < 0.10 — slope unreliable")
  } else {
    flag_str
  }

  data.frame(
    m          = m_row$estimate,
    m_se       = m_row$std.error,
    r2         = glam$r.squared,
    n_used     = n_used,
    max_vif    = round(max_vif, 2),
    model_used = model_used,
    flag       = r2_flag
  )
}

# --- 4b-d  Fit per ID per pathway ------------------------------------------

pathways_2b <- c("internal", "external", "CO2_flux")

slopes_2b <- df %>%
  group_by(ID) %>%
  group_modify(~ {
    map(pathways_2b, function(p) {
      fit_temp_sensitivity(.x, p) %>%
        mutate(pathway = p)
    }) |> list_rbind()
  }) %>%
  ungroup()

cat("--- 4b-d: Temperature sensitivity slopes per ID per pathway ---\n")
print(slopes_2b %>% select(ID, pathway, m, m_se, r2, n_used, max_vif, model_used, flag),
      row.names = FALSE)

# --- 4c  Report VIF fallbacks ------------------------------------------------

vif_fallbacks <- slopes_2b %>%
  filter(grepl("fell back", flag)) %>%
  select(ID, pathway, max_vif, flag)

if (nrow(vif_fallbacks) > 0) {
  cat("--- 4c: IDs/pathways where VIF > 5 triggered fallback to simple model ---\n")
  print(vif_fallbacks, row.names = FALSE)
  cat("For these IDs, m reflects log(flux) ~ TempC only.\n")
  cat("The slope may partly reflect seasonal discharge covariation with temperature.\n\n")
} else {
  cat("--- 4c: All IDs used bivariate model (VIF <= 5). OK.\n\n")
}

# --- 4e  Flag r² < 0.10 -------------------------------------------------------

low_r2_2b <- slopes_2b %>% filter(grepl("r2 < 0.10", flag), !is.na(m))

if (nrow(low_r2_2b) > 0) {
  cat("--- 4e: WARNING — IDs with r² < 0.10 (unreliable m slopes) ---\n")
  print(low_r2_2b %>% select(ID, pathway, r2, flag), row.names = FALSE)
  cat("\n")
} else {
  cat("--- 4e: All ID-pathway r² >= 0.10. OK.\n\n")
}


slopes_2b_wide <- slopes_2b %>%
  select(ID, pathway, m) %>%
  pivot_wider(names_from = pathway, values_from = m) %>%
  rename(m_int   = internal,
         m_ext   = external,
         m_total = CO2_flux) %>%
  left_join(spatial_df, by = "ID")


responses_2b  <- c("m_int", "m_ext", "m_total")
predictors_2b <- c("total.wetland.cover", "CV", "pH", "SpC")

perm_goal2b <- map2(
  rep(responses_2b, each = length(predictors_2b)),
  rep(predictors_2b, times = length(responses_2b)),
  ~ run_perm_spearman(
      slopes_2b_wide[[.x]], slopes_2b_wide[[.y]],
      slopes_2b_wide$ID,  .x, .y, NRESAMPLE
    )
) |> list_rbind()


perm_goal2b$p_BH <- round(p.adjust(perm_goal2b$p_raw, method = "BH"), 5)
perm_goal2b$sig  <- ifelse(perm_goal2b$p_BH < 0.05, "*", "")

r2_summary_2b <- slopes_2b %>%
  group_by(pathway) %>%
  summarise(median_r2 = round(median(r2, na.rm = TRUE), 3), .groups = "drop") %>%
  mutate(response = case_match(pathway,
                               "internal" ~ "m_int",
                               "external" ~ "m_ext",
                               "CO2_flux" ~ "m_total"))

perm_goal2b <- perm_goal2b %>%
  left_join(r2_summary_2b %>% select(response, median_r2), by = "response")

print(perm_goal2b, row.names = FALSE)


# --- r² ~ spatial predictors (Goal 2b) --------------------------------------

r2_2b_wide <- slopes_2b %>%
  select(ID, pathway, r2) %>%
  pivot_wider(names_from = pathway, values_from = r2) %>%
  rename(r2_int   = internal,
         r2_ext   = external,
         r2_total = CO2_flux) %>%
  left_join(spatial_df, by = "ID")

responses_r2_2b <- c("r2_int", "r2_ext", "r2_total")

perm_r2_2b <- map2(
  rep(responses_r2_2b, each = length(predictors_2b)),
  rep(predictors_2b,   times = length(responses_r2_2b)),
  ~ run_perm_spearman(
      r2_2b_wide[[.x]], r2_2b_wide[[.y]],
      r2_2b_wide$ID, .x, .y, NRESAMPLE
    )
) |> list_rbind()

perm_r2_2b$p_BH <- round(p.adjust(perm_r2_2b$p_raw, method = "BH"), 5)
perm_r2_2b$sig  <- ifelse(perm_r2_2b$p_BH < 0.05, "*", "")

print(perm_r2_2b, row.names = FALSE)





