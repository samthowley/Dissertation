
source("03_Scripts/Streams/analysis/data for analysis.R")


df <- int.ext %>%
  mutate(ID = factor(ID))

# Convert wetland cover fraction to percentage for interpretable plot axes
spatial_df <- spatial_df %>%
  mutate(
    ID                  = factor(ID),
    total.wetland.cover = total.wetland.cover * 100
  )

# Verify join key alignment
stopifnot("All site IDs in df must appear in spatial_df" =
  all(as.character(unique(df$ID)) %in% as.character(spatial_df$ID)))

cat("Data loaded.\n")
cat("  Hourly observations:", nrow(df), "\n")
cat("  IDs in df:        ", nlevels(df$ID), "\n")
cat("  IDs in spatial_df:", nrow(spatial_df), "\n\n")


# =============================================================================
# SECTION 1 — PRE-ANALYSIS DIAGNOSTICS
# =============================================================================

cat("=============================================================\n")
cat("SECTION 1 — PRE-ANALYSIS DIAGNOSTICS\n")
cat("=============================================================\n\n")

# --- 1a  Count negative / zero flux values per ID --------------------------

diag_counts <- df %>%
  group_by(ID) %>%
  summarise(
    n_total          = n(),
    n_int_le0        = sum(internal  <= 0, na.rm = TRUE),
    n_ext_le0        = sum(external  <= 0, na.rm = TRUE),
    n_total_le0      = sum(CO2_flux  <= 0, na.rm = TRUE),
    pct_int_le0      = round(100 * n_int_le0   / n_total, 1),
    pct_ext_le0      = round(100 * n_ext_le0   / n_total, 1),
    pct_total_le0    = round(100 * n_total_le0 / n_total, 1),
    .groups = "drop"
  )

cat("--- 1a: Negative/zero flux values per ID ---\n")
print(diag_counts)
cat("\nNOTE: external = CO2_flux - internal (derived residual, not independently measured).\n")
cat("      Negative external occurs when the metabolism model overestimates internal.\n")
cat("      These rows are excluded from all log-transformations.\n\n")

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

cat("--- 1b: Pairwise Spearman correlations among spatial predictors ---\n")
print(corr_pred_pairs, row.names = FALSE)

high_corr_pairs <- corr_pred_pairs %>% filter(abs(rho) > 0.7)
if (nrow(high_corr_pairs) > 0) {
  cat("\nFLAG: |rho| > 0.7 for the following pairs — interpret Stage 2 results cautiously:\n")
  print(high_corr_pairs, row.names = FALSE)
} else {
  cat("\nAll predictor pairs: |rho| <= 0.7. Collinearity OK.\n")
}
cat("\n")

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

cat("--- 1c: Spearman rho(TempC, Q) within each ID ---\n")
print(corr_TQ_ID)
cat("\nIDs with |rho| > 0.7 indicate seasonal T-Q confounding.\n")
cat("For those IDs, VIF > 5 in the bivariate model is expected;\n")
cat("the script will fall back to log(flux) ~ TempC only and flag them.\n\n")

# --- 1d  Diagnostic summary flags --------------------------------------------

cat("--- 1d: Diagnostic summary flags ---\n")

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
cat("\n")


# =============================================================================
# SECTION 2 — GOAL 1: MEAN LOG(internal / external) RATIO PER ID
# =============================================================================

cat("=============================================================\n")
cat("SECTION 2 — GOAL 1: PATHWAY PROMINENCE (MEAN LOG RATIO)\n")
cat("=============================================================\n\n")

# --- 2a-c  Compute mean log ratio per ID ------------------------------------
# Filter to rows where BOTH internal > 0 AND external > 0 before log-transforming.
# Take mean of per-row log-ratios (not log of the mean ratio).

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

cat("--- 2c: Mean log(internal / external) per ID ---\n")
cat("  Positive = internal dominance; negative = external dominance; 0 = parity\n")
print(log_ratio_ID)
cat("\n")

# --- 2d  Join with spatial predictors ----------------------------------------

goal1_df <- log_ratio_ID %>%
  left_join(spatial_df, by = "ID")

# --- 2e-f  Permutation Spearman tests (coin) ----------------------------------

# Helper: run one permutation Spearman test and return a tidy row.
# coin does not return rho directly — computed separately and reported alongside p.
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

NRESAMPLE <- 9999 # set to 9999 for quick dev runs; restore to 99999 for final results

predictors_goal1 <- c("total.wetland.cover", "CV", "pH", "SpC")

perm_goal1 <- map(predictors_goal1, function(pred) {
  run_perm_spearman(goal1_df$mean_log_ratio, goal1_df[[pred]],
                    goal1_df$ID, "mean_log_ratio", pred, NRESAMPLE)
}) |> list_rbind()

# --- 2g  BH correction (2 tests within Goal 1) --------------------------------

perm_goal1$p_BH <- round(p.adjust(perm_goal1$p_raw, method = "BH"), 5)
perm_goal1$sig  <- ifelse(perm_goal1$p_BH < 0.05, "*", "")

# --- 2h  Print results --------------------------------------------------------

cat("--- 2h: Goal 1 Spearman results (BH-corrected across 2 tests) ---\n")
print(perm_goal1, row.names = FALSE)
cat("\nNote: power at n=9 requires |rho| >= ~0.68 for p < 0.05 (two-tailed).\n")
cat("Do not over-interpret non-significant results with moderate rho.\n\n")

# --- 2i  Scatter plots --------------------------------------------------------

pred_labels_goal1 <- c(
  total.wetland.cover = "Total wetland cover (%)",
  CV                  = "Discharge CV",
  pH                  = "Mean pH",
  SpC                 = "Mean specific conductance (µS/cm)"
)

walk(predictors_goal1, function(pred) {
  row  <- perm_goal1[perm_goal1$predictor == pred, ]
  p <- ggplot(goal1_df, aes(x = .data[[pred]], y = mean_log_ratio)) +
    geom_point(size = 3) +
    geom_text_repel(aes(label = ID), size = 3.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title    = paste("Goal 1: Pathway prominence vs.", pred),
      subtitle = paste0("Spearman rho = ", row$rho, ", p(BH) = ", row$p_BH),
      x        = pred_labels_goal1[pred],
      y        = "Mean log(internal / external)",
      caption  = "Positive = internal dominance; negative = external dominance"
    ) +
    theme_bw()
  print(p)
})


# =============================================================================
# SECTION 3 — GOAL 2a: DISCHARGE SENSITIVITY (c SLOPES)
# =============================================================================

cat("=============================================================\n")
cat("SECTION 3 — GOAL 2a: DISCHARGE SENSITIVITY (c = log-log slope)\n")
cat("=============================================================\n\n")

# --- 3a  Safe log-log fitting function ----------------------------------------
# Returns NA row (not an error) if flux or Q <= 0 leaves n < min_n.

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
    return(data.frame(slope = NA_real_, slope_se = NA_real_,
                      r2    = NA_real_, n_used   = n_used,
                      flag  = paste0("n=", n_used, " < ", min_n, " (insufficient)")))
  }

  mod  <- lm(log_flux ~ log_Q, data = d)
  tidm <- tidy(mod)
  glam <- glance(mod)

  data.frame(
    slope    = tidm$estimate[tidm$term == "log_Q"],
    slope_se = tidm$std.error[tidm$term == "log_Q"],
    r2       = glam$r.squared,
    n_used   = n_used,
    flag     = ifelse(glam$r.squared < 0.10,
                      "r2 < 0.10 — slope estimate unreliable", "OK")
  )
}

# --- 3b  Fit per ID per pathway ---------------------------------------------

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

cat("--- 3b: Discharge sensitivity slopes per ID per pathway ---\n")
print(slopes_2a %>% select(ID, pathway, slope, slope_se, r2, n_used, flag),
      row.names = FALSE)
cat("\n")

# --- 3c  Flag IDs with r² < 0.10 -------------------------------------------

low_r2_2a <- slopes_2a %>% filter(flag != "OK", !is.na(slope))

if (nrow(low_r2_2a) > 0) {
  cat("--- 3c: WARNING — IDs with r² < 0.10 (unreliable slopes) ---\n")
  print(low_r2_2a %>% select(ID, pathway, r2, flag), row.names = FALSE)
  cat("These ID-pathway combinations are retained in Stage 2 but flagged.\n")
  cat("Interpret Stage 2 Spearman results cautiously when flagged slopes are included.\n\n")
} else {
  cat("--- 3c: All ID-pathway r² >= 0.10. OK.\n\n")
}

# --- 3d  Pivot wide and join spatial predictors --------------------------------

slopes_2a_wide <- slopes_2a %>%
  select(ID, pathway, slope) %>%
  pivot_wider(names_from = pathway, values_from = slope) %>%
  rename(c_int   = internal,
         c_ext   = external,
         c_total = CO2_flux) %>%
  left_join(spatial_df, by = "ID")

# --- 3e  6 permutation Spearman tests -----------------------------------------

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

# --- 3f  BH correction across 6 tests ----------------------------------------

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

# --- 3g  Print results --------------------------------------------------------

cat("--- 3g: Goal 2a Spearman results (BH-corrected across 6 tests) ---\n")
print(perm_goal2a, row.names = FALSE)
cat("\n")

# --- 3h  Plots ----------------------------------------------------------------

slopes_plot_2a <- slopes_2a %>%
  filter(!is.na(slope)) %>%
  mutate(pathway = factor(pathway,
                          levels = c("internal", "external", "CO2_flux"),
                          labels = c("internal", "external", "Total")))

plot_c_slopes <- ggplot(slopes_plot_2a,
                        aes(x = ID, y = slope, color = pathway, shape = pathway)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = slope - slope_se, ymax = slope + slope_se),
                width = 0.15, position = position_dodge(width = 0.4)) +
  labs(
    title   = "Goal 2a: Discharge sensitivity (c) per ID per pathway",
    x       = "ID",
    y       = "Slope c  [log(flux) ~ log(Q, L/s)]",
    color   = "Pathway",
    shape   = "Pathway",
    caption = "Error bars = ±1 SE.  Dashed line = c of 0."
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_c_slopes)

pred_labels <- c(
  total.wetland.cover = "Total wetland cover (%)",
  CV                  = "Discharge CV",
  pH                  = "Mean pH",
  SpC                 = "Mean specific conductance (µS/cm)"
)

walk(predictors_2a, function(pred) {
  walk(responses_2a, function(resp) {
    row <- perm_goal2a[perm_goal2a$response == resp & perm_goal2a$predictor == pred, ]
    p <- ggplot(slopes_2a_wide, aes(x = .data[[pred]], y = .data[[resp]])) +
      geom_point(size = 3) +
      geom_text_repel(aes(label = ID), size = 3.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        title    = paste("Goal 2a:", resp, "vs.", pred),
        subtitle = paste0("Spearman rho = ", row$rho, ", p(BH) = ", row$p_BH),
        x        = pred_labels[pred],
        y        = paste0(resp, "  [log(flux) ~ log(Q)]")
      ) +
      theme_bw()
    print(p)
  })
})


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

cat("=============================================================\n")
cat("SECTION 4 — GOAL 2b: TEMPERATURE SENSITIVITY (m = partial slope on TempC)\n")
cat("=============================================================\n\n")

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
cat("\n")

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

# --- 4f  Pivot wide and join spatial predictors --------------------------------

slopes_2b_wide <- slopes_2b %>%
  select(ID, pathway, m) %>%
  pivot_wider(names_from = pathway, values_from = m) %>%
  rename(m_int   = internal,
         m_ext   = external,
         m_total = CO2_flux) %>%
  left_join(spatial_df, by = "ID")

# --- 4g  6 permutation Spearman tests -----------------------------------------

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

# --- 4h  BH correction across 6 tests ----------------------------------------

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

# --- 4i  Print results --------------------------------------------------------

cat("--- 4i: Goal 2b Spearman results (BH-corrected across 6 tests) ---\n")
print(perm_goal2b, row.names = FALSE)
cat("\n")

# --- 4j  Plots ----------------------------------------------------------------

slopes_plot_2b <- slopes_2b %>%
  filter(!is.na(m)) %>%
  mutate(pathway = factor(pathway,
                          levels = c("internal", "external", "CO2_flux"),
                          labels = c("internal", "external", "Total")))

plot_m_slopes <- ggplot(slopes_plot_2b,
                        aes(x = ID, y = m, color = pathway, shape = pathway)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = m - m_se, ymax = m + m_se),
                width = 0.15, position = position_dodge(width = 0.4)) +
  labs(
    title   = "Goal 2b: Temperature sensitivity (m) per ID per pathway",
    x       = "ID",
    y       = "Slope m  [partial slope of log(flux) ~ TempC, controlling for Q (L/s)]",
    color   = "Pathway",
    shape   = "Pathway",
    caption = "Error bars = ±1 SE.  See flag column for IDs using simple vs. partial model."
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_m_slopes)

walk(predictors_2b, function(pred) {
  walk(responses_2b, function(resp) {
    row <- perm_goal2b[perm_goal2b$response == resp & perm_goal2b$predictor == pred, ]
    p <- ggplot(slopes_2b_wide, aes(x = .data[[pred]], y = .data[[resp]])) +
      geom_point(size = 3) +
      geom_text_repel(aes(label = ID), size = 3.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        title    = paste("Goal 2b:", resp, "vs.", pred),
        subtitle = paste0("Spearman rho = ", row$rho, ", p(BH) = ", row$p_BH),
        x        = pred_labels[pred],
        y        = paste0(resp, "  [partial slope of log(flux) ~ TempC]")
      ) +
      theme_bw()
    print(p)
  })
})


# =============================================================================
# SECTION 5 — SUMMARY OUTPUT
# =============================================================================

cat("=============================================================\n")
cat("SECTION 5 — COMBINED RESULTS SUMMARY\n")
cat("=============================================================\n\n")

# --- 5a  Combined table: all rho, raw p, BH-adjusted p -----------------------

summary_table <- bind_rows(
  perm_goal1  %>% mutate(goal = "Goal 1: log-ratio"),
  perm_goal2a %>% mutate(goal = "Goal 2a: c slopes"),
  perm_goal2b %>% mutate(goal = "Goal 2b: m slopes")
) %>%
  select(goal, response, predictor, rho, p_raw, p_BH, sig, n) %>%
  arrange(goal, predictor, response)

cat("--- Combined Spearman results (BH correction applied within each Goal) ---\n")
cat("  sig (*) = p_BH < 0.05\n\n")
print(summary_table, row.names = FALSE)

cat("\n--- Methodological notes for manuscript ---\n")
cat("  1. external = CO2_flux - internal (derived residual; not independently measured).\n")
cat("     internal and external are mathematically coupled — always interpret CO2_flux\n")
cat("     (total) as an independent check on directional patterns.\n")
cat("  2. Goal 2b response: log(flux) ~ TempC + Q. Slope m is a proportional\n")
cat("     sensitivity (ln-scale change per 1°C), controlling for discharge.\n")
cat("  3. All Stage 2 tests: n = 9. Findings are exploratory/hypothesis-generating.\n")
cat("     Power requires |rho| >= ~0.68 for p < 0.05 (two-tailed, permutation).\n")
cat("  4. BH correction: Benjamini & Hochberg (1995), J. Royal Stat. Soc. B, 57(1), 289-300.\n")
cat("  5. Permutation Spearman is distribution-free; appropriate for small n.\n")
cat("  6. IDs with r² < 0.10 in Stage 1 have unreliable slope estimates.\n")
cat("     These are flagged above — note them in the Results section.\n\n")

# --- 5b  Session info ---------------------------------------------------------

cat("--- Session info ---\n")
print(sessionInfo())


# =============================================================================
# SECTION 6 — ADVISOR TABLES (printed to console via knitr::kable)
# =============================================================================

library(knitr)

cat("=============================================================\n")
cat("SECTION 6 — ADVISOR TABLES\n")
cat("=============================================================\n\n")

print_table <- function(title, note, data) {
  cat(paste0("\n", strrep("-", 70), "\n"))
  cat(paste0(title, "\n"))
  if (!is.null(note)) cat(paste0("Note: ", note, "\n"))
  cat(strrep("-", 70), "\n")
  print(kable(data, format = "simple", na = "—"))
  cat("\n")
}

# ------------------------------------------------------------------
# TABLE 1 — Site overview: spatial predictors + pathway dominance
# ------------------------------------------------------------------

tbl1 <- spatial_df %>%
  select(ID, total.wetland.cover, CV, pH, SpC) %>%
  left_join(
    log_ratio_ID %>% select(ID, n_valid_logratio, mean_log_ratio, sd_log_ratio),
    by = "ID"
  ) %>%
  mutate(
    dominance          = case_when(
      mean_log_ratio >  0.5 ~ "Internal",
      mean_log_ratio < -0.5 ~ "External",
      TRUE                  ~ "Near parity"
    ),
    total.wetland.cover = round(total.wetland.cover, 1),
    CV                  = round(CV, 2),
    pH                  = round(pH, 2),
    SpC                 = round(SpC, 1),
    mean_log_ratio      = round(mean_log_ratio, 3),
    sd_log_ratio        = round(sd_log_ratio, 3)
  ) %>%
  rename(
    Site                        = ID,
    `Wetland cover (%)`         = total.wetland.cover,
    `Discharge CV`              = CV,
    `Mean pH`                   = pH,
    `Mean SpC (µS/cm)`          = SpC,
    `n (log-ratio)`             = n_valid_logratio,
    `Mean log(int/ext)`         = mean_log_ratio,
    `SD log(int/ext)`           = sd_log_ratio,
    `Pathway dominance`         = dominance
  ) %>%
  arrange(Site)

print_table(
  "TABLE 1 — Site Overview: Spatial Predictors & Pathway Dominance",
  "Positive mean log(int/ext) = internal dominance; negative = external dominance.",
  tbl1
)

# ------------------------------------------------------------------
# TABLE 2 — Data quality: negative/zero flux counts
# ------------------------------------------------------------------

tbl2 <- diag_counts %>%
  select(ID, n_total, n_int_le0, pct_int_le0, n_ext_le0, pct_ext_le0,
         n_total_le0, pct_total_le0) %>%
  rename(
    Site                      = ID,
    `N observations`          = n_total,
    `N internal ≤ 0`          = n_int_le0,
    `% internal ≤ 0`          = pct_int_le0,
    `N external ≤ 0`          = n_ext_le0,
    `% external ≤ 0`          = pct_ext_le0,
    `N total ≤ 0`             = n_total_le0,
    `% total ≤ 0`             = pct_total_le0
  ) %>%
  arrange(Site)

print_table(
  "TABLE 2 — Data Quality: Negative/Zero Flux Counts per Site",
  "Rows with flux <= 0 excluded from all log-transformations. external = CO2_flux - internal (derived residual).",
  tbl2
)

# ------------------------------------------------------------------
# TABLE 3 — Predictor collinearity (Section 1b)
# ------------------------------------------------------------------

tbl3 <- corr_pred_pairs %>%
  rename(
    `Predictor 1` = predictor_1,
    `Predictor 2` = predictor_2,
    `Spearman rho` = rho,
    `p-value`      = p
  ) %>%
  mutate(`Collinearity flag` = ifelse(abs(`Spearman rho`) > 0.7, "HIGH", "OK"))

print_table(
  "TABLE 3 — Predictor Collinearity: Pairwise Spearman Correlations",
  "|rho| > 0.7 would indicate potentially redundant predictors.",
  tbl3
)

# ------------------------------------------------------------------
# TABLE 4 — Discharge sensitivity slopes (Goal 2a), wide per site
# ------------------------------------------------------------------

tbl4_slopes <- slopes_2a %>%
  mutate(
    across(c(slope, slope_se, r2), ~ round(.x, 3)),
    pathway = case_match(pathway,
                        "internal" ~ "Internal", "external" ~ "External", "CO2_flux" ~ "Total")
  ) %>%
  select(ID, pathway, slope, slope_se, r2, n_used, flag) %>%
  rename(
    Site     = ID,
    Pathway  = pathway,
    `c (slope)` = slope,
    `SE`        = slope_se,
    `r²`        = r2,
    `n`         = n_used,
    `Flag`      = flag
  ) %>%
  arrange(Site, Pathway)

print_table(
  "TABLE 4 — Discharge Sensitivity (c) Slopes per Site per Pathway",
  "Model: log(flux) ~ log(Q). FLAG rows have r2 < 0.10 — slope estimates unreliable.",
  tbl4_slopes
)

# ------------------------------------------------------------------
# TABLE 5 — Temperature sensitivity slopes (Goal 2b), wide per site
# ------------------------------------------------------------------

tbl5_slopes <- slopes_2b %>%
  mutate(
    across(c(m, m_se, r2, max_vif), ~ round(.x, 3)),
    pathway = case_match(pathway,
                        "internal" ~ "Internal", "external" ~ "External", "CO2_flux" ~ "Total")
  ) %>%
  select(ID, pathway, m, m_se, r2, n_used, max_vif, model_used, flag) %>%
  rename(
    Site         = ID,
    Pathway      = pathway,
    `m (slope)`  = m,
    `SE`         = m_se,
    `r²`         = r2,
    `n`          = n_used,
    `Max VIF`    = max_vif,
    `Model`      = model_used,
    `Flag`       = flag
  ) %>%
  arrange(Site, Pathway)

print_table(
  "TABLE 5 — Temperature Sensitivity (m) Slopes per Site per Pathway",
  "Preferred model: log(flux) ~ TempC + Q (partial slope on TempC = m). FLAG rows have r2 < 0.10.",
  tbl5_slopes
)

# ------------------------------------------------------------------
# TABLE 6 — Goal 1 Spearman results
# ------------------------------------------------------------------

tbl6 <- perm_goal1 %>%
  mutate(
    rho   = round(rho, 3),
    p_raw = round(p_raw, 4),
    p_BH  = round(p_BH, 4),
    sig   = ifelse(p_BH < 0.05, "*", "")
  ) %>%
  select(predictor, rho, p_raw, p_BH, sig, n) %>%
  rename(
    Predictor      = predictor,
    `Spearman rho` = rho,
    `p (raw)`      = p_raw,
    `p (BH-adj)`   = p_BH,
    `Sig.`         = sig,
    `n sites`      = n
  ) %>%
  arrange(desc(abs(`Spearman rho`)))

print_table(
  "TABLE 6 — Goal 1 Spearman Results: Pathway Dominance ~ Spatial Predictors",
  "Response: mean log(internal/external) per site. BH correction across 4 tests. Sig. (*) = p(BH) < 0.05.",
  tbl6
)

# ------------------------------------------------------------------
# TABLE 7 — Goal 2a Spearman results
# ------------------------------------------------------------------

tbl7 <- perm_goal2a %>%
  mutate(
    rho       = round(rho, 3),
    p_raw     = round(p_raw, 4),
    p_BH      = round(p_BH, 4),
    median_r2 = round(median_r2, 3),
    sig       = ifelse(p_BH < 0.05, "*", ""),
    response  = case_match(response,
                          "c_int"   ~ "Internal (c_int)",
                          "c_ext"   ~ "External (c_ext)",
                          "c_total" ~ "Total (c_total)")
  ) %>%
  select(response, predictor, rho, p_raw, p_BH, sig, median_r2, n) %>%
  rename(
    `Response (pathway)` = response,
    Predictor            = predictor,
    `Spearman rho`       = rho,
    `p (raw)`            = p_raw,
    `p (BH-adj)`         = p_BH,
    `Sig.`               = sig,
    `Median r² (Stage 1)` = median_r2,
    `n sites`            = n
  ) %>%
  arrange(`Response (pathway)`, desc(abs(`Spearman rho`)))

print_table(
  "TABLE 7 — Goal 2a Spearman Results: Discharge Sensitivity (c) ~ Spatial Predictors",
  "BH correction across 12 tests (3 pathways x 4 predictors). Median r2 = Stage 1 fit quality. Sig. (*) = p(BH) < 0.05.",
  tbl7
)

# ------------------------------------------------------------------
# TABLE 8 — Goal 2b Spearman results
# ------------------------------------------------------------------

tbl8 <- perm_goal2b %>%
  mutate(
    rho       = round(rho, 3),
    p_raw     = round(p_raw, 4),
    p_BH      = round(p_BH, 4),
    median_r2 = round(median_r2, 3),
    sig       = ifelse(p_BH < 0.05, "*", ""),
    response  = case_match(response,
                          "m_int"   ~ "Internal (m_int)",
                          "m_ext"   ~ "External (m_ext)",
                          "m_total" ~ "Total (m_total)")
  ) %>%
  select(response, predictor, rho, p_raw, p_BH, sig, median_r2, n) %>%
  rename(
    `Response (pathway)` = response,
    Predictor            = predictor,
    `Spearman rho`       = rho,
    `p (raw)`            = p_raw,
    `p (BH-adj)`         = p_BH,
    `Sig.`               = sig,
    `Median r² (Stage 1)` = median_r2,
    `n sites`            = n
  ) %>%
  arrange(`Response (pathway)`, desc(abs(`Spearman rho`)))

print_table(
  "TABLE 8 — Goal 2b Spearman Results: Temperature Sensitivity (m) ~ Spatial Predictors",
  "BH correction across 12 tests (3 pathways x 4 predictors). Median r2 = Stage 1 fit quality. Sig. (*) = p(BH) < 0.05.",
  tbl8
)
