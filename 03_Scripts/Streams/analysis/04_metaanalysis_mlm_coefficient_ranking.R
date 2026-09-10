
# ── Coefficient-ranking alternative to the backward-elimination models ─────
# Simpler counterpart to 03_metaanalysis_mlm_backwards_elimination.R: instead
# of eliminating predictors step by step (AIC/LRT-driven), fit each full
# 4-predictor model ONCE and rank predictors directly by standardized
# coefficient size, judging "statistically real" via each predictor's own
# individual p-value (not QM, not an LRT, not AIC comparison). Meant to run
# side by side with that script for comparison -- this script does not touch
# or depend on it.
#
# Three response variables are modeled here, same predictor set / weights /
# random-effects structure throughout -- only the outcome changes:
#   - logit_pct_internal:     internal/external SPLIT (% of flux internal)
#   - asinh_internal_pathway: internal pathway flux MAGNITUDE
#   - asinh_external_pathway: external pathway flux MAGNITUDE
# See 01_metaanalysis_mlm_dataprep.R for both transforms.

source("03_Scripts/Streams/analysis/01_metaanalysis_mlm_dataprep.R")
library(metafor)
library(flextable)
library(officer)

predictor_labels <- c(
  z_Temperature_C = "Temperature (z)",
  z_pH = "pH (z)",
  z_Mean_Annual_Precipitation_cm_yr = "Mean annual precipitation (z)",
  z_Discharge_m3s = "Discharge (z)"
)
all_predictors <- names(predictor_labels)

response_specs <- list(
  list(key = "pct_internal",  label = "Internal % of flux (split)", response = "logit_pct_internal"),
  list(key = "internal_flux", label = "Internal pathway flux",      response = "asinh_internal_pathway"),
  list(key = "external_flux", label = "External pathway flux",      response = "asinh_external_pathway")
)

# Fits one model for one response variable: full 4-predictor model + a
# matching intercept-only null model (both REML, both on the same
# complete-case subset), then ranks predictors by |standardized estimate|.
#
# NOTE: magnitude and significance are different things. A predictor's
# p-value depends on its estimate size RELATIVE TO its own standard error,
# not on magnitude alone -- a large coefficient with a large SE can still be
# non-significant, and a small coefficient with a small SE can still be
# significant. Ranking by abs_estimate answers "which predictor moves the
# outcome most per SD", not "which predictor do we trust most" -- don't read
# "biggest" as "most trustworthy".
#
# Temperature_C and Mean_Annual_Precipitation_cm_yr are correlated at r=0.40
# (Script 1 correlation check -- not a VIF-flagged problem, but non-trivial).
# In each joint model below, that correlation can modestly shrink or inflate
# either predictor's apparent significance relative to testing it alone.
# Expected multiple-regression behavior, not a bug -- flagged here so it
# isn't misread as something wrong with the rankings below.
fit_one <- function(spec) {
  # Complete-case subset for THIS response -- each response has its own
  # missingness (e.g. asinh_external_pathway adds 1 NA beyond the 4
  # predictors' own), matching how scripts 03/04/05 each build model_data.
  model_data <- meta %>%
    filter(if_all(all_of(c(all_predictors, spec$response, "n_reaches",
                            "Citation", "Row_ID", "Biome_collapsed")),
                  ~!is.na(.)))

  cat("\n\n=================== ", spec$label, " (REML) ===================\n", sep = "")
  cat("Modeling dataset: n =", nrow(model_data), "of", nrow(meta), "rows.\n")

  model <- rma.mv(
    yi = model_data[[spec$response]],
    V = 0,
    W = model_data$n_reaches,
    mods = reformulate(all_predictors),
    random = ~ 1 | Citation/Row_ID,
    data = model_data,
    method = "REML"
  )
  print(summary(model))

  null_model <- rma.mv(
    yi = model_data[[spec$response]],
    V = 0,
    W = model_data$n_reaches,
    mods = ~1,
    random = ~ 1 | Citation/Row_ID,
    data = model_data,
    method = "REML"
  )
  pseudo_r2 <- 1 - (sum(model$sigma2) / sum(null_model$sigma2))

  coef_tab <- coef(summary(model))
  coef_tab <- coef_tab[rownames(coef_tab) != "intrcpt", , drop = FALSE]

  ranking <- tibble(
    term = rownames(coef_tab),
    predictor = unname(predictor_labels[rownames(coef_tab)]),
    estimate = coef_tab[, "estimate"],
    se = coef_tab[, "se"],
    z = coef_tab[, "zval"],
    p = coef_tab[, "pval"],
    abs_estimate = abs(coef_tab[, "estimate"])
  ) %>%
    arrange(desc(abs_estimate)) %>%
    mutate(rank = row_number(), significant = p < 0.05, pseudo_r2 = round(pseudo_r2, 4))

  cat("\n=== Predictors ranked by standardized coefficient magnitude:", spec$label, "===\n")
  print(ranking, n = Inf, width = Inf)

  top <- ranking[1, ]
  if (top$significant) {
    cat(sprintf("\nLargest standardized effect: %s (estimate = %.3f, p = %.3f) -- also individually significant.\n",
                top$predictor, top$estimate, top$p))
  } else {
    cat(sprintf("\nLargest standardized effect: %s (estimate = %.3f, p = %.3f) -- NOT individually significant, despite being the largest in magnitude.\n",
                top$predictor, top$estimate, top$p))
  }
  cat(sprintf("Pseudo R² (%s, REML): %.4f\n", spec$label, pseudo_r2))

  list(spec = spec, model = model, ranking = ranking, pseudo_r2 = pseudo_r2)
}

results <- map(response_specs, fit_one)
names(results) <- map_chr(response_specs, "key")

# ── Combined CSV: all three models' rankings stacked, with a `model` column ─
ranking_all <- map2_dfr(response_specs, results, function(spec, r) {
  r$ranking %>% mutate(model = spec$label, .before = 1)
})

out_path <- "04_Output/metaanalysis_mlm_coefficient_ranking.csv"
write_csv(ranking_all, out_path)
cat("\nCoefficient ranking table (all 3 models) written to", out_path, "\n")

# =============================================================================
# Publication tables: coefficients across all 3 models side by side, plus
# each model's general fit statistics
# =============================================================================
base_ft_style <- function(ft) {
  ft %>%
    font(fontname = "Aptos", part = "all") %>%
    fontsize(size = 9, part = "all") %>%
    bold(part = "header") %>%
    border_remove() %>%
    hline_top(part = "header",    border = fp_border(width = 2)) %>%
    hline_bottom(part = "header", border = fp_border(width = 1)) %>%
    hline_bottom(part = "body",   border = fp_border(width = 2)) %>%
    height_all(height = 0.25)
}
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
sig_stars <- function(p) case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.1 ~ ".", TRUE ~ "")

# ── Table A: ranked coefficients for all 3 models, one flextable ───────────
# Rows are in a fixed predictor order (each model's own within-model rank is
# shown as its "Rank" column, since the three models don't necessarily agree
# on ordering).
wide_data <- tibble(term = all_predictors, Predictor = unname(predictor_labels[all_predictors]))
for (spec in response_specs) {
  r <- results[[spec$key]]$ranking %>%
    transmute(term, rank, Slope = sprintf("%.3f%s", estimate, sig_stars(p)), p = fmt_p(p))
  names(r)[names(r) != "term"] <- paste0(spec$key, "__", names(r)[names(r) != "term"])
  wide_data <- left_join(wide_data, r, by = "term")
}
wide_data <- wide_data %>% select(-term)

header_df <- tibble(
  col_keys = names(wide_data),
  line1 = c("", rep(map_chr(response_specs, "label"), each = 3)),
  line2 = c("Predictor", rep(c("Rank", "Slope", "p"), times = length(response_specs)))
)

ft_a <- flextable(wide_data, col_keys = header_df$col_keys) %>%
  set_header_df(mapping = header_df, key = "col_keys") %>%
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  base_ft_style() %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  width(j = 1, width = 2.2) %>%
  width(j = 2:ncol(wide_data), width = 0.65)

# ── Table B: general model statistics, one row per model ───────────────────
tbl_b_data <- map2_dfr(response_specs, results, function(spec, r) {
  fs <- fitstats(r$model)
  tibble(
    Model = spec$label,
    k = r$model$k,
    `Var: Citation` = sprintf("%.4f", r$model$sigma2[1]),
    `Var: Citation/Row_ID` = sprintf("%.4f", r$model$sigma2[2]),
    `log-Lik` = sprintf("%.3f", as.numeric(fs["logLik", 1])),
    AIC = sprintf("%.3f", as.numeric(fs["AIC", 1])),
    AICc = sprintf("%.3f", as.numeric(fs["AICc", 1])),
    BIC = sprintf("%.3f", as.numeric(fs["BIC", 1])),
    `QM (df)` = paste0(sprintf("%.2f", r$model$QM), " (", r$model$QMdf[1], ")"),
    `QM p` = fmt_p(r$model$QMp),
    `Pseudo R2` = sprintf("%.4f", r$pseudo_r2)
  )
})

ft_b <- flextable(tbl_b_data) %>%
  base_ft_style() %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:ncol(tbl_b_data), align = "center", part = "all") %>%
  width(j = 1, width = 2.2) %>%
  width(j = 2:ncol(tbl_b_data), width = 0.9)

tables_out_path <- "05_Figures/Table_metaanalysis_mlm_coefficient_ranking.docx"
save_as_docx(ft_a, ft_b, path = tables_out_path)
cat("\nPublication tables (coefficient ranking across all 3 models, general model statistics) written to", tables_out_path, "\n")
