
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
  list(key = "pct_internal",  label = "Internal % of Total CO2 flux",             response = "logit_pct_internal"),
  list(key = "internal_flux", label = "Internal pathway flux (C g/m²/day)",       response = "asinh_internal_pathway"),
  list(key = "external_flux", label = "External pathway flux (C g/m²/day)",       response = "asinh_external_pathway")
)


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

results <- purrr::map(response_specs, fit_one)
names(results) <- purrr::map_chr(response_specs, "key")

# ── Combined CSV: all three models' rankings stacked, with a `model` column ─
ranking_all <- purrr::map2_dfr(response_specs, results, function(spec, r) {
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

# Definition for Pseudo R2 -- attached as a footnote rather than assumed,
# since it means something specific here, not the general textbook R^2.
PSEUDO_R2_NOTE <-
  "Pseudo R² = 1 − (Σσ²_model / Σσ²_null): summed random-effects variance components (σ², one per level of the random-effects structure) in this model vs. in an intercept-only null model with the SAME random-effects structure and data. This is the Nakagawa/Cheung-style meta-analytic pseudo R² -- it measures how much of the total between-study/between-row heterogeneity the fixed effects explain, NOT an OLS R² (there is no residual/error variance being partitioned)."

add_stat_footnote <- function(ft, lines) {
  ft %>%
    add_footer_lines(values = lines) %>%
    fontsize(size = 7, part = "footer") %>%
    italic(part = "footer") %>%
    align(align = "left", part = "footer")
}

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
  line1 = c("", rep(purrr::map_chr(response_specs, "label"), each = 3)),
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
tbl_b_data <- purrr::map2_dfr(response_specs, results, function(spec, r) {
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
  width(j = 2:ncol(tbl_b_data), width = 0.9) %>%
  add_stat_footnote(PSEUDO_R2_NOTE)

tables_out_path <- "05_Figures/Table_metaanalysis_mlm_coefficient_ranking.docx"
save_as_docx(ft_a, ft_b, path = tables_out_path)
cat("\nPublication tables (coefficient ranking across all 3 models, general model statistics) written to", tables_out_path, "\n")
