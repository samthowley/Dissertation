
# ── Multilevel meta-regression: what predicts the internal/external CO2 split? ──
# Response: logit_pct_internal = car::logit(Internal_Pct_of_Flux / 100), i.e.
# log(p / (1-p)) where p = the fraction of total CO2 flux from the INTERNAL
# (in-stream metabolic) pathway. Unitless (log-odds). Sign convention:
# POSITIVE values / coefficients = more internal-pathway-dominated (p > 0.5);
# NEGATIVE values / coefficients = more external-pathway-dominated (p < 0.5);
# 0 = internal and external contribute equally. See metaanalysis_mlm_dataprep.R
# for exactly how the 0/100-boundary rows are handled in this transform.
#
# See metaanalysis_mlm_prelim_checks.R for the assumption checks this model
# specification was validated against before fitting anything here.

source("03_Scripts/Streams/analysis/01_metaanalysis_mlm_dataprep.R")
library(metafor)

predictor_labels <- c(
  z_Temperature_C = "Temperature (z)",
  z_abs_latitude = "abs(Latitude) (z)",
  z_pH = "pH (z)",
  z_Mean_Annual_Precipitation_cm_yr = "Mean annual precipitation (z)",
  z_Discharge_m3s = "Discharge (z)"
)
all_predictors <- names(predictor_labels)

# All backward-elimination models below are fit on ONE fixed complete-case
# subset (rows with no missing value across any of the 5 candidate predictors,
# the response, or n_reaches), not each reduced model's own available-case
# rows. Predictors differ in missingness (Discharge_m3s: 3 NA, Temperature_C:
# 3 NA, pH: 1 NA, Mean_Annual_Precipitation_cm_yr: 10 NA); refitting each
# reduced model on its own available cases would silently change n as
# predictors drop out, which would invalidate the AIC/LRT comparisons between
# steps.
model_data <- meta %>%
  filter(if_all(all_of(c(all_predictors, "logit_pct_internal", "n_reaches",
                          "Citation", "Row_ID", "Biome_collapsed")),
                ~!is.na(.)))

cat("Modeling dataset: n =", nrow(model_data), "of", nrow(meta),
    "rows (complete cases across all 5 candidate predictors + response + n_reaches).\n")

# rows are weighted by the number of streams contributing to that estimate (n_reaches).
# V = 0: no per-row sampling variance is available from the extraction (these
# are point estimates from the literature, not effect sizes with reported
# SEs), so all row-to-row variance is attributed to the random-effects
# structure below rather than split between within- and between-row variance.
fit_rma <- function(terms_included, data = model_data, random, method = "ML") {
  form <- if (length(terms_included) == 0) ~1 else reformulate(terms_included)
  rma.mv(
    yi = logit_pct_internal,
    V = 0,
    W = n_reaches,
    mods = form,
    random = random,
    data = data,
    method = method
  )
}

model_row <- function(model, step, terms_included, dropped_term = NA_character_, status = "accepted") {
  fs <- fitstats(model)
  tibble(
    step = step,
    status = status,
    predictors = if (length(terms_included) == 0) "(intercept only)" else paste(terms_included, collapse = " + "),
    dropped_this_step = dropped_term,
    k = model$k,
    logLik = round(as.numeric(fs["logLik", 1]), 3),
    AIC = round(as.numeric(fs["AIC", 1]), 3),
    AICc = round(as.numeric(fs["AICc", 1]), 3),
    BIC = round(as.numeric(fs["BIC", 1]), 3),
    QM = round(model$QM, 3),
    QM_df = model$QMdf[1],
    QM_p = round(model$QMp, 4),
    QE = round(model$QE, 3)
  )
}

# =============================================================================
# Backward elimination: start with all 5 predictors, drop the weakest one at
# a time, stop when dropping starts costing meaningful fit (AIC) or the LRT
# says the dropped term mattered.
# =============================================================================
random_baseline <- ~ 1 | Citation/Row_ID

current_terms <- all_predictors
current_model <- fit_rma(current_terms, random = random_baseline, method = "ML")
comparison_table <- list(model_row(current_model, step = 0, terms_included = current_terms))

step_i <- 1
repeat {
  if (length(current_terms) == 0) break

  coef_tab <- coef(summary(current_model))
  coef_tab <- coef_tab[rownames(coef_tab) != "intrcpt", , drop = FALSE]
  weakest_term <- rownames(coef_tab)[which.max(coef_tab[, "pval"])]

  candidate_terms <- setdiff(current_terms, weakest_term)
  candidate_model <- fit_rma(candidate_terms, random = random_baseline, method = "ML")

  lrt <- anova(current_model, candidate_model)
  delta_aic <- as.numeric(fitstats(candidate_model)["AIC", 1]) - as.numeric(fitstats(current_model)["AIC", 1])
  aic_meaningfully_worse <- delta_aic > 2   # conventional Burnham & Anderson rule of thumb
  lrt_significant <- lrt$pval < 0.05

  cat(sprintf("\nStep %d: weakest term = %s (p = %.4f). Dropping it -> dAIC = %+.2f, LRT p = %.4f.\n",
              step_i, weakest_term, coef_tab[weakest_term, "pval"], delta_aic, lrt$pval))

  if (aic_meaningfully_worse || lrt_significant) {
    cat("STOP: dropping", weakest_term, "makes AIC meaningfully worse and/or the LRT is significant -- keeping it, previous model is final.\n")
    comparison_table[[length(comparison_table) + 1]] <- model_row(
      candidate_model, step = step_i, terms_included = candidate_terms,
      dropped_term = weakest_term, status = "rejected (stop point)"
    )
    break
  }

  cat("Dropping", weakest_term, "-- fit not meaningfully worse.\n")
  current_terms <- candidate_terms
  current_model <- candidate_model
  comparison_table[[length(comparison_table) + 1]] <- model_row(
    current_model, step = step_i, terms_included = current_terms, dropped_term = weakest_term
  )
  step_i <- step_i + 1
}

comparison_df <- bind_rows(comparison_table)
cat("\n=== Backward-elimination model comparison ===\n")
print(comparison_df, n = Inf, width = Inf)
cat("\nNote: QE is NA throughout -- metafor::rma.mv() only computes a residual-heterogeneity",
    "Q-test for models without a random-effects structure (like rma.uni()); with random=",
    "specified, residual heterogeneity is instead decomposed into the named variance",
    "components (sigma^2 per level) reported in each model's own summary() output.\n")

final_terms <- current_terms
cat("\nFinal fixed-effects structure after backward elimination: ",
    if (length(final_terms) == 0) "(intercept only)" else paste(final_terms, collapse = " + "), "\n", sep = "")

# =============================================================================
# Biome random effect: does adding a Biome_collapsed variance component earn
# its place on top of the chosen fixed-effects structure?
# =============================================================================
model_no_biome   <- current_model
model_with_biome <- fit_rma(final_terms, random = list(~ 1 | Citation/Row_ID, ~ 1 | Biome_collapsed), method = "ML")

cat("\n=== Biome random effect comparison ===\n")
print(bind_rows(
  model_row(model_no_biome, step = "no biome", terms_included = final_terms) ,
  model_row(model_with_biome, step = "+ biome", terms_included = final_terms)
) %>% select(step, k, logLik, AIC, AICc, BIC), n = Inf)

biome_var <- model_with_biome$sigma2[length(model_with_biome$sigma2)]
cat("\nBiome_collapsed variance component (sigma^2):", round(biome_var, 4), "\n")

biome_group_sizes <- model_data %>%
  group_by(Biome_collapsed) %>%
  summarise(n_rows = n(), n_citations = n_distinct(Citation), .groups = "drop") %>%
  arrange(desc(n_citations))
cat("\nCitations per Biome_collapsed level IN THE MODELING SUBSET (n =", nrow(model_data),
    "rows here; Script 1 reports the full-data counts before this subset's missingness filter):\n")
print(biome_group_sizes, n = Inf)

thin_biomes_model <- biome_group_sizes %>% filter(n_citations < 3)
if (nrow(thin_biomes_model) > 0) {
  cat("\n[CAUTION] Biome level(s) with <3 citations in the modeling subset -- their contribution",
      "to the Biome_collapsed variance component is unstable regardless of what sigma^2 comes out to:\n")
  print(thin_biomes_model, n = Inf)
}

aic_gain <- as.numeric(fitstats(model_no_biome)["AIC", 1]) - as.numeric(fitstats(model_with_biome)["AIC", 1])
use_biome_re <- aic_gain > 2
cat("\nAIC(no biome) - AIC(with biome) =", round(aic_gain, 2),
    if (use_biome_re) "-- biome earns its place (AIC improves by more than 2); keeping it in the final model.\n"
    else "-- biome does NOT earn its place (no meaningful AIC improvement); dropping it from the final model.\n")

final_random <- if (use_biome_re) list(~ 1 | Citation/Row_ID, ~ 1 | Biome_collapsed) else random_baseline

# =============================================================================
# Final model: refit with REML for reported variance components/coefficients
# =============================================================================
final_model <- fit_rma(final_terms, random = final_random, method = "REML")

cat("\n\n=================== FINAL MODEL (REML) ===================\n")
print(summary(final_model))

term_labels <- c(intrcpt = "Intercept", predictor_labels)
final_summary_table <- coef(summary(final_model)) %>%
  as_tibble(rownames = "term") %>%
  mutate(term = unname(term_labels[term])) %>%
  rename(SE = se, z = zval, p = pval, CI_lower = ci.lb, CI_upper = ci.ub) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

out_path <- "04_Output/metaanalysis_mlm_final_model_summary.csv"
write_csv(final_summary_table, out_path)
cat("\nFinal model summary table written to", out_path, "\n")
print(final_summary_table, n = Inf, width = Inf)
